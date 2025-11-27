# 1. Read in data ---------------------------------------------------------
surv_full = as_tibble(read_excel("data/grail_survival_formatted.xlsx")) #NCRAS

# 2.  Renaming to match SEER/previous naming conventions ------------------
#to change NCRAS surv data to match SEER format
names(surv_full)[3] = "Cancer"
names(surv_full)[6] = "Stage"
names(surv_full)[7] = "Age"
names(surv_full)[8] = "Months" #currently in years, will need to be converted
names(surv_full)[10] = "CSS" #net rather than cause specific survival, naming it CSS to make code run
names(surv_full)[13] = "pot_unreliable"
surv = select(surv_full,Cancer,Stage,Months,CSS,Age,Sex,Subtype,pot_unreliable) #sex + subtype included to be filtered later

#change cancer names
surv = surv %>%
  mutate(NCRAS_Draw=case_when(
    Cancer %in% c("uterus") ~ "Uterus",
    Cancer %in% c("ovary") ~ "Ovary",
    Cancer %in% c("cervix") ~ "Cervix",
    Cancer %in% c("prostate") ~ "Prostate",
    Cancer %in% c("breast") ~ "Breast",
    Cancer %in% c("thyroid") ~ "Thyroid",
    Cancer %in% c("stomach") ~ "Stomach",
    Cancer %in% c("sarcoma") ~ "Sarcoma",
    Cancer %in% c("pancreas") ~ "Pancreas",
    Cancer %in% c("melanoma") ~ "Melanoma",
    Cancer %in% c("lymphoma") ~ "Lymphoma",
    Cancer %in% c("lung") ~ "Lung",
    Cancer %in% c("kidney") ~ "Kidney",
    Cancer %in% c("gallbladder") ~ "Gallbladder",
    Cancer %in% c("bladder") ~ "Bladder",
    Cancer %in% c("anus") ~ "Anus",
    Cancer %in% c("head_and_neck") ~ 'Head and Neck',
    Cancer %in% c("colon-rectum") ~ "Colon/Rectum",
    Cancer %in% c("lymphoid leukaemia") ~ "Lymphoid Leukemia",
    Cancer %in% c("lymphoid_leukaemia") ~ "Lymphoid Leukemia",
    Cancer %in% c("liver-bile_duct")  ~ "Liver/Bile-duct",
    Cancer %in% c("liver/bile duct")  ~ "Liver/Bile-duct",
    Cancer %in% c("myeloid_neoplasm") ~ "Myeloid Neoplasm",
    Cancer %in% c("oesophagus") ~ "Esophagus",
    Cancer %in% c("plasma_cell_neoplasm") ~ "Plasma Cell Neoplasm",
    Cancer %in% c("urothelial_tract") ~ "Urothelial Tract",
    Cancer %in% c("other") ~ "[OTHER]",
    TRUE~Cancer
  ))  

table(surv$NCRAS_Draw, surv$Cancer) #check renaming has worked 

#change stage names
surv$Stage[surv$Stage==1] = "I" #converts from numeric to roman for the staging variable
surv$Stage[surv$Stage==2] = "II"
surv$Stage[surv$Stage==3] = "III"
surv$Stage[surv$Stage==4] = "IV"
surv$Stage[surv$Stage=="Unstageable/unknown/missing"] = "Unknown/missing" #align naming conventions

stopifnot(all(surv$Stage %in% c("I","II","III","IV","Unknown/missing","All stages combined"))) #check staging info is correct

# 3.  Mutate survival variable --------------------------------------------
surv = surv %>%
  mutate(CSS = CSS/100) %>% #changes survival from % to prob as in SEER dataset 
  mutate(Months = Months*12) %>% #converts to months
  select(-Cancer) #filter Cancer var as no longer needed

surv$CSS[surv$CSS>=1] = 0.999 #to cap net survival values. 

# 4.  Using 'All stages combined' cancer survival for unstageable cancer types ------------
#NOTES: NCRAS have provided staged cancers for lymphoid leukemia, plasma cell neoplasm
#subtypes of this have TNM compliant staging
surv_unstageable = surv %>%
  filter(NCRAS_Draw %in% UNSTAGEABLE) %>%
  filter(Stage == "All stages combined") %>% #all other categories sum to this. 
  mutate(Stage = "NotStaged")  

surv_stageable = surv %>%
  filter(NCRAS_Draw %!in% UNSTAGEABLE) %>%
  filter(Stage != "All stages combined") #filter out as not required further

#to include surv nonstaged - sens is zero, so doesn't matter what survival is
#but it needs to be something to avoid N/A.
surv_OTHER = surv %>%
  filter(NCRAS_Draw == "[OTHER]" ) %>%
  filter(Stage == "All stages combined") %>%
  mutate(Stage = "NotStaged")

surv_X = bind_rows(surv_unstageable, surv_stageable, surv_OTHER)

# 5.  Filter to relevant data ---------------------------------------------

#selecting only main bio sex for cancer to ensure most reliable survival estimate
surv_Y = surv_X %>%
  filter(
    (NCRAS_Draw %in% FemCan & Sex=="Female") | 
      (NCRAS_Draw %in% MalCan & Sex=="Male") |
      (NCRAS_Draw %in% PerCan & Sex=="Persons")
  ) %>%
  select(-Sex)

#filter out all the different subtypes - use 'all combined' (for cancers with 1+ subtype) OR "N/A"
surv_5yr_bands = surv_Y %>%
  filter(Subtype == "N/A" | Subtype == "All combined" ) %>% #if cancer has sub-types, it will be 'All combined', otherwise N/A
  filter(Age %in% age_groups_plus) %>%
  select(-Subtype)

surv_5yr_bands_50_79 = surv_Y %>% #for supplementary infor to present survival data 
  filter(Subtype == "N/A" | Subtype == "All combined" ) %>% #if cancer has sub-types, it will be 'All combined', otherwise N/A
  filter(Age == "50-79") %>%
  select(-Subtype)


#survival data where there is NA - get data from nearest group
#conservative assumption - getting the worst survival data as getting data from the older group
surv_5yr_bands_imp = surv_5yr_bands %>% 
  group_by(NCRAS_Draw, Stage, Months) %>%  
  arrange(desc(Age)) %>% #arrange the data oldest to youngest
  mutate(CSS_imp = na.locf(CSS, na.rm = FALSE)) %>% #search for the most recent non-NA prior (order means it is getting older values)
  mutate(CSS_imp = na.locf(CSS_imp, na.rm = FALSE, fromLast = TRUE)) %>% #next observation carried backwards - for when there is no older age group
  filter(Age %in% age_groups) %>% #filter to all relevant age groups
  ungroup() 

surv_5yr_bands_imp$CSS_withNA = surv_5yr_bands_imp$CSS #rename orig CSS to highlight it has NA values in it
surv_5yr_bands_imp$CSS = surv_5yr_bands_imp$CSS_imp #overwrite CSS with CSS_imp to update to include the interpolated survival estimates

# 6.  Write out data ------------------------------------------------------

#for analysis
surv3 = select(surv_5yr_bands_imp,NCRAS_Draw,Stage,Months,CSS, Age) #select variables to be written out
write_tsv(surv3,sprintf("generated_data/%s_age_joint_raw.tsv", date_code)) #write out to generated data
