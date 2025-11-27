
# 1. Read in data ------------------------------------------------------------
inc_full_5yr_w_Dep = as_tibble(read_excel("data/grail_incidence_formatted_v2.xlsx")) #NCRAS data
#age_bands = as_tibble(read_excel("data/age_bands.xlsx")) #age bands

#NOTES: based on IMD data release, as initial release did not provide sex
inc_full_5yr = inc_full_5yr_w_Dep %>%
  filter(deprivationqunitile == "All deprivations") %>%
  filter(subtype=="N/A") %>% #filter out all sub-types, when they are relevant, they are combined in N/A
  filter(agegroup %in% age_groups_tot) %>%
  select(-deprivationqunitile, -cohort, -geography, -lci95, -uci95, -subtype)#vars that are never used

# 2. Renaming to match SEER/previous naming conventions ---------------------------------------
#to change NCRAS inc data to match SEER format
names(inc_full_5yr)[2] = "Age"  
names(inc_full_5yr)[3] = "Cancer"  
names(inc_full_5yr)[4] = "Stage"  
names(inc_full_5yr)[5] = "Count"
names(inc_full_5yr)[6] = "Pop"  
names(inc_full_5yr)[7] = "Rate" 

#to change british spelling to match SEER format
inc_5yr = inc_full_5yr %>%
    mutate(NCRAS_Draw=case_when(
    Cancer %in% c("Head_and_Neck") ~ 'Head and Neck',
    Cancer %in% c("Colon-Rectum") ~ "Colon/Rectum",
    Cancer %in% c("Lymphoid Leukaemia") ~ "Lymphoid Leukemia",
    Cancer %in% c("Lymphoid_Leukaemia") ~ "Lymphoid Leukemia",
    Cancer %in% c("Liver-Bile_duct")  ~ "Liver/Bile-duct",
    Cancer %in% c("Liver/Bile duct")  ~ "Liver/Bile-duct",
    Cancer %in% c("Myeloid neoplasm") ~ "Myeloid Neoplasm",
    Cancer %in% c("Oesophagus") ~ "Esophagus",
    Cancer %in% c("Plasma cell neoplasm") ~ "Plasma Cell Neoplasm",
    Cancer %in% c("Urothelial_Tract") ~ "Urothelial Tract",
    Cancer %in% c("Other") ~ "[OTHER]",
    TRUE~Cancer
  ))  

table(inc_5yr$NCRAS_Draw, inc_5yr$Cancer)  #check renaming has worked
inc_5yr = select(inc_5yr,-Cancer) #sex + subtype included to be filtered later

stopifnot(length(unique(inc_5yr$NCRAS_Draw))==25)

inc_5yr$Stage[inc_5yr$Stage==1] = "I"
inc_5yr$Stage[inc_5yr$Stage==2] = "II"
inc_5yr$Stage[inc_5yr$Stage==3] = "III"
inc_5yr$Stage[inc_5yr$Stage==4] = "IV"
inc_5yr$Stage[inc_5yr$Stage=="Unstageable/unknown/missing"] = "Unknown/missing"

#if any are NA or are typo - next test will fail
stopifnot(all(inc_5yr$Stage %in% c("I","II","III","IV","Unknown/missing","All stages")))

# 3. Adjust down the IR of sex-specific cancers ---------------------------
#create a df with pop by sex by age band
pop_by_sex = inc_5yr %>%
  select(-NCRAS_Draw, -Stage, -Rate, -Count) %>%
  group_by(sex, Age) %>%
  unique() %>%
  ungroup()

#what proportion do each sex make up of the total pop 
pop_by_sex_persons = pop_by_sex %>%
  filter(sex=="Persons") %>%
  mutate(Persons_pop= Pop) %>% 
  select(-sex, -Pop)

pop_by_sex_sex = pop_by_sex %>%
  filter(sex!="Persons") %>%
  left_join(pop_by_sex_persons, by=c("Age")) %>%
  mutate(prop_sex_pop = Pop/Persons_pop)  %>% #what proportion does each sex make up of the total population?
  select(-Pop, -Persons_pop)

inc_5yr_sex_spec = inc_5yr %>%
  filter(NCRAS_Draw %!in% PerCan) %>%
  left_join(pop_by_sex_sex, by=c("sex","Age")) %>%
  mutate(Rate = Rate*prop_sex_pop, #create person specific rate by multiplying by proportion sex makes up of total pop
         sex = "Persons") %>% #over-write the old sex with persons to match amended rate
  select(-prop_sex_pop, -sex)

inc_5yr_not_sex_spec = inc_5yr %>%
  filter(NCRAS_Draw %in% PerCan) %>%
  filter(sex=="Persons") %>% #select person specific rate
  select(-sex)

inc_5yr_Y = bind_rows(inc_5yr_sex_spec, inc_5yr_not_sex_spec)

# 4.  Using "All stages" for unstageable cancer types ------------
#NOTES: NCRAS have provided staged cancers for lymphoid leukemia, plasma cell neoplasm
#subtypes of this have TNM compliant staging

inc_5yr_unstageable = inc_5yr_Y %>%
  filter(NCRAS_Draw %in% UNSTAGEABLE) %>%
  filter(Stage == "All stages") %>% #all other categories sum to this. 
  mutate(Stage = "UNSTAGEABLE")  

inc_5yr_stageable = inc_5yr_Y %>%
  filter(NCRAS_Draw %!in% UNSTAGEABLE) %>%
  filter(Stage != "All stages")

incidence_df_all = bind_rows(inc_5yr_unstageable, inc_5yr_stageable)
incidence_df = incidence_df_all %>%
  filter(Age %in% age_groups)

#5.  Interpolation -----------------------------------------------------------

#tidy struct and prep for the interpolation
fixed_incidence_df<-incidence_df %>%
  filter(Age!="Unknown") %>%
  unnest(cols=c(Rate)) %>%
  type_convert() %>%
  mutate(tAge=as.numeric(substr(Age,1,2)))

#linear interpolation of Rate / Count / Pop variables by age
full_inc_plus_fcn = fixed_incidence_df %>% 
  separate(Age, into = c("Low", "Hi")) %>%
  mutate(Low = as.numeric(Low),
         Hi = as.numeric(Hi)) %>%
  mutate(MidAge=(Low+Hi+1)/2) %>% #middle of age band
  group_by(NCRAS_Draw, Stage) %>% 
  summarise(
    zeta=list(splinefun(MidAge,Rate,method="natural")), 
    zeta_2=list(splinefun(MidAge,Pop,method="natural")),
    zeta_3=list(splinefun(MidAge,Count,method="natural")),
  ) %>%
  ungroup()

interpolated_inc_by_age = fixed_incidence_df %>%
  select(NCRAS_Draw, Stage) %>%
  distinct() %>%
  left_join(tibble(Age=50:79),by=character()) %>% 
  left_join(full_inc_plus_fcn) %>%
  mutate(Rate=sapply(1:length(Age),function(z){zeta[z][[1]](Age[z])})) %>% 
  mutate(Pop=sapply(1:length(Age),function(z){zeta_2[z][[1]](Age[z])})) %>% 
  mutate(Count=sapply(1:length(Age),function(z){zeta_3[z][[1]](Age[z])})) %>% 
  select(NCRAS_Draw, Stage, Rate, Age, Pop, Count)


# 6. Write out data ----------------------------------------------------------
stopifnot(length(unique(interpolated_inc_by_age$NCRAS_Draw))==25)
write_tsv(interpolated_inc_by_age, sprintf("generated_data/%s_raw_incidence_by_year.tsv", date_code))
