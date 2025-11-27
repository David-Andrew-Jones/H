
#read in the incidence data
interpolated_inc_by_age = read_tsv(sprintf("generated_data/%s_raw_incidence_by_year.tsv", date_code))
 
NCRAS_raw_numbers<-interpolated_inc_by_age %>%
  filter(Age!="Unknown") %>%
  unnest(cols=c(Rate)) %>%
  type_convert() %>%
  mutate(tAge=as.numeric(substr(Age,1,2)))

not_staged<-c("Lymphoid Leukemia","Myeloid Neoplasm","Plasma Cell Neoplasm") #no CUP and Brain for UK data

NCRAS_raw_imputable<-NCRAS_raw_numbers %>%
  filter(!NCRAS_Draw %in% not_staged, NCRAS_Draw!="[OTHER]")

NCRAS_raw_imputed<-NCRAS_raw_imputable %>% 
  group_by(NCRAS_Draw,tAge) %>% 
  mutate(Uflag=1*(Stage=="Unknown/missing"),
         IR=Rate*sum(Rate)/sum(Rate*(1-Uflag))) %>%
  ungroup() %>%
  filter(Stage!="Unknown/missing") %>%
  mutate(IR=case_when(is.nan(IR) ~ 0.0,
                      TRUE ~ IR)) %>%
  select(NCRAS_Draw,Stage,tAge,IR)

#for cancers with no staging system, re-naming staging status to be not staged. 
NCRAS_raw_not_staged<-NCRAS_raw_numbers %>%
  filter(NCRAS_Draw %in% not_staged) %>%
  filter(Stage=="UNSTAGEABLE") %>%
  mutate(Stage="NotStaged") %>%
  select(NCRAS_Draw,Stage,tAge,IR=Rate)

#for [OTHER] cancer category, unstageable pro to be not staged, while keeping staged. 
NCRAS_raw_other<-NCRAS_raw_numbers %>%
  filter(NCRAS_Draw=="[OTHER]") %>%
  mutate(Stage=case_when(Stage=="Unknown/missing" ~ "NotStaged",
                         TRUE ~ Stage)) %>%
  select(NCRAS_Draw,Stage,tAge,IR=Rate)

#joining df together
NCRAS_raw_all = bind_rows(NCRAS_raw_imputed, NCRAS_raw_not_staged, NCRAS_raw_other)

#NCRAS raw all gives imputed values for everything
stopifnot(length(unique(NCRAS_raw_all$NCRAS_Draw))==25)
write_tsv(NCRAS_raw_all,sprintf("generated_data/%s_imputed_incidence_by_year.tsv",date_code))
