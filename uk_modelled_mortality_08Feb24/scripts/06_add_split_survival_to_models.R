#imputed survival, separate by detect
#this does a simple "5-year" separation
#looks at some different hazard ratios.

imputed_survival_by_age<-read_tsv(sprintf("generated_data/%s_imputed_survival_by_age.tsv", date_code))
stopifnot(length(unique(imputed_survival_by_age$NCRAS_Draw))==25)

#this generates split survival for cfdna+ and cfdna- for each age,cancer type, stage

xStage=c("I","II","III","IV","NotStaged")
split_survival<-imputed_survival_by_age %>%
  left_join(final_sens_table) %>%
  left_join(tibble(local_haz_ratio=c(1,2,3)),by=character()) %>%
  mutate(grail_negative=mapply(find_cfdna_negative_survival,CSS,local_haz_ratio,sens)) %>%
  mutate(grail_positive=grail_negative^local_haz_ratio) %>%
  select(Age,NCRAS_Draw,Stage,local_haz_ratio,grail_negative,grail_positive) %>%
  pivot_longer(cols=c("grail_positive","grail_negative"),names_to="type",values_to="CSS") %>%
  mutate(cfdna_detectable=case_when(type=="grail_positive" ~ "yes",
                                    TRUE ~ "no")) %>%
  mutate(prequel=match(Stage,xStage)) %>% 
  left_join(final_sens_table %>% select(NCRAS_Draw,Stage,sens)) %>%
  select(NCRAS_Draw,Age,prequel,cfdna_detectable,local_haz_ratio,CSS, sens) %>%
  mutate(sens=case_when(cfdna_detectable=="yes" ~ sens,
                        TRUE ~ 1-sens)) %>%
  rename(local_fraction=sens)

#join first which expands out local_haz ratio to add survivorship for prequel
#join second which matches local haz ratio to get original survivorship for clinical
#note that survival is only governed by effective stage at interception
#so that anything intercepted at early stage has that survival

interception_plus_survival_ALL_w<-interception_stage_shift_ALL_w %>%
  left_join(split_survival %>%
              rename(tAge=Age,prequel_CSS=CSS) %>%
              select(-local_fraction)) %>%
  left_join(split_survival %>%
              rename(tAge=Age,clinical=prequel,
                     clinical_CSS=CSS) %>%
              select(-local_fraction))

stopifnot(length(unique(interception_plus_survival_ALL_w$NCRAS_Draw))==25)
#check that only three hazard ratios are used so data frame is correctly sized
stopifnot(length(interception_plus_survival_ALL_w$scenario)/3==length(interception_stage_shift_ALL_w$scenario))

