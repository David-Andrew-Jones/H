#script to interpolate to get css per year of age
raw_survival<-read_tsv(sprintf("generated_data/%s_age_joint_raw.tsv", date_code)) 

#NOTES: Some lymphoid leukemia and plasma cell neoplasm are staged in the NCRAS data - should already be excluded at this point. 
not_staged<-c("Lymphoid Leukemia","Myeloid Neoplasm","Plasma Cell Neoplasm","CUP","Brain") 

#5-year survival 
focused_survival_main <- raw_survival %>%
  filter(Months == 60) %>%
  mutate(Stage = case_when(Stage == "Unknown/missing" ~ "NotStaged",
                           TRUE ~ Stage)) %>%
  mutate(
    valid_staged = (Stage != "NotStaged" &
                      !(NCRAS_Draw %in% not_staged)),
    valid_not_staged = Stage == "NotStaged" &
      (NCRAS_Draw %in% not_staged),
    valid = valid_staged | valid_not_staged
  ) %>%
  filter(valid) %>%
  mutate(Age = gsub(" years", "", Age)) %>%
  separate(Age, into = c("Low", "Hi")) %>%
  mutate(Low = as.numeric(Low),
         Hi = as.numeric(Hi)) 

#brute force through the [OTHER] cancer category as a NotStaged cancer also
focused_survival_other <- raw_survival %>%
  filter(Months == 60) %>%
  filter(NCRAS_Draw=="[OTHER]" & Stage == "NotStaged") %>%
  mutate(Age = gsub(" years", "", Age)) %>%
  separate(Age, into = c("Low", "Hi")) %>%
  mutate(Low = as.numeric(Low),
         Hi = as.numeric(Hi))

focused_survival = bind_rows(focused_survival_main, focused_survival_other)

full_css_plus_fcn<-focused_survival %>% 
  mutate(Age=(Low+Hi+1)/2) %>%
  group_by(NCRAS_Draw,Stage,Months) %>%
  summarize(zeta=list(approxfun(Age,CSS,rule=2))) %>%
  ungroup()

imputed_survival_by_age<-focused_survival %>%
  select(NCRAS_Draw,Stage) %>%
  distinct() %>%
  left_join(tibble(Age=50:79),by=character()) %>%
  left_join(full_css_plus_fcn)  %>%
  mutate(CSS=sapply(1:length(Age),function(z){zeta[z][[1]](Age[z])})) %>%
  select(Age,NCRAS_Draw,Stage,Months,CSS)

write_tsv(imputed_survival_by_age,sprintf("generated_data/%s_imputed_survival_by_age.tsv",date_code))

#do the full survival matrix out to whatever maximum months

#full frame of months x age
full_frame_survival <- raw_survival %>%
  #  filter(Months == 60) %>%
  mutate(Stage = case_when(Stage == "Unknown/missing" ~ "NotStaged",
                           TRUE ~ Stage)) %>%
  mutate(
    valid_staged = (Stage != "NotStaged" &
                      !(NCRAS_Draw %in% not_staged)),
    valid_not_staged = Stage == "NotStaged" &
      (NCRAS_Draw %in% not_staged),
    valid = valid_staged | valid_not_staged
  ) %>%
  filter(valid) %>%
  mutate(Age = gsub(" years", "", Age)) %>%
  separate(Age, into = c("Low", "Hi")) %>%
  mutate(Low = as.numeric(Low),
         Hi = as.numeric(Hi)) %>%
  select(Low,Hi,NCRAS_Draw,Stage,Months,CSS)

tmp_full_frame<-full_frame_survival %>%
  select(Low,Hi,NCRAS_Draw,Stage) %>%
  distinct() %>%
  mutate(Months=0,
         CSS=1.0)

full_frame_survival<-full_frame_survival %>%
  bind_rows(tmp_full_frame) %>%
  arrange(Low,Hi,NCRAS_Draw,Stage,Months)

full_frame_css_plus_fcn<-full_frame_survival %>% 
  mutate(Age=(Low+Hi+1)/2) %>%
  group_by(NCRAS_Draw,Stage,Months) %>%
  summarize(zeta=list(approxfun(Age,CSS,rule=2))) %>%
  ungroup()

#very large
full_imputed_survival_by_age<-full_frame_survival %>%
  select(NCRAS_Draw,Stage,Months) %>%
  distinct() %>%
  left_join(tibble(Age=50:79),by=character()) %>%
  left_join(full_frame_css_plus_fcn)  %>%
  mutate(CSS=sapply(1:length(Age),function(z){zeta[z][[1]](Age[z])})) %>%
  select(Age,NCRAS_Draw,Stage,Months,CSS)  %>%
  arrange(NCRAS_Draw,Age,Stage,Months)

write_tsv(full_imputed_survival_by_age,sprintf("generated_data/%s_full_imputed_survival_by_age.tsv",date_code))


