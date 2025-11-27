
#use previously generated sensitivity table
sens_table<-read_tsv(sprintf("generated_data/%s_ccga3_iso_sens.tsv",date_code))

#pull in incidence
NCRAS_raw_all<-read_tsv(sprintf("generated_data/%s_imputed_incidence_by_year.tsv", date_code))

#find all cancer types and stages that need sensitivity
ncras_draw_basic<-NCRAS_raw_all %>% select(NCRAS_Draw,Stage) %>% distinct() 

#make sure every seer entry has a corresponding sensitivity
final_sens_table<-ncras_draw_basic %>% 
  left_join(sens_table %>% 
              rename(NCRAS_Draw=Cancer)) %>%
  select(NCRAS_Draw,Stage,c,n,sens) %>%
  mutate(sens=replace_na(sens,replace=0.0),
         n=replace_na(n,replace=0),
         c=replace_na(c,replace=0))

stopifnot(length(unique(final_sens_table$NCRAS_Draw))==25)
