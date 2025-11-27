
# Basic epi for results ----------------------------------------------------
basic_epi = incidence_df_all %>%
  filter(Age== "50-79") %>%
  select(-Count, -Pop) %>%
  arrange(NCRAS_Draw) 

write.xlsx(basic_epi, sprintf("reports/%s_basic_epi_raw.xlsx", date_code))

#incidence by stage for supplementary materials
basic_epi_wide = basic_epi %>%
  mutate(Rate = format(round(Rate, 2), nsmall=2)) %>%
  pivot_wider(names_from = Stage, values_from = Rate)

write.xlsx(basic_epi_wide, sprintf("reports/%s_basic_epi_TableS2.xlsx", date_code))

#select subsets of staging variables for descriptive stats. 
be_staged = basic_epi %>%
  filter(Stage %in% c("I", "II", "III", "IV")) 

be_late = basic_epi %>%
  filter(Stage %in% c("III", "IV")) 

be_unknown = basic_epi %>% #cancers with a staging system, which have not been staged
  filter(Stage == "Unknown/missing") 

be_unstageable = basic_epi %>% #cancers without a staging system
  filter(Stage == "UNSTAGEABLE")

tot_inc = sum(as.numeric(basic_epi$Rate))
tot_staged = sum(as.numeric(be_staged$Rate))
tot_unknown = sum(as.numeric(be_unknown$Rate))
tot_late = sum(as.numeric(be_late$Rate))
tot_unstageable = sum(as.numeric(be_unstageable$Rate)) 
tot_stageable = tot_inc - tot_unstageable

be_res = data.frame(tot_inc, tot_staged, tot_unknown, tot_late, tot_unstageable, tot_stageable)
write.xlsx(be_res, sprintf("reports/%s_basic_epi_res.xlsx", date_code))
