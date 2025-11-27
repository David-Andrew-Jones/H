#for supplementary materials - 5 year survival by stage
#filter down to the relevant subset
surv_Z_f =  surv_5yr_bands_50_79 %>%
  select(-pot_unreliable) %>%
  filter(Months==60) %>%
  mutate(CSS=CSS*100) %>%
  mutate(CSS = format(round(CSS, 2), nsmall=2)) %>%
  filter(Stage!="Unknown/missing") %>%
  select(-Months, -Age) %>%
  arrange(NCRAS_Draw) %>%
  select(NCRAS_Draw, Stage, CSS) %>%
  pivot_wider(names_from = Stage, values_from = CSS)

write.xlsx(surv_Z_f, sprintf("reports/%s_surv_TableS3.xlsx", date_code))


#for supplementary materials - unreliability of survival estimates
#filter it down to only age bands for analysis
surv_5yr_bands_imp_rel = surv_5yr_bands %>% #filter down relevant age bands
  filter(Age %in% age_groups) %>% #only relevant age bands so as not to overstate amount of potentially unreliable estimates
  select(Stage, Months, pot_unreliable, NCRAS_Draw, Age) %>%
  filter(Stage!="Unknown/missing") %>%
  filter(Months==60) %>%
  arrange(NCRAS_Draw) %>%
  pivot_wider(names_from = Age, values_from = pot_unreliable)

write.xlsx(surv_5yr_bands_imp_rel,sprintf("reports/%s_surv_all_pot_unreliable.xlsx", date_code)) #write out to generated data
