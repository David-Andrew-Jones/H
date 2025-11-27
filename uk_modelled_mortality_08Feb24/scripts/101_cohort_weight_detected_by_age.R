#create graph for cohort weights by age for national screening model


# cohort weight by age ----------------------------------------------------
#filter down to relevant df
weight_screened_by_scenario_f = weight_screened_by_scenario %>%
  filter(scenario == "old_4") %>%  #results are the same by dwell time, so for simplifying 
  filter(model_type == "main") %>% #results are the same by model type
  select(-scenario, -model_type, -perc_skip, -abs_screened_by_age) #filter down to relevant variables

#generate figure 
foo = ggplot(weight_screened_by_scenario_f, aes(y=weight, x=tAge)) +
  geom_bar(stat = "identity", fill = "#3B1C52") +
  labs(
    y="Cohort Weight", 
    x="Age"
  ) +
  coord_flip() +
  theme_classic() +
  theme(text = element_text(size = 13))
print(foo)
ggsave(filename = sprintf("figs/%s_S8_cohort_weight.png", date_code), plot = foo, width = 10, height = 10, units = "cm")

# detected by MCED by age -------------------------------------------------
#filter down to relevant df
weighted_inc_mort_f = weighted_inc_mort %>%
  filter(local_haz_ratio==1) %>% #displaying inc so does not affect
  filter(model_type=="main") %>%
  select(tAge, scenario, weighted_cfdna) %>%
  mutate(scenario=case_when( #change the name for displaying
    scenario %in% c("old_2") ~ 'slow',
    scenario %in% c("old_3") ~ 'medium',
    scenario %in% c("old_4") ~ 'fast',
    TRUE~scenario
  ))

#generate figure 
foo = ggplot(weighted_inc_mort_f, aes(x=tAge, y=weighted_cfdna, group=scenario, color=scenario)) +
  geom_line(stat = "identity") +
  labs(
    x="Age", 
    y="Incidence Rate detected by MCED", 
  ) +
  theme_classic() + 
  scale_color_manual(values=c("#3B1C52","#94724F","#628592")) +
  theme(text = element_text(size = 13))
print(foo)
ggsave(filename = sprintf("figs/%s_S10_by_age_NSP.png", date_code), plot = foo, width = 10, height = 10, units = "cm")





