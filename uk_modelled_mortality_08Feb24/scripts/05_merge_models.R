
#reduce data frames with just stage shift/incidence

interception_stage_shift = interception_stage_shift %>%
  filter(screen_interval==SCREEN_INTERVAL) %>%
  filter(scenario %in% DWELL_TIME) %>% #filters down to 3 DT scenarios we will be looking at
  filter(tAge>=START_AGE) %>%
  filter(tAge<=END_AGE) %>%
  select(-screen_interval)

interception_stage_shift_nonseq_nph = interception_stage_shift_nonseq_nph %>%
  filter(screen_interval==SCREEN_INTERVAL) %>%
  filter(scenario %in% DWELL_TIME) %>% #filters down to 3 DT scenarios we will be looking at
  filter(tAge>=START_AGE) %>%
  filter(tAge<=END_AGE) %>%
  select(-screen_interval)

interception_stage_shift_nonseq_ph = interception_stage_shift_nonseq_ph%>%
  filter(screen_interval==SCREEN_INTERVAL) %>%
  filter(scenario %in% DWELL_TIME) %>% #filters down to 3 DT scenarios we will be looking at
  filter(tAge>=START_AGE) %>%
  filter(tAge<=END_AGE) %>%
  select(-screen_interval)



# combine stage shift results ---------------------------------------------

# 2. nonseq NPH -----------------------------------------------------------


#rename vars in nonseq models to distinguish these from the main result
interception_stage_shift_nonseq_nph_tmp <- interception_stage_shift_nonseq_nph %>%
  rename(caught.nonseq = caught,
         prevalence_caught.nonseq = prevalence_caught)

base_interception_stage_shift_mixed_NPH <- interception_stage_shift %>%
  left_join(interception_stage_shift_nonseq_nph_tmp, 
            by = c("NCRAS_Draw","tAge","scenario","clinical","prequel","cfdna_detectable","found_using")) %>%
  mutate(perc_skip = 999) %>% #dummy column to use in rbind
  mutate(model_type = "main") #dummy column to use in rbind

interception_stage_shift_mixed_NPH<-base_interception_stage_shift_mixed_NPH

rm(interception_stage_shift_nonseq_nph_tmp)
rm(interception_stage_shift_nonseq_nph)


#Weighted average of stage skip and non-stage skip scenarios    
for (j in PERCENT_SKIP){
  tmp = base_interception_stage_shift_mixed_NPH %>%
    mutate(model_type = NPH_MODEL) %>%
    mutate(perc_skip = j) %>%
    mutate(caught = case_when(
      clinical == 4  ~ caught * (1-j) + caught.nonseq * j,
      TRUE ~ caught)) %>%
    mutate(prevalence_caught = case_when(
      clinical == 4  ~ prevalence_caught * (1-j) + prevalence_caught.nonseq * j,
      TRUE ~ prevalence_caught))  
  interception_stage_shift_mixed_NPH = rbind(interception_stage_shift_mixed_NPH, tmp)}
rm(tmp)

interception_stage_shift_mixed_NPH = interception_stage_shift_mixed_NPH %>%
  filter(perc_skip != 999)


# 3.  nonseq PH -----------------------------------------------------------

#rename vars in nonseq models to distinguish these from the main result
interception_stage_shift_nonseq_ph_tmp <- interception_stage_shift_nonseq_ph %>%
  rename(caught.nonseq = caught,
         prevalence_caught.nonseq = prevalence_caught)

base_interception_stage_shift_mixed_PH <- interception_stage_shift %>%
  left_join(interception_stage_shift_nonseq_ph_tmp, 
            by = c("NCRAS_Draw","tAge","scenario","clinical","prequel","cfdna_detectable","found_using")) %>%
  mutate(perc_skip = 999) %>% #dummy column to use in rbind
  mutate(model_type = "main") #dummy column to use in rbind

interception_stage_shift_mixed_PH<-base_interception_stage_shift_mixed_PH

rm(interception_stage_shift_nonseq_ph_tmp)
rm(interception_stage_shift_nonseq_ph)

#Weighted average of stage skip and non-stage skip scenarios    
for (j in PERCENT_SKIP){
  tmp = base_interception_stage_shift_mixed_PH %>%
    mutate(model_type = PH_MODEL) %>%
    mutate(perc_skip = j) %>%
    mutate(caught = case_when(
      clinical == 4  ~ caught * (1-j) + caught.nonseq * j,
      TRUE ~ caught)) %>%
    mutate(prevalence_caught = case_when(
      clinical == 4  ~ prevalence_caught * (1-j) + prevalence_caught.nonseq * j,
      TRUE ~ prevalence_caught)) 
  interception_stage_shift_mixed_PH = rbind(interception_stage_shift_mixed_PH, tmp) 
}

interception_stage_shift_mixed_PH = interception_stage_shift_mixed_PH %>%
  filter(perc_skip != 999)

# combine both nonseq models for stage shift results ----------------------

interception_stage_shift_ALL_w = rbind(interception_stage_shift_mixed_NPH, interception_stage_shift_mixed_PH)

interception_stage_shift_ALL_w = interception_stage_shift_ALL_w %>%
  filter(!(model_type == "NPH" & perc_skip == 0)) %>%
  mutate(model_type = case_when(
    (perc_skip==0) ~ "main",
    TRUE~model_type)) %>%
  distinct()

rm(interception_stage_shift_mixed_NPH)
rm(interception_stage_shift_mixed_PH)