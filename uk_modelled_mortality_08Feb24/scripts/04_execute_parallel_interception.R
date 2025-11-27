# generate two-population interception model
# only depends on grouping columns, incidence, sensitivity
# add survival, lead time to tibble later


#inputs: local_sens_table
#seer_incidence

#local_sens_table<-final_sens_table
#seer_incidence<-seer_raw_all
#local_slip<-multiple_dwell_slip_rate

#all key columns to isolate unique cancer cases
index_cancer<-c("NCRAS_Draw","tAge") #Sex taken out
#all key columns to isolate unique scenarios in dwell time
index_scenario<-c("scenario","screen_interval")

#seer_incidence contains cancer incidence for each cancer and unique situation in index_cancer
#local_sens_table contains sensitivity for each cancer
#local_slip contains slip rate information for each dwell time scenario and screening interval in index_scenario
#index_cancer - cancer + any additional columns providing unique situations
#index_sceanrio - scenario + screening interval + any additional columns providing unique screening 

#1. seq model
#compute scenarios where cancers move sequentially through stages

interception_stage_shift<-compute_parallel_interception_stage_shift(NCRAS_raw_all,
                                                                   final_sens_table,
                                                                  multiple_dwell_slip_rate,
                                                                  index_cancer,
                                                                  index_scenario)

stopifnot(length(unique(interception_stage_shift$NCRAS_Draw))==25)
#verify that model is conserving cancer incidence in long-term estimation
check_invariant_incidence_seq<-interception_stage_shift %>% 
  group_by(tAge,clinical,scenario) %>% 
  summarize(total=sum(caught)) %>% 
  ungroup() %>% 
  group_by(tAge,clinical) %>% 
  summarize(val=sd(total)) %>%
  ungroup() %>% 
  summarize(total=sum(val>1e-6)) %>%
  pull(total)
  
stopifnot(check_invariant_incidence_seq==0)


#2. nonseq no prior history
# executing interception where for certain cancers anything that would be detected at stage 4 is "born bad"
# and therefore cannot be detected at a non-metastatic stage
interception_stage_shift_nonseq_nph<-compute_parallel_interception_stage_shift(NCRAS_raw_all,
                                                                    final_sens_table,
                                                                    multiple_dwell_slip_rate_nonseq_NPH,
                                                                    index_cancer,
                                                                    index_scenario)

stopifnot(length(unique(interception_stage_shift_nonseq_nph$NCRAS_Draw))==25)

#verify that model is conserving cancer incidence in long-term estimation
check_invariant_incidence_nonseq_nph<-interception_stage_shift_nonseq_nph %>% 
  group_by(tAge,clinical,scenario) %>% 
  summarize(total=sum(caught)) %>% 
  ungroup() %>% 
  group_by(tAge,clinical) %>% 
  summarize(val=sd(total)) %>%
  ungroup() %>% 
  summarize(total=sum(val>1e-6)) %>%
  pull(total)

stopifnot(check_invariant_incidence_nonseq_nph==0)

#verify no prior history
#note: small prevalence round estimate at the moment due to 1e-3 dwell time
#can be corrected if desired
check_nph_incidence_round<-interception_stage_shift_nonseq_nph %>%
  mutate(prespec_flag=NCRAS_Draw %in% PRE_SPEC,
         prespec_stage=prequel<4 & clinical==4) %>%
  filter(prespec_flag,prespec_stage) %>%
  summarize(total=sum(caught>1e-6)) %>%
  pull(total)

stopifnot(check_nph_incidence_round==0)


#3. nonseq prior history
#executing interception where for certain cancers anything that would be dteected at stage IV is "bad post birth"
# and can be detected at stage I and be non-metastatic, but every other stage is effectively stage IV
interception_stage_shift_nonseq_ph<-compute_parallel_interception_stage_shift(NCRAS_raw_all,
                                                                               final_sens_table,
                                                                               multiple_dwell_slip_rate_nonseq_PH,
                                                                               index_cancer,
                                                                               index_scenario)

stopifnot(length(unique(interception_stage_shift_nonseq_ph$NCRAS_Draw))==25)

#verify  prior history in stage 1
#note: small prevalence round estimate at the moment due to 1e-3 dwell time
#can be corrected if desired
check_ph_incidence_round<-interception_stage_shift_nonseq_ph %>%
  mutate(prespec_flag=NCRAS_Draw %in% PRE_SPEC,
         prespec_stage=prequel>1 & prequel<4 & clinical==4) %>%
  filter(prespec_flag,prespec_stage) %>%
  summarize(total=sum(caught>1e-6)) %>%
  pull(total)

stopifnot(check_ph_incidence_round==0)

#check we are stopping some at stage 1
check_ph_incidence_round_stop<-interception_stage_shift_nonseq_ph %>%
  mutate(prespec_flag=NCRAS_Draw %in% PRE_SPEC,
         prespec_stage=prequel==1 & clinical==4) %>%
  filter(prespec_flag,prespec_stage) %>%
  summarize(total=sum(caught<1e-6)) %>%
  pull(total)

stopifnot(check_ph_incidence_round_stop==0)

#three interception model outputs giving stage shift by age, scenario, how much PH is allowed
