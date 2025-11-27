#approximate lead time distribution by moment matching
#compute expected mean/variance based on independent, approximately memoryless distributions
#for each prequel,clinical combination for each cancer
#given dwell time scenarios

#this works for exponential
#if we start seriously altering shape parameter
#needs to invert the weibull parametes to variance
compute_lead_time_distribution_for_exponential<-function(local_dwell_time_df){
  xStage<-c("I","II","III","IV","NotStaged")
  
  #same pattern: all prequel/clinical combinations
  prequel_df<-tibble(clinical=1:4) %>%
    left_join(tibble(prequel=1:4),by=character()) %>%
    filter(prequel<=clinical) %>%
    add_row(clinical=5,prequel=5) %>%
    mutate(tmp=case_when(prequel==clinical ~ 3,
                         TRUE ~ 1)) %>%
    uncount(tmp,.id="tmp_state") %>%
    mutate(cfdna_detectable=case_when(tmp_state==1 ~ "yes",
                                      tmp_state==2 ~ "yes",
                                      tmp_state==3 ~ "no"),
           found_using=case_when(tmp_state==1 ~ "cfdna",
                                 tmp_state==2 ~ "soc",
                                 tmp_state==3 ~ "soc")) %>%
    select(clinical,prequel,cfdna_detectable,found_using)
  

  #compute cumulative sums to get expected values by differences
  dwell_sum<-local_dwell_time_df %>%
    mutate(number_stage=match(Stage,xStage),
           ns_flag=number_stage==5) %>%
    group_by(Cancer,scenario,ns_flag) %>%
    mutate(x_dwell=cumsum(dwell),
           v_dwell=cumsum(dwell^2)) %>%  #special case: variance of exponential is just mean^2
    ungroup()
  
  dwell_outer<-prequel_df %>%
    left_join(dwell_sum %>% 
                select(Cancer,scenario,prequel=number_stage,
                       start_x=x_dwell,start_v=v_dwell,start_d=dwell)) %>%
    left_join(dwell_sum %>%
                select(Cancer,scenario,clinical=number_stage,
                       end_x=x_dwell,end_v=v_dwell,end_d=dwell)) %>%
    mutate(expected_lead=end_x-start_x+start_d-0.5*end_d,
           var_lead=end_v-start_v+start_d^2-0.75*end_d^2)  #independent stages
  
  #generate gamma distribution parameters from mean/variance matching  
  dwell_outer<-dwell_outer %>%
    mutate(gamma_beta=expected_lead/var_lead,
           gamma_alpha=gamma_beta*expected_lead,
           gamma_scale=1/gamma_beta,
           gamma_shape=gamma_alpha) %>%
    select(Cancer,clinical,prequel,cfdna_detectable,found_using,scenario,
           expected_lead,var_lead,
           gamma_alpha,gamma_beta,
           gamma_scale,gamma_shape) %>%
    mutate(expected_lead=case_when(found_using=="soc" ~ 0.0,
                                   TRUE ~ expected_lead),
           var_lead=case_when(found_using=="soc" ~ 0.0,
                              TRUE ~ var_lead)) %>% #not clear what degenerate gamma parameters should be
  arrange(scenario,Cancer,clinical,prequel,cfdna_detectable,found_using)

  dwell_outer  
}