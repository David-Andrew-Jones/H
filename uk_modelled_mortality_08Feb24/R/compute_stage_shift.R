#seer_incidence contains cancer incidence for each cancer and unique situation in index_cancer
#local_sens_table contains sensitivity for each cancer
#local_slip contains slip rate information for each dwell time scenario and screening interval in index_scenario
#index_cancer - cancer + any additional columns providing unique situations
#index_sceanrio - scenario + screening interval + any additional columns providing unique screening 

compute_parallel_interception_stage_shift<-function(ncras_raw_all, 
                                                    local_sens_table,
                                                    local_slip,
                                                    index_cancer,
                                                    index_scenario)
{
  xStage=c("I","II","III","IV","NotStaged")
  
  #expansion operator for getting chain of action
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
  
  #generate sensitivity
  #raw sensitivity = total shedding at stage
  #marginal = newly cfdna-detectable at stage
  #residual = number not cfdna-detectable by soc detection
  mis_sens_table<-final_sens_table %>% 
    mutate(prequel=match(Stage,xStage),
           ns_flag=prequel>4) %>%
    group_by(NCRAS_Draw,ns_flag) %>%
    mutate(marginal=diff(c(0,sens)),
           residual=1-sens) %>% 
    ungroup() %>%
    select(NCRAS_Draw,prequel,sens,marginal,residual) %>%
    uncount(3,.id="tmp_state") %>%
    mutate(cfdna_detectable=case_when(tmp_state==1 ~ "yes",
                                      tmp_state==2 ~ "yes",
                                      tmp_state==3 ~ "no"),
           found_using=case_when(tmp_state==1 ~ "cfdna",
                                 tmp_state==2 ~ "soc",
                                 tmp_state==3 ~ "soc")) %>%
    mutate(mis_sens=case_when(cfdna_detectable=="yes" & found_using=="cfdna" ~ marginal,
                              cfdna_detectable=="yes" & found_using=="soc" ~ 0,
                              cfdna_detectable=="no" ~ residual),
           prevalence_detectable=case_when(cfdna_detectable=="yes" & found_using=="cfdna" ~ sens,
                                           cfdna_detectable=="yes" & found_using=="soc" ~ 0,
                                           cfdna_detectable=="no" ~ 0)) %>% #not quite right: see later
    select(NCRAS_Draw,prequel,cfdna_detectable,found_using,mis_sens,prevalence_detectable)
  
  #compute MIS in parallel
  #does not depend on dwell, screening interval, anything else but incidence and sensitivity-by-stage
  #everything found at the earliest possible stage
  mis_unrolled<-ncras_raw_all %>%  
    mutate(clinical=match(Stage,xStage)) %>%
    left_join(prequel_df) %>%
    left_join(mis_sens_table) %>%
    mutate(number_detected=mis_sens*IR,
           prevalence_detected=prevalence_detectable*IR) #fraction detectable in prevalence round
  
  
  #slip rate plus dwell time for prevalence round
  # note dwell for clinical detection stage = half in expectation as clinical detection shortens time
  simple_slip<-local_slip %>% 
    rename(NCRAS_Draw=Cancer) %>%
    mutate(prequel=match(Stage,xStage),
           zero_slip=0.0) %>% 
    pivot_longer(cols=c("slip","slip_clinical","zero_slip"), names_to="status",values_to="slip") %>%
    mutate(dwell=case_when(status=="slip_clinical" ~ dwell*0.5,
                           TRUE ~ dwell)) %>% 
    select(all_of(c("NCRAS_Draw",index_scenario,"prequel","status","slip","dwell")))
  
  #slip to next stage until caught
  #last stage always slip 0
  quick_intercept<-function(ND,slip){
    z<-ND
    x<-0
    for (ii in 1:length(ND)){
      tt<-(x+ND[ii])
      z[ii]<-tt*(1-slip[ii])
      x<-tt-z[ii]
    }
    z
  }
  
  index_clinical_group<-c("clinical","cfdna_detectable")
  
  #execute slippage within each group
  #for each item in each scenario, return stage, whether cfdna detectable, how found, total caught
  # also compute prevalence round 
  # expected years of cancer latent = dwell
  # base rate per expected year of those that are detectable = prevalence_detectable*IR 
  # found through soc = same as in incidence round, as "newly detectable" cancers occur during the interval
  final_interception<-mis_unrolled %>%
    mutate(status=case_when(prequel==clinical & found_using=="cfdna" ~ "slip_clinical",
                            prequel<clinical ~ "slip",
                            TRUE ~ "zero_slip")) %>%
    left_join(simple_slip) %>%
    group_by(across(all_of(c(index_cancer,index_scenario,index_clinical_group)))) %>%
    mutate(caught=quick_intercept(number_detected,slip),
           prevalence_caught=dwell*prevalence_detected) %>%
    ungroup() %>%
    mutate(prevalence_caught=case_when(found_using=="cfdna" ~ prevalence_caught,
                                TRUE ~ caught)) %>%
    select(all_of(c(index_cancer,index_scenario,
                    "clinical","prequel","cfdna_detectable","found_using","caught", "prevalence_caught")))
  
  final_interception
}
