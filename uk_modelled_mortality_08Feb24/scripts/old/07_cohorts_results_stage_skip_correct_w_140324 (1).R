###################
#CORRECTION: Corrected weighting calculation. Same weights are applied but calculation is sum(var*weight)/sum(weight)
###################

# 10 - Generate open cohort and national programme (birth cohort) results for paper from the full combinatorial results
#Includes scenarios with both different dwell times and HR used for cfDNA+/cfDNA-

# Load ONS Life Tables  ---------------------------------------------------
#Average male & female mortality rates for each age, prob death from age x to age x+1, keep only mortality rate and age
population_mortality <- read.csv("data/UK_life_table_1719.csv", header = TRUE, skip = 1) %>%
  mutate(mortality = ((qx+qx.1)/2)) %>% #qx mortality is probability of death before age x+1 per 100,000 alive at age x
  rename(tAge = x) %>%
  select(tAge, mortality)

# Vars used in analysis (groups) ---------------------------------------------
n_analysis <- 100000 #Population size for analysis reporting
SURV_GROUP = c("local_haz_ratio", "scenario", "perc_skip","model_type")
SURV_GROUP_AGE = c( "tAge", "local_haz_ratio", "scenario", "perc_skip","model_type")
SURV_GROUP_AGE_BY_CANCER = c("NCRAS_Draw","tAge", "local_haz_ratio", "scenario", "perc_skip","model_type")
SURV_GROUP_BY_CANCER = c("NCRAS_Draw","local_haz_ratio", "scenario", "perc_skip","model_type")

SS_GROUP = c("scenario", "perc_skip","model_type") 
SS_GROUP_AGE = c("tAge","scenario", "perc_skip","model_type") 
SS_GROUP_AGE_FU = c("tAge","scenario", "perc_skip","model_type","found_using") 
SS_GROUP_BY_CANCER = c("NCRAS_Draw","scenario", "perc_skip","model_type") 
SS_GROUP_AGE_BY_CANCER = c("NCRAS_Draw", "tAge","scenario", "perc_skip","model_type") 
SS_GROUP_AGE_BY_CANCER_FU = c("NCRAS_Draw", "tAge","scenario", "perc_skip","model_type","found_using") 

# Mortality reduction by cancer  -------------------------------------------------
mortality_reduction_by_cancer = interception_plus_survival_ALL_w %>%  
  mutate(
    deaths = (1 - prequel_CSS) * caught, #deaths with MCED
    original_deaths = (1 - clinical_CSS) * caught #deaths without MCED
  ) %>%
  mutate(
    deaths_prevalence = (1 - prequel_CSS) * prevalence_caught, #deaths with MCED in prevalence
    original_deaths_prevalence = (1 - clinical_CSS) * prevalence_caught #deaths without MCED in prevalence
  ) %>%
  group_by_at(SURV_GROUP_AGE_BY_CANCER) %>% 
  summarize(
    shifted_deaths = sum(deaths), #deaths with MCED, shifted
    original_deaths = sum(original_deaths), #deaths without MCED
    shifted_deaths_prevalence = sum(deaths_prevalence), #deaths with MCED, shifted, prevalence
    original_deaths_prevalence = sum(original_deaths_prevalence) #deaths without MCED, prevalence
  ) %>%
  ungroup()

# Mortality reduction agg --------------------------------------
mortality_reduction_agg = mortality_reduction_by_cancer %>%
  group_by_at(SURV_GROUP_AGE) %>%
  summarize(
    shifted_deaths = sum(shifted_deaths),
    original_deaths = sum(original_deaths),
    shifted_deaths_prevalence = sum(shifted_deaths_prevalence),
    original_deaths_prevalence = sum(original_deaths_prevalence)
  ) %>%
  ungroup()

# Incidence by cancer type ---------------------------------------
incidence_by_cancer = interception_stage_shift_ALL_w %>%
  group_by_at(SS_GROUP_AGE_BY_CANCER_FU) %>% #split this out by found_using
  summarize(
    incidence_cancer = sum(caught),
    incidence_cancer_prevalence = sum(prevalence_caught)
  ) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = all_of(c(SS_GROUP_AGE_BY_CANCER)),
    names_from = "found_using",
    values_from = c("incidence_cancer" , "incidence_cancer_prevalence")
  ) %>%
  rename(
    cfdna = incidence_cancer_cfdna,
    soc = incidence_cancer_soc,
    cfdna_prevalence = incidence_cancer_prevalence_cfdna,
    soc_prevalence = incidence_cancer_prevalence_soc
  ) %>%
  mutate(
    total_incidence = cfdna + soc,
    total_incidence_prevalence = cfdna_prevalence + soc_prevalence
  ) %>%
  ungroup()


# Incidence agg -------------------
incidence_agg = incidence_by_cancer %>%
  group_by_at(SS_GROUP_AGE) %>%
  summarize(
    total_incidence = sum(total_incidence),
    cfdna = sum(cfdna),
    soc = sum(soc),
    total_incidence_prevalence = sum(total_incidence_prevalence),
    cfdna_prevalence = sum(cfdna_prevalence),
    soc_prevalence = sum(soc_prevalence)
  ) %>%
  ungroup()

# Create weights for aggregating results ----------------------------------

#Construct the life table for number to be remaining to be screened in each round
#as we are looking for first cancers
#incidence of cancer removes people from the screening population
#note that incidence is per 100K, as it is a rate computed
cumulative_LT = population_mortality %>%
  filter(tAge >= START_AGE &
           tAge <= END_AGE) %>%  #only for ages over which screening takes place
  left_join(incidence_agg, by = "tAge") %>%
  select(tAge,scenario,perc_skip,model_type,total_incidence,total_incidence_prevalence,mortality)

#compute number screened for situation
#individuals leave screening cohort when they either die or are first diagnosed with cancer
#for simplicity, not modeling prevalence round changes in cancer incidence at the moment
#so every scenario is consistent in number of people
#will affect output by <1% in general because 50 year olds don't have much cancer, so not much cancer is pulled
#from the future
cancer_rate_scale<-100000 #rate in database is per 100K - not necessarily the same as the number analyzed for output
cumulative_screened<-cumulative_LT %>%
  group_by(scenario,perc_skip,model_type) %>%
  mutate(n_screened_hazard=1-(mortality+total_incidence/cancer_rate_scale), #need to adjust for prevalence round?
         n_screened_ratio=cumprod(n_screened_hazard)/n_screened_hazard, #screening happens at start of year
         abs_screened_by_age=n_screened_ratio*n_analysis,
         weight=abs_screened_by_age/n_analysis) %>%  #fraction remaining at each year
  ungroup()


weight_screened_by_scenario<-cumulative_screened %>% 
  select(tAge,scenario,perc_skip,model_type,abs_screened_by_age,weight)


# Apply weights to inc and mort -------------------------------------------
weighted_inc_mort_by_cancer = incidence_by_cancer %>%
  left_join(weight_screened_by_scenario, by = c("tAge","scenario","perc_skip","model_type"))  %>%
  left_join(mortality_reduction_by_cancer, by = SS_GROUP_AGE_BY_CANCER) %>%
  mutate(weighted_cfdna = cfdna * weight) %>%
  mutate(weighted_soc = soc * weight) %>%
  mutate(weighted_original_deaths = original_deaths * weight) %>%
  mutate(weighted_shifted_deaths = shifted_deaths * weight)

weighted_inc_mort = weighted_inc_mort_by_cancer %>% #aggregate up to all cancers
  group_by_at(SURV_GROUP_AGE) %>%
  summarise(across(-NCRAS_Draw, sum),.groups = "drop")

w_mort_by_cancer = weighted_inc_mort_by_cancer %>%  
  group_by_at(SURV_GROUP_BY_CANCER) %>%
  summarize(
    screen_detected = sum(weighted_cfdna)/sum(weight), #renamed from cfdna_incidence
    SoC_detected = sum(weighted_soc)/sum(weight), #renamed from soc_incidence
    mortality_no_screening = sum(weighted_original_deaths)/sum(weight), #renamed from original_deaths
    mortality_screening = sum(weighted_shifted_deaths)/sum(weight), #renamed from shifted_deaths
    screen_detected_prev = sum(cfdna_prevalence)/sum(weight), #as above for the renaming conventions
    SoC_detected_prev = sum(soc_prevalence)/sum(weight),
    mortality_no_screening_prev = sum(original_deaths_prevalence)/sum(weight),
    mortality_screening_prev = sum(shifted_deaths_prevalence)/sum(weight)
  ) %>%
  mutate(mortality_difference = mortality_no_screening - mortality_screening) %>%
  mutate(mortality_difference_prev = mortality_no_screening_prev - mortality_screening_prev) %>%
  ungroup() 

w_mort_agg = w_mort_by_cancer %>%  #aggregate up to all cancers
  group_by_at(SURV_GROUP) %>%
  summarise(across(-NCRAS_Draw, sum),.groups = "drop")
  

# For diagnostic modelling -----------------------------------------------------

result_withMCED <- w_mort_by_cancer %>%
  filter(local_haz_ratio == 1,
         scenario == "old_4",
         perc_skip == 0,
         model_type == "main") %>%
  select(NCRAS_Draw,screen_detected_prev) %>%
  mutate(rate_1M = screen_detected_prev * 10)


# Stage shift results -----------------------------------------------------

stage_shift_by_cancer <- interception_stage_shift_ALL_w %>%
  filter(clinical < 5 & prequel < 5) %>% #Remove those not staged
  mutate(
    early_stage = ifelse(prequel < 3, 1, 0) * caught,#incidence rounds
    late_stage = ifelse(prequel > 2, 1, 0) * caught,
    shifted_early = ifelse(prequel < 3 & clinical > 2, 1, 0) * caught,
    shifted_any = ifelse(prequel < clinical, 1, 0) * caught,
    early_stage_prev = ifelse(prequel < 3, 1, 0) * prevalence_caught, #prevalence rounds
    late_stage_prev = ifelse(prequel > 2, 1, 0) * prevalence_caught,
    shifted_early_prev = ifelse(prequel < 3 & clinical > 2, 1, 0) * prevalence_caught,
    shifted_any_prev = ifelse(prequel < clinical, 1, 0) * prevalence_caught,
    initial_late = ifelse(clinical > 2, 1, 0) * caught, #without MCED screening
    initial_late_prev = ifelse(clinical > 2, 1, 0) * prevalence_caught #without MCED screening, prev
  ) %>%
  group_by_at(SS_GROUP_AGE_BY_CANCER) %>%
  summarize(
    early_stage = sum(early_stage),
    late_stage = sum(late_stage),
    shifted_early = sum(shifted_early),
    shifted_any = sum(shifted_any),
    early_stage_prev = sum(early_stage_prev),
    late_stage_prev = sum(late_stage_prev),
    shifted_early_prev = sum(shifted_early_prev),
    shifted_any_prev = sum(shifted_any_prev),
    shifted_percent = sum(shifted_early) / sum(initial_late) * 100,
    shifted_percent_prev = sum(shifted_early_prev) / sum(initial_late_prev) * 100,
    initial_late = sum(initial_late),
    initial_late_prev = sum(initial_late_prev)
  ) %>% #percentage of current late stage ca that are shifted to early
  ungroup() %>%
  mutate(
    early_percent = early_stage / (early_stage + late_stage) * 100,
    early_percent_prev = early_stage_prev / (early_stage_prev + late_stage_prev) * 100
  )

#stage shift aggregated across types
stage_shift_agg = stage_shift_by_cancer %>%  #aggregate up to all cancers
  group_by_at(SS_GROUP_AGE) %>%
  summarise(across(-NCRAS_Draw, sum),.groups = "drop")


# Apply weights to stage shift results ------------------------------------
#join weights to stage shift, by cancer
weighted_stage_shift_by_cancer <- stage_shift_by_cancer %>%
  left_join(weight_screened_by_scenario, by = c("tAge","scenario","perc_skip","model_type"))  %>%
  mutate(
    weighted_early_stage = early_stage * weight,
    weighted_late_stage = late_stage * weight,
    weighted_shifted_early = shifted_early * weight,
    weighted_shifted_any = shifted_any * weight,
    weighted_shifted_percent = shifted_percent * weight,
    weighted_early_percent = early_percent * weight,
    weighted_initial_late = initial_late * weight
  )

#aggregate across cancers
weighted_stage_shift_agg = weighted_stage_shift_by_cancer %>%
  group_by_at(SS_GROUP_AGE) %>%
  summarise(across(-NCRAS_Draw, sum),.groups = "drop")

#aggregate across ages
w_SS_by_cancer <- weighted_stage_shift_by_cancer %>%
  group_by_at(SS_GROUP_BY_CANCER) %>%
  summarize(
    early_stage = sum(weighted_early_stage)/sum(weight),
    late_stage = sum(weighted_late_stage)/sum(weight),
    shifted_early = sum(weighted_shifted_early)/sum(weight),
    shifted_any = sum(weighted_shifted_any)/sum(weight),
    early_stage_prev = sum(early_stage_prev)/sum(weight),
    late_stage_prev = sum(late_stage_prev)/sum(weight),
    shifted_early_prev = sum(shifted_early_prev)/sum(weight),
    shifted_any_prev = sum(shifted_any_prev)/sum(weight),
    shifted_percent = sum(weighted_shifted_percent)/sum(weight),
    shifted_percent_prev = sum(shifted_percent_prev)/sum(weight),
    early_percent = sum(weighted_early_percent)/sum(weight),
    early_percent_prev = sum(early_percent_prev)/sum(weight),
    initial_late = sum(weighted_initial_late)/sum(weight),
    initial_late_prev = sum(initial_late_prev)/sum(weight)
  ) %>%
  ungroup()

#aggregate across ages + cancer types
w_SS_agg = w_SS_by_cancer %>%
  group_by_at(SS_GROUP) %>%
  summarise(across(-NCRAS_Draw, sum),.groups = "drop")

# National Screening Programme results ------------------------------------

#birth cohort results 
#prevalence round at age 50 + sum of all the incidence rounds from 51

#remove the incidence round at age 50, so as not to double count it
weighted_inc_mort_no50 = weighted_inc_mort %>%
  mutate(weighted_cfdna = case_when(tAge == 50 ~ 0, 
          TRUE ~ weighted_cfdna)) %>%
  mutate(weighted_soc = case_when(tAge == 50 ~ 0, 
          TRUE ~ weighted_soc)) %>%
  mutate(weighted_original_deaths = case_when(tAge == 50 ~ 0, 
          TRUE ~ weighted_original_deaths)) %>%
  mutate(weighted_shifted_deaths = case_when(tAge == 50 ~ 0, 
          TRUE ~ weighted_shifted_deaths))

np_SURV_res <-  weighted_inc_mort_no50 %>% 
  arrange(tAge) %>%
  group_by_at(SURV_GROUP) %>% 
  summarize(cfdna_incidence_SA = first(cfdna_prevalence), #to take the prevalence screen at age 50
            soc_incidence_SA = first(soc_prevalence), #START AGE - prevalence screen at age 50
            original_deaths_SA = first(original_deaths_prevalence),
            shifted_deaths_SA = first(shifted_deaths_prevalence),  
            cfdna_incidence_inc_all = sum(weighted_cfdna), #to sum all incident rounds of screening age 51-79
            soc_incidence_inc_all = sum(weighted_soc),
            original_deaths_inc_all = sum(weighted_original_deaths),
            shifted_deaths_inc_all = sum(weighted_shifted_deaths)) %>%
  mutate(screen_detected = cfdna_incidence_inc_all + cfdna_incidence_SA, 
       SoC_detected = soc_incidence_inc_all + soc_incidence_SA, 
       mortality_no_screening = original_deaths_inc_all + original_deaths_SA, 
       mortality_screening = shifted_deaths_inc_all + shifted_deaths_SA, 
       mortality_difference = mortality_no_screening - mortality_screening) %>% 
  ungroup()

#remove the incidence round at age 50, so as not to double count it
weighted_stage_shift_agg_no50 = weighted_stage_shift_agg %>%
  mutate(weighted_early_stage = case_when(tAge == 50 ~ 0, 
              TRUE ~ weighted_early_stage)) %>%
  mutate(weighted_late_stage = case_when(tAge == 50 ~ 0, 
              TRUE ~ weighted_late_stage)) %>%
  mutate(weighted_shifted_early = case_when(tAge == 50 ~ 0, 
              TRUE ~ weighted_shifted_early)) %>%
  mutate(weighted_shifted_any = case_when(tAge == 50 ~ 0, 
              TRUE ~ weighted_shifted_any))

np_SS_res = weighted_stage_shift_agg_no50 %>%
  group_by_at(SS_GROUP) %>% 
  summarize(early_stage_SA = first(early_stage_prev), #to take the prevalence screen at age 50 
    late_stage_SA = first(late_stage_prev), #START AGE 50 for the prev screen
    shifted_early_SA = first(shifted_early_prev),
    shifted_any_SA = first(shifted_any_prev),
    early_stage_inc_all=sum(weighted_early_stage), #to sum all incident rounds of screening age 51-79
    late_stage_inc_all =sum(weighted_late_stage),
    shifted_early_inc_all=sum(weighted_shifted_early),
    shifted_any_inc_all=sum(weighted_shifted_any)) %>%
    mutate(early_stage = early_stage_inc_all + early_stage_SA,
           late_stage = late_stage_inc_all + late_stage_SA,
           shifted_early = shifted_early_inc_all + shifted_early_SA,
           shifted_any = shifted_any_inc_all + shifted_any_SA) %>%
    ungroup()
  