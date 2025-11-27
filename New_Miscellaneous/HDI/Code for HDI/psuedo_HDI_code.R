################################################################################
#______________________________________________________________________________#
#### Example code for HDI costing study
#______________________________________________________________________________#
################################################################################

library(tidyverse)
library(ggpubr)
library(msm)
library(lubridate, warn.conflicts = FALSE)
library(janitor)
library(aod)
devtools::install_version("LDdiag","0.1")
library(nortest)
library(lmtest)

#' _____________________________________________________________________________
#' _____ Assumed variables in dataframe

# "patientid" 
# "site"
# "site_coded"
# 
# ## y variables
# # total cost
# "totalcost"
# # annual cost
# "ac_diag"
# "ac_treat_0_1year"
# "ac_treat_1_2year"
# "ac_treat_2_3year"
# "ac_treat_3_4year"
# "ac_treat_4_5year"
# "ac_treat_5year"
# # phase of care cost
# "diag"
# "eol"
# "treat_0_1year"
# "treat_1_2year"
# "treat_2_3year"
# "treat_3_4year"
# "treat_4_5year"
# "treat_5year"

# ## x variables
# "stage_best" yes
# "age" yes
# "age_group" 
# "sex" yes
# "eth_group"  yes
# "canalliance_2021_name"
# "chrl_tot_78_06" yes
# "quintile_2015" yes
# "final_route" # Presumed final RtD
# "diagnosisyear" yes
# "other_site_after" ?
# "other_site_before"?
# "same_site_after" ?
# "same_site_before" ?
# "singletumour" ?


#' _____________________________________________________________________________
#' _____ Create simulated dataset using assumed variable names

set.seed(1)   
n_sim <- 50000

df_sim <- tibble(patientid = seq(1:n_sim),
                 site = sample(c("Colorectal", "Colorectal", "Pancreas", "Ovary"), n_sim, replace = TRUE),
                 stage_best = sample(c("I", "II", "III", "IV", "missing"), n_sim, replace = TRUE),
                 diag_date = sample(seq(ymd('2015/01/01'), ymd('2017/01/01'), by="day"), n_sim, replace = TRUE),
                 diagnosisyear = substr(diag_date, start = 1, stop = 4),
                 num_events = sample(seq(1:10), n_sim, replace = TRUE),
                 death_status = rbinom(n_sim, 1, .05),
                 age = sample(seq(from = 50, to = 79, by = 1), n_sim, replace = TRUE), # equal chance for simplicity
                 age_group = case_when(between(age, 50, 59) ~ "50-59",
                                     between(age, 60, 69) ~ "60-69",
                                     between(age, 70, 79) ~ "70-79",
                                     .default = NA),
                 sex = sample(c("female", "male"), n_sim, replace = TRUE), # equal chance for simplicity
                 eth_group = sample(c("Asian or Asian British", "Black, Black British, Caribbean or African",
                                      "Mixed or multiple ethnic groups", "White"),
                                    n_sim, replace = TRUE, prob = c(0.05, 0.05, 0.05, 0.85)), 
                 quintile_2015 = sample(seq(1:5), n_sim, replace = TRUE), 
                 CCI_comorbid = sample(seq(0:4), n_sim, replace = TRUE, prob = c (0.2, 0.2, 0.2, 0.2, 0.2))) %>%
    mutate(death_date = case_when(death_status == 1 ~ as_date(sample(diag_date:ymd('2020/01/01'), n(), replace = TRUE)),
                                  .default = NA)) %>%
    mutate(death_date = case_when(death_date == diag_date ~ NA,
                                  .default = death_date)) %>%
    uncount(num_events) %>%
    mutate(date_fu6monthprior = diag_date %m-% months(6)) %>%
    mutate(event_date = as_date(date_fu6monthprior + rlnorm(n(), 6.5, 1))) %>% 
    # filter out silly dates
    filter(death_status == 0 | (death_status ==1 & death_date > event_date)) %>%
    filter(ymd('2020/01/01') > event_date) %>%
    filter(date_fu6monthprior < event_date) %>%
    mutate(event_cost = case_when(stage_best == "I" ~ rlnorm(n(), 9, 1),
                                  stage_best == "II" ~ rlnorm(n(), 9.3, 1),
                                  stage_best == "III" ~ rlnorm(n(), 9.7, 1),
                                  stage_best == "V" ~ rlnorm(n(), 10, 1),
                                  stage_best == "missing" ~ rlnorm(n(), 9.5, 1))) %>%
    group_by(patientid) %>%
    arrange(event_date) %>%
    mutate(event_num = row_number()) %>%
    ungroup() %>%
    arrange(patientid, event_num) %>%
    mutate(date_fu1y = diag_date %m+% years(1),
           date_fu2y = diag_date %m+% years(2),
           date_fu3y = diag_date %m+% years(3),
           date_fu4y = diag_date %m+% years(4),
           date_fu5y = diag_date %m+% years(5),
           date_fu6y = diag_date %m+% years(6)) %>%
    # phase of care costs
    mutate(care_phase = case_when(event_date <= diag_date ~ "diag" ,
                                  !is.na(death_date) & difftime(death_date, event_date, units = c("days")) <= 365.25 ~ "eol",
                                  event_date <= date_fu1y ~ "treat_0_1year",
                                  event_date <= date_fu2y ~ "treat_1_2year",
                                  event_date <= date_fu3y ~ "treat_2_3year",
                                  event_date <= date_fu4y ~ "treat_3_4year",
                                  event_date <= date_fu5y ~ "treat_4_5year",
                                  event_date <= date_fu6y ~ "treat_5year",
                                  .default = NA)) %>%
    mutate(annual_period = case_when(event_date <= diag_date ~ "ac_diag" ,
                                     event_date <= date_fu1y ~ "ac_treat_0_1year",
                                     event_date <= date_fu2y ~ "ac_treat_1_2year",
                                     event_date <= date_fu3y ~ "ac_treat_2_3year",
                                     event_date <= date_fu4y ~ "ac_treat_3_4year",
                                     event_date <= date_fu5y ~ "ac_treat_4_5year",
                                     event_date <= date_fu6y ~ "ac_treat_5year",
                                     .default = NA)) %>%
    pivot_longer(care_phase:annual_period, names_to = "cost_type_outcome", values_to = "cost_outcome") %>%
    select(-event_date, -event_num, -cost_type_outcome) %>%
    group_by(patientid, cost_outcome) %>%
    mutate(sum_costs_outcome = sum(event_cost, na.rm = TRUE)) %>%
    select(-event_cost) %>%
    distinct() %>%
    mutate(cost_outcome = as.factor(cost_outcome))

#' _____________________________________________________________________________
#' _____ Regression analysis - OLS Model tests - Residual plot, shapiro-wilks, breush-pagan

# Create rhs variables
df_sim_prepped <- df_sim %>% 
    fastDummies::dummy_cols(select_columns = c("stage_best"), remove_first_dummy = TRUE) %>%
    fastDummies::dummy_cols(select_columns = c("sex"), remove_first_dummy = TRUE) %>%
    fastDummies::dummy_cols(select_columns = c("eth_group"), remove_first_dummy = TRUE) %>%
    fastDummies::dummy_cols(select_columns = c("quintile_2015"), remove_first_dummy = TRUE) %>%
    fastDummies::dummy_cols(select_columns = c("CCI_comorbid"), remove_first_dummy = TRUE) %>%
    janitor::clean_names() %>%
    mutate(sum_costs_outcome = case_when(sum_costs_outcome == 0 ~ 1,
                                        .default = sum_costs_outcome)) ## Just for the gamma model

#### Run OLS regressions

# This function takes the patient data frame and runs the OLS regression: cost and cost squared as LHS variables
# It output the regression results as well as results from a number of tests looking at the assumptions underpinning OLS
f_ols_regressions <- function(df_patient = df_sim_prepped){
    
    v_rhs_formula = paste(" ~ age + I(age^2) +",
                          paste(df_patient %>%
                                    select(stage_best_ii:cci_comorbid_5) %>%
                                    colnames(), collapse = " + "))
    
    l_lhs_formula <- list(
        ols = "sum_costs_outcome",
        ols_sqrt = "sqrt(sum_costs_outcome)",
        ols_log = "log(sum_costs_outcome)"
    )
    
    l_res_ols <- l_lhs_formula %>%
        purrr::set_names() %>% 
        map(.f = ~ lm(                       
            formula = as.formula(paste(.x, v_rhs_formula)),           
            data = df_patient
        ))
    
    l_res_resid_plots<- l_res_ols %>%
        map(.f = ~ ggplot(.x ,aes(x = .fitted, y = .resid)) + geom_point()
        )
    
    l_res_adtest <- l_res_ols %>%
        map(.f = ~ nortest::ad.test(residuals(.x))
        )
    
    l_res_bptest <- l_res_ols %>%
        map(.f = ~ lmtest::bptest(.x)
        )
    
    return(list(l_res_ols = l_res_ols,
                l_res_resid_plots = l_res_resid_plots,
                l_res_adtest = l_res_adtest, 
                l_res_bptest = l_res_bptest))
    
}

# This function wraps around the other function. It first splits the input dataframe by cancer site and cost outcomes then 
# feeds it into the previous function
fun_bysite_byoutcome_olstests <- function(df_patients = df_sim_prepped){
    
    res_bysite_byoutcome_olstests <- map(.x = split(df_patients, list(df_patients$site, df_patients$cost_outcome)), .f = ~ f_ols_regressions(.x))
    return(res_bysite_byoutcome_olstests = res_bysite_byoutcome_olstests)
    
}

res_bysite_byoutcome_olstests <- fun_bysite_byoutcome_olstests(df_patients = df_sim_prepped)

# Get residual plots
l_OLS_resid_plots <- purrr::map(res_bysite_byoutcome_olstests, 2)


pdf("plots.pdf")
for (i in 1:length(l_OLS_resid_plots)) {
    print(l_OLS_resid_plots[[i]])
}
dev.off()

# Get AD test results, p-value <0.05 indicates residuals are non-normal
df_OLS_AD_test_results <- purrr::map(res_bysite_byoutcome_olstests, 3) %>%
    map(., unlist) %>%
    bind_rows(. , .id = "Cancer_site_cost_outcome") %>%
    select(Cancer_site_cost_outcome, contains("statistic") ,contains("p.value")) %>%
    arrange(Cancer_site_cost_outcome)

# Get BP test results, p-value <0.05 indicates heteroskedasticity are non-normal
df_OLS_BP_test_results <- purrr::map(res_bysite_byoutcome_olstests, 4) %>%
    map(., unlist) %>%
    bind_rows(. , .id = "Cancer_site_cost_outcome") %>%
    select(Cancer_site_cost_outcome, contains("statistic") ,contains("p.value")) %>%
    arrange(Cancer_site_cost_outcome)


#' _____________________________________________________________________________
#' _____ Regression analysis - GLM Model specification tests - Modified park test for family

#' For the GLM models, first the appropriateness of the variance functions will be assessed using a modified version of the Park test 
#' for heterogeneity. Secondly, for the indicated variance function, the best fitting link function will be assessed using the Pregibon link test.
#' goodness-of-fit for all models will be compared using the Aike and Bayesian information criteria. 


# The below function looks at different family function for the GLM model and picks the best fitting one.
# Test of mean-varience relationship (family function) using modified Park test as given in Manning 2001
# - Run glm for each candidate family with a link you are interested in, doesn't matter which - gamma
# - Predict y (yhat) and residuals (res)
# - Take log of yhat (lnyhat) and square residuals (res2)
# - Regress (GLM  with a log link and gamma family (mean prop to var)) residuals against lnyhat
# ecommended family derived from coefficient for lnyhat:
# – If coefficient ~=0, Gaussian
# – If coefficient ~=1, Poisson
# – If coefficient ~=2, Gamma
# – If coefficient ~=3, Inverse Gaussian or Wald
# - Use Chi sq to test

fun_modparktestv1 <- function(df_patients, out = "test") {
   
    v_formula <- paste("sum_costs_outcome ~ age + I(age^2) +",
                           paste(df_patients %>%
                                     select(stage_best_ii:cci_comorbid_5) %>%
                                     colnames(), collapse = " + "))
    
    # Note functionality to run a variety of different family/link combinations. But only one need for test.
    # poisson(link = "log"), Gamma(link = "log"), inverse.gaussian(link = "log")                    
    l_family_link <- list(Gamma(link = "log"))
    
    l_res_glm_family <- l_family_link %>% # iterate through each family
        map(.f = ~glm(                       # pass the family one-by-one to glm()
            formula = as.formula(v_formula),      
            family = .x,           
            data = df_patients))
    
    # create list of tibbles containing log predicted values and the squared residual values
    l_res_pred_logyhat_sqrres <- map(.x = l_res_glm_family ,
                                     .f = ~ tibble(pred = log(predict(object = .x)), res = (.x$residuals)^2)) 
   
    # map above list and regress
    l_res_glm_logyhat_sqrres <- map(.x = l_res_pred_logyhat_sqrres ,
                                    .f = ~glm(
        formula = as.formula("res ~ pred"),      
        family = Gamma(link = "log"),           
        data = .x,
        start = rep(2, 2))) 
    
    # map list of above models to the coeftest 
    l_res_mod_park_wald <- map(.x = l_res_glm_logyhat_sqrres,
                               .f = ~ coeftest(.x))
    
    
    if(out == "test"){
        
        return(l_res_mod_park_wald)
        
    } else {
        
        return(l_res_glm_family)
        
    }
}

fun_modparktest <- function(df_patients) {
    
    v_formula <- paste("sum_costs_outcome ~ age + I(age^2) +",
                       paste(df_patients %>%
                                 select(stage_best_ii:cci_comorbid_5) %>%
                                 colnames(), collapse = " + "))
    
    res_glm_family <- glm(
        formula = as.formula(v_formula),      
        family = poisson(link = "log"),           
        data = df_patients)
    
    # create list of tibbles containing log predicted values and the squared residual values
    res_pred_logyhat_sqrres <- tibble(y = df_patients$sum_costs_outcome, 
                                      y_hat = fitted(object = res_glm_family),
                                      res = y - y_hat)
    
    # Use the above dataframe of predicted cost (log) and residuals (squared)
    res_glm_logyhat_sqrres <- glm(formula = I(res^2) ~ I(log(y_hat)),      
                                        family = Gamma(link = "log"),           
                                        data = res_pred_logyhat_sqrres %>% filter(res != 0),
                                        start = rep(2, 2))
    
    # map list of above models to the coeftest 
    res_mod_park_wald <- coeftest(res_glm_logyhat_sqrres)
    
    return(res_mod_park_wald)

}


# This functions wraps around the first and performs it by cancer site
fun_bysite_byoutcome_modparktest <- function(df_patients = df_sim_prepped){
    
    res_modpark <- map(.x = split(df_patients, list(df_patients$site, df_patients$cost_outcome)),
                       .f = safely(~ fun_modparktest(.x)))
    return(res_modpark = res_modpark)
    
}

res_bysite_modparktest <- fun_bysite_byoutcome_modparktest(df_patients = df_sim_prepped)

df_GLM_mod_park <- purrr::map(res_bysite_modparktest, pluck, 1) %>%
    purrr::map(., 2) %>%
    bind_rows(., .id = "Cancer_outcome") %>%
    rownames_to_column() %>% 
    pivot_longer(!rowname, names_to = "characteristics", values_to = "Slope") %>% 
    select(-rowname) %>%
    mutate(Family = case_when(round(Slope, 0) == 0 ~ "Gaussian",
                              round(Slope, 0) == 1 ~ "Poisson",
                              round(Slope, 0) == 2 ~ "Gamma",
                              round(Slope, 0) == 3 ~ "Inverse Gaussian",
                              .default = NA))

#______________________________________________________________________________#
## Regression analysis - GLM Model specification tests - Pregibon test for link

# The below function chooses the best link given the indicated family from the prior function
# Note only certain links can be used with specific families
# Change family and links as required in the function 
# three argurements need in this function for sum reason do to the nested maps
# Links by family
# 0: Gaussian - c("inverse", "identity", "log")
# 1: Poisson - c("log", "identity",  "sqrt")
# 2: Gamma - c("inverse", "identity", "log")
# 3: Iinverse. - c("1/mu^2", "inverse", "identity", "log")

f_pregibontest <- function(df_patients, family = "inverse.gaussian"){
    
    v_formula <- paste("sum_costs_outcome ~ age + I(age^2) +",
                       paste(df_patients %>%
                                 select(stage_best_ii:cci_comorbid_5) %>%
                                 colnames(), collapse = " + "))
    
    if(family == "gaussian"){
        
        v_links <- c("inverse", "identity", "log")
        l_res_glm_pregibon <- map(.x = v_links,
                                  .f = safely(~ LDdiag::pregibon.glm(glm( formula = as.formula(v_formula),
                                                                   family = gaussian(link = .x), data = df_patients))))
        
        
    } else if(family == "poisson"){
        
        v_links <- c("log", "identity",  "sqrt")
        l_res_glm_pregibon <- map(.x = v_links ,
                                  .f = safely(~ LDdiag::pregibon.glm(glm( formula = as.formula(v_formula),
                                                                   family = poisson(link = .x), data = df_patients))))
        
        
    } else if(family == "Gamma"){
        
        v_links <- c("inverse", "identity", "log")
        l_res_glm_pregibon <- map(.x = v_links ,
                                  .f = safely(~ LDdiag::pregibon.glm(glm( formula = as.formula(v_formula),
                                                                   family = Gamma(link = .x), data = df_patients))))
        
        
    } else if(family == "inverse.gaussian"){
        
        v_links <- c("1/mu^2", "inverse", "log", "identity")
        l_res_glm_pregibon <- map(.x = v_links ,
                                  .f = safely(~ LDdiag::pregibon.glm(glm(formula = as.formula(v_formula),
                                                                  family = inverse.gaussian(link = .x), data = df_patients))))
        
        
    } else {
        
        print("please specify family")
    }


    return(l_res_glm_pregibon)
}

# again a wrapper function to do by site
fun_bysite_byoutcome_pregibontest <- function(df_patients2, fam){
    
    l_res_pregibon <- map(.x = split(df_patients2 , list(df_patients2$site, df_patients2$cost_outcome)), 
                          .f = ~ f_pregibontest(df_patients = .x, family =  fam))
    return(l_res_pregibon = l_res_pregibon)
    
}


v_fam <- "inverse.gaussian"
res_bysite_pregibontest <- fun_bysite_byoutcome_pregibontest(df_patients2 = df_sim_prepped, fam = v_fam)

df_GLM_pregibon <- purrr::map(res_bysite_pregibontest, ~ map(.x, pluck, 1)) %>%
    imap(., ~ set_names(.x, paste(.y, seq_along(.x), sep = "."))) %>% # set names of inner lists    
    purrr::map(., ~ discard(.x, .p = is.null)) %>%
    purrr::map(.,  ~ map(.x, pluck, 3)) %>%
    map(., unlist) %>%
    purrr::flatten(.) %>%
    bind_rows(. , .id = "Cancer_site_pregibon_pvalue") %>%
    rownames_to_column() %>% 
    pivot_longer(!rowname, names_to = "Cancer_site_pregibon_link", values_to = "p_value") %>% 
    select(-rowname) %>%
    separate_wider_delim(cols = "Cancer_site_pregibon_link", delim = ".", names_sep = "",
                         names = c("Cancer type", "cost outcome", "link")) %>%
    mutate(result = case_when(p_value <= 0.05 ~ "Passes this test",
                              .default = "DOES NOT PASS this test"))
    
    
#______________________________________________________________________________#
## Regression analysis - Aike and Bayesian information criteria

# Below function looks at AIC and BIC for all the different GLM specifications
f_aicbic <- function(df_patients,
                     gaussian_links = c("inverse", "identity", "log"),
                     poisson_links = c("log", "identity" ),
                     Gamma_links = c("inverse", "identity", "log"),
                     inverse.gaussian_links = c("1/mu^2", "inverse", "log", "identity")
                     ){
                         
    v_formula = paste("sum_costs_outcome ~ age + I(age^2) +",
                          paste(df_patients %>%
                                    select(stage_best_ii:cci_comorbid_5) %>%
                                    colnames(), collapse = " + "))
    
    l_gauss_glm <- poisson_links %>% 
        purrr::set_names(paste0, "_gauss") %>% 
        map(.f = safely(~ broom::glance(glm(                       
            formula = as.formula(v_formula),      
            family = gaussian(link = .x),           
            data = df_patients
        ))))
    
    
    l_poisson_glm <- poisson_links %>% 
        purrr::set_names(paste0, "_poisson") %>% 
        map(.f = safely(~ broom::glance(glm(                       
            formula = as.formula(v_formula),      
            family = poisson(link = .x),           
            data = df_patients
        ))))
    
    l_Gamma_glm <- Gamma_links %>% 
        purrr::set_names(paste0, "_Gamma") %>% 
        map(.f = safely(~ broom::glance(glm(                       
            formula = as.formula(v_formula),      
            family = Gamma(link = .x),           
            data = df_patients
        ))))
    
    l_invgauss_glm <- inverse.gaussian_links %>%
        purrr::set_names(paste0, "_invgauss") %>%
        map(.f = safely(~ broom::glance(glm(
            formula = as.formula(v_formula),
            family = inverse.gaussian(link = .x),
            data = df_patients
        ))))

    l_res_glm_aicbic <- list(l_gauss_glm = l_gauss_glm, l_poisson_glm = l_poisson_glm, l_Gamma_glm = l_Gamma_glm, l_invgauss_glm = l_invgauss_glm)
                             
    
    return(l_res_glm_aicbic)
}

# again a wrapper by site and outcome
fun_bysite_byoutcome_aicbic <- function(df_patients = df_sim_prepped){
    
    l_res_aicbic <- map(.x = split(df_patients, list(df_patients$site, df_patients$cost_outcome)), .f = ~ f_aicbic(.x))
    return(l_res_aicbic = l_res_aicbic)
    
}

res_bysite_byoutcome_aicbic <- fun_bysite_byoutcome_aicbic(df_patients = df_sim_prepped)

df_GLM_AIC <- purrr::map(res_bysite_byoutcome_aicbic, ~ purrr::list_flatten(.x, name_spec = "{outer}.{inner}")) %>%  
    purrr::map(., ~ map(.x, pluck, 1)) %>%
    purrr::map(., ~ map(.x, pluck, 4)) %>% ## pluck the AIC
    purrr::list_flatten(., name_spec = "{outer}.{inner}") %>%
    bind_rows(. , .id = "Cancer_site.cost_outcome_family.link") %>%
    rownames_to_column() %>% 
    pivot_longer(!rowname, names_to = "Cancer_site.cost_outcome_family.link", values_to = "AIC") %>% 
    separate_wider_delim(cols = "Cancer_site.cost_outcome_family.link", delim = ".",
                         names = c("Cancer type", "cost outcome", "family", "link")) %>%
    slice_min(AIC, by = c(`Cancer type`, `cost outcome`))
    
    