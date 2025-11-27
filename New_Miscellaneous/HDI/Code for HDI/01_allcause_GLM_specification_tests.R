################################################################################
#______________________________________________________________________________#
#' _____ 01 All cause cost regression specification tests
#______________________________________________________________________________#
################################################################################

#' The below code runs specification tests for ordinary least squares and general
#' linear model regressions in an attempts to identify the most appropriate model
#' for the data. 

#' General, the code is grouped into different functions for the
#' different tests. First there is a function which runs the tests on a given dataset
#' and then a wrapper function which splits the input dataframe containing all costs
#' by cancer site and cost outcome type and then inputs these dataframes into the first function.

#' Purrr functions have been used extensively to iterate over lists (stemming from the the spiting),
#' in particular the map function and a variant, safely(). The later allows map to continue
#' Run over a list even after an error has occurred. It also records the error.
#' To see how a function works during QA, the internals of the first functions can be taken out and a 
#' "split" dataframe can be made simply by filtering by cancer type and cost outcome


#' _____________________________________________________________________________
#' Packages (may not all be still in use)
library(tidyverse)
library(ggpubr)
library(msm)
library(lubridate, warn.conflicts = FALSE)
library(janitor)
library(aod)
devtools::install_version("LDdiag","0.1")
library(nortest)
library(lmtest)
library(openxlsx, quietly = TRUE)
library(lubridate)
library(marginaleffects)

#' _____________________________________________________________________________
#' _____ 1. Create Synthetic dataset for further use

set.seed(1)   
n_sim <- 50000

df_sim <- tibble(patientid = seq(1:n_sim),
                 site = sample(c("Colorectal", "Colorectal", "Pancreas", "Ovary"), n_sim, replace = TRUE),
                 stage_best = sample(c("1", "2", "3", "4", "missing"), n_sim, replace = TRUE),
                 diag_date = sample(seq(ymd('2014/01/01'), ymd('2017/12/31'), by="day"), n_sim, replace = TRUE),
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
                 chrl_tot_78_06 = sample(seq(0:4), n_sim, replace = TRUE, prob = c (0.2, 0.2, 0.2, 0.2, 0.2))) %>%
    mutate(death_date = case_when(death_status == 1 ~ as_date(sample(diag_date:ymd('2019/12/31'), n(), replace = TRUE)),
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
#' _____ 2. Prepare for regression analysis by creating dummies for categoric variables
#' and putting in the correct reference group

#' HDI code
# # Remove non-necessary dataframes to help with RAM
# rm(cohort, cohort_clean, cohort_total_costs, test_dates)
# 
# # set pseudo tumourid variable
# cohort_costs <- cohort_costs %>% mutate(ID=row_number())
# 
# # keep only variables to be used in regression analysis 
# cohort_costs_reg <- select(cohort_costs, ID, diagnosisyear, stage_best, age, age_group, sex, ethnicity, eth_group, site, site_coded, canalliance_2021_name,
#                            final_route, chrl_tot_78_06, quintile_2015, singletumour, 
#                            totalcost, ac_diag, ac_treat_0_1year, ac_treat_1_2year, ac_treat_2_3year, ac_treat_3_4year, ac_treat_4_5year, ac_treat_5year, 
#                            diag, eol, treat_0_1year, treat_1_2year, treat_2_3year, treat_3_4year, treat_4_5year, treat_5year)
# 
# # Transform into long format such that there is a row for each type of cost outcome for each patient
# costs_long <- cohort_costs_reg %>% pivot_longer(cols = totalcost:treat_5year, 
#                                                    names_to = "cost_outcome",
#                                                    values_to = "sum_costs_outcome")

# Put data set in format for regression models by creating dummy columns for categoric variables.
# check what the reference categories are - may need to relevel the factors. We want the references to be:
# Stage I, Female, White, Quitile 5 (least deprived), CCI 0

df_sim_prepped <- df_sim %>% 
    mutate(eth_group = factor(eth_group, levels = c("White", "Asian or Asian British", "Black, Black British, Caribbean or African",
                                                    "Mixed or multiple ethnic groups"))) %>%
    fastDummies::dummy_cols(select_columns = c("stage_best"), remove_first_dummy = TRUE) %>%
    fastDummies::dummy_cols(select_columns = c("sex"), remove_first_dummy = TRUE) %>%
    fastDummies::dummy_cols(select_columns = c("eth_group"), remove_first_dummy = TRUE) %>%
    fastDummies::dummy_cols(select_columns = c("quintile_2015"), remove_first_dummy = TRUE) %>%
    fastDummies::dummy_cols(select_columns = c("chrl_tot_78_06"), remove_first_dummy = TRUE) %>%
    janitor::clean_names() %>%
    mutate(sum_costs_outcome = case_when(sum_costs_outcome == 0 ~ 1,
                                         .default = sum_costs_outcome))


#' _____________________________________________________________________________
#' _____ 3. OLS Model specification tests - Residual plot, shapiro-wilks, breush-pagan

#' The below functions performs specification tests on OLS based regression models. 
#' Three different transformations of the cost outcome are looked at:
#' Mon-transformed, square root transformed and log transformed  

#' The function takes the patient data frame and runs the OLS regressions for the three transformations.
#' It also plots the fitted verses the residuals (underpinning assumption that residual variation is normally distributed),
#' and performs an Anderson-Darling Normality Test and Breusch-Pagan Test for heteroskedasticity.
f_ols_regressions <- function(df_patient){
    
    #' Create string of right hand side formula
    v_rhs_formula <- paste(" ~ age + I(age^2) +",
                          paste(df_patient %>%
                                    select(stage_best_2:chrl_tot_78_06_4) %>%
                                    colnames(), collapse = " + "))

    #' Create list of strings of left hand side variables
    l_lhs_formula <- list(
        ols = "sum_costs_outcome",
        ols_sqrt = "sqrt(sum_costs_outcome)",
        ols_log = "log(sum_costs_outcome)"
    )
    
    #' Run OLS regression for each of the three left hand side variables
    l_res_ols <- l_lhs_formula %>%
        purrr::set_names() %>% 
        map(.f = ~ lm(                       
            formula = as.formula(paste(.x, v_rhs_formula)),           
            data = df_patient
        ))
    
    #' Run OLS regression for each of the 3 LHS variables
    #' Set plot title to LHS variable looked at
    l_res_resid_plots <- l_res_ols %>%
        map2(.x = ., .y = names(.), .f = ~ ggplot(.x ,aes(x = .fitted, y = .resid)) + 
                 geom_point() + ggtitle(.y)
        )
    
    #' Perform the AD test for each of the 3 LHS variables
    l_res_adtest <- l_res_ols %>%
        map(.f = ~ nortest::ad.test(residuals(.x))
        )
    
    #' Perform the BP test for each of the 3 LHS variables
    l_res_bptest <- l_res_ols %>%
        map(.f = ~ lmtest::bptest(.x)
        )
    
    #' Return list of results lists
    return(list(l_res_ols = l_res_ols,
                l_res_resid_plots = l_res_resid_plots,
                l_res_adtest = l_res_adtest, 
                l_res_bptest = l_res_bptest))
    
}

#' This function wraps around the other function. It first splits the input dataframe by cancer site and cost outcomes then 
#' feeds it into the previous function
fun_bysite_byoutcome_olstests <- function(df_patients = df_sim_prepped){
    
    res_bysite_byoutcome_olstests <- map(.x = split(df_patients, list(df_patients$site, df_patients$cost_outcome)),
                                         .f = ~ f_ols_regressions(.x))
    
    return(res_bysite_byoutcome_olstests = res_bysite_byoutcome_olstests)
    
}

#' Run the above two functions
res_bysite_byoutcome_olstests <- fun_bysite_byoutcome_olstests(df_patients = df_sim_prepped)

#' Get residual plots from results list and save
#' Easiest way to get all plots on one file. May want to specify a place for the pdf, 
#' will be saved in working directory otherwise
l_OLS_resid_plots <- purrr::map(res_bysite_byoutcome_olstests, 2)
pdf("plots.pdf")
for (i in 1:length(l_OLS_resid_plots)) {
    print(l_OLS_resid_plots[[i]])
}
dev.off()

#' Get AD test results, p-value <0.05 indicates residuals are non-normal
df_OLS_AD_test_results <- purrr::map(res_bysite_byoutcome_olstests, 3) %>%
    map(., unlist) %>%
    bind_rows(. , .id = "Cancer_site_cost_outcome") %>%
    select(Cancer_site_cost_outcome, contains("statistic") ,contains("p.value")) %>%
    arrange(Cancer_site_cost_outcome)

#' Get BP test results, p-value <0.05 indicates heteroskedasticity are non-normal
df_OLS_BP_test_results <- purrr::map(res_bysite_byoutcome_olstests, 4) %>%
    map(., unlist) %>%
    bind_rows(. , .id = "Cancer_site_cost_outcome") %>%
    select(Cancer_site_cost_outcome, contains("statistic") ,contains("p.value")) %>%
    arrange(Cancer_site_cost_outcome)

#' _____________________________________________________________________________
#' _____ 3. GLM Model specification tests 
#' Regression formula used in all the below GLM tests

v_formula <- paste("sum_costs_outcome ~ age + I(age^2) +",
                   paste(df_sim_prepped %>%
                             select(stage_best_2:chrl_tot_78_06_4) %>%
                             colnames(), collapse = " + "))

#' _____ 3a. Modified park test for family


#' For the GLM models, first the appropriateness of the variance functions will be assessed using a modified version of the Park test 
#' for heterogeneity (looks at mean-varience relationship). 
#' Secondly, for the indicated variance function, the best fitting link function will be assessed using the Pregibon link test.

#' The Modified park test originates from: Manning WG, Mullahy J. Estimating log models: to transform or not to transform? J Health Econ. 2001;20(4):461–94.
#' Code was adapted from: Zhou, J., Williams, C., Keng, M.J. et al. Estimating Costs Associated with Disease Model States Using Generalized Linear Models: A Tutorial. PharmacoEconomics 42, 261–273 (2024). https://doi.org/10.1007/s40273-023-01319-x
#' Other helpful links: http://econ.hunter.cuny.edu/parthadeb/wp-content/uploads/sites/4/2014/05/ASHEcon_LosAngeles_minicourse.pdf
#' https://github.com/cran/LDdiag/blob/master/man/park.Rd

#' The Modified park test can be summarised as follows
#' 1. Run glm for each candidate family with a link you are interested in, doesn't matter which - Gamma(link = "log")used below
#' 2. Predict y (yhat) and residuals (res)
#' 3. Take log of yhat (lnyhat) and square residuals (res^2)
#' 4. Regress (GLM  with a log link and gamma family (mean prop to var)) residuals against lnyhat
#' Recommended family derived from coefficient for lnyhat:
#' – If coefficient ~=0, Gaussian
#' – If coefficient ~=1, Poisson
#' – If coefficient ~=2, Gamma
#' – If coefficient ~=3, Inverse Gaussian or Wald

fun_modparktest <- function(df_patients) {
    
    #' Run GLM with arbitrary family and link (1.)
    res_glm_family <- glm(
        formula = as.formula(v_formula),      
        family = Gamma(link = "log"),           
        data = df_patients)
    
    #' create list of tibbles containing log predicted (fitted function needed here not predict) values and the squared residual values
    res_pred_logyhat_sqrres <- tibble(y = df_patients$sum_costs_outcome, 
                                      y_hat = fitted(object = res_glm_family),
                                      res = y - y_hat)
    
    #' Use the above dataframe of predicted cost (log) and residuals (squared)
    res_glm_logyhat_sqrres <- glm(formula = I(res^2) ~ I(log(y_hat)),      
                                  family = Gamma(link = "log"),           
                                  data = res_pred_logyhat_sqrres %>% filter(res != 0),
                                  start = rep(2, 2))
    
    #' map list of above models to the coeftest - get coefficient out below
    res_mod_park_wald <- coeftest(res_glm_logyhat_sqrres)
    
    return(res_mod_park_wald)
    
}


#' This functions wraps around the first and performs it by cancer site
fun_bysite_byoutcome_modparktest <- function(df_patients = df_sim_prepped){
    
    res_modpark <- map(.x = split(df_patients, list(df_patients$site, df_patients$cost_outcome)),
                       .f = safely(~ fun_modparktest(.x)))
    return(res_modpark = res_modpark)
    
}


res_bysite_modparktest <- fun_bysite_byoutcome_modparktest(df_patients = df_sim_prepped)

#' First get the results (rather than error log)
df_GLM_mod_park <- purrr::map(res_bysite_modparktest, pluck, 1) %>%
    #' extract slope coefficient
    purrr::map(., 2) %>%
    #' Reformat
    bind_rows(., .id = "Cancer_outcome") %>%
    rownames_to_column() %>% 
    pivot_longer(!rowname, names_to = "characteristics", values_to = "Slope") %>% 
    select(-rowname) %>%
    #' Which family inferred
    mutate(Family = case_when(round(Slope, 0) == 0 ~ "Gaussian",
                              round(Slope, 0) == 1 ~ "Poisson",
                              round(Slope, 0) == 2 ~ "Gamma",
                              round(Slope, 0) == 3 ~ "Inverse Gaussian",
                              .default = NA))

#' _____ 3b. Pregibon test for link

#' Pregibon, Daryl. “Goodness of Link Tests for Generalized Linear Models.” Journal of the Royal Statistical Society. Series C (Applied Statistics), vol. 29, no. 1, 1980, pp. 15–14. JSTOR, https://doi.org/10.2307/2346405.
#' Tests for best link given a specified family 
#' Note only certain links can be used with specific families

#' The below function takes a family argument which can be inferred from the above modified park test.
#' Uses already written test: LDdiag::pregibon.glm

f_pregibontest <- function(df_patients, family = "inverse.gaussian"){
    

    # Perform Pregibon test. If statements to specify family specific links and the family in the GLM code
    # First list of link strings specified, and then maps over the links
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

# Specify family and then run (see what Mod park test infers)
v_fam <- "inverse.gaussian"
res_bysite_pregibontest <- fun_bysite_byoutcome_pregibontest(df_patients2 = df_sim_prepped, fam = v_fam)

# Get p value from test and see which links pass
df_GLM_pregibon <- purrr::map(res_bysite_pregibontest, ~ map(.x, pluck, 1)) %>%
    imap(., ~ set_names(.x, paste(.y, seq_along(.x), sep = "."))) %>% # set names of inner lists    
    purrr::map(., ~ discard(.x, .p = is.null)) %>% # discard where it hasn't managed to run
    purrr::map(.,  ~ map(.x, pluck, 3)) %>% # get p value from the list of test results
    # Reformat
    map(., unlist) %>%
    purrr::flatten(.) %>%
    bind_rows(. , .id = "Cancer_site_pregibon_pvalue") %>%
    rownames_to_column() %>% 
    pivot_longer(!rowname, names_to = "Cancer_site_pregibon_link", values_to = "p_value") %>% 
    select(-rowname) %>%
    separate_wider_delim(cols = "Cancer_site_pregibon_link", delim = ".", names_sep = "",
                         names = c("Cancer type", "cost outcome", "link")) %>%
    # test outcome
    mutate(result = case_when(p_value < 0.05 ~ "DOES NOT PASS pass this test",
                              .default = "DOES PASS this test"))


#' _____ 3c. Aike information criteria

#' Perform goodness-of-fit for all GLM models using the Aike information criteria. 
#' Function below runs all family-link combinations

f_aicbic <- function(df_patients,
                     gaussian_links = c("inverse", "identity", "log"),
                     poisson_links = c("log", "identity" ),
                     Gamma_links = c("inverse", "identity", "log"),
                     inverse.gaussian_links = c("1/mu^2", "inverse", "log", "identity")
){

    #' Glance creates a table of results including AIC
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

#' again a wrapper by site and outcome
fun_bysite_byoutcome_aicbic <- function(df_patients = df_sim_prepped){
    
    l_res_aicbic <- map(.x = split(df_patients, list(df_patients$site, df_patients$cost_outcome)), .f = ~ f_aicbic(.x))
    return(l_res_aicbic = l_res_aicbic)
    
}

res_bysite_byoutcome_aicbic <- fun_bysite_byoutcome_aicbic(df_patients = df_sim_prepped)

df_GLM_AIC <- purrr::map(res_bysite_byoutcome_aicbic, ~ purrr::list_flatten(.x, name_spec = "{outer}.{inner}")) %>%  #' name lists
    purrr::map(., ~ map(.x, pluck, 1)) %>% #' get the results list from each safely run
    purrr::map(., ~ map(.x, pluck, 4)) %>% #' pluck the AIC
    purrr::list_flatten(., name_spec = "{outer}.{inner}") %>%
    # Reformatting
    bind_rows(. , .id = "Cancer_site.cost_outcome_family.link") %>%
    rownames_to_column() %>% 
    pivot_longer(!rowname, names_to = "Cancer_site.cost_outcome_family.link", values_to = "AIC") %>% 
    separate_wider_delim(cols = "Cancer_site.cost_outcome_family.link", delim = ".",
                         names = c("Cancer type", "cost outcome", "family", "link")) #%>%
    #' #' for each Cancer type and cost outcome, select the family-link model with lowest AIC
    #' slice_min(AIC, by = c(`Cancer type`, `cost outcome`))

#' _____________________________________________________________________________
#' 4. Save results

#' Create a folder for the plots to go in and set path
folder_path <- "~/Miscellaneous/HDI/Specification_results/" # example - change accordingly

#' Save as single workbook - use open xlsx package
#' Create workbook
excel_output <- createWorkbook()

addWorksheet(excel_output, sheetName = "Mod_park")
addWorksheet(excel_output, sheetName = "Pregibon")
addWorksheet(excel_output, sheetName = "AIC")


writeData(excel_output, sheet = "Mod_park", df_GLM_mod_park)
writeData(excel_output, sheet = "Pregibon", df_GLM_pregibon)
writeData(excel_output, sheet = "AIC", df_GLM_AIC)

saveWorkbook(excel_output, paste0(folder_path, "Specification_results.xlsx"), overwrite = TRUE)

#' _____________________________________________________________________________

