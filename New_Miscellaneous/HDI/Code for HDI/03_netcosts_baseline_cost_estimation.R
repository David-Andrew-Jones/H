################################################################################
#______________________________________________________________________________#
#' _____ 03 Net costs - deriving baseline costs
#______________________________________________________________________________#
################################################################################
#' Using df_sim_prepped as example dataframe again - change as needed
#' Using "diag" phase costs for pre-diag costs for this example

df_sim_precosts <- df_sim_prepped %>%
        filter(cost_outcome == "diag") %>%
        # Randomly replace cost with some zeros
        mutate(sum_costs_outcome = replace(sum_costs_outcome, sample(row_number(),size = ceiling(0.4 * n()), replace = TRUE), 0))

df_sim_prepped_patients <- df_sim_prepped %>%
        select(patientid:date_fu6y,stage_best_2:chrl_tot_78_06_5) %>%
        group_by(patientid) %>%
        slice(1) %>%
        ungroup()


#' _____________________________________________________________________________
#' 1. Check for number of patient's with Zero healthcare costs in the annual window pre diagnosis
#' Is there a sufficient number to warrant 2 part model
#' Change as need to HDI dataframe

res_percent0 <- df_sim_precosts %>% 
        summarise(n_zero = sum(sum_costs_outcome == 0),
                  perc_zero = (sum(sum_costs_outcome == 0) / n()) * 100)

#' Use binary outcomes for logistic and non-zero costs for GLM creation
df_sim_precosts1and0 <- df_sim_precosts %>% mutate(sum_costs_outcome = case_when(sum_costs_outcome > 0 ~ 1, .default = 0 ))
df_sim_precosts_nonzero <- df_sim_precosts %>% filter(sum_costs_outcome > 0)

#' _____________________________________________________________________________
#' 2. Perform GLM specification checks: Mod park test, Pregibon, AIC

#' Set formula - not different from previous formula as site now included as an predictor
#' There now no need to split the dataframe by site

v_prediag_formula <- paste("sum_costs_outcome ~ age + I(age^2) + site +",
                   paste(df_patients %>%
                                 select(sex_male:chrl_tot_78_06_4) %>%
                                 colnames(), collapse = " + "))

#' _____ 2a. Mod park test
#' Use previously defined function

res_modparktest_prediag <- data.frame(Slope = (fun_modparktest(df_patients = df_sim_precosts_nonzero)[[2]])) %>%
        #' Which family inferred
        mutate(Family = case_when(round(Slope, 0) == 0 ~ "Gaussian",
                                  round(Slope, 0) == 1 ~ "Poisson",
                                  round(Slope, 0) == 2 ~ "Gamma",
                                  round(Slope, 0) == 3 ~ "Inverse Gaussian",
                                  .default = NA))
#' _____ 2b. Pregibon

res_pregibon_prediag <- map(f_pregibontest(df_patients = df_sim_precosts_nonzero), pluck, 1) %>%
        purrr::set_names(., ~ letters[seq_along(.)]) %>% # Set arbitrary names (letters) for each link function tested
        discard(., .p = is.null) %>% # Discard when link has produced null
        purrr::map(., pluck, 3) %>% # get p value from the list of test results]
        bind_rows(., .id = "Link") %>%
        rownames_to_column() %>% 
        pivot_longer(!rowname, names_to = "pregibon_link", values_to = "p_value") %>% 
        select(-rowname) %>%
        # test outcome
        mutate(result = case_when(p_value < 0.05 ~ "DOES NOT PASS pass this test",
                                  .default = "DOES PASS this test"))

#' _____ 2c. AIC

res_aicbic_prediag <- purrr::list_flatten(f_aicbic(df_patients = df_sim_prepped), name_spec = "{outer}.{inner}") %>%  #' name lists
        purrr::map(., pluck, 1) %>% #' get the results list from each safely run
        purrr::map(., pluck, 4) %>% #' pluck the AIC
        # Reformatting
        bind_rows(. , .id = "link_family") %>%
        rownames_to_column() %>% 
        pivot_longer(!rowname, names_to = "link_family", values_to = "AIC") %>% 
        separate_wider_delim(cols = "link_family", delim = ".",
                             names = c("remove", "link_family")) %>%
        separate_wider_delim(cols = "link_family", delim = "_",
                             names = c("link", "family")) %>%
        select(family, link, AIC) 


#' _____________________________________________________________________________
#' 3. Fit logistic model and best fitting GLM and create predictions of baseline costs for each patient over their next 10 years of age
#' Part 1 Logistic regression - Probability of any cost
fit_p1 <- glm(formula = as.formula(v_prediag_formula), family = binomial(link = 'logit'), data = df_sim_precosts1and0)

#' Part 2 Predicted cost conditional on cost occuring
fit_p2 <- glm(formula = as.formula(v_prediag_formula), family = inverse.gaussian(link = 'inverse'), data = df_sim_precosts_nonzero)

#' _____ Deterministic 
#' Note using the df_sim_prepped_patient dataframe - just the patient characteristics
df_cost_predictions_deterministic <- df_sim_prepped_patients %>%
        # Create rows for each patient with 1 year incremental year increase
        uncount(9, .id = "additional_age") %>%
        # Add this to their age at diagnosis (-2 to account for pre-diag period)
        mutate(age_diag = age,
               age = age + additional_age -2 ) %>%        
        # Predict costs
        mutate(p1_odds_prediagcosts = predict(fit_p1, newdata = ., type = "response"),
               p1_prob_prediagn_costs = p1_odds_prediagcosts / (p1_odds_prediagcosts + 1),
               p2_conditional_cost = predict(fit_p2, newdata = ., type = "response"),
               predicted_annual_cost = p1_prob_prediagn_costs * p2_conditional_cost)


#' _____________________________________________________________________________
#' 4. Save specification results, regression, and AME's

#' Regression results

res_RegSummary_fit1 <- fit_p1 %>%
        broom::tidy(.) %>% 
        dplyr::select(term, estimate, std.error, p.value)

res_RegSummary_fit2 <- fit_p2 %>%
        broom::tidy(.) %>% 
        dplyr::select(term, estimate, std.error, p.value)

#' AME
fit_p2 <- glm(formula = as.formula(v_prediag_formula), family = inverse.gaussian(link = 'inverse'), data = df_sim_precosts_nonzero)

broom.helpers::tidy_marginal_predictions(
        fit_p1,
        variables_list = "no_interaction",
        conf.int = TRUE,
        conf.level = 0.95
)
avg_pred_fit1 <- avg_predictions(fit_p1, conf_level = .9)

avg_pred_fit2 <- avg_predictions(fit_p2)



##  I(age^2) may be causing an error with predict so changing below


v_prediag_formula <- paste("sum_costs_outcome ~ age + age_square + site +",
                           paste(df_patients %>%
                                         select(sex_male:chrl_tot_78_06_4) %>%
                                         colnames(), collapse = " + "))

v_formula <- paste("sum_costs_outcome ~ age + age_square +",
                   paste(df_sim_prepped %>%
                                 select(stage_best_2:chrl_tot_78_06_4) %>%
                                 colnames(), collapse = " + "))

df_sim_precosts <- df_sim_precosts %>%
        mutate(age_square = age^2)


test_fit_tpm <- twopartm::tpm(formula_part1 = as.formula(v_formula), formula_part2 = as.formula(v_prediag_formula), data = df_sim_precosts ,link_part1 = "logit", family_part2 = inverse.gaussian(link = 'inverse'))

x <- test_fit_tpm

x@data <- data.frame(1)
x@data_model1 <- data.frame(1)
x@data_model2 <- data.frame(1)
x@model_part1$data <- 1 
x@model_part1$residuals <- 1 
x@model_part1$fitted.values <- 1 
x@model_part1$linear.predictors <- 1
x@model_part1$weights <- 1 
x@model_part1$model <- 1
x@model_part1$y <- 1
x@model_part1$model <- 1

x@model_part2$data <- 1 
x@model_part2$residuals <- 1 
x@model_part2$fitted.values <- 1 
x@model_part2$linear.predictors <- 1
x@model_part2$weights <- 1 
x@model_part2$model <- 1
x@model_part2$y <- 1
x@model_part2$model <- 1

x@residuals <- 1 
x@weights <- 1 
x@y <- 1 

twopartm::predict(x ,newdata = df_sim_precosts[1,], se.fit =TRUE)

twopartm::predict(test_fit_tpm, newdata = df_sim_precosts[1,], se.fit =TRUE)

quantile(asd,na.rm = T,probs = c(0.025,0.5,0.975))
test <- twopartm::margin(test_fit_tpm)


twopartm::AME(test_fit_tpm, )

res_test_fit_tpm <- test_fit_tpm %>%
        broom.helpers::tidy_avg_slopes(.) %>% 
        dplyr::select(variable,term,estimate, std.error, p.value, conf.low, conf.high)


df_patients <- df_sim_prepped

res_AME_fit1 <- fit_p1 %>%
        broom.helpers::tidy_avg_slopes(.) %>% 
        dplyr::select(variable,term,estimate, std.error, p.value, conf.low, conf.high)

res_AME_fit2 <- fit_p2 %>%
        broom.helpers::tidy_avg_slopes(.) %>% 
        dplyr::select(variable, term,estimate, std.error, p.value, conf.low, conf.high)

res_age_site_interaction <- plot_slopes(fit_p2, variables = "site", condition = "age")

#'1- Create a folder for the plots to go in and set path
folder_path <- "~/Miscellaneous/HDI/Net_cost_specification_results/" # example - change accordingly

#'2- Save as single workbook - use open xlsx package
#' Create workbook
excel_output <- createWorkbook()

addWorksheet(excel_output, sheetName = "Prediag_Mod_park")
addWorksheet(excel_output, sheetName = "Prediag_Pregibon")
addWorksheet(excel_output, sheetName = "Prediag_AIC")
addWorksheet(excel_output, sheetName = "Prediag_RegSummary_2p1")
addWorksheet(excel_output, sheetName = "Prediag_RegSummary_2p2")
addWorksheet(excel_output, sheetName = "Prediag_AME_2p1")
addWorksheet(excel_output, sheetName = "Prediag_AME_2p2")
addWorksheet(excel_output, sheetName = "Prediag_age_site_interaction")

writeData(excel_output, sheet = "Prediag_Mod_park", res_modparktest_prediag)
writeData(excel_output, sheet = "Prediag_Pregibon", res_pregibon_prediag)
writeData(excel_output, sheet = "Prediag_AIC", res_aicbic_prediag)
writeData(excel_output, sheet = "Prediag_RegSummary_2p1", res_RegSummary_fit1)
writeData(excel_output, sheet = "Prediag_RegSummary_2p2", res_RegSummary_fit2)
writeData(excel_output, sheet = "Prediag_AME_2p1", res_AME_fit1)
writeData(excel_output, sheet = "Prediag_AME_2p1", res_AME_fit2)
print(res_age_site_interaction)
insertPlot(excel_output, "Prediag_age_site_interaction", xy = c("J", 2), width = 16, height = 10, fileType = "png", units = "cm")

saveWorkbook(excel_output, paste0(folder_path, "Prediag_Specification_results.xlsx"), overwrite = TRUE)

#' _____________________________________________________________________________
               

