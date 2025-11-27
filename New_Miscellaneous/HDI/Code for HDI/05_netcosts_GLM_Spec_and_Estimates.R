################################################################################
#______________________________________________________________________________#
#' _____ 04 Net costs - specification tests and estimation

#______________________________________________________________________________#
################################################################################
#' _____________________________________________________________________________
#' 1 - Perform glm specification checks: Mod park test, Pregibon, AIC
#' This is likely ot be unchanged as the overall cost distribution just shifted to the left a bit

#' Back to original LHS forumalla
v_formula <- paste("net_cost ~ age + I(age^2) +",
                   paste(df_patients %>%
                                 select(stage_best_2:chrl_tot_78_06_4) %>%
                                 colnames(), collapse = " + "))


#' _____ 1a. Mod park test
#' Use previously defined function (the by site one!)

res_bysite_modparktest_netcosts <- fun_bysite_byoutcome_modparktest(df_patients = df_sim_prepped_prediagcosts_fu)

#' First get the results (rather than error log)
df_GLM_mod_park_netcosts <- purrr::map(res_bysite_modparktest_netcosts, pluck, 1) %>%
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
#' _____ 1b. Pregibon

# Specify family and then run (see what Mod park test infers)
v_fam <- "inverse.gaussian"
res_bysite_pregibontest_netcosts <- fun_bysite_byoutcome_pregibontest(df_patients2 = df_sim_prepped_prediagcosts_fu, fam = v_fam)

# Get p value from test and see which links pass
df_GLM_pregibon_netcosts <- purrr::map(res_bysite_pregibontest_netcosts, ~ map(.x, pluck, 1)) %>%
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

#' _____ 1c. AIC

res_bysite_byoutcome_aicbic_netcosts <- fun_bysite_byoutcome_aicbic(df_patients = df_sim_prepped_prediagcosts_fu)

df_GLM_AIC_netcosts <- purrr::map(res_bysite_byoutcome_aicbic_netcosts, ~ purrr::list_flatten(.x, name_spec = "{outer}.{inner}")) %>%  #' name lists
        purrr::map(., ~ map(.x, pluck, 1)) %>% #' get the results list from each safely run
        purrr::map(., ~ map(.x, pluck, 4)) %>% #' pluck the AIC
        purrr::list_flatten(., name_spec = "{outer}.{inner}") %>%
        # Reformatting
        bind_rows(. , .id = "Cancer_site.cost_outcome_family.link") %>%
        rownames_to_column() %>% 
        pivot_longer(!rowname, names_to = "Cancer_site.cost_outcome_family.link", values_to = "AIC") %>% 
        separate_wider_delim(cols = "Cancer_site.cost_outcome_family.link", delim = ".",
                             names = c("Cancer type", "cost outcome", "family", "link")) 

#' _____________________________________________________________________________
#' 2. Save results

#'1- Create a folder for the plots to go in and set path
folder_path <- "~/Miscellaneous/HDI/netcosts_specification_results/" # example - change accordingly

#'2- Save as single workbook - use open xlsx package
#' Create workbook
excel_output <- createWorkbook()

addWorksheet(excel_output, sheetName = Mod_park)
addWorksheet(excel_output, sheetName = Pregibon)
addWorksheet(excel_output, sheetName = AIC)


writeData(excel_output, sheet = Mod_park, df_GLM_mod_park_netcosts)
writeData(excel_output, sheet = Pregibon, df_GLM_pregibon_netcosts)
writeData(excel_output, sheet = AIC, df_GLM_AIC_netcosts)

saveWorkbook(excel_output, paste0(folder_path, "netcosts_specification_results.xlsx"), overwrite = TRUE)

#' _____________________________________________________________________________
#' 3. Derive model fits

l_glm_res_bysite_byoutcome_netcosts <- f_glm_res_bysite_byoutcome(df_patients = df_sim_prepped_prediagcosts_fu)

#' Save model fits without the data for shiny app
map2(l_glm_res_bysite_byoutcome_netcosts, names(l_glm_res_bysite_byoutcome_netcosts), .f = function(x, y){
        
        x[[1]]$data <- NULL 
        x[[1]]$residuals <- NULL 
        x[[1]]$fitted.values <- NULL 
        x[[1]]$linear.predictors <- NULL
        x[[1]]$weights <- NULL 
        x[[1]]$y <- NULL 
        x[[1]]$model <- NULL 
        saveRDS(x[[1]], file = paste0("~/Miscellaneous/HDI/Shiny_calc_CostofCancerbyStage/", "net_", y, ".rds")) 
        
})

res_glm_RegSummarybysite_byoutcome_netcosts <- purrr::map(l_glm_res_bysite_byoutcome_netcosts, pluck, 1) %>%
        discard(is.null) %>% # discard where it hasn't managed to run
        purrr::map(.f =  ~ broom::tidy(.)) %>%
        # Neaten up variable names and order for output and plotting
        purrr::map(.f = \(x) x %>% dplyr::select(term, estimate, std.error, p.value) %>%
                           mutate(term = case_when(grepl("sex_male", term) ~ "Sex: Male",
                                                   grepl("stage_best_2", term) ~ "Stage: II",
                                                   grepl("stage_best_3", term) ~ "Stage: III",
                                                   grepl("stage_best_4", term) ~ "Stage: IV",
                                                   grepl("stage_best_missing", term) ~ "Stage: Missing/Unknown",
                                                   grepl("chrl_tot_78_06_2", term) ~ "CCI score: 2",
                                                   grepl("chrl_tot_78_06_3", term) ~ "CCI score: 3",
                                                   grepl("chrl_tot_78_06_4", term) ~ "CCI score: 4",
                                                   grepl("eth_group_a", term) ~ "Ethnicity: Asian",
                                                   grepl("eth_group_b", term) ~ "Ethnicity: Black",
                                                   grepl("eth_group_m", term) ~ "Ethnicity: Mixed",
                                                   # grepl("eth_group_o", term) ~ "Ethnicity: Other", - other not in my simulated dataframe, but unhash here
                                                   grepl("quintile_2015_2", term) ~ "IMD quintile: 2",
                                                   grepl("quintile_2015_3", term) ~ "IMD quintile: 3",
                                                   grepl("quintile_2015_4", term) ~ "IMD quintile: 4",
                                                   grepl("quintile_2015_5", term) ~ "IMD quintile: 5",
                                                   grepl("age", term) ~ "Age",
                                                   .default = "Intercept"),
                                  term = factor(term, levels = c("Age","Sex: Male","Stage: II","Stage: III","Stage: IV","Stage: Missing/Unknown",
                                                                 "CCI score: 2","CCI score: 3","CCI score: 4",
                                                                 "Ethnicity: Asian","Ethnicity: Black","Ethnicity: Mixed",
                                                                 # "Ethnicity: Other", unhash
                                                                 "IMD quintile: 2","IMD quintile: 3","IMD quintile: 4","IMD quintile: 5","Intercept"))) %>%
                           arrange(term))


#' Take results out of safety wrapper - a catch function for errors
#' Produce average Marginal effects on the raw scale for each cancer type and outcome
res_glm_AMEbysite_byoutcome_netcosts <- purrr::map(l_glm_res_bysite_byoutcome_netcosts, pluck, 1) %>%
        discard(is.null) %>% # discard where it hasn't managed to run
        purrr::map(.f =  ~ broom.helpers::tidy_avg_slopes(.)) %>%
        # Neaten up variable names and order for output and plotting
        purrr::map(.f = \(x) x %>% dplyr::select(variable, estimate, std.error, p.value, conf.low, conf.high) %>%
                           mutate(variable = case_when(grepl("sex_male", variable) ~ "Sex: Male",
                                                       grepl("stage_best_2", variable) ~ "Stage: II",
                                                       grepl("stage_best_3", variable) ~ "Stage: III",
                                                       grepl("stage_best_4", variable) ~ "Stage: IV",
                                                       grepl("stage_best_missing", variable) ~ "Stage: Missing/Unknown",
                                                       grepl("chrl_tot_78_06_2", variable) ~ "CCI score: 2",
                                                       grepl("chrl_tot_78_06_3", variable) ~ "CCI score: 3",
                                                       grepl("chrl_tot_78_06_4", variable) ~ "CCI score: 4",
                                                       grepl("eth_group_a", variable) ~ "Ethnicity: Asian",
                                                       grepl("eth_group_b", variable) ~ "Ethnicity: Black",
                                                       grepl("eth_group_m", variable) ~ "Ethnicity: Mixed",
                                                       # grepl("eth_group_o", Variable) ~ "Ethnicity: Other", - other not in my simulated dataframe, but unhash here
                                                       grepl("quintile_2015_2", variable) ~ "IMD quintile: 2",
                                                       grepl("quintile_2015_3", variable) ~ "IMD quintile: 3",
                                                       grepl("quintile_2015_4", variable) ~ "IMD quintile: 4",
                                                       grepl("quintile_2015_5", variable) ~ "IMD quintile: 5",
                                                       grepl("age", variable) ~ "Age",
                                                       .default = NA),
                                  variable = factor(variable, levels = c("Age","Sex: Male","Stage: II","Stage: III","Stage: IV","Stage: Missing/Unknown",
                                                                         "CCI score: 2","CCI score: 3","CCI score: 4",
                                                                         "Ethnicity: Asian","Ethnicity: Black","Ethnicity: Mixed",
                                                                         # "Ethnicity: Other", unhash
                                                                         "IMD quintile: 2","IMD quintile: 3","IMD quintile: 4","IMD quintile: 5"))) %>%
                           arrange(variable))

#' See if there were any errors
errors_glm_res_bysite_byoutcome <- purrr::map(l_glm_res_bysite_byoutcome_netcosts, pluck, 2)

#' _____________________________________________________________________________
#' 4. Save results and graphs

#'Create a folder for the plots to go in and set path
folder_path <- "~/Miscellaneous/HDI/Net_cost_Regression_AME/" # example - change accordingly

#' Create workbook
RegSummary_workbook_net <- createWorkbook()
AME_workbook_net <- createWorkbook()

#' Save work books
map2(res_glm_RegSummarybysite_byoutcome_netcosts, names(res_glm_RegSummarybysite_byoutcome_netcosts), .f = function(x, y){ 
        
        addWorksheet(RegSummary_workbook_net, sheetName = y)
        writeData(RegSummary_workbook_net, sheet = y, x)
        
})

map2(res_glm_AMEbysite_byoutcome_netcosts, names(res_glm_AMEbysite_byoutcome_netcosts), .f = function(x, y){ 
        
        addWorksheet(AME_workbook_net, sheetName = y)
        writeData(AME_workbook_net, sheet = y, x)
        
})

#' Save work books in folder
saveWorkbook(RegSummary_workbook_net, paste0(folder_path, "RegSummary_results_net.xlsx"), overwrite = TRUE)

saveWorkbook(AME_workbook_net, paste0(folder_path, "AME_results.xlsx_net"), overwrite = TRUE)



#' Function for plotting the AMEs and conf intervals. Saving .tiff format as this is often used by journals
#' Needs some iteration to scale y accordingly - ideally all plots have same scale
#' And on width and height of .tiff file

map2(res_glm_AMEbysite_byoutcome_netcosts, names(res_glm_AMEbysite_byoutcome_netcosts), f_plot_save_AMEs)


#' _____________________________________________________________________________