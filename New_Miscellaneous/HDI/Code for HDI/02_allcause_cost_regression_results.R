################################################################################
#______________________________________________________________________________#
#' _____ 02 All cause cost regression results
#______________________________________________________________________________#
################################################################################

#' _____________________________________________________________________________
#' _____ 1. Function to fit model
#' LHS formula to be used in regressions

v_formula <- paste("sum_costs_outcome ~ age + I(age^2) +",
                   paste(df_sim_prepped %>%
                                 select(stage_best_2:chrl_tot_78_06_4) %>%
                                 colnames(), collapse = " + "))

#' Specify best fitting model and apply to each site and cost outcome - 
#' Inverse.gaussian with inverse link used as an example
f_glm_res <- function(df_patients, family_type = "inverse.gaussian", link_type = "inverse"){
        
        if(family_type == "gaussian"){

                l_res_glm <- glm(formula = as.formula(v_formula), family = gaussian(link = link_type), data = df_patients)
                
        } else if(family_type == "poisson"){
                
                l_res_glm <- glm(formula = as.formula(v_formula), family = poisson(link = link_type), data = df_patients)
                
        } else if(family_type == "Gamma"){
                
                l_res_glm <- glm(formula = as.formula(v_formula), family = Gamma(link = link_type), data = df_patients)
                
        } else if(family_type == "inverse.gaussian"){
                
                l_res_glm <- glm(formula = as.formula(v_formula), family = inverse.gaussian(link = link_type), data = df_patients)

        } else {
                
                print("please specify family_type - gaussian, poisson, Gamma or inverse.gaussian, and a valid link")
        }
        
        return(l_res_glm)
}

#' Wrapper function for map over sites and cost outcome
f_glm_res_bysite_byoutcome <- function(df_patients = df_sim_prepped){
        
        l_glm_res <- map(.x = split(df_patients, list(df_patients$site, df_patients$cost_outcome)), .f = safely(~ f_glm_res(.x)))
        return(l_res = l_glm_res)
}

#' _____________________________________________________________________________
#' _____ 2. Derive model fits
l_glm_res_bysite_byoutcome <- f_glm_res_bysite_byoutcome(df_patients = df_sim_prepped)

#' Save model fits without the data for shiny app
map2(l_glm_res_bysite_byoutcome, names(l_glm_res_bysite_byoutcome), .f = function(x, y){
        
        # Remove data for obvious reasons, and remove results from which IDP could be roughly recontructed
        x[[1]]$data <- NULL 
        x[[1]]$residuals <- NULL 
        x[[1]]$fitted.values <- NULL 
        x[[1]]$linear.predictors <- NULL
        x[[1]]$weights <- NULL 
        x[[1]]$y <- NULL 
        x[[1]]$model <- NULL 
        
        saveRDS(x[[1]], file = paste0("~/Miscellaneous/HDI/Shiny_calc_CostofCancerbyStage/", "allcause_", y, ".rds")) 
        
        })


#' _____________________________________________________________________________
#' _____ 3. Derive average marginal effects

#' Take results out of safety wrapper - a catch function for errors
#' Produce average Marginal effects on the raw scale for each cancer type and outcome
#' Makes use of the "margins" package
#' NB. margins will take a long time to run!!!!

df_patients <- df_sim_prepped

#' #' OLD uses margins::margins
#' res_glm_bysite_byoutcome <- purrr::map(l_glm_res_bysite_byoutcome, pluck, 1) %>%
#'         discard(is.null) %>% # discard where it hasn't managed to run
#'         purrr::map(.f = ~ summary(margins::margins(.))) %>%
#'         # Neaten up variable names and order for output and plotting
#'         purrr::map(.f = \(x) x %>% rename(Variable = factor) %>%
#'                            mutate(Variable = case_when(grepl("sex_male", Variable) ~ "Sex: Male",
#'                                                        grepl("stage_best_2", Variable) ~ "Stage: II",
#'                                                        grepl("stage_best_3", Variable) ~ "Stage: III",
#'                                                        grepl("stage_best_4", Variable) ~ "Stage: IV",
#'                                                        grepl("stage_best_missing", Variable) ~ "Stage: Missing/Unknown",
#'                                                        grepl("chrl_tot_78_06_2", Variable) ~ "CCI score: 2",
#'                                                        grepl("chrl_tot_78_06_3", Variable) ~ "CCI score: 3",
#'                                                        grepl("chrl_tot_78_06_4", Variable) ~ "CCI score: 4",
#'                                                        grepl("eth_group_a", Variable) ~ "Ethnicity: Asian",
#'                                                        grepl("eth_group_b", Variable) ~ "Ethnicity: Black",
#'                                                        grepl("eth_group_m", Variable) ~ "Ethnicity: Mixed",
#'                                                        # grepl("eth_group_o", Variable) ~ "Ethnicity: Other", - other not in my simulated dataframe, but unhash here
#'                                                        grepl("quintile_2015_2", Variable) ~ "IMD quintile: 2",
#'                                                        grepl("quintile_2015_3", Variable) ~ "IMD quintile: 3",
#'                                                        grepl("quintile_2015_4", Variable) ~ "IMD quintile: 4",
#'                                                        grepl("quintile_2015_5", Variable) ~ "IMD quintile: 5",
#'                                                        grepl("age", Variable) ~ "Age",
#'                                                        .default = NA),
#'                                   Variable = factor(Variable, levels = c("Age","Sex: Male","Stage: II","Stage: III","Stage: IV","Stage: Missing/Unknown",
#'                                                                          "CCI score: 2","CCI score: 3","CCI score: 4",
#'                                                                          "Ethnicity: Asian","Ethnicity: Black","Ethnicity: Mixed",
#'                                                                          # "Ethnicity: Other", unhash 
#'                                                                          "IMD quintile: 2","IMD quintile: 3","IMD quintile: 4","IMD quintile: 5"))) %>%
#'                            arrange(Variable)) 


#' _____ Normal regression results
res_glm_RegSummarybysite_byoutcome <- purrr::map(l_glm_res_bysite_byoutcome, pluck, 1) %>%
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

#' _____ AME
#' NEW - uses marginaleffcts::avg_slope, with broom.helpers wrapper tidy_avg_slopes
res_glm_AMEbysite_byoutcome <- purrr::map(l_glm_res_bysite_byoutcome, pluck, 1) %>%
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
errors_glm_res_bysite_byoutcome <- purrr::map(l_glm_res_bysite_byoutcome, pluck, 2)

#' _____________________________________________________________________________
#' _____ 4. Save results and graphs

#'1- Create a folder for the plots to go in and set path
folder_path <- "~/Miscellaneous/HDI/Regression_AME/" # example - change accordingly

#'2- Save as single workbook - use open xlsx package
library(openxlsx, quietly = TRUE)

#' Create workbook
RegSummary_workbook <- createWorkbook()

AME_workbook <- createWorkbook()

#' Save work books
map2(res_glm_RegSummarybysite_byoutcome, names(res_glm_RegSummarybysite_byoutcome), .f = function(x, y){ 
        
        addWorksheet(RegSummary_workbook, sheetName = y)
        writeData(RegSummary_workbook, sheet = y, x)

        })

map2(res_glm_AMEbysite_byoutcome, names(res_glm_AMEbysite_byoutcome), .f = function(x, y){ 
        
        addWorksheet(AME_workbook, sheetName = y)
        writeData(AME_workbook, sheet = y, x)
        
})
     
#' Save work books in folder
saveWorkbook(RegSummary_workbook, paste0(folder_path, "RegSummary_results.xlsx"), overwrite = TRUE)

saveWorkbook(AME_workbook, paste0(folder_path, "AME_results.xlsx"), overwrite = TRUE)

#' Function for plotting the AMEs and conf intervals. Saving .tiff format as this is often used by journals
#' Needs some iteration to scale y accordingly - ideally all plots have same scale
#' And on width and height of .tiff file

f_plot_save_AMEs <- function(data, name){
        
        plot <- data %>% 
                ggplot(aes(x = forcats::fct_rev(variable), y = estimate)) +
                geom_point(shape = 16,size  = 3, color="black") + 
                geom_errorbar(aes(ymin  = conf.low,ymax  = conf.high), size  = 0.5, position = "dodge", color="black") +
                xlab("") + ylab("Average Marginal Effect") +
                coord_flip() + 
                geom_hline(yintercept = 0, color = "grey", size = 1, linetype = 'dotted') +
                theme_bw(base_size = 14) +
                theme(axis.title = element_text(face = "bold", size = 17)) +
                ylim(-20000, 20000) # Scale accordingly - ideally all plots have same scale
                scale_y_continuous(labels = scales::label_currency(prefix = "£"))
                
        # Made need iteration on best width and height
        ggsave(paste0(folder_path, name, ".tiff"), plot, device = "tiff", width = 160, height = 160, units = "mm")
        

}

map2(res_glm_AMEbysite_byoutcome, names(res_glm_AMEbysite_byoutcome), f_plot_save_AMEs)

#' _____________________________________________________________________________


