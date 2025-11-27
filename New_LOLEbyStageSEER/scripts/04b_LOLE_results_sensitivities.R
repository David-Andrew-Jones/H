#______________________________________________________________________________#
#____ 04b_LOLE_results_sensitivities


#______________________________________________________________________________#
#' Post-results processing
#' Load in per cancer tables and merge
#' 5 year knot intervals
#' 

res_LOLE_primary<- res_LE_tables_all_full %>%
        filter(SEER_Draw == "Lung") %>%
        mutate(LOLE_median = ex - median,
               LOLE_2.5 = ex - `2.5%`,
               LOLE_97.5 = ex - `97.5%`) %>%
        mutate(analysis = "Primary: Mixture-cure model") %>%
        rename(`Cancer site` = SEER_Draw,
               `Age at diagnosis` = Age_lower)

res_LOLE_5yknot<- list.files(path = result_path_sens_knots5y, pattern = "^LE") %>% # Defined in 01 script
        map(~read_csv(paste0(result_path_sens_knots5y, .))) %>%
        bind_rows() %>%
        # Column name differences - easier to change here then go back to the 03 scripts
        rename(`Cancer site` = NCRAS_Draw,
               `Age at diagnosis` = Age_lower,
               AJCC_stage = Stage) %>%
        select(Sex, `Cancer site`, `Age at diagnosis`, Age_string, AJCC_stage, median_CrI, median, `2.5%`, `97.5%`, ex) %>%
        mutate(LOLE_median = ex - median,
               LOLE_2.5 = ex - `2.5%`,
               LOLE_97.5 = ex - `97.5%`) %>%
        mutate(analysis = "Sensitivity: 5 year knots") 

#' 10 year knot intervals
res_LOLE_10yknot <- list.files(path = result_path_sens_knots10y, pattern = "^LE") %>% # Defined in 01 script
        map(~read_csv(paste0(result_path_sens_knots10y, .))) %>%
        bind_rows() %>%
        # Column name differences - easier to change here then go back to the 03 scripts
        rename(`Cancer site` = NCRAS_Draw,
               `Age at diagnosis` = Age_lower,
               AJCC_stage = Stage) %>%
        select(Sex, `Cancer site`, `Age at diagnosis`, Age_string, AJCC_stage, median_CrI, median, `2.5%`, `97.5%`, ex) %>%
        mutate(LOLE_median = ex - median,
               LOLE_2.5 = ex - `2.5%`,
               LOLE_97.5 = ex - `97.5%`) %>%
        mutate(analysis = "Sensitivity: 10 year knots") 

#' No cure fraction 
res_LOLE_noCF <- list.files(path = result_path_sens_noCF, pattern = "^LE") %>% # Defined in 01 script
        map(~read_csv(paste0(result_path_sens_noCF, .))) %>%
        bind_rows() %>%
        # Column name differences - easier to change here then go back to the 03 scripts
        rename(`Cancer site` = NCRAS_Draw,
               `Age at diagnosis` = Age_lower,
               AJCC_stage = Stage) %>%
        select(Sex, `Cancer site`, `Age at diagnosis`, Age_string, AJCC_stage, median_CrI, median, `2.5%`, `97.5%`, ex) %>%
        mutate(LOLE_median = ex - median,
               LOLE_2.5 = ex - `2.5%`,
               LOLE_97.5 = ex - `97.5%`) %>%
        mutate(analysis = "Sensitivity: No cure fraction") 


res_primary_sens_lung_LOLE <- bind_rows(res_LOLE_primary, 
                                        # res_LOLE_5yknot, 
                                        # res_LOLE_10yknot,
                                        res_LOLE_noCF)

table_primary_sens_lung_LOLE <- res_primary_sens_lung_LOLE %>%
        mutate(across(c(LOLE_median, LOLE_2.5, LOLE_97.5 ), \(x) round(x, 3) )) %>%
        unite(LOLE_CrI_temp, c(LOLE_97.5, LOLE_2.5), sep = " to ", remove = FALSE) %>%
        mutate(LOLE_CrI = paste0("(", LOLE_CrI_temp, ")")) %>%
        unite(LOLE_median_CrI, c(LOLE_median, LOLE_CrI), sep = " ", remove = FALSE)  %>%
        select(analysis, Sex, Age_string, AJCC_stage, LOLE_median_CrI) %>%
        arrange(Sex, Age_string) %>%
        pivot_wider(names_from = analysis, values_from = c(LOLE_median_CrI)) 

addWorksheet(excel_output, sheetName = "Table 2 sensitivity")
writeData(excel_output, sheet = "Table 2 sensitivity", table_primary_sens_lung_LOLE)



#Figure - unused

res_primary_sens_lung_LOLE %>%
        ggplot() +
        geom_bar(aes(x = forcats::fct_rev(Age_string), y = LOLE_median, group = forcats::fct_rev(analysis), fill = forcats::fct_rev(analysis)), position= position_dodge(0.5), stat = "identity", width = .5) +
        geom_errorbar(aes(x = forcats::fct_rev(Age_string),  ymin = LOLE_97.5, ymax = LOLE_2.5, group = forcats::fct_rev(analysis)), position= position_dodge(0.5), width=.1) +
        facet_grid(rows  = vars(Sex), cols = vars(AJCC_stage), switch = "y" , scales = "fixed") +
        ggsci::scale_fill_jama() +
        labs(fill = "Stage", x = NULL, y = "Years of Life Lost (YLL)") +
        theme_bw() +
        coord_flip() +
        theme(strip.text.y.left = element_text(angle = 0),
              legend.position = "bottom",
              #axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              text = element_text(size = 16, face="bold"))



res_LE_primary<- res_LE_tables_all_full %>%
        filter(SEER_Draw == "Lung") %>%
        mutate(analysis = "Primary: Mixture-cure model") %>%
        rename(`Cancer site` = SEER_Draw,
               `Age at diagnosis` = Age_lower)

#' No cure fraction 
res_LE_noCF <- list.files(path = result_path_sens_noCF, pattern = "^LE") %>% # Defined in 01 script
        map(~read_csv(paste0(result_path_sens_noCF, .))) %>%
        bind_rows() %>%
        # Column name differences - easier to change here then go back to the 03 scripts
        rename(`Cancer site` = NCRAS_Draw,
               `Age at diagnosis` = Age_lower,
               AJCC_stage = Stage) %>%
        select(Sex, `Cancer site`, `Age at diagnosis`, Age_string, AJCC_stage, median_CrI, median, `2.5%`, `97.5%`, ex) %>%
        mutate(analysis = "Sensitivity: No cure fraction") 


res_primary_sens_lung_LE <- bind_rows(res_LE_primary, 
                                      
                                        res_LE_noCF)

table_primary_sens_lung_LE <- res_primary_sens_lung_LE %>%
        mutate(across(c(median, `2.5%`, `97.5%` ), \(x) round(x, 3) )) %>%
        unite(CrI_temp, c(`97.5%`, `2.5%`), sep = " to ", remove = FALSE) %>%
        mutate(CrI = paste0("(", CrI_temp, ")")) %>%
        unite(median_CrI, c(median, CrI), sep = " ", remove = FALSE)  %>%
        select(analysis, Sex, Age_string, AJCC_stage, median_CrI) %>%
        arrange(Sex, Age_string) %>%
        pivot_wider(names_from = analysis, values_from = c(median_CrI)) 

#Figure - unused

res_primary_sens_lung_LE %>%
        ggplot() +
        geom_bar(aes(x = forcats::fct_rev(Age_string), y = median, group = forcats::fct_rev(analysis), fill = forcats::fct_rev(analysis)), position= position_dodge(0.5), stat = "identity", width = .5) +
        geom_errorbar(aes(x = forcats::fct_rev(Age_string),  ymin = `97.5%`, ymax = `2.5%`, group = forcats::fct_rev(analysis)), position= position_dodge(0.5), width=.1) +
        facet_grid(rows  = vars(Sex), cols = vars(AJCC_stage), switch = "y" , scales = "fixed") +
        ggsci::scale_fill_jama() +
        labs(fill = "Stage", x = NULL, y = "Life expectancy") +
        theme_bw() +
        coord_flip() +
        theme(strip.text.y.left = element_text(angle = 0),
              legend.position = "bottom",
              #axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              text = element_text(size = 16, face="bold"))




