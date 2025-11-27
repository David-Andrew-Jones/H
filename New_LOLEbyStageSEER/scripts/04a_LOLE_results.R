#______________________________________________________________________________#
#____ 04a_LOLE_results_M_spline

#' Uses a Bayesian flexible parametric survival model, based on an M-spline
#' to extrapolate the SEER individual patient data 



#' _____________________________________________________________________________
#' _____ 1.  Paths and create an excel document for all of the tabular results

table_path <- paste0(getwd(), "/results/results_tables/")
figure_path <- paste0(getwd(), "/results/figures/")
excel_output <- createWorkbook()

#' _____________________________________________________________________________
#' Create template dataframe with all potential combinations
#' 
df_all_combos <- expand.grid(Sex = levels(as.factor(TTE_data$Sex)),
                             SEER_Draw = c("all", "allexcprostate", levels(as.factor(TTE_data$SEER_Draw))),
                             AJCC_stage = levels(as.factor(TTE_data$AJCC_stage)),
                             Age_lower = as.numeric(levels(as.factor(TTE_data$Age_lower)))) %>%
        filter( !(grepl(c("Cervix|Breast|Uterus"), SEER_Draw) & Sex == "Male")) %>%
        filter( !(grepl(c("Prostate"), SEER_Draw) & Sex == "Female")) %>%
        #filter( !(grepl(c("Prostate|allexcprostate"), SEER_Draw) & Sex == "Female")) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor)) 

#' Check to see which patient presentations produces errors and save
df_all_combos_primary <- df_all_combos %>%
        mutate(in_dfallcombos = "yes") %>%
        left_join(list.files(path = results_path, pattern = "^LE") %>% # Defined in 01 script
                          map(~read_csv(paste0(results_path, .))) %>%
                          bind_rows() %>%
                          mutate(modelled = "Yes"),
                  by = c("Sex", "SEER_Draw", "AJCC_stage", "Age_lower")) %>%
        # Remove the cancer type-sex incompatible rows
        # filter(!(Sex == "Male" & NCRAS_Draw == "Cervix")) %>%
        # filter(!(Sex == "Male" & NCRAS_Draw == "Breast")) %>%
        # filter(!(Sex == "Male" & NCRAS_Draw == "Ovary")) %>%
        # filter(!(Sex == "Male" & NCRAS_Draw == "Uterus")) %>%
        # filter(!(Sex == "Female" & NCRAS_Draw == "Prostate")) %>%
        rename(`Cancer type` = "SEER_Draw") %>%
        select(Sex, `Cancer type`, AJCC_stage, Age_lower, modelled) %>%
        pivot_wider(names_from = "Age_lower", values_from = "modelled") %>%
        mutate(`Cancer type` = factor(`Cancer type`, levels = c("all", "allexcprostate", levels(as.factor(TTE_data$SEER_Draw))))) %>%
        arrange(Sex, `Cancer type`, AJCC_stage) %>%
        # Create a column sum row to see errors by age
        mutate(across(`40`:`85`, ~ case_when(. == "Yes" ~ 1, .default = 0))) %>%
        bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))


addWorksheet(excel_output, sheetName = "1. Outcome check")
writeData(excel_output, sheet = "1. Outcome check",  df_all_combos_primary)

#' _____________________________________________________________________________
#' _____ Life expectancy

res_LE_tables_all_primary <- list.files(path = results_path, pattern = "^LE") %>% # Defined in 01 script
        map(~read_csv(paste0(results_path, .))) %>%
        bind_rows() %>%
        filter(!(grepl(c("Cervix|Breast|Uterus"), SEER_Draw) & Sex == "Male")) %>%
        filter(!(grepl(c("Prostate"), SEER_Draw) & Sex == "Female")) %>%
        filter(!(SEER_Draw== "allexcprostate" & Sex == "Female")) %>%
        rename(`Cancer type` = "SEER_Draw",
               Stage = "AJCC_stage") %>%
        mutate(`Cancer type` = case_when(`Cancer type` == "all" ~"All cancers", 
                                         `Cancer type` == "allexcprostate" ~"All excluding prostate",
                                         .default = `Cancer type`)) %>%
        select(Sex, `Cancer type`, Age_lower, Age_string, Stage, median_CrI, median, `2.5%`, `97.5%`, ex) 

                
res_LE_tables_all_sens <- list.files(path = results_path_sens_CF, pattern = "^LE") %>% # Defined in 01 script
        map(~read_csv(paste0(results_path_sens_CF, .))) %>%
        bind_rows() %>%
        filter(!(grepl(c("Cervix|Breast|Uterus"), SEER_Draw) & Sex == "Male")) %>%
        filter(!(grepl(c("Prostate"), SEER_Draw) & Sex == "Female")) %>%
        filter(!(SEER_Draw== "allexcprostate" & Sex == "Female")) %>%
        rename(`Cancer type` = "SEER_Draw",
               Stage = "AJCC_stage") %>%
                mutate(`Cancer type` = case_when(`Cancer type` == "all" ~"All cancers", 
                                                 `Cancer type` == "allexcprostate" ~"All excluding prostate",
                                                 .default = `Cancer type`)) %>%
        select(Sex, `Cancer type`,Age_lower,  Age_string, Stage, median_CrI, median, `2.5%`, `97.5%`, ex) 

addWorksheet(excel_output, sheetName = "2.1 Primary LE no HR")
addWorksheet(excel_output, sheetName = "2.2 Sensitivity LE no HR")
writeData(excel_output, sheet = "2.1 Primary LE no HR", res_LE_tables_all_primary)
writeData(excel_output, sheet = "2.2 Sensitivity LE no HR", res_LE_tables_all_sens)

#' _____________________________________________________________________________
#' _____ Estimate years of life lost

res_YLL_tables_all_primary<- res_LE_tables_all_primary %>%
        # Get mid year age
        mutate(Age_mid = Age_lower + 2.5) %>%
        mutate(`median YLL` = round(ex - median, 2) ,`YLL 2.5% CrI` = round(ex - `2.5%`, 2)  ,`YLL 97.5% CrI` = round(ex - `97.5%`, 2)) %>%
        unite(YLL_CrI_temp, c(`YLL 2.5% CrI`, `YLL 97.5% CrI`), sep = " to ", remove = FALSE) %>%
        mutate(YLL_CrI = paste0("(", YLL_CrI_temp, ")")) %>%
        unite(YLL_median_CrI, c(`median YLL`, YLL_CrI), sep = " ", remove = FALSE) %>%
        select(Sex, `Cancer type`, Age_lower, Age_string, Stage, YLL_median_CrI, `median YLL`, `YLL 2.5% CrI`, `YLL 97.5% CrI`) 

res_YLL_tables_all_sens<- res_LE_tables_all_sens %>%
        # Get mid year age
        mutate(Age_mid = Age_lower + 2.5) %>%
        mutate(`median YLL` = round(ex - median, 2) ,`YLL 2.5% CrI` = round(ex - `2.5%`, 2)  ,`YLL 97.5% CrI` = round(ex - `97.5%`, 2)) %>%
        unite(YLL_CrI_temp, c(`YLL 2.5% CrI`, `YLL 97.5% CrI`), sep = " to ", remove = FALSE) %>%
        mutate(YLL_CrI = paste0("(", YLL_CrI_temp, ")")) %>%
        unite(YLL_median_CrI, c(`median YLL`, YLL_CrI), sep = " ", remove = FALSE) %>%
        select(Sex, `Cancer type`, Age_lower, Age_string, Stage, YLL_median_CrI, `median YLL`, `YLL 2.5% CrI`, `YLL 97.5% CrI`) 

addWorksheet(excel_output, sheetName = "3.1 Primary YLL")
addWorksheet(excel_output, sheetName = "3.2 Sensitivity")
writeData(excel_output, sheet = "3.1 Primary YLL", res_YLL_tables_all_primary)
writeData(excel_output, sheet = "3.2 Sensitivity", res_YLL_tables_all_sens)

#' Save the results workbook
saveWorkbook(excel_output, paste0(results_path, "results_workbook"), overwrite = TRUE)


#' _____________________________________________________________________________
#' _____ LE all cancers plot
#' 
abstract1_plot_LOLE_table <- rbind(res_LE_tables_all_primary %>%
                                           filter(Sex == "Female") %>%
                                           filter(Age_string %in% c("50 years at diagnosis", "60 years at diagnosis" ,
                                                                    "70 years at diagnosis", "80 years at diagnosis")),
                                   res_LE_tables_all_primary %>%
                                           filter(Sex == "Male") %>%
                                           filter(Age_string %in% c("50 years at diagnosis","60 years at diagnosis",
                                                                    "70 years at diagnosis", "80 years at diagnosis"))) %>%
        mutate(Age_string = case_when(Age_string == "50 years at diagnosis" ~ "50-54 years at diagnosis",
                                      Age_string == "60 years at diagnosis" ~ "60-64 years at diagnosis",
                                      Age_string == "70 years at diagnosis" ~ "70-74 years at diagnosis",
                                      Age_string == "80 years at diagnosis" ~ "80-84 years at diagnosis")) %>%
        filter(`Cancer type` %in% c("All cancers", "All excluding prostate")) %>%
        filter(!(`Cancer type` == "All excluding prostate" & Sex == "Female")) %>%
        unite(Cancers, c(Sex, `Cancer type`), sep = ":\n", remove = TRUE) %>%
        ggplot() +
        geom_bar(aes(x = forcats::fct_rev(Stage), y = median, fill = Stage), position= position_dodge(0.5), stat = "identity", width = .5) +
        geom_errorbar(aes(x = Stage,  ymin = `97.5%`, ymax = `2.5%`), position= position_dodge(0.5), width=.1) +
        facet_grid(rows = vars(Cancers), cols = vars(Age_string), switch = "y" , scales = "fixed") +
        ggsci::scale_fill_jama() +        labs(fill = "Stage", x = NULL, y = "Life expectancy") +
        theme_bw() +
        theme(strip.text.y.left = element_text(angle = 0),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              text = element_text(size = 16, face="bold"))


ggsave("~/LOLEbyStageSEER/results/EDCC_abstract/tables_and_figures/EDCCFig1.png", abstract1_plot_LOLE_table, device = "png",
       width = 250,
       height = 250,
       units = "mm")



#' _____________________________________________________________________________
#' _____ 5. Plot years of life lost for potential future publication

#' Can ignore all of the below for LE code review at this point
#' Below does some YLL graphing and then a bit on aggregate YLL

#' Plot YLL separately for each age group
for(var in levels(as.factor(res_YLL_tables_all_primary$Age_string))) {
        
        temp1 <- res_YLL_tables_all_primary %>%
                filter(Sex == "Female" & Age_string %in% c(var)) %>%
                ggplot() +
                geom_bar(aes(x = Stage, y = `median YLL`, group = Stage, fill = Stage), position= position_dodge(0.5), stat = "identity", width = .5) +
                geom_errorbar(aes(x = Stage, ymin = `YLL 97.5% CrI`, ymax = `YLL 2.5% CrI`, group = Stage), colour = "black", position= position_dodge(0.5), width=.1) +
                facet_wrap(facets = vars(`Cancer type`), ncol = 4) +
                scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592")) +
                labs(colour = "Stage", x = "Stage at diagnosis", y = "Years of Life Lost", title = "Females") +
                guides(fill="none", colour = "none")+
                theme_bw() +
                theme(plot.title =element_text(face = "bold", size = 14),
                      strip.background = element_blank(),
                      strip.text = element_text(face = "bold", size = 10),
                      axis.text.x = element_text(face = "bold", size = 12),
                      axis.text.y = element_text(face = "bold", size = 12),
                      axis.title = element_text(face = "bold", size = 12))
        # +
        #         scale_y_continuous(limits = c(0,50))
        
        temp2 <- res_YLL_tables_all_primary %>%
                filter(Sex == "Male" & Age_string %in% c( var)) %>%
                ggplot() +
                geom_bar(aes(x = Stage, y = `median YLL`, group = Stage, fill = Stage), position= position_dodge(0.5), stat = "identity", width = .5) +
                geom_errorbar(aes(x = Stage, ymin = `YLL 97.5% CrI`, ymax = `YLL 2.5% CrI`, group = Stage), colour = "black", position= position_dodge(0.5), width=.1) +
                facet_wrap(facets = vars(`Cancer type`), ncol = 4) +
                scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592")) +
                labs(colour = "Stage", x = "Stage at diagnosis", y = "", title = "Males") +
                guides(fill="none", colour = "none")+
                theme_bw() +
                theme(plot.title =element_text(face = "bold", size = 14),
                      strip.background = element_blank(),
                      strip.text = element_text(face = "bold", size = 10),
                      axis.text.y = element_text(face = "bold", size = 12),
                      axis.text.x = element_text(face = "bold", size = 12),
                      axis.title = element_text(face = "bold", size = 12)) 
        # +
        #         scale_y_continuous(limits = c(0,50))
        
        ggsave(paste0(figure_path, "YLLplot", var, ".png"),
               patchwork::wrap_plots(temp1, temp2, ncol = 2), device = "png",
               width = 300,
               height = 200,
               units = "mm")
        
}

#' _____________________________________________________________________________
#' Create dataframe and Plot YLL differences between stages separately for each age group
res_YLL_table_Stage4v1_4v3_3v2_2v1 <- res_YLL_tables_all_primary %>%
        select(Sex, `Cancer type`, Age_string, Stage, `median YLL`)%>%
        pivot_wider(names_from = Stage, values_from = `median YLL`) %>%
        mutate(`IV v III` = `IV` - `III`) %>%
        mutate(`III v II` = `III` - `II`) %>%
        mutate(`II v I` = `II` - `I`) %>%
        select(Sex, `Cancer type`, Age_string,
               `IV v III`,`III v II`, `II v I`) %>%
        mutate(highest = case_when((`IV v III` > `III v II`) & (`IV v III` > `II v I`) ~ "4v3 Highest",
                                   (`III v II` > `IV v III`) & (`III v II`  > `II v I`) ~ "3v2 Highest",
                                   (`II v I` > `IV v III`) &  (`II v I` > `III v II`) ~ "2v1 Highest",
                                   .default = NA), .after = Age_string) %>%
        pivot_longer(!Sex:highest, names_to = "Stage comparison", values_to = "Difference in years of life lost") %>%
        mutate(highest = case_when(highest == "4v3 Highest" & `Stage comparison` == "IV v III" ~ "Yes",
                                   highest == "3v2 Highest" & `Stage comparison` == "III v II" ~ "Yes",
                                   highest == "2v1 Highest" & `Stage comparison` == "II v I" ~ "Yes",
                                   .default = "No"), .after = Age_string)

for(var in levels(as.factor(res_YLL_tables_all_primary$Age_string))) {
        
        temp1 <- res_YLL_table_Stage4v1_4v3_3v2_2v1 %>%
                filter(Age_string %in% c(var) ) %>%
                mutate(`Cancer type` = fct_rev(`Cancer type`)) %>%
                ggplot() +
                # ordered alphabetically
                geom_bar(aes(x = `Cancer type`, y = `Difference in years of life lost`, fill = highest),
                         position= position_dodge(0.5), stat = "identity", width = .5) +
                facet_grid(Sex ~ fct_rev(`Stage comparison`), scales = "free_y") +
                tidytext::scale_x_reordered() +
                coord_flip() +
                ggsci::scale_color_jama() +
                labs( x = "Cancer type", y = "Difference in years of life lost") +
                theme_bw() +
                theme(strip.background = element_blank(),
                      strip.text = element_text(face = "bold", size = 14),
                      axis.text.y = element_text( size = 14),
                      axis.text.x = element_text( size = 14),
                      axis.title = element_text(face = "bold", size = 14)) +
                scale_fill_manual(values = c( "#D29A22", "#3B1C52")) +
                scale_y_continuous(breaks = c(-2,0,2,4,6,8,10,12,14,16,18,20), limits = c(-2,20)) +
                guides(fill="none", colour = "none")
        
        ggsave(paste0(figure_path, "Differences_plot_", var, ".png"),
               temp1,
               device = "png",
               width = 300,
               height = 200,
               units = "mm")
}

#______________________________________________________________________________#
#_________ Aggregate


aggregate_YLL<- TTE_data %>%
        select(SEER_Draw, Sex, Age_lower, AJCC_stage) %>%
        left_join(res_YLL_tables_all_primary ,
                  by = c( "Age_lower", "Sex" , "SEER_Draw" = "Cancer type", "AJCC_stage" = "Stage")) %>%
        group_by(SEER_Draw, AJCC_stage) %>%
        summarise(`Total YLL` = sum(`median YLL`, na.rm = TRUE)) %>%
        ungroup()

# Total years of life lost
res_total_YLL<- aggregate_YLL %>% summarise(`Total YLL` = sum(`Total YLL`))

# Percentage YLL by cancer type
res_perc_YLL_bytype <- aggregate_YLL %>%
        group_by(SEER_Draw) %>%
        summarise(`Total YLL by type` = sum(`Total YLL`, na.rm = TRUE)) %>%
        mutate(freq = paste0("(",round(`Total YLL by type` / sum(`Total YLL by type`) * 100, 1), "%", ")"))

# Percentage YLL by cancer type
res_perc_YLL_bystage  <- aggregate_YLL %>%
        group_by(AJCC_stage) %>%
        summarise(`Total YLL by stage` = sum(`Total YLL`, na.rm = TRUE)) %>%
        mutate(freq = paste0("(",round(`Total YLL by stage` / sum(`Total YLL by stage`) * 100, 0), "%", ")"))

#' Mosaic plot
factor_site_mosaic <- aggregate_YLL %>%
        left_join(res_perc_YLL_bytype  %>% select(-`Total YLL by type`), by = c("SEER_Draw")) %>%
        unite(`Cancer type (%)`, c(SEER_Draw, freq), sep = " ", remove = FALSE) %>%
        group_by(`Cancer type (%)`,SEER_Draw) %>%
        summarise(`Total YLL by type` = sum(`Total YLL`, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(SEER_Draw = as.factor(SEER_Draw)) %>%
        mutate(SEER_Draw = fct_reorder(SEER_Draw, (`Total YLL by type`))) %>%
        mutate(`Cancer type (%)` = as.factor(`Cancer type (%)`)) %>%
        mutate(`Cancer type (%)` = fct_reorder(`Cancer type (%)`, (`Total YLL by type`))) %>%
        arrange(desc(`Cancer type (%)`)) %>%
        select(`Cancer type (%)`)


# 800*600
library(ggmosaic)
plot_mosaic_LOLE <- aggregate_YLL %>%
        left_join(res_perc_YLL_bytype %>% select(-`Total YLL by type`) , by = c("SEER_Draw")) %>%
        filter(`Total YLL` >0) %>%
        unite(`Cancer type (%)`, c(SEER_Draw, freq), sep = " ", remove = FALSE) %>%
        mutate(test = fct_rev(fct_relevel(`Cancer type (%)`, as.character(pull(factor_site_mosaic))))) %>%
        ggplot() +
        ggmosaic::geom_mosaic(aes(x = product( AJCC_stage, test), weight = `Total YLL`, fill = AJCC_stage), offset = 0.001) +
        scale_x_productlist(position = "bottom", expand = c(0, 0) , labels = c("","","","","","","", "","","",rev(as.character(pull(factor_site_mosaic[1:15,]))))) +
        scale_y_productlist(expand = c(0, 0)) +
        theme_bw(base_size = 10) +
        theme(panel.grid = element_blank(),
              legend.position = "bottom",
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())+
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592"),labels = c("I (7%)", "II (11%)", "III (24%)", "IV (59%)")) +
        labs(fill = "Stage",
             y = "",
             #ç"Percentage of years of life lost attributable to each cancer type",
             #The proportion of total YLL by index cancer (total proportion across all stages for given cancer)",
             x = "Cancer type (% attributable to the total year of life lost)") +
        #"Percentage of years of life lost attributable to stage for each cancer type"
        #The proportion of total YLL by cancer stage for each cancer (total proportion across all cancers for a given stage)") +
        #guides(y = guide_axis(n.dodge = 2)) +
        coord_flip()+
        theme(axis.title = element_text(face = "bold", size = 17))


plot_mosaic_LOLE_alt <- aggregate_YLL %>%
        left_join(res_perc_YLL_bytype %>% select(-`Total YLL by type`) , by = c("NCRAS_Draw")) %>%
        unite(`Cancer type (%)`, c(NCRAS_Draw, freq), sep = " ", remove = FALSE) %>%
        mutate(test = fct_relevel(`Cancer type (%)`, as.character(pull(factor_site_mosaic)))) %>%
        ggplot() +
        ggmosaic::geom_mosaic(aes(x = product( Stage, test), weight = `Total YLL`, fill = Stage), offset = 0.001) +
        scale_x_productlist(position = "bottom", expand = c(0, 0) ,   labels = c(as.character(pull(factor_site_mosaic[1:13,])), "","","", "","","","",""))+
        scale_y_productlist(expand = c(0, 0)) +
        theme_bw(base_size = 14) +
        theme(panel.grid = element_blank(),
              legend.position = "top")+
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592"),labels = c("I (7%)", "II (11%)", "III (24%)", "IV (59%)")) +
        labs(fill = "Stage",
             y = "",
             #ç"Percentage of years of life lost attributable to each cancer type",
             #The proportion of total YLL by index cancer (total proportion across all stages for given cancer)",
             x = "Cancer type (% attributable to the total year of life lost)") +
        #"Percentage of years of life lost attributable to stage for each cancer type"
        #The proportion of total YLL by cancer stage for each cancer (total proportion across all cancers for a given stage)") +
        #guides(y = guide_axis(n.dodge = 2)) +
        #coord_flip()+
        theme(axis.text.x = element_text(angle = 45, hjust =1),
              axis.title = element_text(face = "bold", size = 17))



#______________________________________________________________________________#
#_________ 3.1. SEER population tabulation





















#______________________________________________________________________________#
#_________ 3.1. SEER population tabulation
# In script 01

#______________________________________________________________________________#
#_________ 3.2. Full results
#' Life expectancy - not included in publication
res_LE_tables_all_short <-  res_LE_tables_all_full %>%
        filter(Age_lower %in% c(50, 60, 70)) %>%
        select(Sex, SEER_Draw, Age_lower, AJCC_stage, median_CrI) %>%
        pivot_wider(names_from = AJCC_stage, values_from = c(median_CrI)) %>%
        mutate(SEER_Draw = case_when(SEER_Draw == "all" ~ "All",
                                     .default = SEER_Draw)) %>%
        rename(`Cancer site` = SEER_Draw,
               `Age at diagnosis` = Age_lower) %>%
        group_split(Sex)


plot_female_LE_by_site_stage_age <- res_LE_tables_all_full %>%
        filter(Sex == "Female") %>%
        mutate(Age_lower = paste0(Age_lower, "yr")) %>%
        ggplot() +
        geom_point(aes(x = Age_lower, y = median, colour = AJCC_stage), position= position_dodge(0.5)) +
        geom_errorbar(aes(x = Age_lower, colour = AJCC_stage, ymin = `2.5%`, ymax = `97.5%`), position= position_dodge(0.4), width=.1) +
        facet_wrap(facets = ~ SEER_Draw) +
        stat_summary(aes(x = Age_lower,  y = ex, fill = Age_lower), fun = median, geom = "point", shape = "-", size = 7, inherit.aes = FALSE, show.legend=TRUE) +
        ggsci::scale_color_jama() +
        labs(color = "Stage", x = "Age at diagnosis", y = "Female median life expectancy (years) and 95% credible interval by age and stage at diagnosis")+
        theme_bw() +
        theme(legend.position="bottom") +
        scale_fill_manual(
                guide = guide_legend(title = NULL),
                limits = c('50yr'),
                values = c("50yr" = "orange", "55yr" = "blue", "60yr" = "purple", "65yr" = "blue", "70yr" = "blue", "75yr" = "blue"),
                labels = c("Gen pop conditional life expectancy", "55yr", "60yr" , "65yr" , "70yr", "75yr" )
        )

plot_male_LE_by_site_stage_age <- res_LE_tables_all_full %>%
        filter(Sex == "Male") %>%
        mutate(Age_lower = paste0(Age_lower, "yr")) %>%
        ggplot() +
        geom_point(aes(x = Age_lower, y = median, colour = AJCC_stage), position= position_dodge(0.5)) +
        geom_errorbar(aes(x = Age_lower, colour = AJCC_stage, ymin = `2.5%`, ymax = `97.5%`), position= position_dodge(0.4), width=.1) +
        facet_wrap(facets = ~ SEER_Draw) +
        stat_summary(aes(x = Age_lower,  y = ex, fill = Age_lower), fun = median, geom = "point", shape = "-", size = 7, inherit.aes = FALSE, show.legend=TRUE) +
        ggsci::scale_color_jama() +
        labs(color = "Stage", x = "Age at diagnosis", y = "Female median life expectancy (years) and 95% credible interval by age and stage at diagnosis")+
        theme_bw() +
        theme(legend.position="bottom") +
        scale_fill_manual(
                guide = guide_legend(title = NULL),
                limits = c('50yr'),
                values = c("50yr" = "orange", "55yr" = "blue", "60yr" = "purple", "65yr" = "blue", "70yr" = "blue", "75yr" = "blue"),
                labels = c("Gen pop conditional life expectancy", "55yr", "60yr" , "65yr" , "70yr", "75yr" )
        )

#______________________________________________________________________________#
#' Loss of life expectancy

# Loss of life expectancy
res_LOLE_tables_all_full <- res_LE_tables_all_full %>%
        mutate(LOLE_median = ex - median,
               LOLE_2.5 = ex - `2.5%`,
               LOLE_97.5 = ex - `97.5%`,
        ) %>%
        mutate(across(c(LOLE_median, LOLE_2.5, LOLE_97.5 ), \(x) round(x, 2) )) %>%
        unite(LOLE_CrI_temp, c(LOLE_97.5, LOLE_2.5), sep = " to ", remove = FALSE) %>%
        mutate(LOLE_CrI = paste0("(", LOLE_CrI_temp, ")")) %>%
        unite(LOLE_median_CrI, c(LOLE_median, LOLE_CrI), sep = " ", remove = FALSE)  %>%
        mutate(SEER_Draw = case_when(SEER_Draw == "all" ~ "All cancers",
                                     SEER_Draw == "allexcprostate" ~ "All excluding prostate",
                                     .default = SEER_Draw)) 

#____ Table 2 and 3
res_LOLE_tables_all_short <- res_LOLE_tables_all_full %>%
        select(Sex, SEER_Draw, Age_lower, AJCC_stage, LOLE_median_CrI) %>%
        pivot_wider(names_from = AJCC_stage, values_from = c(LOLE_median_CrI)) %>%
        mutate(SEER_Draw = case_when(SEER_Draw == "all" ~ "All cancers",
                                     .default = SEER_Draw)) %>%
        rename(`Cancer site` = SEER_Draw,
               `Age at diagnosis` = Age_lower) 

addWorksheet(excel_output, sheetName = "Table S3 YLL female")
writeData(excel_output, sheet = "Table S3 YLL female", (res_LOLE_tables_all_short %>%
                                                               group_split(Sex))[[1]])

addWorksheet(excel_output, sheetName = "Table S4 YLL male")
writeData(excel_output, sheet = "Table S4 YLL male", (res_LOLE_tables_all_short %>%
                                                             group_split(Sex))[[2]])

#____ Figure 1
plot_LOLE_female_table <- res_LOLE_tables_all_full %>%
        filter(Sex == "Female") %>%
        filter(Age_string %in% c("50 years at diagnosis", "60 years at diagnosis" , "70 years at diagnosis")) %>%
        mutate(Age_string = case_when(Age_string == "50 years at diagnosis" ~ "50-54 years at diagnosis\nUS pop. expectancy: 33.6yrs",
                                      Age_string == "60 years at diagnosis" ~ "60-64 years at diagnosis\nUS pop. expectancy: 24.9yrs",
                                      Age_string == "70 years at diagnosis" ~ "70-74 years at diagnosis\nUS pop. expectancy: 16.9yrs")) %>%
        ggplot() +
        geom_point(aes(x = forcats::fct_rev(AJCC_stage), y = LOLE_median, colour = AJCC_stage), position= position_dodge(0.5), stat = "identity", width = .5) +
        geom_errorbar(aes(x = AJCC_stage, colour = AJCC_stage, ymin = LOLE_97.5, ymax = LOLE_2.5), position= position_dodge(0.5), width=.1) +
        facet_grid(rows = vars(SEER_Draw), cols = vars(Age_string), switch = "y" , scales = "fixed") +
        ggsci::scale_color_jama() +
        labs(colour = "Stage", x = "Cancer type", y = "Year of Life Lost") +
        theme_bw() +
        coord_flip() +
        theme(strip.text.y.left = element_text(angle = 0)) +
        theme(axis.text.y=element_blank(),
              axis.ticks.y=element_blank())

plot_female_LE_by_site_stage_age <- res_LOLE_tables_all_full %>%
        filter(Age_lower %in% c(50, 60, 70)) %>%
        filter(Sex == "Female") %>%
        mutate(Age_lower = paste0(Age_lower, "yr")) %>%
        ggplot() +
        geom_bar(aes(x = Age_lower, y = LOLE_median, group = AJCC_stage, fill = AJCC_stage), position= position_dodge(0.9), stat = "identity") +
        geom_errorbar(aes(x = Age_lower, group = AJCC_stage, ymin = LOLE_2.5, ymax = LOLE_97.5), position= position_dodge(0.9), width=.1) +
        facet_wrap(facets = ~ SEER_Draw, ncol = 4) +
        ggsci::scale_fill_jama() +
        labs(fill = "Stage", x = "Age at diagnosis", y = "Median YLL and 95% credible interval")+
        theme_bw(base_size = 14) +
        theme(legend.position="bottom") +
        theme(strip.text.y.left = element_text(angle = 0)) +
        theme(axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        theme(axis.title = element_text(face = "bold", size = 17)) 


plot_male_LE_by_site_stage_age <- res_LOLE_tables_all_full %>%
        filter(Age_lower %in% c(50, 60, 70)) %>%
        filter(Sex == "Male") %>%
        mutate(Age_lower = paste0(Age_lower, "yr")) %>%
        ggplot() +
        geom_bar(aes(x = Age_lower, y = LOLE_median, group = AJCC_stage, fill = AJCC_stage), position= position_dodge(0.9), stat = "identity") +
        geom_errorbar(aes(x = Age_lower, group = AJCC_stage, ymin = LOLE_2.5, ymax = LOLE_97.5), position= position_dodge(0.9), width=.1) +
        facet_wrap(facets = ~ SEER_Draw, ncol = 4) +
        ggsci::scale_fill_jama() +
        labs(fill = "Stage", x = "Age at diagnosis", y = "Median YLL and 95% credible interval")+
        theme_bw() +
        theme(legend.position="bottom")

ggsave("~/LOLEbyStageSEER/results/mcmc/tables_and_figures/Fig1.png", plot_LOLE_female_table, device = "png",
       width = 250,
       height = 300,
       units = "mm")

#____ Figure 2

plot_LOLE_male_table <- res_LOLE_tables_all_full %>%
        filter(Sex == "Male") %>%
        filter(Age_string %in% c("50 years at diagnosis","60 years at diagnosis", "70 years at diagnosis")) %>%
        mutate(Age_string = case_when(Age_string == "50 years at diagnosis" ~ "50-54  years at diagnosis\nUS pop. expectancy: 30.0yrs",
                                      Age_string == "60 years at diagnosis" ~ "60-64 years at diagnosis\nUS pop. expectancy: 21.9yrs",
                                      Age_string == "70 years at diagnosis" ~ "70-74 years at diagnosis\nUS pop. expectancy: 14.7yrs")) %>%
        ggplot() +
        geom_point(aes(x = forcats::fct_rev(AJCC_stage), y = LOLE_median, colour = AJCC_stage), position= position_dodge(0.5), stat = "identity", width = .5) +
        geom_errorbar(aes(x = AJCC_stage, colour = AJCC_stage, ymin = LOLE_97.5, ymax = LOLE_2.5), position= position_dodge(0.5), width=.1) +
        facet_grid(rows = vars(SEER_Draw), cols = vars(Age_string), switch = "y" , scales = "fixed") +
        ggsci::scale_color_jama() +
        labs(colour = "Stage", x = "Cancer type", y = "Year of Life Lost") +
        theme_bw() +
        coord_flip() +
        theme(strip.text.y.left = element_text(angle = 0)) +
        theme(axis.text.y=element_blank(),
              axis.ticks.y=element_blank())

ggsave("~/LOLEbyStageSEER/results/mcmc/tables_and_figures/Fig2.png", plot_LOLE_male_table, device = "png",
       width = 250,
       height = 300,
       units = "mm")

#' #' Revised figures for EDCC abstract
#' #' abstract Figure 1 - All cancers by age
#' 
#' abstract1_plot_LOLE_table <- rbind(res_LOLE_tables_all_full %>%
#'                           filter(Sex == "Female") %>%
#'                           filter(Age_string %in% c("50 years at diagnosis", "60 years at diagnosis" , "70 years at diagnosis")),
#'                   res_LOLE_tables_all_full %>%
#'                           filter(Sex == "Male") %>%
#'                           filter(Age_string %in% c("50 years at diagnosis","60 years at diagnosis", "70 years at diagnosis"))) %>%
#'         mutate(Age_string = case_when(Age_string == "50 years at diagnosis" ~ "50-54 years at diagnosis",
#'                                       Age_string == "60 years at diagnosis" ~ "60-64 years at diagnosis",
#'                                       Age_string == "70 years at diagnosis" ~ "70-74 years at diagnosis")) %>%
#'         filter(SEER_Draw %in% c("All cancers", "All excluding prostate")) %>%
#'         filter(!(SEER_Draw == "All excluding prostate" & Sex == "Female")) %>%
#'         unite(Cancers, c(Sex, SEER_Draw), sep = ":\n", remove = TRUE) %>%
#'         ggplot() +
#'         geom_bar(aes(x = forcats::fct_rev(AJCC_stage), y = LOLE_median, fill = AJCC_stage), position= position_dodge(0.5), stat = "identity", width = .5) +
#'         geom_errorbar(aes(x = AJCC_stage,  ymin = LOLE_97.5, ymax = LOLE_2.5), position= position_dodge(0.5), width=.1) +
#'         facet_grid(rows = vars(Cancers), cols = vars(Age_string), switch = "y" , scales = "fixed") +
#'         ggsci::scale_fill_jama() +
#'         labs(fill = "Stage", x = NULL, y = "Years of Life Lost (YLL)") +
#'         theme_bw() +
#'         coord_flip() +
#'         theme(strip.text.y.left = element_text(angle = 0),
#'               axis.text.y=element_blank(),
#'               axis.ticks.y=element_blank(),
#'               text = element_text(size = 16, face="bold")) 
#' 
#' 
#' ggsave("~/LOLEbyStageSEER/results/EDCC_abstract/tables_and_figures/EDCCFig1.png", abstract1_plot_LOLE_table, device = "png",
#'        width = 250,
#'        height = 250,
#'        units = "mm")
#' 
#' #' abstrqact Figure 2 cancer types for age 60
#' # Long 700*800
#' abstract2_plot_LOLE_table <- rbind(res_LOLE_tables_all_full %>%
#'                                            filter(Sex == "Female") %>%
#'                                            filter(Age_string %in% c("60 years at diagnosis")),
#'                                    res_LOLE_tables_all_full %>%
#'                                            filter(Sex == "Male") %>%
#'                                            filter(Age_string %in% c("60 years at diagnosis"))) %>%
#'         filter(!(SEER_Draw %in% c("All cancers", "All excluding prostate"))) %>%
#'         filter(!(SEER_Draw == "Prostate" & Sex == "Female")) %>%
#'         filter(!(SEER_Draw == "Cervix" & Sex == "Male")) %>%
#'         filter(!(SEER_Draw == "Uterus" & Sex == "Male")) %>%
#'         filter(!(SEER_Draw == "Ovary" & Sex == "Male")) %>%
#'         # Ordered by aggregate YLL size - need the run 05 first for this
#'         mutate(SEER_Draw = as.factor(SEER_Draw)) %>%
#'         mutate(SEER_Draw = fct_relevel(SEER_Draw, factor_site_Fig2)) %>%
#'         mutate(SEER_Draw = fct_rev(SEER_Draw)) %>%
#'         ggplot() +
#'         geom_bar(aes(x = forcats::fct_rev(AJCC_stage), y = LOLE_median, fill = AJCC_stage), position= position_dodge(0.5), stat = "identity", width = .9) +
#'         geom_errorbar(aes(x = AJCC_stage,  ymin = LOLE_97.5, ymax = LOLE_2.5), position= position_dodge(0.5), width=.1) +
#'         facet_grid(rows = vars(SEER_Draw), cols = vars(Sex), switch = "y" , scales = "fixed") +
#'         ggsci::scale_fill_jama() +
#'         labs(fill = "Stage", x = NULL, y = "Years of Life Lost (YLL)") +
#'         theme_bw() +
#'         coord_flip() +
#'         theme(strip.text.y.left = element_text(angle = 0),
#'               axis.text.y=element_blank(),
#'               axis.ticks.y=element_blank(),
#'               text = element_text(size = 16, face="bold")) 
#' 
#' 
#' ggsave("~/LOLEbyStageSEER/results/EDCC_abstract/tables_and_figures/EDCCFig2.png", abstract2_plot_LOLE_table, device = "png",
#'        width = 200,
#'        height = 250,
#'        units = "mm")

#_________ 3.3. Stage 4 vs 3 comparison and stage 3 v 2

res_LOLE_table_Stage4v1_4v3_3v2_2v1_60 <- res_LOLE_tables_all_full %>%
        filter(Age_lower %in% c(60)) %>%
        select(Sex, SEER_Draw, Age_lower, AJCC_stage, LOLE_median) %>%
        pivot_wider(names_from = AJCC_stage, values_from = c(LOLE_median)) %>%
        mutate(`IV v III` = `IV` - `III`) %>%
        mutate(`III v II` = `III` - `II`) %>%
        mutate(`II v I` = `II` - `I`) %>%
        mutate(`IV v I` = `IV` - `I`) %>%
        mutate(SEER_Draw = case_when(SEER_Draw == "all" ~ "All",
                                     .default = SEER_Draw)) %>%
        rename(`Cancer site` = SEER_Draw,
               `Age at diagnosis` = Age_lower) %>%
        select(Sex, `Cancer site`, `Age at diagnosis`,
               `IV v III`,`III v II`, `II v I`, `IV v I`) %>%
        pivot_longer(!Sex:`Age at diagnosis`, names_to = "Stage comparison", values_to = "Difference in loss of life expectancy (years)") 

res_LOLE_table_Stage4v3_3v2_2v1_60  <- res_LOLE_table_Stage4v1_4v3_3v2_2v1_60 %>%
        filter(`Stage comparison` != "IV v I") %>%
        mutate(across(where(is.character), as.factor)) %>%
        mutate(`Stage comparison` = fct_relevel(`Stage comparison`, c("IV v III","III v II", "II v I") ))
















#'_____________ Plot faceted
#'Unused now
#'First set order according to difference between stage IV and III
# factor_order_male_4v3 <- as.character(res_LOLE_table_Stage4v3_3v2_2v1_60 %>%
#                                               filter(Sex == "Male", `Stage comparison` == "IV v III" ) %>%
#                                               #arrange(desc(`Difference in loss of life expectancy (years)`)) %>%
#                                               mutate(`Cancer site` = fct_reorder(`Cancer site`, desc(`Difference in loss of life expectancy (years)`))) %>%
#                                               arrange(desc(`Cancer site`)) %>%
#                                               select(`Cancer site`) %>%
#                                               distinct() %>%
#                                               pull(`Cancer site`))
# 
# # 700 x 500
# plot_LOLE_male_Stage4v3_3v2_2v1_60 <-  res_LOLE_table_Stage4v3_3v2_2v1_60 %>%
#         filter(Sex == "Male") %>%
#         filter(`Age at diagnosis` == 60) %>%
#         mutate(`Cancer site` = fct_rev(`Cancer site`)) %>%
#         ggplot() + 
#         # ordered alphabetically
#         geom_bar(aes(x = `Cancer site`,
#                      y = `Difference in loss of life expectancy (years)`), position= position_dodge(0.5), stat = "identity", width = .5) +
#         # ordered by differrence
#         # geom_bar(aes(x = fct_relevel(`Cancer site`, factor_order_male_4v3),
#         #              y = `Difference in loss of life expectancy (years)`), position= position_dodge(0.5), stat = "identity", width = .5) +
#         facet_grid( ~ `Stage comparison`, scales = "fixed") +
#         scale_x_reordered() +
#         coord_flip() +
#         ggsci::scale_color_jama() +
#         labs(Fill = "Stage", x = "Cancer type", y = "Difference in loss of life expectancy (years)") +
#         theme_bw() 
# 
# factor_order_female_4v3 <- as.character(res_LOLE_table_Stage4v3_3v2_2v1_60 %>%
#                                                 filter(Sex == "Female", `Stage comparison` == "IV v III" ) %>%
#                                                 #arrange(desc(`Difference in loss of life expectancy (years)`)) %>%
#                                                 mutate(`Cancer site` = fct_reorder(`Cancer site`, desc(`Difference in loss of life expectancy (years)`))) %>%
#                                                 arrange(desc(`Cancer site`)) %>%
#                                                 select(`Cancer site`) %>%
#                                                 distinct() %>%
#                                                 pull(`Cancer site`))
# 
# plot_LOLE_female_Stage4v3_3v2_2v1_60 <-  res_LOLE_table_Stage4v3_3v2_2v1_60 %>%
#         filter(Sex == "Female") %>%
#         filter(`Age at diagnosis` == 60) %>%
#         mutate(`Cancer site` = fct_rev(`Cancer site`)) %>%
#         ggplot() + 
#         # ordered alphabetically
#         geom_bar(aes(x = `Cancer site`,
#                      y = `Difference in loss of life expectancy (years)`), position= position_dodge(0.5), stat = "identity", width = .5) +
#         # ordered by differrence
#         # geom_bar(aes(x = fct_relevel(`Cancer site`, factor_order_female_4v3),
#         #              y = `Difference in loss of life expectancy (years)`), position= position_dodge(0.5), stat = "identity", width = .5) +
#         facet_grid( ~ `Stage comparison`, scales = "fixed") +
#         scale_x_reordered() +
#         coord_flip() +
#         ggsci::scale_color_jama() +
#         labs(Fill = "Stage", x = "Cancer type", y = "Difference in loss of life expectancy (years)") +
#         theme_bw() 

plot_YLL_Stage4v3_3v2_2v1_60 <-  res_LOLE_table_Stage4v3_3v2_2v1_60 %>%
        filter(`Age at diagnosis` == 60) %>%
        mutate(`Cancer site` = fct_rev(`Cancer site`)) %>%
        ggplot() + 
        # ordered alphabetically
        geom_bar(aes(x = `Cancer site`,
                     y = `Difference in loss of life expectancy (years)`), position= position_dodge(0.5), stat = "identity", width = .5) +
        # ordered by differrence
        # geom_bar(aes(x = fct_relevel(`Cancer site`, factor_order_female_4v3),
        #              y = `Difference in loss of life expectancy (years)`), position= position_dodge(0.5), stat = "identity", width = .5) +
        facet_grid(Sex ~ `Stage comparison`, scales = "free_y") +
        scale_x_reordered() +
        coord_flip() +
        ggsci::scale_color_jama() +
        labs( x = "Cancer type", y = "Difference in Year of Life Lost (years)") +
        theme(axis.title = element_text(face = "bold", size = 17)) +
        theme_bw(base_size = 14) 
        

ggsave("~/LOLEbyStageSEER/results/mcmc/tables_and_figures/Fig3.png", plot_YLL_Stage4v3_3v2_2v1_60, device = "png",
       width = 250,
       height = 300,
       units = "mm")


#' Unused now
#'_____________  Stage 4 vs 1 comparison
# factor_order_male_4v1 <- as.character(res_LOLE_table_Stage4v1_4v3_3v2_2v1_60 %>%
#                                               filter(Sex == "Male", `Stage comparison` == "IV v I" ) %>%
#                                               mutate(across(where(is.character), as.factor)) %>%
#                                               mutate(`Cancer site` = fct_reorder(`Cancer site`, (`Difference in loss of life expectancy (years)`), .na_rm = FALSE)) %>%
#                                               arrange(`Cancer site`) %>%
#                                               select(`Cancer site`) %>%
#                                               distinct() %>%
#                                               pull(`Cancer site`))
# 
# factor_order_female_4v1 <- as.character(res_LOLE_table_Stage4v1_4v3_3v2_2v1_60 %>%
#                                                 filter(Sex == "Female", `Stage comparison` == "IV v I" ) %>%
#                                                 mutate(across(where(is.character), as.factor)) %>%
#                                                 mutate(`Cancer site` = fct_reorder(`Cancer site`, (`Difference in loss of life expectancy (years)`), .na_rm = FALSE)) %>%
#                                                 arrange(`Cancer site`) %>%
#                                                 select(`Cancer site`) %>%
#                                                 distinct() %>%
#                                                 pull(`Cancer site`))
# 
# plot_LOLE_male_Stage4v1 <-  res_LOLE_table_Stage4v3_3v2_2v1_60 %>%
#         filter(Sex == "Male") %>%
#         filter(`Age at diagnosis` == 60) %>%
#         ggplot() + 
#         geom_bar(aes(x = fct_relevel(`Cancer site`, factor_order_male_4v1),
#                      y = `Difference in loss of life expectancy (years)`,
#                      fill = `Stage comparison`), stat = "identity", width = .5) +
#         scale_x_reordered() +
#         coord_flip() +
#         ggsci::scale_fill_jama() +
#         labs(Fill = "Stage", x = "Cancer type", y = "Difference in loss of life expectancy (years)") +
#         theme_bw()
# 
# plot_LOLE_female_Stage4v1 <-  res_LOLE_table_Stage4v3_3v2_2v1_60 %>%
#         filter(Sex == "Female") %>%
#         filter(`Age at diagnosis` == 60) %>%
#         ggplot() + 
#         geom_bar(aes(x = fct_relevel(`Cancer site`, factor_order_female_4v1),
#                      y = `Difference in loss of life expectancy (years)`,
#                      fill = `Stage comparison`), stat = "identity", width = .5) +
#         scale_x_reordered() +
#         coord_flip() +
#         ggsci::scale_fill_jama() +
#         labs(Fill = "Stage", x = "Cancer type", y = "Difference in loss of life expectancy (years)") +
#         theme_bw()
