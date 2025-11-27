################################################################################
#______________________________________________________________________________#
#' _____ Results tables and plots
#______________________________________________________________________________#
################################################################################
#' _____________________________________________________________________________
#' Libraries and data
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(stringr)
library(tidyverse)
library(purrr)
library(readxl)

################################################################################
#' _____________________________________________________________________________
#' Load data: Unadjusted all-cause costs
#' _____________________________________________________________________________
#' _____ Total costs

res_path_allcause_unadj_total_costs_8cancers <- "~/Miscellaneous/HDI/Final_results/All cause costs/Unadjusted/Descriptives and unadjusted allcause results - 8 cancer sites/Costs_GRAIL_2024-11-06.xlsx"
res_path_allcause_unadj_total_costs_ColoSplits <- "~/Miscellaneous/HDI/Final_results/All cause costs/Unadjusted/Descriptives and unadjusted allcause results - colorectal and lymphoma split/Costs_GRAIL_colorectal_2024-11-21.xlsx"
res_path_allcause_unadj_total_costs_lymphSplits<- "~/Miscellaneous/HDI/Final_results/All cause costs/Unadjusted/Descriptives and unadjusted allcause results - colorectal and lymphoma split/Costs_GRAIL_lymphoma_2024-11-22.xlsx"

df_allcause_unadj_total_costs <- rbind(
        
        # The 8 original cancer
        res_path_allcause_unadj_total_costs_8cancers %>% 
        excel_sheets() %>% 
        set_names() %>% 
        map_df(~ read_excel(path = res_path_allcause_unadj_total_costs_8cancers, sheet = .x), .id = "Sheet") %>%
        rename(Variable_main = `...1`,
               Variable_type = `...2`) %>% 
        fill(Variable_main, .direction = "down") %>%
        unite("Variable", Variable_main:Variable_type, na.rm = TRUE, remove = TRUE) %>%
        mutate(cancer_type = case_when(Sheet == "T2costs_lung" ~ "Lung",
                                       Sheet == "T2costs_colorectal" ~ "Colorectal",
                                       Sheet == "T2costs_head_neck" ~ "Head and neck",
                                       Sheet == "T2costs_liver" ~ "Liver",
                                       Sheet == "T2costs_lymphoma" ~ "Lymphoma",
                                       Sheet == "T2costs_oesophagus" ~ "Oesophagus",
                                       Sheet == "T2costs_ovary" ~ "Ovary",
                                       Sheet == "T2costs_pancreas" ~ "Pancreas",
                                       .default = Sheet )) %>%
        select(-Sheet),
        
        #The colorectal splits
        res_path_allcause_unadj_total_costs_ColoSplits %>% 
                excel_sheets() %>% 
                set_names() %>% 
                map_df(~ read_excel(path = res_path_allcause_unadj_total_costs_ColoSplits, sheet = .x), .id = "Sheet") %>%
                rename(Variable_main = `...1`,
                       Variable_type = `...2`) %>% 
                fill(Variable_main, .direction = "down") %>%
                unite("Variable", Variable_main:Variable_type, na.rm = TRUE, remove = TRUE) %>%
                mutate(cancer_type = case_when(Sheet == "T2costs_Colon" ~ "Colon and rectosigmoid junction",
                                               Sheet == "T2costs_Rectum" ~ "Rectum",
                                               .default = Sheet  ) ) %>%
                select(-Sheet),
        
        # The lymphoma splits
        res_path_allcause_unadj_total_costs_lymphSplits %>% 
                excel_sheets() %>% 
                set_names() %>% 
                map_df(~ read_excel(path = res_path_allcause_unadj_total_costs_lymphSplits, sheet = .x), .id = "Sheet") %>%
                rename(Variable_main = `...1`,
                       Variable_type = `...2`) %>% 
                fill(Variable_main, .direction = "down") %>%
                unite("Variable", Variable_main:Variable_type, na.rm = TRUE, remove = TRUE) %>%
                mutate(cancer_type = case_when(Sheet == "T2costs_Hodgkins" ~ "Lymphoma: Hodgkins",
                                               Sheet == "T2costs_High_grade_NHL" ~ "Lymphoma: High grade NHL",
                                               Sheet == "T2costs_Low_grade_NHL" ~ "Lymphoma: Low grade NHL",
                                               .default = Sheet  ) ) %>%
                select(-Sheet)
)

#' _____ Annual costs

res_path_allcause_unadj_annual_costs_8cancers <- "~/Miscellaneous/HDI/Final_results/All cause costs/Unadjusted/Descriptives and unadjusted allcause results - 8 cancer sites/Costs_annual_GRAIL_2025-01-23.xlsx"
res_path_allcause_unadj_annual_costs_ColoSplits <- "~/Miscellaneous/HDI/Final_results/All cause costs/Unadjusted/Descriptives and unadjusted allcause results - colorectal and lymphoma split/Costs_annual_GRAIL_colorectal_2024-11-21.xlsx"
res_path_allcause_unadj_annual_costs_lymphSplits <- "~/Miscellaneous/HDI/Final_results/All cause costs/Unadjusted/Descriptives and unadjusted allcause results - colorectal and lymphoma split/Costs_annual_GRAIL_lymphoma_2024-11-25.xlsx"

df_allcause_unadj_annual_costs <- rbind(
        
        # The 8 original cancer
        res_path_allcause_unadj_annual_costs_8cancers %>% 
                excel_sheets() %>% 
                set_names() %>% 
                map_df(~ read_excel(path = res_path_allcause_unadj_annual_costs_8cancers, sheet = .x), .id = "Sheet") %>%
                rename(Variable_main = `...1`,
                       Variable_type = `...2`) %>% 
                fill(Variable_main, .direction = "down") %>%
                unite("Variable", Variable_main:Variable_type, na.rm = TRUE, remove = TRUE) %>%
                mutate(cancer_type = case_when(Sheet == "T4_lung" ~ "Lung",
                                               Sheet == "T4_colorectal" ~ "Colorectal",
                                               Sheet == "T4_head_neck" ~ "Head and neck",
                                               Sheet == "T4_liver" ~ "Liver",
                                               Sheet == "T4_lymphoma" ~ "Lymphoma",
                                               Sheet == "T4_oesophagus" ~ "Oesophagus",
                                               Sheet == "T4_ovary" ~ "Ovary",
                                               Sheet == "T4_pancreas" ~ "Pancreas",
                                               .default = Sheet )) %>%
                select(-Sheet),
        
        #The colorectal splits
        res_path_allcause_unadj_annual_costs_ColoSplits %>% 
                excel_sheets() %>% 
                set_names() %>% 
                map_df(~ read_excel(path = res_path_allcause_unadj_annual_costs_ColoSplits, sheet = .x), .id = "Sheet") %>%
                rename(Variable_main = `...1`,
                       Variable_type = `...2`) %>% 
                fill(Variable_main, .direction = "down") %>%
                unite("Variable", Variable_main:Variable_type, na.rm = TRUE, remove = TRUE) %>%
                mutate(cancer_type = case_when(Sheet == "T4_Colon" ~ "Colon and rectosigmoid junction",
                                               Sheet == "T4_Rectum" ~ "Rectum",
                                               .default = Sheet  ) ) %>%
                select(-Sheet),
        
        # The lymphoma splits
        res_path_allcause_unadj_annual_costs_lymphSplits %>% 
                excel_sheets() %>% 
                set_names() %>% 
                map_df(~ read_excel(path = res_path_allcause_unadj_annual_costs_lymphSplits, sheet = .x), .id = "Sheet") %>%
                rename(Variable_main = `...1`,
                       Variable_type = `...2`) %>% 
                fill(Variable_main, .direction = "down") %>%
                unite("Variable", Variable_main:Variable_type, na.rm = TRUE, remove = TRUE) %>%
                mutate(cancer_type = case_when(Sheet == "T4_Hodgkins" ~ "Lymphoma: Hodgkins",
                                               Sheet == "T4_High_grade_NHL" ~ "Lymphoma: High grade NHL",
                                               Sheet == "T4_Low_grade_NHL" ~ "Lymphoma: Low grade NHL",
                                               .default = Sheet  ) ) %>%
                select(-Sheet)
)


#' _____ PoC costs

res_path_allcause_unadj_PoC_costs_8cancers <- "~/Miscellaneous/HDI/Final_results/All cause costs/Unadjusted/Descriptives and unadjusted allcause results - 8 cancer sites/Costs_phase_GRAIL_2024-11-06.xlsx"
res_path_allcause_unadj_PoC_costs_ColoSplits <- "~/Miscellaneous/HDI/Final_results/All cause costs/Unadjusted/Descriptives and unadjusted allcause results - colorectal and lymphoma split/Costs_phase_GRAIL_colorectal_2024-11-21.xlsx"
res_path_allcause_unadj_PoC_costs_lymphSplits <- "~/Miscellaneous/HDI/Final_results/All cause costs/Unadjusted/Descriptives and unadjusted allcause results - colorectal and lymphoma split/Costs_phase_GRAIL_lymphoma_2024-11-25.xlsx"

df_allcause_unadj_PoC_costs <- rbind(
        
        # The 8 original cancer
        res_path_allcause_unadj_PoC_costs_8cancers %>% 
                excel_sheets() %>% 
                set_names() %>% 
                map_df(~ read_excel(path = res_path_allcause_unadj_PoC_costs_8cancers, sheet = .x), .id = "Sheet") %>%
                rename(Variable_main = `...1`,
                       Variable_type = `...2`) %>% 
                fill(Variable_main, .direction = "down") %>%
                unite("Variable", Variable_main:Variable_type, na.rm = TRUE, remove = TRUE) %>%
                mutate(cancer_type = case_when(Sheet == "T3_lung" ~ "Lung",
                                               Sheet == "T3_colorectal" ~ "Colorectal",
                                               Sheet == "T3_head_neck" ~ "Head and neck",
                                               Sheet == "T3_liver" ~ "Liver",
                                               Sheet == "T3_lymphoma" ~ "Lymphoma",
                                               Sheet == "T3_oesophagus" ~ "Oesophagus",
                                               Sheet == "T3_ovary" ~ "Ovary",
                                               Sheet == "T3_pancreas" ~ "Pancreas",
                                               .default = Sheet )) %>%
                select(-Sheet),
        
        #The colorectal splits
        res_path_allcause_unadj_PoC_costs_ColoSplits %>% 
                excel_sheets() %>% 
                set_names() %>% 
                map_df(~ read_excel(path = res_path_allcause_unadj_PoC_costs_ColoSplits, sheet = .x), .id = "Sheet") %>%
                rename(Variable_main = `...1`,
                       Variable_type = `...2`) %>% 
                fill(Variable_main, .direction = "down") %>%
                unite("Variable", Variable_main:Variable_type, na.rm = TRUE, remove = TRUE) %>%
                mutate(cancer_type = case_when(Sheet == "T3_Colon" ~ "Colon and rectosigmoid junction",
                                               Sheet == "T3_Rectum" ~ "Rectum",
                                               .default = Sheet  ) ) %>%
                select(-Sheet),
        
        # The lymphoma splits
        res_path_allcause_unadj_PoC_costs_lymphSplits %>% 
                excel_sheets() %>% 
                set_names() %>% 
                map_df(~ read_excel(path = res_path_allcause_unadj_PoC_costs_lymphSplits, sheet = .x), .id = "Sheet") %>%
                rename(Variable_main = `...1`,
                       Variable_type = `...2`) %>% 
                fill(Variable_main, .direction = "down") %>%
                unite("Variable", Variable_main:Variable_type, na.rm = TRUE, remove = TRUE) %>%
                mutate(cancer_type = case_when(Sheet == "T3_Hodgkins" ~ "Lymphoma: Hodgkins",
                                               Sheet == "T3_High_grade_NHL" ~ "Lymphoma: High grade NHL",
                                               Sheet == "T3_Low_grade_NHL" ~ "Lymphoma: Low grade NHL",
                                               .default = Sheet  ) ) %>%
                select(-Sheet)
)


################################################################################
#' _____________________________________________________________________________
#' Tabulate data: Unadjusted all-cause costs
#' _____________________________________________________________________________

#' _____________________________________________________________________________
#' Total costs by stage

#' _____ Create dataframe
df_allcause_unadj_bystage_total_costs <- df_allcause_unadj_total_costs %>% 
        pivot_longer(cols = cost_2014:cost_2017, names_to = "year") %>%
        separate_wider_delim(cols = value, delim = " ", names = c("mean", "sd", "se"), names_sep = "_",
                             too_few = "align_start") %>%
        mutate(value_sd = as.numeric(str_sub(value_sd, start = 2L, end = -2L)),
               value_se = as.numeric(str_sub(value_se, start = 2L, end = -2L)),
               value_mean = as.numeric(value_mean)) %>%
        filter(grepl(c("Stage at diagnosis"), Variable)) %>%
        mutate(value_2.5CI  = value_mean - qnorm(0.975) * value_se,
               value_97.5CI  = value_mean + qnorm(0.975) * value_se) %>%
        mutate(stage = str_sub(Variable, start = 20L, end = -1L)) %>%
        select(cancer_type, stage, year, value_mean, value_2.5CI, value_97.5CI) %>%
        mutate(year = str_sub(year, start = 6L, end = -1L)) %>% 
        mutate(across(where(is.character), as.factor)) %>%
        #mutate(stage = forcats::fct_rev(stage)) %>%
        mutate(cancer_type = forcats::fct_rev(cancer_type)) %>%
        filter(stage != "0" & stage != "Unknown/Missing") 








#' _____ Plot of costs by cancer type, stage and year of diagnosis

plot_allcause_unadj__bystage_8cancers <- df_allcause_unadj_bystage_total_costs %>%
        filter(cancer_type %in% c( "Lung", "Colorectal", "Head and neck", "Liver", "Lymphoma", "Oesophagus", "Ovary", "Pancreas")) %>%
        ggplot() + 
        geom_bar(aes(y = value_mean, x = cancer_type, group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = value_2.5CI, ymin = value_97.5CI, x = cancer_type, group = fct_rev(stage)),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        # ordered by differrence
        # geom_bar(aes(x = fct_relevel(`Cancer site`, factor_order_male_4v3),
        #              y = `Difference in loss of life expectancy (years)`), position= position_dodge(0.5), stat = "identity", width = .5) +
        facet_grid( ~ year) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Mean costs") +
        theme_bw(base_size = 14) +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              axis.ticks.y=element_blank())  

plot_allcause_unadj_bystage_splits <- df_allcause_unadj_bystage_total_costs %>%
        filter(cancer_type %in% c( "Colorectal", "Colon and rectosigmoid junction", "Rectum",
                                   "Lymphoma", "Lymphoma: Hodgkins", "Lymphoma: High grade NHL", "Lymphoma: Low grade NHL")) %>%
        mutate(cancer_type = fct_rev(fct_relevel(cancer_type, c( "Colorectal", "Colon and rectosigmoid junction", "Rectum",
                                                         "Lymphoma", "Lymphoma: Hodgkins", "Lymphoma: High grade NHL", "Lymphoma: Low grade NHL")))) %>%
        ggplot() + 
        geom_bar(aes(y = value_mean, x = cancer_type, group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = value_2.5CI, ymin = value_97.5CI, x = cancer_type, group = fct_rev(stage)),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        # ordered by differrence
        # geom_bar(aes(x = fct_relevel(`Cancer site`, factor_order_male_4v3),
        #              y = `Difference in loss of life expectancy (years)`), position= position_dodge(0.5), stat = "identity", width = .5) +
        facet_grid( ~ year) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Mean costs") +
        theme_bw(base_size = 14) +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              axis.ticks.y=element_blank())  

plot_allcause_unadj_bystage_4bad <- df_allcause_unadj_bystage_total_costs %>%
        filter(cancer_type %in% c( "Lung", "Liver", "Oesophagus", "Pancreas")) %>%
        filter(year == "2014") %>%
        ggplot() + 
        geom_bar(aes(y = value_mean, x = cancer_type, group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = value_2.5CI, ymin = value_97.5CI, x = cancer_type, group = fct_rev(stage)),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        # ordered by differrence
        # geom_bar(aes(x = fct_relevel(`Cancer site`, factor_order_male_4v3),
        #              y = `Difference in loss of life expectancy (years)`), position= position_dodge(0.5), stat = "identity", width = .5) +
        facet_grid( ~ year) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage", x = "", y = "") +
        theme_bw(base_size = 14) +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              axis.ticks.y=element_blank())  

plot_allcause_unadj_bystage_4good <- df_allcause_unadj_bystage_total_costs %>%
        filter(cancer_type %in% c( "Colorectal", "Head and neck",  "Lymphoma", "Ovary")) %>%
        filter(year == "2014") %>%
        ggplot() + 
        geom_bar(aes(y = value_mean, x = cancer_type, group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = value_2.5CI, ymin = value_97.5CI, x = cancer_type, group = fct_rev(stage)),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        # ordered by differrence
        # geom_bar(aes(x = fct_relevel(`Cancer site`, factor_order_male_4v3),
        #              y = `Difference in loss of life expectancy (years)`), position= position_dodge(0.5), stat = "identity", width = .5) +
        facet_grid( ~ year) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage", x = "", y = "") +
        theme_bw(base_size = 14) +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              axis.ticks.y=element_blank())  


#' _____________________________________________________________________________
#' Annual costs by stage

#' _____ Create dataframe Plot of costs by cancer type, stage and year of diagnosis
df_allcause_unadj_bystage_annual_costs <- df_allcause_unadj_annual_costs %>% 
        rename(`Diag phase` = ac_Diagnosis,
               `0-1 years` = ac_Treat0_1_phase,
               `1-2 years` = ac_Treat1_2_phase,
               `2-3 years` = ac_Treat2_3_phase,
               `3-4 years` = ac_Treat3_4_phase,
               `4-5 years` = ac_Treat4_5_phase,
               `5-6 years` = ac_Treat5_phase
               ) %>% 
        pivot_longer(cols = `Diag phase`:`5-6 years`, names_to = "Period", values_to = "value") %>%
        mutate(Period = factor(Period, levels = c("Diag phase", "0-1 years", "1-2 years", "2-3 years", "3-4 years", "4-5 years", "5-6 years") )) %>%
        separate_wider_delim(cols = value, delim = " ", names = c("mean", "sd", "se"), names_sep = "_",
                             too_few = "align_start") %>%
        mutate(value_sd = as.numeric(str_sub(value_sd, start = 2L, end = -2L)),
               value_se = as.numeric(str_sub(value_se, start = 2L, end = -2L)),
               value_mean = as.numeric(value_mean)) %>%
        filter(grepl(c("Stage at diagnosis"), Variable)) %>%
        mutate(value_2.5CI  = value_mean - qnorm(0.975) * value_se,
               value_97.5CI  = value_mean + qnorm(0.975) * value_se) %>% 
        mutate(stage = str_sub(Variable, start = 20L, end = -1L)) %>%
        mutate(across(where(is.character), as.factor)) %>%
        select(cancer_type, stage, Period, value_mean, value_2.5CI, value_97.5CI) %>%
        mutate(stage = forcats::fct_rev(stage)) %>%
        mutate(cancer_type = forcats::fct_rev(cancer_type)) %>%
        filter(stage != "0" & stage != "Unknown/Missing")

#' _____ Create dataframe Plot of costs by cancer type, stage and year of diagnosis
plot_allcause_unadj_annual_bystage_8cancers <- df_allcause_unadj_bystage_annual_costs %>%
        filter(cancer_type %in% c( "Lung", "Colorectal", "Head and neck", "Liver", "Lymphoma", "Oesophagus", "Ovary", "Pancreas")) %>%
        ggplot() + 
        geom_bar(aes(y = value_mean, x = stage, group = stage, fill = stage),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = value_2.5CI, ymin = value_97.5CI, x = stage, group = stage),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        # ordered by differrence
        # geom_bar(aes(x = fct_relevel(`Cancer site`, factor_order_male_4v3),
        #              y = `Difference in loss of life expectancy (years)`), position= position_dodge(0.5), stat = "identity", width = .5) +
        facet_grid(rows = vars(cancer_type), cols = vars(Period) , switch = "y", scales = "fixed") +
        #facet_grid( ~ Period) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Mean costs") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              strip.text.y.left = element_text(angle = 0),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) 

plot_allcause_unadj_annual_bystage_split <- df_allcause_unadj_bystage_annual_costs %>%
        filter(cancer_type %in% c( "Colorectal", "Colon and rectosigmoid junction", "Rectum",
                                   "Lymphoma", "Lymphoma: Hodgkins", "Lymphoma: High grade NHL", "Lymphoma: Low grade NHL")) %>%
        mutate(cancer_type = fct_rev(fct_relevel(cancer_type, c( "Colorectal", "Colon and rectosigmoid junction", "Rectum",
                                                                 "Lymphoma", "Lymphoma: Hodgkins", "Lymphoma: High grade NHL", "Lymphoma: Low grade NHL")))) %>%
        ggplot() + 
        geom_bar(aes(y = value_mean, x = stage, group = stage, fill = stage),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = value_2.5CI, ymin = value_97.5CI, x = stage, group = stage),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        # ordered by differrence
        # geom_bar(aes(x = fct_relevel(`Cancer site`, factor_order_male_4v3),
        #              y = `Difference in loss of life expectancy (years)`), position= position_dodge(0.5), stat = "identity", width = .5) +
        facet_grid(rows = vars(cancer_type), cols = vars(Period) , switch = "y", scales = "fixed") +
        #facet_grid( ~ Period) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Mean costs") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              strip.text.y.left = element_text(angle = 0),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())  

#' _____________________________________________________________________________
#' Phase of care costs by stage

#### Plot of costs by cancer type, stage and year of diagnosis
df_allcause_unadj_bystage_PoC_costs <- df_allcause_unadj_PoC_costs %>% 
        pivot_longer(cols = `Diag phase`:`End of life`, names_to = "Phase", values_to = "value") %>%
        mutate(Phase = factor(Phase, levels = c("Diag phase", "Initial treatment\n(0-1 years)", "1-2 years", "2-3 years", "3-4 years", "4-5 years", "5-6 years", "End of life") )) %>%
        separate_wider_delim(cols = value, delim = " ", names = c("mean", "sd", "se"), names_sep = "_",
                             too_few = "align_start") %>%
        mutate(value_sd = as.numeric(str_sub(value_sd, start = 2L, end = -2L)),
               value_se = as.numeric(str_sub(value_se, start = 2L, end = -2L)),
               value_mean = as.numeric(value_mean)) %>%
        filter(grepl(c("Stage at diagnosis"), Variable)) %>%
        mutate(value_2.5CI  = value_mean - qnorm(0.975) * value_se,
               value_97.5CI  = value_mean + qnorm(0.975) * value_se) %>% 
        mutate(stage = str_sub(Variable, start = 20L, end = -1L)) %>%
        mutate(across(where(is.character), as.factor)) %>%
        select(cancer_type, stage, Phase, value_mean, value_2.5CI, value_97.5CI) %>%
        mutate(stage = forcats::fct_rev(stage)) %>%
        mutate(cancer_type = forcats::fct_rev(cancer_type)) %>%
        filter(stage != "0" & stage != "Unknown/Missing")

write.csv(df_allcause_unadj_bystage_PoC_costs %>% mutate(across(value_2.5CI:value_97.5CI, \(x) round(x,1))),
          file = "~/Miscellaneous/PoC_allcause_unadjusted.csv",
          row.names = FALSE)

plot_allcause_unadj_PoC_bystage_8cancers <- df_allcause_unadj_bystage_PoC_costs %>%
        filter(cancer_type %in% c( "Lung", "Colorectal", "Head and neck", "Liver", "Lymphoma", "Oesophagus", "Ovary", "Pancreas")) %>%
        ggplot() + 
        geom_bar(aes(y = value_mean, x = stage, group = stage, fill = stage),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = value_2.5CI, ymin = value_97.5CI, x = stage, group = stage),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        # ordered by differrence
        # geom_bar(aes(x = fct_relevel(`Cancer site`, factor_order_male_4v3),
        #              y = `Difference in loss of life expectancy (years)`), position= position_dodge(0.5), stat = "identity", width = .5) +
        facet_grid(rows = vars(cancer_type), cols = vars(Phase) , switch = "y", scales = "fixed") +
        #facet_grid( ~ Phase) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Mean costs") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              strip.text.y.left = element_text(angle = 0),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())  


plot_allcause_unadj_PoC_bystage_splits <- df_allcause_unadj_bystage_PoC_costs %>%
        filter(cancer_type %in% c( "Colorectal", "Colon and rectosigmoid junction", "Rectum",
                                   "Lymphoma", "Lymphoma: Hodgkins", "Lymphoma: High grade NHL", "Lymphoma: Low grade NHL")) %>%
        mutate(cancer_type = fct_rev(fct_relevel(cancer_type, c( "Colorectal", "Colon and rectosigmoid junction", "Rectum",
                                                                 "Lymphoma", "Lymphoma: Hodgkins", "Lymphoma: High grade NHL", "Lymphoma: Low grade NHL")))) %>%
        ggplot() + 
        geom_bar(aes(y = value_mean, x = stage, group = stage, fill = stage),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = value_2.5CI, ymin = value_97.5CI, x = stage, group = stage),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        # ordered by differrence
        # geom_bar(aes(x = fct_relevel(`Cancer site`, factor_order_male_4v3),
        #              y = `Difference in loss of life expectancy (years)`), position= position_dodge(0.5), stat = "identity", width = .5) +
        facet_grid(rows = vars(cancer_type), cols = vars(Phase) , switch = "y", scales = "fixed") +
        #facet_grid( ~ Phase) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Mean costs") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              strip.text.y.left = element_text(angle = 0),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())  

################################################################################
#' _____________________________________________________________________________
#' Adjusted all-cause costs
#' _____________________________________________________________________________

#' ______ Load data 
#' All the AMEs are in one excel file

res_path_allcause_adj_AME <- "~/Miscellaneous/HDI/Final_results/All cause costs/Regression adjusted/Regression_results_updated_Jan25/AME_results_2025-01-24.xlsx"

df_allcause_AME <- res_path_allcause_adj_AME %>% 
        excel_sheets() %>% 
        set_names() %>% 
        map_df(~ read_excel(path = res_path_allcause_adj_AME, sheet = .x), .id = "Sheet") %>%
        separate_wider_delim(Sheet, ".", names = c("cancer_type", "cost_outcome")) %>%
        filter(!is.na(variable))

#' _____ Plot all AME results in a big PDF
pdf("~/Miscellaneous/HDI/Final_results/Figures/allcause_AME_plots.pdf")
split(df_allcause_AME , list(df_allcause_AME$cancer_type, df_allcause_AME$cost_outcome)) %>%
        map2(.x = ., 
     .y = names(.),
     .f = ~ print(ggplot(.x ,aes(x = forcats::fct_rev(variable), y = estimate)) +
                                         geom_point(shape = 16, size  = 3, color="black") + 
                                         geom_errorbar(aes(ymin  = conf.low,ymax  = conf.high), size  = 0.5, position = "dodge", color="black") +
                                         xlab("") + ylab("Average Marginal Effect") +
                                         coord_flip() + 
                                         geom_hline(yintercept = 0, color = "grey", size = 1, linetype = 'dotted') +
                                         theme_bw(base_size = 14) +
                                         theme(axis.title = element_text(face = "bold", size = 17)) +
                                         ylim(-20000, 20000) + # Scale accordingly - ideally all plots have same scale
                                         scale_y_continuous(labels = scales::label_currency(prefix = "£"))+
                                         ggtitle(.y))
)

dev.off()

#' _____ Plot AME overall
df_bystage_allcause_AME_total <- df_allcause_AME %>%
        filter(grepl(c("Stage"), variable)) %>%
        filter(variable != "Stage: Missing/Unknown") %>%
        filter(grepl("totalcost", cost_outcome)) %>%
        mutate(cost_outcome = case_when(cost_outcome == "totalcost" ~ "Total cost",
                                        .default = NA ) ) %>%
        mutate(cancer_type = case_when(cancer_type == "colorectal" ~ "Colorectal",
                                       cancer_type == "head_neck" ~ "Head and neck",
                                       cancer_type == "liver" ~ "Liver",
                                       cancer_type == "lung" ~ "Lung",
                                       cancer_type == "lymphoma" ~ "Lymphoma",
                                       cancer_type == "oesophagus" ~ 'Oesophagus',
                                       cancer_type == "ovary" ~ "Ovary",
                                       cancer_type == "pancreas" ~ "Pancreas",
                                       .default = NA) )  

plot_bystage_allcause_AME_total <- df_bystage_allcause_AME_total %>%
        ggplot() +
        geom_bar(aes(y = estimate, x = forcats::fct_rev(variable), group = forcats::fct_rev(variable), fill = forcats::fct_rev(variable)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = conf.high, ymin = conf.low, x = forcats::fct_rev(variable), group = forcats::fct_rev(variable)),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        facet_grid(rows = vars(cancer_type), cols = vars(cost_outcome) , switch = "y", scales = "fixed") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#94724F", "#628592", "#666666"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Average Marginal Effect") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              strip.text.y.left = element_text(angle = 0),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())  

ALT_plot_bystage_allcause_AME_total <- df_bystage_allcause_AME_total %>%
        #mutate(cancer_type = factor(cancer_type, levels = c( "Colorectal", "Head and neck", "Lymphoma", "Ovary" ,"Liver", "Lung", "Oesophagus", "Pancreas") )) %>%
        mutate(variable = str_sub(variable, 8, -1)) %>%
        ggplot() +
        geom_bar(aes(y = estimate, x = variable, group = variable, fill = variable),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = conf.high, ymin = conf.low, x = variable, group = variable),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        facet_grid(rows = vars(cost_outcome), cols = vars(cancer_type) , switch = "y", scales = "fixed") +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52")) +
        #, guide = guide_legend(reverse = TRUE)) +
        geom_hline(yintercept=0)  +      
        guides(fill="none", colour = "none")+
        labs(fill = "", x = "Stage at diagnosis", y = "Average Marginal Effect:\nChange in cost to stage I") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 0, vjust = 0, hjust=0,  size = 13),
              strip.text.x = element_text(  size = 13),
              axis.text.y = element_text(angle = 0, vjust = 0, hjust=0,  size = 13),
              strip.text.y = element_blank(),
              axis.ticks.y=element_blank()) 


#' _____ Plot AME annual
df_bystage_allcause_AME_annual <- df_allcause_AME %>%
        filter(grepl(c("Stage"), variable)) %>%
        filter(variable != "Stage: Missing/Unknown") %>%
        filter(grepl("ac_", cost_outcome)) %>%
        mutate(cost_outcome = case_when(cost_outcome == "ac_diag" ~ "Diag phase",
                                        cost_outcome == "ac_treat_0_1year" ~ "0-1 years",
                                        cost_outcome == "ac_treat_1_2year" ~ "1-2 years",
                                        cost_outcome == "ac_treat_2_3year" ~ "2-3 years",
                                        cost_outcome == "ac_treat_3_4year" ~ "3-4 years",
                                        cost_outcome == "ac_treat_4_5year" ~ '4-5 years',
                                        cost_outcome == "ac_treat_5year" ~ "5-6 years",
                                        .default = NA ) ) %>%
        mutate(cost_outcome = factor(cost_outcome, levels = c("Diag phase", "0-1 years", "1-2 years", "2-3 years", "3-4 years", "4-5 years", "5-6 years") )) %>%
        mutate(cancer_type = case_when(cancer_type == "colorectal" ~ "Colorectal",
                                       cancer_type == "head_neck" ~ "Head and neck",
                                       cancer_type == "liver" ~ "Liver",
                                       cancer_type == "lung" ~ "Lung",
                                       cancer_type == "lymphoma" ~ "Lymphoma",
                                       cancer_type == "oesophagus" ~ 'Oesophagus',
                                       cancer_type == "ovary" ~ "Ovary",
                                       cancer_type == "pancreas" ~ "Pancreas",
                                       .default = NA) ) 

plot_bystage_allcause_AME_annual <- df_bystage_allcause_AME_annual %>%
        ggplot() +
        geom_bar(aes(y = estimate, x = forcats::fct_rev(variable), group = forcats::fct_rev(variable), fill = forcats::fct_rev(variable)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = conf.high, ymin = conf.low, x = forcats::fct_rev(variable), group = forcats::fct_rev(variable)),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        facet_grid(rows = vars(cancer_type), cols = vars(cost_outcome) , switch = "y", scales = "fixed") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#94724F", "#628592", "#666666"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Average Marginal Effect") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              strip.text.y.left = element_text(angle = 0),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())  

#' _____ Plot AME PoC
df_bystage_allcause_AME_PoC <- df_allcause_AME %>%
        filter(grepl(c("Stage"), variable)) %>%
        filter(variable != "Stage: Missing/Unknown") %>%
        filter(!grepl("ac_|totalcost", cost_outcome)) %>%
        mutate(cost_outcome = case_when(cost_outcome == "diag" ~ "Diag phase",
                                        cost_outcome == "treat_0_1year" ~ "Initial treatment\n(0-1 years)",
                                        cost_outcome == "treat_1_2year" ~ "1-2 years",
                                        cost_outcome == "treat_2_3year" ~ "2-3 years",
                                        cost_outcome == "treat_3_4year" ~ "3-4 years",
                                        cost_outcome == "treat_4_5year" ~ '4-5 years',
                                        cost_outcome == "treat_5year" ~ "5-6 years",
                                        cost_outcome == "eol" ~ "End of life",
                                        .default = NA ) ) %>%
        mutate(cost_outcome = factor(cost_outcome, levels = c("Diag phase", "Initial treatment\n(0-1 years)", "1-2 years", "2-3 years", "3-4 years", "4-5 years", "5-6 years", "End of life") )) %>%
        mutate(cancer_type = case_when(cancer_type == "colorectal" ~ "Colorectal",
                                       cancer_type == "head_neck" ~ "Head and neck",
                                       cancer_type == "liver" ~ "Liver",
                                       cancer_type == "lung" ~ "Lung",
                                       cancer_type == "lymphoma" ~ "Lymphoma",
                                       cancer_type == "oesophagus" ~ 'Oesophagus',
                                       cancer_type == "ovary" ~ "Ovary",
                                       cancer_type == "pancreas" ~ "Pancreas",
                                       .default = NA) )
        
plot_bystage_allcause_AME_PoC <- df_bystage_allcause_AME_PoC %>%
        ggplot() +
        geom_bar(aes(y = estimate, x = forcats::fct_rev(variable), group = forcats::fct_rev(variable), fill = forcats::fct_rev(variable)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = conf.high, ymin = conf.low, x = forcats::fct_rev(variable), group = forcats::fct_rev(variable)),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        facet_grid(rows = vars(cancer_type), cols = vars(cost_outcome) , switch = "y", scales = "fixed") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Average Marginal Effect") +
        theme_bw() +
        theme(strip.text = element_text(face = "bold", size = 10),
              axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              strip.text.y.left = element_text(angle = 0),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())  

#################################################################################
#' _____________________________________________________________________________
#' Comparing unadjusted to adjusted all-cause
#' _____________________________________________________________________________

#' _____ Total
df_total_allcause_unadjusted_V_adjusted <- rbind(
        
        df_allcause_unadj_bystage_total_costs %>%
                filter(cancer_type %in% c( "Lung", "Colorectal", "Head and neck", "Liver", "Lymphoma", "Oesophagus", "Ovary", "Pancreas")) %>%
                filter(year == "2015") %>%
                select(cancer_type, stage, value_mean) %>%
                pivot_wider(names_from = stage, names_prefix = "Stage_", values_from = c("value_mean") ) %>%
                mutate(Stage1_2 = Stage_2 - Stage_1,
                       Stage1_3 = Stage_3 - Stage_1,
                       Stage1_4 = Stage_4 - Stage_1) %>%
                select(cancer_type, Stage1_2, Stage1_3,Stage1_4) %>%
                pivot_longer(Stage1_2:Stage1_4, names_to = "Stage", values_to = "estimate") %>%
                mutate(stage = case_when(Stage == "Stage1_2" ~ "2",
                                         Stage == "Stage1_3" ~ "3",
                                         Stage == "Stage1_4" ~ "4")) %>%
                select(cancer_type, stage, estimate) %>%
                mutate(analysis_type = "Unadjusted") %>%
                mutate(across(where(is.factor), as.character)),
        
        
        df_bystage_allcause_AME_total %>%
                select(-cost_outcome, -std.error, -p.value, -conf.low , -conf.high ) %>%
                rename(stage = variable) %>%
                mutate(stage = case_when(stage == "Stage: II" ~ "2",
                                         stage == "Stage: III" ~ "3",
                                         stage == "Stage: IV" ~ "4")) %>%
                mutate(analysis_type = "Adjusted")
        ) 


plot_total_allcause_unadjusted_V_adjusted <- df_total_allcause_unadjusted_V_adjusted %>% 
        mutate(across(where(is.character), as.factor)) %>%
        mutate(cost_outcome = "Total all-cause hospital costs") %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = fct_rev(cancer_type), group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid( fct_rev(analysis_type) ~ cost_outcome, switch = "y") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Cost difference to stage I (reference stage)") +
        theme_bw(base_size = 14) +
                theme(axis.title = element_text(face = "bold", size = 17),
                      axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
                      axis.ticks.y=element_blank())
        
# Individual cancers 900 500
        
plot_ind_total_allcause_unadjusted_V_adjusted <- df_total_allcause_unadjusted_V_adjusted %>%
        filter(cancer_type %in% c( "Ovary")) %>%
        mutate(across(where(is.character), as.factor)) %>%
        mutate(cost_outcome = "Total all-cause hospital costs") %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = fct_rev(cancer_type), group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid( fct_rev(analysis_type) ~ cost_outcome, switch = "y") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Cost difference to stage I (reference stage)") +
        theme_bw(base_size = 14) +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              axis.ticks.y=element_blank())

#' _____ Annual
df_annual_allcause_unadjusted_V_adjusted <- rbind(
        
       df_allcause_unadj_bystage_annual_costs %>%
                filter(cancer_type %in% c( "Lung", "Colorectal", "Head and neck", "Liver", "Lymphoma", "Oesophagus", "Ovary", "Pancreas")) %>%
                select(cancer_type, stage, Period, value_mean) %>%
                pivot_wider(names_from = stage, names_prefix = "Stage_", values_from = c("value_mean") ) %>%
                mutate(Stage1_2 = Stage_2 - Stage_1,
                       Stage1_3 = Stage_3 - Stage_1,
                       Stage1_4 = Stage_4 - Stage_1) %>%
                select(cancer_type, Period, Stage1_2, Stage1_3,Stage1_4) %>%
                pivot_longer(Stage1_2:Stage1_4, names_to = "Stage", values_to = "estimate") %>%
                mutate(stage = case_when(Stage == "Stage1_2" ~ "2",
                                         Stage == "Stage1_3" ~ "3",
                                         Stage == "Stage1_4" ~ "4")) %>%
                select(cancer_type, Period, stage, estimate) %>%
                mutate(analysis_type = "Unadjusted") %>%
                rename(cost_outcome = Period) %>%
                mutate(across(where(is.factor), as.character)),
        
        
        df_bystage_allcause_AME_annual %>%
                select( -std.error, -p.value, -conf.low , -conf.high ) %>%
                rename(stage = variable) %>%
                mutate(stage = case_when(stage == "Stage: II" ~ "2",
                                         stage == "Stage: III" ~ "3",
                                         stage == "Stage: IV" ~ "4")) %>%
                mutate(analysis_type = "Adjusted")) %>% 
        mutate(cost_outcome = factor(cost_outcome, levels = c("Diag phase", "0-1 years", "1-2 years", "2-3 years", "3-4 years", "4-5 years", "5-6 years") )) %>%
        mutate(across(where(is.character), as.factor)) 

plot_annual_allcause_unadjusted_V_adjusted <- df_annual_allcause_unadjusted_V_adjusted %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = fct_rev(cancer_type), group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid( cost_outcome ~ analysis_type) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Cost difference to stage I") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              axis.ticks.y=element_blank())  

plot_annual_allcause_unadjusted_V_adjusted_Colorectaletc <- df_annual_allcause_unadjusted_V_adjusted %>%
        filter(cancer_type %in% c( "Colorectal", "Head and neck",  "Lymphoma", "Ovary")) %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = fct_rev(cancer_type), group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid( cost_outcome ~ analysis_type) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Cost difference to stage I") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              axis.ticks.y=element_blank())  

plot_annual_allcause_unadjusted_V_adjusted_Lungetc <- df_annual_allcause_unadjusted_V_adjusted %>%
        filter(cancer_type %in% c("Liver", "Lung", "Oesophagus", "Pancreas")) %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = fct_rev(cancer_type), group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid( cost_outcome ~ analysis_type) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Cost difference to stage I") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              axis.ticks.y=element_blank())  

#' _____ PoC
df_PoC_allcause_unadjusted_V_adjusted <- rbind(
        
        df_allcause_unadj_bystage_PoC_costs %>%
                filter(cancer_type %in% c( "Lung", "Colorectal", "Head and neck", "Liver", "Lymphoma", "Oesophagus", "Ovary", "Pancreas")) %>%
                select(cancer_type, stage, Phase, value_mean) %>%
                pivot_wider(names_from = stage, names_prefix = "Stage_", values_from = c("value_mean") ) %>%
                mutate(Stage1_2 = Stage_2 - Stage_1,
                       Stage1_3 = Stage_3 - Stage_1,
                       Stage1_4 = Stage_4 - Stage_1) %>%
                select(cancer_type, Phase, Stage1_2, Stage1_3,Stage1_4) %>%
                pivot_longer(Stage1_2:Stage1_4, names_to = "Stage", values_to = "estimate") %>%
                mutate(stage = case_when(Stage == "Stage1_2" ~ "2",
                                         Stage == "Stage1_3" ~ "3",
                                         Stage == "Stage1_4" ~ "4")) %>%
                select(cancer_type, Phase, stage, estimate) %>%
                mutate(analysis_type = "Unadjusted") %>%
                rename(cost_outcome = Phase) %>%
                mutate(across(where(is.factor), as.character)),
        
        
        df_bystage_allcause_AME_PoC %>%
                select( -std.error, -p.value, -conf.low , -conf.high ) %>%
                rename(stage = variable) %>%
                mutate(stage = case_when(stage == "Stage: II" ~ "2",
                                         stage == "Stage: III" ~ "3",
                                         stage == "Stage: IV" ~ "4")) %>%
                mutate(analysis_type = "Adjusted")) %>% 
        mutate(cost_outcome = factor(cost_outcome, levels = c("Diag phase", "Initial treatment\n(0-1 years)", "1-2 years", "2-3 years", "3-4 years", "4-5 years", "5-6 years", "End of life") )) %>%
        mutate(across(where(is.character), as.factor)) 

plot_PoC_allcause_unadjusted_V_adjusted <- df_PoC_allcause_unadjusted_V_adjusted %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = fct_rev(cancer_type), group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid( cost_outcome ~ analysis_type) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Cost difference to stage I") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              axis.ticks.y=element_blank())  

plot_PoC_allcause_unadjusted_V_adjusted_Colorectaletc <- df_PoC_allcause_unadjusted_V_adjusted %>%
        filter(cancer_type %in% c( "Colorectal", "Head and neck",  "Lymphoma", "Ovary")) %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = analysis_type, group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid(   fct_rev(cancer_type) ~ cost_outcome) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "", y = "Cost difference to stage I") +
        theme_bw() +
        theme(strip.text = element_text(face = "bold", size = 10),
              axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              axis.ticks.y=element_blank())  


plot_PoC_all_cause_unadjusted_V_adjusted_Lungetc <- df_PoC_allcause_unadjusted_V_adjusted %>%
        filter(cancer_type %in% c("Liver", "Lung", "Oesophagus", "Pancreas")) %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = analysis_type, group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid(   fct_rev(cancer_type) ~ cost_outcome) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "", y = "Cost difference to stage I") +
        theme_bw() +
        theme(strip.text = element_text(face = "bold", size = 10),
              axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              axis.ticks.y=element_blank())  


################################################################################
#' _____________________________________________________________________________
#' Load data: Unadjusted net costs
#' _____________________________________________________________________________
#' _____ Total costs

res_path_net_unadj <- "~/Miscellaneous/HDI/Final_results/Net costs/Unadjusted/Costs_GRAIL_net_2025-01-29.xlsx"

df_net_unadj_total_costs <- res_path_net_unadj %>% 
                excel_sheets() %>% 
                set_names() %>% 
                map_df(~ read_excel(path = res_path_net_unadj, sheet = .x), .id = "Sheet") %>%
                rename(Variable_main = `...1`,
                       Variable_type = `...2`) %>% 
                fill(Variable_main, .direction = "down") %>%
                unite("Variable", Variable_main:Variable_type, na.rm = TRUE, remove = TRUE) %>%
                mutate(cancer_type = case_when(Sheet == "T2_total_costs_lung" ~ "Lung",
                                               Sheet == "T2_total_costs_colorectal" ~ "Colorectal",
                                               Sheet == "T2_total_costs_head_neck" ~ "Head and neck",
                                               Sheet == "T2_total_costs_liver" ~ "Liver",
                                               Sheet == "T2_total_costs_lymphoma" ~ "Lymphoma",
                                               Sheet == "T2_total_costs_oesophagus" ~ "Oesophagus",
                                               Sheet == "T2_total_costs_ovary" ~ "Ovary",
                                               Sheet == "T2_total_costs_pancreas" ~ "Pancreas",
                                               .default = Sheet )) %>%
                select(-Sheet) %>%
        select(Variable, cancer_type, costs_2014:costs_2017) %>%
        filter(!is.na(costs_2014))

#' _____ Annual costs

df_net_unadj_annual_costs <- res_path_net_unadj %>% 
        excel_sheets() %>% 
        set_names() %>% 
        map_df(~ read_excel(path = res_path_net_unadj, sheet = .x), .id = "Sheet") %>%
        rename(Variable_main = `...1`,
               Variable_type = `...2`) %>% 
        fill(Variable_main, .direction = "down") %>%
        unite("Variable", Variable_main:Variable_type, na.rm = TRUE, remove = TRUE) %>%
        mutate(cancer_type = case_when(Sheet == "T2_costs_lung" ~ "Lung",
                                       Sheet == "T2_costs_colorectal" ~ "Colorectal",
                                       Sheet == "T2_costs_head_neck" ~ "Head and neck",
                                       Sheet == "T2_costs_liver" ~ "Liver",
                                       Sheet == "T2_costs_lymphoma" ~ "Lymphoma",
                                       Sheet == "T2_costs_oesophagus" ~ "Oesophagus",
                                       Sheet == "T2_costs_ovary" ~ "Ovary",
                                       Sheet == "T2_costs_pancreas" ~ "Pancreas",
                                       .default = Sheet )) %>%
        select(-Sheet) %>%
        select(Variable, cancer_type, costs_ac_diag:costs_ac_treat_5year) %>%
        filter(!is.na(costs_ac_diag))

#' _____ PoC costs

df_net_unadj_PoC_costs <- res_path_net_unadj %>% 
        excel_sheets() %>% 
        set_names() %>% 
        map_df(~ read_excel(path = res_path_net_unadj, sheet = .x), .id = "Sheet") %>%
        rename(Variable_main = `...1`,
               Variable_type = `...2`) %>% 
        fill(Variable_main, .direction = "down") %>%
        unite("Variable", Variable_main:Variable_type, na.rm = TRUE, remove = TRUE) %>%
        mutate(cancer_type = case_when(Sheet == "T2_costs_lung" ~ "Lung",
                                       Sheet == "T2_costs_colorectal" ~ "Colorectal",
                                       Sheet == "T2_costs_head_neck" ~ "Head and neck",
                                       Sheet == "T2_costs_liver" ~ "Liver",
                                       Sheet == "T2_costs_lymphoma" ~ "Lymphoma",
                                       Sheet == "T2_costs_oesophagus" ~ "Oesophagus",
                                       Sheet == "T2_costs_ovary" ~ "Ovary",
                                       Sheet == "T2_costs_pancreas" ~ "Pancreas",
                                       .default = Sheet )) %>%
        select(-Sheet) %>%
        select(Variable, cancer_type, costs_diag:costs_treat_5year) %>%
        filter(!is.na(costs_diag))

################################################################################
#' _____________________________________________________________________________
#' Tabulate data: Unadjusted net costs
#' _____________________________________________________________________________

#' _____________________________________________________________________________
#' Total costs by stage

#' _____ Create dataframe
df_net_unadj_bystage_total_costs <- df_net_unadj_total_costs %>% 
        pivot_longer(cols = costs_2014:costs_2017, names_to = "year") %>%
        separate_wider_delim(cols = value, delim = " ", names = c("mean", "sd", "se"), names_sep = "_",
                             too_few = "align_start") %>%
        mutate(value_sd = as.numeric(str_sub(value_sd, start = 2L, end = -2L)),
               value_se = as.numeric(str_sub(value_se, start = 2L, end = -2L)),
               value_mean = as.numeric(value_mean)) %>%
        filter(grepl(c("Stage at diagnosis"), Variable)) %>%
        mutate(value_2.5CI  = value_mean - qnorm(0.975) * value_se,
               value_97.5CI  = value_mean + qnorm(0.975) * value_se) %>%
        mutate(stage = str_sub(Variable, start = 20L, end = -1L)) %>%
        select(cancer_type, stage, year, value_mean, value_2.5CI, value_97.5CI) %>%
        mutate(year = str_sub(year, start = 7L, end = -1L)) %>% 
        mutate(across(where(is.character), as.factor)) %>%
        #mutate(stage = forcats::fct_rev(stage)) %>%
        mutate(cancer_type = forcats::fct_rev(cancer_type)) %>%
        filter(stage != "0" & stage != "Unknown/Missing") 

#' _____ Plot of costs by cancer type, stage and year of diagnosis

plot_net_unadj_bystage_8cancers <- df_net_unadj_bystage_total_costs %>%
        filter(cancer_type %in% c( "Lung", "Colorectal", "Head and neck", "Liver", "Lymphoma", "Oesophagus", "Ovary", "Pancreas")) %>%
        ggplot() + 
        geom_bar(aes(y = value_mean, x = cancer_type, group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = value_2.5CI, ymin = value_97.5CI, x = cancer_type, group = fct_rev(stage)),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        # ordered by differrence
        # geom_bar(aes(x = fct_relevel(`Cancer site`, factor_order_male_4v3),
        #              y = `Difference in loss of life expectancy (years)`), position= position_dodge(0.5), stat = "identity", width = .5) +
        facet_grid( ~ year) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Mean costs") +
        theme_bw(base_size = 14) +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              axis.ticks.y=element_blank())  


ALT_plot_net_unadj_bystage_8cancers <- df_net_unadj_bystage_total_costs %>%
        filter(year == "2014") %>%
        mutate(stage = case_when(stage == "1"~ "I",
                                 stage == "2"~ "II",
                                 stage == "3"~ "III",
                                 stage == "4"~ "IV",
                                 .default = stage)) %>%
        filter(cancer_type %in% c( "Lung", "Colorectal", "Head and neck", "Liver", "Lymphoma", "Oesophagus", "Ovary", "Pancreas")) %>%
        mutate(cancer_type = factor(cancer_type, levels = c( "Colorectal", "Head and neck", "Lymphoma", "Ovary" ,"Liver", "Lung", "Oesophagus", "Pancreas") )) %>%
        ggplot() + 
        geom_bar(aes(y = value_mean, x = stage, group = stage, fill = stage),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = value_2.5CI, ymin = value_97.5CI, x = stage, group = stage),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        facet_grid( cols = vars(cancer_type) , switch = "y", scales = "fixed") +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592")) +
        #, guide = guide_legend(reverse = TRUE)) +
        geom_hline(yintercept=0)  +      
        guides(fill="none", colour = "none")+
        labs(fill = "", x = "Stage at diagnosis", y = "Mean NHS hospital costs per patient") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 0, vjust = 0, hjust=0,  size = 13),
              strip.text.x = element_text(  size = 13),
              axis.text.y = element_text(angle = 0, vjust = 0, hjust=0,  size = 13),
              strip.text.y = element_blank(),
              axis.ticks.y=element_blank()) 

ALT_plot_net_unadj_bystage_Lung <- df_net_unadj_bystage_total_costs %>%
        mutate(stage = case_when(stage == "1"~ "I",
                                 stage == "2"~ "II",
                                 stage == "3"~ "III",
                                 stage == "4"~ "IV",
                                 .default = stage)) %>%
        filter(cancer_type %in% c( "Lung")) %>%
        ggplot() + 
        geom_bar(aes(y = value_mean, x = stage, group = stage, fill = stage),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = value_2.5CI, ymin = value_97.5CI, x = stage, group = stage),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        facet_grid( cols = vars(year) , switch = "y", scales = "fixed") +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592")) +
        #, guide = guide_legend(reverse = TRUE)) +
        geom_hline(yintercept=0)  +      
        guides(fill="none", colour = "none")+
        labs(fill = "", x = "Stage at diagnosis", y = "Mean NHS hospital costs per lung patient") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 0, vjust = 0, hjust=0,  size = 13),
              strip.text.x = element_text(  size = 13),
              axis.text.y = element_text(angle = 0, vjust = 0, hjust=0,  size = 13),
              strip.text.y = element_blank(),
              axis.ticks.y=element_blank()) 

ALT_plot_net_unadj_bystage_Lung <- df_net_unadj_bystage_total_costs %>%
        mutate(stage = case_when(stage == "1"~ "I",
                                 stage == "2"~ "II",
                                 stage == "3"~ "III",
                                 stage == "4"~ "IV",
                                 .default = stage)) %>%
        filter(cancer_type %in% c( "Lung")) %>%
        ggplot() + 
        geom_bar(aes(y = value_mean, x = year, group = stage, fill = stage),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = value_2.5CI, ymin = value_97.5CI, x = year, group = year),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        facet_grid( cols = vars(stage) , switch = "y", scales = "fixed") +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592")) +
        #, guide = guide_legend(reverse = TRUE)) +
        geom_hline(yintercept=0)  +      
        guides(fill="none", colour = "none")+
        labs(fill = "", x = "Year of diagnosis", y = "Mean NHS hospital costs per lung patient") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 90, vjust = 0, hjust=0,  size = 13),
              strip.text.x = element_text(  size = 13),
              axis.text.y = element_text(angle = 0, vjust = 0, hjust=0,  size = 13),
              strip.text.y = element_blank(),
              axis.ticks.y=element_blank()) 



plot_net_unadj__bystage_4bad <- df_net_unadj_bystage_total_costs %>%
        filter(cancer_type %in% c( "Lung", "Liver", "Oesophagus", "Pancreas")) %>%
        filter(year == "2014") %>%
        ggplot() + 
        geom_bar(aes(y = value_mean, x = cancer_type, group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = value_2.5CI, ymin = value_97.5CI, x = cancer_type, group = fct_rev(stage)),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        # ordered by differrence
        # geom_bar(aes(x = fct_relevel(`Cancer site`, factor_order_male_4v3),
        #              y = `Difference in loss of life expectancy (years)`), position= position_dodge(0.5), stat = "identity", width = .5) +
        facet_grid( ~ year) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage", x = "", y = "") +
        theme_bw(base_size = 14) +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              axis.ticks.y=element_blank())  

plot_net_unadj__bystage_4good <- df_net_unadj_bystage_total_costs %>%
        filter(cancer_type %in% c( "Colorectal", "Head and neck",  "Lymphoma", "Ovary")) %>%
        filter(year == "2014") %>%
        ggplot() + 
        geom_bar(aes(y = value_mean, x = cancer_type, group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = value_2.5CI, ymin = value_97.5CI, x = cancer_type, group = fct_rev(stage)),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        # ordered by differrence
        # geom_bar(aes(x = fct_relevel(`Cancer site`, factor_order_male_4v3),
        #              y = `Difference in loss of life expectancy (years)`), position= position_dodge(0.5), stat = "identity", width = .5) +
        facet_grid( ~ year) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage", x = "", y = "") +
        theme_bw(base_size = 14) +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              axis.ticks.y=element_blank())  

#' _____________________________________________________________________________
#' Annual costs by stage

#' _____ Create dataframe Plot of costs by cancer type, stage and year of diagnosis
df_net_unadj_bystage_annual_costs <- df_net_unadj_annual_costs %>% 
        rename(`Diag phase` = costs_ac_diag,
               `0-1 years` = costs_ac_treat_0_1year,
               `1-2 years` = costs_ac_treat_1_2year,
               `2-3 years` = costs_ac_treat_2_3year,
               `3-4 years` = costs_ac_treat_3_4year,
               `4-5 years` = costs_ac_treat_4_5year,
               `5-6 years` = costs_ac_treat_5year
        ) %>% 
        pivot_longer(cols = `Diag phase`:`5-6 years`, names_to = "Period", values_to = "value") %>%
        mutate(Period = factor(Period, levels = c("Diag phase", "0-1 years", "1-2 years", "2-3 years", "3-4 years", "4-5 years", "5-6 years") )) %>%
        separate_wider_delim(cols = value, delim = " ", names = c("mean", "sd", "se"), names_sep = "_",
                             too_few = "align_start") %>%
        mutate(value_sd = as.numeric(str_sub(value_sd, start = 2L, end = -2L)),
               value_se = as.numeric(str_sub(value_se, start = 2L, end = -2L)),
               value_mean = as.numeric(value_mean)) %>%
        filter(grepl(c("Stage at diagnosis"), Variable)) %>%
        mutate(value_2.5CI  = value_mean - qnorm(0.975) * value_se,
               value_97.5CI  = value_mean + qnorm(0.975) * value_se) %>% 
        mutate(stage = str_sub(Variable, start = 20L, end = -1L)) %>%
        mutate(across(where(is.character), as.factor)) %>%
        select(cancer_type, stage, Period, value_mean, value_2.5CI, value_97.5CI) %>%
        mutate(stage = forcats::fct_rev(stage)) %>%
        mutate(cancer_type = forcats::fct_rev(cancer_type)) %>%
        filter(stage != "0" & stage != "Unknown/Missing")

#' _____ Create dataframe Plot of costs by cancer type, stage and year of diagnosis
plot_net_unadj_annual_bystage_8cancers <- df_net_unadj_bystage_annual_costs %>%
        filter(cancer_type %in% c( "Lung", "Colorectal", "Head and neck", "Liver", "Lymphoma", "Oesophagus", "Ovary", "Pancreas")) %>%
        ggplot() + 
        geom_bar(aes(y = value_mean, x = stage, group = stage, fill = stage),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .7) +
        geom_errorbar(aes(ymax = value_2.5CI, ymin = value_97.5CI, x = stage, group = stage),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        # ordered by differrence
        # geom_bar(aes(x = fct_relevel(`Cancer site`, factor_order_male_4v3),
        #              y = `Difference in loss of life expectancy (years)`), position= position_dodge(0.5), stat = "identity", width = .5) +
        facet_grid(rows = vars(fct_rev(cancer_type)), cols = vars(Period) , switch = "y", scales = "fixed") +
        #facet_grid( ~ Period) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = NULL, y = "Mean costs") +
        theme_bw(base_size = 12) +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0),
              strip.text.y.left = element_text(angle = 0, size = 12),
              strip.text.x.top = element_text(angle = 0, size = 12),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              legend.position="bottom") 

# 
# ALT_plot_net_unadj_bystage_annual_Lung <- df_net_unadj_annual_costs %>% 
#         rename(`Diag phase` = costs_ac_diag,
#                `0-1 years` = costs_ac_treat_0_1year,
#                `1-2 years` = costs_ac_treat_1_2year,
#                `2-3 years` = costs_ac_treat_2_3year,
#                `3-4 years` = costs_ac_treat_3_4year,
#                `4-5 years` = costs_ac_treat_4_5year,
#                `5-6 years` = costs_ac_treat_5year
#         ) %>% 
#         pivot_longer(cols = `Diag phase`:`5-6 years`, names_to = "Period", values_to = "value") %>%
#         mutate(Period = factor(Period, levels = c("Diag phase", "0-1 years", "1-2 years", "2-3 years", "3-4 years", "4-5 years", "5-6 years") )) %>%
#         separate_wider_delim(cols = value, delim = " ", names = c("mean", "sd", "se"), names_sep = "_",
#                              too_few = "align_start") %>%
#         mutate(value_sd = as.numeric(str_sub(value_sd, start = 2L, end = -2L)),
#                value_se = as.numeric(str_sub(value_se, start = 2L, end = -2L)),
#                value_mean = as.numeric(value_mean)) %>%
#         filter(grepl(c("Period"), Variable)) %>%
#         mutate(value_2.5CI  = value_mean - qnorm(0.975) * value_se,
#                value_97.5CI  = value_mean + qnorm(0.975) * value_se) %>% 
#         mutate(stage = str_sub(Variable, start = 20L, end = -1L)) %>%
#         mutate(across(where(is.character), as.factor)) %>%
#         select(cancer_type, stage, Period, value_mean, value_2.5CI, value_97.5CI) %>%
#         mutate(stage = forcats::fct_rev(stage)) %>%
#         mutate(cancer_type = forcats::fct_rev(cancer_type)) %>%
#         filter(stage != "0" & stage != "Unknown/Missing") %>%
#         filter(cancer_type %in% c( "Lung")) %>%
#         filter(Period %in% c( "0-1 years")) %>%
#         ggplot() + 
#         geom_bar(aes(y = value_mean, x = stage, group = stage, fill = stage),
#                  position= position_dodge(0.5),
#                  stat = "identity",
#                  width = .5) +
#         geom_errorbar(aes(ymax = value_2.5CI, ymin = value_97.5CI, x = stage, group = stage),
#                       position= position_dodge(0.5), width = 0.2, colour = "black",) +
#         facet_grid( cols = vars(year) , switch = "y", scales = "fixed") +
#         scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
#         scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592")) +
#         #, guide = guide_legend(reverse = TRUE)) +
#         geom_hline(yintercept=0)  +      
#         guides(fill="none", colour = "none")+
#         labs(fill = "", x = "Stage at diagnosis", y = "Mean NHS hospital costs per lung patient") +
#         theme_bw() +
#         theme(axis.title = element_text(face = "bold", size = 17),
#               axis.text.x = element_text(angle = 0, vjust = 0, hjust=0,  size = 13),
#               strip.text.x = element_text(  size = 13),
#               axis.text.y = element_text(angle = 0, vjust = 0, hjust=0,  size = 13),
#               strip.text.y = element_blank(),
#               axis.ticks.y=element_blank()) 

#' _____________________________________________________________________________
#' Phase of care costs by stage

#### Plot of costs by cancer type, stage and year of diagnosis
df_net_unadj_bystage_PoC_costs <- df_net_unadj_PoC_costs %>% 
        rename(`Diag phase` = costs_diag,
               `Initial treatment\n(0-1 years)` = costs_treat_0_1year,
               `1-2 years` = costs_treat_1_2year,
               `2-3 years` = costs_treat_2_3year,
               `3-4 years` = costs_treat_3_4year,
               `4-5 years` = costs_treat_4_5year,
               `5-6 years` = costs_treat_5year,
               `End of life` = costs_eol) %>% 
        pivot_longer(cols = `Diag phase`:`5-6 years`, names_to = "Phase", values_to = "value") %>%
        mutate(Phase = factor(Phase, levels = c("Diag phase", "Initial treatment\n(0-1 years)", "1-2 years", "2-3 years", "3-4 years", "4-5 years", "5-6 years", "End of life") )) %>%
        separate_wider_delim(cols = value, delim = " ", names = c("mean", "sd", "se"), names_sep = "_",
                             too_few = "align_start") %>%
        mutate(value_sd = as.numeric(str_sub(value_sd, start = 2L, end = -2L)),
               value_se = as.numeric(str_sub(value_se, start = 2L, end = -2L)),
               value_mean = as.numeric(value_mean)) %>%
        filter(grepl(c("Stage at diagnosis"), Variable)) %>%
        mutate(value_2.5CI  = value_mean - qnorm(0.975) * value_se,
               value_97.5CI  = value_mean + qnorm(0.975) * value_se) %>% 
        mutate(stage = str_sub(Variable, start = 20L, end = -1L)) %>%
        mutate(across(where(is.character), as.factor)) %>%
        select(cancer_type, stage, Phase, value_mean, value_2.5CI, value_97.5CI) %>%
        mutate(stage = forcats::fct_rev(stage)) %>%
        mutate(cancer_type = forcats::fct_rev(cancer_type)) %>%
        filter(stage != "0" & stage != "Unknown/Missing")

plot_net_unadj_PoC_bystage_8cancers <- df_net_unadj_bystage_PoC_costs %>%
        filter(cancer_type %in% c( "Lung", "Colorectal", "Head and neck", "Liver", "Lymphoma", "Oesophagus", "Ovary", "Pancreas")) %>%
        ggplot() + 
        geom_bar(aes(y = value_mean, x = stage, group = stage, fill = stage),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .7) +
        geom_errorbar(aes(ymax = value_2.5CI, ymin = value_97.5CI, x = stage, group = stage),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        # ordered by differrence
        # geom_bar(aes(x = fct_relevel(`Cancer site`, factor_order_male_4v3),
        #              y = `Difference in loss of life expectancy (years)`), position= position_dodge(0.5), stat = "identity", width = .5) +
        facet_grid(rows = vars(fct_rev(cancer_type)), cols = vars(Phase) , switch = "y", scales = "fixed") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = NULL, y = "Mean costs") +
        theme_bw(base_size = 12) +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0),
              strip.text.y.left = element_text(angle = 0, size = 12),
              strip.text.x.top = element_text(angle = 0, size = 12),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              legend.position="bottom") 


################################################################################
#' _____________________________________________________________________________
#' Adjusted net costs
#' _____________________________________________________________________________

#' ______ Load data 
#' All the AMEs are in one excel file

res_path_net_adj_AME_PoC_annual <- "~/Miscellaneous/HDI/Final_results/Net costs/Regression adjusted/AME_results_2025-01-24.xlsx"
res_path_net_adj_AME_total <- "~/Miscellaneous/HDI/Final_results/Net costs/Regression adjusted/AME_results_totalcost_2025-01-24.xlsx"

df_net_AME <- rbind(
        
        res_path_net_adj_AME_PoC_annual %>% 
        excel_sheets() %>% 
        set_names() %>% 
        map_df(~ read_excel(path = res_path_net_adj_AME_PoC_annual, sheet = .x), .id = "Sheet") %>%
        separate_wider_delim(Sheet, ".", names = c("cancer_type", "cost_outcome")) %>%
        filter(!is.na(variable)),
        
        res_path_net_adj_AME_total %>% 
                excel_sheets() %>% 
                set_names() %>% 
                map_df(~ read_excel(path = res_path_net_adj_AME_total, sheet = .x), .id = "Sheet") %>%
                separate_wider_delim(Sheet, ".", names = c("cancer_type", "cost_outcome")) %>%
                filter(!is.na(variable))
        
)

#' _____ Plot all AME results in a big PDF
pdf("~/Miscellaneous/HDI/Final_results/Figures/net_AME_plots.pdf")
split(df_net_AME , list(df_net_AME$cancer_type, df_net_AME$cost_outcome)) %>%
        map2(.x = ., 
             .y = names(.),
             .f = ~ print(ggplot(.x ,aes(x = forcats::fct_rev(variable), y = estimate)) +
                                  geom_point(shape = 16, size  = 3, color="black") + 
                                  geom_errorbar(aes(ymin  = conf.low,ymax  = conf.high), size  = 0.5, position = "dodge", color="black") +
                                  xlab("") + ylab("Average Marginal Effect") +
                                  coord_flip() + 
                                  geom_hline(yintercept = 0, color = "grey", size = 1, linetype = 'dotted') +
                                  theme_bw(base_size = 14) +
                                  theme(axis.title = element_text(face = "bold", size = 17)) +
                                  ylim(-20000, 20000) + # Scale accordingly - ideally all plots have same scale
                                  scale_y_continuous(labels = scales::label_currency(prefix = "£"))+
                                  ggtitle(.y))
        )

dev.off()

#' _____ Plot AME overall
df_bystage_net_AME_total <- df_net_AME %>%
        filter(grepl(c("Stage"), variable)) %>%
        filter(variable != "Stage: Missing/Unknown") %>%
        filter(grepl("totalcost", cost_outcome)) %>%
        mutate(cost_outcome = case_when(cost_outcome == "totalcost" ~ "Total cost",
                                        .default = NA ) ) %>%
        mutate(cancer_type = case_when(cancer_type == "colorectal" ~ "Colorectal",
                                       cancer_type == "head_neck" ~ "Head and neck",
                                       cancer_type == "liver" ~ "Liver",
                                       cancer_type == "lung" ~ "Lung",
                                       cancer_type == "lymphoma" ~ "Lymphoma",
                                       cancer_type == "oesophagus" ~ 'Oesophagus',
                                       cancer_type == "ovary" ~ "Ovary",
                                       cancer_type == "pancreas" ~ "Pancreas",
                                       .default = NA) )  

plot_bystage_net_AME_total <- df_bystage_net_AME_total %>%
        ggplot() +
        geom_bar(aes(y = estimate, x = forcats::fct_rev(variable), group = forcats::fct_rev(variable), fill = forcats::fct_rev(variable)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = conf.high, ymin = conf.low, x = forcats::fct_rev(variable), group = forcats::fct_rev(variable)),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        facet_grid(rows = vars(cancer_type), cols = vars(cost_outcome) , switch = "y", scales = "fixed") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#94724F", "#628592", "#666666"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Average Marginal Effect") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              strip.text.y.left = element_text(angle = 0),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())  

ALT_plot_bystage_net_AME_total <- df_bystage_net_AME_total %>%
        #mutate(cancer_type = factor(cancer_type, levels = c( "Colorectal", "Head and neck", "Lymphoma", "Ovary" ,"Liver", "Lung", "Oesophagus", "Pancreas") )) %>%
        mutate(variable = str_sub(variable, 8, -1)) %>%
        ggplot() +
        geom_bar(aes(y = estimate, x = variable, group = variable, fill = variable),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = conf.high, ymin = conf.low, x = variable, group = variable),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        facet_grid(rows = vars(cost_outcome), cols = vars(cancer_type) , switch = "y", scales = "fixed") +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52")) +
        #, guide = guide_legend(reverse = TRUE)) +
        geom_hline(yintercept=0)  +      
        guides(fill="none", colour = "none")+
        labs(fill = "", x = "Stage at diagnosis", y = "Average Marginal Effect") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 0, vjust = 0, hjust=0,  size = 13),
              strip.text.x = element_text(  size = 13),
              axis.text.y = element_text(angle = 0, vjust = 0, hjust=0,  size = 13),
              strip.text.y = element_blank(),
              axis.ticks.y=element_blank()) 


#' _____ Plot AME annual
df_bystage_net_AME_annual <- df_net_AME %>%
        filter(grepl(c("Stage"), variable)) %>%
        filter(variable != "Stage: Missing/Unknown") %>%
        filter(grepl("ac_", cost_outcome)) %>%
        mutate(cost_outcome = case_when(cost_outcome == "ac_diag" ~ "Diag phase",
                                        cost_outcome == "ac_treat_0_1year" ~ "0-1 years",
                                        cost_outcome == "ac_treat_1_2year" ~ "1-2 years",
                                        cost_outcome == "ac_treat_2_3year" ~ "2-3 years",
                                        cost_outcome == "ac_treat_3_4year" ~ "3-4 years",
                                        cost_outcome == "ac_treat_4_5year" ~ '4-5 years',
                                        cost_outcome == "ac_treat_5year" ~ "5-6 years",
                                        .default = NA ) ) %>%
        mutate(cost_outcome = factor(cost_outcome, levels = c("Diag phase", "0-1 years", "1-2 years", "2-3 years", "3-4 years", "4-5 years", "5-6 years") )) %>%
        mutate(cancer_type = case_when(cancer_type == "colorectal" ~ "Colorectal",
                                       cancer_type == "head_neck" ~ "Head and neck",
                                       cancer_type == "liver" ~ "Liver",
                                       cancer_type == "lung" ~ "Lung",
                                       cancer_type == "lymphoma" ~ "Lymphoma",
                                       cancer_type == "oesophagus" ~ 'Oesophagus',
                                       cancer_type == "ovary" ~ "Ovary",
                                       cancer_type == "pancreas" ~ "Pancreas",
                                       .default = NA) ) 

plot_bystage_net_AME_annual <- df_bystage_net_AME_annual %>%
        ggplot() +
        geom_bar(aes(y = estimate, x = forcats::fct_rev(variable), group = forcats::fct_rev(variable), fill = forcats::fct_rev(variable)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = conf.high, ymin = conf.low, x = forcats::fct_rev(variable), group = forcats::fct_rev(variable)),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        facet_grid(rows = vars(cancer_type), cols = vars(cost_outcome) , switch = "y", scales = "fixed") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#94724F", "#628592", "#666666"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Average Marginal Effect") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              strip.text.y.left = element_text(angle = 0),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())  

#' _____ Plot AME PoC
df_bystage_net_AME_PoC <- df_net_AME %>%
        filter(grepl(c("Stage"), variable)) %>%
        filter(variable != "Stage: Missing/Unknown") %>%
        filter(!grepl("ac_|totalcost", cost_outcome)) %>%
        mutate(cost_outcome = case_when(cost_outcome == "diag" ~ "Diag phase",
                                        cost_outcome == "treat_0_1year" ~ "Initial treatment\n(0-1 years)",
                                        cost_outcome == "treat_1_2year" ~ "1-2 years",
                                        cost_outcome == "treat_2_3year" ~ "2-3 years",
                                        cost_outcome == "treat_3_4year" ~ "3-4 years",
                                        cost_outcome == "treat_4_5year" ~ '4-5 years',
                                        cost_outcome == "treat_5year" ~ "5-6 years",
                                        cost_outcome == "eol" ~ "End of life",
                                        .default = NA ) ) %>%
        mutate(cost_outcome = factor(cost_outcome, levels = c("Diag phase", "Initial treatment\n(0-1 years)", "1-2 years", "2-3 years", "3-4 years", "4-5 years", "5-6 years", "End of life") )) %>%
        mutate(cancer_type = case_when(cancer_type == "colorectal" ~ "Colorectal",
                                       cancer_type == "head_neck" ~ "Head and neck",
                                       cancer_type == "liver" ~ "Liver",
                                       cancer_type == "lung" ~ "Lung",
                                       cancer_type == "lymphoma" ~ "Lymphoma",
                                       cancer_type == "oesophagus" ~ 'Oesophagus',
                                       cancer_type == "ovary" ~ "Ovary",
                                       cancer_type == "pancreas" ~ "Pancreas",
                                       .default = NA) )

plot_bystage_net_AME_PoC <- df_bystage_net_AME_PoC %>%
        ggplot() +
        geom_bar(aes(y = estimate, x = forcats::fct_rev(variable), group = forcats::fct_rev(variable), fill = forcats::fct_rev(variable)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = conf.high, ymin = conf.low, x = forcats::fct_rev(variable), group = forcats::fct_rev(variable)),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        facet_grid(rows = vars(cancer_type), cols = vars(cost_outcome) , switch = "y", scales = "fixed") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Average Marginal Effect") +
        theme_bw() +
        theme(strip.text = element_text(face = "bold", size = 10),
              axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              strip.text.y.left = element_text(angle = 0),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())  


ALT_plot_bystage_net_AME_total <- df_bystage_net_AME_total %>%
        mutate(variable = str_sub(variable, 8, -1)) %>%
        ggplot() +
        geom_bar(aes(y = estimate, x = variable, group = variable, fill = variable),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = conf.high, ymin = conf.low, x = variable, group = variable),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        facet_grid(rows = vars(cost_outcome), cols = vars(cancer_type) , switch = "y", scales = "fixed") +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52")) +
        #, guide = guide_legend(reverse = TRUE)) +
        geom_hline(yintercept=0)  +      
        guides(fill="none", colour = "none")+
        labs(fill = "", x = "Stage at diagnosis", y = "Average Marginal Effect") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 0, vjust = 0, hjust=0,  size = 13),
              strip.text.x = element_text(  size = 13),
              axis.text.y = element_text(angle = 0, vjust = 0, hjust=0,  size = 13),
              strip.text.y = element_blank(),
              axis.ticks.y=element_blank()) 

#################################################################################
#' _____________________________________________________________________________
#' Comparing unadjusted to adjusted net
#' _____________________________________________________________________________

#' _____ Total
df_total_net_unadjusted_V_adjusted <- rbind(
        
        df_net_unadj_bystage_total_costs %>%
                filter(cancer_type %in% c( "Lung", "Colorectal", "Head and neck", "Liver", "Lymphoma", "Oesophagus", "Ovary", "Pancreas")) %>%
                filter(year == "2015") %>%
                select(cancer_type, stage, value_mean) %>%
                pivot_wider(names_from = stage, names_prefix = "Stage_", values_from = c("value_mean") ) %>%
                mutate(Stage1_2 = Stage_2 - Stage_1,
                       Stage1_3 = Stage_3 - Stage_1,
                       Stage1_4 = Stage_4 - Stage_1) %>%
                select(cancer_type, Stage1_2, Stage1_3,Stage1_4) %>%
                pivot_longer(Stage1_2:Stage1_4, names_to = "Stage", values_to = "estimate") %>%
                mutate(stage = case_when(Stage == "Stage1_2" ~ "2",
                                         Stage == "Stage1_3" ~ "3",
                                         Stage == "Stage1_4" ~ "4")) %>%
                select(cancer_type, stage, estimate) %>%
                mutate(analysis_type = "Unadjusted") %>%
                mutate(across(where(is.factor), as.character)),
        
        
        df_bystage_net_AME_total %>%
                select(-cost_outcome, -std.error, -p.value, -conf.low , -conf.high ) %>%
                rename(stage = variable) %>%
                mutate(stage = case_when(stage == "Stage: II" ~ "2",
                                         stage == "Stage: III" ~ "3",
                                         stage == "Stage: IV" ~ "4")) %>%
                mutate(analysis_type = "Adjusted")
) 


plot_total_net_unadjusted_V_adjusted <- df_total_net_unadjusted_V_adjusted %>% 
        mutate(across(where(is.character), as.factor)) %>%
        mutate(cost_outcome = "Total all-cause hospital costs") %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = fct_rev(cancer_type), group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid( fct_rev(analysis_type) ~ cost_outcome, switch = "y") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Cost difference to stage I (reference stage)") +
        theme_bw(base_size = 14) +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              axis.ticks.y=element_blank())

ALT_plot_total_net_unadjusted_difference <- df_total_net_unadjusted_V_adjusted %>% 
        mutate(stage = case_when(stage == "1"~ "I",
                                 stage == "2"~ "II",
                                 stage == "3"~ "III",
                                 stage == "4"~ "IV",
                                 .default = stage)) %>%
        filter(analysis_type == "Unadjusted") %>%
        mutate(across(where(is.character), as.factor)) %>%
        mutate(cancer_type = factor(cancer_type, levels = c( "Colorectal", "Head and neck", "Lymphoma", "Ovary" ,"Liver", "Lung", "Oesophagus", "Pancreas") )) %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = stage, group = stage, fill = stage),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid( cols = vars(cancer_type) , switch = "y", scales = "fixed") +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592")) +
        #, guide = guide_legend(reverse = TRUE)) +
        geom_hline(yintercept=0)  +      
        guides(fill="none", colour = "none")+
        labs(fill = "", x = "Stage at diagnosis", y = "Difference in mean NHS hospital\ncosts per patient relative to stage I") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 0, vjust = 0, hjust=0,  size = 13),
              strip.text.x = element_text(  size = 13),
              axis.text.y = element_text(angle = 0, vjust = 0, hjust=0,  size = 13),
              strip.text.y = element_blank(),
              axis.ticks.y=element_blank()) 




# Individual cancers 900 500

plot_ind_total_net_unadjusted_V_adjusted <- df_total_net_unadjusted_V_adjusted %>%
        filter(cancer_type %in% c( "Ovary")) %>%
        mutate(across(where(is.character), as.factor)) %>%
        mutate(cost_outcome = "Total all-cause hospital costs") %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = fct_rev(cancer_type), group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid( fct_rev(analysis_type) ~ cost_outcome, switch = "y") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Cost difference to stage I (reference stage)") +
        theme_bw(base_size = 14) +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              axis.ticks.y=element_blank())

#' _____ Annual
df_annual_net_unadjusted_V_adjusted <- rbind(
        
        df_net_unadj_bystage_annual_costs %>%
                filter(cancer_type %in% c( "Lung", "Colorectal", "Head and neck", "Liver", "Lymphoma", "Oesophagus", "Ovary", "Pancreas")) %>%
                select(cancer_type, stage, Period, value_mean) %>%
                pivot_wider(names_from = stage, names_prefix = "Stage_", values_from = c("value_mean") ) %>%
                mutate(Stage1_2 = Stage_2 - Stage_1,
                       Stage1_3 = Stage_3 - Stage_1,
                       Stage1_4 = Stage_4 - Stage_1) %>%
                select(cancer_type, Period, Stage1_2, Stage1_3,Stage1_4) %>%
                pivot_longer(Stage1_2:Stage1_4, names_to = "Stage", values_to = "estimate") %>%
                mutate(stage = case_when(Stage == "Stage1_2" ~ "2",
                                         Stage == "Stage1_3" ~ "3",
                                         Stage == "Stage1_4" ~ "4")) %>%
                select(cancer_type, Period, stage, estimate) %>%
                mutate(analysis_type = "Unadjusted") %>%
                rename(cost_outcome = Period) %>%
                mutate(across(where(is.factor), as.character)),
        
        
        df_bystage_net_AME_annual %>%
                select( -std.error, -p.value, -conf.low , -conf.high ) %>%
                rename(stage = variable) %>%
                mutate(stage = case_when(stage == "Stage: II" ~ "2",
                                         stage == "Stage: III" ~ "3",
                                         stage == "Stage: IV" ~ "4")) %>%
                mutate(analysis_type = "Adjusted")) %>% 
        mutate(cost_outcome = factor(cost_outcome, levels = c("Diag phase", "0-1 years", "1-2 years", "2-3 years", "3-4 years", "4-5 years", "5-6 years") )) %>%
        mutate(across(where(is.character), as.factor)) 

plot_annual_net_unadjusted_V_adjusted <- df_annual_net_unadjusted_V_adjusted %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = fct_rev(cancer_type), group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid( cost_outcome ~ analysis_type) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Cost difference to stage I") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              axis.ticks.y=element_blank())  

plot_annual_net_unadjusted_V_adjusted_Colorectaletc <- df_annual_net_unadjusted_V_adjusted %>%
        filter(cancer_type %in% c( "Colorectal", "Head and neck",  "Lymphoma", "Ovary")) %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = fct_rev(cancer_type), group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid( cost_outcome ~ analysis_type) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Cost difference to stage I") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              axis.ticks.y=element_blank())  

plot_annual_net_unadjusted_V_adjusted_Lungetc <- df_annual_net_unadjusted_V_adjusted %>%
        filter(cancer_type %in% c("Liver", "Lung", "Oesophagus", "Pancreas")) %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = fct_rev(cancer_type), group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid( cost_outcome ~ analysis_type) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Cost difference to stage I") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              axis.ticks.y=element_blank())  

#' _____ PoC
df_PoC_net_unadjusted_V_adjusted <- rbind(
        
        df_net_unadj_bystage_PoC_costs %>%
                filter(cancer_type %in% c( "Lung", "Colorectal", "Head and neck", "Liver", "Lymphoma", "Oesophagus", "Ovary", "Pancreas")) %>%
                select(cancer_type, stage, Phase, value_mean) %>%
                pivot_wider(names_from = stage, names_prefix = "Stage_", values_from = c("value_mean") ) %>%
                mutate(Stage1_2 = Stage_2 - Stage_1,
                       Stage1_3 = Stage_3 - Stage_1,
                       Stage1_4 = Stage_4 - Stage_1) %>%
                select(cancer_type, Phase, Stage1_2, Stage1_3, Stage1_4) %>%
                pivot_longer(Stage1_2:Stage1_4, names_to = "Stage", values_to = "estimate") %>%
                mutate(stage = case_when(Stage == "Stage1_2" ~ "2",
                                         Stage == "Stage1_3" ~ "3",
                                         Stage == "Stage1_4" ~ "4")) %>%
                select(cancer_type, Phase, stage, estimate) %>%
                mutate(analysis_type = "Unadjusted") %>%
                rename(cost_outcome = Phase) %>%
                mutate(across(where(is.factor), as.character)),
        
        
        df_bystage_net_AME_PoC %>%
                select( -std.error, -p.value, -conf.low , -conf.high ) %>%
                rename(stage = variable) %>%
                mutate(stage = case_when(stage == "Stage: II" ~ "2",
                                         stage == "Stage: III" ~ "3",
                                         stage == "Stage: IV" ~ "4")) %>%
                mutate(analysis_type = "Adjusted")) %>% 
        mutate(cost_outcome = factor(cost_outcome, levels = c("Diag phase", "Initial treatment\n(0-1 years)", "1-2 years", "2-3 years", "3-4 years", "4-5 years", "5-6 years", "End of life") )) %>%
        mutate(across(where(is.character), as.factor)) 

plot_PoC_net_unadjusted_V_adjusted <- df_PoC_net_unadjusted_V_adjusted %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = fct_rev(cancer_type), group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid( cost_outcome ~ analysis_type) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Cost difference to stage I") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              axis.ticks.y=element_blank())  

plot_PoC_net_unadjusted_V_adjusted_Colorectaletc <- df_PoC_net_unadjusted_V_adjusted %>%
        filter(cancer_type %in% c( "Colorectal", "Head and neck",  "Lymphoma", "Ovary")) %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = analysis_type, group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid(   fct_rev(cancer_type) ~ cost_outcome) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "", y = "Cost difference to stage I") +
        theme_bw() +
        theme(strip.text = element_text(face = "bold", size = 10),
              axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              axis.ticks.y=element_blank())  


plot_PoC_net_unadjusted_V_adjusted_Lungetc <- df_PoC_net_unadjusted_V_adjusted %>%
        filter(cancer_type %in% c("Liver", "Lung", "Oesophagus", "Pancreas")) %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = analysis_type, group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid(   fct_rev(cancer_type) ~ cost_outcome) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "", y = "Cost difference to stage I") +
        theme_bw() +
        theme(strip.text = element_text(face = "bold", size = 10),
              axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              axis.ticks.y=element_blank())  


#################################################################################
#' _____________________________________________________________________________
#' Comparing allcause to net 
#' _____________________________________________________________________________

df_total_allcause_V_net_unadjusted_V_adjusted <- rbind( df_total_allcause_unadjusted_V_adjusted %>% mutate(cost_type = "All-cause costs"),
                                                        df_total_net_unadjusted_V_adjusted %>% mutate(cost_type = "Net costs"))

plot_total_allcause_V_net_unadjusted_V_adjusted <- df_total_allcause_V_net_unadjusted_V_adjusted %>% 
        mutate(across(where(is.character), as.factor)) %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = fct_rev(cancer_type), group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid( cost_type ~ fct_rev(analysis_type) , switch = "y") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Cost difference to stage I (reference stage)") +
        theme_bw(base_size = 14) +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust= 0),
              axis.ticks.y=element_blank())

plot_total_allcause_V_net_unadjusted_V_adjusted_Colorectal <- df_total_allcause_V_net_unadjusted_V_adjusted %>% 
        filter(cancer_type == "Colorectal") %>%
        mutate(across(where(is.character), as.factor)) %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = fct_rev(cancer_type), group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid( cost_type ~ fct_rev(analysis_type) , switch = "y") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage", x = "", y = "Cost difference to stage I") +
        theme_bw(base_size = 14) +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust= 0),
              axis.ticks.y=element_blank())

plot_total_allcause_V_net_unadjusted_V_adjusted_HeadandNeck <- df_total_allcause_V_net_unadjusted_V_adjusted %>% 
        filter(cancer_type == "Head and neck") %>%
        mutate(across(where(is.character), as.factor)) %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = fct_rev(cancer_type), group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid( cost_type ~ fct_rev(analysis_type) , switch = "y") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage", x = "", y = "Cost difference to stage I") +
        theme_bw(base_size = 14) +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust= 0),
              axis.ticks.y=element_blank())

plot_total_allcause_V_net_unadjusted_V_adjusted_Lymphoma <- df_total_allcause_V_net_unadjusted_V_adjusted %>% 
        filter(cancer_type == "Lymphoma") %>%
        mutate(across(where(is.character), as.factor)) %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = fct_rev(cancer_type), group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid( cost_type ~ fct_rev(analysis_type) , switch = "y") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage", x = "", y = "Cost difference to stage I") +
        theme_bw(base_size = 14) +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust= 0),
              axis.ticks.y=element_blank())

plot_total_allcause_V_net_unadjusted_V_adjusted_Ovary <- df_total_allcause_V_net_unadjusted_V_adjusted %>% 
        filter(cancer_type == "Ovary") %>%
        mutate(across(where(is.character), as.factor)) %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = fct_rev(cancer_type), group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid( cost_type ~ fct_rev(analysis_type) , switch = "y") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage", x = "", y = "Cost difference to stage I") +
        theme_bw(base_size = 14) +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust= 0),
              axis.ticks.y=element_blank())


plot_total_allcause_V_net_unadjusted_V_adjusted_Lung <- df_total_allcause_V_net_unadjusted_V_adjusted %>% 
        filter(cancer_type == "Lung") %>%
        mutate(across(where(is.character), as.factor)) %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = fct_rev(cancer_type), group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid( cost_type ~ fct_rev(analysis_type) , switch = "y") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage", x = "", y = "Cost difference to stage I") +
        theme_bw(base_size = 14) +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust= 0),
              axis.ticks.y=element_blank())

plot_total_allcause_V_net_unadjusted_V_adjusted_Liver <- df_total_allcause_V_net_unadjusted_V_adjusted %>% 
        filter(cancer_type == "Liver") %>%
        mutate(across(where(is.character), as.factor)) %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = fct_rev(cancer_type), group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid( cost_type ~ fct_rev(analysis_type) , switch = "y") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage", x = "", y = "Cost difference to stage I") +
        theme_bw(base_size = 14) +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust= 0),
              axis.ticks.y=element_blank())

plot_total_allcause_V_net_unadjusted_V_adjusted_Oesophagus <- df_total_allcause_V_net_unadjusted_V_adjusted %>% 
        filter(cancer_type == "Oesophagus") %>%
        mutate(across(where(is.character), as.factor)) %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = fct_rev(cancer_type), group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid( cost_type ~ fct_rev(analysis_type) , switch = "y") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage", x = "", y = "Cost difference to stage I") +
        theme_bw(base_size = 14) +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust= 0),
              axis.ticks.y=element_blank())

plot_total_allcause_V_net_unadjusted_V_adjusted_Pancreas <- df_total_allcause_V_net_unadjusted_V_adjusted %>% 
        filter(cancer_type == "Pancreas") %>%
        mutate(across(where(is.character), as.factor)) %>%
        ggplot() + 
        geom_bar(aes(y = estimate, x = fct_rev(cancer_type), group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        facet_grid( cost_type ~ fct_rev(analysis_type) , switch = "y") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage", x = "", y = "Cost difference to stage I") +
        theme_bw(base_size = 14) +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust= 0),
              axis.ticks.y=element_blank())





















```{r}


df_total_difference_compare <- rbind( 
        
        df_allcause_unadj_total_costs %>% 
                pivot_longer(cols = cost_2014:cost_2017, names_to = "year") %>%
                separate_wider_delim(cols = value, delim = " ", names = c("mean", "sd", "se"), names_sep = "_",
                                     too_few = "align_start") %>%
                mutate(value_sd = as.numeric(str_sub(value_sd, start = 2L, end = -2L)),
                       value_se = as.numeric(str_sub(value_se, start = 2L, end = -2L)),
                       value_mean = as.numeric(value_mean)) %>%
                filter(grepl(c("Stage at diagnosis"), Variable)) %>%
                mutate(stage = str_sub(Variable, start = 20L, end = -1L)) %>%
                select(cancer_type, stage, year, value_mean, value_sd) %>%
                mutate(year = str_sub(year, start = 6L, end = -1L)) %>% 
                mutate(across(where(is.character), as.factor)) %>%
                mutate(cancer_type = forcats::fct_rev(cancer_type)) %>%
                filter(stage != "0" & stage != "Unknown/Missing") %>%
                filter(cancer_type %in% c( "Lung", "Colorectal", "Head and neck", "Liver", "Lymphoma", "Oesophagus", "Ovary", "Pancreas")) %>%
                filter(year == "2014") %>%
                select(cancer_type, stage, value_mean, value_sd) %>%
                # Add number on for SE calc
                left_join(l_allcause_descriptives_8cancers[[1]][27:30 ,2:10] %>% 
                                  rename(stage = `...2`) %>%
                                  pivot_longer(`Lung N (%)`:`Pancreas N (%)`, values_to = "N", names_to = "cancer_type") %>%
                                  mutate(across(cancer_type:N, \(x) word(x,1))) %>% mutate(N = as.numeric(N)) %>%
                                  mutate(cancer_type = case_when(cancer_type == "Head" ~ "Head and neck",
                                                                 .default = cancer_type))) %>%
                pivot_wider(names_from = stage, names_prefix = "Stage_", values_from = c("value_mean", "value_sd", "N") ) %>%
                mutate(mean_Stage1_2 = value_mean_Stage_2 - value_mean_Stage_1,
                       se_Stage1_2 = sqrt( ((value_sd_Stage_1^2)/N_Stage_1) +  ((value_sd_Stage_2^2)/N_Stage_2)),
                       mean_Stage1_3 = value_mean_Stage_3 - value_mean_Stage_1,
                       se_Stage1_3 = sqrt( ((value_sd_Stage_1^2)/N_Stage_1) +  ((value_sd_Stage_3^2)/N_Stage_3)),
                       mean_Stage1_4 = value_mean_Stage_4 - value_mean_Stage_1,
                       se_Stage1_4 = sqrt( ((value_sd_Stage_1^2)/N_Stage_1) +  ((value_sd_Stage_4^2)/N_Stage_4))) %>%
                select(cancer_type, mean_Stage1_2, se_Stage1_2,mean_Stage1_3,se_Stage1_3, mean_Stage1_4, se_Stage1_4) %>%
                pivot_longer(mean_Stage1_2:se_Stage1_4, names_to = "type_stage", values_to = "estimate") %>%
                separate_wider_delim(type_stage, "_", names = c("outcome", "stage"), too_many = "merge")%>%
                mutate(stage = str_sub(stage,-1,-1)) %>%
                pivot_wider(names_from = "outcome", values_from = "estimate") %>%
                mutate(value_2.5CI  = mean - qnorm(0.975) * se,
                       value_97.5CI  = mean + qnorm(0.975) * se) %>%
                mutate(stage = case_when(stage == "2" ~ "II",
                                         stage == "3" ~ "III",
                                         stage == "4" ~ "IV")) %>%
                mutate(analysis_type = "All-cause unadjusted") %>%
                mutate(across(where(is.factor), as.character)) %>%
                select(cancer_type, stage, mean, value_2.5CI, value_97.5CI, analysis_type),
        
        
        df_bystage_allcause_AME_total %>%
                select(-cost_outcome, -p.value, -std.error ) %>%
                rename(stage = variable) %>%
                mutate(stage = case_when(stage == "Stage: II" ~ "II",
                                         stage == "Stage: III" ~ "III",
                                         stage == "Stage: IV" ~ "IV")) %>%
                mutate(analysis_type = "All-cause covariate-adjusted") %>%
                rename(mean = estimate,
                       value_2.5CI = conf.low,
                       value_97.5CI = conf.high),
        
        
        df_net_unadj_total_costs %>% 
                pivot_longer(cols = costs_2014:costs_2017, names_to = "year") %>%
                separate_wider_delim(cols = value, delim = " ", names = c("mean", "sd", "se"), names_sep = "_",
                                     too_few = "align_start") %>%
                mutate(value_sd = as.numeric(str_sub(value_sd, start = 2L, end = -2L)),
                       value_se = as.numeric(str_sub(value_se, start = 2L, end = -2L)),
                       value_mean = as.numeric(value_mean)) %>%
                filter(grepl(c("Stage at diagnosis"), Variable)) %>%
                mutate(stage = str_sub(Variable, start = 20L, end = -1L)) %>%
                select(cancer_type, stage, year, value_mean, value_sd) %>%
                mutate(year = str_sub(year, start = 7L, end = -1L)) %>% 
                mutate(across(where(is.character), as.factor)) %>%
                mutate(cancer_type = forcats::fct_rev(cancer_type)) %>%
                filter(stage != "0" & stage != "Unknown/Missing") %>%
                filter(cancer_type %in% c( "Lung", "Colorectal", "Head and neck", "Liver", "Lymphoma", "Oesophagus", "Ovary", "Pancreas")) %>%
                filter(year == "2014") %>%
                select(cancer_type, stage, value_mean, value_sd) %>%
                # Add number on for SE calc
                left_join(l_net_descriptives_8cancers[[1]][26:29 ,2:10] %>% 
                                  rename(stage = `...2`) %>%
                                  pivot_longer(`Lung N (%)`:`Pancreas N (%)`, values_to = "N", names_to = "cancer_type") %>%
                                  mutate(across(cancer_type:N, \(x) word(x,1))) %>% mutate(N = as.numeric(N)) %>%
                                  mutate(cancer_type = case_when(cancer_type == "Head_neck" ~ "Head and neck",
                                                                 .default = cancer_type))) %>%
                pivot_wider(names_from = stage, names_prefix = "Stage_", values_from = c("value_mean", "value_sd", "N") ) %>%
                mutate(mean_Stage1_2 = value_mean_Stage_2 - value_mean_Stage_1,
                       se_Stage1_2 = sqrt( ((value_sd_Stage_1^2)/N_Stage_1) +  ((value_sd_Stage_2^2)/N_Stage_2)),
                       mean_Stage1_3 = value_mean_Stage_3 - value_mean_Stage_1,
                       se_Stage1_3 = sqrt( ((value_sd_Stage_1^2)/N_Stage_1) +  ((value_sd_Stage_3^2)/N_Stage_3)),
                       mean_Stage1_4 = value_mean_Stage_4 - value_mean_Stage_1,
                       se_Stage1_4 = sqrt( ((value_sd_Stage_1^2)/N_Stage_1) +  ((value_sd_Stage_4^2)/N_Stage_4))) %>%
                select(cancer_type, mean_Stage1_2, se_Stage1_2,mean_Stage1_3,se_Stage1_3, mean_Stage1_4, se_Stage1_4) %>%
                pivot_longer(mean_Stage1_2:se_Stage1_4, names_to = "type_stage", values_to = "estimate") %>%
                separate_wider_delim(type_stage, "_", names = c("outcome", "stage"), too_many = "merge")%>%
                mutate(stage = str_sub(stage,-1,-1)) %>%
                pivot_wider(names_from = "outcome", values_from = "estimate") %>%
                mutate(value_2.5CI  = mean - qnorm(0.975) * se,
                       value_97.5CI  = mean + qnorm(0.975) * se) %>%
                mutate(stage = case_when(stage == "2" ~ "II",
                                         stage == "3" ~ "III",
                                         stage == "4" ~ "IV")) %>%
                mutate(analysis_type = "Net unadjusted") %>%
                mutate(across(where(is.factor), as.character)) %>%
                select(cancer_type, stage, mean, value_2.5CI, value_97.5CI, analysis_type),
        
        
        df_bystage_net_AME_total %>%
                select(-cost_outcome, -p.value, -std.error ) %>%
                rename(stage = variable) %>%
                mutate(stage = case_when(stage == "Stage: II" ~ "II",
                                         stage == "Stage: III" ~ "III",
                                         stage == "Stage: IV" ~ "IV")) %>%
                mutate(analysis_type = "Net covariate-adjusted") %>%
                rename(mean = estimate,
                       value_2.5CI = conf.low,
                       value_97.5CI = conf.high)
        
)

plot_total_difference_compare <- df_total_difference_compare %>%
        mutate(analysis_type = factor(analysis_type, levels = c("All-cause unadjusted","All-cause covariate-adjusted","Net unadjusted","Net covariate-adjusted"))) %>%
        ggplot() +
        geom_bar(aes(y = mean, x = stage, group = stage, fill = stage),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = value_97.5CI, ymin = value_2.5CI, x = stage, group = stage),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        facet_grid(rows = vars(analysis_type), cols = vars(cancer_type) , switch = "y", scales = "fixed") +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52")) +
        #, guide = guide_legend(reverse = TRUE)) +
        geom_hline(yintercept=0)  +      
        guides(fill="none", colour = "none")+
        labs(fill = "", x = "Stage at diagnosis", y = "Difference in cost to stage I") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 0, vjust = 0, hjust=0,  size = 11),
              strip.text.x = element_text(  size = 11),
              axis.text.y = element_text(angle = 0, vjust = 0, hjust=0,  size = 11),
              strip.text.y = element_text(  size = 11),
              axis.ticks.y=element_blank())

ggsave(paste0("~/Miscellaneous/HDI/Code for publication/", "fig_1.png"), plot_total_difference_compare, device = "png",
       width = 270,
       height = 270,
       units = "mm")

```




```{r}

# Reshape to link on to costs dataframe

df_allcause_unadj_bystage_PoC_costs_annualised <- df_allcause_unadj_bystage_PoC_costs %>%
        filter(cancer_type %in% c( "Lung", "Colorectal", "Head and neck", "Liver", "Lymphoma", "Oesophagus", "Ovary", "Pancreas")) %>%
        left_join(df_PoC_NmonthsPersons_8cancers %>% 
                          pivot_longer(`Diag phase mean proportion`:`End of life mean proportion`, names_to = "Phase", values_to = "Mean proportion of phase") %>%
                          mutate(Phase = str_remove(Phase, " mean proportion")) %>%
                          filter(stage %in% c("1", "2", "3", "4")),
                  by = c("cancer_type" = "Cancer type", "stage", "Phase" )) %>%
        mutate(`Annualised mean cost` =  value_mean / `Mean proportion of phase` ,
               `Annualised 2.5CI` =   value_2.5CI / `Mean proportion of phase`,
               `Annualised 97.5CI` =  value_97.5CI / `Mean proportion of phase`) %>%
        mutate(Phase = factor(Phase, levels = c("Diag phase", "Initial treatment\n(0-1 years)", "1-2 years", "2-3 years", "3-4 years", "4-5 years", "5-6 years", "End of life") )) %>%
        mutate(stage = forcats::fct_rev(stage)) 

plot_allcause_unadj_PoC_bystage_8cancers_annualised <- df_allcause_unadj_bystage_PoC_costs_annualised %>%
        ggplot() + 
        geom_bar(aes(y = `Annualised mean cost`, x = stage, group = stage, fill = stage),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .7) +
        geom_errorbar(aes(ymax = `Annualised 2.5CI`, ymin = `Annualised 97.5CI`, x = stage, group = stage),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        
        facet_grid(rows = vars(cancer_type), cols = vars(Phase) , switch = "y", scales = "fixed") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = NULL, y = "Mean costs") +
        theme_bw(base_size = 12) +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0),
              strip.text.y.left = element_text(angle = 0, size = 12),
              strip.text.x.top = element_text(angle = 0, size = 12),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              legend.position="bottom") 

# Focus on initial phase
df_allcause_unadj_bystage_PoC_costs_annualised_initial <- df_allcause_unadj_bystage_PoC_costs_annualised %>% filter(Phase == "Initial treatment\n(0-1 years)") %>%
        select(-`Mean proportion of phase`) %>%
        pivot_longer(cols = value_mean:`Annualised 97.5CI`, names_to = "outcome", values_to = "value") %>%
        mutate(Type = case_when(grepl(c("Annualised"), outcome) ~ "Annualised",
                                .default = "Non-annualised"),
               outcome = case_when(grepl(c("mean"), outcome) ~ "Mean",
                                   grepl(c("2.5"), outcome) ~ "2.5CI",
                                   grepl(c("97.5CI"), outcome) ~ "97.5CI",
                                   .default = outcome)) %>%
        pivot_wider(names_from = outcome, values_from = value) %>%
        mutate(Type = factor(Type, levels = c( "Non-annualised", "Annualised") )) %>%
        select(-Phase)


plot_allcause_unadj_PoC_bystage_8cancers_annualised_initial <- df_allcause_unadj_bystage_PoC_costs_annualised_initial %>%
        ggplot() + 
        geom_bar(aes(y = `Mean`, x = Type, group = fct_rev(stage), fill = fct_rev(stage)),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = `2.5CI`, ymin = `97.5CI`, x = Type, group = fct_rev(stage)),
                      position= position_dodge(0.5), width = 0.2, colour = "black") +
        facet_wrap(facets = "cancer_type", ncol = 2) +
        scale_y_continuous(labels = scales::label_currency(scale = .001, prefix = "£", suffix = "K")) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592"), guide = guide_legend(reverse = FALSE)) +
        labs(fill = "Stage at diagnosis", x = NULL, y = "Mean initial treatment (0-1 years) cost") +
        theme_bw(base_size = 12) +
        theme(axis.title = element_text(face = "bold", size = 17),
              strip.text.y.left = element_text(angle = 0, size = 12),
              strip.text.x.top = element_text(angle = 0, size = 12),
              legend.position="bottom") 









```




Regression summaries for the two part regression models used for the prediction of baseline cancer costs

### Supplemental Table S34: Regression summaries for the two part regression models used for the prediction of baseline cancer costs: Colorectum cancer

```{r}

```

### Supplemental Table S35: Regression summaries for the two part regression models used for the prediction of baseline cancer costs: Head and neck cancer

```{r}
```

### Supplemental Table S36: Regression summaries for the two part regression models used for the prediction of baseline cancer costs: Liver cancer

```{r}
```

### Supplemental Table S37: Regression summaries for the two part regression models used for the prediction of baseline cancer costs: Lung cancer

```{r}
```

### Supplemental Table S38: Regression summaries for the two part regression models used for the prediction of baseline cancer costs: Lymphoma cancer

```{r}
```

### Supplemental Table S39: Regression summaries for the two part regression models used for the prediction of baseline cancer costs: Oesophagus cancer

```{r}
```

### Supplemental Table S40: Regression summaries for the two part regression models used for the prediction of baseline cancer costs: Ovarian cancer

```{r}
```

### Supplemental Table S41: Regression summaries for the two part regression models used for the prediction of baseline cancer costs: Pancreas cancer

```{r}
```















