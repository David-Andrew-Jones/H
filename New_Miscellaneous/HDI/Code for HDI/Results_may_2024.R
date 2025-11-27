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

#' _____ Total costs

results_path <- "~/Miscellaneous/HDI/23_Oct_2024/Costs_GRAIL_SE_2024-10-21.xlsx"

df_total_costs <- results_path %>% 
        excel_sheets() %>% 
        set_names() %>% 
        map_df(~ read_excel(path = results_path, sheet = .x), .id = "Sheet") %>%
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
                                       .default = Sheet  ) ) %>%
        select(-Sheet)

#' _____ Annual costs

results_path <- "~/Miscellaneous/HDI/23_Oct_2024/Costs_annual_GRAIL_SE_2024-10-21.xlsx"

df_annual_costs <- results_path %>% 
        excel_sheets() %>% 
        set_names() %>% 
        map_df(~ read_excel(path = results_path, sheet = .x), .id = "Sheet") %>%
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
                                       .default = Sheet  ) ) %>%
        select(-Sheet)

#' _____ PoC costs

results_path <- "~/Miscellaneous/HDI/23_Oct_2024/Costs_phase_GRAIL_SE_2024-10-21.xlsx"

df_PoC_costs <- results_path %>% 
        excel_sheets() %>% 
        set_names() %>% 
        map_df(~ read_excel(path = results_path, sheet = .x), .id = "Sheet") %>%
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
                                       .default = Sheet  ) ) %>%
        select(-Sheet)


#' _____________________________________________________________________________
#' Total costs by stage

df_bystage_total_costs <- df_total_costs %>% 
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

#### Plot of costs by cancer type, stage and year of diagnosis
plot_allcosts_bystage <- df_bystage_total_costs %>%
        ggplot() + 
        geom_bar(aes(y = value_mean, x = cancer_type, group = stage, fill = stage),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        geom_errorbar(aes(ymax = value_2.5CI, ymin = value_97.5CI, x = cancer_type, group = stage),
                      position= position_dodge(0.5), width = 0.2, colour = "black",) +
        # ordered by differrence
        # geom_bar(aes(x = fct_relevel(`Cancer site`, factor_order_male_4v3),
        #              y = `Difference in loss of life expectancy (years)`), position= position_dodge(0.5), stat = "identity", width = .5) +
        facet_grid( ~ year) +
        coord_flip() +
        ggsci::scale_fill_nejm() +
        labs(fill = "Stage at diagnosis", y = "Mean costs (£)", x = "Cancer type") +
        theme_bw() 
        
#### Difference in the mean of costs between stage 1 and 4 and SD - change still

df_mean_differences <- df_bystage_total_costs %>%
        select(cancer_type:costs_2014_sd) %>%
        filter(stage %in% c("1", "4")) %>%
        mutate(costs_2014_sd = str_sub(costs_2014_sd, 2, -2)) %>%
        mutate(across(c(costs_2014_mean, costs_2014_sd), as.numeric)) %>%
        mutate(costs_2014_var = costs_2014_sd^2) %>%
        select(-costs_2014_sd) %>%
        pivot_wider(names_from = stage, names_prefix = "Stage_", values_from = c("costs_2014_mean", "costs_2014_var") ) %>%
        mutate(diff_1v4_meancosts = costs_2014_mean_Stage_4 - costs_2014_mean_Stage_1,
               var_diff_1v4_meancosts = costs_2014_var_Stage_1 + costs_2014_var_Stage_4,
               sd_diff_1v4_meancosts = sqrt(var_diff_1v4_meancosts)) %>%
        select(cancer_type, diff_1v4_meancosts, sd_diff_1v4_meancosts)
        
#' _____________________________________________________________________________
#' Annual costs by stage


#### Plot of costs by cancer type, stage and year of diagnosis
plot_annual_bystage <- df_bystage_annual_costs %>%
        select(cancer_type, stage, ends_with("mean")) %>%
        rename(`6 months to Diag` = ac_Diagnosisphase_mean,
               `0-1 years` = ac_Treat_0_1_phase_mean,
               `1-2 years` = ac_Treat_1_2_phase_mean,
               `2-3 years` = ac_Treat_2_3_phase_mean,
               `3-4 years` = ac_Treat_3_4_phase_mean,
               `4-5 years` = ac_Treat_4_5_phase_mean,
               `5-6 years` = ac_Treat_5_phase_mean) %>% 
        pivot_longer(cols = `6 months to Diag`:`5-6 years`, names_to = "Period", values_to = "mean_cost") %>%
        mutate(Period = factor(Period, levels = c("6 months to Diag","0-1 years" ,"1-2 years", "2-3 years", "3-4 years", "4-5 years", "5-6 years") )) %>%
        mutate(mean_cost = as.numeric(mean_cost)) %>%
        mutate(across(where(is.character), as.factor)) %>%
        mutate(stage = forcats::fct_rev(stage)) %>%
        mutate(cancer_type = forcats::fct_rev(cancer_type)) %>%
       # filter(stage != "0" & stage != "Unknown/Missing") %>%
        ggplot() + 
        geom_bar(aes(y = mean_cost, x = stage, fill = stage),
                 position= position_dodge(0.5),
                 stat = "identity",
                 width = .5) +
        # ordered by differrence
        # geom_bar(aes(x = fct_relevel(`Cancer site`, factor_order_male_4v3),
        #              y = `Difference in loss of life expectancy (years)`), position= position_dodge(0.5), stat = "identity", width = .5) +
        facet_grid(rows = vars(cancer_type), cols = vars(Period) , switch = "y", scales = "fixed") +
        coord_flip() +
        ggsci::scale_fill_nejm() +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Mean costs (£)") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 70, vjust = -0.1, hjust=0),
              strip.text.y.left = element_text(angle = 0),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())  

#' _____________________________________________________________________________
#' Phase of care costs by stage

#### Plot of costs by cancer type, stage and year of diagnosis
df_bystage_PoC_costs <- df_PoC_costs %>% 
        rename(`Diag phase` = Diagnosis,
               `Initial treatment\n(0-1 years)` = Treat0_1_phase,
               `1-2 years` = Treat1_2_phase,
               `2-3 years` = Treat2_3_phase,
               `3-4 years` = Treat3_4_phase,
               `4-5 years` = Treat4_5_phase,
               `5-6 years` = Treat5_phase,
               `End of life` = Eol) %>% 
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
        
plot_PoC_bystage <- df_bystage_PoC_costs %>%
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
        ggsci::scale_fill_nejm() +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Mean costs (£)") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 70, vjust = -0.1, hjust=0),
              strip.text.y.left = element_text(angle = 0),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())  



#_______________________________________________________________________________
#' ISPOR, total costs and Phase of care costs  

df_allcosts_and_PoC_bystage <- rbind(
        
        df_bystage_total_costs %>%
                filter(year == "2014") %>%
                mutate(Phase = "Total mean costs") %>%
                select(cancer_type, stage , Phase, value_mean, value_2.5CI, value_97.5CI) %>%
                mutate(across(where(is.factor), as.character)),
        
        df_bystage_PoC_costs %>% mutate(across(where(is.factor), as.character))
        
) %>%
        mutate(across(where(is.character), as.factor)) %>%
        mutate(stage = forcats::fct_rev(stage)) %>%
        mutate(Phase = factor(Phase, levels = c("Total mean costs", "Diag phase", "End of life", "Initial treatment\n(0-1 years)", "1-2 years", "2-3 years", "3-4 years", "4-5 years", "5-6 years") ))
        
        
plot_allcosts_and_PoC_bystage <- df_allcosts_and_PoC_bystage %>%
        ggplot() + 
        geom_bar(aes(y = value_mean, x = stage, fill = stage),
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
        scale_fill_manual(values = c("#B73D27", "#D29A22", "#628592", "#546741"), guide = guide_legend(reverse = TRUE)) +
        labs(fill = "Stage at diagnosis", x = "Cancer type", y = "Mean costs") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 17),
              axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
              strip.text.y.left = element_text(angle = 0),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())  

ggsave("~/Miscellaneous/HDI/ISPOREU2024_figures/Fig1.svg", plot_allcosts_and_PoC_bystage, device = "svg",
       width = 310,
       height = 200,
       units = "mm")

ggsave("~/Miscellaneous/HDI/ISPOREU2024_figures/Fig1.eps", plot_allcosts_and_PoC_bystage, device = "eps",
       width = 310,
       height = 200,
       units = "mm")

ggsave("~/Miscellaneous/HDI/ISPOREU2024_figures/Fig1.tiff", plot_allcosts_and_PoC_bystage, device = "tiff",
       width = 310,
       height = 200,
       units = "mm")

        
        
