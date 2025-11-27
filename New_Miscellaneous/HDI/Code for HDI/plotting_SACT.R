
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(stringr)

#' _____________________________________________________________________________
#' Treatment data 2013 to 2021

df_treatments13to21_bycancertype <- read_csv("~/Miscellaneous/HDI/NCRAS-CRUK - Treatment raw data for all cancers 2013-2021 2024-08-29.csv") %>%
        # Add up all head and neck, lung, Haematological malignancy
        mutate(`Cancer type` = case_when(grepl(c("Head and neck"),`Cancer type`) ~ "Head and neck",
                                         grepl(c("Lung"),`Cancer type`) ~ "Lung",
                                         grepl(c("Haematological malignancy"),`Cancer type`) ~ "Haematological malignancy",
                                         grepl(c("Liver"),`Cancer type`) ~ "Liver (experimental)",
                                         .default = `Cancer type`)) %>%
        group_by(`Cancer type`,`Year of Diagnosis`, `Factor of interest`,`Levels of factor`) %>%
        mutate(`Number of tumours receiving SACT` = case_when(`Chemotherapy (Y/N)` == "Yes" ~ `Number of tumours treated`, .default = 0),
               `Number of tumours receiving other treatment only` = case_when(`Chemotherapy (Y/N)` == "No" &
                                                                                      `Radiotherapy (Y/N)` == "No" &
                                                                              `Tumour Resection (Y/N)` == "No"~ `Number of tumours treated`, .default = 0)) %>%
        summarise(`Total number of tumours`= sum(`Number of tumours treated`),
                  `Total number of tumours receiving SACT`= sum(`Number of tumours receiving SACT`),
                  `Total number of tumours receiving other treatment only`= sum(`Number of tumours receiving other treatment only`)) %>%
        mutate(`Percentage of tumours receiving SACT` = round((`Total number of tumours receiving SACT`/ `Total number of tumours`) * 100 ),
               `Percentage of tumours receiving other treatment only` = round((`Total number of tumours receiving other treatment only`/ `Total number of tumours`) * 100 )) %>%
        ungroup() 

df_NOTreat13to21 <- df_treatments13to21_bycancertype %>%
        filter(`Year of Diagnosis` == "2013-2021") %>%
        #mutate(`Cancer type` = forcats::fct_rev(forcats::fct_reorder(`Cancer type`, .x = `Percentage change in number of patient recieving SACT from 2013`)) ) %>%
        filter(`Cancer type` %in% c("Breast",
                                    "Cervical",
                                    "Colon and rectosigmoid junction",
                                    "Haematological malignancy",
                                    "Head and neck",                                                                       
                                    "Liver (experimental)",                                                              
                                    "Lung",                                                                                            
                                    "Oesophagus",
                                    "Ovary",
                                    "Pancreatic",
                                    "Prostate",
                                    "Rectum",
                                    "Stomach"))

# Figure 1 plot by stage
plot_bystage_NOTreat13to21 <- df_NOTreat13to21 %>%
        filter(`Factor of interest` == "Stage at cancer diagnosis") %>%
        mutate(across(c("Cancer type", "Levels of factor"), \(x) as.factor(x))) %>%
        ggplot() +
        geom_bar(aes(y = `Percentage of tumours receiving other treatment only`, x = forcats::fct_rev(`Cancer type`)),
                 position= position_dodge(0.5), stat = "identity", width = .5) +
        theme_bw(base_size = 12) +
        theme(panel.grid = element_blank()) +
        facet_wrap( ~ `Levels of factor`, nrow = 1) +
        labs(x = "Cancer type") +
        coord_flip() 

# Figure 2 plot by age
plot_byage_NOTreat13to21 <- df_NOTreat13to21 %>%
        filter(`Factor of interest` == "Age at cancer diagnosis") %>%
        mutate(across(c("Cancer type", "Levels of factor"), \(x) as.factor(x))) %>%
        ggplot() +
        geom_bar(aes(y = `Percentage of tumours receiving other treatment only`, x = forcats::fct_rev(`Cancer type`)),
                 position= position_dodge(0.5), stat = "identity", width = .5) +
        theme_bw(base_size = 12) +
        theme(panel.grid = element_blank()) +
        facet_wrap( ~ `Levels of factor`, nrow = 1) +
        labs(x = "Cancer type") +
        coord_flip() 


# Figure 3 plot by stage over time
plot_NoCare_by_year <- df_treatments13to21_bycancertype %>%
    filter(`Factor of interest` == "Stage at cancer diagnosis") %>%
    filter(`Year of Diagnosis` != "2013-2021") %>%
    filter(`Cancer type` %in% c("Breast",
                                "Cervical",
                                "Colon and rectosigmoid junction",
                                "Haematological malignancy",
                                "Head and neck",                                                                       
                                "Liver (experimental)",                                                              
                                "Lung",                                                                                            
                                "Oesophagus",
                                "Ovary",
                                "Pancreatic",
                                "Prostate",
                                "Rectum",
                                "Stomach")) %>%
    group_by(`Cancer type`,`Levels of factor`) %>%
    arrange(`Year of Diagnosis`, .by_group = TRUE) %>%
    mutate(initial = first(`Percentage of tumours receiving other treatment only`)) %>% 
    ungroup() %>%
    mutate(`Change in percentage of patient recieving other care only` = `Percentage of tumours receiving other treatment only` - initial) %>%
    mutate(across(c("Cancer type", "Levels of factor"), \(x) as.factor(x))) %>%
    ggplot() +
    geom_line(aes(y = `Change in percentage of patient recieving other care only`, x = `Year of Diagnosis`, group = `Cancer type`, colour = `Cancer type`)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    theme_bw(base_size = 12) +
    theme(panel.grid = element_blank()) +
    facet_wrap( ~ `Levels of factor`, ncol = 2) +
    labs(colour = "Stage at diagnosis")
        

# Figure 4 plot by stage over time

plot_SACT13to21_bycancertype <- df_treatments13to21_bycancertype %>%
        filter(`Factor of interest` == "Stage at cancer diagnosis") %>%
        filter(`Year of Diagnosis` != "2013-2021") %>%
    filter(`Cancer type` %in% c("Breast",
                                "Cervical",
                                "Colon and rectosigmoid junction",
                                "Haematological malignancy",
                                "Head and neck",                                                                       
                                "Liver (experimental)",                                                              
                                "Lung",                                                                                            
                                "Oesophagus",
                                "Ovary",
                                "Pancreatic",
                                "Prostate",
                                "Rectum",
                                "Stomach")) %>%
        group_by(`Cancer type`,`Levels of factor`) %>%
        arrange(`Year of Diagnosis`, .by_group = TRUE) %>%
        mutate(initial = first(`Percentage of tumours receiving SACT`)) %>% 
        ungroup() %>%
        mutate(`Change in percentage of patient recieving SACT` = `Percentage of tumours receiving SACT` - initial) %>%
        #mutate(`Cancer type` = forcats::fct_rev(forcats::fct_reorder(`Cancer type`, .x = `Percentage change in number of patient recieving SACT from 2013`)) ) %>%
        mutate(across(c("Cancer type", "Levels of factor"), \(x) as.factor(x))) %>%
        ggplot() +
        geom_line(aes(y = `Change in percentage of patient recieving SACT`, x = `Year of Diagnosis`, group = `Levels of factor`, colour = `Levels of factor`)) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        theme_bw(base_size = 12) +
        theme(panel.grid = element_blank()) +
        facet_wrap( ~ `Cancer type`, ncol = 2) +
        labs(colour = "Stage at diagnosis")
        
        

#' _____________________________________________________________________________
#' SACT activity data Jan 2019 to Nov 2023

df_SACT_regimens19to23_bycancertype <- read_csv("~/Miscellaneous/HDI/Code for HDI/SACTactivity_oneway_breakdown_data National Regimens Tumour group.csv") %>%
        filter(!is.na(Month)) %>%
        mutate(month_numeric = as.integer(factor(Month, levels = month.name))) %>%
        mutate(Date = lubridate::make_date(Year, month_numeric)) %>%
        rename(`Cancer type` = `Breakdown value`) %>%
        group_by(`Cancer type`) %>%
        arrange(Date, .by_group = TRUE) %>%
        mutate(initial = first(`Adjusted count`)) %>% 
        ungroup() %>%
        mutate(`Percentage change from Jan 2019` = round(((`Adjusted count`- initial)/initial)*100, 1)) %>%
        mutate(`Cancer type` = forcats::fct_rev(forcats::fct_reorder(`Cancer type`, .x = `Percentage change from Jan 2019`)) ) %>%
        group_by(`Cancer type`) %>%
        slice_max(Date) %>%
        ungroup() 






%>%
        ggplot() +
        geom_line(aes(y = `Percentage change from Jan 2019`, x = Date)) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        theme_bw(base_size = 12) +
        theme(panel.grid = element_blank()) +
        facet_wrap( ~ `Cancer type`, ncol = 3) 
