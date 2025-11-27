#' _____________________________________________________________________________

#' Calculate Absolute and Proportional QALY shortfall

#' _____________________________________________________________________________
library(tidyverse)
data_dir <- "~/GalleriCEmodel/data-raw/England_pilot_planning/"

# Read in results files

l_model_outcomes <- list(model_outcomes_survival_1HR = read.csv(file  = "~/GalleriCEmodel/analyses/NICE_modifier/model_outcomes_survival_1HR_fast.csv", header = TRUE),
                         model_outcomes_survival_1halfHR = read.csv(file = "~/GalleriCEmodel/analyses/NICE_modifier/model_outcomes_survival_1halfHR_fast.csv", header = TRUE),
                         model_outcomes_survival_3HR = read.csv(file = "~/GalleriCEmodel/analyses/NICE_modifier/model_outcomes_survival_3HR_fast.csv", header = TRUE))


l_model_outcomes <- l_model_outcomes %>%
        map(function(x){
                x %>%
        mutate(Age = X+49) %>% select(-X) %>%
        pivot_longer(cols = Lung_Stage1_SoCnumpatients_scenario1:OtherStage4_MCEDQALYS_scenario10, names_to = "Variables", values_to = "values") %>%
        mutate(Variables = case_when(grepl(c("_MCEDnumpatients|_MCEDQALYS"), Variables) ~ str_replace(Variables, "Stage4", "_Stage4" ), .default = Variables )) %>%
        separate_wider_delim(Variables, delim = "_", names = c("Cancer type", "Stage", "Outcome", "Scenario"), too_few = "align_end") %>%
        unite("Stage_Outcome_Scenario" ,  Stage:Scenario, remove = TRUE) %>%
        pivot_wider(names_from = "Stage_Outcome_Scenario", values_from = "values" ) %>%
        mutate(stage_1_num_patients = Stage1_MCEDnumpatients_scenario1 + Stage1_MCEDnumpatients_scenario2 +Stage1_MCEDnumpatients_scenario3,
               stage_2_num_patients = Stage2_MCEDnumpatients_scenario1 + Stage2_MCEDnumpatients_scenario2 +Stage2_MCEDnumpatients_scenario3 +Stage2_MCEDnumpatients_scenario4 +
                       Stage2_MCEDnumpatients_scenario5 +Stage2_MCEDnumpatients_scenario6 ,
               stage_3_num_patients = Stage3_MCEDnumpatients_scenario1 + Stage3_MCEDnumpatients_scenario2 +Stage3_MCEDnumpatients_scenario3 +Stage3_MCEDnumpatients_scenario4 +
                       Stage3_MCEDnumpatients_scenario5 +Stage3_MCEDnumpatients_scenario6 +Stage3_MCEDnumpatients_scenario7  + Stage3_MCEDnumpatients_scenario8 ,
               stage_4_num_patients = Stage4_MCEDnumpatients_scenario1 + Stage4_MCEDnumpatients_scenario2 +Stage4_MCEDnumpatients_scenario3 +Stage4_MCEDnumpatients_scenario4 +
                       Stage4_MCEDnumpatients_scenario5 +Stage4_MCEDnumpatients_scenario6 +Stage4_MCEDnumpatients_scenario7  + Stage4_MCEDnumpatients_scenario8 +Stage4_MCEDnumpatients_scenario9 +Stage4_MCEDnumpatients_scenario10,
               undiag_stage_1_QALYS = (Stage1_MCEDQALYS_scenario1 + Stage1_MCEDQALYS_scenario2 +Stage1_MCEDQALYS_scenario3) / (stage_1_num_patients),
               undiag_stage_2_QALYS = (Stage2_MCEDQALYS_scenario1 + Stage2_MCEDQALYS_scenario2 +Stage2_MCEDQALYS_scenario3 + Stage2_MCEDQALYS_scenario4 +
                                               Stage2_MCEDQALYS_scenario5 +Stage2_MCEDQALYS_scenario6 ) / (stage_2_num_patients),
               undiag_stage_3_QALYS = (Stage3_MCEDQALYS_scenario1 + Stage3_MCEDQALYS_scenario2 +Stage3_MCEDQALYS_scenario3 + Stage3_MCEDQALYS_scenario4 +
                                               Stage3_MCEDQALYS_scenario5 +Stage3_MCEDQALYS_scenario6 + Stage3_MCEDQALYS_scenario7 + Stage3_MCEDQALYS_scenario8 ) / (stage_3_num_patients),
               undiag_stage_4_QALYS = (Stage4_MCEDQALYS_scenario1 + Stage4_MCEDQALYS_scenario2 +Stage4_MCEDQALYS_scenario3 + Stage4_MCEDQALYS_scenario4 +
                                               Stage4_MCEDQALYS_scenario5 +Stage4_MCEDQALYS_scenario6 + Stage4_MCEDQALYS_scenario7 + Stage4_MCEDQALYS_scenario8 + Stage4_MCEDQALYS_scenario9 + Stage4_MCEDQALYS_scenario10) / (stage_4_num_patients)) %>%
        select(Age, `Cancer type`, stage_1_num_patients, stage_2_num_patients, stage_3_num_patients, stage_4_num_patients,
               undiag_stage_1_QALYS, undiag_stage_2_QALYS,undiag_stage_3_QALYS,undiag_stage_4_QALYS)
                })


res_comparisonstage2 <- data.frame(Age = l_model_outcomes[[1]]$Age,
                              `Cancer type` = l_model_outcomes[[1]]$`Cancer type`,
                              `QALYs no HR` = l_model_outcomes[[1]]$undiag_stage_1_QALYS,
                              `QALYs 1.5 HR` = l_model_outcomes[[2]]$undiag_stage_1_QALYS,
                              `QALYs 3 HR`  = l_model_outcomes[[3]]$undiag_stage_1_QALYS)

#____ Calculate expected QALYs for gen pop by age
#' Load in LE and QALY data from QALE shiny app and function
df_genpop_LE_QALY <- read_csv(file = paste0(data_dir,"ref_df_appended.csv"), col_names = TRUE)
source(paste0("~/GalleriCEmodel/R/", "f_compQale.R"))

genpop_QALY <- data.frame(Age = 50:79,
                         gen_pop_QALYs = rep(0, 30))

for(cyc in 0:29){

        genpop_QALY[cyc+1, 2] <- compQale(
                ons_df = df_genpop_LE_QALY,
                prop_female = 0.53,
                start_age = 50 + cyc,
                disc_rate = 0.035,
                utils = "dsu_2014",
                cycle = 0)$Qx[1]

}

#____ Merge on to model QALYs
l_Abs_Prop_QALYs_full <- l_model_outcomes %>%
        map(function(x){
                x %>%
        left_join(genpop_QALY, by = c("Age")) %>%
        mutate(across(c(undiag_stage_1_QALYS:undiag_stage_4_QALYS), function(x) gen_pop_QALYs - x , .names = "{.col}_abssf")) %>%
        mutate(across(c(undiag_stage_1_QALYS_abssf:undiag_stage_4_QALYS_abssf), ~ case_when(. > 12 & . < 18 ~ 1.2,
                                                     . > 12 ~ 1.7, .default = 1), .names = "{.col}_weight")) %>%
        mutate(across(c(undiag_stage_1_QALYS_abssf:undiag_stage_4_QALYS_abssf), function(x) x/gen_pop_QALYs, .names = "{.col}_propsf")) %>%
        mutate(across(c(undiag_stage_1_QALYS_abssf_propsf:undiag_stage_4_QALYS_abssf_propsf), ~ case_when(. > 0.85 & . < 0.95 ~ 1.2,
                                                     . >= 0.95 ~ 1.7, .default = 1), .names = "{.col}_weight"))
        })

l_modifier <- l_Abs_Prop_QALYs_full %>%
        map(function(x){
                x %>%
        select(Age, `Cancer type`, contains("abssf_weight"), contains("_propsf_weight")) %>%
        rowwise() %>%
        mutate(`I` = max(undiag_stage_1_QALYS_abssf_weight, undiag_stage_1_QALYS_abssf_propsf_weight),
               `II` = max(undiag_stage_2_QALYS_abssf_weight, undiag_stage_2_QALYS_abssf_propsf_weight),
               `III` = max(undiag_stage_3_QALYS_abssf_weight, undiag_stage_3_QALYS_abssf_propsf_weight),
               `IV` = max(undiag_stage_4_QALYS_abssf_weight, undiag_stage_4_QALYS_abssf_propsf_weight)) %>%
        select(Age, `Cancer type`,  `I`, `II`, `III`, `IV`) %>%
        mutate(`Cancer type` = case_when(`Cancer type` == "Anus" ~ "Anal",
                                         `Cancer type` == "Cervix" ~ "Cervical",
                                         `Cancer type` == "Colon.Rectum" ~ "Colorectal",
                                         `Cancer type` == "Esophagus" ~ "Oesophageal",
                                         `Cancer type` == "Head.and.Neck" ~ "Head and neck",
                                         `Cancer type` == "Liver.Bile.duct" ~ "Liver/Bile-duct",
                                         `Cancer type` == "Ovary" ~ "Ovarian" ,
                                         `Cancer type` == "Pancreas" ~ "Pancreatic",
                                         `Cancer type` == "Urothelial.Tract" ~ "Urothelial tract",
                                         `Cancer type` == "Uterus" ~ "Uterine",
                                         .default = `Cancer type`)) %>%
        filter(`Cancer type` %in% c("Anal", "Bladder", "Breast", "Cervical", "Colorectal", "Head and neck", "Kidney", "Liver/Bile-duct",
                                    "Lung", "Lymphoma", "Oesophageal", "Ovarian", "Pancreatic", "Prostate", "Stomach", "Urothelial tract", "Uterine"))
        })

#' Create modifier matrix to be used in the CE model
df_modifier_noHR <- l_modifier[[1]] %>%
        pivot_longer(cols = `I`:`IV`, names_to = "Stage", values_to = "modifier") %>%
        pivot_wider(names_from = "Age", values_from = "modifier") %>%
        add_column(!!!set_names(as.list(rep(NA, 49)),nm=1:49), .before = "50") %>%
        add_column(!!!set_names(as.list(rep(NA, 21)),nm=80:100), .after = "79") %>%
        right_join(expand.grid(`Cancer type` = c("Lung","Colorectal",  "Pancreatic", "Liver/Bile-duct", "Breast",
                                                 "Oesophageal", "Head and neck", "Stomach", "Ovarian", "Kidney",
                                                 "Prostate", "Brst: HR-positive", "Lymphoma", "Anal", "Uterine",
                                                 "Bladder", "Cervical", "Urothelial tract", "Other"),
                               Stage = c("I", "II", "III", "IV")) %>%
                           arrange(`Cancer type`, Stage), by = c("Cancer type", "Stage")) %>%
        mutate(`Cancer type` = factor(`Cancer type`, levels = c("Lung","Colorectal",  "Pancreatic", "Liver/Bile-duct", "Breast",
                                                                "Oesophageal", "Head and neck", "Stomach", "Ovarian", "Kidney",
                                                                "Prostate", "Brst: HR-positive", "Lymphoma", "Anal", "Uterine",
                                                                "Bladder", "Cervical", "Urothelial tract", "Other")),
               Stage = factor(Stage, levels =  c("I", "II", "III", "IV"))) %>%
        arrange(`Cancer type`, Stage) %>%
        replace(is.na(.), 1) %>%
        select(-`Cancer type`, -Stage)

write.table(df_modifier_noHR, file = "~/GalleriCEmodel/data-raw/England_pilot_planning/df_modifier.csv", row.names=F, col.names=F, sep=",", na = "1")


fig_heatmap <- bind_rows(l_modifier, .id = "HR") %>%
        mutate(HR = case_when(HR == "model_outcomes_survival_1HR" ~ "No increased hazard of death",
                              HR == "model_outcomes_survival_1halfHR" ~ "cfDNA + to - hazard ratio: 1.5",
                              HR == "model_outcomes_survival_3HR" ~ "cfDNA + to - hazard ratio: 3"),
               HR = factor(HR, levels = c("No increased hazard of death", "cfDNA + to - hazard ratio: 1.5", "cfDNA + to - hazard ratio: 3" ))) %>%
        pivot_longer(cols = `I`:`IV`, names_to = "Stage", values_to = "Modifier") %>%
        mutate(Modifier = factor(Modifier, levels = c("1", "1.2", "1.7" ))) %>%
        ggplot(aes(y = fct_rev(`Cancer type`), x= Age, fill = Modifier)) +
        geom_tile(color="white", size=0.1) +
        facet_grid(Stage~HR) +
        theme_bw() +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52")) +
        labs(fill = "Modifier", x = "Cohort age", y = "Cancer type") +
        theme_bw() +
        theme(legend.position="bottom",
              axis.title = element_text(face = "bold", size = 17),
              strip.background = element_rect(fill="white"),
              strip.text.y = element_text(angle = 0, size = 11),
              strip.text = element_text( size = 11),
              axis.text.x = element_text(angle = 0, size = 11),
              axis.text.y = element_text(angle = 0, size = 11))


ggsave(paste0("~/GalleriCEmodel/analyses/NICE_modifier/", "fig_heatmap_fastdwell_onlyshifted.png"), fig_heatmap, device = "png",
       width = 300,
       height = 300,
       units = "mm")

