#______________________________________________________________________________#
#### 00_cal_modifier_matrix.R

#' Code to assign the QALY modifier for each cycle of diagnosis and cancer/stage
#' combination, based on absolute/proportional QALY shortfall

library(tidyverse)
library(data.table)
#______________________________________________________________________________#

#____ Calculate expected QALYs with disease by age, cancer and stage

#' Load survival data
v_data_path = "~/GalleriCEmodel/data-raw/England_pilot_planning/"
df_surv_noHR <- read_csv(file = paste0(v_data_path,"survival_1HR.csv"), col_names = FALSE)

#' Transform into shape of markov trace matrix
cancer_names <- c("Lung","Colon/Rectum",  "Pancreas", "Liver/Bile-duct", "Breast", "Esophagus", "Head and Neck", "Stomach",
                  "Ovary", "Kidney", "Prostate", "Brst: HR-positive", "Lymphoma", "Anus", "Uterus",  "Bladder", "Cervix", "Urothelial Tract", "Other")

age_bands <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
               "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-100")

Stage <- c("I", "II", "III", "IV")

n_cancer <- length(cancer_names)
cancer_names <- rep(cancer_names,each=4)
number_stage <- rep(Stage,n_cancer)
n_cycles <- 51
disc_health = 0.035

df_surv_noHR_util <- df_surv_noHR %>%
        rename_all(~age_bands) %>%
        add_column(cancer_names = cancer_names, .before = 1) %>%
        add_column(number_stage = number_stage, .before = 2) %>%
        unite(cancer_stage, cancer_names, number_stage) %>%
        data.table::transpose(., make.names = "cancer_stage") %>%
        mutate(age_band = age_bands) %>%
        left_join(data.table::transpose(read_csv(file = paste0(v_data_path,"gen_pop_utility.csv"), col_names = FALSE)) %>%
                          mutate(age_band = age_bands),
                  by = c("age_band")) %>%
        rename(util_pop = V1) %>%
        filter(age_band != c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                             "40-44", "45-49") ) %>%
        mutate(reps = case_when(age_band == "95-100" ~ 6,
                                .default = 5)) %>%
        uncount(reps) %>%
        add_column(cycle = seq_along(1:51), .before = 1)

#' Read in utility decrements
df_cancer_util <- data.table::transpose(read_csv(file = paste0(v_data_path,"util_cancer.csv"), col_names = FALSE) %>%
                                                add_column(cancer_names = cancer_names, number_stage = number_stage, .before = 1) %>%
                                                unite(cancer_stage, cancer_names, number_stage),
                                        make.names = "cancer_stage")

df_qaly_discounted <- df_surv_noHR_util


for(i in 1:nrow(df_qaly_discounted)){     #' Rep over the cycles

        for(c in 2:(ncol(df_qaly_discounted)-2)) {   #' Rep over the cancer-stage pairs

                #' First create vector of length the number of years survived
                #' rounded to the highest number. Sums to the mean survival
                v_years <- c(rep(1, times = floor(df_qaly_discounted[i,c])), df_qaly_discounted[i,c] - floor(df_qaly_discounted[i,c]))
                v_utility <- df_qaly_discounted$util_pop[i:(i - 1 + length(v_years))] * tidyr::replace_na(df_cancer_util[1:length(v_years), c - 1], 1 )
                v_qaly_discounted <- v_years * v_utility* 1/(1+disc_health)**(1:( length(v_years)))

                        #(1+disc_health)**(1:(i - 1 + length(v_years)))

                df_qaly_discounted[i,c] <- sum(v_qaly_discounted)
        }
}


#' Note NAs introduced in the final cycles of quite a few of the cancer-stages.
#' This is because the life expectancy exceeds the cycle number. I.e. someone
#' aged 99 with stage 1 cancer has over 3 years of life expectancy left. Realistically,
#' suvival with disease should be capped by survival by the gen pop expectation. But doesn't matter
#' as these are not the cells in which a QALY weight would apply to

#______________________________________________________________________________#

#____ Calculate expected QALYs for gen pop by age
#' Load in LE and QALY data from QALE shiny app and function
df_genpop_LE_QALY <- read_csv(file = paste0(v_data_path,"ref_df_appended.csv"), col_names = TRUE)
source(paste0("~/GalleriCEmodel/R/", "f_compQale.R"))

genpop_QALY <- rep(0, n_cycles)

for(cyc in 0:(n_cycles-1)){

        genpop_QALY[cyc+1] <- compQale(
                ons_df = df_genpop_LE_QALY,
                prop_female = 0.53,
                start_age = 50 + cyc,
                disc_rate = 0.035,
                utils = "dsu_2014",
                cycle = 0)$Qx[1]

}

#____ Match with expected QALYs for general population by age. Calculate absolute
#____ absolute and proportionate QALY shortfall and utility weight implied

df_qaly_discounted <-  cbind(genpop_QALY, df_qaly_discounted)

df_absoluteprop_sf <- df_qaly_discounted %>%
        mutate(across(c(Lung_I:Other_IV), function(x) genpop_QALY - x )) %>%
        mutate(across(c(Lung_I:Other_IV), ~ case_when(. > 12 & . < 18 ~ 1.2,
                                                     . > 12 ~ 1.7,
                                                     .default = 1), .names = "{.col}_abssf_weight")) %>%
        mutate(across(c(Lung_I:Other_IV), function(x) x/genpop_QALY)) %>%
        mutate(across(c(Lung_I:Other_IV), ~ case_when(. > 0.85 & . < 0.95 ~ 1.2,
                                                     . >= 0.95 ~ 1.7,
                                                     .default = 1), .names = "{.col}_propsf_weight"))

v_cancerstage_names <- colnames(df_absoluteprop_sf %>% select(Lung_I:Other_IV))

df_aggregate_absoluteprop_sf <- v_cancerstage_names %>%
        map(.f = ~ df_absoluteprop_sf %>% select(paste0(.x,"_abssf_weight"), paste0(.x,"_propsf_weight")) %>%
                       rowwise() %>%
                       mutate(!!.x := max(get(paste0(.x,"_abssf_weight")), get(paste0(.x,"_propsf_weight")))) %>%
                       select(.x)) %>%
        list_cbind() %>%
        add_column( Age = 50:100, .before = 1) %>%
        add_column( Filler = 50:100, .before = 1) %>%
        mutate(across(Lung_I:Other_IV, ~ case_when(Age > 79 ~ 1,.default = .) ))


write.table(df_aggregate_absoluteprop_sf, file = "~/GalleriCEmodel/data-raw/England_pilot_planning/df_aggregate_absoluteprop_sf.csv", row.names=F, col.names=F, sep=",", na = "0")


fig_heatmap <-  df_aggregate_absoluteprop_sf %>%
        pivot_longer(Lung_I:Other_IV, names_to = "cancer_stage", values_to = "modifier" ) %>%
        separate_wider_delim(cancer_stage, "_", names = c("Cancer type", "Stage")) %>%
        pivot_wider(names_from = "Stage", values_from = "modifier") %>%
        mutate(`Cancer type` = case_when(`Cancer type` == "Anus" ~ "Anal",
                                         `Cancer type` == "Cervix" ~ "Cervical",
                                         `Cancer type` == "Colon/Rectum" ~ "Colorectal",
                                         `Cancer type` == "Esophagus" ~ "Oesophageal",
                                         `Cancer type` == "Head and Neck" ~ "Head and neck",
                                         `Cancer type` == "Ovary" ~ "Ovarian" ,
                                         `Cancer type` == "Pancreas" ~ "Pancreatic",
                                         `Cancer type` == "Urothelial.Tract" ~ "Urothelial tract",
                                         `Cancer type` == "Uterus" ~ "Uterine",
                                         .default = `Cancer type`)) %>%
        filter(`Cancer type` %in% c("Anal", "Bladder", "Breast", "Cervical", "Colorectal", "Head and neck", "Kidney", "Liver/Bile-duct",
                                    "Lung", "Lymphoma", "Oesophageal", "Ovarian", "Pancreatic", "Prostate", "Stomach", "Urothelial tract", "Uterine")) %>%
        filter(Age < 80) %>%
        pivot_longer(cols = `I`:`IV`, names_to = "Stage", values_to = "Modifier") %>%
        mutate(Modifier = factor(Modifier, levels = c("1", "1.2", "1.7" ))) %>%
        ggplot(aes(y = fct_rev(`Cancer type`), x= Age, fill = Modifier)) +
        geom_tile(color="white", size=0.1) +
        facet_grid(Stage~.) +
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


ggsave(paste0("~/GalleriCEmodel/analyses/NICE_modifier/", "fig_heatmap_modifier_at_clinical_diag.png"), fig_heatmap, device = "png",
       width = 300,
       height = 300,
       units = "mm")


