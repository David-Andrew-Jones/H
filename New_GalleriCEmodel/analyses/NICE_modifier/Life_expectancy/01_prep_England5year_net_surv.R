#______________________________________________________________________________#
#### 01_prep_England5year_net_surv.R

library(tidyverse)
library(readxl)
library(survextrap)
library(ggplot2)
library(dplyr)
library(viridis)
library(zoo)
library(SHELF)

#______________________________________________________________________________#
#____ 1 Data
#' Load in general pop life table to give background hazard and load in cure fraction

df_gen_pop_hz <- read_csv("~/GalleriCEmodel/data-raw/NICE_modifier/UK_life_table_1719.csv") %>%
        select(age, sex, mx) %>%
        rename(hazard = mx) %>%
        mutate(sex = case_when(sex == "Males" ~ "Male",
                               sex == "Females" ~ "Female"))

df_cure_fraction_byageandstage <- read_excel("~/GalleriCEmodel/data-raw/NICE_modifier/cure_fractions_byageandstage.xlsx",
                                             col_names = TRUE)

surv_full = as_tibble(read_excel("~/GalleriCEmodel/data-raw/NICE_modifier/grail_survival_formatted.xlsx")) #NCRAS
age_bands = as_tibble(read_excel("~/GalleriCEmodel/data-raw/NICE_modifier/age_bands.xlsx")) #age bands

df_sensitivity <- read_tsv("~/GalleriCEmodel/data-raw/NICE_modifier/08022023_ccga3_iso_sens.tsv") %>%
        mutate(across(where(is.character), as.factor))


#______________________________________________________________________________#
#____ 2
#' Prepare 5-year net survival provided by NCRAS.
#' The data provided give for each age, cancer, and stage the number of patients
#' observable and net survival for quarter year time intervals.

#' First, perform some data cleaning on original NCRAS data- code take from Sasieni et al
#' This is based on Earl's parallel interception model code

#based on SEER spelling of cancer names
FemCan = c("Uterus", "Ovary", "Cervix") # female cancers - breast taken out
MalCan = c("Prostate") # male cancers
PerCan = c("Urothelial Tract","Breast", "Thyroid", "Stomach", "Sarcoma", "Plasma Cell Neoplasm", "Pancreas", "[OTHER]",
           "Esophagus", "Myeloid Neoplasm",   "Melanoma",  "Lymphoma" ,  "Lymphoid Leukemia",
           "Lung", "Liver/Bile-duct" , "Kidney", "Head and Neck", "Gallbladder", "Colon/Rectum", "Bladder", "Anus")# persons cancers - male & female taken out

#to change NCRAS surv data to match SEER format
surv <- surv_full %>%
        rename(Cancer = `Cancer site`, Stage = `Stage at diagnosis`, Age = `Age group`,
                                  Years = `Years since diagnosis`, n_patients = `Number of patients`,
                                  CSS =`Net survival (%)`) %>%
        select(Cancer, Stage, Years, CSS, Age, Sex, Subtype, n_patients) %>% #sex + subtype included to be filtered later
        filter(Age != "50-74" & Age != "45-74" & Age != "45-79" & Age != "50-79") %>%#filter out the larger survival age bands as not needed & it is causing issues when we need to bring forward missing observations
        mutate(CSS = CSS/100) %>% #changes survival from % to prob as in SEER dataset
       #mutate(Years = Years*12) %>% #converts to months
        filter(Stage != "All stages combined") %>% #filter out all stages as not in SEER dataset
        mutate(Stage = case_when(Stage == 1 ~ "I",
                                 Stage == 2 ~ "II",
                                 Stage == 3 ~ "III",
                                 Stage == 4 ~ "IV",
                                 Stage == "Unstageable/unknown/missing" ~ "Unknown/missing",
                                 .default = NA)) %>%
        mutate(Age = paste0(Age, ' years') ) %>%
        mutate(NCRAS_Draw=case_when(
                Cancer %in% c("uterus") ~ "Uterus",
                Cancer %in% c("ovary") ~ "Ovary",
                Cancer %in% c("cervix") ~ "Cervix",
                Cancer %in% c("prostate") ~ "Prostate",
                Cancer %in% c("breast") ~ "Breast",
                Cancer %in% c("thyroid") ~ "Thyroid",
                Cancer %in% c("stomach") ~ "Stomach",
                Cancer %in% c("sarcoma") ~ "Sarcoma",
                Cancer %in% c("pancreas") ~ "Pancreas",
                Cancer %in% c("melanoma") ~ "Melanoma",
                Cancer %in% c("lymphoma") ~ "Lymphoma",
                Cancer %in% c("lung") ~ "Lung",
                Cancer %in% c("kidney") ~ "Kidney",
                Cancer %in% c("gallbladder") ~ "Gallbladder",
                Cancer %in% c("bladder") ~ "Bladder",
                Cancer %in% c("anus") ~ "Anus",
                Cancer %in% c("head_and_neck") ~ 'Head and Neck',
                Cancer %in% c("colon-rectum") ~ "Colon/Rectum",
                Cancer %in% c("lymphoid leukaemia") ~ "Lymphoid Leukemia",
                Cancer %in% c("lymphoid_leukaemia") ~ "Lymphoid Leukemia",
                Cancer %in% c("liver-bile_duct")  ~ "Liver/Bile-duct",
                Cancer %in% c("liver/bile duct")  ~ "Liver/Bile-duct",
                Cancer %in% c("myeloid_neoplasm") ~ "Myeloid Neoplasm",
                Cancer %in% c("oesophagus") ~ "Esophagus",
                Cancer %in% c("plasma_cell_neoplasm") ~ "Plasma Cell Neoplasm",
                Cancer %in% c("urothelial_tract") ~ "Urothelial Tract",
                Cancer %in% c("other") ~ "[OTHER]",
                TRUE~Cancer
        )) %>%
        filter(
                (NCRAS_Draw %in% FemCan & Sex=="Female") |
                        (NCRAS_Draw %in% MalCan & Sex=="Male") |
                        (NCRAS_Draw %in% PerCan & Sex=="Persons")
        ) %>%
        mutate(CSS = case_when(CSS>1 ~ 1,
                               .default = CSS)) %>% #filter out all the different subtypes - use 'all combined' (for cancers with 1+ subtype) OR "N/A"
        filter(Subtype == "N/A" | Subtype == "All combined" ) %>% #if cancer has sub-types, it will be 'All combined', otherwise N/A
        select(-Cancer) %>%
        mutate(`Age group` = gsub('.{6}$','', Age)) %>% #to take off the " years" at the end
        left_join(age_bands, by = c("Age group")) %>%
        select(-`Age group`) %>%
        group_by(NCRAS_Draw, Stage, Years) %>% #select stage, cancer, Months, to ensure selecting within a type
        arrange(desc(age_group_5yr)) %>% #arrange the data oldest to youngest
        mutate(CSS_imp = na.locf(CSS, na.rm = FALSE)) %>% #search for the most recent non-NA prior (order means it is getting older values)
        mutate(CSS = na.locf(CSS_imp, na.rm = FALSE, fromLast = TRUE)) %>% #next observation carried backwards - for when there is no older age group
        ungroup() %>%
        filter(age_group_5yr != "30-34" & age_group_5yr != "85-89" & age_group_5yr != "90-94" & age_group_5yr != "95-100") %>%#get rid 30-34, otherwise it causes 30-39 to be duplicated in next stage
        select(Age, Sex, NCRAS_Draw, Stage, Years, CSS, n_patients) %>% #select variables to be written out
        mutate(across(where(is.character), as.factor))

library(purrr)
fill_in <- function(prev, new) {
        if_else(prev < new, prev, new)
}

df_surv_formatted <- surv %>%
        mutate(CSS = round(CSS, 3)) %>%
        group_by(Age, Sex, NCRAS_Draw, Stage) %>%
        mutate(CSS_adjusted = accumulate(CSS, fill_in)) %>%
        mutate(start = lag(Years, default = 0)) %>%
        rename(stop = Years) %>%
        ungroup() %>%
        filter(Age != "30-39 years" & Age != "40-44 years" & Age != "45-49 years") %>%
        filter(NCRAS_Draw %in% levels(as.factor(df_cure_fraction_byageandstage$`Cancer Type`))) %>%
        mutate(across(c(Age, NCRAS_Draw), as.character),
               across(c(Age, NCRAS_Draw), as.factor) ) %>%
        filter(Stage != "Unknown/missing") %>%
        filter(Age != "80-84 years")

df_surv_formatted_lung <- df_surv_formatted %>%
        filter(NCRAS_Draw == "Lung") %>%
        filter(stop == 5)

#______________________________________________________________________________#
#____ 3
#' Generate CSS results for hazard ratios of 2 and 3
#' N.b. need to make sure the CE model estimates both cfDNA + cancer intercepted and those not (see MM paper)

#  test sensitivities which gives detection rate (proportion cfDNAa- and cfDNA+)
# Add on sensitivities then produce net-survival stratified by cfDNA status using MM formula for HRs 1, 2 and 3
df_surv_byHR <- df_surv_formatted %>%
        left_join(df_sensitivity, by = c("NCRAS_Draw" = "Cancer", "Stage")) %>%
        select(Age:Stage, start, stop, CSS_adjusted, sens, n_patients) %>%
        # check no NAs filter(is.na(sens))
        cross_join(tibble(haz_ratio = c(1, 2, 3))) %>%
        mutate(cfDNA_neg_CSS = mapply(f_derive_netsurv_cfDNAstatus, CSS_adjusted, haz_ratio, sens)) %>%
        mutate(cfDNA_pos_CSS = cfDNA_neg_CSS ^ haz_ratio) %>%
        pivot_longer(cols = starts_with("cfDNA"), names_to = "cfDNA_status", values_to = "CSS_HR_adjusted") %>%
        mutate(cfDNA_status = case_when(cfDNA_status == "cfDNA_neg_CSS" ~ "negative",
                                        cfDNA_status == "cfDNA_pos_CSS" ~ "positive")) %>%
        mutate(across(haz_ratio:cfDNA_status, \(x) as.factor(x))) %>%
        # Need to create Male and Female rows where the sex is person
        mutate(sex_person = case_when( Sex == "Persons" ~ 2, .default = 1)) %>%
        uncount(sex_person, .remove = FALSE, .id = "count") %>%
        mutate(Sex = as.factor(case_when(Sex == "Persons" & count == 1 ~ "Female",
                               Sex == "Persons" & count == 2 ~ "Male",
                                .default = Sex)))

#______________________________________________________________________________#
#____
#' Prepare cure fraction data

df_cure_fraction_byageandstage_tidy <-  df_cure_fraction_byageandstage %>%
        select(`Cancer Type`:`CF_Stage IV`) %>%
        pivot_longer(
                cols = `CF_Stage I`:`CF_Stage IV`,
                names_to = "staging",
                values_to = "cf_and95"
        ) %>%
        mutate(`Age Group` = gsub(" years", "", `Age Group`)) %>%
        separate(`Age Group`, sep="-", into = c("Age_lower", "Age_upper")) %>%
        separate(staging, sep=" ", into = c("prefix", "AJCC_stage")) %>%
        separate(cf_and95, sep=" ", into = c("cure_fraction", "cure_fraction_95")) %>%
        mutate(cure_fraction_95 = str_sub(cure_fraction_95, 2, -2)) %>%
        separate(cure_fraction_95, sep="-", into = c("cf_lower95", "cf_upper95")) %>%
        select(-prefix) %>%
        rename(SEER_Draw = `Cancer Type`) %>%
        mutate(across(c("Age_lower", "Age_upper", "cure_fraction", "cf_lower95", "cf_upper95"), as.numeric)) %>%
        # Conditions for SHELF fitdist to run
        mutate(across(cure_fraction:cf_upper95, ~ . / 100)) %>%
        mutate(across(cure_fraction:cf_upper95, ~ case_when(. <= 0 ~ 0.001,
                                                            . >= 1 ~ 0.999,
                                                            .default = .))) %>%
        mutate(cf_lower95 = case_when( cf_lower95 == cure_fraction ~ cf_lower95 - 0.005,
                                       .default = cf_lower95)) %>%
        mutate(cf_upper95 = case_when( cf_upper95 == cure_fraction ~ cf_upper95 + 0.005,
                                       .default = cf_upper95)) %>%
        select(-Age_upper)

fun_gen_beta <- function(lower, mean, upper) {

        out <- SHELF::fitdist(vals=c(lower, mean, upper), probs=c(0.025, 0.5, 0.975), lower=0, upper=1)$Beta
        return(out)

}

df_cf_beta_params <- pmap_dfr(df_cure_fraction_byageandstage_tidy %>% select(cf_lower95, cure_fraction, cf_upper95) %>%
                                      rename(lower = cf_lower95,
                                             mean = cure_fraction,
                                             upper = cf_upper95) ,
                              fun_gen_beta)

df_cf_byageandstage_beta_params <- cbind(df_cure_fraction_byageandstage_tidy, df_cf_beta_params)

#______________________________________________________________________________#
# CF on to survival

res_cancers_evaluated <- intersect(levels(as.factor(df_surv_byHR$NCRAS_Draw)) , levels(as.factor(df_cure_fraction_byageandstage$`Cancer Type`)))
res_cancers_excluded <- setdiff(levels(as.factor(df_surv_byHR$NCRAS_Draw)) , levels(as.factor(df_cure_fraction_byageandstage$`Cancer Type`)))

df_surv_byHR_CF <- df_surv_byHR %>%
        mutate(Age = gsub(" years", "", Age)) %>%
        separate(Age, sep="-", into = c("Age_lower", "Age_upper"), remove = FALSE) %>%
        mutate(across(c("Age_lower", "Age_upper"), as.numeric)) %>%
        mutate(cf_df_ages = case_when(Age_lower == 50 ~ 40,
                                      Age_lower == 60 ~ 55,
                                      Age_lower == 70 ~ 65,
                                      .default = Age_lower)) %>%
        left_join(df_cf_byageandstage_beta_params, by = c( "NCRAS_Draw" = "SEER_Draw" , "Stage" = "AJCC_stage" , "cf_df_ages" = "Age_lower") ) %>%
        select(-c(sex_person, count, cf_df_ages, cf_upper95)) %>%
        group_by(Age, Sex, NCRAS_Draw, Stage, haz_ratio, cfDNA_status) %>%
        arrange(start, .by_group = TRUE) %>%
        ungroup() %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor)) %>%
        mutate(n_patients = case_when(is.na(n_patients) ~ 0,
                                      .default = n_patients))

#______________________________________________________________________________#
