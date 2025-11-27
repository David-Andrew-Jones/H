#' _____________________________________________________________________________
#### 01_prep_England5year_net_surv.R

#' Libraries used
library(tidyverse)
library(readxl)
library(survextrap)
library(ggplot2)
library(dplyr)
library(zoo)

#' source functions from project directory
lapply(list.files(path = paste0(getwd(), "/functions/"), full.names = TRUE), source)

#' _____________________________________________________________________________
#' _____ 1. Data

#' Load survival data (currently the 5-year net survival taken from MM paper)
surv_full = as_tibble(read_excel("~/NCRAS_LE/data/grail_survival_formatted.xlsx"))

#' Load test sensitivity by stage to be used for the differential survival (currently taken from MM paper)
df_sensitivity <- read_tsv("~/NCRAS_LE/data/08022023_ccga3_iso_sens.tsv") %>%
        mutate(across(where(is.character), as.factor))

#' Load life table to be used for survival model and YLL estimation - will need to
#' decide on one for the updates OS data

df_UK_LifeTable1719 <- read_csv("~/NCRAS_LE/data/UK_life_table_1719.csv") %>%
        rename(hazard = mx) %>%
        mutate(sex = case_when(sex == "Males" ~ "Male",
                               sex == "Females" ~ "Female"))

#' _____________________________________________________________________________
#' OLD - no longer doing interpolation for 0.5 age increment
#' The overall survival data is given in 5-year age bands. For the survival modelling
#' and especially the YLL estimation, an indicative age needs to within the age band
#' to match the general #' population survival which is instead given yearly.
#' The most obvious is using the mid point of the age band, but as this will be in a .5 increment
#' this requires interpolating between the years. This is done below using the spline functions
#' code from the MM model
# df_UK_LifeTable1719_with_splinefunction <- df_UK_LifeTable1719 %>%
#         select(sex) %>%
#         distinct() %>%
#         left_join(tibble(age = seq(from=0,to=100,by=0.5)), by = character()) %>% #incidence data is only used up to 85
#         left_join(df_UK_LifeTable1719 %>%
#                           select(age, sex, hazard, ex) %>% group_by(sex) %>%
#                           summarise(
#                                   zeta_1 = list(splinefun(age, hazard, method="natural")),
#                                   zeta_2 = list(splinefun(age, ex, method="natural"))
#                           ) %>%
#                           ungroup()) %>%
#         mutate(interpolated_haz = sapply(1:length(age), function(z){zeta_1[z][[1]](age[z])})) %>%
#         mutate(interpolated_ex = sapply(1:length(age), function(z){zeta_2[z][[1]](age[z])})) %>%
#         select(sex, age, interpolated_haz, interpolated_ex)
#
#
# plot_hz_LE <- patchwork::wrap_plots(
#
#         ggplot(data = df_UK_LifeTable1719_with_splinefunction, aes(x = age, y = interpolated_haz)) +
#         geom_point(data = . %>% filter(age %in% c(1:100)) ,colour = "#3B1C52")+
#         geom_line(data = df_UK_LifeTable1719_with_splinefunction, colour = "#D29A22") +
#         facet_grid(cols = vars(sex),  scales = "fixed") +
#         xlim(c(40,100))+
#         xlab("Age")+
#         ylab("Hazard")+
#                 theme_bw(base_size = 14),
#
#         ggplot(data = df_UK_LifeTable1719_with_splinefunction, aes(x = age, y = interpolated_ex)) +
#                 geom_point(data = . %>% filter(age %in% c(1:100)) ,colour = "#3B1C52")+
#                 geom_line(data = df_UK_LifeTable1719_with_splinefunction, colour = "#D29A22") +
#                 facet_grid(cols = vars(sex),  scales = "fixed") +
#                 xlim(c(40,100))+
#                 xlab("Age")+
#                 ylab("Life expectancy") +
#                 theme_bw(base_size = 14),
#         ncol = 1
# )

#' #' Create the gen population hazard dataframe
#' df_gen_pop_hz <- df_UK_LifeTable1719_with_splinefunction %>%
#'         select(age, sex, interpolated_haz) %>% rename(hazard = interpolated_haz)
#'
#' #' Create the gen population lif expectancy dataframe
#' df_gen_pop_ex <- df_UK_LifeTable1719_with_splinefunction %>%
#'         select(age, sex, interpolated_ex) %>% rename(ex = interpolated_ex)
#' _____________________________________________________________________________

#' Create the gen population hazard dataframe
df_gen_pop_hz <- df_UK_LifeTable1719 %>%
        select(age, sex, hazard)

#' Create the gen population life expectancy dataframe
df_gen_pop_ex <- df_UK_LifeTable1719 %>%
        select(age, sex, ex)

#' Used for the below MM copied code
age_bands = as_tibble(read_excel("~/NCRAS_LE/data/age_bands.xlsx")) #age bands


#' _____________________________________________________________________________
#' _____ 2. Dataframe prep

#' THIS IS PROBABLY NOT NEEDED FOR THE NEW DATA!!!!
#' Tidy up 5-year net survival data from NCRAS with
#' most of the below  taken from the MM code "based on Earl's parallel interception model code".
#' Note also that the "persons" survival was used for most cancer types rather than by gender. In the OS data it should be by gender
#' Comments below are from the MM code copied over

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
        filter(Age != "50-74" & Age != "45-74" & Age != "45-79" & Age != "50-79") %>% #filter out the larger survival age bands
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
        mutate(across(c(Age, NCRAS_Draw), as.character),
               across(c(Age, NCRAS_Draw), as.factor) ) %>%
        filter(Age != "80-84 years")



#' _____________________________________________________________________________
#' _____ 3. Create split survival - CHANGE TO DESIRED HR!!!!!

#' Generate cfDNA+ and cfDNA- survival for a mortality hazard ratio of 2 using the 'two population' approach from the MM paper
#' Uses the f_derive_netsurv_cfDNAstatus (taken from MM)

#' Test sensitivities give detection rate (proportion cfDNAa- and cfDNA+)
#' Add on sensitivities then produce survival stratified by cfDNA status using MM formula
df_surv_byHR <- df_surv_formatted %>%
        left_join(df_sensitivity, by = c("NCRAS_Draw" = "Cancer", "Stage")) %>%
        select(Age:Stage, start, stop, CSS_adjusted, sens, n_patients) %>%
        # check no NAs filter(is.na(sens))
        cross_join(tibble(haz_ratio = c(1, 2))) %>%
        mutate(cfDNA_neg_CSS = mapply(f_derive_surv_cfDNAstatus, CSS_adjusted, haz_ratio, sens)) %>%
        mutate(cfDNA_pos_CSS = cfDNA_neg_CSS ^ haz_ratio) %>%
        pivot_longer(cols = starts_with("cfDNA"), names_to = "cfDNA_status", values_to = "CSS_HR_adjusted") %>%
        # get rid of poos/neg when theres no HR
        mutate(CSS_HR_adjusted = case_when(haz_ratio == 1 ~ CSS_adjusted,
                                           .default = CSS_HR_adjusted)) %>%
        filter(!(haz_ratio == 1 & cfDNA_status == "cfDNA_pos_CSS")) %>%
        mutate(cfDNA_status = case_when(cfDNA_status == "cfDNA_neg_CSS" ~ "negative",
                                        cfDNA_status == "cfDNA_pos_CSS" ~ "positive")) %>%
        mutate(across(haz_ratio:cfDNA_status, \(x) as.factor(x))) %>%
        # Need to create Male and Female rows where the sex is person
        mutate(sex_person = case_when( Sex == "Persons" ~ 2, .default = 1)) %>%
        uncount(sex_person, .remove = FALSE, .id = "count") %>%
        mutate(Sex = as.factor(case_when(Sex == "Persons" & count == 1 ~ "Female",
                               Sex == "Persons" & count == 2 ~ "Male",
                                .default = Sex)))

#' Plot differential survival for checking
figure_path <- paste0(getwd(), "/results/figures/")

for(age in c("50-54 years","55-59 years","60-64 years","65-69 years","70-74 years","75-79 years")) {

        for(stage in c("I", "II", "III", "IV")){

                temp1 <- df_surv_byHR %>%
                        filter(Age == age) %>%
                        filter(Stage == stage) %>%
                        mutate(cfDNA_status = case_when(haz_ratio == 1 ~ "All",
                                                        .default = cfDNA_status)) %>%
                        ggplot() +
                        geom_line(aes(x = start, y = CSS_HR_adjusted, group = cfDNA_status, colour = cfDNA_status) )+
                        facet_wrap(vars(NCRAS_Draw),  scales = "fixed", nrow = 5) +
                        xlab("Time")+
                        ylab("Survival")+
                        theme_bw(base_size = 12) +
                        theme(legend.position = "bottom")

                ggsave(paste0(figure_path, "SpitSurvivalplot_", stage, "_" ,age, ".png"),
                       temp1, device = "png",
                       width = 200,
                       height = 300,
                       units = "mm")
        }
}




#' _____________________________________________________________________________
#' _____ 4. Final dataframe clean up (again may not be needed with final data)

df_surv_byHR <- df_surv_byHR %>% mutate(Age = gsub(" years", "", Age)) %>%
        separate(Age, sep="-", into = c("Age_lower", "Age_upper"), remove = FALSE) %>%
        mutate(across(c("Age_lower", "Age_upper"), as.numeric)) %>%
        select(-c(sex_person, count, sens)) %>%
        group_by(Age_lower, Sex, NCRAS_Draw, Stage, haz_ratio, cfDNA_status) %>%
        arrange(start, .by_group = TRUE) %>%
        ungroup() %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor)) %>%
        mutate(n_patients = case_when(is.na(n_patients) ~ 0,
                                      .default = n_patients))

#' _____________________________________________________________________________



