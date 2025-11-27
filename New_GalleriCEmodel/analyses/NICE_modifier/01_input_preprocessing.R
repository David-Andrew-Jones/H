#' _____________________________________________________________________________

#' Generate survival in correct format for model

#' First bit taken from:
#' _____ 03_LE_and_YLL

#' _____________________________________________________________________________

results_path <- "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/Results/mcmc_5ysurv_splithaz/"

#' Create template dataframe with all potential combinations
df_all_combos <- expand.grid(Gender = c("Male","Female"),
                             `Cancer type` = c("Anus", "Bladder", "Breast", "Cervix", "Colon/Rectum", "Esophagus", "Gallbladder",
                                               "Head and Neck", "Kidney", "Liver/Bile-duct", "Lung", "Lymphoma", "Melanoma", "Ovary",
                                               "Pancreas", "Prostate", "Sarcoma", "Stomach", "Thyroid", "Urothelial Tract", "Uterus"),
                             Stage = c("I", "II", "III", "IV"),
                             Age = c("50-54","55-59","60-64","65-69","70-74","75-79","80-84", "85-89", "90-94", "95-100"),
                             HR = c("1", "1andhalf", "3"),
                             `cfDNA status` = c("negative", "positive")) %>%
        unite(characteristics, Gender:`cfDNA status`, sep = ".", remove = TRUE) %>%
        left_join(list.files(path = results_path, pattern = "^LE") %>%
                          map_df(~read_csv(paste0(results_path, .))) %>%
                          mutate(characteristics = str_replace(characteristics, pattern = "1.5", "1andhalf")) %>%
                          select(characteristics) %>%
                          mutate(modelled = "Yes"),
                  by = c("characteristics")) %>%
        filter(!grepl(c("Male.Cervix|Male.Breast|Female.Prostate|Male.Ovary|Male.Uterus"), characteristics)) %>%
        #filter(!grepl(c("1andhalf|3"), characteristics) & grepl(c("negative"), characteristics)) %>%
        separate_wider_delim(cols = characteristics, delim = ".", names = c("Gender", "Cancer type", "Stage", "Age", "HR", "cfDNA status"))

# Read in results
res_LE_tables_all_full <- list.files(path = results_path, pattern = "^LE") %>%
        map_df(~read_csv(paste0(results_path, .))) %>%
        mutate(characteristics = str_replace(characteristics, pattern = "1.5", "1andhalf")) %>% # Change as below will not work otherwise %>%
        filter(!grepl(c("Male.Cervix|Male.Breast|Female.Prostate|Male.Ovary|Male.Uterus"), characteristics))

# Join on the all possible combos dataframe in readiness for interpolation
res_LE_tables <- df_all_combos %>%
        left_join(res_LE_tables_all_full %>%
                          separate_wider_delim(cols = characteristics, delim = ".", names = c("Gender", "Cancer type", "Stage", "Age", "HR", "cfDNA status")) %>%
                          select(-t, -variable ,-CrI_temp, -CrI, -median_CrI),
                  by = c("Gender", "Cancer type", "Stage", "Age", "HR", "cfDNA status")
        ) %>%
        ## remove the combinations where all are NA
        group_by(Gender, `Cancer type`, Stage, HR, `cfDNA status`) %>%
        filter(any(!is.na(median))) %>%
        ungroup() %>%
        arrange(Gender, `Cancer type`, Stage, HR, `cfDNA status`) %>%
        # Condition that age 75-79 must have equal or lower LE (or else extrapolation gets weird for higher ages)
        mutate(across(median:`97.5%`, ~case_when(Age == "75-79" & lead(.) < . ~ lead(.), .default = .)))


# Perform interpolation
df_LE_tables_with_splinefunction <- res_LE_tables %>%
        separate(Age, into = c("Low", "Hi")) %>%
        group_by(Gender, `Cancer type`, `Stage`, HR, `cfDNA status`) %>%
        summarise(
                zeta_1 = list(splinefun(Low, median, method = "natural")),
                zeta_2 = list(splinefun(Low, `2.5%`, method = "natural")),
                zeta_3 = list(splinefun(Low, `97.5%`, method = "natural")),
        ) %>% ungroup()


df_interpolated_inc_by_age = res_LE_tables %>%
        select(Gender, `Cancer type`, `Stage`, HR, `cfDNA status`) %>%
        distinct() %>%
        left_join(tibble(Age = 0:100), by = character()) %>%
        left_join(df_LE_tables_with_splinefunction) %>%
        mutate(interpolated_median = sapply(1:length(Age), function(z){zeta_1[z][[1]](Age[z])})) %>%
        mutate(`interpolated_2.5%` = sapply(1:length(Age), function(z){zeta_2[z][[1]](Age[z])})) %>%
        mutate(`interpolated_97.5%` = sapply(1:length(Age), function(z){zeta_3[z][[1]](Age[z])})) %>%
        select(Gender, `Cancer type`, Stage, HR, `cfDNA status`,Age, interpolated_median, `interpolated_2.5%`, `interpolated_97.5%`)

# Join back on to the results table
res_LE_tables_interpolated <- res_LE_tables %>%
        separate(Age, into = c("Low", "Hi"), remove = FALSE) %>%
        mutate(Low = as.numeric(Low)) %>%
        left_join(df_interpolated_inc_by_age, by = c("Gender", "Cancer type", "Stage", "Low" = "Age", "HR", "cfDNA status"))%>%
        mutate(median = case_when(is.na(median) & !(`Cancer type` %in% c("Thyroid", "Prostate")) ~ round(interpolated_median, 2), .default = median),
               `2.5%` = case_when(is.na(`2.5%`) & !(`Cancer type` %in% c("Thyroid", "Prostate")) ~ round(`interpolated_2.5%`, 2), .default = `2.5%`),
               `97.5%` = case_when(is.na(`97.5%`) & !(`Cancer type` %in% c("Thyroid", "Prostate")) ~ round(`interpolated_97.5%`,2), .default = `97.5%`)) %>%
        select(-interpolated_median, -`interpolated_2.5%`, -`interpolated_97.5%`, -Hi)


# If still NA, set survival to general pop.
df_UK_LifeTable1719 <- read_csv("~/GalleriCEmodel/data-raw/NICE_modifier/UK_life_table_1719.csv") %>%
        select(age, sex, ex)

res_LE_tables_interpolated <- res_LE_tables_interpolated %>%
        left_join(df_UK_LifeTable1719 %>% mutate(sex = str_sub(sex, end=-2)), by = c("Low" = "age" , "Gender" = "sex")) %>%
        mutate(across(median:`97.5%`, ~ case_when(is.na(.) ~ ex, .default = .) ))

# Shape it into matrix
l_survs <- map(c("1", "1andhalf", "3"), function(x){


      as.matrix(res_LE_tables_interpolated %>%
        filter(HR == x, `cfDNA status` == "positive") %>%
        select(-`2.5%`,-`97.5%`,-modelled, -HR, -`cfDNA status`, -Low, -ex ) %>%
        pivot_wider(names_from = Gender, values_from = median) %>%
        mutate(survival_combinedsex = (Female * 0.53) + (Male * 0.47),
               survival_combinedsex = case_when(is.na(survival_combinedsex) & is.na(Female) ~ Male,
                                                is.na(survival_combinedsex) & is.na(Male) ~ Female, .default = survival_combinedsex )) %>%
        select(-Female, -Male) %>%
        right_join(expand.grid(`Cancer type` = c("Lung","Colon/Rectum",  "Pancreas", "Liver/Bile-duct", "Breast", "Esophagus", "Head and Neck", "Stomach",
                                 "Ovary", "Kidney", "Prostate", "Brst: HR-positive", "Lymphoma", "Anus", "Uterus",  "Bladder", "Cervix", "Urothelial Tract", "Other"),
                               Stage = c("I", "II", "III", "IV"),
                               Age = c("0-4", "5-9", "10-14", "15-19",
                                       "20-24", "25-29", "30-34", "35-39",
                                       "40-44", "45-49", "50-54", "55-59",
                                       "60-64", "65-69", "70-74", "75-79",
                                       "80-84", "85-89", "90-94", "95-100")), by = c("Cancer type", "Stage", "Age")) %>%
        mutate(`Cancer type` = factor(`Cancer type`, levels = c("Lung","Colon/Rectum",  "Pancreas", "Liver/Bile-duct", "Breast", "Esophagus", "Head and Neck", "Stomach",
                                 "Ovary", "Kidney", "Prostate", "Brst: HR-positive", "Lymphoma", "Anus", "Uterus",  "Bladder", "Cervix", "Urothelial Tract", "Other")),
               Stage = factor(Stage, levels = c("I", "II", "III", "IV")),
               Age = factor(Age, levels = c("0-4", "5-9", "10-14", "15-19",
                       "20-24", "25-29", "30-34", "35-39",
                       "40-44", "45-49", "50-54", "55-59",
                       "60-64", "65-69", "70-74", "75-79",
                       "80-84", "85-89", "90-94", "95-100"))) %>%
        arrange(`Cancer type`, Stage, Age) %>%
        mutate(survival_combinedsex = case_when(survival_combinedsex < 0 ~ 0.001, .default = survival_combinedsex)) %>%
        pivot_wider(names_from = Age, values_from = survival_combinedsex) %>%
        select(-`Cancer type`, -Stage) )

}
)

write.table(l_survs[[1]], file = "~/GalleriCEmodel/data-raw/England_pilot_planning/survival_1HR.csv", row.names=F, col.names=F, sep=",", na = "0")
write.table(l_survs[[2]], file = "~/GalleriCEmodel/data-raw/England_pilot_planning/survival_1halfHR.csv", row.names=F, col.names=F, sep=",", na = "0")
write.table(l_survs[[3]], file = "~/GalleriCEmodel/data-raw/England_pilot_planning/survival_3HR.csv", row.names=F, col.names=F, sep=",", na = "0")


fig_LE_bytypestagehr_75 <- res_LE_tables_interpolated %>%
  filter(`cfDNA status` == "positive") %>%
        filter(Age == "75-79") %>%
        filter(Gender == "Male") %>%
        ggplot() +
        geom_line(aes(x = Stage, y = median, group = `HR`, colour = `HR`)) +
        facet_wrap(vars(`Cancer type`), nrow = 2)









