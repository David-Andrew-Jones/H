#______________________________________________________________________________#

source("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/00_prep_surv.R")

#____ 1
#' Perform manually by sex and cancer site to allow for visual inspection
#______________________________________________________________________________#
#' Male
df_gen_pop_hz_bysex <- df_gen_pop_hz %>% filter(sex == "Males")
#_______________________________#
#' Anus
surv_temp <- surv_formatted %>%
        filter(Sex == "Persons" | Sex == "Male") %>%
        filter(NCRAS_Draw %in% c("Anus")) %>%
        mutate(across(c(Stage, NCRAS_Draw), as.character),
               across(c(Stage, NCRAS_Draw), as.factor) ) # relevel for split below

l_res_rmst_temp <- map(.x = split(surv_temp, list(surv_temp$Age, surv_temp$Stage)),
                       .f = possibly(~ f_mean_survival(.x, gen_hz = df_gen_pop_hz_bysex, cure_fraction = df_cure_fraction), otherwise = NA ))

res_rmst_male_anus <- unlist(l_res_rmst_temp, recursive = FALSE) %>%
        keep(. , str_detect(names(.), 'rmst')) %>%
        do.call("rbind", .) %>%
        rownames_to_column("patients") %>%
        separate_wider_delim(cols = patients, delim = ".", names = c("age", "stage", "get_rid", "model")) %>%
        select(-c(get_rid, variable, t)) %>%
        mutate(across(c(median,`2.5%`,`97.5%` ), \(x) round(x, 2) )) %>%
        unite(CrI_temp, c(`2.5%`, `97.5%`), sep = " to ", remove = TRUE) %>%
        mutate(CrI = paste0("(", CrI_temp, ")")) %>%
        unite(median_CrI, c(median, CrI), sep = " ", remove = FALSE) %>%
        select(age, stage, model, median_CrI) %>%
        pivot_wider(names_from = age, values_from = c(median_CrI)) %>%
        mutate(site = "Anus") %>% select(site, everything())

write_csv(res_rmst_male_anus, file = "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/res_rmst_male_anus.csv")
#_______________________________#
#' Bladder
#'
surv_temp <- surv_formatted %>%
        filter(Sex == "Persons" | Sex == "Male") %>%
        filter(NCRAS_Draw %in% c("Bladder")) %>%
        mutate(across(c(Stage, NCRAS_Draw), as.character),
               across(c(Stage, NCRAS_Draw), as.factor) ) # relevel for split below

l_res_rmst_temp <- map(.x = split(surv_temp, list(surv_temp$Age, surv_temp$Stage)),
                       .f = possibly(~ f_mean_survival(.x, gen_hz = df_gen_pop_hz_bysex, cure_fraction = df_cure_fraction), otherwise = NA ))

res_rmst_male_bladder <- unlist(l_res_rmst_temp, recursive = FALSE) %>%
        keep(. , str_detect(names(.), 'rmst')) %>%
        do.call("rbind", .) %>%
        rownames_to_column("patients") %>%
        separate_wider_delim(cols = patients, delim = ".", names = c("age", "stage", "get_rid", "model")) %>%
        select(-c(get_rid, variable, t)) %>%
        mutate(across(c(median,`2.5%`,`97.5%` ), \(x) round(x, 2) )) %>%
        unite(CrI_temp, c(`2.5%`, `97.5%`), sep = " to ", remove = TRUE) %>%
        mutate(CrI = paste0("(", CrI_temp, ")")) %>%
        unite(median_CrI, c(median, CrI), sep = " ", remove = FALSE) %>%
        select(age, stage, model, median_CrI) %>%
        pivot_wider(names_from = age, values_from = c(median_CrI)) %>%
        mutate(site = "Bladder") %>% select(site, everything())

write_csv(res_rmst_male_bladder, file = "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/res_rmst_male_bladder.csv")

#_______________________________#
#' Colon/Rectum
#'
surv_temp <- surv_formatted %>%
        filter(Sex == "Persons" | Sex == "Male") %>%
        filter(NCRAS_Draw %in% c("Colon/Rectum")) %>%
        mutate(across(c(Stage, NCRAS_Draw), as.character),
               across(c(Stage, NCRAS_Draw), as.factor) ) # relevel for split below

l_res_rmst_temp <- map(.x = split(surv_temp, list(surv_temp$Age, surv_temp$Stage)),
                       .f = possibly(~ f_mean_survival(.x, gen_hz = df_gen_pop_hz_bysex, cure_fraction = df_cure_fraction), otherwise = NA ))

res_rmst_male_ColonRectum <- unlist(l_res_rmst_temp, recursive = FALSE) %>%
        keep(. , str_detect(names(.), 'rmst')) %>%
        do.call("rbind", .) %>%
        rownames_to_column("patients") %>%
        separate_wider_delim(cols = patients, delim = ".", names = c("age", "stage", "get_rid", "model")) %>%
        select(-c(get_rid, variable, t)) %>%
        mutate(across(c(median,`2.5%`,`97.5%` ), \(x) round(x, 2) )) %>%
        unite(CrI_temp, c(`2.5%`, `97.5%`), sep = " to ", remove = TRUE) %>%
        mutate(CrI = paste0("(", CrI_temp, ")")) %>%
        unite(median_CrI, c(median, CrI), sep = " ", remove = FALSE) %>%
        select(age, stage, model, median_CrI) %>%
        pivot_wider(names_from = age, values_from = c(median_CrI)) %>%
        mutate(site = "Colon/Rectum") %>% select(site, everything())

write_csv(res_rmst_male_ColonRectum, file = "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/res_rmst_male_ColonRectum.csv")


#_______________________________#
#' Gallbladder
#'
surv_temp <- surv_formatted %>%
        filter(Sex == "Persons" | Sex == "Male") %>%
        filter(NCRAS_Draw %in% c("Gallbladder")) %>%
        mutate(across(c(Stage, NCRAS_Draw), as.character),
               across(c(Stage, NCRAS_Draw), as.factor) ) # relevel for split below

l_res_rmst_temp <- map(.x = split(surv_temp, list(surv_temp$Age, surv_temp$Stage)),
                       .f = possibly(~ f_mean_survival(.x, gen_hz = df_gen_pop_hz_bysex, cure_fraction = df_cure_fraction), otherwise = NA ))

res_rmst_male_Gallbladder <- unlist(l_res_rmst_temp, recursive = FALSE) %>%
        keep(. , str_detect(names(.), 'rmst')) %>%
        do.call("rbind", .) %>%
        rownames_to_column("patients") %>%
        separate_wider_delim(cols = patients, delim = ".", names = c("age", "stage", "get_rid", "model")) %>%
        select(-c(get_rid, variable, t)) %>%
        mutate(across(c(median,`2.5%`,`97.5%` ), \(x) round(x, 2) )) %>%
        unite(CrI_temp, c(`2.5%`, `97.5%`), sep = " to ", remove = TRUE) %>%
        mutate(CrI = paste0("(", CrI_temp, ")")) %>%
        unite(median_CrI, c(median, CrI), sep = " ", remove = FALSE) %>%
        select(age, stage, model, median_CrI) %>%
        pivot_wider(names_from = age, values_from = c(median_CrI)) %>%
        mutate(site = "Gallbladder") %>% select(site, everything())

write_csv(res_rmst_male_Gallbladder, file = "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/res_rmst_male_Gallbladder.csv")

#_______________________________#
#' Head and Neck
#'
surv_temp <- surv_formatted %>%
        filter(Sex == "Persons" | Sex == "Male") %>%
        filter(NCRAS_Draw %in% c("Head and Neck")) %>%
        mutate(across(c(Stage, NCRAS_Draw), as.character),
               across(c(Stage, NCRAS_Draw), as.factor) ) # relevel for split below

l_res_rmst_temp <- map(.x = split(surv_temp, list(surv_temp$Age, surv_temp$Stage)),
                       .f = possibly(~ f_mean_survival(.x, gen_hz = df_gen_pop_hz_bysex, cure_fraction = df_cure_fraction), otherwise = NA ))

res_rmst_male_HeadandNeck<- unlist(l_res_rmst_temp, recursive = FALSE) %>%
        keep(. , str_detect(names(.), 'rmst')) %>%
        do.call("rbind", .) %>%
        rownames_to_column("patients") %>%
        separate_wider_delim(cols = patients, delim = ".", names = c("age", "stage", "get_rid", "model")) %>%
        select(-c(get_rid, variable, t)) %>%
        mutate(across(c(median,`2.5%`,`97.5%` ), \(x) round(x, 2) )) %>%
        unite(CrI_temp, c(`2.5%`, `97.5%`), sep = " to ", remove = TRUE) %>%
        mutate(CrI = paste0("(", CrI_temp, ")")) %>%
        unite(median_CrI, c(median, CrI), sep = " ", remove = FALSE) %>%
        select(age, stage, model, median_CrI) %>%
        pivot_wider(names_from = age, values_from = c(median_CrI)) %>%
        mutate(site = "Head and Neck") %>% select(site, everything())

write_csv(res_rmst_male_HeadandNeck, file = "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/res_rmst_male_HeadandNeck.csv")


#_______________________________#
#' Kidney
#'
surv_temp <- surv_formatted %>%
        filter(Sex == "Persons" | Sex == "Male") %>%
        filter(NCRAS_Draw %in% c("Kidney")) %>%
        mutate(across(c(Stage, NCRAS_Draw), as.character),
               across(c(Stage, NCRAS_Draw), as.factor) ) # relevel for split below

l_res_rmst_temp <- map(.x = split(surv_temp, list(surv_temp$Age, surv_temp$Stage)),
                       .f = possibly(~ f_mean_survival(.x, gen_hz = df_gen_pop_hz_bysex, cure_fraction = df_cure_fraction), otherwise = NA ))

res_rmst_male_Kidney<- unlist(l_res_rmst_temp, recursive = FALSE) %>%
        keep(. , str_detect(names(.), 'rmst')) %>%
        do.call("rbind", .) %>%
        rownames_to_column("patients") %>%
        separate_wider_delim(cols = patients, delim = ".", names = c("age", "stage", "get_rid", "model")) %>%
        select(-c(get_rid, variable, t)) %>%
        mutate(across(c(median,`2.5%`,`97.5%` ), \(x) round(x, 2) )) %>%
        unite(CrI_temp, c(`2.5%`, `97.5%`), sep = " to ", remove = TRUE) %>%
        mutate(CrI = paste0("(", CrI_temp, ")")) %>%
        unite(median_CrI, c(median, CrI), sep = " ", remove = FALSE) %>%
        select(age, stage, model, median_CrI) %>%
        pivot_wider(names_from = age, values_from = c(median_CrI)) %>%
        mutate(site = "Kidney") %>% select(site, everything())

write_csv(res_rmst_male_Kidney, file = "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/res_rmst_male_Kidney.csv")


#_______________________________#
#' Liver/Bile Duct
#'
surv_temp <- surv_formatted %>%
        filter(Sex == "Persons" | Sex == "Male") %>%
        filter(NCRAS_Draw %in% c("Liver/Bile-duct")) %>%
        mutate(across(c(Stage, NCRAS_Draw), as.character),
               across(c(Stage, NCRAS_Draw), as.factor) ) # relevel for split below

l_res_rmst_temp <- map(.x = split(surv_temp, list(surv_temp$Age, surv_temp$Stage)),
                       .f = possibly(~ f_mean_survival(.x, gen_hz = df_gen_pop_hz_bysex, cure_fraction = df_cure_fraction), otherwise = NA ))

res_rmst_male_LiverBileDuct<- unlist(l_res_rmst_temp, recursive = FALSE) %>%
        keep(. , str_detect(names(.), 'rmst')) %>%
        do.call("rbind", .) %>%
        rownames_to_column("patients") %>%
        separate_wider_delim(cols = patients, delim = ".", names = c("age", "stage", "get_rid", "model")) %>%
        select(-c(get_rid, variable, t)) %>%
        mutate(across(c(median,`2.5%`,`97.5%` ), \(x) round(x, 2) )) %>%
        unite(CrI_temp, c(`2.5%`, `97.5%`), sep = " to ", remove = TRUE) %>%
        mutate(CrI = paste0("(", CrI_temp, ")")) %>%
        unite(median_CrI, c(median, CrI), sep = " ", remove = FALSE) %>%
        select(age, stage, model, median_CrI) %>%
        pivot_wider(names_from = age, values_from = c(median_CrI)) %>%
        mutate(site = "Liver/Bile-duct") %>% select(site, everything())

write_csv(res_rmst_male_LiverBileDuct, file = "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/res_rmst_male_LiverBileDuct.csv")


#_______________________________#
#' Lung
#'
surv_temp <- surv_formatted %>%
        filter(Sex == "Persons" | Sex == "Male") %>%
        filter(NCRAS_Draw %in% c("Lung")) %>%
        mutate(across(c(Stage, NCRAS_Draw), as.character),
               across(c(Stage, NCRAS_Draw), as.factor) ) # relevel for split below

l_res_rmst_temp <- map(.x = split(surv_temp, list(surv_temp$Age, surv_temp$Stage)),
                       .f = possibly(~ f_mean_survival(.x, gen_hz = df_gen_pop_hz_bysex, cure_fraction = df_cure_fraction), otherwise = NA ))

res_rmst_male_Lung <- unlist(l_res_rmst_temp, recursive = FALSE) %>%
        keep(. , str_detect(names(.), 'rmst')) %>%
        do.call("rbind", .) %>%
        rownames_to_column("patients") %>%
        separate_wider_delim(cols = patients, delim = ".", names = c("age", "stage", "get_rid", "model")) %>%
        select(-c(get_rid, variable, t)) %>%
        mutate(across(c(median,`2.5%`,`97.5%` ), \(x) round(x, 2) )) %>%
        unite(CrI_temp, c(`2.5%`, `97.5%`), sep = " to ", remove = TRUE) %>%
        mutate(CrI = paste0("(", CrI_temp, ")")) %>%
        unite(median_CrI, c(median, CrI), sep = " ", remove = FALSE) %>%
        select(age, stage, model, median_CrI) %>%
        pivot_wider(names_from = age, values_from = c(median_CrI)) %>%
        mutate(site = "Lung") %>% select(site, everything())

write_csv(res_rmst_male_Lung, file = "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/res_rmst_male_Lung.csv")


#_______________________________#
#' Lymphoma
#'
surv_temp <- surv_formatted %>%
        filter(Sex == "Persons" | Sex == "Male") %>%
        filter(NCRAS_Draw %in% c("Lymphoma")) %>%
        mutate(across(c(Stage, NCRAS_Draw), as.character),
               across(c(Stage, NCRAS_Draw), as.factor) ) # relevel for split below

l_res_rmst_temp <- map(.x = split(surv_temp, list(surv_temp$Age, surv_temp$Stage)),
                       .f = possibly(~ f_mean_survival(.x, gen_hz = df_gen_pop_hz_bysex, cure_fraction = df_cure_fraction), otherwise = NA ))

res_rmst_male_Lymphoma <- unlist(l_res_rmst_temp, recursive = FALSE) %>%
        keep(. , str_detect(names(.), 'rmst')) %>%
        do.call("rbind", .) %>%
        rownames_to_column("patients") %>%
        separate_wider_delim(cols = patients, delim = ".", names = c("age", "stage", "get_rid", "model")) %>%
        select(-c(get_rid, variable, t)) %>%
        mutate(across(c(median,`2.5%`,`97.5%` ), \(x) round(x, 2) )) %>%
        unite(CrI_temp, c(`2.5%`, `97.5%`), sep = " to ", remove = TRUE) %>%
        mutate(CrI = paste0("(", CrI_temp, ")")) %>%
        unite(median_CrI, c(median, CrI), sep = " ", remove = FALSE) %>%
        select(age, stage, model, median_CrI) %>%
        pivot_wider(names_from = age, values_from = c(median_CrI)) %>%
        mutate(site = "Lymphoma") %>% select(site, everything())

write_csv(res_rmst_male_Lymphoma, file = "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/res_rmst_male_Lymphoma.csv")


#_______________________________#
#' Melanoma
#'
surv_temp <- surv_formatted %>%
        filter(Sex == "Persons" | Sex == "Male") %>%
        filter(NCRAS_Draw %in% c("Melanoma")) %>%
        mutate(across(c(Stage, NCRAS_Draw), as.character),
               across(c(Stage, NCRAS_Draw), as.factor) ) # relevel for split below

l_res_rmst_temp <- map(.x = split(surv_temp, list(surv_temp$Age, surv_temp$Stage)),
                       .f = possibly(~ f_mean_survival(.x, gen_hz = df_gen_pop_hz_bysex, cure_fraction = df_cure_fraction), otherwise = NA ))

res_rmst_male_Melanoma <- unlist(l_res_rmst_temp, recursive = FALSE) %>%
        keep(. , str_detect(names(.), 'rmst')) %>%
        do.call("rbind", .) %>%
        rownames_to_column("patients") %>%
        separate_wider_delim(cols = patients, delim = ".", names = c("age", "stage", "get_rid", "model")) %>%
        select(-c(get_rid, variable, t)) %>%
        mutate(across(c(median,`2.5%`,`97.5%` ), \(x) round(x, 2) )) %>%
        unite(CrI_temp, c(`2.5%`, `97.5%`), sep = " to ", remove = TRUE) %>%
        mutate(CrI = paste0("(", CrI_temp, ")")) %>%
        unite(median_CrI, c(median, CrI), sep = " ", remove = FALSE) %>%
        select(age, stage, model, median_CrI) %>%
        pivot_wider(names_from = age, values_from = c(median_CrI)) %>%
        mutate(site = "Melanoma") %>% select(site, everything())

write_csv(res_rmst_male_Melanoma, file = "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/res_rmst_male_Melanoma.csv")


#_______________________________#
#' Oesophagus
#'
surv_temp <- surv_formatted %>%
        filter(Sex == "Persons" | Sex == "Male") %>%
        filter(NCRAS_Draw %in% c("Esophagus")) %>%
        mutate(across(c(Stage, NCRAS_Draw), as.character),
               across(c(Stage, NCRAS_Draw), as.factor) ) # relevel for split below

l_res_rmst_temp <- map(.x = split(surv_temp, list(surv_temp$Age, surv_temp$Stage)),
                       .f = possibly(~ f_mean_survival(.x, gen_hz = df_gen_pop_hz_bysex, cure_fraction = df_cure_fraction), otherwise = NA ))

res_rmst_male_Esophagus <- unlist(l_res_rmst_temp, recursive = FALSE) %>%
        keep(. , str_detect(names(.), 'rmst')) %>%
        do.call("rbind", .) %>%
        rownames_to_column("patients") %>%
        separate_wider_delim(cols = patients, delim = ".", names = c("age", "stage", "get_rid", "model")) %>%
        select(-c(get_rid, variable, t)) %>%
        mutate(across(c(median,`2.5%`,`97.5%` ), \(x) round(x, 2) )) %>%
        unite(CrI_temp, c(`2.5%`, `97.5%`), sep = " to ", remove = TRUE) %>%
        mutate(CrI = paste0("(", CrI_temp, ")")) %>%
        unite(median_CrI, c(median, CrI), sep = " ", remove = FALSE) %>%
        select(age, stage, model, median_CrI) %>%
        pivot_wider(names_from = age, values_from = c(median_CrI)) %>%
        mutate(site = "Esophagus") %>% select(site, everything())

write_csv(res_rmst_male_Esophagus, file = "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/res_rmst_male_Oesophagus.csv")

#_______________________________#
#' Pancreas
#'
surv_temp <- surv_formatted %>%
        filter(Sex == "Persons" | Sex == "Male") %>%
        filter(NCRAS_Draw %in% c("Pancreas")) %>%
        mutate(across(c(Stage, NCRAS_Draw), as.character),
               across(c(Stage, NCRAS_Draw), as.factor) ) # relevel for split below

l_res_rmst_temp <- map(.x = split(surv_temp, list(surv_temp$Age, surv_temp$Stage)),
                       .f = possibly(~ f_mean_survival(.x, gen_hz = df_gen_pop_hz_bysex, cure_fraction = df_cure_fraction), otherwise = NA ))

res_rmst_male_Pancreas <- unlist(l_res_rmst_temp, recursive = FALSE) %>%
        keep(. , str_detect(names(.), 'rmst')) %>%
        do.call("rbind", .) %>%
        rownames_to_column("patients") %>%
        separate_wider_delim(cols = patients, delim = ".", names = c("age", "stage", "get_rid", "model")) %>%
        select(-c(get_rid, variable, t)) %>%
        mutate(across(c(median,`2.5%`,`97.5%` ), \(x) round(x, 2) )) %>%
        unite(CrI_temp, c(`2.5%`, `97.5%`), sep = " to ", remove = TRUE) %>%
        mutate(CrI = paste0("(", CrI_temp, ")")) %>%
        unite(median_CrI, c(median, CrI), sep = " ", remove = FALSE) %>%
        select(age, stage, model, median_CrI) %>%
        pivot_wider(names_from = age, values_from = c(median_CrI)) %>%
        mutate(site = "Pancreas") %>% select(site, everything())

write_csv(res_rmst_male_Pancreas, file = "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/res_rmst_male_Pancreas.csv")

#_______________________________#
#' Prostate
#'
surv_temp <- surv_formatted %>%
        filter(Sex == "Persons" | Sex == "Male") %>%
        filter(NCRAS_Draw %in% c("Prostate")) %>%
        mutate(across(c(Stage, NCRAS_Draw), as.character),
               across(c(Stage, NCRAS_Draw), as.factor) ) # relevel for split below

l_res_rmst_temp <- map(.x = split(surv_temp, list(surv_temp$Age, surv_temp$Stage)),
                       .f = possibly(~ f_mean_survival(.x, gen_hz = df_gen_pop_hz_bysex, cure_fraction = df_cure_fraction), otherwise = NA ))

res_rmst_male_Prostate <- unlist(l_res_rmst_temp, recursive = FALSE) %>%
        keep(. , str_detect(names(.), 'rmst')) %>%
        do.call("rbind", .) %>%
        rownames_to_column("patients") %>%
        separate_wider_delim(cols = patients, delim = ".", names = c("age", "stage", "get_rid", "model")) %>%
        select(-c(get_rid, variable, t)) %>%
        mutate(across(c(median,`2.5%`,`97.5%` ), \(x) round(x, 2) )) %>%
        unite(CrI_temp, c(`2.5%`, `97.5%`), sep = " to ", remove = TRUE) %>%
        mutate(CrI = paste0("(", CrI_temp, ")")) %>%
        unite(median_CrI, c(median, CrI), sep = " ", remove = FALSE) %>%
        select(age, stage, model, median_CrI) %>%
        pivot_wider(names_from = age, values_from = c(median_CrI)) %>%
        mutate(site = "Prostate") %>% select(site, everything())

write_csv(res_rmst_male_Prostate, file = "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/res_rmst_male_Prostate.csv")


#_______________________________#
#' Sarcoma
#'
surv_temp <- surv_formatted %>%
        filter(Sex == "Persons" | Sex == "Male") %>%
        filter(NCRAS_Draw %in% c("Sarcoma")) %>%
        mutate(across(c(Stage, NCRAS_Draw), as.character),
               across(c(Stage, NCRAS_Draw), as.factor) ) # relevel for split below

l_res_rmst_temp <- map(.x = split(surv_temp, list(surv_temp$Age, surv_temp$Stage)),
                       .f = possibly(~ f_mean_survival(.x, gen_hz = df_gen_pop_hz_bysex, cure_fraction = df_cure_fraction), otherwise = NA ))

res_rmst_male_Sarcoma <- unlist(l_res_rmst_temp, recursive = FALSE) %>%
        keep(. , str_detect(names(.), 'rmst')) %>%
        do.call("rbind", .) %>%
        rownames_to_column("patients") %>%
        separate_wider_delim(cols = patients, delim = ".", names = c("age", "stage", "get_rid", "model")) %>%
        select(-c(get_rid, variable, t)) %>%
        mutate(across(c(median,`2.5%`,`97.5%` ), \(x) round(x, 2) )) %>%
        unite(CrI_temp, c(`2.5%`, `97.5%`), sep = " to ", remove = TRUE) %>%
        mutate(CrI = paste0("(", CrI_temp, ")")) %>%
        unite(median_CrI, c(median, CrI), sep = " ", remove = FALSE) %>%
        select(age, stage, model, median_CrI) %>%
        pivot_wider(names_from = age, values_from = c(median_CrI)) %>%
        mutate(site = "Sarcoma") %>% select(site, everything())

write_csv(res_rmst_male_Sarcoma, file = "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/res_rmst_male_Sarcoma.csv")


#_______________________________#
#' Stomach
#'
surv_temp <- surv_formatted %>%
        filter(Sex == "Persons" | Sex == "Male") %>%
        filter(NCRAS_Draw %in% c("Stomach")) %>%
        mutate(across(c(Stage, NCRAS_Draw), as.character),
               across(c(Stage, NCRAS_Draw), as.factor) ) # relevel for split below

l_res_rmst_temp <- map(.x = split(surv_temp, list(surv_temp$Age, surv_temp$Stage)),
                       .f = possibly(~ f_mean_survival(.x, gen_hz = df_gen_pop_hz_bysex, cure_fraction = df_cure_fraction), otherwise = NA ))

res_rmst_male_Stomach <- unlist(l_res_rmst_temp, recursive = FALSE) %>%
        keep(. , str_detect(names(.), 'rmst')) %>%
        do.call("rbind", .) %>%
        rownames_to_column("patients") %>%
        separate_wider_delim(cols = patients, delim = ".", names = c("age", "stage", "get_rid", "model")) %>%
        select(-c(get_rid, variable, t)) %>%
        mutate(across(c(median,`2.5%`,`97.5%` ), \(x) round(x, 2) )) %>%
        unite(CrI_temp, c(`2.5%`, `97.5%`), sep = " to ", remove = TRUE) %>%
        mutate(CrI = paste0("(", CrI_temp, ")")) %>%
        unite(median_CrI, c(median, CrI), sep = " ", remove = FALSE) %>%
        select(age, stage, model, median_CrI) %>%
        pivot_wider(names_from = age, values_from = c(median_CrI)) %>%
        mutate(site = "Stomach") %>% select(site, everything())

write_csv(res_rmst_male_Stomach, file = "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/res_rmst_male_Stomach.csv")

#_______________________________#
#' Thyroid
#'
surv_temp <- surv_formatted %>%
        filter(Sex == "Persons" | Sex == "Male") %>%
        filter(NCRAS_Draw %in% c("Thyroid")) %>%
        mutate(across(c(Stage, NCRAS_Draw), as.character),
               across(c(Stage, NCRAS_Draw), as.factor) ) # relevel for split below

l_res_rmst_temp <- map(.x = split(surv_temp, list(surv_temp$Age, surv_temp$Stage)),
                       .f = possibly(~ f_mean_survival(.x, gen_hz = df_gen_pop_hz_bysex, cure_fraction = df_cure_fraction), otherwise = NA ))

res_rmst_male_Thyroid <- unlist(l_res_rmst_temp, recursive = FALSE) %>%
        keep(. , str_detect(names(.), 'rmst')) %>%
        do.call("rbind", .) %>%
        rownames_to_column("patients") %>%
        separate_wider_delim(cols = patients, delim = ".", names = c("age", "stage", "get_rid", "model")) %>%
        select(-c(get_rid, variable, t)) %>%
        mutate(across(c(median,`2.5%`,`97.5%` ), \(x) round(x, 2) )) %>%
        unite(CrI_temp, c(`2.5%`, `97.5%`), sep = " to ", remove = TRUE) %>%
        mutate(CrI = paste0("(", CrI_temp, ")")) %>%
        unite(median_CrI, c(median, CrI), sep = " ", remove = FALSE) %>%
        select(age, stage, model, median_CrI) %>%
        pivot_wider(names_from = age, values_from = c(median_CrI)) %>%
        mutate(site = "Thyroid") %>% select(site, everything())

write_csv(res_rmst_male_Thyroid, file = "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/res_rmst_male_Thyroid.csv")

#_______________________________#
#' Urothelial Tract
#'
surv_temp <- surv_formatted %>%
        filter(Sex == "Persons" | Sex == "Male") %>%
        filter(NCRAS_Draw %in% c("Urothelial Tract")) %>%
        mutate(across(c(Stage, NCRAS_Draw), as.character),
               across(c(Stage, NCRAS_Draw), as.factor) ) # relevel for split below

l_res_rmst_temp <- map(.x = split(surv_temp, list(surv_temp$Age, surv_temp$Stage)),
                       .f = possibly(~ f_mean_survival(.x, gen_hz = df_gen_pop_hz_bysex, cure_fraction = df_cure_fraction), otherwise = NA ))

res_rmst_male_UrothelialTract <- unlist(l_res_rmst_temp, recursive = FALSE) %>%
        keep(. , str_detect(names(.), 'rmst')) %>%
        do.call("rbind", .) %>%
        rownames_to_column("patients") %>%
        separate_wider_delim(cols = patients, delim = ".", names = c("age", "stage", "get_rid", "model")) %>%
        select(-c(get_rid, variable, t)) %>%
        mutate(across(c(median,`2.5%`,`97.5%` ), \(x) round(x, 2) )) %>%
        unite(CrI_temp, c(`2.5%`, `97.5%`), sep = " to ", remove = TRUE) %>%
        mutate(CrI = paste0("(", CrI_temp, ")")) %>%
        unite(median_CrI, c(median, CrI), sep = " ", remove = FALSE) %>%
        select(age, stage, model, median_CrI) %>%
        pivot_wider(names_from = age, values_from = c(median_CrI)) %>%
        mutate(site = "Urothelial Tract") %>% select(site, everything())

write_csv(res_rmst_male_UrothelialTract, file = "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/res_rmst_male_UrothelialTract.csv")

#______________________________________________________________________________#


#' Female
df_gen_pop_hz_bysex <- df_gen_pop_hz %>% filter(sex == "Females")
surv_female <- surv_formatted %>% filter(Sex == "Persons" | Sex == "Male")
l_female_rmst <- f_mean_surv_byagecancersitesex()


#______________________________________________________________________________#

#____ 2
#' Load in LE results to graph
#______________________________________________________________________________#
library(ggsci)

df_male_rmst <-
        list.files(path = "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", pattern = "res_rmst_male") %>%
        map_df(~read_csv(paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", .)))

# Undo all earlier formatting
df_male_rmst_long <- df_male_rmst %>%
        pivot_longer(cols = contains("years"),
                     names_to = "age",
                     values_to = "median_CrI") %>%
        separate_wider_delim( cols = median_CrI, delim = " ", names = c("median", "2.5% CrI", "to", "97.5% CrI")) %>%
        mutate(`2.5% CrI` = str_sub(`2.5% CrI`, 2, -1), `97.5% CrI` = str_sub(`97.5% CrI`, 1, -2)) %>%
        filter(model == 1) %>%
        select(-c(to, model)) %>%
        mutate(across(c(median, `2.5% CrI`,`97.5% CrI`), as.numeric))

plot__male_LE_by_site_stage_age <- df_male_rmst_long %>%
        #filter(age == "50-54 years") %>%
        mutate(age = paste0(str_sub(age, 1, 2), "yr")) %>%
        ggplot() +
        geom_point(aes(x = age, y = median, colour = stage), position= position_dodge(0.5)) +
        geom_errorbar(aes(x = age, colour = stage, ymin = `2.5% CrI`, ymax = `97.5% CrI`,), position= position_dodge(0.5), width=.1) +
        facet_wrap(facets = ~site) +
        scale_color_jco() +
        labs(color = "Stage", x = "Age", y = "Male median life expectancy (years) and 95% credible interval by age and stage at diagnosis")+
        theme_bw()


#____ 2
#' Life years lost
#______________________________________________________________________________#

df_UK_LifeTable1719 <- read_csv("~/GalleriCEmodel/data-raw/NICE_modifier/UK_life_table_1719.csv") %>%
        select(age, sex, ex) %>%
        filter(age %in% c(50,55,60,65,70,75,80)) %>%
        mutate(across(c(age), as.character))

df_male_rmst_long_genpop_LE <-  df_male_rmst_long %>%
        mutate(age = str_sub(age, 1, 2)) %>%
        left_join(df_UK_LifeTable1719 %>%
                          filter(sex == "Males"), by = c("age")) %>%
        mutate(`median LOLE` = median - ex, `LOLE 2.5% CrI` = `2.5% CrI` - ex ,`LOLE 97.5% CrI` = `97.5% CrI` - ex)

plot_male_LOLE_by_site_stage_age <- df_male_rmst_long_genpop_LE %>%
        mutate(age = paste0(age, "yr")) %>%
        ggplot() +
        geom_point(aes(x = age, y = `median LOLE`, colour = stage), position= position_dodge(0.5)) +
        geom_errorbar(aes(x = age, colour = stage, ymin = `LOLE 2.5% CrI`, ymax = `LOLE 97.5% CrI`,), position= position_dodge(0.5), width=.1) +
        facet_wrap(facets = ~site) +
        scale_color_jco() +
        labs(color = "Stage", x = "Age", y = "Male median loss of life expectancy (years) and 95% credible interval by age and stage at diagnosis")+
        theme_bw()

#____ 2
#' LOLE difference from stage 1
#______________________________________________________________________________#

df_male_rmst_diff_LOLE <- df_male_rmst_long_genpop_LE %>%
        select(site, stage, age, `median LOLE`, `LOLE 2.5% CrI`, `LOLE 97.5% CrI`) %>%
        pivot_wider(names_from = stage, values_from = c( `median LOLE`, `LOLE 2.5% CrI`, `LOLE 97.5% CrI`)) %>%
        mutate(stage_1 = 0, `diff_LOLE_1_2 median` = `median LOLE_II` - `median LOLE_I`, `diff_LOLE_1_2 2.5% CrI` = `LOLE 2.5% CrI_II` - `LOLE 2.5% CrI_I`, `diff_LOLE_1_2 97.5% CrI` = `LOLE 97.5% CrI_II` - `LOLE 97.5% CrI_I`) %>%
        mutate(`diff_LOLE_1_3 median`= `median LOLE_III` - `median LOLE_I`, `diff_LOLE_1_3 2.5% CrI` = `LOLE 2.5% CrI_III` - `LOLE 2.5% CrI_I`, `diff_LOLE_1_3 97.5% CrI` = `LOLE 97.5% CrI_III` - `LOLE 97.5% CrI_I`) %>%
        mutate(`diff_LOLE_1_4 median` = `median LOLE_IV` - `median LOLE_I`, `diff_LOLE_1_4 2.5% CrI` = `LOLE 2.5% CrI_IV` - `LOLE 2.5% CrI_I`, `diff_LOLE_1_4 97.5% CrI` = `LOLE 97.5% CrI_IV` - `LOLE 97.5% CrI_I`) %>%
        select(site, age, `diff_LOLE_1_2 median`, `diff_LOLE_1_2 2.5% CrI`, `diff_LOLE_1_2 97.5% CrI`,
               `diff_LOLE_1_3 median`, `diff_LOLE_1_3 2.5% CrI`, `diff_LOLE_1_3 97.5% CrI`,
               `diff_LOLE_1_4 median`, `diff_LOLE_1_4 2.5% CrI`, `diff_LOLE_1_4 97.5% CrI`) %>%
        pivot_longer(
                cols = starts_with("diff"),
                names_to = "type",
                values_to = "value",
                values_drop_na = FALSE
        ) %>%
        separate_wider_delim(cols = type, delim = " ", names = c("stage", "statistic"), too_many = c( "merge")) %>%
        mutate(across(where(is.character), as.factor)) %>%
        pivot_wider(names_from = statistic, values_from = value)

plot_male_diff_LOLE_by_site_stage_age <- df_male_rmst_diff_LOLE %>%
        ggplot() +
        geom_point(aes(x = age, y = `median`, colour = stage), position= position_dodge(0.5)) +
        geom_errorbar(aes(x = age, colour = stage, ymin = `2.5% CrI`, ymax = `97.5% CrI`,), position= position_dodge(0.5), width=.1) +
        facet_wrap(facets = ~site) +
        scale_color_jco() +
        labs(color = "Stage", x = "Age", y = "Male difference in loss of life expectancy (years) and 95% credible interval by age")+
        theme_bw()




#______________________________________________________________________________#


#' Female
df_gen_pop_hz_bysex <- df_gen_pop_hz %>% filter(sex == "Females")
surv_female <- surv_formatted %>% filter(Sex == "Persons" | Sex == "Male")
l_female_rmst <- f_mean_surv_byagecancersitesex()








































#______________________________________________________________________________#
#' Life years lost calculation
#'

df_lifetbl1719 <- read_csv("~/GalleriCEmodel/data-raw/NICE_modifier/UK_life_table_1719.csv")










###### For presentation

# #______________________________________________________________________________#
# # Colorectal cancer example
# Colorectal.I <- surv_formatted %>%
#         filter(NCRAS_Draw %in% levels(as.factor(df_cure_fraction$Cancer))) %>%
#         mutate(across(c(Age, NCRAS_Draw), as.character),
#                across(c(Age, NCRAS_Draw), as.factor) ) %>%
#         filter(NCRAS_Draw == "Colon/Rectum") %>%
#         filter(Age == "50-54 years") %>%
#         filter(Stage == "I")
#
# res.I <- f_mean_survival(Colorectal.I)
#
# Colorectal.II <- surv_formatted %>%
#         filter(NCRAS_Draw %in% levels(as.factor(df_cure_fraction$Cancer))) %>%
#         mutate(across(c(Age, NCRAS_Draw), as.character),
#                across(c(Age, NCRAS_Draw), as.factor) ) %>%
#         filter(NCRAS_Draw == "Colon/Rectum") %>%
#         filter(Age == "50-54 years") %>%
#         filter(Stage == "II")
#
# res.II <- f_mean_survival(Colorectal.II)
#
# Colorectal.III <- surv_formatted %>%
#         filter(NCRAS_Draw %in% levels(as.factor(df_cure_fraction$Cancer))) %>%
#         mutate(across(c(Age, NCRAS_Draw), as.character),
#                across(c(Age, NCRAS_Draw), as.factor) ) %>%
#         filter(NCRAS_Draw == "Colon/Rectum") %>%
#         filter(Age == "50-54 years") %>%
#         filter(Stage == "III")
#
# res.III <- f_mean_survival(Colorectal.III)
#
# Colorectal.IV <- surv_formatted %>%
#         filter(NCRAS_Draw %in% levels(as.factor(df_cure_fraction$Cancer))) %>%
#         mutate(across(c(Age, NCRAS_Draw), as.character),
#                across(c(Age, NCRAS_Draw), as.factor) ) %>%
#         filter(NCRAS_Draw == "Colon/Rectum") %>%
#         filter(Age == "50-54 years") %>%
#         filter(Stage == "IV")
#
# res.IV <- f_mean_survival(Colorectal.IV)
