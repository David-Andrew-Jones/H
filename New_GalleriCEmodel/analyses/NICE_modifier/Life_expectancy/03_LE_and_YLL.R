#' _____________________________________________________________________________
#' _____ 03_LE_and_YLL

#' Post-analysis processing
#' _____________________________________________________________________________
library(openxlsx)
results_path <- "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/Results/mcmc_5ysurv_splithaz/"
excel_output <- createWorkbook()

#' Create template dataframe with all potential combinations
df_all_combos <- expand.grid(Gender = c("Male","Female"),
                         `Cancer type` = c("all", "Anus", "Bladder", "Breast", "Cervix", "Colon/Rectum", "Esophagus", "Gallbladder",
                                           "Head and Neck", "Kidney", "Liver/Bile-duct", "Lung", "Lymphoma", "Melanoma", "Ovary",
                                           "Pancreas", "Prostate", "Sarcoma", "Stomach", "Thyroid", "Urothelial Tract", "Uterus"),
                         Stage = c("I", "II", "III", "IV"),
                         Age = c("50-54","55-59","60-64","65-69","70-74","75-79"),
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


#' Add on the results files (only generated when the model did not error out) to check which estimates are missing
df_combos_checklist_full <- df_all_combos %>%
  pivot_wider(names_from = "Age", values_from = "modelled") %>%
  arrange(Gender, `Cancer type`, Stage) %>%
  mutate(across(`50-54`:`75-79`, ~ case_when(. == "Yes" ~ 1, .default = 0))) %>%
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))

write.csv(df_combos_checklist_full, "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/Results/model_convergence_full.csv")

df_combos_checklist_noHR<- df_all_combos %>%
  pivot_wider(names_from = "Age", values_from = "modelled") %>%
  arrange(Gender, `Cancer type`, Stage) %>%
  mutate(across(`50-54`:`75-79`, ~ case_when(. == "Yes" ~ 1, .default = 0))) %>%
  filter(HR == "1" & `cfDNA status` == "negative") %>%
  select(-HR,-`cfDNA status`) %>%
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))

write.csv(df_combos_checklist_noHR, "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/Results/model_convergence_noHR.csv")


#asd <- df_surv_byHR_CF %>% filter(NCRAS_Draw == 'Gallbladder') %>% filter(Age == "75-79")%>% filter(Stage == "IV")

#' _____________________________________________________________________________
#' _____ Life Expectancy
#' Load in per cancer tables and merge
res_LE_tables_all_full <- list.files(path = results_path, pattern = "^LE") %>%
        map_df(~read_csv(paste0(results_path, .))) %>%
        mutate(characteristics = str_replace(characteristics, pattern = "1.5", "1andhalf")) %>% # Change as below will not work otherwise %>%
        filter(!grepl(c("Male.Cervix|Male.Breast|Female.Prostate|Male.Ovary|Male.Uterus"), characteristics))

#' Write out in nice format
res_LE_tables_all_write <- res_LE_tables_all_full %>%
  separate_wider_delim(cols = characteristics, delim = ".", names = c("Gender", "Cancer type", "Stage", "Age", "HR", "cfDNA status"))

#' Save by hazard ratio
addWorksheet(excel_output, sheetName = "1.1 LE no HR")
addWorksheet(excel_output, sheetName = "1.2 LE 1.5 HR")
addWorksheet(excel_output, sheetName = "1.3 LE 3 HR")
writeData(excel_output, sheet = "1.1 LE no HR", res_LE_tables_all_write %>% filter(HR == "1"))
writeData(excel_output, sheet = "1.2 LE 1.5 HR", res_LE_tables_all_write %>% filter(HR == "1andhalf"))
writeData(excel_output, sheet = "1.3 LE 3 HR", res_LE_tables_all_write %>% filter(HR == "3"))

# Merge on the results to the all combos dataframe to perform interpolation
res_LE_tables <- df_all_combos %>%
  left_join(res_LE_tables_all_full %>%
              separate_wider_delim(cols = characteristics, delim = ".", names = c("Gender", "Cancer type", "Stage", "Age", "HR", "cfDNA status")) %>%
              select(-t, -variable ,-CrI_temp, -CrI, -median_CrI),
            by = c("Gender", "Cancer type", "Stage", "Age", "HR", "cfDNA status")
  ) %>%
  mutate(`Cancer type` = case_when(`Cancer type` == "all" ~ "All combined", .default = `Cancer type`)) %>%
  ## remove the combinations where all are NA
  group_by(Gender, `Cancer type`, Stage, HR, `cfDNA status`) %>%
  filter(any(!is.na(median))) %>%
  ungroup() %>%
  arrange(Gender, `Cancer type`, Stage, HR, `cfDNA status`)

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
  left_join(tibble(Age = 0:79), by = character()) %>%
  left_join(df_LE_tables_with_splinefunction) %>%
  mutate(interpolated_median = sapply(1:length(Age), function(z){zeta_1[z][[1]](Age[z])})) %>%
  mutate(`interpolated_2.5%` = sapply(1:length(Age), function(z){zeta_2[z][[1]](Age[z])})) %>%
  mutate(`interpolated_97.5%` = sapply(1:length(Age), function(z){zeta_3[z][[1]](Age[z])})) %>%
  select(Gender, `Cancer type`, Stage, HR, `cfDNA status`,Age, interpolated_median, `interpolated_2.5%`, `interpolated_97.5%`)


res_LE_tables_interpolated <- res_LE_tables %>%
  separate(Age, into = c("Low", "Hi"), remove = FALSE) %>%
  mutate(Low = as.numeric(Low)) %>%
  left_join(df_interpolated_inc_by_age, by = c("Gender", "Cancer type", "Stage", "Low" = "Age", "HR", "cfDNA status"))%>%
  mutate(median = case_when(is.na(median) & !(`Cancer type` %in% c("Thyroid", "Prostate")) ~ round(interpolated_median, 2), .default = median),
         `2.5%` = case_when(is.na(`2.5%`) & !(`Cancer type` %in% c("Thyroid", "Prostate")) ~ round(`interpolated_2.5%`, 2), .default = `2.5%`),
         `97.5%` = case_when(is.na(`97.5%`) & !(`Cancer type` %in% c("Thyroid", "Prostate")) ~ round(`interpolated_97.5%`,2), .default = `97.5%`)) %>%
  select(-interpolated_median, -`interpolated_2.5%`, -`interpolated_97.5%`, -Low, -Hi)


#' _____ All cancer, all exc prostate and thyroid, and prespecified 12


v_12_prespecified <- c("Anus", "Bladder", "Colorectal", "Esophagus", "Head and Neck", "Liver/Bile-Duct", "Lung", "Lymphoma", "Ovary", "Pancreatic", "Stomach")


#' _____________________________________________________________________________
#' _____ Year of lost life

df_UK_LifeTable1719 <- read_csv("~/GalleriCEmodel/data-raw/NICE_modifier/UK_life_table_1719.csv") %>%
        select(age, sex, ex)


df_UK_LifeTable1719_with_splinefunction <- df_UK_LifeTable1719 %>%
  group_by(sex) %>%
  summarise(
    zeta_1 = list(splinefun(age, ex, method="natural")),
  ) %>%
  ungroup()

df_interpolated_UK_LifeTable1719 = df_UK_LifeTable1719 %>%
  select(sex) %>%
  distinct() %>%
  left_join(tibble(age = seq(from=0,to=79,by=0.5)), by = character()) %>% #incidence data is only used up to 85
  left_join(df_UK_LifeTable1719_with_splinefunction) %>%
  mutate(interpolated_ex = round(sapply(1:length(age), function(z){zeta_1[z][[1]](age[z])}), 2)) %>%
  select(sex, age, interpolated_ex) %>%
  filter(age %in% c(52.5, 57.5,62.5, 67.5, 72.5, 77.5)) %>%
  mutate(across(c(age), as.character)) %>%
  mutate(sex = case_when(sex == "Females" ~ "Female",
                         sex == "Males" ~ "Male")) %>%
  mutate(age = case_when(age == 52.5 ~ "50-54",
                         age == 57.5 ~ "55-59",
                         age == 62.5 ~ "60-64",
                         age == 67.5 ~ "65-69",
                         age == 72.5 ~ "70-74",
                         age == 77.5 ~ "75-79", .default = NA))


res_YLL_tables_all_full <- res_LE_tables %>%
  left_join(df_interpolated_UK_LifeTable1719, by = c("Age" = "age", "Gender" = "sex")) %>%
  mutate(`median YLL` = round(interpolated_ex - median, 2) ,`YLL 2.5% CrI` = round(interpolated_ex - `2.5%`, 2)  ,`YLL 97.5% CrI` = round(interpolated_ex - `97.5%`, 2)) %>%
  unite(YLL_CrI_temp, c(`YLL 2.5% CrI`, `YLL 97.5% CrI`), sep = " to ", remove = FALSE) %>%
  mutate(YLL_CrI = paste0("(", YLL_CrI_temp, ")")) %>%
  unite(YLL_median_CrI, c(`median YLL`, YLL_CrI), sep = " ", remove = FALSE) %>%
  select(Gender:`cfDNA status`, YLL_median_CrI, `median YLL`, `YLL 2.5% CrI`, `YLL 97.5% CrI`)


res_YLL_tables_all_full_interpolated <- res_LE_tables_interpolated %>%
        left_join(df_interpolated_UK_LifeTable1719, by = c("Age" = "age", "Gender" = "sex")) %>%
        mutate(`median YLL` = round(interpolated_ex - median, 2) ,`YLL 2.5% CrI` = round(interpolated_ex - `2.5%`, 2)  ,`YLL 97.5% CrI` = round(interpolated_ex - `97.5%`, 2)) %>%
        unite(YLL_CrI_temp, c(`YLL 2.5% CrI`, `YLL 97.5% CrI`), sep = " to ", remove = FALSE) %>%
        mutate(YLL_CrI = paste0("(", YLL_CrI_temp, ")")) %>%
        unite(YLL_median_CrI, c(`median YLL`, YLL_CrI), sep = " ", remove = FALSE) %>%
        select(Gender:`cfDNA status`, YLL_median_CrI, `median YLL`, `YLL 2.5% CrI`, `YLL 97.5% CrI`)

addWorksheet(excel_output, sheetName = "2.1 YLL no HR")
addWorksheet(excel_output, sheetName = "2.2 YLL 1.5 HR")
addWorksheet(excel_output, sheetName = "2.3 YLL 3 HR")
writeData(excel_output, sheet = "2.1 YLL no HR", res_YLL_tables_all_full %>% filter(HR == "1"))
writeData(excel_output, sheet = "2.2 YLL 1.5 HR", res_YLL_tables_all_full %>% filter(HR == "1andhalf"))
writeData(excel_output, sheet = "2.3 YLL 3 HR", res_YLL_tables_all_full %>% filter(HR == "3"))

plot_YLL_female_noHR_70 <- res_YLL_tables_all_full %>%
        # filter(Gender == "Female" & Age %in% c("50", "60", "70") & HR == "1") %>%
        # mutate(Age_string = case_when(Age == "50" ~ "50-54 years at diagnosis",
        #                               Age == "60" ~ "60-64 years at diagnosis",
        #                               Age == "70" ~ "70-74 years at diagnosis")) %>%
        filter(Gender == "Female" & Age %in% c( "70-74") & HR == "1") %>%
        filter(HR == "1" & `cfDNA status` == "negative") %>%
        ggplot() +
        geom_bar(aes(x = Stage, y = `median YLL`, group = Stage, fill = Stage), position= position_dodge(0.5), stat = "identity", width = .5) +
        geom_errorbar(aes(x = Stage, ymin = `YLL 97.5% CrI`, ymax = `YLL 2.5% CrI`, group = Stage), colour = "black", position= position_dodge(0.5), width=.1) +
        facet_wrap(facets = vars(`Cancer type`), ncol = 5) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592")) +
        labs(colour = "Stage", x = "Stage at diagnosis", y = "Years of Life Lost", title = "Females") +
        guides(fill="none", colour = "none")+
        theme_bw() +
        theme(plot.title =element_text(face = "bold", size = 12),
              strip.background = element_blank(),
              strip.text = element_text(face = "bold", size = 10),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.y = element_text(face = "bold", size = 10),
              axis.title = element_text(face = "bold", size = 12)) +
        scale_y_continuous(limits = c(0,20))

plot_YLL_male_noHR_70 <- res_YLL_tables_all_full %>%
        # filter(Gender == "Female" & Age %in% c("50", "60", "70") & HR == "1") %>%
        # mutate(Age_string = case_when(Age == "50" ~ "50-54 years at diagnosis",
        #                               Age == "60" ~ "60-64 years at diagnosis",
        #                               Age == "70" ~ "70-74 years at diagnosis")) %>%
  filter(Gender == "Male" & Age %in% c( "70-74") & HR == "1") %>%
  filter(HR == "1" & `cfDNA status` == "negative") %>%
        ggplot() +
        geom_bar(aes(x = Stage, y = `median YLL`, group = Stage, fill = Stage), position= position_dodge(0.5), stat = "identity", width = .5) +
        geom_errorbar(aes(x = Stage, ymin = `YLL 97.5% CrI`, ymax = `YLL 2.5% CrI`, group = Stage), colour = "black", position= position_dodge(0.5), width=.1) +
        facet_wrap(facets = vars(`Cancer type`), ncol = 5) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592")) +
        labs(colour = "Stage", x = "Stage at diagnosis", y = "", title = "Males") +
        guides(fill="none", colour = "none")+
        theme_bw() +
        theme(plot.title =element_text(face = "bold", size = 12),
              strip.background = element_blank(),
              strip.text = element_text(face = "bold", size = 10),
              axis.text.y = element_text(face = "bold", size = 10),
              axis.text.x = element_text(face = "bold", size = 10),
              axis.title = element_text(face = "bold", size = 12)) +
        scale_y_continuous(limits = c(0,20))

patchwork::wrap_plots(plot_YLL_female_noHR_70, plot_YLL_male_noHR_70, ncol = 1)


res_YLL_table_Stage4v1_4v3_3v2_2v1_70 <- res_YLL_tables_all_full %>%
        filter(Age %in% c( "70-74") & HR == "1" & `cfDNA status` == "negative") %>%
        select(Gender, `Cancer type`, Age, Stage, `median YLL`)%>%
        pivot_wider(names_from = Stage, values_from = `median YLL`) %>%
        mutate(`IV v III` = `IV` - `III`) %>%
        mutate(`III v II` = `III` - `II`) %>%
        mutate(`II v I` = `II` - `I`) %>%
        select(Gender, `Cancer type`, Age,
               `IV v III`,`III v II`, `II v I`) %>%
        mutate(highest = case_when((`IV v III` > `III v II`) & (`IV v III` > `II v I`) ~ "4v3 Highest",
                                   (`III v II` > `IV v III`) & (`III v II`  > `II v I`) ~ "3v2 Highest",
                                   (`II v I` > `IV v III`) &  (`II v I` > `III v II`) ~ "2v1 Highest",
                                   .default = NA), .after = Age) %>%
        pivot_longer(!Gender:highest, names_to = "Stage comparison", values_to = "Difference in years of life lost") %>%
        mutate(highest = case_when(highest == "4v3 Highest" & `Stage comparison` == "IV v III" ~ "Yes",
                                   highest == "3v2 Highest" & `Stage comparison` == "III v II" ~ "Yes",
                                   highest == "2v1 Highest" & `Stage comparison` == "II v I" ~ "Yes",
                                   .default = "No"), .after = Age)

plot_YLL_Stage4v3_3v2_2v1_noHR_70 <-  res_YLL_table_Stage4v1_4v3_3v2_2v1_70 %>%
        mutate(`Cancer type` = fct_rev(`Cancer type`)) %>%
        ggplot() +
        # ordered alphabetically
        geom_bar(aes(x = `Cancer type`, y = `Difference in years of life lost`, fill = highest),
                 position= position_dodge(0.5), stat = "identity", width = .5) +
        # ordered by differrence
        # geom_bar(aes(x = fct_relevel(`Cancer site`, factor_order_female_4v3),
        #              y = `Difference in loss of life expectancy (years)`), position= position_dodge(0.5), stat = "identity", width = .5) +
        facet_grid(Gender ~ fct_rev(`Stage comparison`), scales = "free_y") +
        tidytext::scale_x_reordered() +
        coord_flip() +
        ggsci::scale_color_jama() +
        labs( x = "Cancer type", y = "Difference in years of life lost") +
        theme(axis.title = element_text(face = "bold", size = 17)) +
        theme_bw(base_size = 14) +
        scale_fill_manual(values = c( "#D29A22", "#3B1C52")) +
        scale_y_continuous(breaks = c(-2,0,2,4,6,8,10, 12), limits = c(-2,12)) +
        guides(fill="none", colour = "none")

#' _____________________________________________________________________________
#' _____ Aggregate years of life lost

#' Script 01 needs to have been run first for the df_surv_byHR_CF dataframe
aggregate_YLL<- df_surv_byHR_CF %>%
        filter(start == 0 & cfDNA_status == "negative", haz_ratio == 1) %>%
        filter(!((Sex == "Female" & NCRAS_Draw == "Prostate") | (Sex == "Male" & grepl(c("Cervix|Breast|Ovary|Uterus"), NCRAS_Draw)))) %>%
        select(Age:start, n_patients) %>%
        left_join(res_YLL_tables_all_full %>%
                          filter( HR == 1 & `cfDNA status` == "negative"),
                  by = c("Age" = "Age", "Sex" = "Gender", "NCRAS_Draw" = "Cancer type", "Stage")) %>%
        mutate(`Total YLL` = n_patients * `median YLL`) %>%
        group_by(NCRAS_Draw, Stage) %>%
        summarise(`Total YLL` = sum(`Total YLL`, na.rm = TRUE)) %>%
        ungroup()

# Total incidenct cancers
res_total_cancer <- df_surv_byHR_CF %>%
  filter(start == 0 & cfDNA_status == "negative", haz_ratio == 1) %>%
  filter(!((Sex == "Female" & NCRAS_Draw == "Prostate") | (Sex == "Male" & grepl(c("Cervix|Breast|Ovary|Uterus"), NCRAS_Draw)))) %>%
  select(Age:start, n_patients) %>%
  summarise(total_patient = sum(n_patients))

# Total years of life lost
res_total_YLL<- aggregate_YLL %>% summarise(`Total YLL` = sum(`Total YLL`))

        # Percentage YLL by cancer type
res_perc_YLL_bytype <- aggregate_YLL %>%
     group_by(NCRAS_Draw) %>%
     summarise(`Total YLL by type` = sum(`Total YLL`, na.rm = TRUE)) %>%
     mutate(freq = paste0("(",round(`Total YLL by type` / sum(`Total YLL by type`) * 100, 1), "%", ")"))
# Percentage YLL by cancer type
res_perc_YLL_bystage  <- aggregate_YLL %>%
        group_by(Stage) %>%
        summarise(`Total YLL by stage` = sum(`Total YLL`, na.rm = TRUE)) %>%
        mutate(freq = paste0("(",round(`Total YLL by stage` / sum(`Total YLL by stage`) * 100, 0), "%", ")"))

#' Mosaic plot
factor_site_mosaic <- aggregate_YLL %>%
        left_join(res_perc_YLL_bytype  %>% select(-`Total YLL by type`), by = c("NCRAS_Draw")) %>%
        unite(`Cancer type (%)`, c(NCRAS_Draw, freq), sep = " ", remove = FALSE) %>%
        group_by(`Cancer type (%)`,NCRAS_Draw) %>%
        summarise(`Total YLL by type` = sum(`Total YLL`, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(NCRAS_Draw = as.factor(NCRAS_Draw)) %>%
        mutate(NCRAS_Draw = fct_reorder(NCRAS_Draw, (`Total YLL by type`))) %>%
        mutate(`Cancer type (%)` = as.factor(`Cancer type (%)`)) %>%
        mutate(`Cancer type (%)` = fct_reorder(`Cancer type (%)`, (`Total YLL by type`))) %>%
        arrange(desc(`Cancer type (%)`)) %>%
        select(`Cancer type (%)`)


# 800*600
library(ggmosaic)
plot_mosaic_LOLE <- aggregate_YLL %>%
        left_join(res_perc_YLL_bytype %>% select(-`Total YLL by type`) , by = c("NCRAS_Draw")) %>%
        unite(`Cancer type (%)`, c(NCRAS_Draw, freq), sep = " ", remove = FALSE) %>%
  mutate(test = fct_rev(fct_relevel(`Cancer type (%)`, as.character(pull(factor_site_mosaic))))) %>%
        ggplot() +
        ggmosaic::geom_mosaic(aes(x = product( Stage, test), weight = `Total YLL`, fill = Stage), offset = 0.001) +
        scale_x_productlist(position = "bottom", expand = c(0, 0) , labels = c("","","", "","","",rev(as.character(pull(factor_site_mosaic[1:15,]))))) +
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


