#' _____________________________________________________________________________
#' _____ 04_LE_and_YLL

#' This script saves the life exptancy results, estimates and saves years
#' of life lost and creates plots of YLL

#' Currently, it assumed all the models have run. If there are patient presentations
#' where the model errors (e.g. due to non convergence), post processing such as
#' interpolation or changes to the categories such as collapsing age bands may be needed

#' _____________________________________________________________________________
#' _____ 1.  Paths and create an excel document for all of the tabular results

#' Path names of primary analysis outcomes
res_save_path_primary <- paste0(getwd(), "/results/primary_raw_tables/")
res_meth_name_primary <- "opt_nocure_relsurv"

#' Path names of sensitivity analysis outcomes
res_save_path_sens <- paste0(getwd(), "/results/sensitivity_raw_tables/")
res_meth_name_sens <- "opt_cure_relsurv"

library(openxlsx)
results_path <- paste0(getwd(), "/results/results_tables/")
figure_path <- paste0(getwd(), "/results/figures/")
excel_output <- createWorkbook()

#' _____________________________________________________________________________
#' _____ 2. Life Expectancy

#' Load in per cancer tables and merge
res_LE_tables_all_primary<- list.files(path = res_save_path_primary, pattern = paste0( res_meth_name_primary, "_LE")) %>%
        map_df(~read_csv(paste0(res_save_path_primary, .))) %>%
  # Remove the cancer type-sex incompatible rows
  filter(!(Sex == "Male" & NCRAS_Draw == "Cervix")) %>%
  filter(!(Sex == "Male" & NCRAS_Draw == "Breast")) %>%
  filter(!(Sex == "Male" & NCRAS_Draw == "Ovary")) %>%
  filter(!(Sex == "Male" & NCRAS_Draw == "Uterus")) %>%
  filter(!(Sex == "Female" & NCRAS_Draw == "Prostate")) %>%
  rename(`Cancer type` = "NCRAS_Draw",
         `cfDNA status` = "cfDNA_status",
         HR = haz_ratio)

res_LE_tables_all_sens<- list.files(path = res_save_path_sens, pattern = paste0( res_meth_name_sens, "_LE")) %>%
  map_df(~read_csv(paste0(res_save_path_sens, .))) %>%
  # Remove the cancer type-sex incompatible rows
  filter(!(Sex == "Male" & NCRAS_Draw == "Cervix")) %>%
  filter(!(Sex == "Male" & NCRAS_Draw == "Breast")) %>%
  filter(!(Sex == "Male" & NCRAS_Draw == "Ovary")) %>%
  filter(!(Sex == "Male" & NCRAS_Draw == "Uterus")) %>%
  filter(!(Sex == "Female" & NCRAS_Draw == "Prostate")) %>%
  rename(`Cancer type` = "NCRAS_Draw",
         `cfDNA status` = "cfDNA_status",
         HR = haz_ratio)

#' Save by hazard ratio
addWorksheet(excel_output, sheetName = "1.1 Primary LE no HR")
addWorksheet(excel_output, sheetName = "1.2 Primary LE 2 HR")
addWorksheet(excel_output, sheetName = "1.3 Sensitivity LE no HR")
addWorksheet(excel_output, sheetName = "1.4 Sensitivity LE 2 HR")
writeData(excel_output, sheet = "1.1 Primary LE no HR", res_LE_tables_all_primary %>% filter(HR == "1"))
writeData(excel_output, sheet = "1.2 Primary LE 2 HR", res_LE_tables_all_primary %>% filter(HR == "2"))
writeData(excel_output, sheet = "1.3 Sensitivity LE no HR", res_LE_tables_all_sens %>% filter(HR == "1"))
writeData(excel_output, sheet = "1.4 Sensitivity LE 2 HR", res_LE_tables_all_sens %>% filter(HR == "2"))

#' _____________________________________________________________________________
#' _____ 3. Compare primary specification (no cure) to sensitivity (cure)


#' Assuming here that the sensitivity was run only for Anal to colorecta;
#' Filter the primary results table to just these cancer types and combine with
#' the sensitivity table
df_LE_primary_sens_compare <- bind_rows(res_LE_tables_all_primary %>%
                                          filter(`Cancer type` %in% levels(as.factor(res_LE_tables_all_sens$`Cancer type`))) %>%
                                          mutate(`Analysis type` = "Primary"),
                                        res_LE_tables_all_sens %>%
                                          mutate(`Analysis type` = "Sensitivity")
                                        )
#' Plot of the absolute life expectancy. Graph needs to be filter down to a specific sex,
#' HR and cfDNA status to have a good look.
fig_LE_primary_sens_compare_female_HR1 <- df_LE_primary_sens_compare %>%
  filter(HR == 1) %>%
  filter(Sex == "Female") %>%
        ggplot() +
        geom_bar(aes(x = Stage, y = median, group = `Analysis type`, fill = `Analysis type`),
                 position= position_dodge(0.5), stat = "identity", width = .5) +
        geom_errorbar(aes(x = Stage,  ymin = `2.5%`, ymax = `97.5%`, group =`Analysis type`),
                      position= position_dodge(0.5), width=.1) +
        facet_grid(rows = vars(`Cancer type`), cols = vars(Age),  scales = "fixed") +
        scale_fill_manual(values = c( "#3B1C52","#D29A22")) +
        labs(fill = "Analysis type", x = "Stage", y = "Life expectancy") +
        theme_bw() +
        theme(strip.text.y.right = element_text(angle = 0),
              text = element_text(size = 16, face="bold"))

#' Plot of the difference
#' In the US SEER analysis, the the mixture cure specification typically increased the life expectancy
#' of higher stages at the lower age bands. It has little effect older ages,
#' the below graph is set up to show this - but this many not come out in the code
#' review using the "opt" method and low iterations
fig_LE_primary_sens_compare_female_HR1_differences <- df_LE_primary_sens_compare %>%
  filter(`cfDNA status` == "negative") %>%
  filter(HR == 1) %>%
  filter(Sex == "Female") %>%
  select(Sex, `Cancer type`, Stage, Age, HR, `cfDNA status`, median,`Analysis type`) %>%
  pivot_wider(names_from = "Analysis type", values_from = "median") %>%
  mutate(difference = Primary - Sensitivity) %>%
  ggplot() +
  geom_bar(aes(x = Stage, y = difference,
               group = Stage, fill = Stage),
           position= position_dodge(0.5), stat = "identity", width = .5) +
  facet_grid(rows  = vars(`Cancer type`), cols = vars(Age),  scales = "fixed") +
  scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592")) +
  labs(fill = NULL, x = "Stage", y = "Difference in Life expectancy") +
  theme_bw() +
  theme(strip.text.y.right = element_text(angle = 0),
        text = element_text(size = 16, face="bold"),
        legend.position = "none")


#' _____________________________________________________________________________
#' _____ 4. Estimate years of life lost

#' Join on general population life expectancy to the Life expectany tables
#' General population life expectancy assumed to be the mid point of the age band,
#' making use of the interpolation performed in the 01 script.

res_YLL_tables_all_primary<- res_LE_tables_all_primary %>%
  # Get mid year age
  mutate(Age_lower = as.numeric(substr(Age, 1, 2)),
         Age_mid = Age_lower + 2.5) %>%
  left_join(df_gen_pop_ex, by = c("Age_mid" = "age", "Sex" = "sex")) %>%
  mutate(`median YLL` = round(ex - median, 2) ,`YLL 2.5% CrI` = round(ex - `2.5%`, 2)  ,`YLL 97.5% CrI` = round(ex - `97.5%`, 2)) %>%
  unite(YLL_CrI_temp, c(`YLL 2.5% CrI`, `YLL 97.5% CrI`), sep = " to ", remove = FALSE) %>%
  mutate(YLL_CrI = paste0("(", YLL_CrI_temp, ")")) %>%
  unite(YLL_median_CrI, c(`median YLL`, YLL_CrI), sep = " ", remove = FALSE) %>%
  select(Sex:`cfDNA status`, YLL_median_CrI, `median YLL`, `YLL 2.5% CrI`, `YLL 97.5% CrI`)

res_YLL_tables_all_sens<- res_LE_tables_all_sens %>%
  # Get mid year age
  mutate(Age_lower = as.numeric(substr(Age, 1, 2)),
         Age_mid = Age_lower + 2.5) %>%
  left_join(df_gen_pop_ex, by = c("Age_mid" = "age", "Sex" = "sex")) %>%
  mutate(`median YLL` = round(ex - median, 2) ,`YLL 2.5% CrI` = round(ex - `2.5%`, 2)  ,`YLL 97.5% CrI` = round(ex - `97.5%`, 2)) %>%
  unite(YLL_CrI_temp, c(`YLL 2.5% CrI`, `YLL 97.5% CrI`), sep = " to ", remove = FALSE) %>%
  mutate(YLL_CrI = paste0("(", YLL_CrI_temp, ")")) %>%
  unite(YLL_median_CrI, c(`median YLL`, YLL_CrI), sep = " ", remove = FALSE) %>%
  select(Sex:`cfDNA status`, YLL_median_CrI, `median YLL`, `YLL 2.5% CrI`, `YLL 97.5% CrI`)

addWorksheet(excel_output, sheetName = "2.1 Primary YLL no HR")
addWorksheet(excel_output, sheetName = "2.2 Primary YLL 2 HR")
addWorksheet(excel_output, sheetName = "2.3 Sensitivity YLL no HR")
addWorksheet(excel_output, sheetName = "2.4 Sensitivity YLL 2 HR")
writeData(excel_output, sheet = "2.1 Primary YLL no HR", res_YLL_tables_all_primary %>% filter(HR == "1"))
writeData(excel_output, sheet = "2.2 Primary YLL 2 HR", res_YLL_tables_all_primary %>% filter(HR == "2"))
writeData(excel_output, sheet = "2.3 Sensitivity YLL no HR", res_YLL_tables_all_sens %>% filter(HR == "1"))
writeData(excel_output, sheet = "2.4 Sensitivity YLL 2 HR", res_YLL_tables_all_sens %>% filter(HR == "2"))

#' Save the results workbook
saveWorkbook(excel_output, paste0(results_path, "results_workbook"), overwrite = TRUE)

#' _____________________________________________________________________________
#' _____ 5. Plot years of life lost for potential future publication

#' Can ignore all of the below for LE code review at this point
#' Below does some YLL graphing and then a bit on aggregate YLL


#' Plot YLL separately for each age group
for(var in c("50-54","55-59","60-64","65-69","70-74","75-79")) {

  temp1 <- res_YLL_tables_all_primary %>%
    filter(Sex == "Female" & Age %in% c(var) & HR == "1") %>%
    filter(HR == "1" & `cfDNA status` == "negative") %>%
    ggplot() +
    geom_bar(aes(x = Stage, y = `median YLL`, group = Stage, fill = Stage), position= position_dodge(0.5), stat = "identity", width = .5) +
    geom_errorbar(aes(x = Stage, ymin = `YLL 97.5% CrI`, ymax = `YLL 2.5% CrI`, group = Stage), colour = "black", position= position_dodge(0.5), width=.1) +
    facet_wrap(facets = vars(`Cancer type`), ncol = 4) +
    scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592")) +
    labs(colour = "Stage", x = "Stage at diagnosis", y = "Years of Life Lost", title = "Females") +
    guides(fill="none", colour = "none")+
    theme_bw() +
    theme(plot.title =element_text(face = "bold", size = 14),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 10),
          axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_text(face = "bold", size = 12),
          axis.title = element_text(face = "bold", size = 12)) +
    scale_y_continuous(limits = c(0,40))

  temp2 <- res_YLL_tables_all_primary %>%
    filter(Sex == "Male" & Age %in% c( var) & HR == "1") %>%
    filter(HR == "1" & `cfDNA status` == "negative") %>%
    ggplot() +
    geom_bar(aes(x = Stage, y = `median YLL`, group = Stage, fill = Stage), position= position_dodge(0.5), stat = "identity", width = .5) +
    geom_errorbar(aes(x = Stage, ymin = `YLL 97.5% CrI`, ymax = `YLL 2.5% CrI`, group = Stage), colour = "black", position= position_dodge(0.5), width=.1) +
    facet_wrap(facets = vars(`Cancer type`), ncol = 4) +
    scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592")) +
    labs(colour = "Stage", x = "Stage at diagnosis", y = "", title = "Males") +
    guides(fill="none", colour = "none")+
    theme_bw() +
    theme(plot.title =element_text(face = "bold", size = 14),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 10),
          axis.text.y = element_text(face = "bold", size = 12),
          axis.text.x = element_text(face = "bold", size = 12),
          axis.title = element_text(face = "bold", size = 12)) +
    scale_y_continuous(limits = c(0,40))

  ggsave(paste0(figure_path, "YLLplotNoHR_", var, ".png"),
         patchwork::wrap_plots(temp1, temp2, ncol = 2), device = "png",
         width = 300,
         height = 200,
         units = "mm")

}

#' Create dataframe and Plot YLL differences between stages separately for each age group
res_YLL_table_Stage4v1_4v3_3v2_2v1 <- res_YLL_tables_all_primary %>%
  filter(HR == "1" & `cfDNA status` == "negative") %>%
  select(Sex, `Cancer type`, Age, Stage, `median YLL`)%>%
  pivot_wider(names_from = Stage, values_from = `median YLL`) %>%
  mutate(`IV v III` = `IV` - `III`) %>%
  mutate(`III v II` = `III` - `II`) %>%
  mutate(`II v I` = `II` - `I`) %>%
  select(Sex, `Cancer type`, Age,
         `IV v III`,`III v II`, `II v I`) %>%
  mutate(highest = case_when((`IV v III` > `III v II`) & (`IV v III` > `II v I`) ~ "4v3 Highest",
                             (`III v II` > `IV v III`) & (`III v II`  > `II v I`) ~ "3v2 Highest",
                             (`II v I` > `IV v III`) &  (`II v I` > `III v II`) ~ "2v1 Highest",
                             .default = NA), .after = Age) %>%
  pivot_longer(!Sex:highest, names_to = "Stage comparison", values_to = "Difference in years of life lost") %>%
  mutate(highest = case_when(highest == "4v3 Highest" & `Stage comparison` == "IV v III" ~ "Yes",
                             highest == "3v2 Highest" & `Stage comparison` == "III v II" ~ "Yes",
                             highest == "2v1 Highest" & `Stage comparison` == "II v I" ~ "Yes",
                             .default = "No"), .after = Age)

for(var in c("50-54","55-59","60-64","65-69","70-74","75-79")) {

  temp1 <- res_YLL_table_Stage4v1_4v3_3v2_2v1 %>%
    filter(Age %in% c(var) ) %>%
    mutate(`Cancer type` = fct_rev(`Cancer type`)) %>%
    ggplot() +
    # ordered alphabetically
    geom_bar(aes(x = `Cancer type`, y = `Difference in years of life lost`, fill = highest),
             position= position_dodge(0.5), stat = "identity", width = .5) +
    facet_grid(Sex ~ fct_rev(`Stage comparison`), scales = "free_y") +
    tidytext::scale_x_reordered() +
    coord_flip() +
    ggsci::scale_color_jama() +
    labs( x = "Cancer type", y = "Difference in years of life lost") +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 14),
          axis.text.y = element_text( size = 14),
          axis.text.x = element_text( size = 14),
          axis.title = element_text(face = "bold", size = 14)) +
    scale_fill_manual(values = c( "#D29A22", "#3B1C52")) +
    scale_y_continuous(breaks = c(-2,0,2,4,6,8,10,12,14,16,18,20), limits = c(-2,20)) +
    guides(fill="none", colour = "none")

  ggsave(paste0(figure_path, "Differences_plot_", var, ".png"),
         temp1,
         device = "png",
         width = 300,
         height = 200,
         units = "mm")
}


#' _____________________________________________________________________________
#' _____ 6. Calculate and plot aggregate years of life lost

aggregate_YLL<- df_surv_byHR %>%
        filter(start == 0 & cfDNA_status == "negative", haz_ratio == 1) %>%
        filter(!((Sex == "Female" & NCRAS_Draw == "Prostate") | (Sex == "Male" & grepl(c("Cervix|Breast|Ovary|Uterus"), NCRAS_Draw)))) %>%
        select(Age:start, n_patients) %>%
        left_join(res_YLL_tables_all_primary %>%
                          filter( HR == 1 & `cfDNA status` == "negative"),
                  by = c( "Age", "Sex" , "NCRAS_Draw" = "Cancer type", "Stage")) %>%
        mutate(`Total YLL` = n_patients * `median YLL`) %>%
        group_by(NCRAS_Draw, Stage) %>%
        summarise(`Total YLL` = sum(`Total YLL`, na.rm = TRUE)) %>%
        ungroup()

# Total incidenct cancers
res_total_cancer <- df_surv_byHR %>%
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


