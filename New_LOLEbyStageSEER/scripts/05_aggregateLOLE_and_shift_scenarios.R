#______________________________________________________________________________#
#____ 05a_shiftingIVtoIII_M_spline

#' Uses a Bayesian flexible parametric survival model, based on an M-spline
#' to extrapolate the SEER individual patient data 

#______________________________________________________________________________#
#_________ 3.4. Aggregate LOLE
# Merge The results by type, stage, age-band and sex on to TTE_data_cured

aggregate_LOLE <- TTE_data_curefrac %>%
        left_join(res_LOLE_tables_all_full, by = c("Age_lower", "Sex", "SEER_Draw", "AJCC_stage")) %>%
        group_by(SEER_Draw, AJCC_stage) %>%
        summarise(totalLOLE = sum(LOLE_median)) %>%
        ungroup() %>%
        mutate(across(where(is.character), as.factor))

# Total years of life lost
res_total_LOLE <- aggregate_LOLE %>% summarise(totalLOLEall = sum(totalLOLE))

# Percentage YLL by cancer type
res_perc_LOLE_bytype <- aggregate_LOLE %>% 
        group_by(SEER_Draw) %>%
        summarise(totalLOLEall = sum(totalLOLE, na.rm = TRUE)) %>%
        mutate(freq = paste0("(",round(totalLOLEall / sum(totalLOLEall) * 100, 1), "%", ")")) 

# Percentage YLL by cancer type
res_perc_LOLE_bystage  <- aggregate_LOLE %>% 
        group_by(AJCC_stage) %>%
        summarise(totalLOLEall = sum(totalLOLE, na.rm = TRUE)) %>%
        mutate(freq = paste0("(",round(totalLOLEall / sum(totalLOLEall) * 100, 0), "%", ")")) 

#' Mosaic plot
factor_site_mosaic <- as.character(aggregate_LOLE %>%
                                           left_join(res_perc_LOLE_bytype %>% select(-totalLOLEall) , by = c("SEER_Draw")) %>%
                                           unite(SEER_Draw, c(SEER_Draw, freq), sep = " ", remove = TRUE) %>%
                                           group_by(SEER_Draw) %>%
                                           summarise(totalLOLESsite = sum(totalLOLE)) %>%
                                           left_join(res_perc_LOLE_bytype %>% select(-totalLOLEall) , by = c("SEER_Draw")) %>%
                                           mutate(SEER_Draw = as.factor(SEER_Draw)) %>%
                                           mutate(SEER_Draw = fct_reorder(SEER_Draw, (totalLOLESsite))) %>%
                                           arrange(SEER_Draw) %>%
                                           select(SEER_Draw) %>%
                                           distinct() %>%
                                           pull(SEER_Draw))

#' Do factors here for Fig 2 of EDCC so they're the same order
factor_site_Fig2 <- as.character(aggregate_LOLE %>%
                                           left_join(res_perc_LOLE_bytype %>% select(-totalLOLEall) , by = c("SEER_Draw")) %>%
                                           group_by(SEER_Draw) %>%
                                           summarise(totalLOLESsite = sum(totalLOLE)) %>%
                                           left_join(res_perc_LOLE_bytype %>% select(-totalLOLEall) , by = c("SEER_Draw")) %>%
                                           mutate(SEER_Draw = as.factor(SEER_Draw)) %>%
                                           mutate(SEER_Draw = fct_reorder(SEER_Draw, (totalLOLESsite))) %>%
                                           arrange(SEER_Draw) %>%
                                           select(SEER_Draw) %>%
                                           distinct() %>%
                                           pull(SEER_Draw))


# 800*600
plot_mosaic_LOLE <- aggregate_LOLE %>%
        # Ordered alphabetically
        left_join(res_perc_LOLE_bytype %>% select(-totalLOLEall) , by = c("SEER_Draw")) %>%
        unite(SEER_Draw, c(SEER_Draw, freq), sep = " ", remove = TRUE) %>%
        # mutate(SEER_Draw = fct_rev(SEER_Draw)) %>%
        # Ordered by size
        mutate(SEER_Draw = as.factor(SEER_Draw)) %>%
        mutate(SEER_Draw = fct_relevel(SEER_Draw, factor_site_mosaic)) %>%
        # left_join(res_perc_LOLE_bystage %>% select(-totalLOLEall) , by = c("AJCC_stage")) %>%
        # unite(AJCC_stage, c(AJCC_stage, freq), sep = " ", remove = TRUE) %>%
        ggplot() +
        geom_mosaic(aes(x = product( AJCC_stage, SEER_Draw,), weight = totalLOLE, fill = AJCC_stage), offset = 0.001) +
        scale_x_productlist(position = "bottom", expand = c(0, 0)) +
        scale_y_productlist(expand = c(0, 0)) +
        theme_bw(base_size = 10) +
        theme(panel.grid = element_blank(),
              legend.position = "bottom",
              axis.text.x=element_blank(), 
              axis.ticks.x=element_blank())+
        ggsci::scale_fill_jama() +
        labs(fill = "Stage",
             x = "",
             #ç"Percentage of years of life lost attributable to each cancer type",
             #The proportion of total YLL by index cancer (total proportion across all stages for given cancer)",
             y = "") +
        #"Percentage of years of life lost attributable to stage for each cancer type"
             #The proportion of total YLL by cancer stage for each cancer (total proportion across all cancers for a given stage)") +
        #guides(y = guide_axis(n.dodge = 2)) +
        coord_flip() 

ggsave("~/LOLEbyStageSEER/results/mcmc/tables_and_figures/Fig5.png", plot_mosaic_LOLE, device = "png",
       width = 175,
       height = 250,
       units = "mm")

#______________________________________________________________________________#
#_________ Scenario 1: All Stage 4 to 3 
res_totalLOLE <- aggregate_LOLE %>% 
        summarise(totalLOLE = sum(totalLOLE))

res_totalLOLE_4to3 <- TTE_data_curefrac %>%
        mutate(AJCC_stage = case_when(AJCC_stage == "IV" ~ "III",
                                      .default = AJCC_stage)) %>%
        left_join(res_LOLE_tables_all_full, by = c("Age_lower", "Sex", "SEER_Draw", "AJCC_stage")) %>%
        group_by(SEER_Draw, AJCC_stage) %>%
        summarise(totalLOLE = sum(LOLE_median)) %>%
        ungroup() %>%
        summarise(totalLOLEIII = sum(totalLOLE))

res_4to3_diff <- res_totalLOLE - res_totalLOLE_4to3 
#res_4to3_diff / res_totalLOLE

#______________________________________________________________________________#
#_________ Scenario 2: All Stage 4 split so that 33.3% goes to III, II, and I
# Shift is randomly sampled 

res_totalLOLE <- aggregate_LOLE %>% 
        summarise(totalLOLE = sum(totalLOLE))

res_totalLOLE_4onethirdsplit <- TTE_data_curefrac %>%
        mutate(AJCC_stage = case_when(AJCC_stage == "IV" ~ sample(c("III", "II", "I"), size = n(),
                                                                  replace = TRUE, prob = c((1/3), (1/3), (1/3))),
                                      .default = AJCC_stage)) %>%
        left_join(res_LOLE_tables_all_full, by = c("Age_lower", "Sex", "SEER_Draw", "AJCC_stage")) %>%
        group_by(SEER_Draw, AJCC_stage) %>%
        summarise(totalLOLE = sum(LOLE_median)) %>%
        ungroup() %>%
        summarise(totalLOLE = sum(totalLOLE))

res_4onethirdsplit_diff <- res_totalLOLE - res_totalLOLE_4onethirdsplit 
#res_4onethirdsplit_diff / res_totalLOLE

        
#______________________________________________________________________________#
#_________ Scenario 3: Stage IV and III cancers were shifted to stage II

res_totalLOLE_4and3to2 <- TTE_data_curefrac %>%
        mutate(AJCC_stage = case_when(AJCC_stage == "IV" ~ "II",
                                      AJCC_stage == "III" ~ "II",
                                      .default = AJCC_stage)) %>%
        left_join(res_LOLE_tables_all_full, by = c("Age_lower", "Sex", "SEER_Draw", "AJCC_stage")) %>%
        group_by(SEER_Draw, AJCC_stage) %>%
        summarise(totalLOLE = sum(LOLE_median)) %>%
        ungroup() %>%
        summarise(totalLOLE = sum(totalLOLE))

res_4and3to2 <- res_totalLOLE - res_totalLOLE_4and3to2 
#res_4and3to2 / res_totalLOLE

#______________________________________________________________________________#








