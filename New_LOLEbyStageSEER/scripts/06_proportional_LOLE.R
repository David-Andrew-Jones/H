#______________________________________________________________________________#
#____ 06_proportional_LOLE.R

#' Calculate PLOLE after script 04a

#______________________________________________________________________________#


res_PLOLE_tables_all_full <- res_LOLE_tables_all_full %>%
        mutate(PLOLE_median = round(LOLE_median/ex, 2)*100)

plot_PLOLE_female_table <- res_PLOLE_tables_all_full %>%
        filter(Sex == "Female") %>% 
        ggplot() +
        geom_point(aes(x = forcats::fct_rev(Age_string), y = PLOLE_median, color = Age_string),
                   position= position_dodge(0.5), stat = "identity") +
        facet_grid(rows = vars(SEER_Draw), cols = vars(AJCC_stage), switch = "y" , scales = "fixed") +
        ggsci::scale_color_jama() +
        labs(color = "Age at diagnosis", x = "Cancer type", y = "Proportional loss of life expectancy (%)") +
        theme_bw() +
        coord_flip() +
        theme(strip.text.y.left = element_text(angle = 0)) +
        theme(axis.text.y=element_blank(),
              axis.ticks.y=element_blank())

ggsave("~/LOLEbyStageSEER/results/mcmc/tables_and_figures/Fig4.png", plot_PLOLE_female_table, device = "png",
       width = 250,
       height = 300,
       units = "mm")


plot_PLOLE_male_table <- res_PLOLE_tables_all_full %>%
        filter(Sex == "Male") %>% 
        ggplot() +
        geom_point(aes(x = forcats::fct_rev(Age_string), y = PLOLE_median, color = Age_string),
                   position= position_dodge(0.5), stat = "identity") +
        facet_grid(rows = vars(SEER_Draw), cols = vars(AJCC_stage), switch = "y" , scales = "fixed") +
        ggsci::scale_color_jama() +
        labs(Fill = "Age at diagnosis", x = "Cancer type", y = "Proportional loss of life expectancy (%)") +
        theme_bw() +
        coord_flip() +
        theme(strip.text.y.left = element_text(angle = 0)) +
        theme(axis.text.y=element_blank(),
              axis.ticks.y=element_blank())

ggsave("~/LOLEbyStageSEER/results/mcmc/tables_and_figures/FigS1.png", plot_PLOLE_male_table, device = "png",
       width = 250,
       height = 300,
       units = "mm")