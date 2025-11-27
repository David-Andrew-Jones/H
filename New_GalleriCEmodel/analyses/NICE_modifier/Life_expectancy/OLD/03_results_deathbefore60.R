#______________________________________________________________________________#
#____ 03_results_deathbefore60


#______________________________________________________________________________#
#' Post-results processing
#' Load in per cancer tables and merge. plot of Life expectancy with CrI
#'
res_LE_tables_all_full <- list.files(path = "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/Death_60/", pattern = "^nocure_MCMC_LE") %>% # Defined in 01 script
        map(~read_csv(paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/Death_60/", .))) %>%
        bind_rows() %>%
        select(NCRAS_Draw, Age, Sex, Stage, median,CrI)
#______________________________________________________________________________#
#____ 03_results_deathbefore60


#______________________________________________________________________________#
#' Post-results processing
#' Load in per cancer tables and merge. plot of Life expectancy with CrI
#'
res_LE_tables_all_full <- list.files(path = "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/Death_60/", pattern = "^nocure_MCMC_LE") %>% # Defined in 01 script
        map(~read_csv(paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/Death_60/", .))) %>%
        bind_rows() %>%
        select(NCRAS_Draw, Age, Sex, Stage, median,CrI)
#______________________________________________________________________________#
#____ 03_results_deathbefore60


#______________________________________________________________________________#
#' Post-results processing
#' Load in per cancer tables and merge. plot of Life expectancy with CrI
#'
res_LE_tables_all_full <- list.files(path = "~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/Death_60/", pattern = "^nocure_MCMC_LE") %>% # Defined in 01 script
        map(~read_csv(paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/Death_60/", .))) %>%
        bind_rows() %>%
        select(NCRAS_Draw, Age, Sex, Stage, median, t) %>%
        rename(`Probability of surviving beyond specified number of years` = median,
               `Number of years` = t) %>%
        mutate(`Number of years` = case_when(`Number of years` == 1 ~ "1 year after diagnosis",
                                             `Number of years` == 5 ~ "5 years after diagnosis",
                                             .default = NA))

plot_1 <- res_LE_tables_all_full %>%
        #filter(Sex == "Male") %>%
        ggplot() +
        geom_line(aes(y = `Probability of surviving beyond specified number of years`, x = Age, group = NCRAS_Draw, colour = NCRAS_Draw)) +
        facet_grid(Sex ~ `Number of years`  , space= "free", scales="free") +
        labs(color = "Cancer type", x = "Age at diagnosis")+
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

plot_2 <- res_LE_tables_all_full %>%
        filter(Sex == "Male") %>%
        filter(`Number of years` == "5 years after diagnosis") %>%
        ggplot() +
        geom_col(aes(y = `Probability of surviving beyond specified number of years`, x = Age)) +
        facet_grid(vars( NCRAS_Draw), scales = "free") +
        labs(color = "Cancer type", x = "Age at diagnosis", y = "Probability of surviving beyond 5 years")+
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
