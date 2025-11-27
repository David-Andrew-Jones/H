
survival <- df_surv_byHR_CF %>% filter(NCRAS_Draw == "Lung") %>%
        filter(Age == "75-79") %>%
        filter(Sex == "Female") %>%
        filter(haz_ratio == "1") %>%
        filter(Stage == "I") %>%
        filter(cfDNA_status == "positive") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))


ggplot() +
        geom_step(data = survival,  mapping =aes(x= stop, y = CSS_adjusted), linewidth = 1) +
        theme_bw(base_size = 15)+
        ylab("Net survival probability")

get_age <- as.numeric(substr(survival$Age[1], start = 1, stop = 2)) + 3
get_sex <- as.character(survival$Sex[1])
get_bckgrnd_hz <- df_gen_pop_hz %>%
        filter(age >= get_age) %>%
        filter(sex == get_sex) %>%
        mutate(time = row_number() - 1) %>%
        select(time, hazard)

knotts <- NULL

surv_times <- bind_rows(data.frame(stop = 0, CSS_HR_adjusted = 1), survival %>%
                                select(stop, CSS_HR_adjusted)) %>%
        rename(tk = stop,
               sk = CSS_HR_adjusted) %>%
        mutate(k = row_number()) %>%
        select(k, tk, sk)

surv_nrisk <- survival %>%
        filter(start %in% c(0,1,2,3,4,4.75)) %>%
        rename(trisk = start,
               nrisk = n_patients) %>%
        mutate(i = row_number()) %>%
        select(i, trisk, nrisk ) %>%
        mutate(lower = c(1,9,13,17,21,25),
               upper = c(8,12,16,20,24,25)) %>%
        select(i, trisk, lower, upper, nrisk)

write.table(surv_times, file = "~/GalleriCEmodel/data-raw/NICE_modifier/surv_times.txt", sep = "\t",
            row.names = FALSE)

write.table(surv_nrisk, file = "~/GalleriCEmodel/data-raw/NICE_modifier/surv_nrisk.txt", sep = "\t",
            row.names = FALSE)

survHE::digitise(surv_inp = "~/GalleriCEmodel/data-raw/NICE_modifier/surv_times.txt" ,
                 nrisk_inp = "~/GalleriCEmodel/data-raw/NICE_modifier/surv_nrisk.txt",
                 km_output = "~/GalleriCEmodel/data-raw/NICE_modifier/surv_KMdata.txt",
                 ipd_output = "~/GalleriCEmodel/data-raw/NICE_modifier/surv_IPDdata.txt")

surv_IPDdata <- read.delim("~/GalleriCEmodel/data-raw/NICE_modifier/surv_IPDdata.txt")


library(ggfortify)

fit <- survfit(Surv(time, event) ~ 1, data=surv_IPDdata)
autoplot(fit)+
        theme_bw(base_size = 15)+
        ylab("Net survival probability") +
        geom_step(data = survival,  mapping =aes(x= stop, y = CSS_adjusted), linewidth = 1)

####### Compare with and without background hazard

mspline <- survextrap::mspline_spec(Surv(time, event) ~ 1, data = surv_IPDdata, add_knots = knotts)


model_bck_haz <- survextrap::survextrap(Surv(time, event) ~ 1, data=surv_IPDdata,
                                        mspline=mspline, backhaz = get_bckgrnd_hz,
                                        fit_method = "opt")


model <- survextrap::survextrap(Surv(time, event) ~ 1, data=surv_IPDdata,
                                mspline=mspline, fit_method = "opt")

plot_surv <- survextrap::plot_survival(model, tmax = 5, niter = 100)+
 survextrap::plot_survival(model_bck_haz, tmax = 5, niter = 100)















##############

plot_LE_female_noHR_70 <- res_LE_tables %>%
        filter(Gender == "Female" & Age %in% c( "70-74") & HR == "1") %>%
        filter(HR == "1" & `cfDNA status` == "negative") %>%
        ggplot() +
        geom_bar(aes(x = Stage, y = `median`, group = Stage, fill = Stage), position= position_dodge(0.5), stat = "identity", width = .5) +
        geom_errorbar(aes(x = Stage, ymin = `2.5%`, ymax = `97.5%`, group = Stage), colour = "black", position= position_dodge(0.5), width=.1) +
        facet_wrap(facets = vars(`Cancer type`), ncol = 5) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592")) +
        labs(colour = "Stage", x = "Stage at diagnosis", y = "Life expectancy", title = "Life expectancy") +
        guides(fill="none", colour = "none")+
        theme_bw() +
        theme(plot.title =element_text(face = "bold", size = 12),
              strip.background = element_blank(),
              strip.text = element_text(face = "bold", size = 10),

              axis.text.y = element_text(face = "bold", size = 10),
              axis.title = element_text(face = "bold", size = 12)) +
        scale_y_continuous(limits = c(0,20))


plot_YLL_female_noHR_70 <- res_YLL_tables_all_full %>%
        filter(Gender == "Female" & Age %in% c( "70-74") & HR == "1") %>%
        filter(HR == "1" & `cfDNA status` == "negative") %>%
        ggplot() +
        geom_bar(aes(x = Stage, y = `median YLL`, group = Stage, fill = Stage), position= position_dodge(0.5), stat = "identity", width = .5) +
        geom_errorbar(aes(x = Stage, ymin = `YLL 97.5% CrI`, ymax = `YLL 2.5% CrI`, group = Stage), colour = "black", position= position_dodge(0.5), width=.1) +
        facet_wrap(facets = vars(`Cancer type`), ncol = 5) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592")) +
        labs(colour = "Stage", x = "Stage at diagnosis", y = "Years of Life Lost", title = "Year of life lost") +
        guides(fill="none", colour = "none")+
        theme_bw() +
        theme(plot.title =element_text(face = "bold", size = 12),
              strip.background = element_blank(),
              strip.text = element_text(face = "bold", size = 10),
              axis.text.y = element_text(face = "bold", size = 10),
              axis.title = element_text(face = "bold", size = 12)) +
        scale_y_continuous(limits = c(0,20))

patchwork::wrap_plots(plot_LE_female_noHR_70, plot_YLL_female_noHR_70, nrow   = 1)




plot_age_female_noHR_70 <- res_YLL_tables_all_full %>%
        filter(Gender == "Female" & `Cancer type` %in% c( "Cervix") & HR == "1") %>%
        filter(HR == "1" & `cfDNA status` == "negative") %>%
        ggplot() +
        geom_bar(aes(x = Age, y = `median YLL`, group = Age, fill = Age), position= position_dodge(0.5), stat = "identity", width = .5) +
        geom_errorbar(aes(x = Age, ymin = `YLL 97.5% CrI`, ymax = `YLL 2.5% CrI`, group = Stage), colour = "black", position= position_dodge(0.5), width=.1) +
        facet_wrap(facets = vars(Stage), ncol = 5) +
        scale_fill_manual(values = c("#666666", "#D29A22", "#3B1C52", "#628592", "#546741", "#B73D27")) +
        labs(colour = "Stage", x = "Stage at diagnosis", y = "Years of Life Lost", title = "Year of life lost") +
        guides(fill="none", colour = "none")+
        theme_bw() +
        theme(plot.title =element_text(face = "bold", size = 14),
              strip.background = element_blank(),
              strip.text = element_text(face = "bold", size = 14),
              axis.text.y = element_text(face = "bold", size = 14),
              axis.title = element_text(face = "bold", size = 14)) +
        scale_y_continuous(limits = c(0,30))



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

