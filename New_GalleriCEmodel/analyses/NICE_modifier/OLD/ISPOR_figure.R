
# by age not by age (50-79)
# D average by 5 year age band (not weighted)


library(tidyverse)

df_QALYS_criteria <- read.csv("~/GalleriCEmodel/analyses/NICE_modifier/Excel model/Lung cancer results/QALY_Criteria_results.csv",
                              check.names = FALSE)


h_lines <- data.frame(Population_outcome = as.factor(c("Absolute QALY shortfall", "Absolute QALY shortfall", "Absolute QALY shortfall", "Absolute QALY shortfall",
                               "Absolute QALY shortfall", "Absolute QALY shortfall", "Absolute QALY shortfall", "Absolute QALY shortfall",
                            "Proportional QALY shortfall", "Proportional QALY shortfall", "Proportional QALY shortfall", "Proportional QALY shortfall",
                            "Proportional QALY shortfall", "Proportional QALY shortfall", "Proportional QALY shortfall", "Proportional QALY shortfall")),
                      Population_stage = as.factor(c("Stage I", "Stage II",
                                  "Stage III", "Stage IV")),
                      Z = c(12, 12, 12, 12,
                            18, 18, 18, 18,
                            0.85, 0.85, 0.85, 0.85,
                            0.95, 0.95, 0.95, 0.95))

h_text <- data.frame(Population_outcome = as.factor(c("Absolute QALY shortfall", "Absolute QALY shortfall", "Absolute QALY shortfall", "Absolute QALY shortfall",
                                                       "Absolute QALY shortfall", "Absolute QALY shortfall", "Absolute QALY shortfall", "Absolute QALY shortfall",
                                                       "Proportional QALY shortfall", "Proportional QALY shortfall", "Proportional QALY shortfall", "Proportional QALY shortfall",
                                                       "Proportional QALY shortfall", "Proportional QALY shortfall", "Proportional QALY shortfall", "Proportional QALY shortfall")),
                      Population_stage = as.factor(c("Stage I", "Stage II",
                                                     "Stage III", "Stage IV")),
                     lab = c("1.2 criteria", "", "", "",
                             "1.7 criteria", "", "", "",
                             "1.2 criteria", "", "", "",
                             "1.7 criteria", "", "", ""),
                      Z = c(11, 11, 11, 11,
                            17, 17, 17, 17,
                            0.81, 0.81, 0.81, 0.81,
                            0.91, 0.91, 0.91, 0.91))

plot_QALYS <- df_QALYS_criteria %>%
        mutate(`Age band` = as.factor(case_when(between(Age, 50, 54) ~ "50-54",
                                      between(Age, 55, 59) ~ "55-59",
                                      between(Age, 60, 64) ~ "60-64",
                                      between(Age, 65, 69) ~ "65-69",
                                      between(Age, 70, 74) ~ "70-74",
                                      between(Age, 75, 79) ~ "75-79",
                                      .default = NA))) %>%
        group_by(`Age band`) %>%
        summarise(across(contains(c("Expected remaining QALYs","Proportional QALY shortfall")) , mean)) %>%
        mutate(across(contains("Expected remaining QALYs"), ~  round((`Expected remaining QALYs: General population` - .x ), 1))) %>%
        select(-`Expected remaining QALYs: General population`) %>%
        pivot_longer(cols = c(contains("Expected remain"),contains("Proportional QALY")),  names_to = "Population", values_to = "QALY shortfall") %>%
        mutate(Modifier = case_when(grepl( "Expected remaining", Population) & `QALY shortfall` < 12 ~ "Not met",
                                    grepl( "Expected remaining", Population) & `QALY shortfall` < 18 ~ "1.2 criteria met",
                                    grepl( "Expected remaining", Population) & `QALY shortfall` >= 18 ~ "1.7 criteria met",
                                    grepl( "Proportional", Population) & `QALY shortfall` < 0.85 ~ "Not met",
                                    grepl( "Proportional", Population) & `QALY shortfall` < 0.95 ~ "1.2 criteria met",
                                    grepl( "Proportional", Population) & `QALY shortfall` >= 0.95 ~ "1.7 criteria met",
                                    .default = NA)) %>%
        separate_wider_delim(cols = c("Population"), delim = ":", names = c("outcome", "stage"), names_sep = "_",
                             too_few = "align_start" ) %>%
        mutate(Modifier = fct_relevel(Modifier, "Not met", "1.2 criteria met", "1.7 criteria met")) %>%
        mutate(Population_outcome = case_when(Population_outcome == "Expected remaining QALYs" ~ "Absolute QALY shortfall",
                                              .default = Population_outcome)) %>%
        mutate(Population_stage = as.factor( str_squish(Population_stage))) %>%
        ggplot() +
        geom_bar(aes(y = `QALY shortfall`, x = `Age band`, fill = Modifier),
                 position= position_dodge(0.5),
                 stat = "identity") +
        facet_grid(Population_outcome ~ Population_stage, scales="free", switch = "y") +
        geom_hline(data = h_lines, aes(yintercept = Z), linetype = 'dashed', col = 'red')+
        geom_text(data = h_text, aes(x = "65-69", y = Z, label = lab)) +
        ggsci::scale_fill_jama() +
        labs(fill = "NICE Severity Criteria:", x = "Age", y = "") +
        theme_bw(base_size = 18) +
        theme(axis.text.x = element_text(angle = 70, vjust = -0.1, hjust=0),
              legend.position="bottom")

library("svglite")
ggsave("~/GalleriCEmodel/analyses/NICE_modifier/Excel model/Lung cancer results/ISPORFig1.svg", plot_QALYS, device = "svg",
       width = 250,
       height = 200,
       units = "mm")

# install.packages("ggtext")

# by age not by age (50-79)

library(ggtext)

text_lab <- "<span style='font-size:16pt'> On average 46% of undiagnosed lung cancers met either the <span style = 'color:orange'>1.2</span> and <span style = 'color:blue'>1.7</span> criteria across the ages</span>"

df_prevalence_size <- df_QALYS_criteria %>%
        mutate(`Age band` = as.factor(case_when(between(Age, 50, 54) ~ "50-54",
                                                between(Age, 55, 59) ~ "55-59",
                                                between(Age, 60, 64) ~ "60-64",
                                                between(Age, 65, 69) ~ "65-69",
                                                between(Age, 70, 74) ~ "70-74",
                                                between(Age, 75, 79) ~ "75-79",
                                                .default = NA))) %>%
        group_by(`Age band`) %>%
        summarise(across(contains(c("Prevalent", "Expected remaining QALYs", "Proportional QALY shortfall")) , mean)) %>%
        mutate(across(contains("Expected remaining QALYs"), ~  round((`Expected remaining QALYs: General population` - .x ), 1))) %>%
        select(-`Expected remaining QALYs: General population`) %>%
        pivot_longer(cols = c(contains("Expected remain"), contains("Proportional QALY")),  names_to = "Population", values_to = "QALY shortfall") %>%
        mutate(Modifier = case_when(grepl( "Expected remaining", Population) & `QALY shortfall` < 12 ~ "Not met",
                                    grepl( "Expected remaining", Population) & `QALY shortfall` < 18 ~ "1.2 criteria met",
                                    grepl( "Expected remaining", Population) & `QALY shortfall` >= 18 ~ "1.7 criteria met",
                                    grepl( "Proportional", Population) & `QALY shortfall` < 0.85 ~ "Not met",
                                    grepl( "Proportional", Population) & `QALY shortfall` < 0.95 ~ "1.2 criteria met",
                                    grepl( "Proportional", Population) & `QALY shortfall` >= 0.95 ~ "1.7 criteria met",
                                    .default = NA)) %>%
        separate_wider_delim(cols = c("Population"), delim = ":", names = c("outcome", "stage"), names_sep = "_",
                             too_few = "align_start" ) %>%
        select(-`QALY shortfall`) %>%
        pivot_wider(names_from = Population_outcome, values_from = Modifier) %>%
        pivot_longer(contains("Prevalent"), names_to = "Population stage", values_to = "Population") %>%
        separate_wider_delim(cols = `Population stage`, delim = ":", names = c("outcome", "stage"), names_sep = "_",
                             too_few = "align_start" ) %>%
        filter(Population_stage == `Population stage_stage`) %>%
        mutate(Population_stage = as.factor( str_squish(Population_stage))) %>%
        select(-`Population stage_stage`, -`Population stage_outcome`) %>%
        mutate(Modifier = case_when(`Expected remaining QALYs` == "Not met" & `Proportional QALY shortfall` == "Not met" ~ `Expected remaining QALYs`,
                                    `Expected remaining QALYs` != "Not met" & `Proportional QALY shortfall` == "Not met" ~ `Expected remaining QALYs`,
                                    `Proportional QALY shortfall` != "Not met" & `Expected remaining QALYs` == "Not met" ~ `Proportional QALY shortfall`,
                                    `Expected remaining QALYs` != "Not met" & `Proportional QALY shortfall` == "1.2 criteria met" ~ `Expected remaining QALYs`,
                                    `Proportional QALY shortfall` != "Not met" & `Expected remaining QALYs` == "1.2 criteria met" ~ `Proportional QALY shortfall`,
                                    `Expected remaining QALYs` != "Not met" & `Proportional QALY shortfall` == "1.7 criteria met" ~ `Expected remaining QALYs`,
                                    `Proportional QALY shortfall` != "Not met" & `Expected remaining QALYs` == "1.7 criteria met" ~ `Proportional QALY shortfall`)) %>%
        select( - `Expected remaining QALYs`, - `Proportional QALY shortfall`) %>%
        mutate(Modifier = fct_relevel(Modifier, "Not met", "1.2 criteria met", "1.7 criteria met")) %>%
        mutate(Population_stage = fct_relevel(Population_stage, "Stage IV", "Stage III", "Stage II", "Stage I"))

percentage <- df_prevalence_size %>% select(`Age band`, Population_stage, Population) %>%
        group_by(`Age band`) %>%
        summarise(Population = sum(Population)) %>%
        mutate(Percent = rep(sprintf("%0.0f%%", c(60.0, 52.1, 53.4, 49.0, 34.3, 30.2)), each = 1)) %>%
        ungroup()
# 700*700
plot_prevalence_size <- df_prevalence_size%>%
        ggplot() +
        geom_bar(aes(y = Population, x = `Age band`, fill = Modifier, group = Population_stage), colour="black",
                 position= "stack",
                 stat = "identity") +
        ggsci::scale_fill_jama() +
        labs(fill = "NICE Severity Criteria:", x = "5-year age band", y = "Number of undiagnosed lung cancers in a cohort of 100,000 persons") +
        theme_bw(base_size = 16) +
        theme(legend.position="bottom") +
        geom_label(aes(x = "75-79", y = 200, label = "Stage I"), fill = "lightgray") +
        geom_label(aes(x = "75-79", y = 800, label = "Stage II"), fill = "lightgray") +
        geom_label(aes(x = "75-79", y = 1450, label = "Stage III"), fill = "lightgray") +
        geom_label(aes(x = "75-79", y = 2200, label = "Stage IV"), fill = "lightgray")
        # geom_richtext(aes(x = "60-64", y = 2800,
        #                   label = text_lab)) +
       # geom_text(data = percentage, aes(x = `Age band`, y = Population, label = Percent), vjust = -1)


ggsave("~/GalleriCEmodel/analyses/NICE_modifier/Excel model/Lung cancer results/ISPORFig2.svg", plot_prevalence_size, device = "svg",
       width = 200,
       height = 230,
       units = "mm")


## OLD

# df_QALYS_criteria %>%
#         select(-`Expected remaining QALYs: General population`, - contains("Expected remaining QALYs"),  - contains("Proportional QALY")) %>%
#         pivot_longer(cols = contains("Prevalent"), names_to = "Prevalent", values_to = "Number of undiagnosed lung cancers per 100,000*30") %>%
#         pivot_longer(cols = contains("criteria"), names_to = "Criteria", values_to = "Modifier") %>%
#         separate_wider_delim(cols = c("Prevalent", "Criteria"), delim = ":", names = c("outcome", "stage"), names_sep = "_",
#                              too_few = "align_start") %>%
#         filter(Prevalent_stage == Criteria_stage) %>%
#         select(-Criteria_stage) %>%
#         pivot_wider(names_from = Criteria_outcome, values_from = Modifier) %>%
#         mutate(Modifier = case_when(`Absolute criteria met` == "Not met" & `Proportional criteria met` == "Not met" ~ `Absolute criteria met`,
#                                     `Absolute criteria met` != "Not met" & `Proportional criteria met` == "Not met" ~ `Absolute criteria met`,
#                                     `Proportional criteria met` != "Not met" & `Absolute criteria met` == "Not met" ~ `Proportional criteria met`,
#                                     `Absolute criteria met` != "Not met" & `Proportional criteria met` == "1.2 criteria met" ~ `Absolute criteria met`,
#                                     `Proportional criteria met` != "Not met" & `Absolute criteria met` == "1.2 criteria met" ~ `Proportional criteria met`,
#                                     `Absolute criteria met` != "Not met" & `Proportional criteria met` == "1.7 criteria met" ~ `Absolute criteria met`,
#                                     `Proportional criteria met` != "Not met" & `Absolute criteria met` == "1.7 criteria met" ~ `Proportional criteria met`)) %>%
#         select( - `Absolute criteria met`, - `Proportional criteria met`, - Prevalent_outcome) %>%
#         mutate(Prevalent_stage = as.factor( str_squish(Prevalent_stage))) %>%
#         mutate(Age = as.factor(Age)) %>%
#         mutate(`Number of undiagnosed lung cancers per 100,000` = `Number of undiagnosed lung cancers per 100,000*30`) %>%
#
#         mutate(Modifier = fct_relevel(Modifier, "Not met", "1.2 criteria met", "1.7 criteria met")) %>%
#         mutate(Prevalent_stage = fct_relevel(Prevalent_stage, "Stage IV", "Stage III", "Stage II", "Stage I"))
#
# percentage <- df_prevalence_size %>% select(Age, Prevalent_stage, `Number of undiagnosed lung cancers per 100,000`) %>%
#         group_by(Age) %>%
#         summarise(`Number of undiagnosed lung cancers per 100,000` = sum(`Number of undiagnosed lung cancers per 100,000`)) %>%
#         mutate(Percent = rep(sprintf("%0.0f%%", c(73,73,51.2,51.3,51.3,52,52,52.1,52.1,52.2,53.2,53.3,53.4,53.4,53.5,54,54.1,54.2,54.3,28.6,54.9,28.9,29,29.2,29.3,29.7,29.9,30.2,30.4,30.6)), each = 1)) %>%
#         ungroup()
