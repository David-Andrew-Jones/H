#______________________________________________________________________________#
#____ 01_prepare_SEER_data
#' Loads in individual patient data and life tables

#______________________________________________________________________________#
#____ Case listing from SEER*STAT survival session
#' 
#' https://seer.cancer.gov/news/SEER-Relative-Survival-Overview.pdf
#' https://seer.cancer.gov/seerstat/508_WebHelp/Display_Options_for_a_Survival_Session.htm

list_individual_data_original <- list(
        
        individual_data_original_50to64_F = read_excel("data/newSEER/Case listing F 50-64y.xlsx",
                                                       #range="A1:I341580", #bug in cellranger for large rectangles
                                                       #range = cell_limits(c(1, 1), c(365426, 13)), 
                                                       col_names = TRUE,
                                                       col_types = "text"),
        
        individual_data_original_65to79_F = read_excel("data/newSEER/Case listing F 65-79y.xlsx",
                                                       #range="A1:I341580", #bug in cellranger for large rectangles
                                                       #range = cell_limits(c(1, 1), c(365426, 13)), 
                                                       col_names = TRUE,
                                                       col_types = "text"),
        
        individual_data_original_50to64_M = read_excel("data/newSEER/Case listing M 50-64y.xlsx",
                                                       #range="A1:I341580", #bug in cellranger for large rectangles
                                                      # range = cell_limits(c(1, 1), c(365426, 13)), 
                                                       col_names = TRUE,
                                                       col_types = "text"),
        
        individual_data_original_65to79_M = read_excel("data/newSEER/Case listing M 65-79y.xlsx",
                                                       #range="A1:I341580", #bug in cellranger for large rectangles
                                                      # range = cell_limits(c(1, 1), c(365426, 13)), 
                                                       col_names = TRUE,
                                                       col_types = "text")
        
)


individual_data <- bind_rows(list_individual_data_original) %>% 
        type_convert() %>%
        `colnames<-`(c("SEER_Draw", "Stage", "Sex", "Age", "Race", "Status" ,"Diag_year", "End_year", "Adjusted_Status", "Num_intervals", "Cum_expected_survival", "Final_expected_12monthinterval",
                       "Final_expected_interval_year")) 


#' Create time-to-event data from the IPD
#' Of form: time, event, arm 
#' Time = number of intervals ~annualise~, event given by adjusted status

TTE_data <- individual_data %>%
        #select(SEER_Draw, Stage, Sex, Age, Diag_year, Num_intervals, Adjusted_Status) %>%
        mutate(Age = gsub(" years", "", Age)) %>%
        separate(Age, sep="-", into = c("Age_lower", "Age_upper")) %>%
        mutate(across(c("Age_lower", "Age_upper"), as.numeric)) %>%
        mutate(time = Num_intervals/12) %>%
        mutate(event = case_when(Adjusted_Status == "Dead" ~ 1,
                                 Adjusted_Status == "Untraced" ~ 0,
                                 .default = 0),
               arm = 1) %>%
        mutate(AJCC_stage = case_when(grepl(c("IV"), Stage) ~ "IV",
                                      grepl(c("III"), Stage) ~ "III",
                                      grepl(c("II"), Stage) ~ "II",
                                      grepl(c("I"), Stage) ~ "I",
                                      .default = Stage)) %>%
        mutate(across(where(is.character), as.factor))

# synthetic_TTE_data <- synthpop::syn(TTE_data %>% select(SEER_Draw, AJCC_stage, Sex, Age_lower, time, event) %>% mutate(event =  as.factor(event)))
# 
# compare((TTE_data %>% 
#           select(SEER_Draw, AJCC_stage, Sex, Age_lower, time, event) %>% 
#           mutate(event =  as.factor(event))), 
#         synthetic_TTE_data$syn, stat = "counts")
# 
# write.csv(as.data.frame(synthetic_TTE_data$syn), "data/synthetic_TTE_data.csv")
# 
# a <- synthetic_TTE_data$syn
#______________________________________________________________________________#
#'____ Load in national life tables

US_lifetable19_original <- bind_rows(list(
        
        male = read_excel("data/Table 2. Life table for males- United States, 2019.xlsx",
                         range = cell_limits(c(3, 1), c(104, 7)), #note reversal
                         col_names = TRUE,
                         col_types = "text") %>%
                mutate(Sex = "Male"),
        
        female = read_excel("data/Table 3. Life table for females- United States, 2019.xlsx",
                            range = cell_limits(c(3, 1), c(104, 7)), #note reversal
                            col_names = TRUE,
                            col_types = "text") %>%
                mutate(Sex = "Female")
        )) %>%
        rename(Age_range = `...1` ) %>%
        mutate(Age_lower = as.numeric(gsub(".*?([0-9]+).*", "\\1", Age_range))    ) %>%
        relocate(Age_lower, Sex) %>%
        mutate(across(qx:ex, as.numeric)) %>%
        mutate(hazard = dx/Lx)

bckgrnd_hz <- US_lifetable19_original


#______________________________________________________________________________#
#'____ Load cure fraction sheet

df_cure_fraction_byageandstage <- read_excel("data/cure_fractions_byageandstage.xlsx",
                                         col_names = TRUE) %>%
  mutate(across(contains("ER_Stage"), \(x) as.character(x))) %>%
  select(`Cancer Type`:`ER_Stage IV`) %>%
  pivot_longer(
    cols = `CF_Stage I`:`ER_Stage IV`,
    names_to = "staging",
    values_to = "cf_and95_andER"
  ) %>% 
  mutate(`Age Group` = gsub(" years", "", `Age Group`)) %>%
  separate(`Age Group`, sep="-", into = c("Age_lower", "Age_upper")) %>%
  separate(staging, sep=" ", into = c("prefix", "AJCC_stage")) %>%
  pivot_wider(names_from = prefix, values_from = cf_and95_andER) %>%
  separate(CF_Stage, sep=" ", into = c("cure_fraction", "cure_fraction_95")) %>%
  mutate(cure_fraction_95 = str_sub(cure_fraction_95, 2, -2)) %>%
  separate(cure_fraction_95, sep="-", into = c("cf_lower95", "cf_upper95")) %>%
  rename(excess_risk = ER_Stage) %>%
  rename(SEER_Draw = `Cancer Type`) %>%
  mutate(across(c("Age_lower", "Age_upper", "cure_fraction", "cf_lower95", "cf_upper95", "excess_risk"), as.numeric)) %>%
  # transform excess risk into hazard
  mutate(CFlongterm_hz = -(log(excess_risk)/12) ) %>%
  # Conditions for SHELF fitdist to run
  mutate(across(cure_fraction:cf_upper95, ~ . / 100)) %>%
  mutate(across(cure_fraction:cf_upper95, ~ case_when(. <= 0 ~ 0.001,
                                                      . >= 1 ~ 0.999,
                            .default = .))) %>%
  mutate(cf_lower95 = case_when( cf_lower95 == cure_fraction ~ cf_lower95 - 0.005,
                                 .default = cf_lower95)) %>%
  mutate(cf_upper95 = case_when( cf_upper95 == cure_fraction ~ cf_upper95 + 0.005,
                               .default = cf_upper95)) %>%
  select(-Age_upper)

fun_gen_beta <- function(lower, mean, upper) {
  
  out <- SHELF::fitdist(vals=c(lower, mean, upper), probs=c(0.025, 0.5, 0.975), lower=0, upper=1)$Beta
  return(out)
  
}

df_cf_beta_params <- pmap_dfr(df_cure_fraction_byageandstage %>% select(cf_lower95, cure_fraction, cf_upper95) %>%
                      rename(lower = cf_lower95,
                             mean = cure_fraction,
                             upper = cf_upper95) ,
                    fun_gen_beta)

df_cure_fraction_byageandstage <- cbind(df_cure_fraction_byageandstage, df_cf_beta_params) 

res_cancers_evaluated <- intersect(levels(as.factor(TTE_data$SEER_Draw)) , levels(as.factor(df_cure_fraction_byageandstage$SEER_Draw)))
res_cancers_excluded <- setdiff(levels(as.factor(TTE_data$SEER_Draw)) , levels(as.factor(df_cure_fraction_byageandstage$SEER_Draw)))

#______________________________________________________________________________#
# Join

TTE_data_curefrac <- TTE_data %>%
  mutate(cf_df_ages = case_when(Age_lower == 50 ~ 40,
                                Age_lower == 60 ~ 55,
                                Age_lower == 70 ~ 65,
                                .default = Age_lower)) %>%
        right_join(df_cure_fraction_byageandstage, by = c("SEER_Draw" , "AJCC_stage" , "cf_df_ages" = "Age_lower") ) %>%
        group_by(Sex, SEER_Draw, AJCC_stage, Age_lower) %>%
        arrange(time) %>%
        ungroup() %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor)) %>%
        filter( !(grepl(c("Cervix|Ovary|Breast|Uterus"), SEER_Draw) & Sex == "Male")) %>%
        filter( !(grepl(c("Prostate"), SEER_Draw) & Sex == "Female")) 

#______________________________________________________________________________#

# Table 1 - Patient characteristics
# Distribution of age, sex, race/ethnicity, year of diagnosis, follow-up time (median or categorized), cancer type, stratified by stage at dx?
# Vital status
# Function
f_gen_SEERcharacteristic_table <- function( varname){
        
        var <- ensym(varname)
        
        res_bystage <- TTE_data_curefrac_categorised %>%
                group_by(AJCC_stage, {{var}}) %>%
                summarise(n = n()) %>%
                mutate(freq = paste0("(",round(n / sum(n) * 100, 0), "%", ")")) %>%
                unite(N_percentage, n, freq, remove = TRUE, sep = " " ) %>%
                pivot_wider(names_from = AJCC_stage, values_from = N_percentage) %>%
                ungroup() %>%
                mutate(across(.cols = I:IV, .fns = str_extract, pattern = "[[:digit:]]+", .names = "n_{.col}"))%>%
                mutate(Total_n = as.numeric(n_I) + as.numeric(n_II) + as.numeric(n_III) + as.numeric(n_IV), .before = 2) %>%
                mutate(freq = paste0("(",round(Total_n / sum(Total_n) * 100, 0), "%", ")"), .before = 3) %>%
                unite(`All stages`, Total_n, freq, remove = TRUE, sep = " " ) %>%
                select(-c(n_I:n_IV)) %>%
                add_column( characteristic = varname, .before = 1) %>%
                rename(grouping = {{var}})
  
        return(res_bystage)
        
}

TTE_data_curefrac_categorised <- TTE_data_curefrac %>%
        unite(Age, Age_lower, Age_upper, sep=" to ") %>% 
        mutate(follow_up_duration = case_when( between(Num_intervals, 0, 12) ~ "0–12",
                                               between(Num_intervals, 13, 24) ~ "13-24",
                                               between(Num_intervals, 25, 36) ~ "25-36",
                                               between(Num_intervals, 37, 48) ~ "37-48",
                                               between(Num_intervals, 49, 60) ~ "49-60",
                                               between(Num_intervals, 61, 72) ~ "61-72",
                                               between(Num_intervals, 73, 84) ~ "73-84",
                                               between(Num_intervals, 85, 96) ~ "85-96",
                                               between(Num_intervals, 97, 108) ~ "97-108",
                                               between(Num_intervals, 109, 120) ~ "109-120",
                                               between(Num_intervals, 121, 132) ~ "121-132",
                                               between(Num_intervals, 133, 144) ~ "133-144",
                                               between(Num_intervals, 145, 156) ~ "145-156",
                                               between(Num_intervals, 157, 168) ~ "157-168",
                                               between(Num_intervals, 169, 180) ~ "169-180",
                                               between(Num_intervals, 181, 192) ~ "181-192")) %>%
        mutate(across(where(is.character), as.factor),
               follow_up_duration = fct_relevel(follow_up_duration, c("0–12", "13-24", "25-36","37-48","49-60","61-72","73-84","85-96",
                                                                      "97-108","109-120","121-132","133-144","145-156","157-168","169-180"))) 

v_characteristics <- c("Status", "Sex", "Age", "Race", "SEER_Draw" ,"follow_up_duration")

res_bystage <- map(v_characteristics, f_gen_SEERcharacteristic_table) %>%
        bind_rows()
#______________________________________________________________________________#

addWorksheet(excel_output, sheetName = "Table 1 Descriptives")
writeData(excel_output, sheet = "Table 1 Descriptives", res_bystage)

#______________________________________________________________________________#


