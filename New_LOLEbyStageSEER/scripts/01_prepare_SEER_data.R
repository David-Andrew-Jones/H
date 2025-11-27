#______________________________________________________________________________#
#____ 01_prepare_SEER_data
#' Loads in individual patient data and life tables

#______________________________________________________________________________#
#____ Case listing from SEER*STAT survival session

individual_data <- list.files( path="data/SEER_none_CTS/", pattern="\\.csv$", full.names=TRUE ) %>% 
  map_dfr( read_csv) %>%
  select( `Site recode ICD-O-3/WHO 2008`, `Derived AJCC Stage Group, 6th ed (2004-2015)`,
         Sex, `Age recode with <1 year olds and 90+`, `Vital status recode (study cutoff used)`,
         `Year of diagnosis`, `Year of follow-up recode`,
         `Survival months`, `Survival months flag`) %>%
  `colnames<-`(c("SEER_Draw", "Stage", "Sex", "Age", "Status" ,"Diag_year", "End_year", "Num_intervals", "Survival months flag")) 

#' _____ 
#' #' Create time-to-event data from the IPD
#' Of form: time, event, arm 
#' Time = number of intervals ~annualise~, event given by adjusted status
#' Add larger groupings

df_icdo3map <- read.csv("data/icdo3groups.csv") %>%
  # Space
  mutate(subgroupings = stringr::str_replace_all(subgroupings, "\\s", " ")) %>%
  mutate(groupings = stringr::str_replace_all(groupings, "\\s", " "))


TTE_data <- individual_data %>%
        mutate(Age = gsub(" years", "", Age)) %>%
        separate(Age, sep="-", into = c("Age_lower", "Age_upper")) %>%
        mutate(across(c("Age_lower", "Age_upper"), as.numeric)) %>%
        mutate(Num_intervals = as.numeric(str_remove(Num_intervals, "^0+"))) %>%
  filter(!is.na(Num_intervals)) %>%
        mutate(time = Num_intervals/12) %>%
        mutate(event = case_when(Status == "Dead" ~ 1,
                                 Status == "Status" ~ 0,
                                 .default = 0),
               arm = 1) %>%
        mutate(AJCC_stage = case_when(grepl(c("IV"), Stage) ~ "IV",
                                      grepl(c("III"), Stage) ~ "III",
                                      grepl(c("II"), Stage) ~ "II",
                                      grepl(c("I"), Stage) ~ "I",
                                      .default = NA)) %>%
  filter(!is.na(AJCC_stage)) %>%
  group_by(Sex, SEER_Draw, AJCC_stage, Age_lower) %>%
  arrange(time) %>%
  ungroup() %>%
  mutate(SEER_Draw = as.character(SEER_Draw)) %>%
  mutate(SEER_Draw =  str_remove_all(SEER_Draw, ",")) %>%
  left_join(df_icdo3map, by = c("SEER_Draw" = "subgroupings")) %>%
  mutate(SEER_Draw = case_when(is.na(groupings) ~ SEER_Draw,
                          .default = groupings)) %>%
  # Remove cancer types with <5000 incidences 
  filter(!(SEER_Draw %in% c("Other Digestive Organs", "Myeloma", "Trachea Mediastinum and Other Respiratory Organs",
                            "Peritoneum Omentum and Mesentery", "Other Male Genital Organs", "Retroperitoneum",
                            "Bones and Joints", "Penis", "Nose Nasal Cavity and Middle Ear", "Eye and Orbit", "Testis"))) %>%
  mutate(SEER_Draw = str_replace(SEER_Draw, " Cancer", "")) %>%
  filter( !(grepl(c("Cervix|Breast|Uterus"), SEER_Draw) & Sex == "Male")) %>%
  filter( !(grepl(c("Prostate"), SEER_Draw) & Sex == "Female")) %>%
  mutate(across(where(is.factor), as.character)) %>%
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

bckgrnd_hz <- US_lifetable19_original %>%
  select(Sex) %>%
  distinct() %>%
  left_join(tibble(Age_lower = seq(from=0,to=100,by=0.5)), by = character()) %>% #incidence data is only used up to 85
  left_join(US_lifetable19_original %>%
              select(Age_lower, Sex, hazard, ex) %>% group_by(Sex) %>%
              summarise(
                zeta_1 = list(splinefun(Age_lower, hazard, method="natural")),
                zeta_2 = list(splinefun(Age_lower, ex, method="natural"))
              ) %>%
              ungroup()) %>%
  mutate(hazard = sapply(1:length(Age_lower), function(z){zeta_1[z][[1]](Age_lower[z])})) %>%
  mutate(ex = sapply(1:length(Age_lower), function(z){zeta_2[z][[1]](Age_lower[z])})) %>%
  select(Sex, Age_lower, hazard, ex)

# Table 1 - Patient characteristics
# Distribution of age, sex, race/ethnicity, year of diagnosis, follow-up time (median or categorized), cancer type, stratified by stage at dx?
# Vital status
# Function
f_gen_SEERcharacteristic_table <- function( varname){
        
        var <- ensym(varname)
        
        res_bystage <- TTE_data_categorised %>%
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

TTE_data_categorised <- TTE_data %>%
        unite(Age, Age_lower, Age_upper, sep=" to ") %>% 
        mutate(follow_up_duration = case_when( between(Num_intervals, 0, 11) ~ "<1 year",
                                               between(Num_intervals, 12, 59) ~ "1 to <5 years",
                                               between(Num_intervals, 60, 119) ~ "5 to <10 years",
                                               between(Num_intervals, 120, 179) ~ "10 to <15 years",
                                               between(Num_intervals, 180, 239) ~ "15 to <20 years",
                                              .default = NA)) %>%
        mutate(across(where(is.character), as.factor),
               follow_up_duration = fct_relevel(follow_up_duration, c("<1 year", "1 to <5 years", "5 to <10 years",
                                                                      "10 to <15 years","15 to <20 years"))) 

v_characteristics <- c("Status", "Sex", "Age", "SEER_Draw" ,"follow_up_duration")

res_bystage <- map(v_characteristics, f_gen_SEERcharacteristic_table) %>%
        bind_rows()
#______________________________________________________________________________#

addWorksheet(excel_output, sheetName = "Table 1 Descriptives")
writeData(excel_output, sheet = "Table 1 Descriptives", res_bystage)

#______________________________________________________________________________#


