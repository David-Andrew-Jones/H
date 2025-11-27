################################################################################
#______________________________________________________________________________#
#' _____ Impact of an MCED screening programme on treatment demand
#______________________________________________________________________________#
################################################################################
#' _____________________________________________________________________________
#' Packages (may not all be still in use)
suppressPackageStartupMessages(library(tidyverse))
library(readxl, quietly = TRUE)
library(gt, quietly = TRUE)
library(openxlsx, quietly = TRUE)

#' _____________________________________________________________________________
#' Parameters to vary

uptake <- "res_70"

#' _____________________________________________________________________________
# 1 Stage shift from the Interception model (adapted for England data)

### 1.1 Table 1. Stage shift (incidence rate per 100,000 population, 95% CIs) in prevalent round and steady-state screening programme (age-weighted lifetime incidence rate)

# Read in interception model stage shift and create Ratio of incidence between standard of care and Prev round and SS
df_intercept_model_output <- readxl::read_xlsx(paste0("~/Miscellaneous/Treatments/interception_model_results/",
                                                      list.files(path = "~/Miscellaneous/Treatments/interception_model_results", pattern = paste0( uptake )))) %>%
        # reformat to long with stage 
        pivot_longer(!NCRAS_Draw:model_type, names_to = "outcome_type", values_to = "number") %>%
        # Create stage column
        mutate(stage = readr::parse_number(outcome_type),
               outcome_type = as.factor(gsub("[^a-zA-Z]", "", outcome_type))) %>%
        # Pivot back to wide and rename variables
        pivot_wider(names_from = outcome_type, values_from = number) %>%
        rename(`IRper100 prev round pre MCED` = weightedstartstageSA,
               `IRper100 prev round post MCED` = weightedendstageSA,
               `IRper100 SS pre MCED` = weightedstartstage,
               `IRper100 SS post MCED` = weightedendstage) %>%
        mutate(`Ratio IRper100 prev round` = `IRper100 prev round post MCED` / `IRper100 prev round pre MCED` ) %>%
        mutate(`Ratio IRper100 SS` = `IRper100 SS post MCED` / `IRper100 SS pre MCED` ) %>%
        select(-c(scenario:model_type)) %>%
        mutate(across(NCRAS_Draw:stage, \(x) as.factor(x))) %>%
        ### Missing in previous iteration - need to drop [other] cancer type as this is not found in NCRAS data
        filter(NCRAS_Draw != "[OTHER]")

df_table_1 <- df_intercept_model_output %>% mutate(across(where(is.numeric), \(x) round(x, 1)))
# GT table for word output
gt::gt(df_table_1)

# Excel output
excel_output <- createWorkbook()
addWorksheet(excel_output, sheetName = "1. Stage-shift proportions")
writeData(excel_output, sheet = "1. Stage-shift proportions", df_table_1)

#' _____________________________________________________________________________
# 2. Apply modelled stage-shift to 2014-2019 incidence (including by IMD)

### 2.1 Table 2. Stage-shifted incidence (incidence rate per 100,000 population, 95% CIs)

# Read in incidence data from NCRAS
df_England_Inc_14to19 <- readxl::read_xlsx("~/Miscellaneous/Treatments/20241205_LibbyEllis_GRAIL_ShiftedTreatmentPathwaysData_ext.xlsx", sheet = "Inc Stage") %>% 
        # the numbers span 6 years, use the average to make annual
        mutate(`Annual number of tumours diagnosed` = `Number of tumours diagnosed`/ 6)

# Create colon and rectum sites for the new data - same ratios
df_intercept_model_output_splits <- rbind(
        
        df_intercept_model_output %>% mutate(NCRAS_Draw = case_when(NCRAS_Draw  == "Colon/Rectum" ~ "Colon", .default = NCRAS_Draw )),
        
        df_intercept_model_output %>% filter(NCRAS_Draw  == "Colon/Rectum") %>% 
                mutate(NCRAS_Draw = case_when(NCRAS_Draw  == "Colon/Rectum" ~ "Rectum", .default = NCRAS_Draw ))
)

# Join incidence data to interception model IR ratios to derive number of tumours diagnosed
df_England_Inc_joined_intercept <- df_England_Inc_14to19 %>%
        # All ages being 50-79
        filter(`Age at diagnosis` == "All ages") %>%
        # Standardize cancer names
        mutate(`Cancer type` = case_when(`Cancer type` == "Headneck" ~ "Head and Neck",
                                         `Cancer type` == "Liver" ~ "Liver/Bile-duct",
                                         `Cancer type` == "Oesophagus" ~ "Esophagus",
                                         `Cancer type` == "Urothelial" ~ "Urothelial Tract",
                                         .default = `Cancer type`)) %>%
        # Join by type and stage
        left_join(df_intercept_model_output_splits %>% select(NCRAS_Draw, stage, `Ratio IRper100 prev round`,  `Ratio IRper100 SS`), 
                  by = c("Cancer type" = "NCRAS_Draw", "Stage at diagnosis" = "stage")) %>%
        # Change NA ratios (i.e. those not modeled by the intception mode) to 1 to assume no change
        mutate(across(`Ratio IRper100 SS`:`Ratio IRper100 prev round`,
                      ~ case_when(is.na(.) ~ 1, .default = .)   )) %>%
        # Calculate number of cancer diagnosed in prev and SS round based on no. tumours in standard care and the IR ratios
        mutate(`Prev no. of tumours diagnosed` = `Annual number of tumours diagnosed` * 
                       `Ratio IRper100 prev round`,
               `SS no. of tumours diagnosed` = `Annual number of tumours diagnosed` * 
                       `Ratio IRper100 SS`) %>%
        select(`Cancer type`, `Stage at diagnosis`, `Annual number of tumours diagnosed`,
               `Prev no. of tumours diagnosed`, `SS no. of tumours diagnosed`)

# Check that the way of doing colon and rectum add up to previous script numbers
check_colon_rectum <- df_England_Inc_joined_intercept %>%
        filter(`Cancer type` %in% c("Colon", "Rectum")) %>%
        group_by(`Stage at diagnosis`) %>%
        summarise(across(`Annual number of tumours diagnosed`:`SS no. of tumours diagnosed`, \(x) sum(x)))

# Add all cancer row and calculate changes
df_England_Inc_joined_intercept <- rbind(
        
        df_England_Inc_joined_intercept %>%
                group_by(`Stage at diagnosis`) %>%
                summarise(across(`Annual number of tumours diagnosed`:`SS no. of tumours diagnosed`, \(x) sum(x))) %>%
                mutate(`Cancer type` = "All cancers combined", .before = `Stage at diagnosis`),
        
        df_England_Inc_joined_intercept
        
) %>%
        # Calculate % change in the annual number of cancers diagnosed campred to SoC
        mutate(`Prev % change in no. diagnosed` = ((`Prev no. of tumours diagnosed` - `Annual number of tumours diagnosed`) / `Annual number of tumours diagnosed`) * 100,
               `SS % change in no. diagnosed` = ((`SS no. of tumours diagnosed` - `Annual number of tumours diagnosed`) / `Annual number of tumours diagnosed`) * 100) %>%
        rename(`SoC no. of tumours diagnosed` = `Annual number of tumours diagnosed`) %>%
        select(`Cancer type`, `Stage at diagnosis`, `SoC no. of tumours diagnosed`, `Prev no. of tumours diagnosed`, `Prev % change in no. diagnosed` , `SS no. of tumours diagnosed`, `SS % change in no. diagnosed` ) %>%
        mutate(across(where(is.character), as.factor))

# GT table for word output
df_table_2 <- df_England_Inc_joined_intercept %>% mutate(across(where(is.numeric), \(x) round(x, 1)))
gt::gt(df_table_2)

# Excel output
addWorksheet(excel_output, sheetName = "2. Stage-shifted incidence")
writeData(excel_output, sheet = "2. Stage-shifted incidence", df_table_2)


#' _________
#' Repeat for IMD
df_England_IncXIMD_14to19 <- readxl::read_xlsx("~/Miscellaneous/Treatments/20241205_LibbyEllis_GRAIL_ShiftedTreatmentPathwaysData_ext.xlsx", sheet = "Inc X IMD") %>% 
        mutate(`Annual number of tumours diagnosed` = `Number of tumours diagnosed`/ 6)

df_England_IncXIMD_joined_intercept <- df_England_IncXIMD_14to19 %>%
        filter(`Age at diagnosis` == "All ages") %>%
        mutate(`Cancer type` = case_when(`Cancer type` == "Headneck" ~ "Head and Neck",
                                         `Cancer type` == "Liver" ~ "Liver/Bile-duct",
                                         `Cancer type` == "Oesophagus" ~ "Esophagus",
                                         `Cancer type` == "Urothelial" ~ 	"Urothelial Tract",
                                         .default = `Cancer type`)) %>%
        left_join(df_intercept_model_output_splits, by = c("Cancer type" = "NCRAS_Draw", "Stage at diagnosis" = "stage")) %>%
        mutate(across(`Ratio IRper100 SS`:`Ratio IRper100 prev round`, ~ case_when(is.na(.) ~ 1, .default = .)   )) %>%
        mutate(`Prev no. of tumours diagnosed` = `Annual number of tumours diagnosed` * `Ratio IRper100 prev round`,
               `SS no. of tumours diagnosed` = `Annual number of tumours diagnosed` * `Ratio IRper100 SS`) %>%
        select(`Cancer type`, `Index of Multiple Deprivation Quintile`, `Stage at diagnosis`, `Annual number of tumours diagnosed`, `Prev no. of tumours diagnosed`, `SS no. of tumours diagnosed`) 

## Add all cancer row and calculate changes
df_England_IncXIMD_joined_intercept <- rbind(
        
        df_England_IncXIMD_joined_intercept %>%
                group_by(`Index of Multiple Deprivation Quintile`,`Stage at diagnosis`) %>%
                summarise(across(`Annual number of tumours diagnosed`:`SS no. of tumours diagnosed`, \(x) sum(x))) %>%
                mutate(`Cancer type` = "All cancers combined", .before = `Stage at diagnosis`),
        
        df_England_IncXIMD_joined_intercept
        
) %>%
        mutate(`Prev % change in no. diagnosed` = ((`Prev no. of tumours diagnosed` - `Annual number of tumours diagnosed`) / `Annual number of tumours diagnosed`) * 100,
               `SS % change in no. diagnosed` = ((`SS no. of tumours diagnosed` - `Annual number of tumours diagnosed`) / `Annual number of tumours diagnosed`) * 100) %>%
        rename(`SoC no. of tumours diagnosed` = `Annual number of tumours diagnosed`) %>%
        select(`Index of Multiple Deprivation Quintile`,`Cancer type`, `Stage at diagnosis`, `SoC no. of tumours diagnosed`, `Prev no. of tumours diagnosed`, `Prev % change in no. diagnosed` , `SS no. of tumours diagnosed`, `SS % change in no. diagnosed` ) %>%
        mutate(across(where(is.character), as.factor))

#' _________
#' Repeat for Cancer alliance
df_England_IncXCA_14to19 <- readxl::read_xlsx("~/Miscellaneous/Treatments/20241205_LibbyEllis_GRAIL_ShiftedTreatmentPathwaysData_ext.xlsx", sheet = "Inc X Alliance") %>% 
        mutate(`Annual number of tumours diagnosed` = `Number of tumours diagnosed`/ 6)

df_England_IncXCA_joined_intercept <- df_England_IncXCA_14to19 %>%
        filter(`Age at diagnosis` == "All ages") %>%
        mutate(`Cancer type` = case_when(`Cancer type` == "Colorectal" ~ "Colon/Rectum",
                                         `Cancer type` == "Headneck" ~ "Head and Neck",
                                         `Cancer type` == "Liver" ~ "Liver/Bile-duct",
                                         `Cancer type` == "Oesophagus" ~ "Esophagus",
                                         `Cancer type` == "Urothelial" ~ 	"Urothelial Tract",
                                         .default = `Cancer type`)) %>%
        left_join(df_intercept_model_output, by = c("Cancer type" = "NCRAS_Draw", "Stage at diagnosis" = "stage")) %>%
        mutate(across(`Ratio IRper100 SS`:`Ratio IRper100 prev round`, ~ case_when(is.na(.) ~ 1, .default = .)   )) %>%
        mutate(`Prev no. of tumours diagnosed` = `Annual number of tumours diagnosed` * `Ratio IRper100 prev round`,
               `SS no. of tumours diagnosed` = `Annual number of tumours diagnosed` * `Ratio IRper100 SS`) %>%
        select(`Cancer type`, `Cancer Alliance`, `Stage at diagnosis`, `Annual number of tumours diagnosed`, `Prev no. of tumours diagnosed`, `SS no. of tumours diagnosed`) 

## Add all cancer row and calculate changes
df_England_IncXCA_joined_intercept <- rbind(df_England_IncXCA_joined_intercept %>%
                                                    group_by(`Cancer Alliance`,`Stage at diagnosis`) %>%
                                                    summarise(across(`Annual number of tumours diagnosed`:`SS no. of tumours diagnosed`, \(x) sum(x))) %>%
                                                    mutate(`Cancer type` = "All cancers combined", .before = `Stage at diagnosis`),
                                            df_England_IncXCA_joined_intercept) %>%
        mutate(`Prev % change in no. diagnosed` = ((`Prev no. of tumours diagnosed` - `Annual number of tumours diagnosed`) / `Annual number of tumours diagnosed`) * 100,
               `SS % change in no. diagnosed` = ((`SS no. of tumours diagnosed` - `Annual number of tumours diagnosed`) / `Annual number of tumours diagnosed`) * 100) %>%
        rename(`SoC no. of tumours diagnosed` = `Annual number of tumours diagnosed`) %>%
        select(`Cancer Alliance`, `Cancer type`, `Stage at diagnosis`, `SoC no. of tumours diagnosed`, `Prev no. of tumours diagnosed`, `Prev % change in no. diagnosed` , `SS no. of tumours diagnosed`, `SS % change in no. diagnosed` ) %>%
        mutate(across(where(is.character), as.factor))


#' _____________________________________________________________________________
# 3. Apply treatment proportions to stage-shifted incidence/numbers

# Read in treatment data from NCRAS
df_England_treatmentXstageXmodality_14to19 <- readxl::read_xlsx("~/Miscellaneous/Treatments/20241205_LibbyEllis_GRAIL_ShiftedTreatmentPathwaysData_ext.xlsx", sheet = "Treat Stage X Modality X Intent") %>%
        mutate(`Number of tumours treated` = as.numeric(`Number of tumours treated`)) %>%
        mutate(`Annual number of tumours diagnosed` = `Number of tumours diagnosed`/ 6) %>%
        mutate(`Annual number of tumours treated` = `Number of tumours treated`/ 6) %>%
        mutate(`Cancer type` = case_when(`Cancer type` == "Headneck" ~ "Head and Neck",
                                         `Cancer type` == "Liver" ~ "Liver/Bile-duct",
                                         `Cancer type` == "Oesophagus" ~ "Esophagus",
                                         `Cancer type` == "Urothelial" ~ 	"Urothelial Tract",
                                         .default = `Cancer type`)) %>%
        select(-c(  `Number of tumours diagnosed`, `Number of tumours treated`, `Proportion`:`UCI`)) 

# check to see if number diagnosed is the same as the number treated for the combination categories (as they should be exhaustive and non-overlappingg) 
# as given in the annual number of tumours diagnosed column of df_England_treatmentXstageXmodality_14to19
check1 <-  df_England_treatmentXstageXmodality_14to19 %>%
        filter(!(`Treatment modality` %in% c("Tumour resection", "Chemotherapy (cytotoxic)", "Chemotherapy (other)", "Radiotherapy"))) %>%
        group_by(`Cancer type`, `Stage at diagnosis`) %>%
        summarise(total = sum(`Annual number of tumours treated`))

# Combine with the incidence and stage shift dataframe
df_treatentXstage_joined_intercept <- df_England_treatmentXstageXmodality_14to19 %>%
        # Merge on the incidence and stage shift dataframe
        left_join(df_England_Inc_joined_intercept, by = c("Cancer type" , "Stage at diagnosis" )) %>%
        # Calculate the ratio of incidence again (this is the same as the Ratio of IR)
        mutate(prev_multiplier = (`Prev no. of tumours diagnosed`/`SoC no. of tumours diagnosed`),
               SS_multiplier = (`SS no. of tumours diagnosed`/`SoC no. of tumours diagnosed`)) %>%
        # For each treatment type, calculate numbers for the prev and SS rounds 
        mutate(`Prev no. of tumours treated` = `Annual number of tumours treated` * prev_multiplier,
               `SS no. of tumours treated` = `Annual number of tumours treated` * SS_multiplier) %>%
        rename(`SoC no. of tumours treated` = `Annual number of tumours treated`)

# check to see if number treated in prev round for the categories is the same as is the same as the number diagnosed 
# as given in the prev/SS no. tumours diagnosed columns of df_treatentXstage_joined_intercept
check <-  df_treatentXstage_joined_intercept %>%
        filter(!(`Treatment modality` %in% c("Tumour resection", "Chemotherapy (cytotoxic)", "Chemotherapy (other)", "Radiotherapy"))) %>%
        group_by(`Cancer type`, `Stage at diagnosis`) %>%
        summarise(total = sum(`Prev no. of tumours treated`, na.rm = TRUE))

#' Create a number diagnosed by cancer type and stage data set for later joining on
df_num_diagnosed_bytype_bystage <- df_treatentXstage_joined_intercept %>%
        group_by(`Cancer type`,`Stage at diagnosis`) %>%
        slice(1) %>%
        select(`Cancer type`,`Stage at diagnosis`, `SoC no. of tumours diagnosed`, `Prev no. of tumours diagnosed`, `SS no. of tumours diagnosed`) %>%
        rbind(.,  (.) %>% 
                      group_by(`Stage at diagnosis`) %>%
                      summarise(across(`SoC no. of tumours diagnosed`:`SS no. of tumours diagnosed`, ~ sum(.x, na.rm = TRUE))) %>%
                      mutate(`Cancer type` = "All cancers combined", .before = `Stage at diagnosis`)  
              %>% ungroup()) %>% 
        mutate(`Cancer type` = factor(`Cancer type` )) %>%
        arrange(`Cancer type`)

#' __________ 
#' Create dataframe for "volume" results (6 - Resection, chemo cyto, chemo other, rad curative, rad, palliative, rad unknown
df_volume_treatentXstage_joined_intercept <- df_treatentXstage_joined_intercept %>% 
        select(`Cancer type`:`RTDS treatment intent`, `SoC no. of tumours treated`,`Prev no. of tumours treated`,`SS no. of tumours treated`) %>%
        filter(`Treatment modality` %in% c("Tumour resection", "Chemotherapy (cytotoxic)", "Chemotherapy (other)", "Radiotherapy"))  %>% 
        # Unite the modality and intent columns
        unite("modality_SACTintent_RTDSintent", `Treatment modality`:`RTDS treatment intent`, remove = TRUE) %>%
        # group the modalities into the 6
        mutate(modality_SACTintent_RTDSintent = case_when(grepl(c("resection"),modality_SACTintent_RTDSintent) ~ "Tumour resection",
                                                          grepl(c("Chemotherapy"), modality_SACTintent_RTDSintent) &
                                                                  grepl(c("cytotoxic"), modality_SACTintent_RTDSintent)~ "Chemotherapy (cytotoxic)",
                                                          grepl(c("Chemotherapy"),modality_SACTintent_RTDSintent)&
                                                                  !grepl(c("cytotoxic"), modality_SACTintent_RTDSintent) ~ "Chemotherapy (other)",
                                                          grepl(c("Radiotherapy_Curative_Curative|Radiotherapy_Palliative_Curative|Radiotherapy_Other or unknown_Curative|Radiotherapy_Not applicable_Curative"), modality_SACTintent_RTDSintent) ~ "Radiotherapy (curative)",
                                                          grepl(c("Radiotherapy_Curative_Palliative|Radiotherapy_Palliative_Palliative|Radiotherapy_Other or unknown_Palliative|Radiotherapy_Not applicable_Palliative"),modality_SACTintent_RTDSintent) ~ "Radiotherapy (palliative)",
                                                          grepl(c("Radiotherapy_Curative_Other or unknown|Radiotherapy_Palliative_Other or unknown|Radiotherapy_Other or unknown_Other or unknown|Radiotherapy_Not applicable_Other or unknown"),modality_SACTintent_RTDSintent) ~ "Radiotherapy (other or unknown)",
                                                          .default = modality_SACTintent_RTDSintent
        )) %>%
        # Remove the Radiotherpy_Not applicable which by nature there are none of
        filter(!(modality_SACTintent_RTDSintent %in% c("Radiotherapy_Palliative_Not applicable", "Radiotherapy_Curative_Not applicable", "Radiotherapy_Other or unknown_Not applicable","Radiotherapy_Not applicable_Not applicable"))) %>%
        mutate(across(where(is.character), as.factor)) %>%
        ## order the columns
        select(`Cancer type`, `Stage at diagnosis`, `modality_SACTintent_RTDSintent`,
               `SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        # group by these newly created categories and then mutate and slice (rather than summarise)
        group_by(`Cancer type`,`Stage at diagnosis`, `modality_SACTintent_RTDSintent`) %>%
        mutate(`SoC no. of tumours treated` = sum(`SoC no. of tumours treated`),
               `Prev no. of tumours treated` = sum(`Prev no. of tumours treated`),
               `SS no. of tumours treated` = sum(`SS no. of tumours treated`)) %>%
        slice(1) %>%
        ungroup() %>%
        # Create the "all cancers" group and join to the above dataset
        rbind(.,  (.) %>% 
                      group_by(`Stage at diagnosis`, `modality_SACTintent_RTDSintent`) %>%
                      summarise(across(`SoC no. of tumours treated`:`SS no. of tumours treated`, ~ sum(.x, na.rm = TRUE))) %>%
                      mutate(`Cancer type` = "All cancers combined", .before = `Stage at diagnosis`)  %>% ungroup()) %>% 
        mutate(`Cancer type` = fct_relevel(`Cancer type` , "All cancers combined")) %>%
        arrange(`Cancer type`) %>%
        # Add on number diagnosed back on
        left_join(df_num_diagnosed_bytype_bystage, by = c("Cancer type", "Stage at diagnosis"))

## Check to make sure all cancer numbers equal what they should YES
check <- df_volume_treatentXstage_joined_intercept %>% filter(`Cancer type` == "All cancers combined") %>%
        summarise(across(`SoC no. of tumours treated`:`SS no. of tumours treated`, ~ sum(.x, na.rm = TRUE)))

check2 <- df_volume_treatentXstage_joined_intercept %>% filter(`Cancer type` != "All cancers combined") %>%
        summarise(across(`SoC no. of tumours treated`:`SS no. of tumours treated`, ~ sum(.x, na.rm = TRUE)))

#' __________ 
# Create dataframe for ALL combinations of treatments
df_combined_treatentXstage_joined_intercept <- df_treatentXstage_joined_intercept %>% 
        select(`Cancer type`:`RTDS treatment intent`, `SoC no. of tumours treated`,`Prev no. of tumours treated`,`SS no. of tumours treated`) %>%
        # Remove the volume categories
        filter(!(`Treatment modality` %in% c("Tumour resection", "Chemotherapy (cytotoxic)", "Chemotherapy (other)", "Radiotherapy"))) %>%
        # Remove rows which do not make sense (do check they are zero though)
        # Take out the rows with Tumour resection on but with Curative|Palliative|Other or unknown RTDs and SACT (i.e. keep the not applicables)
        filter(!(`Treatment modality` ==  "Tumour resection only" & (grepl(c("Curative|Palliative|Other or unknown"), `SACT treatment intent`) | grepl(c("Curative|Palliative|Other or unknown"), `RTDS treatment intent`) ))) %>%
        # Take out combinations which involve chemotherapy but SACT is not applicable
        filter(!(grepl(c("Chemotherapy|chemotherapy"), `Treatment modality`) & grepl(c("cytotoxic|other"), `Treatment modality`) & grepl(c("Not applicable"), `SACT treatment intent`))) %>%
        # Take out combinations which involve radiotherapy but RTDS is not applicable
        filter(!(grepl(c("Radiotherapy|radiotherapy"), `Treatment modality`)  & grepl(c("Not applicable"), `RTDS treatment intent`))) %>%
        # Take out combinations which do no invole radiotherapy but have positive RTDS intent
        filter(!((!(grepl(c("Radiotherapy|radiotherapy"), `Treatment modality`))  & grepl(c("Curative|Palliative|Other or unknown"), `RTDS treatment intent`)))) %>%
        # Take out combinations which do no invole Chemotherapy but have positive RTDS intent
        filter(!((!(grepl(c("Chemotherapy|chemotherapy"), `Treatment modality`))  & grepl(c("Curative|Palliative|Other or unknown"), `SACT treatment intent`)))) %>%
        # Combine and clean names
        unite("combination_SACTintent_RTDSintent", `Treatment modality`:`SACT treatment intent`,  sep='_SACT intent ', remove = FALSE) %>%
        mutate(combination_SACTintent_RTDSintent = case_when(`SACT treatment intent` == "Not applicable" ~ str_extract(combination_SACTintent_RTDSintent, "[^_]+"),
                                                             .default = combination_SACTintent_RTDSintent)) %>%
        unite("combination_SACTintent_RTDSintent", c("combination_SACTintent_RTDSintent", `RTDS treatment intent`),  sep='-RTDS intent ', remove = FALSE) %>%
        mutate(combination_SACTintent_RTDSintent = case_when(`RTDS treatment intent` == "Not applicable" ~ str_extract(combination_SACTintent_RTDSintent, "[^-]+"),
                                                             .default = combination_SACTintent_RTDSintent)) %>%
        select(-`RTDS treatment intent`, -`SACT treatment intent`) %>%
        mutate(across(where(is.character), as.factor)) %>%
        ## order the columns
        select(`Cancer type`, `Stage at diagnosis`, `combination_SACTintent_RTDSintent`,
               `SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        # Create the "all cancers" group and join the the above dataset
        rbind(.,  (.) %>% 
                      group_by(`Stage at diagnosis`, `combination_SACTintent_RTDSintent`) %>%
                      summarise(across(`SoC no. of tumours treated`:`SS no. of tumours treated`, ~ sum(.x, na.rm = TRUE))) %>%
                      mutate(`Cancer type` = "All cancers combined", .before = `Stage at diagnosis`) %>%
                      ungroup()) %>% 
        mutate(`Cancer type` = fct_relevel(`Cancer type` , "All cancers combined")) %>%
        arrange(`Cancer type`) %>%
        # Join back on the diagnosed count
        left_join(df_num_diagnosed_bytype_bystage, by = c("Cancer type", "Stage at diagnosis"))

## Check to make sure all cancer numbers equal what they should (yes) - the checks are the same, and they equal number diagnosed in above dataframe
check <- df_combined_treatentXstage_joined_intercept   %>% 
        group_by(`Cancer type`, `Stage at diagnosis`) %>%
        summarise(across(`SoC no. of tumours treated`:`SS no. of tumours treated`, ~ sum(.x, na.rm = TRUE))) %>%
        filter(`Cancer type` == "All cancers combined") 


check2 <- df_combined_treatentXstage_joined_intercept   %>% 
        filter(`Cancer type` != "All cancers combined") %>%
        group_by(`Stage at diagnosis`) %>%
        summarise(across(`SoC no. of tumours treated`:`SS no. of tumours treated`, ~ sum(.x, na.rm = TRUE)))

#' __________ 
## Repeat for IMD - unchanged in new data
df_England_treatmentXstageIMD_14to19 <- readxl::read_xlsx("~/Miscellaneous/Treatments/20241205_LibbyEllis_GRAIL_ShiftedTreatmentPathwaysData_ext.xlsx", sheet = "Treat Stage X IMD") %>%
        mutate(`Number of tumours treated` = as.numeric(`Number of tumours treated`)) %>%
        mutate(`Annual number of tumours diagnosed` = `Number of tumours diagnosed`/ 6) %>%
        mutate(`Annual number of tumours treated` = `Number of tumours treated`/ 6) %>%
        mutate(`Cancer type` = case_when(`Cancer type` == "Headneck" ~ "Head and Neck",
                                         `Cancer type` == "Liver" ~ "Liver/Bile-duct",
                                         `Cancer type` == "Oesophagus" ~ "Esophagus",
                                         `Cancer type` == "Urothelial" ~ 	"Urothelial Tract",
                                         .default = `Cancer type`)) %>%
        select(-c( `Annual number of tumours diagnosed`, `Number of tumours diagnosed`, `Number of tumours treated`, `Proportion`:`UCI`)) 


df_treatentXstageIMD_joined_intercept <- df_England_treatmentXstageIMD_14to19 %>%
        left_join(df_England_IncXIMD_joined_intercept, by = c("Index of Multiple Deprivation Quintile", "Cancer type" , "Stage at diagnosis" )) %>%
        ungroup() %>% 
        mutate(`Prev no. of tumours treated` = `Annual number of tumours treated` * (`Prev no. of tumours diagnosed`/`SoC no. of tumours diagnosed`),
               `SS no. of tumours treated` = `Annual number of tumours treated` * (`SS no. of tumours diagnosed`/`SoC no. of tumours diagnosed`)) %>%
        rename(`SoC no. of tumours treated` = `Annual number of tumours treated`) 


#' Create a number diagnosed by cancer type and stage data set for later joining on
df_IMDnum_diagnosed_bytype_bystage <- df_treatentXstageIMD_joined_intercept %>%
        group_by(`Cancer type`,`Stage at diagnosis`, `Index of Multiple Deprivation Quintile`) %>%
        slice(1) %>%
        select(`Cancer type`,`Stage at diagnosis`, `Index of Multiple Deprivation Quintile`, `SoC no. of tumours diagnosed`, `Prev no. of tumours diagnosed`, `SS no. of tumours diagnosed`) %>%
        # Create all cancer rows
        rbind(.,  (.) %>% 
                      group_by(`Stage at diagnosis`, `Index of Multiple Deprivation Quintile`) %>%
                      summarise(across(`SoC no. of tumours diagnosed`:`SS no. of tumours diagnosed`, ~ sum(.x, na.rm = TRUE))) %>%
                      mutate(`Cancer type` = "All cancers combined", .before = `Stage at diagnosis`)  
              %>% ungroup()) %>% 
        mutate(`Cancer type` = factor(`Cancer type` )) %>%
        arrange(`Cancer type`)

df_3treatentXstageIMD_joined_intercept <- df_treatentXstageIMD_joined_intercept %>% 
        select(`Cancer type`:`Treatment modality`,`SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        filter(`Treatment modality` %in% c("Tumour resection", "Chemotherapy", "Radiotherapy"))  %>%
        mutate(across(where(is.character), as.factor)) %>%
        select(`Cancer type`, `Stage at diagnosis`, `Index of Multiple Deprivation Quintile`,`Treatment modality`,
               `SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        rbind(.,  (.) %>% 
                      group_by(`Stage at diagnosis`, `Index of Multiple Deprivation Quintile`, `Treatment modality`) %>%
                      summarise(across(`SoC no. of tumours treated`:`SS no. of tumours treated`, ~ sum(.x, na.rm = TRUE))) %>%
                      mutate(`Cancer type` = "All cancers combined", .before = `Stage at diagnosis`) %>% ungroup()) %>% 
        mutate(`Cancer type` = fct_relevel(`Cancer type` , "All cancers combined")) %>%
        arrange(`Cancer type`) %>%   # Join back on the diagnosed count
        left_join(df_IMDnum_diagnosed_bytype_bystage, by = c("Cancer type", "Stage at diagnosis", "Index of Multiple Deprivation Quintile"))

df_8treatentXstageIMD_joined_intercept <- df_treatentXstageIMD_joined_intercept %>% 
        select(`Cancer type`:`Treatment modality`,`SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        filter(!(`Treatment modality` %in% c("Tumour resection", "Chemotherapy", "Radiotherapy"))) %>% 
        mutate(across(where(is.character), as.factor)) %>%
        ## order the columns
        select(`Cancer type`, `Stage at diagnosis`, `Index of Multiple Deprivation Quintile`, `Treatment modality`,
               `SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        rbind(.,  (.) %>% 
                      group_by(`Stage at diagnosis`, `Index of Multiple Deprivation Quintile`,`Treatment modality`) %>%
                      summarise(across(`SoC no. of tumours treated`:`SS no. of tumours treated`, ~ sum(.x, na.rm = TRUE))) %>%
                      mutate(`Cancer type` = "All cancers combined", .before = `Stage at diagnosis`) %>% ungroup()) %>% 
        mutate(`Cancer type` = fct_relevel(`Cancer type` , "All cancers combined")) %>%
        arrange(`Cancer type`) %>%   # Join back on the diagnosed count
        left_join(df_IMDnum_diagnosed_bytype_bystage, by = c("Cancer type", "Stage at diagnosis", "Index of Multiple Deprivation Quintile"))


#' __________ 
#' Repeat for Charlson score - note, no differentiation by stage
# Therefore need to create an incidence dataframe of all stages
df_England_Inc_joined_intercept_allstage <- df_England_Inc_joined_intercept %>% 
        select(`Cancer type`, `Stage at diagnosis` , contains("no. of tumours diagnosed")) %>%
        group_by(`Cancer type`) %>%
        summarise(across(contains("no. of tumours diagnosed"), \(x) sum(x)))

# Load in treatment data
df_England_treatmentXCharlson_14to19 <- readxl::read_xlsx("~/Miscellaneous/Treatments/20241205_LibbyEllis_GRAIL_ShiftedTreatmentPathwaysData_ext.xlsx", sheet = "Treat Charlson") %>%
        mutate(`Number of tumours treated` = as.numeric(`Number of tumours treated`)) %>%
        mutate(`Annual number of tumours diagnosed` = `Number of tumours diagnosed`/ 6) %>%
        mutate(`Annual number of tumours treated` = `Number of tumours treated`/ 6) %>%
        mutate(`Cancer type` = case_when(`Cancer type` == "Colorectal" ~ "Colon/Rectum",
                                         `Cancer type` == "Headneck" ~ "Head and Neck",
                                         `Cancer type` == "Liver" ~ "Liver/Bile-duct",
                                         `Cancer type` == "Oesophagus" ~ "Esophagus",
                                         `Cancer type` == "Urothelial" ~ 	"Urothelial Tract",
                                         .default = `Cancer type`)) %>%
        select(-c( `Number of tumours diagnosed`, `Number of tumours treated`, `Proportion`:`UCI`))

# Join to all stage dataframe
df_treatentXCharlson_joined_intercept <- df_England_treatmentXCharlson_14to19 %>%
        # Merge on the incidence and stage shift dataframe
        left_join(df_England_Inc_joined_intercept_allstage, by = c("Cancer type" )) %>%
        # For each treatment type, calculate numbers for the prev and SS rounds 
        mutate(`Prev no. of tumours treated` = `Annual number of tumours treated` * (`Prev no. of tumours diagnosed`/`SoC no. of tumours diagnosed`),
               `SS no. of tumours treated` = `Annual number of tumours treated` * (`SS no. of tumours diagnosed`/`SoC no. of tumours diagnosed`)) %>%
        rename(`SoC no. of tumours treated` = `Annual number of tumours treated`)

# Seperate dataframe of total diagnosed BY Charlson
df_Charlsonnum_diagnosed_bytype_bystage <- df_treatentXCharlson_joined_intercept %>%
        group_by(`Cancer type`, `Charlson comorbidity index`) %>%
        slice(1) %>%
        # Current prev and SS is for all CCIs, use the Ratio (from ratio of IRs) to get by CCI - this is aggregate across al stages
        mutate(`Prev no. of tumours diagnosed` = `Annual number of tumours diagnosed` * (`Prev no. of tumours diagnosed`/`SoC no. of tumours diagnosed`),
               `SS no. of tumours diagnosed` = `Annual number of tumours diagnosed` * (`SS no. of tumours diagnosed`/`SoC no. of tumours diagnosed`)) %>%
        select(- `SoC no. of tumours diagnosed`) %>%
        rename(`SoC no. of tumours diagnosed` = `Annual number of tumours diagnosed`) %>%
        select(`Cancer type`, `Charlson comorbidity index`, `SoC no. of tumours diagnosed`, `Prev no. of tumours diagnosed`, `SS no. of tumours diagnosed`) %>%
        # Create all cancer rows
        rbind(.,  (.) %>% 
                      group_by(`Charlson comorbidity index`) %>%
                      summarise(across(`SoC no. of tumours diagnosed`:`SS no. of tumours diagnosed`, ~ sum(.x, na.rm = TRUE))) %>%
                      mutate(`Cancer type` = "All cancers combined", .before = `Charlson comorbidity index`)  
              %>% ungroup()) %>% 
        mutate(`Cancer type` = factor(`Cancer type` )) %>%
        arrange(`Cancer type`)

# Create dataframe for "volume" results (anyy resection/chemo/radio)
df_3treatentXCharlson_joined_intercept <- df_treatentXCharlson_joined_intercept %>% 
        select(`Cancer type`:`Treatment modality`,`SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        filter(`Treatment modality` %in% c("Tumour resection", "Chemotherapy", "Radiotherapy"))  %>% 
        mutate(across(where(is.character), as.factor))%>% 
        ## order the columns
        select(`Cancer type`, `Charlson comorbidity index`, `Treatment modality`,
               `SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        rbind(.,  (.) %>% 
                      group_by(`Charlson comorbidity index`, `Treatment modality`) %>%
                      summarise(across(`SoC no. of tumours treated`:`SS no. of tumours treated`, ~ sum(.x, na.rm = TRUE))) %>%
                      mutate(`Cancer type` = "All cancers combined", .before = `Charlson comorbidity index`) %>% ungroup()) %>% 
        mutate(`Cancer type` = fct_relevel(`Cancer type` , "All cancers combined")) %>%
        arrange(`Cancer type`) %>%   # Join back on the diagnosed count
        left_join(df_Charlsonnum_diagnosed_bytype_bystage, by = c("Cancer type", "Charlson comorbidity index"))

# Create dataframe for combinations of treatments
df_8treatentXCharlson_joined_intercept <- df_treatentXCharlson_joined_intercept %>%
        select(`Cancer type`:`Treatment modality`,`SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        filter(!(`Treatment modality` %in% c("Tumour resection", "Chemotherapy", "Radiotherapy"))) %>% 
        mutate(across(where(is.character), as.factor)) %>%
        ## order the columns
        select(`Cancer type`, `Charlson comorbidity index`, `Treatment modality`,
               `SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        rbind(.,  (.) %>% 
                      group_by(`Charlson comorbidity index`, `Treatment modality`) %>%
                      summarise(across(`SoC no. of tumours treated`:`SS no. of tumours treated`, ~ sum(.x, na.rm = TRUE))) %>%
                      mutate(`Cancer type` = "All cancers combined", .before = `Charlson comorbidity index`) %>% ungroup()) %>% 
        mutate(`Cancer type` = fct_relevel(`Cancer type` , "All cancers combined")) %>%
        arrange(`Cancer type`) %>%   # Join back on the diagnosed count
        left_join(df_Charlsonnum_diagnosed_bytype_bystage, by = c("Cancer type", "Charlson comorbidity index"))

# Check that when grouped into cancer type, this numbers treated align with number diagnosed (df_England_Inc_joined_intercept_allstage)
 check <- df_8treatentXCharlson_joined_intercept   %>% 
        filter(`Cancer type` != "All cancers combined") %>%
        group_by(`Cancer type`) %>%
        summarise(across(`SoC no. of tumours treated`:`SS no. of tumours treated`, ~ sum(.x, na.rm = TRUE)))

#' __________
#' Repeat for cancer alliance split

df_England_treatmentXstageCA_14to19 <- readxl::read_xlsx("~/Miscellaneous/Treatments/20241205_LibbyEllis_GRAIL_ShiftedTreatmentPathwaysData_ext.xlsx", sheet = "Treat Stage X Alliance") %>%
        mutate(`Number of tumours treated` = as.numeric(`Number of tumours treated`)) %>%
        mutate(`Annual number of tumours diagnosed` = `Number of tumours diagnosed`/ 6) %>%
        mutate(`Annual number of tumours treated` = `Number of tumours treated`/ 6) %>%
        mutate(`Cancer type` = case_when(`Cancer type` == "Colorectal" ~ "Colon/Rectum",
                                         `Cancer type` == "Headneck" ~ "Head and Neck",
                                         `Cancer type` == "Liver" ~ "Liver/Bile-duct",
                                         `Cancer type` == "Oesophagus" ~ "Esophagus",
                                         `Cancer type` == "Urothelial" ~ 	"Urothelial Tract",
                                         .default = `Cancer type`)) %>%
        select(-c( `Annual number of tumours diagnosed`, `Number of tumours diagnosed`, `Number of tumours treated`, `Proportion`:`UCI`)) 

df_treatentXstageCA_joined_intercept <- df_England_treatmentXstageCA_14to19 %>%
        left_join(df_England_IncXCA_joined_intercept, by = c("Cancer Alliance" ,"Cancer type" , "Stage at diagnosis" )) %>%
        ungroup() %>% 
        mutate(`Prev no. of tumours treated` = `Annual number of tumours treated` * (`Prev no. of tumours diagnosed`/`SoC no. of tumours diagnosed`),
               `SS no. of tumours treated` = `Annual number of tumours treated` * (`SS no. of tumours diagnosed`/`SoC no. of tumours diagnosed`)) %>%
        rename(`SoC no. of tumours treated` = `Annual number of tumours treated`) 

#' Create a number diagnosed by cancer type and stage data set for later joining on
df_CAnum_diagnosed_bytype_bystage <- df_treatentXstageCA_joined_intercept %>%
        group_by(`Cancer type`,`Stage at diagnosis`, `Cancer Alliance` ) %>%
        slice(1) %>%
        select(`Cancer type`,`Stage at diagnosis`, `Cancer Alliance` , `SoC no. of tumours diagnosed`, `Prev no. of tumours diagnosed`, `SS no. of tumours diagnosed`) %>%
        # Create all cancer rows
        rbind(.,  (.) %>% 
                      group_by(`Stage at diagnosis`, `Cancer Alliance` ) %>%
                      summarise(across(`SoC no. of tumours diagnosed`:`SS no. of tumours diagnosed`, ~ sum(.x, na.rm = TRUE))) %>%
                      mutate(`Cancer type` = "All cancers combined", .before = `Stage at diagnosis`)  
              %>% ungroup()) %>% 
        mutate(`Cancer type` = factor(`Cancer type` )) %>%
        arrange(`Cancer type`)

df_3treatentXstageCA_joined_intercept <- df_treatentXstageCA_joined_intercept %>% 
        select(`Cancer type`:`Treatment modality`,`SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        filter(`Treatment modality` %in% c("Tumour resection", "Chemotherapy", "Radiotherapy"))  %>%
        mutate(across(where(is.character), as.factor))%>% 
        ## order the columns
        select(`Cancer Alliance`, `Cancer type`, `Stage at diagnosis`, `Treatment modality`,
               `SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%   # Join back on the diagnosed count
        left_join(df_CAnum_diagnosed_bytype_bystage, by = c("Cancer type", "Stage at diagnosis", "Cancer Alliance"))


df_8treatentXstageCA_joined_intercept <- df_treatentXstageCA_joined_intercept %>% 
        select(`Cancer type`:`Treatment modality`,`SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        filter(!(`Treatment modality` %in% c("Tumour resection", "Chemotherapy", "Radiotherapy"))) %>% 
        mutate(across(where(is.character), as.factor)) %>%
        ## order the columns
        select(`Cancer Alliance`,`Cancer type`, `Stage at diagnosis`, `Treatment modality`,
               `SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%  # Join back on the diagnosed count
left_join(df_CAnum_diagnosed_bytype_bystage, by = c("Cancer type", "Stage at diagnosis", "Cancer Alliance"))


#' __________
# 3.1 Total volume (resection, chemotherapy, radiotherapy)
### Table 3.1 Total annual treatment volume with usual care (N, %) and change with stage shift from MCED screening (N) by cancer type and stage - prevalent round and steady state

df_table_3.1 <- df_volume_treatentXstage_joined_intercept %>%
        # calculate % of patients who receive treatment modality in their treatment
        mutate(`SoC % treated` = paste0("(", round((`SoC no. of tumours treated` / `SoC no. of tumours diagnosed`) * 100), ")")) %>%
        mutate(across(where(is.numeric), \(x) round(x, 1))) %>%
        # Probably dont actually need this percentage but was specified before
        unite("SoC no. of tumours treated (%)" ,  c('SoC no. of tumours treated', 'SoC % treated'), sep=' ' ) %>%
        # rename back to treatment modality here
        rename(`Treatment modality` = modality_SACTintent_RTDSintent) %>%
        select(`Cancer type`, `Stage at diagnosis`, `Treatment modality`, `SoC no. of tumours treated (%)` , `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        rename(`SoC` = `SoC no. of tumours treated (%)`,
               `Prev round`= `Prev no. of tumours treated`,
               `SS`= `SS no. of tumours treated`) %>%
        pivot_wider(names_from = `Treatment modality`, values_from = c(`SoC`, `Prev round`, `SS`), names_glue ="{`Treatment modality`}: {.value} no.") %>%
        rename_with(~str_c(., " (%)"), contains("SoC")) %>%
        select(`Cancer type`, `Stage at diagnosis`, contains("Tumour resection"), contains("Chemotherapy (cytotoxic)"), 
               contains("Chemotherapy (other)"), contains("Radiotherapy (curative)"), contains("Radiotherapy (palliative)"), contains("Radiotherapy (other or unknown)"))

# GT table for word output
gt::gt(df_table_3.1 %>% arrange(`Stage at diagnosis`)) %>%
        tab_spanner_delim(delim = ":", columns = `Tumour resection: SoC no. (%)`:`Radiotherapy (other or unknown): SS no.` ,
                          split = c("first", "last"), limit = NULL, reverse = FALSE)

# Excel output
addWorksheet(excel_output, sheetName = "3.1. Tx vol (N) prev&SS")
writeData(excel_output, sheet = "3.1. Tx vol (N) prev&SS", df_table_3.1)

#' __________
# 3.2 Treatment combinations

### Table 3.2.1 Standard of care distribution of treatment modality combinations (N, %) by cancer type and stage (NCRAS data)

# First, present the SoC distribution of treatment combinations
df_table_3.2.1 <- df_combined_treatentXstage_joined_intercept %>%
        mutate(`SoC % of total tumour treated` = (`SoC no. of tumours treated` / `SoC no. of tumours diagnosed`) * 100) %>%
        rename(`Treatment modality`= combination_SACTintent_RTDSintent) %>%
        select(`Cancer type`, `Stage at diagnosis`, `Treatment modality`, `SoC % of total tumour treated`) %>%
        pivot_wider(names_from = `Treatment modality`, values_from = c(`SoC % of total tumour treated`), names_glue ="{`Treatment modality`}: % of SoC treatments") %>%
        # The NA cells is where the combination didnt appear int he data presumably because there are zero counts
        replace(is.na(.), 0) %>%
        # Check the rows add to 100% approx (rounding above)
        #mutate(check = rowSums(across(where(is.numeric))))
        select(where(~ !is.numeric(.x) || sum(.x) !=0 )) %>%
        select(`Cancer type`, `Stage at diagnosis`, contains("Tumour resection only"), contains("Chemotherapy (cytotoxic) only"), contains("Chemotherapy (other) only"),
               contains("Radiotherapy only"), contains("Tumour resection and chemotherapy"),
               contains("Tumour resection and radiotherapy"), contains("Chemotherapy (cytotoxic) and radiotherapy"), contains("Chemotherapy (other) and radiotherapy"),
               contains("Tumour resection, radiotherapy and chemotherapy (cytotoxic)"), contains("Tumour resection, radiotherapy and chemotherapy (other)"), contains("Other care")) %>%
        mutate(across(where(is.numeric), \(x) round(x, 3)))
        
# GT table for word output
gt::gt(df_table_3.2.1) %>%
        tab_spanner_delim(delim = ":", columns = everything(), split = c("last", "first"), limit = NULL, reverse = TRUE)

# Excel output
addWorksheet(excel_output, sheetName = "3.2.1 NCRAS Tx combo")
writeData(excel_output, sheet = "3.2.1 NCRAS Tx combo", df_table_3.2.1)

saveWorkbook(excel_output, paste0("~/Miscellaneous/Treatments/", uptake,"%_participation_results_tables_newdata.xlsx"), overwrite = TRUE)

#' __________
### Table 3.2.2 Distribution of treatment modality combinations with stage shift from MCED screening (N) by cancer type and stage - prevalent round & steady-state

df_table_3.2.2 <- df_combined_treatentXstage_joined_intercept %>%
        rename(`Treatment modality`= combination_SACTintent_RTDSintent) %>%
        select(`Cancer type`, `Stage at diagnosis`, `Treatment modality`, `SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        rename(`SoC` = `SoC no. of tumours treated`,
               `Prev round`= `Prev no. of tumours treated`,
               `SS`= `SS no. of tumours treated`) %>%
        pivot_wider(names_from = `Treatment modality`, values_from = c(`SoC`, `Prev round`, `SS`), names_glue ="{`Treatment modality`}: {.value} (no.)") %>%
        mutate(across(where(is.numeric), \(x) round(x, 1))) %>%
        select(`Cancer type`, `Stage at diagnosis`, contains("Tumour resection only"), contains("Tumour resection and chemotherapy"),
               contains("Tumour resection and radiotherapy"), contains("Tumour resection, radiotherapy and chemotherapy"),
               contains("Chemotherapy and radiotherapy"), contains("Radiotherapy only"),
               contains("Chemotherapy only"), contains("Other care"))

gt::gt(df_table_3.2.2) %>%
        tab_spanner_delim(delim = ":", columns = everything(), split = c("first", "last"), limit = NULL, reverse = FALSE)

addWorksheet(excel_output, sheetName = "3.2.2. Tx combi (N) prev&SS")
writeData(excel_output, sheet = "3.2.2. Tx combi (N) prev&SS", df_table_3.2.2)  

#' __________
### Table 3.3.1 Distribution of SoC total treatment volume (N, %) by IMD quintile, stage and cancer type

df_table_3.3.1 <- df_3treatentXstageIMD_joined_intercept %>%
        # calculate % of patients who receive treatment modality in their treatment
        mutate(`SoC % treated` = paste0("(", round((`SoC no. of tumours treated` / `SoC no. of tumours diagnosed`) * 100), ")")) %>%
        mutate(across(where(is.numeric), \(x) round(x, 1))) %>%
        unite("SoC no. of tumours treated (%)" ,  c('SoC no. of tumours treated', 'SoC % treated'), sep=' ' ) %>%
        select(`Index of Multiple Deprivation Quintile`,`Cancer type`, `Stage at diagnosis`, `Treatment modality`, `SoC no. of tumours treated (%)`) %>%
        # Organise with IMD as columns for comparability
        pivot_wider(names_from = `Index of Multiple Deprivation Quintile`, values_from = c(`SoC no. of tumours treated (%)`), names_glue ="IMD Quntile {`Index of Multiple Deprivation Quintile`}: {.value}") 

# GT table for word output
gt::gt(df_table_3.3.1) %>%
        tab_spanner_delim(delim = ":", columns = `IMD Quntile 1 - most deprived: SoC no. of tumours treated (%)`:`IMD Quntile 5 - least deprived: SoC no. of tumours treated (%)` ,
                          split = c("first", "last"), limit = NULL, reverse = TRUE)

# Excel output
addWorksheet(excel_output, sheetName = "3.3.1 NCRAS Tx vol X IMD")
writeData(excel_output, sheet = "3.3.1 NCRAS Tx vol X IMD", df_table_3.3.1)

#' __________
### Table 3.3.2 Distribution of SoC treatment modality combinations (N, %) by IMD quintile, stage and cancer type

# SoC distribution of treatment combinations
df_table_3.3.2 <- df_8treatentXstageIMD_joined_intercept %>%
        mutate(`SoC % treated` = paste0("(", round((`SoC no. of tumours treated` / `SoC no. of tumours diagnosed`) * 100), ")")) %>%
        mutate(across(where(is.numeric), \(x) round(x, 1))) %>%
        unite("SoC no. of tumours treated (%)" ,  c('SoC no. of tumours treated', 'SoC % treated'), sep=' ' ) %>%
        select(`Index of Multiple Deprivation Quintile`,`Cancer type`, `Stage at diagnosis`, `Treatment modality`, `SoC no. of tumours treated (%)`) %>%
        # Organise with IMD as columns for comparability
        pivot_wider(names_from = `Index of Multiple Deprivation Quintile`, values_from = c(`SoC no. of tumours treated (%)`), names_glue ="IMD Quntile {`Index of Multiple Deprivation Quintile`}: {.value}") %>%
        mutate(`Treatment modality` = factor(`Treatment modality`, levels = c("Tumour resection only","Tumour resection and chemotherapy",
                                                                              "Tumour resection and radiotherapy","Tumour resection, radiotherapy and chemotherapy",
                                                                              "Chemotherapy and radiotherapy",
                                                                              "Radiotherapy only","Chemotherapy only", "Other care"))) %>%
        group_by(`Cancer type`, `Stage at diagnosis`) %>%
        arrange(`Treatment modality`, .by_group = TRUE ) %>%
        ungroup()

# GT table for word output
gt::gt(df_table_3.3.2) %>%
        tab_spanner_delim(delim = ":", columns = everything(), split = c("last", "first"), limit = NULL, reverse = TRUE)

# Excel output
addWorksheet(excel_output, sheetName = "3.3.2 NCRAS Tx combo X IMD")
writeData(excel_output, sheet = "3.3.2 NCRAS Tx combo X IMD", df_table_3.3.2)

#' __________
### Table 3.4.1 Distribution of SoC total treatment volume (N, %) by comorbidity score and cancer type.
df_table_3.4.1 <- df_3treatentXCharlson_joined_intercept %>%
        # calculate % of patients who receive treatment modality in their treatment
        mutate(`SoC % treated` = paste0("(", round((`SoC no. of tumours treated` / `SoC no. of tumours diagnosed`) * 100), ")")) %>%
        mutate(across(where(is.numeric), \(x) round(x, 1))) %>%
        unite("SoC no. of tumours treated (%)" ,  c('SoC no. of tumours treated', 'SoC % treated'), sep=' ' ) %>%
        select(`Charlson comorbidity index`,`Cancer type`, `Treatment modality`, `SoC no. of tumours treated (%)`) %>%
        # Organise with IMD as columns for comparability
        pivot_wider(names_from = `Charlson comorbidity index`, values_from = c(`SoC no. of tumours treated (%)`), names_glue ="Charlson Comorbidity Index score {`Charlson comorbidity index`}: {.value}") %>%
        group_by(`Cancer type`) %>%
        arrange(`Treatment modality`, .by_group = TRUE)

# GT table for word output
gt::gt(df_table_3.4.1) %>%
        tab_spanner_delim(delim = ":", columns = `Charlson Comorbidity Index score 0: SoC no. of tumours treated (%)`:`Charlson Comorbidity Index score 3+: SoC no. of tumours treated (%)` ,
                          split = c("first", "last"), limit = NULL, reverse = TRUE)

# Excel output
addWorksheet(excel_output, sheetName = "3.4.1 NCRAS Tx vol X CCI")
writeData(excel_output, sheet = "3.4.1 NCRAS Tx vol X CCI", df_table_3.4.1)

#' __________
### Table 3.4.2 Distribution of SoC treatment modality combinations (N, %) by comorbidity score, stage and cancer type. N.B. % is for cancer total not by CCI

# SoC distribution of treatment combinations
df_table_3.4.2 <- df_8treatentXCharlson_joined_intercept %>%
        mutate(`SoC % treated` = paste0("(", round((`SoC no. of tumours treated` / `SoC no. of tumours diagnosed`) * 100), ")")) %>%
        mutate(across(where(is.numeric), \(x) round(x, 1))) %>%
        unite("SoC no. of tumours treated (%)" ,  c('SoC no. of tumours treated', 'SoC % treated'), sep=' ' ) %>%
        select(`Charlson comorbidity index`,`Cancer type`, `Treatment modality`, `SoC no. of tumours treated (%)`) %>%
        # Organise with CCI as columns for comparability
        pivot_wider(names_from = `Charlson comorbidity index`, values_from = c(`SoC no. of tumours treated (%)`), names_glue ="Charlson Comorbidity Index score {`Charlson comorbidity index`}: {.value}") %>%
        mutate(`Treatment modality` = factor(`Treatment modality`, levels = c("Tumour resection only","Tumour resection and chemotherapy",
                                                                              "Tumour resection and radiotherapy","Tumour resection, radiotherapy and chemotherapy",
                                                                              "Chemotherapy and radiotherapy",
                                                                              "Radiotherapy only","Chemotherapy only", "Other care"))) %>%
        group_by(`Cancer type`) %>%
        arrange(`Treatment modality`, .by_group = TRUE ) %>%
        ungroup()

# GT table for word output
gt::gt(df_table_3.4.2) %>%
        tab_spanner_delim(delim = ":", columns = everything(), split = c("last", "first"), limit = NULL, reverse = TRUE)


# Excel output
addWorksheet(excel_output, sheetName = "3.4.2 NCRAS Tx combo X CCI")
writeData(excel_output, sheet = "3.4.2 NCRAS Tx combo X CCI", df_table_3.4.2)

#' _____________________________________________________________________________
# 4. Results (1/2) Main summary tables
## 4.1 Resection, chemotherapy and radiotherapy treatment volumes
### Table 4.1.1 Overall annual treatment volume for each cancer (all stages) and % change with MCED screening - prevalent round
df_table_4.1.1 <- df_volume_treatentXstage_joined_intercept %>%
        rename(`Treatment modality` = modality_SACTintent_RTDSintent) %>%
        select(`Cancer type`, `Stage at diagnosis`, `Treatment modality`, `SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        rename(`SoC` = `SoC no. of tumours treated`,
               `Prev round`= `Prev no. of tumours treated`,
               `SS`= `SS no. of tumours treated`) %>%
        pivot_wider(names_from = `Treatment modality`, values_from = c(`SoC`, `Prev round`, `SS`), names_glue ="{`Treatment modality`}: {.value} (no.)") %>%
        ## Added 30/04/2025
        mutate(`Radiotherapy (combined intent): SoC (no.)` = `Radiotherapy (curative): SoC (no.)` + `Radiotherapy (palliative): SoC (no.)` + `Radiotherapy (other or unknown): SoC (no.)`,
               `Radiotherapy (combined intent): Prev round (no.)` = `Radiotherapy (curative): Prev round (no.)` + `Radiotherapy (palliative): Prev round (no.)` + `Radiotherapy (other or unknown): Prev round (no.)`,
               `Radiotherapy (combined intent): SS (no.)` = `Radiotherapy (curative): SS (no.)` + `Radiotherapy (palliative): SS (no.)` + `Radiotherapy (other or unknown): SS (no.)`) %>%
        ##
        group_by(`Cancer type`) %>%
        summarise(across(contains(":"), \(x) sum(x))) %>%
        select(`Cancer type`, contains("SoC"), contains("Prev")) %>%
        mutate(`Tumour resection: Change (%)` = ((`Tumour resection: Prev round (no.)`-`Tumour resection: SoC (no.)`)/`Tumour resection: SoC (no.)`) * 100,
               `Chemotherapy (cytotoxic): Change (%)` = ((`Chemotherapy (cytotoxic): Prev round (no.)`-`Chemotherapy (cytotoxic): SoC (no.)`)/`Chemotherapy (cytotoxic): SoC (no.)`) * 100,
               `Chemotherapy (other): Change (%)` = ((`Chemotherapy (other): Prev round (no.)`-`Chemotherapy (other): SoC (no.)`)/`Chemotherapy (other): SoC (no.)`) * 100,
               `Radiotherapy (curative): Change (%)` = ((`Radiotherapy (curative): Prev round (no.)`-`Radiotherapy (curative): SoC (no.)`)/`Radiotherapy (curative): SoC (no.)`) * 100,
               `Radiotherapy (other or unknown): Change (%)` = ((`Radiotherapy (other or unknown): Prev round (no.)`-`Radiotherapy (other or unknown): SoC (no.)`)/`Radiotherapy (other or unknown): SoC (no.)`) * 100,
               `Radiotherapy (palliative): Change (%)` = ((`Radiotherapy (palliative): Prev round (no.)`-`Radiotherapy (palliative): SoC (no.)`)/`Radiotherapy (palliative): SoC (no.)`) * 100,
               `Radiotherapy (combined intent): Change (%)` = ((`Radiotherapy (combined intent): Prev round (no.)`-`Radiotherapy (combined intent): SoC (no.)`)/`Radiotherapy (combined intent): SoC (no.)`) * 100) %>%
        select(`Cancer type`, contains("Tumour resection"), contains("Chemotherapy (cytotoxic)"), contains("Chemotherapy (other)"),
               contains("Radiotherapy (curative)"), contains("Radiotherapy (other or unknown)"), contains("Radiotherapy (palliative)"), contains("Radiotherapy (combined intent)")) %>%
        mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
        mutate(across(where(is.numeric) & !contains("Change"), \(x) round(x, 1))) 
        
gt::gt(df_table_4.1.1) %>%
        tab_spanner_delim(delim = ":", columns = everything(), split = c("first", "last"), limit = NULL, reverse = FALSE)

addWorksheet(excel_output, sheetName = "4.1.1 Tx vol (%) prev")
writeData(excel_output, sheet = "4.1.1 Tx vol (%) prev", df_table_4.1.1)

#' __________
### Table 4.1.2 Overall annual treatment volume for each cancer (all stages) and % change with MCED screening - steady state

df_table_4.1.2 <- df_volume_treatentXstage_joined_intercept %>%
        rename(`Treatment modality` = modality_SACTintent_RTDSintent) %>%
        select(`Cancer type`, `Stage at diagnosis`, `Treatment modality`, `SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        rename(`SoC` = `SoC no. of tumours treated`,
               `Prev round`= `Prev no. of tumours treated`,
               `SS`= `SS no. of tumours treated`) %>%
        pivot_wider(names_from = `Treatment modality`, values_from = c(`SoC`, `Prev round`, `SS`), names_glue ="{`Treatment modality`}: {.value} (no.)") %>%
        ## Added 30/04/2025
        mutate(`Radiotherapy (combined intent): SoC (no.)` = `Radiotherapy (curative): SoC (no.)` + `Radiotherapy (palliative): SoC (no.)` + `Radiotherapy (other or unknown): SoC (no.)`,
               `Radiotherapy (combined intent): Prev round (no.)` = `Radiotherapy (curative): Prev round (no.)` + `Radiotherapy (palliative): Prev round (no.)` + `Radiotherapy (other or unknown): Prev round (no.)`,
               `Radiotherapy (combined intent): SS (no.)` = `Radiotherapy (curative): SS (no.)` + `Radiotherapy (palliative): SS (no.)` + `Radiotherapy (other or unknown): SS (no.)`) %>%
        group_by(`Cancer type`) %>%
        summarise(across(contains(":"), \(x) sum(x))) %>%
        select(`Cancer type`, contains("SoC"), contains("SS")) %>%
        mutate(`Tumour resection: Change (%)` = ((`Tumour resection: SS (no.)`-`Tumour resection: SoC (no.)`)/`Tumour resection: SoC (no.)`) * 100,
               `Chemotherapy (cytotoxic): Change (%)` = ((`Chemotherapy (cytotoxic): SS (no.)`-`Chemotherapy (cytotoxic): SoC (no.)`)/`Chemotherapy (cytotoxic): SoC (no.)`) * 100,
               `Chemotherapy (other): Change (%)` = ((`Chemotherapy (other): SS (no.)`-`Chemotherapy (other): SoC (no.)`)/`Chemotherapy (other): SoC (no.)`) * 100,
               `Radiotherapy (curative): Change (%)` = ((`Radiotherapy (curative): SS (no.)`-`Radiotherapy (curative): SoC (no.)`)/`Radiotherapy (curative): SoC (no.)`) * 100,
               `Radiotherapy (other or unknown): Change (%)` = ((`Radiotherapy (other or unknown): SS (no.)`-`Radiotherapy (other or unknown): SoC (no.)`)/`Radiotherapy (other or unknown): SoC (no.)`) * 100,
               `Radiotherapy (palliative): Change (%)` = ((`Radiotherapy (palliative): SS (no.)`-`Radiotherapy (palliative): SoC (no.)`)/`Radiotherapy (palliative): SoC (no.)`) * 100,
               `Radiotherapy (combined intent): Change (%)` = ((`Radiotherapy (combined intent): SS (no.)`-`Radiotherapy (combined intent): SoC (no.)`)/`Radiotherapy (combined intent): SoC (no.)`) * 100) %>%
        select(`Cancer type`, contains("Tumour resection"), contains("Chemotherapy (cytotoxic)"), contains("Chemotherapy (other)"),
               contains("Radiotherapy (curative)"), contains("Radiotherapy (other or unknown)"), contains("Radiotherapy (palliative)"), contains("Radiotherapy (combined intent)")) %>%
        mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
        mutate(across(where(is.numeric) & !contains("Change"), \(x) round(x, 1))) 


gt::gt(df_table_4.1.2) %>%
        tab_spanner_delim(delim = ":", columns = everything(), split = c("first", "last"), limit = NULL, reverse = FALSE)

addWorksheet(excel_output, sheetName = "4.1.2 Tx vol (%) SS")
writeData(excel_output, sheet = "4.1.2 Tx vol (%) SS", df_table_4.1.2)

#' __________
# Figures For EDCC abstract and paper

EDCC <- df_table_4.1.1 %>%
        rename( `Tumour resection: Prev change (%)`=`Tumour resection: Change (%)`,
                `Chemotherapy (cytotoxic): Prev change (%)`=`Chemotherapy (cytotoxic): Change (%)`,
                `Chemotherapy (other): Prev change (%)`=`Chemotherapy (other): Change (%)`,
                `Radiotherapy (curative): Prev change (%)`=`Radiotherapy (curative): Change (%)`,
                `Radiotherapy (palliative): Prev change (%)`=`Radiotherapy (palliative): Change (%)`,
                `Radiotherapy (other or unknown): Prev change (%)`=`Radiotherapy (other or unknown): Change (%)`,
                `Radiotherapy (combined intent): Prev change (%)`=`Radiotherapy (combined intent): Change (%)`) %>%
        left_join(df_table_4.1.2 %>%
                          rename( `Tumour resection: SS change (%)`=`Tumour resection: Change (%)`,
                                  `Chemotherapy (cytotoxic): SS change (%)`=`Chemotherapy (cytotoxic): Change (%)`,
                                  `Chemotherapy (other): SS change (%)`=`Chemotherapy (other): Change (%)`,
                                  `Radiotherapy (curative): SS change (%)`=`Radiotherapy (curative): Change (%)`,
                                  `Radiotherapy (palliative): SS change (%)`=`Radiotherapy (palliative): Change (%)`,
                                  `Radiotherapy (other or unknown): SS change (%)`=`Radiotherapy (other or unknown): Change (%)`,
                                  `Radiotherapy (combined intent): SS change (%)`=`Radiotherapy (combined intent): Change (%)`) %>%
                          select(-contains("SoC")), by = c("Cancer type")) %>%
        pivot_longer(-`Cancer type`, names_to = "outcome", values_to = "change") %>%
        separate_wider_delim(cols = "outcome", delim = ":", names_sep = "") %>%
        rename(`Treatment modality` = outcome1,
               `Outcome type` = outcome2)%>%
        mutate(`Cancer type` = case_when(`Cancer type` == "Oesophagus" ~ "Oesophageal",
                                         .default = `Cancer type`)) %>%
        mutate(`Cancer type` = case_when(`Cancer type` == "All cancers combined" ~ "All cancers",
                                         `Cancer type` == "Anus" ~ "Anal",
                                         `Cancer type` == "Cervix" ~ "Cervical",
                                         `Cancer type` == "Ovary" ~ "Ovarian",
                                         `Cancer type` == "Pancreas" ~ "Pancreatic",
                                         `Cancer type` == "Rectum" ~ "Rectal",
                                         `Cancer type` == "Uterus" ~ "Uterine",
                                         `Cancer type` == "Liver/Bile-duct" ~ "Liver/Bile Duct",
                                         `Cancer type` == "Esophagus" ~ "Oesophageal",
                                         .default = `Cancer type`)) 

# All cancers
# EDCC_fig1_allcancer <- EDCC %>%
#         filter(grepl("change", `Outcome type`) & `Cancer type` == "All cancers combined") %>%
#         ggplot() +
#         geom_bar(aes(x = forcats::fct_rev(`Treatment modality`), y = change, fill = `Outcome type`), position= position_dodge(0.5), stat = "identity", width = .5) +
#         labs(fill = "Screening round", x = "Treatment modality", y = "Percentage (%) change in volume") +
#         theme_bw() +
#         theme(text = element_text(size = 18, face="bold"),) +
#         scale_fill_discrete(labels=c("Prevalent", "Steady state"),
#                             type=c("#3B1C52","#94724F"))


#' _____ Manuscript figures
# Select cancer for resection

order_fig_2 <- pull(EDCC %>%
        filter(grepl(c(" Prev round|SoC"), `Outcome type`) & `Treatment modality` == "Tumour resection") %>%
                pivot_wider(names_from = `Outcome type`, values_from = change) %>%
                mutate(change = abs(` Prev round (no.)` - ` SoC (no.)`)) %>%
        filter(!(`Cancer type` %in% c("Sarcoma", "Lymphoma" , "Urothelial Tract",  "Melanoma"))) %>%
        mutate(`Cancer type` = fct_reorder(`Cancer type`, desc(change))) %>%
                arrange(`Cancer type`) %>%
                select(`Cancer type`))


fig_2_resection <- EDCC %>%
        filter(grepl("change", `Outcome type`)) %>%
        filter(!(`Cancer type` %in% c("Sarcoma", "Lymphoma" , "Urothelial Tract",  "Melanoma"))) %>%
        filter(`Treatment modality` == "Tumour resection") %>%
        mutate(`Cancer type` = factor(`Cancer type` , levels = order_fig_2)) %>%
        mutate(`Cancer type` = relevel(`Cancer type`, "All cancers")) %>%
        ggplot() +
        geom_bar(aes(x = `Cancer type`, y = change, fill = `Outcome type`), position= position_dodge(0.5), stat = "identity", width = .5) +
        labs(title = NULL, fill = "Screening round", x = "Cancer type", y = "Change in annual no. of resections (%)") +
        theme_bw() +
        theme(text = element_text(size = 12, face="bold"),
              axis.text.x = element_text( size = 11, angle = 90, hjust = 1),
              axis.text.y = element_text( size = 11),
              legend.position=c(.7,.9),
              legend.background = element_blank(),
              legend.title=element_blank()) +
        scale_fill_discrete(labels=c("Initial screening round", "Steady-state screening programme"),
                            type=c("#3B1C52","#94724F")) +
        scale_y_continuous(breaks = c(0,50,100,150,200), limits = c(-10,200)) +
        annotate("segment", xend = "Anal", x = "Rectal", y = 150, yend = 150,
                   colour = "black", arrow = arrow(length = unit(0.2, "cm"), ends = "last")) +
        annotate("text", x = "Pancreatic", y = 140, label = "Ordered from largest to smallest absolute change in initial screening round")


order_fig_3a <- pull(EDCC %>%
                            filter(grepl(c(" Prev round|SoC"), `Outcome type`) & `Treatment modality` == "Chemotherapy (cytotoxic)") %>%
                            pivot_wider(names_from = `Outcome type`, values_from = change) %>%
                            mutate(change = abs(` Prev round (no.)` - ` SoC (no.)`)) %>%
                             filter(!(`Cancer type` %in% c( "Urothelial Tract",  "Melanoma"))) %>%
                            mutate(`Cancer type` = fct_reorder(`Cancer type`, desc(change))) %>%
                            arrange(`Cancer type`) %>%
                            select(`Cancer type`))

fig_3a_chemo_cyto <- EDCC %>%
        filter(grepl("change", `Outcome type`)) %>%
        filter(!(`Cancer type` %in% c( "Urothelial Tract",  "Melanoma"))) %>%
        filter(`Treatment modality` == "Chemotherapy (cytotoxic)") %>%
        mutate(`Cancer type` = factor(`Cancer type` , levels = order_fig_3a)) %>%
        mutate(`Cancer type` = relevel(`Cancer type`, "All cancers")) %>%
        ggplot() +
        geom_bar(aes(x = `Cancer type`, y = change, fill = `Outcome type`), position= position_dodge(0.5), stat = "identity", width = .5) +
        labs(title = NULL, fill = "Screening round", x = "Cancer type", y = "Change in annual no. of cancers treated with SACT (%)") +
        theme_bw() +
        theme(text = element_text(size = 12, face="bold"),
              axis.text.x = element_text( size = 11, angle = 90, hjust = 1),
              axis.text.y = element_text( size = 11),
              legend.position=c(.7,.9),
              legend.background = element_blank(),
              legend.title=element_blank()) +
        scale_fill_discrete(labels=c("Initial screening round", "Steady-state screening programme"),
                            type=c("#3B1C52","#94724F")) +
        scale_y_continuous(limits = c(-40,60))+
        annotate("segment", x = "Ovarian", xend = "Cervical", y = 40, yend = 40,
                 colour = "black", arrow = arrow(length = unit(0.2, "cm"), ends = "last")) +
        annotate("text", x = "Breast", y = 35, label = "Ordered from largest to smallest absolute change in initial screening round")+
        annotate("text", x = "All cancers", y = 55, label = "A", size = 10) 



order_fig_3b <- pull(EDCC %>%
                             filter(grepl(c(" Prev round|SoC"), `Outcome type`) & `Treatment modality` == "Chemotherapy (other)") %>%
                             pivot_wider(names_from = `Outcome type`, values_from = change) %>%
                             mutate(change = abs(` Prev round (no.)` - ` SoC (no.)`)) %>%
                             filter(!(`Cancer type` %in% c("Colon", "Gallbladder","Pancreatic", "Uterine", "Ovarian", "Sarcoma" , "Prostate", "Urothelial Tract",  "Melanoma"))) %>%
                             mutate(`Cancer type` = fct_reorder(`Cancer type`, desc(change))) %>%
                             arrange(`Cancer type`) %>%
                             select(`Cancer type`))

fig_3b_chemo_other <- EDCC %>%
        filter(grepl("change", `Outcome type`)) %>%
        filter(!(`Cancer type` %in% c("Colon", "Gallbladder", "Pancreatic", "Uterine", "Ovarian", "Sarcoma" , "Prostate", "Urothelial Tract",  "Melanoma"))) %>%
        filter(`Treatment modality` == "Chemotherapy (other)") %>%
        mutate(`Cancer type` = factor(`Cancer type` , levels = order_fig_3b)) %>%
        mutate(`Cancer type` = relevel(`Cancer type`, "All cancers")) %>%
        ggplot() +
        geom_bar(aes(x = `Cancer type`, y = change, fill = `Outcome type`), position= position_dodge(0.5), stat = "identity", width = .5) +
        labs(title = NULL, fill = "Screening round", x = "Cancer type", y = "") +
        theme_bw() +
        theme(text = element_text(size = 12, face="bold"),
              axis.text.x = element_text( size = 11, angle = 90, hjust = 1),
              axis.text.y = element_text( size = 11),
              legend.background = element_blank(),
              legend.title=element_blank(),
              legend.position="none") +
        scale_fill_discrete(labels=c("Initial screening round", "Steady-state screening programme"),
                            type=c("#3B1C52","#94724F")) +
        scale_y_continuous(limits = c(-40,60)) +
        annotate("text", x = "All cancers", y = 55, label = "B", size = 10) 


order_fig_4b <- pull(EDCC %>%
                             filter(grepl(c(" Prev round|SoC"), `Outcome type`) & `Treatment modality` == "Radiotherapy (curative)") %>%
                             pivot_wider(names_from = `Outcome type`, values_from = change) %>%
                             mutate(change = abs(` Prev round (no.)` - ` SoC (no.)`)) %>%
                             filter(!(`Cancer type` %in% c("Colon", "Ovarian" , "Urothelial Tract",  "Melanoma"))) %>%
                             mutate(`Cancer type` = fct_reorder(`Cancer type`, desc(change))) %>%
                             arrange(`Cancer type`) %>%
                             select(`Cancer type`))

fig_4b_radio_cur <- EDCC %>%
        filter(grepl("change", `Outcome type`)) %>%
        filter(!(`Cancer type` %in% c("Colon", "Ovarian" , "Urothelial Tract",  "Melanoma"))) %>%
        filter(`Treatment modality` == "Radiotherapy (curative)") %>%
        mutate(`Cancer type` = factor(`Cancer type` , levels = order_fig_4b)) %>%
        mutate(`Cancer type` = relevel(`Cancer type`, "All cancers")) %>%
        ggplot() +
        geom_bar(aes(x = `Cancer type`, y = change, fill = `Outcome type`), position= position_dodge(0.5), stat = "identity", width = .5) +
        labs(title = NULL, fill = "Screening round", x = "", y = "Change in annual no. of cancers treated with radiotherapy (%)") +
        theme_bw() +
        theme(text = element_text(size = 12, face="bold"),
              axis.text.x = element_text( size = 11, angle = 90, hjust = 1),
              axis.text.y = element_text( size = 11),
              legend.position="none",
              legend.background = element_blank(),
              legend.title=element_blank()) +
        scale_fill_discrete(labels=c("Initial screening round", "Steady-state screening programme"),
                            type=c("#3B1C52","#94724F")) +
        scale_y_continuous(breaks = c(-40, -20, 0, 20, 40, 60, 80, 100), limits = c(-40,100)) +
        annotate("text", x = "All cancers", y = 90, label = "B", size = 10) 

order_fig_4c <- pull(EDCC %>%
                             filter(grepl(c(" Prev round|SoC"), `Outcome type`) & `Treatment modality` == "Radiotherapy (palliative)") %>%
                             pivot_wider(names_from = `Outcome type`, values_from = change) %>%
                             mutate(change = abs(` Prev round (no.)` - ` SoC (no.)`)) %>%
                             filter(!(`Cancer type` %in% c("Colon", "Ovarian" , "Urothelial Tract",  "Melanoma"))) %>%
                             mutate(`Cancer type` = fct_reorder(`Cancer type`, desc(change))) %>%
                             arrange(`Cancer type`) %>%
                             select(`Cancer type`))

fig_4c_radio_pal <- EDCC %>%
        filter(grepl("change", `Outcome type`)) %>%
        filter(!(`Cancer type` %in% c("Colon", "Ovarian" , "Urothelial Tract",  "Melanoma"))) %>%
        filter(`Treatment modality` == "Radiotherapy (palliative)") %>%
        mutate(`Cancer type` = factor(`Cancer type` , levels = order_fig_4c)) %>%
        mutate(`Cancer type` = relevel(`Cancer type`, "All cancers")) %>%
        ggplot() +
        geom_bar(aes(x = `Cancer type`, y = change, fill = `Outcome type`), position= position_dodge(0.5), stat = "identity", width = .5) +
        labs(title = NULL, fill = "Screening round", x = "Cancer type", y = "") +
        theme_bw() +
        theme(text = element_text(size = 12, face="bold"),
              axis.text.x = element_text( size = 11, angle = 90, hjust = 1),
              axis.text.y = element_text( size = 11),
              legend.position="none",
              legend.background = element_blank(),
              legend.title=element_blank()) +
        scale_fill_discrete(labels=c("Initial screening round", "Steady-state screening programme"),
                            type=c("#3B1C52","#94724F"))  +
        scale_y_continuous(breaks = c(-40, -20, 0, 20, 40, 60, 80, 100), limits = c(-40,100)) +
        annotate("text", x = "All cancers", y = 90, label = "C", size = 10)

order_fig_4a <- pull(EDCC %>%
                             filter(grepl(c(" Prev round|SoC"), `Outcome type`) & `Treatment modality` == "Radiotherapy (combined intent)") %>%
                             pivot_wider(names_from = `Outcome type`, values_from = change) %>%
                             mutate(change = abs(` Prev round (no.)` - ` SoC (no.)`)) %>%
                             filter(!(`Cancer type` %in% c("Colon", "Ovarian" , "Urothelial Tract",  "Melanoma"))) %>%
                             mutate(`Cancer type` = fct_reorder(`Cancer type`, desc(change))) %>%
                             arrange(`Cancer type`) %>%
                             select(`Cancer type`))

fig_4a_radio_combined <- EDCC %>%
        filter(grepl("change", `Outcome type`)) %>%
        filter(!(`Cancer type` %in% c("Colon", "Ovarian" , "Urothelial Tract",  "Melanoma"))) %>%
        filter(`Treatment modality` == "Radiotherapy (combined intent)") %>%
        mutate(`Cancer type` = factor(`Cancer type` , levels = order_fig_4a)) %>%
        mutate(`Cancer type` = relevel(`Cancer type`, "All cancers")) %>%
        ggplot() +
        geom_bar(aes(x = `Cancer type`, y = change, fill = `Outcome type`), position= position_dodge(0.5), stat = "identity", width = .5) +
        labs(title = NULL, fill = "Screening round", x = "", y = "") +
        theme_bw() +
        theme(text = element_text(size = 12, face="bold"),
              axis.text.x = element_text( size = 11, angle = 90, hjust = 1),
              axis.text.y = element_text( size = 11),
              legend.position=c(.7,.85),
              legend.background = element_blank(),
              legend.title=element_blank()) +
        scale_fill_discrete(labels=c("Initial screening round", "Steady-state screening programme"),
                            type=c("#3B1C52","#94724F"))  +
        scale_y_continuous(breaks = c(-40, -20, 0, 20, 40, 60, 80, 100), limits = c(-40,100)) +
        annotate("segment", x = "Head and Neck", xend = "Stomach", y = 60, yend = 60,
                 colour = "black", arrow = arrow(length = unit(0.2, "cm"), ends = "last")) +
        annotate("text", x = "Pancreatic", y = 45, label = "Ordered from largest to smallest absolute change in initial screening round") +
        annotate("text", x = "All cancers", y = 90, label = "A", size = 10)



## Save figures for paper
ggsave(paste0("~/Miscellaneous/Treatments/Figures/", uptake, "fig_2_resection.png"), fig_2_resection, device = "png",
       width = 200,
       height = 150,
       units = "mm")

library(patchwork)

ggsave(paste0("~/Miscellaneous/Treatments/Figures/", uptake, "fig_4_SACT.png"), fig_3a_chemo_cyto + fig_3b_chemo_other , device = "png",
       width = 400,
       height = 200,
       units = "mm")

ggsave(paste0("~/Miscellaneous/Treatments/Figures/", uptake, "fig_3_Radio.png"), fig_4a_radio_combined + fig_4b_radio_cur + fig_4c_radio_pal + plot_layout(ncol = 1) , device = "png",
       width = 200,
       height = 350,
       units = "mm")


#' __________
## 4.2 Treatment modality combinations: 
#' __________
### 4.2.1 Annual treatment volume for treatment modality combination, for each cancer (all stages) and % change with MCED screening - prevalent round
#' Considering here 9 combinations:
#' 1 Resection only
#' 2 Resection with any SACT (any intent)
#'    Vast majority is resection with cytotoxic chemo, mainly of curative intent
#' 3 Resection with radiotherapy (any intent)
#'    Vast majority is resection with curative radiotherapy
#' 4 Resection with any SACT + radiotherapy 
#'    Majority is resection with curative cytotoxic chemo and curative radiotherapy
#' 5 Any SACT + radiotherapy combination (any intent)
#'    majority curative cytotoxic chemo + curative radiotherapy, some palliative cytotoxic chemo + palliative radiotherapy
#' 6 Any SACT only (any intent)
#'    Majority is cytotoxic chemotherapy only with palliative intent
#' 7 Curative radiotherapy only
#' 8 Palliative radiotherapy only
#' Radiotherapy with unknown intent - Not interpretable 
#' 9 Other care

df_table_4.2.1 <- df_combined_treatentXstage_joined_intercept %>%
        # adjusted the combined data frame by colapsing the combinations to 9 only
        select(`Cancer type`, `Stage at diagnosis`, combination_SACTintent_RTDSintent, `SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        mutate(`Treatment modality` = case_when(grepl(c("Tumour resection only"), combination_SACTintent_RTDSintent ) ~ "Tumour resection only",
                                                grepl(c("Tumour resection and chemotherapy"), combination_SACTintent_RTDSintent ) ~ "Tumour resection and chemotherapy",
                                                grepl(c("Tumour resection and radiotherapy"), combination_SACTintent_RTDSintent ) ~ "Tumour resection and radiotherapy",
                                                grepl(c("Tumour resection, radiotherapy and chemotherapy"), combination_SACTintent_RTDSintent ) ~ "Tumour resection, radiotherapy and chemotherapy",
                                                grepl(c("Chemotherapy \\(cytotoxic\\) and radiotherapy|Chemotherapy \\(other\\) and radiotherapy"), combination_SACTintent_RTDSintent ) ~ "Chemotherapy and radiotherapy",
                                                grepl(c("Chemotherapy \\(cytotoxic\\) only|Chemotherapy \\(other\\) only"), combination_SACTintent_RTDSintent ) ~ "Chemotherapy only",
                                                grepl(c("Radiotherapy only-RTDS intent Curative"), combination_SACTintent_RTDSintent ) ~ "Radiotherapy (curative intent) only",
                                                grepl(c("Radiotherapy only-RTDS intent Other or unknown"), combination_SACTintent_RTDSintent ) ~ "Radiotherapy (other or unknown) only",
                                                grepl(c("Radiotherapy only-RTDS intent Palliative"), combination_SACTintent_RTDSintent ) ~ "Radiotherapy (palliative intent) only",
                                                .default = combination_SACTintent_RTDSintent),  .after = `Stage at diagnosis`) %>%
        mutate(`Treatment modality` = factor(`Treatment modality`, levels = c("Tumour resection only" , "Tumour resection and chemotherapy" ,"Tumour resection and radiotherapy" ,
                                                                              "Tumour resection, radiotherapy and chemotherapy", "Chemotherapy and radiotherapy" ,"Chemotherapy only",
                                                                              "Radiotherapy (curative intent) only", "Radiotherapy (other or unknown) only", "Radiotherapy (palliative intent) only",
                                                                              "Other care"))) %>%
        select(-combination_SACTintent_RTDSintent) %>%
        # Collapse the rows by the smaller number of combinations
        group_by(`Cancer type`, `Stage at diagnosis`,`Treatment modality`) %>%
        summarise(across(`SoC no. of tumours treated`:`SS no. of tumours treated`, \(x) sum(x))) %>%
        ungroup() %>%
        # reformat to wider
        rename(`SoC` = `SoC no. of tumours treated`,
               `Prev round`= `Prev no. of tumours treated`,
               `SS`= `SS no. of tumours treated`) %>%
        pivot_wider(names_from = `Treatment modality`, values_from = c(`SoC`, `Prev round`, `SS`), names_glue ="{`Treatment modality`}: {.value} (no.)") %>%
        group_by(`Cancer type`) %>%
        # collapse stages
        summarise(across(contains(":"), \(x) sum(x))) %>%
        ungroup() %>%
        select(`Cancer type`, contains("SoC"), contains("Prev")) %>%
        mutate(`Tumour resection only: Change (%)` = ((`Tumour resection only: Prev round (no.)`-`Tumour resection only: SoC (no.)`)/`Tumour resection only: SoC (no.)`) * 100,
               `Chemotherapy only: Change (%)` = ((`Chemotherapy only: Prev round (no.)`-`Chemotherapy only: SoC (no.)`)/`Chemotherapy only: SoC (no.)`) * 100,
               `Radiotherapy (curative intent) only: Change (%)` = ((`Radiotherapy (curative intent) only: Prev round (no.)`-`Radiotherapy (curative intent) only: SoC (no.)`)/`Radiotherapy (curative intent) only: SoC (no.)`) * 100,
               `Radiotherapy (other or unknown) only: Change (%)` = ((`Radiotherapy (other or unknown) only: Prev round (no.)`-`Radiotherapy (other or unknown) only: SoC (no.)`)/`Radiotherapy (other or unknown) only: SoC (no.)`) * 100,
               `Radiotherapy (palliative intent) only: Change (%)` = ((`Radiotherapy (palliative intent) only: Prev round (no.)`-`Radiotherapy (palliative intent) only: SoC (no.)`)/`Radiotherapy (palliative intent) only: SoC (no.)`) * 100,
               `Tumour resection and chemotherapy: Change (%)` = ((`Tumour resection and chemotherapy: Prev round (no.)`-`Tumour resection and chemotherapy: SoC (no.)`)/`Tumour resection and chemotherapy: SoC (no.)`) * 100,
               `Tumour resection and radiotherapy: Change (%)` = ((`Tumour resection and radiotherapy: Prev round (no.)`-`Tumour resection and radiotherapy: SoC (no.)`)/`Tumour resection and radiotherapy: SoC (no.)`) * 100,
               `Chemotherapy and radiotherapy: Change (%)` = ((`Chemotherapy and radiotherapy: Prev round (no.)`-`Chemotherapy and radiotherapy: SoC (no.)`)/`Chemotherapy and radiotherapy: SoC (no.)`) * 100,
               `Other care: Change (%)` = ((`Other care: Prev round (no.)`-`Other care: SoC (no.)`)/`Other care: SoC (no.)`) * 100,
               `Tumour resection, radiotherapy and chemotherapy: Change (%)` = ((`Tumour resection, radiotherapy and chemotherapy: Prev round (no.)`-`Tumour resection, radiotherapy and chemotherapy: SoC (no.)`)/`Tumour resection, radiotherapy and chemotherapy: SoC (no.)`) * 100) %>%
        mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
        select(`Cancer type`, contains("Tumour resection only"), contains("Tumour resection and chemotherapy"),
               contains("Tumour resection and radiotherapy"), contains("Tumour resection, radiotherapy and chemotherapy"),
               contains("Chemotherapy and radiotherapy"), contains("Radiotherapy (curative intent) only"), contains("Radiotherapy (palliative intent) only"), 
               contains("Chemotherapy only"), contains("Other care") )

gt::gt(df_table_4.2.1) %>%
        tab_spanner_delim(delim = ":", columns = everything(), split = c("first", "last"), limit = NULL, reverse = FALSE)

addWorksheet(excel_output, sheetName = "4.2.1 Tx combi (%) prev")
writeData(excel_output, sheet = "4.2.1 Tx combi (%) prev", df_table_4.2.1)

#' __________
### Table 4.2.2 Annual treatment volume for treatment modality combination, for each cancer (all stages) and % change with MCED screening - steady state

df_table_4.2.2 <- df_combined_treatentXstage_joined_intercept %>%
        # adjusted the combined data frame by colapsing the combinations to 9 only
        select(`Cancer type`, `Stage at diagnosis`, combination_SACTintent_RTDSintent, `SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        mutate(`Treatment modality` = case_when(grepl(c("Tumour resection only"), combination_SACTintent_RTDSintent ) ~ "Tumour resection only",
                                                grepl(c("Tumour resection and chemotherapy"), combination_SACTintent_RTDSintent ) ~ "Tumour resection and chemotherapy",
                                                grepl(c("Tumour resection and radiotherapy"), combination_SACTintent_RTDSintent ) ~ "Tumour resection and radiotherapy",
                                                grepl(c("Tumour resection, radiotherapy and chemotherapy"), combination_SACTintent_RTDSintent ) ~ "Tumour resection, radiotherapy and chemotherapy",
                                                grepl(c("Chemotherapy \\(cytotoxic\\) and radiotherapy|Chemotherapy \\(other\\) and radiotherapy"), combination_SACTintent_RTDSintent ) ~ "Chemotherapy and radiotherapy",
                                                grepl(c("Chemotherapy \\(cytotoxic\\) only|Chemotherapy \\(other\\) only"), combination_SACTintent_RTDSintent ) ~ "Chemotherapy only",
                                                grepl(c("Radiotherapy only-RTDS intent Curative"), combination_SACTintent_RTDSintent ) ~ "Radiotherapy (curative intent) only",
                                                grepl(c("Radiotherapy only-RTDS intent Other or unknown"), combination_SACTintent_RTDSintent ) ~ "Radiotherapy (other or unknown) only",
                                                grepl(c("Radiotherapy only-RTDS intent Palliative"), combination_SACTintent_RTDSintent ) ~ "Radiotherapy (palliative intent) only",
                                                .default = combination_SACTintent_RTDSintent),  .after = `Stage at diagnosis`) %>%
        mutate(`Treatment modality` = factor(`Treatment modality`, levels = c("Tumour resection only" , "Tumour resection and chemotherapy" ,"Tumour resection and radiotherapy" ,
                                                                              "Tumour resection, radiotherapy and chemotherapy", "Chemotherapy and radiotherapy" ,"Chemotherapy only",
                                                                              "Radiotherapy (curative intent) only", "Radiotherapy (other or unknown) only", "Radiotherapy (palliative intent) only",
                                                                              "Other care"))) %>%
        select(-combination_SACTintent_RTDSintent) %>%
        # Collapse the rows by the smaller number of combinations
        group_by(`Cancer type`, `Stage at diagnosis`,`Treatment modality`) %>%
        summarise(across(`SoC no. of tumours treated`:`SS no. of tumours treated`, \(x) sum(x))) %>%
        ungroup() %>%
        # reformat to wider
        rename(`SoC` = `SoC no. of tumours treated`,
               `Prev round`= `Prev no. of tumours treated`,
               `SS`= `SS no. of tumours treated`) %>%
        pivot_wider(names_from = `Treatment modality`, values_from = c(`SoC`, `Prev round`, `SS`), names_glue ="{`Treatment modality`}: {.value} (no.)") %>%
        group_by(`Cancer type`) %>%
        # collapse stages
        summarise(across(contains(":"), \(x) sum(x))) %>%
        ungroup() %>%
        select(`Cancer type`, contains("SoC"), contains("SS")) %>%
        mutate(`Tumour resection only: Change (%)` = ((`Tumour resection only: SS (no.)`-`Tumour resection only: SoC (no.)`)/`Tumour resection only: SoC (no.)`) * 100,
               `Chemotherapy only: Change (%)` = ((`Chemotherapy only: SS (no.)`-`Chemotherapy only: SoC (no.)`)/`Chemotherapy only: SoC (no.)`) * 100,
               `Radiotherapy (curative intent) only: Change (%)` = ((`Radiotherapy (curative intent) only: SS (no.)`-`Radiotherapy (curative intent) only: SoC (no.)`)/`Radiotherapy (curative intent) only: SoC (no.)`) * 100,
               `Radiotherapy (other or unknown) only: Change (%)` = ((`Radiotherapy (other or unknown) only: SS (no.)`-`Radiotherapy (other or unknown) only: SoC (no.)`)/`Radiotherapy (other or unknown) only: SoC (no.)`) * 100,
               `Radiotherapy (palliative intent) only: Change (%)` = ((`Radiotherapy (palliative intent) only: SS (no.)`-`Radiotherapy (palliative intent) only: SoC (no.)`)/`Radiotherapy (palliative intent) only: SoC (no.)`) * 100,
               `Tumour resection and chemotherapy: Change (%)` = ((`Tumour resection and chemotherapy: SS (no.)`-`Tumour resection and chemotherapy: SoC (no.)`)/`Tumour resection and chemotherapy: SoC (no.)`) * 100,
               `Tumour resection and radiotherapy: Change (%)` = ((`Tumour resection and radiotherapy: SS (no.)`-`Tumour resection and radiotherapy: SoC (no.)`)/`Tumour resection and radiotherapy: SoC (no.)`) * 100,
               `Chemotherapy and radiotherapy: Change (%)` = ((`Chemotherapy and radiotherapy: SS (no.)`-`Chemotherapy and radiotherapy: SoC (no.)`)/`Chemotherapy and radiotherapy: SoC (no.)`) * 100,
               `Other care: Change (%)` = ((`Other care: SS (no.)`-`Other care: SoC (no.)`)/`Other care: SoC (no.)`) * 100,
               `Tumour resection, radiotherapy and chemotherapy: Change (%)` = ((`Tumour resection, radiotherapy and chemotherapy: SS (no.)`-`Tumour resection, radiotherapy and chemotherapy: SoC (no.)`)/`Tumour resection, radiotherapy and chemotherapy: SoC (no.)`) * 100) %>%
        mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
        select(`Cancer type`, contains("Tumour resection only"), contains("Tumour resection and chemotherapy"),
               contains("Tumour resection and radiotherapy"), contains("Tumour resection, radiotherapy and chemotherapy"),
               contains("Chemotherapy and radiotherapy"), contains("Radiotherapy (curative intent) only"), contains("Radiotherapy (palliative intent) only"), 
               contains("Chemotherapy only"), contains("Other care") )

gt::gt(df_table_4.2.2) %>%
        tab_spanner_delim(delim = ":", columns = everything(), split = c("first", "last"), limit = NULL, reverse = FALSE)

addWorksheet(excel_output, sheetName = "4.2.2 Tx combi (%) SS")
writeData(excel_output, sheet = "4.2.2 Tx combi (%) SS", df_table_4.2.2)

#' __________
### 4.2.3 Annual treatment volume for CURATIVE treatment modality combination, for each cancer (all stages) and % change with MCED screening - prevalent round
#' Considering here 2 combinations:
#' Resection only + resection + curative chemo/radio

df_table_4.2.3 <- df_combined_treatentXstage_joined_intercept %>%
        # adjusted the combined data frame by colapsing the combinations to 2 only
        select(`Cancer type`, `Stage at diagnosis`, combination_SACTintent_RTDSintent, `SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        mutate(`Treatment modality` = case_when(combination_SACTintent_RTDSintent %in% c("Tumour resection and chemotherapy (cytotoxic)_SACT intent Curative",
                                                        "Tumour resection and chemotherapy (other)_SACT intent Curative",
                                                        "Tumour resection and radiotherapy-RTDS intent Curative",
                                                        "Tumour resection only",
                                                        "Tumour resection, radiotherapy and chemotherapy (cytotoxic)_SACT intent Curative-RTDS intent Curative",
                                                        "Tumour resection, radiotherapy and chemotherapy (other)_SACT intent Curative-RTDS intent Curative") ~ "Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy",
                                                combination_SACTintent_RTDSintent %in% c("Chemotherapy (cytotoxic) and radiotherapy_SACT intent Curative-RTDS intent Curative",
                                                        "Chemotherapy (other) and radiotherapy_SACT intent Curative-RTDS intent Curative",
                                                        "Radiotherapy only-RTDS intent Curative") ~ "Curative intent radiotherapy/chemoradiotherapy",
                                                .default = "Other combinations"),  .after = `Stage at diagnosis`) %>%
        filter(`Treatment modality` != "Other combinations") %>%
        mutate(`Treatment modality` = factor(`Treatment modality`, levels = c("Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy",
                                                                              "Curative intent radiotherapy/chemoradiotherapy"))) %>%
        select(-combination_SACTintent_RTDSintent) %>%
        # Collapse the rows by the smaller number of combinations
        group_by(`Cancer type`, `Stage at diagnosis`,`Treatment modality`) %>%
        summarise(across(`SoC no. of tumours treated`:`SS no. of tumours treated`, \(x) sum(x))) %>%
        ungroup() %>%
        # reformat to wider
        rename(`SoC` = `SoC no. of tumours treated`,
               `Prev round`= `Prev no. of tumours treated`,
               `SS`= `SS no. of tumours treated`) %>%
        pivot_wider(names_from = `Treatment modality`, values_from = c(`SoC`, `Prev round`, `SS`), names_glue ="{`Treatment modality`}: {.value} (no.)") %>%
        group_by(`Cancer type`) %>%
        # collapse stages
        summarise(across(contains(":"), \(x) sum(x))) %>%
        ungroup() %>%
        select(`Cancer type`, contains("SoC"), contains("Prev")) %>%
        mutate(`Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: Change (%)` = ((`Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: Prev round (no.)`-`Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: SoC (no.)`)/`Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: SoC (no.)`) * 100,
               `Curative intent radiotherapy/chemoradiotherapy: Change (%)` = ((`Curative intent radiotherapy/chemoradiotherapy: Prev round (no.)`-`Curative intent radiotherapy/chemoradiotherapy: SoC (no.)`)/`Curative intent radiotherapy/chemoradiotherapy: SoC (no.)`) * 100) %>%
        mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
        select(`Cancer type`, contains("Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy"), contains("Curative intent radiotherapy/chemoradiotherapy"))

gt::gt(df_table_4.2.3) %>%
        tab_spanner_delim(delim = ":", columns = everything(), split = c("first", "last"), limit = NULL, reverse = FALSE)

addWorksheet(excel_output, sheetName = "4.2.3 Tx combi cure (%) prev")
writeData(excel_output, sheet = "4.2.3 Tx combi cure (%) prev", df_table_4.2.3)

#' __________
### Table 4.2.2 Annual treatment volume for treatment modality combination, for each cancer (all stages) and % change with MCED screening - steady state

df_table_4.2.4 <- df_combined_treatentXstage_joined_intercept %>%
        # adjusted the combined data frame by colapsing the combinations to 9 only
        select(`Cancer type`, `Stage at diagnosis`, combination_SACTintent_RTDSintent, `SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        mutate(`Treatment modality` = case_when(combination_SACTintent_RTDSintent %in% c("Tumour resection and chemotherapy (cytotoxic)_SACT intent Curative",
                                                                                         "Tumour resection and chemotherapy (other)_SACT intent Curative",
                                                                                         "Tumour resection and radiotherapy-RTDS intent Curative",
                                                                                         "Tumour resection only",
                                                                                         "Tumour resection, radiotherapy and chemotherapy (cytotoxic)_SACT intent Curative-RTDS intent Curative",
                                                                                         "Tumour resection, radiotherapy and chemotherapy (other)_SACT intent Curative-RTDS intent Curative") ~ "Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy",
                                                combination_SACTintent_RTDSintent %in% c("Chemotherapy (cytotoxic) and radiotherapy_SACT intent Curative-RTDS intent Curative",
                                                                                         "Chemotherapy (other) and radiotherapy_SACT intent Curative-RTDS intent Curative",
                                                                                         "Radiotherapy only-RTDS intent Curative") ~ "Curative intent radiotherapy/chemoradiotherapy",
                                                .default = "Other combinations"),  .after = `Stage at diagnosis`) %>%
        filter(`Treatment modality` != "Other combinations") %>%
        mutate(`Treatment modality` = factor(`Treatment modality`, levels = c("Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy",
                                                                              "Curative intent radiotherapy/chemoradiotherapy"))) %>%
        select(-combination_SACTintent_RTDSintent) %>%
        # Collapse the rows by the smaller number of combinations
        group_by(`Cancer type`, `Stage at diagnosis`,`Treatment modality`) %>%
        summarise(across(`SoC no. of tumours treated`:`SS no. of tumours treated`, \(x) sum(x))) %>%
        ungroup() %>%
        # reformat to wider
        rename(`SoC` = `SoC no. of tumours treated`,
               `Prev round`= `Prev no. of tumours treated`,
               `SS`= `SS no. of tumours treated`) %>%
        pivot_wider(names_from = `Treatment modality`, values_from = c(`SoC`, `Prev round`, `SS`), names_glue ="{`Treatment modality`}: {.value} (no.)") %>%
        group_by(`Cancer type`) %>%
        # collapse stages
        summarise(across(contains(":"), \(x) sum(x))) %>%
        ungroup() %>%
        select(`Cancer type`, contains("SoC"), contains("SS")) %>%
        mutate(`Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: Change (%)` = ((`Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: SS (no.)`-`Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: SoC (no.)`)/`Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: SoC (no.)`) * 100,
               `Curative intent radiotherapy/chemoradiotherapy: Change (%)` = ((`Curative intent radiotherapy/chemoradiotherapy: SS (no.)`-`Curative intent radiotherapy/chemoradiotherapy: SoC (no.)`)/`Curative intent radiotherapy/chemoradiotherapy: SoC (no.)`) * 100) %>%
        mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
        select(`Cancer type`, contains("Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy"), contains("Curative intent radiotherapy/chemoradiotherapy"))

gt::gt(df_table_4.2.4) %>%
        tab_spanner_delim(delim = ":", columns = everything(), split = c("first", "last"), limit = NULL, reverse = FALSE)

addWorksheet(excel_output, sheetName = "4.2.4 Tx combi cure (%) SS")
writeData(excel_output, sheet = "4.2.4 Tx combi cure (%) SS", df_table_4.2.4)

fig_5_curative_resection <-  rbind(df_table_4.2.3 %>%
                                          mutate(`Outcome type` = "Prev round") %>%
                                          rename(`Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: MCED (no.)` = `Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: Prev round (no.)`,
                                                 `Curative intent radiotherapy/chemoradiotherapy: MCED (no.)` = `Curative intent radiotherapy/chemoradiotherapy: Prev round (no.)`),
                                  
                                  df_table_4.2.4 %>% 
                                          mutate(`Outcome type` = "SS") %>%
                                          rename(`Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: MCED (no.)` = `Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: SS (no.)`,
                                                 `Curative intent radiotherapy/chemoradiotherapy: MCED (no.)` = `Curative intent radiotherapy/chemoradiotherapy: SS (no.)`)
                                  
                                  ) %>%
        mutate(`Cancer type` = case_when(`Cancer type` == "Anus" ~ "Anal",
                                         `Cancer type` == "Cervix" ~ "Cervical",
                                         `Cancer type` == "Ovary" ~ "Ovarian",
                                         `Cancer type` == "Pancreas" ~ "Pancreatic",
                                         `Cancer type` == "Rectum" ~ "Rectal",
                                         `Cancer type` == "Uterus" ~ "Uterine",
                                         `Cancer type` == "Liver/Bile-duct" ~ "Liver/Bile Duct",
                                         `Cancer type` == "Esophagus" ~ "Oesophageal",
                                         `Cancer type` == "Head and Neck" ~ "Head and Neck",
                                         .default = `Cancer type`)) %>%
        filter(!(`Cancer type` %in% c("Sarcoma", "Lymphoma" , "Urothelial Tract",  "Melanoma"))) %>%
        select(`Cancer type`,
               `Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: Change (%)`,
               `Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: MCED (no.)`,
               `Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: SoC (no.)`,
               `Outcome type`) %>%
        # select(`Cancer type`, `Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: Change (%)`, `Outcome type`) %>%
                rename(change = `Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: Change (%)`) %>%
                mutate(num = `Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: MCED (no.)` -`Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: SoC (no.)`,
                       abs_num = abs(`Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: MCED (no.)` -`Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: SoC (no.)`)) %>%
                group_by(`Outcome type`) %>%
                mutate( `Cancer type` = fct_rev(fct_reorder(`Cancer type`, abs_num)),
                        `Cancer type` = relevel(`Cancer type`, "All cancers combined")) %>%
                mutate(across(where(is.numeric), round,1))  %>%
        ggplot() +
        geom_bar(aes(x = `Cancer type`, y = change, fill = `Outcome type`), position= position_dodge(0.5), stat = "identity", width = .5) +
        labs(title = NULL, fill = "Screening round", x = "Cancer type", y = "Change in annual no. of cancers treated with resection\nonly or resection plus radiotherapy and/or SACT (%)") +
        theme_bw() +
        theme(text = element_text(size = 11, face="bold"),
              axis.text.x = element_text( size = 11, angle = 90, hjust = 1),
              axis.text.y = element_text( size = 11),
              legend.position=c(.7,.9),
              legend.background = element_blank(),
              legend.title=element_blank()) +
        scale_fill_discrete(labels=c("Initial screening round", "Steady-state screening programme"),
                            type=c("#3B1C52","#94724F")) +
        scale_y_continuous(limits = c(-20,200)) +
        annotate("segment", x = "Rectal", xend = "Cervical", y = 140, yend = 140,
                 colour = "black", arrow = arrow(length = unit(0.2, "cm"), ends = "last")) +
        annotate("text", x = "Liver/Bile Duct", y = 130, label = "Ordered from largest to smallest absolute change in initial screening round")


        
df_fig2_curative_radio <-  rbind(df_table_4.2.3 %>%
                                          mutate(`Outcome type` = "Prev round") %>%
                                          rename(`Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: MCED (no.)` = `Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: Prev round (no.)`,
                                                 `Curative intent radiotherapy/chemoradiotherapy: MCED (no.)` = `Curative intent radiotherapy/chemoradiotherapy: Prev round (no.)`),
                                  
                                  df_table_4.2.4 %>% 
                                          mutate(`Outcome type` = "SS") %>%
                                          rename(`Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: MCED (no.)` = `Tumour resection or tumour resection plus curative\nintent radiotherapy/chemotherapy/chemoradiotherapy: SS (no.)`,
                                                 `Curative intent radiotherapy/chemoradiotherapy: MCED (no.)` = `Curative intent radiotherapy/chemoradiotherapy: SS (no.)`)
                                  
) %>%
        mutate(`Cancer type` = case_when(`Cancer type` == "Anus" ~ "Anal",
                                         `Cancer type` == "Cervix" ~ "Cervical",
                                         `Cancer type` == "Ovary" ~ "Ovarian",
                                         `Cancer type` == "Pancreas" ~ "Pancreatic",
                                         `Cancer type` == "Rectum" ~ "Rectal",
                                         `Cancer type` == "Uterus" ~ "Uterine",
                                         `Cancer type` == "Liver/Bile-duct" ~ "Liver/Bile Duct",
                                         `Cancer type` == "Esophagus" ~ "Oesophageal",
                                         `Cancer type` == "Head and Neck" ~ "Head and Neck",
                                         .default = `Cancer type`)) %>%
        filter(!(`Cancer type` %in% c("Urothelial Tract",  "Melanoma", "Colon", "Gallbladder", "Kidney", "Liver/Bile Duct" , "Ovarian"))) %>%
        select(`Cancer type`, `Curative intent radiotherapy/chemoradiotherapy: Change (%)`, `Curative intent radiotherapy/chemoradiotherapy: MCED (no.)`, `Curative intent radiotherapy/chemoradiotherapy: SoC (no.)`,`Outcome type`) %>%
        rename(change = `Curative intent radiotherapy/chemoradiotherapy: Change (%)`) %>%
        mutate(num = `Curative intent radiotherapy/chemoradiotherapy: MCED (no.)` -`Curative intent radiotherapy/chemoradiotherapy: SoC (no.)`,
               abs_num = abs(`Curative intent radiotherapy/chemoradiotherapy: MCED (no.)` -`Curative intent radiotherapy/chemoradiotherapy: SoC (no.)`)) %>%
        group_by(`Outcome type`) %>%
        mutate( `Cancer type` = fct_rev(fct_reorder(`Cancer type`, abs_num)),
                `Cancer type` = relevel(`Cancer type`, "All cancers combined")) %>%
        mutate(across(where(is.numeric), round,1))


fig_6_curative_radio <- ggplot(data = df_fig2_curative_radio %>% filter(`Cancer type` != "Uterine") ) +
        
        geom_bar(aes(x = `Cancer type`, y = change, fill = `Outcome type`), position= position_dodge(0.5), stat = "identity", width = .5) +
        #geom_point(aes(x = `Cancer type`, y = change, fill = `Outcome type`, size = abs_num), position= position_dodge(0.5), colour = "darkred", show.legend = FALSE) +
        labs(title = NULL, fill = "Screening round", x = "Cancer type", y = "Change in annual no. of cancers treated with\ncurative intent radiotherapy/chemoradiotherapy (%)") +
        theme_bw() +
        theme(text = element_text(size = 11, face="bold"),
              axis.text.x = element_text( size = 11, angle = 90, hjust = 1),
              axis.text.y = element_text( size = 11),
              legend.position=c(.7,.9),
              legend.background = element_blank(),
              legend.title=element_blank()) +
        scale_fill_discrete(labels=c("Initial screening round", "Steady-state screening programme"),
                            type=c("#3B1C52","#94724F")) +
        scale_y_continuous(limits = c(-50,200)) +
        annotate("segment", x = "Lung", xend = "Sarcoma", y = 140, yend = 140,
                 colour = "black", arrow = arrow(length = unit(0.2, "cm"), ends = "last")) +
        annotate("text", x = "Pancreatic", y = 130, label = "Ordered from largest to smallest absolute change in initial screening round")


ggsave(paste0("~/Miscellaneous/Treatments/Figures/", uptake, "fig_5_curative_resection.png"), fig_5_curative_resection, device = "png",
       width = 200,
       height = 150,
       units = "mm")

ggsave(paste0("~/Miscellaneous/Treatments/Figures/", uptake, "fig_6_curative_radio.png"), fig_6_curative_radio, device = "png",
       width = 200,
       height = 150,
       units = "mm")



#' _____________________________________________________________________________
# 5. Sensitivity analysis

## 5.1 Summary tables by IMD

### Table 5.1.1 Overall annual treatment volume for each cancer (all stages) and % change with MCED screening - prevalent round

df_table_5.1.1 <- df_3treatentXstageIMD_joined_intercept %>%
        select(`Index of Multiple Deprivation Quintile`, `Cancer type`, `Stage at diagnosis`, `Treatment modality`, `SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        rename(`SoC` = `SoC no. of tumours treated`,
               `Prev round`= `Prev no. of tumours treated`,
               `SS`= `SS no. of tumours treated`) %>%
        pivot_wider(names_from = `Treatment modality`, values_from = c(`SoC`, `Prev round`, `SS`), names_glue ="{`Treatment modality`}: {.value} (no.)") %>%
        group_by( `Cancer type`, `Index of Multiple Deprivation Quintile`) %>%
        summarise(across(contains(":"), \(x) sum(x))) %>%
        select(`Cancer type`, `Index of Multiple Deprivation Quintile`, contains("SoC"), contains("Prev")) %>%
        mutate(`Tumour resection: Change (%)` = ((`Tumour resection: Prev round (no.)`-`Tumour resection: SoC (no.)`)/`Tumour resection: SoC (no.)`) * 100,
               `Chemotherapy: Change (%)` = ((`Chemotherapy: Prev round (no.)`-`Chemotherapy: SoC (no.)`)/`Chemotherapy: SoC (no.)`) * 100,
               `Radiotherapy: Change (%)` = ((`Radiotherapy: Prev round (no.)`-`Radiotherapy: SoC (no.)`)/`Radiotherapy: SoC (no.)`) * 100) %>%
        mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
        select(`Cancer type`, `Index of Multiple Deprivation Quintile`, contains("Tumour resection"), contains("Chemotherapy"), contains("Radiotherapy")) %>%
        ungroup()

gt::gt(df_table_5.1.1) %>%
        tab_spanner_delim(delim = ":", columns = everything(), split = c("first", "last"), limit = NULL, reverse = FALSE)

addWorksheet(excel_output, sheetName = "5.1.1 Tx IMD vol (%) prev")
writeData(excel_output, sheet = "5.1.1 Tx IMD vol (%) prev", df_table_5.1.1)

#' __________
### Table 5.1.2 Overall annual treatment volume for each cancer (all stages) and % change with MCED screening - steady state

df_table_5.1.2 <- df_3treatentXstageIMD_joined_intercept %>%
        select(`Index of Multiple Deprivation Quintile`, `Cancer type`, `Stage at diagnosis`, `Treatment modality`, `SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        rename(`SoC` = `SoC no. of tumours treated`,
               `Prev round`= `Prev no. of tumours treated`,
               `SS`= `SS no. of tumours treated`) %>%
        pivot_wider(names_from = `Treatment modality`, values_from = c(`SoC`, `Prev round`, `SS`), names_glue ="{`Treatment modality`}: {.value} (no.)") %>%
        group_by(`Cancer type`, `Index of Multiple Deprivation Quintile`) %>%
        summarise(across(contains(":"), \(x) sum(x))) %>%
        select(`Cancer type`, `Index of Multiple Deprivation Quintile`, contains("SoC"), contains("SS")) %>%
        mutate(`Tumour resection: Change (%)` = ((`Tumour resection: SS (no.)`-`Tumour resection: SoC (no.)`)/`Tumour resection: SoC (no.)`) * 100,
               `Chemotherapy: Change (%)` = ((`Chemotherapy: SS (no.)`-`Chemotherapy: SoC (no.)`)/`Chemotherapy: SoC (no.)`) * 100,
               `Radiotherapy: Change (%)` = ((`Radiotherapy: SS (no.)`-`Radiotherapy: SoC (no.)`)/`Radiotherapy: SoC (no.)`) * 100) %>%
        mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
        select(`Cancer type`, `Index of Multiple Deprivation Quintile`, contains("Tumour resection"), contains("Chemotherapy"), contains("Radiotherapy"))%>%
        ungroup()

gt::gt(df_table_5.1.2) %>%
        tab_spanner_delim(delim = ":", columns = everything(), split = c("first", "last"), limit = NULL, reverse = FALSE)

addWorksheet(excel_output, sheetName = "5.1.2 Tx IMD vol (%) SS")
writeData(excel_output, sheet = "5.1.2 Tx IMD vol (%) SS", df_table_5.1.2)

#' __________
### Table 5.1.3 Annual treatment volume for treatment modality combination, for each cancer (all stages) and % change with MCED screening - prevalent round

df_table_5.1.3 <- df_8treatentXstageIMD_joined_intercept %>%
        select(`Index of Multiple Deprivation Quintile`,`Cancer type`, `Stage at diagnosis`, `Treatment modality`, `SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        rename(`SoC` = `SoC no. of tumours treated`,
               `Prev round`= `Prev no. of tumours treated`,
               `SS`= `SS no. of tumours treated`) %>%
        pivot_wider(names_from = `Treatment modality`, values_from = c(`SoC`, `Prev round`, `SS`), names_glue ="{`Treatment modality`}: {.value} (no.)") %>%
        group_by(`Cancer type`, `Index of Multiple Deprivation Quintile`) %>%
        summarise(across(contains(":"), \(x) sum(x))) %>%
        select(`Cancer type`, `Index of Multiple Deprivation Quintile`, contains("SoC"), contains("Prev")) %>%
        mutate(`Tumour resection only: Change (%)` = ((`Tumour resection only: Prev round (no.)`-`Tumour resection only: SoC (no.)`)/`Tumour resection only: SoC (no.)`) * 100,
               `Chemotherapy only: Change (%)` = ((`Chemotherapy only: Prev round (no.)`-`Chemotherapy only: SoC (no.)`)/`Chemotherapy only: SoC (no.)`) * 100,
               `Radiotherapy only: Change (%)` = ((`Radiotherapy only: Prev round (no.)`-`Radiotherapy only: SoC (no.)`)/`Radiotherapy only: SoC (no.)`) * 100,
               `Tumour resection and chemotherapy: Change (%)` = ((`Tumour resection and chemotherapy: Prev round (no.)`-`Tumour resection and chemotherapy: SoC (no.)`)/`Tumour resection and chemotherapy: SoC (no.)`) * 100,
               `Tumour resection and radiotherapy: Change (%)` = ((`Tumour resection and radiotherapy: Prev round (no.)`-`Tumour resection and radiotherapy: SoC (no.)`)/`Tumour resection and radiotherapy: SoC (no.)`) * 100,
               `Chemotherapy and radiotherapy: Change (%)` = ((`Chemotherapy and radiotherapy: Prev round (no.)`-`Chemotherapy and radiotherapy: SoC (no.)`)/`Chemotherapy and radiotherapy: SoC (no.)`) * 100,
               `Other care: Change (%)` = ((`Other care: Prev round (no.)`-`Other care: SoC (no.)`)/`Other care: SoC (no.)`) * 100,
               `Tumour resection, radiotherapy and chemotherapy: Change (%)` = ((`Tumour resection, radiotherapy and chemotherapy: Prev round (no.)`-`Tumour resection, radiotherapy and chemotherapy: SoC (no.)`)/`Tumour resection, radiotherapy and chemotherapy: SoC (no.)`) * 100) %>%
        mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
        select(`Cancer type`, `Index of Multiple Deprivation Quintile`, contains("Tumour resection only"), contains("Tumour resection and chemotherapy"),
               contains("Tumour resection and radiotherapy"), contains("Tumour resection, radiotherapy and chemotherapy"),
               contains("Chemotherapy and radiotherapy"), contains("Radiotherapy only"),
               contains("Chemotherapy only"), contains("Other care") ) %>%
        ungroup()

gt::gt(df_table_5.1.3) %>%
        tab_spanner_delim(delim = ":", columns = everything(), split = c("first", "last"), limit = NULL, reverse = FALSE)

addWorksheet(excel_output, sheetName = "5.1.3 Tx IMD combi (%) prev")
writeData(excel_output, sheet = "5.1.3 Tx IMD combi (%) prev", df_table_5.1.3)

#' __________
### Table 5.1.4 Annual treatment volume for treatment modality combination, for each cancer (all stages) and % change with MCED screening - steady state

df_table_5.1.4 <- df_8treatentXstageIMD_joined_intercept %>%
        select(`Index of Multiple Deprivation Quintile`, `Cancer type`, `Stage at diagnosis`, `Treatment modality`, `SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        rename(`SoC` = `SoC no. of tumours treated`,
               `Prev round`= `Prev no. of tumours treated`,
               `SS`= `SS no. of tumours treated`) %>%
        pivot_wider(names_from = `Treatment modality`, values_from = c(`SoC`, `Prev round`, `SS`), names_glue ="{`Treatment modality`}: {.value} (no.)") %>%
        group_by(`Cancer type`,`Index of Multiple Deprivation Quintile`) %>%
        summarise(across(contains(":"), \(x) sum(x))) %>%
        select(`Cancer type`, `Index of Multiple Deprivation Quintile`, contains("SoC"), contains("SS")) %>%
        mutate(`Tumour resection only: Change (%)` = ((`Tumour resection only: SS (no.)`-`Tumour resection only: SoC (no.)`)/`Tumour resection only: SoC (no.)`) * 100,
               `Chemotherapy only: Change (%)` = ((`Chemotherapy only: SS (no.)`-`Chemotherapy only: SoC (no.)`)/`Chemotherapy only: SoC (no.)`) * 100,
               `Radiotherapy only: Change (%)` = ((`Radiotherapy only: SS (no.)`-`Radiotherapy only: SoC (no.)`)/`Radiotherapy only: SoC (no.)`) * 100,
               `Tumour resection and chemotherapy: Change (%)` = ((`Tumour resection and chemotherapy: SS (no.)`-`Tumour resection and chemotherapy: SoC (no.)`)/`Tumour resection and chemotherapy: SoC (no.)`) * 100,
               `Tumour resection and radiotherapy: Change (%)` = ((`Tumour resection and radiotherapy: SS (no.)`-`Tumour resection and radiotherapy: SoC (no.)`)/`Tumour resection and radiotherapy: SoC (no.)`) * 100,
               `Chemotherapy and radiotherapy: Change (%)` = ((`Chemotherapy and radiotherapy: SS (no.)`-`Chemotherapy and radiotherapy: SoC (no.)`)/`Chemotherapy and radiotherapy: SoC (no.)`) * 100,
               `Other care: Change (%)` = ((`Other care: SS (no.)`-`Other care: SoC (no.)`)/`Other care: SoC (no.)`) * 100,
               `Tumour resection, radiotherapy and chemotherapy: Change (%)` = ((`Tumour resection, radiotherapy and chemotherapy: SS (no.)`-`Tumour resection, radiotherapy and chemotherapy: SoC (no.)`)/`Tumour resection, radiotherapy and chemotherapy: SoC (no.)`) * 100) %>%
        mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
        select(`Cancer type`, `Index of Multiple Deprivation Quintile`, contains("Tumour resection only"), contains("Tumour resection and chemotherapy"),
               contains("Tumour resection and radiotherapy"), contains("Tumour resection, radiotherapy and chemotherapy"),
               contains("Chemotherapy and radiotherapy"), contains("Radiotherapy only"),
               contains("Chemotherapy only"), contains("Other care") )

gt::gt(df_table_5.1.4) %>%
        tab_spanner_delim(delim = ":", columns = everything(), split = c("first", "last"), limit = NULL, reverse = FALSE)

addWorksheet(excel_output, sheetName = "5.1.4 Tx IMD combi (%) SS")
writeData(excel_output, sheet = "5.1.4 Tx IMD combi (%) SS", df_table_5.1.4)

#' __________
## 5.2 Cancer alliance
### Table 5.2.1 Overall annual treatment volume for each cancer (all stages) and % change with MCED screening - prevalent round

df_table_5.2.1 <- df_3treatentXstageCA_joined_intercept %>%
        select(`Cancer Alliance`, `Cancer type`, `Stage at diagnosis`, `Treatment modality`, `SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        rename(`SoC` = `SoC no. of tumours treated`,
               `Prev round`= `Prev no. of tumours treated`,
               `SS`= `SS no. of tumours treated`) %>%
        pivot_wider(names_from = `Treatment modality`, values_from = c(`SoC`, `Prev round`, `SS`), names_glue ="{`Treatment modality`}: {.value} (no.)") %>%
        group_by( `Cancer type`, `Cancer Alliance`) %>%
        summarise(across(contains(":"), \(x) sum(x))) %>%
        select(`Cancer type`, `Cancer Alliance`, contains("SoC"), contains("Prev")) %>%
        mutate(`Tumour resection: Change (%)` = ((`Tumour resection: Prev round (no.)`-`Tumour resection: SoC (no.)`)/`Tumour resection: SoC (no.)`) * 100,
               `Chemotherapy: Change (%)` = ((`Chemotherapy: Prev round (no.)`-`Chemotherapy: SoC (no.)`)/`Chemotherapy: SoC (no.)`) * 100,
               `Radiotherapy: Change (%)` = ((`Radiotherapy: Prev round (no.)`-`Radiotherapy: SoC (no.)`)/`Radiotherapy: SoC (no.)`) * 100) %>%
        mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
        select(`Cancer type`, `Cancer Alliance`, contains("Tumour resection"), contains("Chemotherapy"), contains("Radiotherapy")) %>%
        ungroup()

gt::gt(df_table_5.2.1) %>%
        tab_spanner_delim(delim = ":", columns = everything(), split = c("first", "last"), limit = NULL, reverse = FALSE)

addWorksheet(excel_output, sheetName = "5.2.1 Tx CA vol (%) prev")
writeData(excel_output, sheet = "5.2.1 Tx CA vol (%) prev", df_table_5.2.1)

#' __________
### Table 5.2.2 Overall annual treatment volume for each cancer (all stages) and % change with MCED screening - steady state

df_table_5.2.2 <- df_3treatentXstageCA_joined_intercept %>%
        select(`Cancer Alliance`, `Cancer type`, `Stage at diagnosis`, `Treatment modality`, `SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        rename(`SoC` = `SoC no. of tumours treated`,
               `Prev round`= `Prev no. of tumours treated`,
               `SS`= `SS no. of tumours treated`) %>%
        pivot_wider(names_from = `Treatment modality`, values_from = c(`SoC`, `Prev round`, `SS`), names_glue ="{`Treatment modality`}: {.value} (no.)") %>%
        group_by(`Cancer type`, `Cancer Alliance`) %>%
        summarise(across(contains(":"), \(x) sum(x))) %>%
        select(`Cancer type`, `Cancer Alliance`, contains("SoC"), contains("SS")) %>%
        mutate(`Tumour resection: Change (%)` = ((`Tumour resection: SS (no.)`-`Tumour resection: SoC (no.)`)/`Tumour resection: SoC (no.)`) * 100,
               `Chemotherapy: Change (%)` = ((`Chemotherapy: SS (no.)`-`Chemotherapy: SoC (no.)`)/`Chemotherapy: SoC (no.)`) * 100,
               `Radiotherapy: Change (%)` = ((`Radiotherapy: SS (no.)`-`Radiotherapy: SoC (no.)`)/`Radiotherapy: SoC (no.)`) * 100) %>%
        mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
        select(`Cancer type`, `Cancer Alliance`, contains("Tumour resection"), contains("Chemotherapy"), contains("Radiotherapy"))%>%
        ungroup()

gt::gt(df_table_5.2.2) %>%
        tab_spanner_delim(delim = ":", columns = everything(), split = c("first", "last"), limit = NULL, reverse = FALSE)

addWorksheet(excel_output, sheetName = "5.2.2 Tx CA vol (%) SS")
writeData(excel_output, sheet = "5.2.2 Tx CA vol (%) SS", df_table_5.2.2)

#' __________
### Table 5.1.3 Annual treatment volume for treatment modality combination, for each cancer (all stages) and % change with MCED screening - prevalent round

df_table_5.2.3 <- df_8treatentXstageCA_joined_intercept %>%
        select(`Cancer Alliance`,`Cancer type`, `Stage at diagnosis`, `Treatment modality`, `SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        rename(`SoC` = `SoC no. of tumours treated`,
               `Prev round`= `Prev no. of tumours treated`,
               `SS`= `SS no. of tumours treated`) %>%
        pivot_wider(names_from = `Treatment modality`, values_from = c(`SoC`, `Prev round`, `SS`), names_glue ="{`Treatment modality`}: {.value} (no.)") %>%
        group_by(`Cancer type`, `Cancer Alliance`) %>%
        summarise(across(contains(":"), \(x) sum(x))) %>%
        select(`Cancer type`, `Cancer Alliance`, contains("SoC"), contains("Prev")) %>%
        mutate(`Tumour resection only: Change (%)` = ((`Tumour resection only: Prev round (no.)`-`Tumour resection only: SoC (no.)`)/`Tumour resection only: SoC (no.)`) * 100,
               `Chemotherapy only: Change (%)` = ((`Chemotherapy only: Prev round (no.)`-`Chemotherapy only: SoC (no.)`)/`Chemotherapy only: SoC (no.)`) * 100,
               `Radiotherapy only: Change (%)` = ((`Radiotherapy only: Prev round (no.)`-`Radiotherapy only: SoC (no.)`)/`Radiotherapy only: SoC (no.)`) * 100,
               `Tumour resection and chemotherapy: Change (%)` = ((`Tumour resection and chemotherapy: Prev round (no.)`-`Tumour resection and chemotherapy: SoC (no.)`)/`Tumour resection and chemotherapy: SoC (no.)`) * 100,
               `Tumour resection and radiotherapy: Change (%)` = ((`Tumour resection and radiotherapy: Prev round (no.)`-`Tumour resection and radiotherapy: SoC (no.)`)/`Tumour resection and radiotherapy: SoC (no.)`) * 100,
               `Chemotherapy and radiotherapy: Change (%)` = ((`Chemotherapy and radiotherapy: Prev round (no.)`-`Chemotherapy and radiotherapy: SoC (no.)`)/`Chemotherapy and radiotherapy: SoC (no.)`) * 100,
               `Other care: Change (%)` = ((`Other care: Prev round (no.)`-`Other care: SoC (no.)`)/`Other care: SoC (no.)`) * 100,
               `Tumour resection, radiotherapy and chemotherapy: Change (%)` = ((`Tumour resection, radiotherapy and chemotherapy: Prev round (no.)`-`Tumour resection, radiotherapy and chemotherapy: SoC (no.)`)/`Tumour resection, radiotherapy and chemotherapy: SoC (no.)`) * 100) %>%
        mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
        select(`Cancer type`, `Cancer Alliance`, contains("Tumour resection only"), contains("Tumour resection and chemotherapy"),
               contains("Tumour resection and radiotherapy"), contains("Tumour resection, radiotherapy and chemotherapy"),
               contains("Chemotherapy and radiotherapy"), contains("Radiotherapy only"),
               contains("Chemotherapy only"), contains("Other care") ) %>%
        ungroup()

gt::gt(df_table_5.2.3) %>%
        tab_spanner_delim(delim = ":", columns = everything(), split = c("first", "last"), limit = NULL, reverse = FALSE)

addWorksheet(excel_output, sheetName = "5.2.3 Tx CA combi (%) prev")
writeData(excel_output, sheet = "5.2.3 Tx CA combi (%) prev", df_table_5.2.3)

#' __________
### Table 5.2.4 Annual treatment volume for treatment modality combination, for each cancer (all stages) and % change with MCED screening - steady state
df_table_5.2.4 <- df_8treatentXstageCA_joined_intercept %>%
        select(`Cancer Alliance`, `Cancer type`, `Stage at diagnosis`, `Treatment modality`, `SoC no. of tumours treated`, `Prev no. of tumours treated`, `SS no. of tumours treated`) %>%
        rename(`SoC` = `SoC no. of tumours treated`,
               `Prev round`= `Prev no. of tumours treated`,
               `SS`= `SS no. of tumours treated`) %>%
        pivot_wider(names_from = `Treatment modality`, values_from = c(`SoC`, `Prev round`, `SS`), names_glue ="{`Treatment modality`}: {.value} (no.)") %>%
        group_by(`Cancer type`,`Cancer Alliance`) %>%
        summarise(across(contains(":"), \(x) sum(x))) %>%
        select(`Cancer type`, `Cancer Alliance`, contains("SoC"), contains("SS")) %>%
        mutate(`Tumour resection only: Change (%)` = ((`Tumour resection only: SS (no.)`-`Tumour resection only: SoC (no.)`)/`Tumour resection only: SoC (no.)`) * 100,
               `Chemotherapy only: Change (%)` = ((`Chemotherapy only: SS (no.)`-`Chemotherapy only: SoC (no.)`)/`Chemotherapy only: SoC (no.)`) * 100,
               `Radiotherapy only: Change (%)` = ((`Radiotherapy only: SS (no.)`-`Radiotherapy only: SoC (no.)`)/`Radiotherapy only: SoC (no.)`) * 100,
               `Tumour resection and chemotherapy: Change (%)` = ((`Tumour resection and chemotherapy: SS (no.)`-`Tumour resection and chemotherapy: SoC (no.)`)/`Tumour resection and chemotherapy: SoC (no.)`) * 100,
               `Tumour resection and radiotherapy: Change (%)` = ((`Tumour resection and radiotherapy: SS (no.)`-`Tumour resection and radiotherapy: SoC (no.)`)/`Tumour resection and radiotherapy: SoC (no.)`) * 100,
               `Chemotherapy and radiotherapy: Change (%)` = ((`Chemotherapy and radiotherapy: SS (no.)`-`Chemotherapy and radiotherapy: SoC (no.)`)/`Chemotherapy and radiotherapy: SoC (no.)`) * 100,
               `Other care: Change (%)` = ((`Other care: SS (no.)`-`Other care: SoC (no.)`)/`Other care: SoC (no.)`) * 100,
               `Tumour resection, radiotherapy and chemotherapy: Change (%)` = ((`Tumour resection, radiotherapy and chemotherapy: SS (no.)`-`Tumour resection, radiotherapy and chemotherapy: SoC (no.)`)/`Tumour resection, radiotherapy and chemotherapy: SoC (no.)`) * 100) %>%
        mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
        select(`Cancer type`, `Cancer Alliance`, contains("Tumour resection only"), contains("Tumour resection and chemotherapy"),
               contains("Tumour resection and radiotherapy"), contains("Tumour resection, radiotherapy and chemotherapy"),
               contains("Chemotherapy and radiotherapy"), contains("Radiotherapy only"),
               contains("Chemotherapy only"), contains("Other care") )

gt::gt(df_table_5.2.4) %>%
        tab_spanner_delim(delim = ":", columns = everything(), split = c("first", "last"), limit = NULL, reverse = FALSE)

addWorksheet(excel_output, sheetName = "5.2.4 Tx CA combi (%) SS")
writeData(excel_output, sheet = "5.2.4 Tx CA combi (%) SS", df_table_5.2.4)


saveWorkbook(excel_output, paste0("~/Miscellaneous/Treatments/", uptake,"%_participation_results_tables_newdata_curativecombos_2dp_radtogether.xlsx"), overwrite = TRUE)

































