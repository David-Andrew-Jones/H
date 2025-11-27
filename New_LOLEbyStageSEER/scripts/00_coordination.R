#______________________________________________________________________________#
#____ 01_prepare_SEER_data
#' Loads in individual patient data and life tables

#______________________________________________________________________________#
#' Libraries
library(tidyverse)
library(readxl)
library(SHELF)
library(survextrap)
library(tidytext)
library(ggmosaic)
library(openxlsx)

#______________________________________________________________________________#
#' Future paths
results_path <- "~/LOLEbyStageSEER/results/mcmc/primary_analysis/"
results_path_sens_CF <- "~/LOLEbyStageSEER/results/mcmc/sens_CF/"

#' Create a folder for the tables and plots to go in and set path
folder_path <- "~/LOLEbyStageSEER/results/mcmc/tables_and_figures/" # example - change accordingly

#' Save tables as single workbook - use open xlsx package
#' Create workbook
excel_output <- createWorkbook()

#______________________________________________________________________________#

#' Load in SEER and life table data and clean.
#' Also outputs Table 1 §- Patient characteristics.
source("scripts/01_prepare_SEER_data.R")

#' Define functions to run the extrapolation model.
source("scripts/02_function_extrapolate_M_spline.R")

#' Run the function over the patient data and save results table to output paths.
#' There are different versions relating to how this is performed:
#' 03a - input data all in one big data frame, map split data into function - only feasbile when using "opt" method
#' 03b - split the data first and then run functions on each split - For MCMC method - each site/age/stage split will still take days to run
#' 03c - Like 3a but with different model specifications, for lung cancer only
# source("scripts/03a_analysis_M_spline.R")
# source("scripts/03b_analysis_M_spline_splitbysite.R")
# source("scripts/03a_analysis_M_spline_sensitivities.R")

#' Load in results tables.
#' Derives tables and figures relating to per patient LOLE outcomes.
source("scripts/04a_LOLE_results.R")

#' Load in sensitivity scenario results tables.
#' Compare to primary analysis LOLE results
source("scripts/04b_LOLE_results_sensitivities.R")

#' Derive aggregate LOLE results include stage-shift scenarios
source("scripts/05_aggregateLOLE_and_shift_scenarios.R")

#' Derives proportional LOLE tables and figures 
source("scripts/06_proportional_LOLE.R")

#______________________________________________________________________________#
#' Save tables

saveWorkbook(excel_output, paste0(folder_path, "pubication_tables.xlsx"), overwrite = TRUE)

#______________________________________________________________________________#








