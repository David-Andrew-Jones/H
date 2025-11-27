# ----------------------------------------------------------------------------------------------------------------------------------------
#' @project EVA-34279-00
#' @name Main.R
#' @description MCED Model
#' @version 1.0
#' @author Evidera
# ----------------------------------------------------------------------------------------------------------------------------------------

# ---------- Initialize environment ----------

# # Set directory location
rm(list = ls())

start_time <- Sys.time()
args<-commandArgs(TRUE)
setwd(args[1])  # CHANGE - Uncomment this line while deploying
# setwd('C:/Local Folder/GRAIL/R conversion/14Aug23 cost growth rate')  # CHANGE - Comment this line while deploying

# Source required files
source('GlobalFunctions.R')

# Store directory locations
current_dir <- paste0(getwd(),'/')
inputs_dir <- paste0(getwd(),"/Excel2R/")
results_dir <- paste0(getwd(),"/Results/Intermediate Results/")


# ---------- Read in inputs ----------
setwd(inputs_dir) # set input directory
read_inputs() 
setwd(current_dir) # reset working directory

# ---------- Inputs pre-processing ----------
source('InputPreprocess.R')
source('InterceptModel.R')

# ---------- Model Engine ----------
source('Engine.R')

# ---------- Summarize Results ----------
source('ExportResults.R')

# Export merged results to CSV file 
write.csv(res_export,paste0(results_dir,'results_',iPop,'-',start_age_band,'.csv'))

end_time <- Sys.time()
print(end_time - start_time)