# ----------------------------------------------------------------------------------------------------------------------------------------
#' @project EVA-34279-00
#' @name PSA.R
#' @description PSA module
#' @version 1.0
#' @author Evidera
# ----------------------------------------------------------------------------------------------------------------------------------------

# # Sensitivity
# setwd('C:/Local Folder/GRAIL/R conversion/13Jun23')
# # Source required files
# source('GlobalFunctions.R')
# sensitivity_dir <- paste0(getwd(),"/Sensitivity/")
# setwd(sensitivity_dir)
# read_data_from_file("SensitivitySamplesForPSA.csv")
# # Sensitivity tables includes 4000 iteration of random draws,
# # each iteration stored in 4 rows (stages) and 22 cancers in columns (number of columns: 46, rows: 16001)
# # Order in Excel: Lung and bronchus, 2 Colon and Rectum, 3 Pancreas, 4 Liver and Intrahepatic Bile Duct, 5 Breast: HR-negative, 6 Esophagus, 7 Head and Neck, 8 Stomach, 9 Ovarian, 10 Kidney and Renal Pelvis, 11 Prostate, 12 Breast: HR-positive, 13 Lymphoma, 14 Anus, 15 Uterus, 16 Bladder, 17 Cervix, 18 Urothelial, 19 Other
# #Corrected Col: 23,11,33,21,7,13,17,39,31,19,35,7,25,3,43,5,9,NA(assume 0% stage I-III; 100% stage IV),29
# cols_sensitivity <- c(23,11,33,21,7,13,17,39,31,19,35,7,25,3,43,5,9,1,29)
# nSim <- 4000
# sensitivity_PSA <- array(dim=c(76,1,nSim))
# 
# for (i in 1:nSim){
#   for (j in 1:length(cols_sensitivity)){
#     sensitivity_PSA[(1:4)+(j-1)*4,1,i] <- SensitivitySamplesForPSA[(1:4)+(i-1)*4+1,cols_sensitivity[j]]
#   }
# }
# write.csv(sensitivity_PSA,paste0(getwd(),'/sensitivity_PSA.csv'))
# #Sensitivity for Urothelial was assumed to be 0% for stage I-III and 100% for stage IV. Update is needed in sensitivity_PSA.csv file


start_time <- Sys.time()

#Constants
NormDist <- qnorm(0.975)

# Store directory locations
args<-commandArgs(TRUE)
setwd(args[1])  # CHANGE - Uncomment this line while deploying
# setwd('C:/Local Folder/GRAIL/R conversion/13Jun23') # CHANGE - Comment this line while deploying

# Source required files
source('GlobalFunctions.R')

current_dir <- paste0(getwd(),'/')
inputs_dir <- paste0(getwd(),"/Excel2R/")
results_dir <- paste0(getwd(),"/Results/Intermediate Results/")
sensitivity_dir <- paste0(getwd(),"/Sensitivity/")

setwd(inputs_dir)
read_inputs() 
setwd(sensitivity_dir)
read_data_from_file("sensitivity_PSA.csv")
setwd(current_dir)

#---- PSA Setup ----
nSim <- nPSA

SE_or_perc <- c(optPSA_soc_test_costs == 2, 
               optPSA_costs_cancer_tx == 2, 
               optPSA_costs_workup_FP == 2, 
               optPSA_costs_workup_misdiagnosis == 2, 
               optPSA_util_cancer == 2)

stdErr_soc_test_costs <- pctPSA_soc_test_costs
stdErr_costs_cancer_tx <- pctPSA_costs_cancer_tx
stdErr_costs_workup_FP <- pctPSA_costs_workup_FP
stdErr_costs_workup_misdiagnosis <- pctPSA_costs_workup_misdiagnosis
stdErr_util_cancer <- pctPSA_util_cancer

ifelse(SE_or_perc[1],soc_test_costs_SE <- stdErr_soc_test_costs*soc_test_costs,soc_test_costs_SE)
ifelse(SE_or_perc[2],costs_cancer_tx_SE <- stdErr_costs_cancer_tx*costs_cancer_tx,costs_cancer_tx_SE)
ifelse(SE_or_perc[3],costs_workup_FP_SE <- stdErr_costs_workup_FP*costs_workup_FP,costs_workup_FP_SE)
ifelse(SE_or_perc[4],costs_workup_misdiagnosis_SE <- stdErr_costs_workup_misdiagnosis*costs_workup_misdiagnosis,costs_workup_misdiagnosis_SE)
ifelse(SE_or_perc[5],util_cancer_SE <- stdErr_util_cancer*util_cancer,util_cancer_SE)

#---- PSA Sampling Function ----
f_PSA_samples <- function(soc_test_costs,
                          #sensitivity,
                          costs_cancer_tx,
                          costs_workup_misdiagnosis,
                          costs_workup_FP,
                          util_cancer) {
  tmp_samples <- list()
  
  # SOC test costs
  sample_soc_test_costs <- array(dim=c(length(soc_test_costs),1,nSim))
  for (i in 1:length(soc_test_costs)){
    set.seed(11)  # use the same random seed assuming full correlation
    sample_soc_test_costs[i,1,1:nSim] <- f_rgamma(nSim, soc_test_costs[i], soc_test_costs_SE[i,])
  }
  tmp_samples$soc_test_costs <- sample_soc_test_costs
  
  # Costs workup misdiagnosis
  sample_costs_workup_misdiagnosis <- array(dim=c(length(costs_workup_misdiagnosis[,1]),1,nSim))
  for (i in 1:length(costs_workup_misdiagnosis[,1])){
    set.seed(12)  # use the same random seed assuming full correlation
    sample_costs_workup_misdiagnosis[i,1,1:nSim] <- f_rgamma(nSim, costs_workup_misdiagnosis[i,], costs_workup_misdiagnosis_SE[i,])
  }
  tmp_samples$costs_workup_misdiagnosis <- sample_costs_workup_misdiagnosis
  
  # Costs workup FP
  sample_costs_workup_FP <- array(dim=c(length(costs_workup_FP[,1]),1,nSim))
  for (i in 1:length(costs_workup_FP[,1])){
    set.seed(13)  # use the same random seed assuming full correlation
    sample_costs_workup_FP[i,1,1:nSim] <- f_rgamma(nSim, costs_workup_FP[i,], costs_workup_FP_SE[i,])
  }
  tmp_samples$costs_workup_FP <- sample_costs_workup_FP
  
  # Costs cancer tx
  sample_costs_cancer_tx <- array(0,dim=c(length(costs_cancer_tx[,1]),length(costs_cancer_tx[1,]),nSim))
  for (i in 1:length(costs_cancer_tx[1,])) {
    for (j in 1:length(costs_cancer_tx[,1])) {
      set.seed(14)  # use the same random seed assuming full correlation
      sample_costs_cancer_tx[j,i,1:nSim] <- f_rgamma(nSim, costs_cancer_tx[j,i], costs_cancer_tx_SE[j,i])
    }
  }
  tmp_samples$costs_cancer_tx <- sample_costs_cancer_tx
  
  # Sensitivity
  # Skip, No randomisation here, samples are provided by the client
  
  # Utility Cancer
  sample_util_cancer <- array(0,dim=c(length(util_cancer[,1]),length(util_cancer[1,]),nSim))
  for (i in 1:length(util_cancer[1,])) {
    for (j in 1:length(util_cancer[,1])) {
      set.seed(15)  # use the same random seed assuming full correlation
      sample_util_cancer[j,i,1:nSim] <- f_rbeta(nSim, util_cancer[j,i], util_cancer_SE[j,i])
      if (i == 6) {sample_util_cancer[j,i,1:nSim] <- 1}
    }
  }
  tmp_samples$util_cancer <- sample_util_cancer
  
  #Return sampled values into
  return(tmp_samples)
}


#---- PSA Engine ----
PSA_res<-array(0,dim=c(nSim,6))

# Inputs pre-processing
source('InputPreprocess.R')

# PSA sampling
PSA_samples <- f_PSA_samples(soc_test_costs,
                             #sensitivity,
                             costs_cancer_tx,
                             costs_workup_misdiagnosis,
                             costs_workup_FP,
                             util_cancer)

# PSA loop
for (iSim in 1:nSim) {

  # Generate stage shift matrix
  PSA_sensitivity <- as.numeric(sensitivity_PSA[,iSim])
  sensitivity_intercept <- as.data.frame(PSA_sensitivity)
  source('InterceptModel.R')
  
  # other PSA inputs
  # soc_test_costs <- PSA_samples$soc_test_costs[,,iSim] * incl_cancer
  sensitivity <- as.numeric(PSA_sensitivity)
  # costs_cancer_tx <- PSA_samples$costs_cancer_tx[,,iSim]
  # costs_workup_misdiagnosis <- PSA_samples$costs_workup_misdiagnosis[,,iSim]
  # costs_workup_FP <- PSA_samples$costs_workup_FP[,,iSim]
  # util_cancer <- PSA_samples$util_cancer[,,iSim]
  
  # Run Model
  source('Engine.R')
  source('ExportResultsSA.R')
  # source('ExportResults.R')
  
  # Store PSA results
  PSA_res[iSim,] <- res_export
}

write.csv(PSA_res,paste0(getwd(),'/Results/results_PSA.csv'))

end_time <- Sys.time()
print(end_time - start_time)
