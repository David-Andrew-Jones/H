# ----------------------------------------------------------------------------------------------------------------------------------------
#' @project EVA-34279-00
#' @name DSA.R
#' @description DSA module
#' @version 1.0
#' @author Evidera
# ----------------------------------------------------------------------------------------------------------------------------------------

start_time <- Sys.time()

# Store directory locations
args<-commandArgs(TRUE)
setwd(args[1])  # CHANGE - Uncomment this line while deploying
# setwd('C:/Local Folder/GRAIL/R conversion/20Jun23 PSA test') # CHANGE - Comment this line while deploying

# Source required files
source('GlobalFunctions.R')

current_dir <- paste0(getwd(),'/')
inputs_dir <- paste0(getwd(),"/Excel2R/")
results_dir <- paste0(getwd(),"/Results/Intermediate Results/")
sensitivity_dir <- paste0(getwd(),"/Sensitivity/DSA/")

setwd(inputs_dir)
read_inputs() 

# Read all DSA inputs and store values in memory
read_inputs_DSA = function() {
  lowers = vector('list',nDSA)
  uppers = vector('list',nDSA)
  #Read dataframes from csv files in the inputs folder
  assign('DSAvarnames',as.data.frame(read_csv('DSAvarnames.csv', col_names = FALSE, show_col_types = FALSE)),pos = 1)
  assign('DSAvarnames_dup',as.data.frame(read_csv('DSAvarnames_dup.csv', col_names = FALSE, show_col_types = FALSE)),pos = 1)
  assign('DSAinclude',as.data.frame(read_csv('DSAinclude.csv', col_names = FALSE, show_col_types = FALSE)),pos = 1)
  for (i in 1:nDSA){
  	if(DSAvarnames_dup[i,1]<1){
  		lowers[[i]] <- as.data.frame(read_csv(paste0(DSAvarnames[i,1],'_lower.csv'), col_names = FALSE, show_col_types = FALSE))
  		uppers[[i]] <- as.data.frame(read_csv(paste0(DSAvarnames[i,1],'_upper.csv'), col_names = FALSE, show_col_types = FALSE))
  	} else {
  		lowers[[i]] <- as.data.frame(read_csv(paste0(DSAvarnames[i,1],'_lower',as.character(DSAvarnames_dup[i,1]),'.csv'), col_names = FALSE, show_col_types = FALSE))
  		uppers[[i]] <- as.data.frame(read_csv(paste0(DSAvarnames[i,1],'_upper',as.character(DSAvarnames_dup[i,1]),'.csv'), col_names = FALSE, show_col_types = FALSE))
  	}
  }
  DSA = list('Lower'=lowers,'Upper'=uppers)	# Set DSA list for indexing
  return(DSA)
}

setwd(sensitivity_dir)
DSA <- read_inputs_DSA()

setwd(current_dir)

results_DSA_dir <- paste0(getwd(),"/Results/DSA/")

DSAvarBase = vector('list',nDSA)	# Initialize DSA base case value list
DSA_res = array(data=0,dim=c(nDSA,12))	# Initialize DSA results
for (iDSA in 1:nDSA) {
	if(DSAinclude[iDSA,1]==1){
		for (jDSA in 1:2) {	#loop over upper and lower values
			
			# Reset inputs
			setwd(inputs_dir) # set input directory
			read_inputs()
			setwd(current_dir) # reset working directory
		
			if(class(get(DSAvarnames[iDSA,1]))=='data.frame'){
				assign(DSAvarnames[iDSA,1],as.data.frame(DSA[[jDSA]][[iDSA]]),envir = environment())
			} else{
				assign(DSAvarnames[iDSA,1],as.numeric(DSA[[jDSA]][[iDSA]]),envir = environment())
			}

			# Run Model
			source("InputPreprocess.R")
			source("InterceptModel.R")
			source("Engine.R")

			# Summarize Results
			source("ExportResultsSA.R")

			# Store DSA results
			DSA_res[iDSA,(1+(jDSA-1)*6):(6+(jDSA-1)*6)] <- res_export
		} # End upper and lower value loop
	} else {
		DSA_res[iDSA,1:12] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
	} # end if include
} # end loop over DSA parameters

# Write DSA results to file
write.csv(DSA_res,paste0(getwd(),'/Results/results_DSA.csv'))

end_time <- Sys.time()
print(end_time - start_time)
