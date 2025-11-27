#global parameters we will use over and over again to keep situations consistent
PRE_SPEC = c("Anus","Bladder","Colon/Rectum","Esophagus","Head and Neck","Liver/Bile-duct","Lymphoma","Lung","Ovary","Pancreas","Stomach") #cancers which will nonseq, plasma cell neoplasm out
UNSTAGEABLE = c("Lymphoid Leukemia","Myeloid Neoplasm","Plasma Cell Neoplasm") #
PERCENT_SKIP = c(0, 0.1, 0.25, 0.5) #perc that are nonseq when models are blended together
START_AGE <- 50 #Age first eligible for screening
END_AGE <- 79 #Age last screening
SCREEN_INTERVAL = 1
DWELL_TIME = c("old_2","old_3","old_4")
age_groups = c("50-54","55-59","60-64","65-69","70-74","75-79")#5yr age bands in NHS-Galleri trial 
age_groups_tot = c("50-54","55-59","60-64","65-69","70-74","75-79","50-79")#5yr age bands in NHS-Galleri trial, plus total age group. 
age_groups_plus = c("50-54","55-59","60-64","65-69","70-74","75-79","80-84")#for survival data - plus older age band to fill in where missing data
PerCan = c("Urothelial Tract","Breast", "Thyroid", "Stomach", "Sarcoma", "Plasma Cell Neoplasm", "Pancreas", "[OTHER]",               
           "Esophagus", "Myeloid Neoplasm",   "Melanoma",  "Lymphoma" ,  "Lymphoid Leukemia",  
           "Lung", "Liver/Bile-duct" , "Kidney", "Head and Neck", "Gallbladder", "Colon/Rectum", "Bladder", "Anus")# persons cancers - prostate, ovary, cervix, uterus out
FemCan = c("Ovary","Uterus","Cervix")
MalCan = c("Prostate")

#NO prior history model
NPH_MODEL = "NPH" #for reading into df
NO_PRIOR_HISTORY = c("I","II","III") #stages to be skipped if we assuming no prior history
NPH_DWELL = 0.0001 #no prior history dwell time
NPH_SLIP_RATE = 1

#prior history model - short dwell time in stage I only model
PH_MODEL = "PH" #for reading into df
PRIOR_HISTORY = c("II","III") #stages to be skipped if some prior history (i.e some period in 1)
E_PRIOR_HISTORY = c("I") #to have some time in stage I
PH_DWELL = 0.5 #6months prior history dwell time
