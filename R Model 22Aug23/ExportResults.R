# ----------------------------------------------------------------------------------------------------------------------------------------
#' @project EVA-34279-00
#' @name ExportResults.R
#' @description Summarize results
#' @version 1.0
#' @author Evidera
# ----------------------------------------------------------------------------------------------------------------------------------------

# ---------- Summarize Results -----------
# ---------- Aggregate Number Diagnosis Outcomes -----------
# Cancer diagnosis
res_soc_diagnosed_1 <- sum(rowSums(soc_markov_trace[,c(seq(3,2+n_cancer*4,by=4))]))
res_soc_diagnosed_2 <- sum(rowSums(soc_markov_trace[,c(seq(4,2+n_cancer*4,by=4))]))
res_soc_diagnosed_3 <- sum(rowSums(soc_markov_trace[,c(seq(5,2+n_cancer*4,by=4))]))
res_soc_diagnosed_4 <- sum(rowSums(soc_markov_trace[,c(seq(6,2+n_cancer*4,by=4))]))
res_MCED_diagnosed_1 <- sum(rowSums(time_shift_markov_trace[,c(seq(3,2+n_cancer*4,by=4))]))
res_MCED_diagnosed_2 <- sum(rowSums(time_shift_markov_trace[,c(seq(4,2+n_cancer*4,by=4))]))
res_MCED_diagnosed_3 <- sum(rowSums(time_shift_markov_trace[,c(seq(5,2+n_cancer*4,by=4))]))
res_MCED_diagnosed_4 <- sum(rowSums(time_shift_markov_trace[,c(seq(6,2+n_cancer*4,by=4))]))
res_MCED_diagnosed_1_TS <- sum(rowSums(time_shift_markov_trace_TS[,c(seq(3,2+n_cancer*4,by=4))]))
res_MCED_diagnosed_2_TS <- sum(rowSums(time_shift_markov_trace_TS[,c(seq(4,2+n_cancer*4,by=4))]))
res_MCED_diagnosed_3_TS <- sum(rowSums(time_shift_markov_trace_TS[,c(seq(5,2+n_cancer*4,by=4))]))
res_MCED_diagnosed_4_TS <- sum(rowSums(time_shift_markov_trace_TS[,c(seq(6,2+n_cancer*4,by=4))]))

# Overdiagnosis
res_MCED_total_overdiagnosed <- sum(overdiagnosed_time_shift_markov_trace)
res_MCED_overdiagnosed_1 <- sum(rowSums(overdiagnosed_time_shift_markov_trace[,c(seq(3,2+n_cancer*4,by=4))]))
res_MCED_overdiagnosed_2 <- sum(rowSums(overdiagnosed_time_shift_markov_trace[,c(seq(4,2+n_cancer*4,by=4))]))
res_MCED_overdiagnosed_3 <- sum(rowSums(overdiagnosed_time_shift_markov_trace[,c(seq(5,2+n_cancer*4,by=4))]))
res_MCED_overdiagnosed_4 <- sum(rowSums(overdiagnosed_time_shift_markov_trace[,c(seq(6,2+n_cancer*4,by=4))]))

# Total number outcomes
res_number_outcomes <-
  rbind(
    c(res_MCED_total_diagnosed, res_soc_total_diagnosed), #Total number diagnosed
    c(res_MCED_diagnosed_1, res_soc_diagnosed_1), #Total diagnosed stage I
    c(res_MCED_diagnosed_2, res_soc_diagnosed_2), #Total diagnosed stage II
    c(res_MCED_diagnosed_3, res_soc_diagnosed_3), #Total diagnosed stage III
    c(res_MCED_diagnosed_4, res_soc_diagnosed_4), #Total diagnosed stage IV
    
    c(res_MCED_total_overdiagnosed, 0), #Total number overdiagnosed
    c(res_MCED_overdiagnosed_1, 0), #Total overdiagnosed stage I
    c(res_MCED_overdiagnosed_2, 0),#Total overdiagnosed stage II
    c(res_MCED_overdiagnosed_3, 0), #Total overdiagnosed stage III
    c(res_MCED_overdiagnosed_4, 0), #Total overdiagnosed stage Iv
    
    c(sum(res_MCED_soc_screen$soc_FPs,res_MCED_MCED_screen$MCED_FPs), sum(res_soc_soc_screen$soc_FPs)), # Total FPs
    c(sum(misdiagnosis), 0) # Total misdiagnosed
  )

names_res_number_row <- c('diagnosed_total', 'diagnosed_1', 'diagnosed_2', 'diagnosed_3', 'diagnosed_4',
                          'overdiagnosed_total', 'overdiagnosed_1', 'overdiagnosed_2', 'overdiagnosed_3', 'overdiagnosed_4',
                          'total_FPs', 'total_misdiagnosed')
row.names(res_number_outcomes) <- names_res_number_row

# ---------- Aggregate Health Outcomes -----------
# Pre-diagnosis LYs
res_soc_LYs_pre_diag <- sum(res_soc_pre_diag$disc_LYs)/res_soc_total_diagnosed
res_MCED_LYs_pre_diag <- sum(res_MCED_pre_diag$disc_LYs)/res_MCED_total_diagnosed

# Post-diagnosis LYs
res_soc_LYs_post_diag <- sum(rowSums(res_soc_LYs, na.rm = T))/res_soc_total_diagnosed
res_MCED_LYs_post_diag <- sum(rowSums(res_MCED_LYs, na.rm = T))/res_MCED_total_diagnosed
res_MCED_LYs_post_diag_TS <- sum(rowSums(res_MCED_LYs_TS, na.rm = T))/res_MCED_total_diagnosed_TS

# Pre-diagnosis QALYs
res_soc_QALYs_pre_diag <- sum(res_soc_pre_diag$disc_QALYs)/res_soc_total_diagnosed
res_MCED_QALYs_pre_diag <- sum(res_MCED_pre_diag$disc_QALYs)/res_MCED_total_diagnosed

# Post-diagnosis QALYs
res_soc_QALYs_post_diag <- sum(rowSums(res_soc_QALYs, na.rm = T))/res_soc_total_diagnosed
res_MCED_QALYs_post_diag <- sum(rowSums(res_MCED_QALYs, na.rm = T))/res_MCED_total_diagnosed
res_MCED_QALYs_post_diag_TS <- sum(rowSums(res_MCED_QALYs_TS, na.rm = T))/res_MCED_total_diagnosed_TS

# Total diagnosis LYs and QALYs
res_soc_LYs_total_diag <- res_soc_LYs_pre_diag + res_soc_LYs_post_diag
res_MCED_LYs_total_diag <- res_MCED_LYs_pre_diag + res_MCED_LYs_post_diag
res_soc_QALYs_total_diag <- res_soc_QALYs_pre_diag + res_soc_QALYs_post_diag
res_MCED_QALYs_total_diag <- res_MCED_QALYs_pre_diag + res_MCED_QALYs_post_diag

# Total no-cancer diagnosis LYs and QALYs
res_soc_LYs_no_diag <- sum(res_soc_no_diag[,'disc_LYs'])
res_MCED_LYs_no_diag <- sum(res_MCED_no_diag[,'disc_LYs'])
res_soc_QALYs_no_diag <- sum(res_soc_no_diag[,'disc_QALYs'])
res_MCED_QALYs_no_diag <- sum(res_MCED_no_diag[,'disc_QALYs'])

# Overdiagnosis results 
res_MCED_disutil_overdiag <- sum(res_overdiag_QALYs)/cohort_size
res_MCED_costs_overdiag <- sum(res_overdiag_costs)/cohort_size
res_MCED_screen_costs_overdiag <- sum(res_overdiag_screen_costs)/cohort_size

# Record total LYs and QALYs 
res_soc_total_LYs <- (res_soc_LYs_no_diag*(cohort_size - res_soc_total_diagnosed) +
                        res_soc_LYs_total_diag*res_soc_total_diagnosed)/cohort_size
res_MCED_total_LYs <- (res_MCED_LYs_no_diag*(cohort_size - res_MCED_total_diagnosed) +
                         res_MCED_LYs_total_diag*res_MCED_total_diagnosed)/cohort_size

res_soc_total_QALYs <- (res_soc_QALYs_no_diag*(cohort_size-res_soc_total_diagnosed) + 
                          res_soc_QALYs_total_diag*res_soc_total_diagnosed)/cohort_size - res_soc_disutil_FPs
res_MCED_total_QALYs <- (res_MCED_QALYs_no_diag*(cohort_size - res_MCED_total_diagnosed) +
                           res_MCED_QALYs_total_diag*res_MCED_total_diagnosed)/cohort_size - res_MCED_disutil_FPs - res_MCED_disutil_misdiagnosis - res_MCED_disutil_overdiag
res_health_outcomes <- 
  rbind(
    c(res_MCED_total_LYs, res_soc_total_LYs), # Total LYs
    c(res_MCED_LYs_no_diag, res_soc_LYs_no_diag), # Total No Cancer diagnosis LYs
    c(res_MCED_LYs_total_diag, res_soc_LYs_total_diag), # Total Cancer Diagnosis LYs
    c(res_MCED_LYs_pre_diag, res_soc_LYs_pre_diag), # Pre-diagnosis LYs
    c(res_MCED_LYs_post_diag, res_soc_LYs_post_diag), # Post-diagnosis LYs
    
    c(res_MCED_total_QALYs, res_soc_total_QALYs), # Total QALYs
    c(res_MCED_QALYs_no_diag, res_soc_QALYs_no_diag), # Total No Cancer diagnosis QALYs
    c(res_MCED_QALYs_total_diag, res_soc_QALYs_total_diag), # Total Cancer Diagnosis QALYs
    c(res_MCED_QALYs_pre_diag, res_soc_QALYs_pre_diag), # Pre-diagnosis QALYs
    c(res_MCED_QALYs_post_diag, res_soc_QALYs_post_diag), # Post-diagnosis QALYs
    
    c(-res_MCED_disutil_FPs, -res_soc_disutil_FPs),
    c(-res_MCED_disutil_misdiagnosis, 0),
    c(-res_MCED_disutil_overdiag, 0)
  )
names_res_health_row <- c('LYs_total','LYs_total_no_diag','LYs_total_diag','LYs_pre_diag','LYs_post_diag',
                          'QALYs_total','QALYs_total_no_diag','QALYs_total_diag','QALYs_pre_diag','QALYs_post_diag',
                          'disutility_FP', 'disutility_misdiag', 'disutility_OD')
row.names(res_health_outcomes) <- names_res_health_row

# ---------- Aggregate Deaths outcomes -----------
res_soc_deaths_1 <- sum(rowSums(res_soc_Deaths[,c(seq(1,n_cancer*4,by=4))]))
res_soc_deaths_2 <- sum(rowSums(res_soc_Deaths[,c(seq(2,n_cancer*4,by=4))])) 
res_soc_deaths_3 <- sum(rowSums(res_soc_Deaths[,c(seq(3,n_cancer*4,by=4))]))
res_soc_deaths_4 <- sum(rowSums(res_soc_Deaths[,c(seq(4,n_cancer*4,by=4))]))
res_MCED_deaths_1 <- sum(rowSums(res_MCED_Deaths[,c(seq(1,n_cancer*4,by=4))]))
res_MCED_deaths_2 <- sum(rowSums(res_MCED_Deaths[,c(seq(2,n_cancer*4,by=4))]))
res_MCED_deaths_3 <- sum(rowSums(res_MCED_Deaths[,c(seq(3,n_cancer*4,by=4))]))
res_MCED_deaths_4 <- sum(rowSums(res_MCED_Deaths[,c(seq(4,n_cancer*4,by=4))]))

res_soc_deaths_total <- res_soc_deaths_1 + res_soc_deaths_2 + res_soc_deaths_3 + res_soc_deaths_4
res_MCED_deaths_total <- res_MCED_deaths_1 + res_MCED_deaths_2 + res_MCED_deaths_3 + res_MCED_deaths_4

res_deaths_outcomes <-
  rbind(
    c(res_MCED_deaths_total, res_soc_deaths_total), # Total deaths
    c(res_MCED_deaths_1, res_soc_deaths_1), # Deaths Stage I
    c(res_MCED_deaths_2, res_soc_deaths_2), # Deaths Stage II
    c(res_MCED_deaths_3, res_soc_deaths_3), # Deaths Stage III
    c(res_MCED_deaths_4, res_soc_deaths_4) # Deaths Stage IV
  )

names_res_deaths_row <- c('deaths_total', 'deaths_1', 'deaths_2', 'deaths_3', 'deaths_4')
row.names(res_deaths_outcomes) <- names_res_deaths_row

# ---------- Aggregate Cost Outcomes -----------
# Screening costs
res_soc_scren_cost_pre_diag <- sum(res_soc_soc_screen[,'soc_screening_costs']*cost_growth_rates_screening*discount_cost)/cohort_size
res_soc_scren_cost_post_diag <- 0
res_soc_screen_cost_total <- res_soc_scren_cost_pre_diag + res_soc_scren_cost_post_diag

res_MCED_scren_cost_pre_diag <- sum((res_MCED_soc_screen[,'soc_screening_costs']+res_MCED_MCED_screen['MCED_screening_costs'])*cost_growth_rates_screening*discount_cost)/cohort_size
res_MCED_scren_cost_pre_diag <- res_MCED_scren_cost_pre_diag - res_MCED_screen_costs_overdiag
res_MCED_scren_cost_post_diag <- 0 
res_MCED_screen_cost_total <- res_MCED_scren_cost_pre_diag + res_MCED_scren_cost_post_diag

# Cancer treatment costs per individual
soc_cancer_tx_cost_total <- sum(rowSums(res_soc_Costs[,c(seq(1,n_cancer*4,by=1))]))
soc_cancer_tx_cost_1 <- sum(rowSums(res_soc_Costs[,c(seq(1,n_cancer*4,by=4))]))
soc_cancer_tx_cost_2 <- sum(rowSums(res_soc_Costs[,c(seq(2,n_cancer*4,by=4))]))
soc_cancer_tx_cost_3 <- sum(rowSums(res_soc_Costs[,c(seq(3,n_cancer*4,by=4))]))
soc_cancer_tx_cost_4 <- sum(rowSums(res_soc_Costs[,c(seq(4,n_cancer*4,by=4))]))

# without OD
MCED_cancer_tx_cost_total <- sum(rowSums(res_MCED_Costs[,c(seq(1,n_cancer*4,by=1))]))
MCED_cancer_tx_cost_1 <- sum(rowSums(res_MCED_Costs[,c(seq(1,n_cancer*4,by=4))]))
MCED_cancer_tx_cost_2 <- sum(rowSums(res_MCED_Costs[,c(seq(2,n_cancer*4,by=4))]))
MCED_cancer_tx_cost_3 <- sum(rowSums(res_MCED_Costs[,c(seq(3,n_cancer*4,by=4))]))
MCED_cancer_tx_cost_4 <- sum(rowSums(res_MCED_Costs[,c(seq(4,n_cancer*4,by=4))]))

# OD
MCED_cancer_tx_cost_total_OD <- sum(rowSums(res_overdiag_costs[,c(seq(1,n_cancer*4,by=1))]))
MCED_cancer_tx_cost_1_OD <- sum(rowSums(res_overdiag_costs[,c(seq(1,n_cancer*4,by=4))]))
MCED_cancer_tx_cost_2_OD <- sum(rowSums(res_overdiag_costs[,c(seq(2,n_cancer*4,by=4))]))
MCED_cancer_tx_cost_3_OD <- sum(rowSums(res_overdiag_costs[,c(seq(3,n_cancer*4,by=4))]))
MCED_cancer_tx_cost_4_OD <- sum(rowSums(res_overdiag_costs[,c(seq(4,n_cancer*4,by=4))]))

# Shifted individual (without OD)
MCED_cancer_tx_cost_total_TS <- sum(rowSums(res_MCED_Costs_TS[,c(seq(1,n_cancer*4,by=1))]))
MCED_cancer_tx_cost_1_TS <- sum(rowSums(res_MCED_Costs_TS[,c(seq(1,n_cancer*4,by=4))]))
MCED_cancer_tx_cost_2_TS <- sum(rowSums(res_MCED_Costs_TS[,c(seq(2,n_cancer*4,by=4))]))
MCED_cancer_tx_cost_3_TS <- sum(rowSums(res_MCED_Costs_TS[,c(seq(3,n_cancer*4,by=4))]))
MCED_cancer_tx_cost_4_TS <- sum(rowSums(res_MCED_Costs_TS[,c(seq(4,n_cancer*4,by=4))]))

# SoC total
res_soc_cancer_tx_cost_total <- soc_cancer_tx_cost_total/cohort_size
res_soc_cancer_tx_cost_1 <- soc_cancer_tx_cost_1/cohort_size
res_soc_cancer_tx_cost_2 <- soc_cancer_tx_cost_2/cohort_size
res_soc_cancer_tx_cost_3 <- soc_cancer_tx_cost_3/cohort_size
res_soc_cancer_tx_cost_4 <- soc_cancer_tx_cost_4/cohort_size

# MCED total
res_MCED_cancer_tx_cost_total <-(MCED_cancer_tx_cost_total+MCED_cancer_tx_cost_total_OD)/cohort_size
res_MCED_cancer_tx_cost_1 <- (MCED_cancer_tx_cost_1+MCED_cancer_tx_cost_1_OD)/cohort_size
res_MCED_cancer_tx_cost_2 <- (MCED_cancer_tx_cost_2+MCED_cancer_tx_cost_2_OD)/cohort_size
res_MCED_cancer_tx_cost_3 <- (MCED_cancer_tx_cost_3+MCED_cancer_tx_cost_3_OD)/cohort_size
res_MCED_cancer_tx_cost_4 <- (MCED_cancer_tx_cost_4+MCED_cancer_tx_cost_4_OD)/cohort_size

# Cancer treatment costs per diagnosed patients (without OD)
res_soc_diag_cancer_tx_cost_total <- soc_cancer_tx_cost_total/res_soc_total_diagnosed
res_soc_diag_cancer_tx_cost_1 <- soc_cancer_tx_cost_1/res_soc_total_diagnosed
res_soc_diag_cancer_tx_cost_2 <- soc_cancer_tx_cost_2/res_soc_total_diagnosed
res_soc_diag_cancer_tx_cost_3 <- soc_cancer_tx_cost_3/res_soc_total_diagnosed
res_soc_diag_cancer_tx_cost_4 <- soc_cancer_tx_cost_4/res_soc_total_diagnosed

res_MCED_diag_cancer_tx_cost_total <-MCED_cancer_tx_cost_total/res_MCED_total_diagnosed
res_MCED_diag_cancer_tx_cost_1 <- MCED_cancer_tx_cost_1/res_MCED_total_diagnosed
res_MCED_diag_cancer_tx_cost_2 <- MCED_cancer_tx_cost_2/res_MCED_total_diagnosed
res_MCED_diag_cancer_tx_cost_3 <- MCED_cancer_tx_cost_3/res_MCED_total_diagnosed
res_MCED_diag_cancer_tx_cost_4 <- MCED_cancer_tx_cost_4/res_MCED_total_diagnosed

# Cancer treatment costs per shifted individuals (without OD)
res_MCED_cancer_tx_cost_total_TS <-MCED_cancer_tx_cost_total_TS/res_MCED_total_diagnosed_TS
res_MCED_cancer_tx_cost_1_TS <- MCED_cancer_tx_cost_1_TS/res_MCED_total_diagnosed_TS
res_MCED_cancer_tx_cost_2_TS <- MCED_cancer_tx_cost_2_TS/res_MCED_total_diagnosed_TS
res_MCED_cancer_tx_cost_3_TS <- MCED_cancer_tx_cost_3_TS/res_MCED_total_diagnosed_TS
res_MCED_cancer_tx_cost_4_TS <- MCED_cancer_tx_cost_4_TS/res_MCED_total_diagnosed_TS

# Record additional workup costs
# Calculate misdiagnosed pts by cancer for application on misdiagnosis cost
res_soc_cost_misdiagnosis <- 0
res_MCED_cost_misdiagnosis <- sum(rowSums(t(t(misdiagnosis)*costs_workup_misdiagnosis[,1])*cost_growth_rates*discount_cost))/cohort_size

# Calculate weighted average cost for FPs based on cancer incidence
i<-1
total_diagnosis_by_cancer <- sum(colSums(soc_markov_trace[,c(seq(3,n_cancer*4,by=4)[i]:seq(6,n_cancer*4+2,by=4)[i])]))
for(i in 2:n_cancer){
  total_diagnosis_by_cancer <- c(total_diagnosis_by_cancer,sum(colSums(soc_markov_trace[,c(seq(3,n_cancer*4,by=4)[i]:seq(6,n_cancer*4+2,by=4)[i])])))
}
cost_workup_FP <- sum(total_diagnosis_by_cancer*costs_workup_FP)/sum(total_diagnosis_by_cancer)

res_soc_cost_FPs <- sum(res_soc_soc_screen[,'soc_FPs']*cost_workup_FP*cost_growth_rates*discount_cost)/cohort_size
res_MCED_cost_FPs <- sum((res_MCED_soc_screen[,'soc_FPs']+res_MCED_MCED_screen[,'MCED_FPs'])*cost_workup_FP*cost_growth_rates*discount_cost)/cohort_size

res_soc_total_cost_workup <- res_soc_cost_misdiagnosis + res_soc_cost_FPs
res_MCED_total_cost_workup <- res_MCED_cost_misdiagnosis + res_MCED_cost_FPs

# Record societal costs
res_soc_societal_cost <- sum(rowSums(res_soc_societal_costs[,c(seq(1,n_cancer*4,by=1))]))
res_MCED_societal_cost <- sum(rowSums(res_MCED_societal_costs[,c(seq(1,n_cancer*4,by=1))]))

# Record Total Costs
res_soc_costs_total <- res_soc_screen_cost_total + res_soc_cancer_tx_cost_total + res_soc_total_cost_workup + res_soc_societal_cost
res_MCED_costs_total <- res_MCED_screen_cost_total + res_MCED_cancer_tx_cost_total + res_MCED_total_cost_workup + res_MCED_societal_cost

res_cost_outcomes <-
  rbind(
    c(res_MCED_costs_total, res_soc_costs_total), # Total costs
    
    c(res_MCED_screen_cost_total, res_soc_screen_cost_total), # Total screening costs
    c(res_MCED_scren_cost_pre_diag, res_soc_scren_cost_pre_diag), # Pre-diagnosis Screening Costs
    c(res_MCED_scren_cost_post_diag, res_soc_scren_cost_post_diag), # Post-diagnosis Screening Costs
    
    c(res_MCED_cancer_tx_cost_total, res_soc_cancer_tx_cost_total), # Total Cancer tx costs
    c(res_MCED_cancer_tx_cost_1, res_soc_cancer_tx_cost_1), # Cancer tx costs stage I
    c(res_MCED_cancer_tx_cost_2, res_soc_cancer_tx_cost_2), # Cancer tx costs stage II
    c(res_MCED_cancer_tx_cost_3, res_soc_cancer_tx_cost_3), # Cancer tx costs stage III
    c(res_MCED_cancer_tx_cost_4, res_soc_cancer_tx_cost_4), # Cancer tx costs stage IV
    
    c(res_MCED_total_cost_workup, res_soc_total_cost_workup), # Total workup costs
    c(res_MCED_cost_misdiagnosis, res_soc_cost_misdiagnosis), # misdiagnosis costs
    c(res_MCED_cost_FPs, res_soc_cost_FPs), # FPs costs
    
    c(res_MCED_societal_cost, res_soc_societal_cost), # Societal costs
    
    c(res_MCED_diag_cancer_tx_cost_total,res_soc_diag_cancer_tx_cost_total), # Total Cancer tx costs - diagnosed
    c(res_MCED_diag_cancer_tx_cost_1, res_soc_diag_cancer_tx_cost_1), # Cancer tx costs stage I - diagnosed
    c(res_MCED_diag_cancer_tx_cost_2, res_soc_diag_cancer_tx_cost_2), # Cancer tx costs stage II - diagnosed
    c(res_MCED_diag_cancer_tx_cost_3, res_soc_diag_cancer_tx_cost_3), # Cancer tx costs stage III - diagnosed
    c(res_MCED_diag_cancer_tx_cost_4, res_soc_diag_cancer_tx_cost_4) # Cancer tx costs stage IV - diagnosed
)
names_res_costs_row <- c('costs_total', 'costs_total_screen','costs_screen_pre_diag','costs_screen_post_diag',
                         'costs_total_cancer_tx', 'costs_cancer_tx_1', 'costs_cancer_tx_2', 'costs_cancer_tx_3', 'costs_cancer_tx_4',
                         'costs_total_workup', 'costs_misdiagnosis', 'costs_FPs', 'costs_societal',
                         'costs_diag_cancer_tx_total','costs_diag_cancer_tx_1','costs_diag_cancer_tx_2','costs_diag_cancer_tx_3','costs_diag_cancer_tx_4')
row.names(res_cost_outcomes) <- names_res_costs_row

# ---------- Aggregate Outcomes for Shifted Individuals -----------
# Total number outcomes
res_number_outcomes_TS <-
  rbind(
    c(res_MCED_total_diagnosed_TS, 0), #Total number diagnosed
    c(res_MCED_diagnosed_1_TS, 0), #Total diagnosed stage I
    c(res_MCED_diagnosed_2_TS, 0), #Total diagnosed stage II
    c(res_MCED_diagnosed_3_TS, 0), #Total diagnosed stage III
    c(res_MCED_diagnosed_4_TS, 0) #Total diagnosed stage IV
)
names_res_number_row_TS <- c('diagnosed_total_TS', 'diagnosed_1_TS', 'diagnosed_2_TS', 'diagnosed_3_TS', 'diagnosed_4_TS')
row.names(res_number_outcomes_TS) <- names_res_number_row_TS

res_health_outcomes_TS <- 
  rbind(
    c(res_MCED_LYs_post_diag_TS, 0), # Post-diagnosis LYs
    c(res_MCED_QALYs_post_diag_TS, 0) # Post-diagnosis QALYs
)
names_res_health_row_TS <- c('LYs_post_diag_TS', 'QALYs_post_diag_TS')
row.names(res_health_outcomes_TS) <- names_res_health_row_TS

res_cost_outcomes_TS <-
  rbind(
    c(res_MCED_cancer_tx_cost_total_TS,0), # Total Cancer tx costs - shifted individuals
    c(res_MCED_cancer_tx_cost_1_TS, 0), # Cancer tx costs stage I - shifted individuals
    c(res_MCED_cancer_tx_cost_2_TS, 0), # Cancer tx costs stage II - shifted individuals
    c(res_MCED_cancer_tx_cost_3_TS, 0), # Cancer tx costs stage III - shifted individuals
    c(res_MCED_cancer_tx_cost_4_TS, 0) # Cancer tx costs stage IV - shifted individuals
)
names_res_costs_row_TS <- c('costs_cancer_tx_total_TS','costs_cancer_tx_1_TS','costs_cancer_tx_2_TS','costs_cancer_tx_3_TS','costs_cancer_tx_4_TS')
row.names(res_cost_outcomes_TS) <- names_res_costs_row_TS

#---- Updated 8/14/2023: Export total costs over time to find cost-neutral over X years
# SoC total costs = Soc screening cost + SoC cancer treatment cost + SoC FP workup costs
res_soc_costs_total_over_time <- (res_soc_soc_screen[,'soc_screening_costs']*cost_growth_rates_screening*discount_cost + 
                                    rowSums(res_soc_Costs[,c(seq(1,n_cancer*4,by=1))]) + 
                                    res_soc_soc_screen[,'soc_FPs']*cost_workup_FP*cost_growth_rates*discount_cost)/cohort_size
# MCED total costs = MCED+SoC screening cost - 
#                    Overdiagnosis screening cost + 
#                    MCED cancer treatment cost + 
#                    Overdiagnosis cancer treatment cost + 
#                    MCED misdiagnosis workup costs + 
#                    MCED FP workup costs
res_MCED_costs_total_over_time <- ((res_MCED_soc_screen[,'soc_screening_costs']+res_MCED_MCED_screen['MCED_screening_costs'])*cost_growth_rates_screening*discount_cost - res_overdiag_screen_costs + 
                                     rowSums(res_MCED_Costs[,c(seq(1,n_cancer*4,by=1))]) + rowSums(res_overdiag_costs[,c(seq(1,n_cancer*4,by=1))]) + 
                                     rowSums(t(t(misdiagnosis)*costs_workup_misdiagnosis[,1])*cost_growth_rates*discount_cost) + 
                                     (res_MCED_soc_screen[,'soc_FPs']+res_MCED_MCED_screen[,'MCED_FPs'])*cost_workup_FP*cost_growth_rates*discount_cost)/cohort_size

res_cost_outcomes_over_time <-
  rbind(
    c(sum(res_MCED_costs_total_over_time[1:5,1]),sum(res_soc_costs_total_over_time[1:5])), # Total costs over 5 years
    c(sum(res_MCED_costs_total_over_time[1:10,1]),sum(res_soc_costs_total_over_time[1:10])), # Total costs over 10 years
    c(sum(res_MCED_costs_total_over_time[1:15,1]),sum(res_soc_costs_total_over_time[1:15])), # Total costs over 15 years
    c(sum(res_MCED_costs_total_over_time[1:20,1]),sum(res_soc_costs_total_over_time[1:20])), # Total costs over 20 years
    c(sum(res_MCED_costs_total_over_time[1:25,1]),sum(res_soc_costs_total_over_time[1:25])), # Total costs over 25 years
    c(sum(res_MCED_costs_total_over_time[1:30,1]),sum(res_soc_costs_total_over_time[1:30])) # Total costs over 30 years
  )
names_res_costs_over_time_row <- c('total_costs_5_yr','total_costs_10_yr','total_costs_15_yr','total_costs_20_yr','total_costs_25_yr','total_costs_30_yr')
row.names(res_cost_outcomes_over_time) <- names_res_costs_over_time_row

#----------- Export results -----------
res_export <- rbind(res_health_outcomes, res_number_outcomes, res_deaths_outcomes, res_cost_outcomes, res_number_outcomes_TS, res_health_outcomes_TS, res_cost_outcomes_TS, res_cost_outcomes_over_time)
colnames(res_export) <- c('MCED+SoC','SoC')


