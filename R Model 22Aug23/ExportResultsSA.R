# ---------- Summarize results -----------
# ---------- Aggregate Health Outcomes -----------
# Pre-diagnosis LYs
res_soc_LYs_pre_diag <- sum(res_soc_pre_diag$disc_LYs)/res_soc_total_diagnosed
res_MCED_LYs_pre_diag <- sum(res_MCED_pre_diag$disc_LYs)/res_MCED_total_diagnosed

# Post-diagnosis LYs
res_soc_LYs_post_diag <- sum(rowSums(res_soc_LYs, na.rm = T))/res_soc_total_diagnosed
res_MCED_LYs_post_diag <- sum(rowSums(res_MCED_LYs, na.rm = T))/res_MCED_total_diagnosed

# Pre-diagnosis QALYs
res_soc_QALYs_pre_diag <- sum(res_soc_pre_diag$disc_QALYs)/res_soc_total_diagnosed
res_MCED_QALYs_pre_diag <- sum(res_MCED_pre_diag$disc_QALYs)/res_MCED_total_diagnosed

# Post-diagnosis QALYs
res_soc_QALYs_post_diag <- sum(rowSums(res_soc_QALYs, na.rm = T))/res_soc_total_diagnosed
res_MCED_QALYs_post_diag <- sum(rowSums(res_MCED_QALYs, na.rm = T))/res_MCED_total_diagnosed

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

MCED_cancer_tx_cost_1 <- sum(rowSums(res_MCED_Costs[,c(seq(1,n_cancer*4,by=4))])) + sum(rowSums(res_overdiag_costs[,c(seq(1,n_cancer_base*4,by=4))]))
MCED_cancer_tx_cost_2 <- sum(rowSums(res_MCED_Costs[,c(seq(2,n_cancer*4,by=4))])) + sum(rowSums(res_overdiag_costs[,c(seq(2,n_cancer_base*4,by=4))]))
MCED_cancer_tx_cost_3 <- sum(rowSums(res_MCED_Costs[,c(seq(3,n_cancer*4,by=4))])) + sum(rowSums(res_overdiag_costs[,c(seq(3,n_cancer_base*4,by=4))]))
MCED_cancer_tx_cost_4 <- sum(rowSums(res_MCED_Costs[,c(seq(4,n_cancer*4,by=4))])) + sum(rowSums(res_overdiag_costs[,c(seq(4,n_cancer_base*4,by=4))]))
MCED_cancer_tx_cost_total <- MCED_cancer_tx_cost_1+MCED_cancer_tx_cost_2+MCED_cancer_tx_cost_3+MCED_cancer_tx_cost_4

res_soc_cancer_tx_cost_total <- soc_cancer_tx_cost_total/cohort_size

res_MCED_cancer_tx_cost_total <-MCED_cancer_tx_cost_total/cohort_size

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


#----------- Export results -----------
res_export <- c(res_MCED_total_LYs, res_soc_total_LYs, res_MCED_total_QALYs, res_soc_total_QALYs, res_MCED_costs_total, res_soc_costs_total)
