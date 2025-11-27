#______________________________________________________________________________#
#____ 02a_M_spline

#' Uses a Bayesian flexible parametric survival model, based on an M-spline
#' to extrapolate the SEER individual patient data 

#______________________________________________________________________________#
#' Create function that runs the M-spline model and outputs RMST 100 estimate
#' and plots of survival and hazard

f_life_expectancy <- function(survival,
                              include_additive_hzds,
                              include_cure_fraction,
                              include_Loo,
                              plotsurvhz,
                              survextrap_add_knotts_interval,
                              survextrap_fit_method,
                              survextrap_niter)
        {
        
        #__________________________________#
        #' Get variables and options needed for parametric model
        get_age <- as.numeric(survival$Age_lower[1]) + 2.5
        get_sex <- as.character(survival$Sex[1])
        
        if(include_additive_hzds){
                
                get_bckgrnd_hz <- bckgrnd_hz %>%
                        filter(Age_lower >= get_age & Sex == Sex) %>%
                        mutate(time = row_number() - 1) %>%
                        select(time, hazard)
        }
        
        
        if(!include_additive_hzds & include_cure_fraction) {
                
                stop(print("Must include additive hazards aswell as cure fraction"))
        }
        
        if(is.na(survextrap_add_knotts_interval)){
                
                knotts = NULL 
        } else {
                knotts = seq.int(from = 10, to = 100 - get_age, by = survextrap_add_knotts_interval)  
        }
        
        
        #__________________________________#
        #' specify M-spline model based on options
        mspline <- survextrap::mspline_spec(Surv(time, event) ~ 1, data = survival, add_knots = knotts)
        
        if(include_additive_hzds){
                
                if(include_cure_fraction){
                        
                        if(include_Loo){
                                
                                model <- survextrap::survextrap(Surv(time, event) ~ 1, 
                                                                data=survival, mspline=mspline, backhaz = get_bckgrnd_hz,
                                                                cure = TRUE,
                                                                fit_method = survextrap_fit_method)
                                print("Model with backhaz and cure probability and LOO")
                                
                        } else {
                                
                                model <- survextrap::survextrap(Surv(time, event) ~ 1, 
                                                                data=survival, mspline=mspline, backhaz = get_bckgrnd_hz,
                                                                cure = TRUE,
                                                                fit_method = survextrap_fit_method, loo = FALSE)
                                print("Model with backhaz and cure probability")
                                
                        }
                        
                } else if(include_Loo){
                        
                        model <- survextrap::survextrap(Surv(time, event) ~ 1, data=survival,
                                                        mspline=mspline, backhaz = get_bckgrnd_hz,
                                                        fit_method = survextrap_fit_method)
                        print("Model with backhaz and LOO")
                        
                } else {
                        model <- survextrap::survextrap(Surv(time, event) ~ 1, data=survival,
                                                        mspline=mspline, backhaz = get_bckgrnd_hz,
                                                        fit_method = survextrap_fit_method, loo = FALSE)
                        print("Model with backhaz")
                        
                        
                }
                
        } else {
                
                model <- survextrap::survextrap(Surv(time, event) ~ 1, data=survival,
                                                mspline=mspline, fit_method = survextrap_fit_method)
                print("Model on observations only")
        }
        
        
        #__________________________________#
        #' specify M-spline model based on options
        if(include_additive_hzds){
                
                rmst_mod <- rmst(model, t=100 - get_age, niter = survextrap_niter)
        } else {
                
                rmst_mod <- rmst(model, t=12, niter = survextrap_niter)
        }
        
        
        if(plotsurvhz){
                
                plot_surv <- survextrap::plot_survival(model, tmax=100 - get_age, niter = survextrap_niter) 
                plot_hz <- survextrap::plot_hazard(model, show_knots = TRUE, tmax=100 - get_age , niter = survextrap_niter)
                
                return(list(rmst_mod, plot_surv, plot_hz, model))
                
        } else if(include_Loo){
                
                return(list(cbind(rmst_mod, LOOIC = model$loo$estimates["looic","Estimate"])))
                
        } else {
                
                return(list(cbind(rmst_mod, LOOIC = NA)))
        
        }
        
 
}


#______________________________________________________________________________#
#' Function to produce nice tables after batch running code

f_LE_tables <- function(list) {
        
        purrr::map(list, 1) %>%
                discard(is.null) %>%
                list_flatten() %>%
                bind_rows( .id = "characteristics") %>%
                separate_wider_delim(characteristics, delim = ".", names = c("Sex", "SEER_Draw", "AJCC_stage", "Age_lower")) %>%
                mutate(across(`variable`:`97.5%`, unlist)) %>%
                mutate(across(c(median,`2.5%`,`97.5%` ), \(x) round(x, 2) )) %>%
                unite(CrI_temp, c(`2.5%`, `97.5%`), sep = " to ", remove = FALSE) %>%
                mutate(CrI = paste0("(", CrI_temp, ")")) %>%
                unite(median_CrI, c(median, CrI), sep = " ", remove = FALSE) %>%
                group_by(Sex, SEER_Draw, AJCC_stage, Age_lower) %>%
                arrange(as.factor(AJCC_stage), .by_group = TRUE) %>%
                ungroup() %>%
                mutate(Age_string = paste0(Age_lower, " years at diagnosis")) %>%
                mutate(Age_mid = as.numeric(Age_lower) + 2.5) %>%
                left_join(bckgrnd_hz %>% select(Age_lower, Sex, ex),
                          by = c("Age_mid" = "Age_lower", "Sex")) 
        
}

#______________________________________________________________________________#
#' Function to log any error (safely used)


f_LE_errors <- function(list){
        
        purrr::map(list, 2) %>%
                discard(is.null) %>%
                purrr::map(1) %>%
                bind_rows( .id = "characteristics")
        
}
