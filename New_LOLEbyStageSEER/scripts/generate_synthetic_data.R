##########################################################################################
##========================================================================================
## Lung cancer - Normal tissue complication probability models 
##========================================================================================
##########################################################################################

library(tidyverse)
library(truncnorm)
library(EnvStats)
library(copula)

#______________________________________________________________________________#
#' For each categorical variable which needs transforming into continuous, first
#' find the range and mid point of that range in terms of the proportion the 
#' variable appears

v_characteristics <- c("SEER_Draw", "AJCC_stage", "Age_lower", "Race" ,"Status")

f_copula_gen_proportions <- function(varname){
        
        var <- ensym(varname)

        res <- TTE_data %>%
                filter(SEER_Draw %in% res_cancers_evaluated) %>%
                filter(AJCC_stage %in% c("I", "II", "III", "IV")) %>%
                dplyr::select(SEER_Draw, AJCC_stage, Age_lower, Race, Status, Num_intervals) %>%
                group_by({{var}}) %>%
                summarise(total = n()) %>%
                mutate(freq = round(total / sum(total) * 100, 2),
                       mid_point_freq = freq / 2) %>%
                mutate(across(where(is.factor), as.character),
                       across(contains(varname), as.character),
                       upperlimit = cumsum(freq),
                       mid_point_cum = upperlimit - mid_point_freq,
                       lowerlimit = lag(upperlimit, default = 0)) %>%
                rename("upperlimit_{{var}}" := upperlimit,
                       "midpoint_{{var}}" := mid_point_cum,
                       "lowerlimit_{{var}}" := lowerlimit ) %>%
                select(!c(total:mid_point_freq))
        
        return(res)
        
}

res_freq_bygroup <- map(v_characteristics, f_copula_gen_proportions) 

#______________________________________________________________________________#

df_TTE_data_cont_transfrom <- TTE_data %>%
        filter(SEER_Draw %in% res_cancers_evaluated) %>%
        filter(AJCC_stage %in% c("I", "II", "III", "IV")) %>%
        mutate(across(contains("Age_lower"), as.character)) %>%
        left_join(res_freq_bygroup$SEER_Draw, by = c("SEER_Draw")) %>%
        left_join(res_freq_bygroup$AJCC_stage, by = c("AJCC_stage")) %>%
        left_join(res_freq_bygroup$Age_lower, by = c("Age_lower")) %>%
        left_join(res_freq_bygroup$Race, by = c("Race")) %>%
        left_join(res_freq_bygroup$Status, by = c("Status")) %>%
        mutate(cont_SEER_Draw = rtruncnorm(1, a=lowerlimit_SEER_Draw, b=upperlimit_SEER_Draw, mean = midpoint_SEER_Draw, sd = (upperlimit_SEER_Draw - midpoint_SEER_Draw))) %>%
        mutate(cont_AJCC_stage = rtruncnorm(1, a=lowerlimit_AJCC_stage, b=upperlimit_AJCC_stage, mean = midpoint_AJCC_stage, sd = (upperlimit_AJCC_stage - midpoint_AJCC_stage))) %>%
        mutate(cont_Age_lower = rtruncnorm(1, a=lowerlimit_Age_lower, b=upperlimit_Age_lower, mean = midpoint_Age_lower, sd = (upperlimit_Age_lower - midpoint_Age_lower))) %>%
        mutate(cont_Race = rtruncnorm(1, a=lowerlimit_Race, b=upperlimit_Race, mean = midpoint_Race, sd = (upperlimit_Race - midpoint_Race))) %>%
        mutate(cont_Status = rtruncnorm(1, a=lowerlimit_Status, b=upperlimit_Status, mean = midpoint_Status, sd = (upperlimit_Status - midpoint_Status))) 

ggplot(df_TTE_data_cont_transfrom) +
        geom_density(aes(cont_AJCC_stage, colour = AJCC_stage))

#______________________________________________________________________________#

cop_data <- as.matrix(df_TTE_data_cont_transfrom %>%
        select(cont_SEER_Draw:cont_Status))

mycop <- ellipCopula("normal", dim=5)
u <- pobs(cop_data)

fit.tau <- fitCopula(mycop, u, method="itau")


SimData <- as.data.frame(rMvdc(mvdc = mvdcS4Output, n = 1000))



#______________________________________________________________________________#
#' 
#' Re-transform categorical data
SimData <- as.data.frame(rMvdc(mvdc = mvdcS4Output, n = 1000))


##========================================================================================
## Copula method
##========================================================================================

## Function for estimating Beta shape and scale paramaters
estBetaParams <- function(mu, sd) {
        alpha <- ((1 - mu) / (sd^2) - 1 / mu) * mu ^ 2
        beta <- alpha * (1 / mu - 1)
        return(params = list(alpha = alpha, beta = beta))
}

BetaParams <- as.data.frame(estBetaParams(MeanVec[4:6],SDVec[4:6]))

## For Histogram of parametric margins
ParametricMarginalsDF <- data.frame(rnorm(1000, MeanSdDF[1,1] , MeanSdDF[1,2]),
                                    rnorm(1000, MeanSdDF[2,1] , MeanSdDF[2,2]),
                                    rnorm(1000, MeanSdDF[3,1] , MeanSdDF[3,2]),
                                    rbeta(1000, BetaParams[1,1], BetaParams[1,2]), 
                                    rbeta(1000, BetaParams[2,1], BetaParams[2,2]),
                                    rbeta(1000, BetaParams[3,1], BetaParams[3,2]))

colnames(ParametricMarginalsDF) <- dimnames(DoseVarDF)[[2]]

## Generating Copula!
SimCopfun <- function( Data, Margins = c("norm","norm","norm","beta","beta","beta"), 
                       Copula = "Gaussian", Dependence1 = "kendall", Dependence2 = NULL , DegrFreed = NULL ){
        ## Data - The margnial data. 
        ## Margins - the marginal distribution of each varaible as string character vector.
        ## Copula - Type of copula, only Gausian and Student in function.
        ## Dependence - Method of estimating dependence structure of data.
        ## DegrFreed - Degrees of freedom for Student's t copula. 
        ## Dependence2 - Can specify own dependence structure as a positive-semidefinite matrix
        
        ## Checks
        if (Copula != "Gaussian" & Copula != "Student"){
                stop("Please specify either a Gaussian or Student copula")
        }
        if (Dependence1 != "kendall" & Dependence1 != "spearman" & Dependence1 != "pearson" ){
                stop("Please specify either a kendall, spearman or pearson dependence")
        }
        if (Copula == "Student" & is.null(DegrFreed)){
                stop("Please specify the degrees of freedom")
        }
        
        ## Data - N.B. Specific for data provided!
        
        ## Dependence structure
        if (is.null(Dependence2)){
                corr <- cor(Data, method = Dependence1)
        } else if(is.positive.definite(Dependence2) == TRUE){ ## Check
                corr <- Dependence2
        } else {
                stop("Dependence structure must be a positive semideifnite matrix")
        }
        
        ## Parametric margins
        
        ## Simulating data using different copulas
        if(Copula == "Gaussian"){
                ## Copula
                myCop <- normalCopula(param=lower(corr), dim = length(Margins), dispstr = "un")
                
        } else if(Copula == "Student") {
                ## Copula
                myCop <- tCopula(param=lower(corr), dim = length(Margins), df = DegrFreed)
                
        } 
        
        myMvd <- mvdc(copula=myCop, margins=Margins,
                      paramMargins=list(list(mean=MeanVec[1], sd=SDVec[1]),
                                        list(mean=MeanVec[2], sd=SDVec[2]),
                                        list(mean=MeanVec[3], sd=SDVec[3]),
                                        list(shape1=BetaparamMargins[1,1],shape2=BetaparamMargins[1,2]),
                                        list(shape1=BetaparamMargins[2,1],shape2=BetaparamMargins[2,2]),
                                        list(shape1=BetaparamMargins[3,1],shape2=BetaparamMargins[3,2])))
}

mvdcS4Output <- SimCopfun(Data = DoseVarDF)

##========================================================================================
## Visuals
##========================================================================================

SimData <- as.data.frame(rMvdc(mvdc = mvdcS4Output, n = 1000))
colnames(SimData) <- dimnames(DoseVarDF)[[2]]
