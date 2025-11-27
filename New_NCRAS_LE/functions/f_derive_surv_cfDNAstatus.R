#' Derive differential survival by cfDNA status
#'
#' Calculates differential survival based on hazard ratio between cfDNA statuses
#'
#' @param survival Survival probability
#' @param hazard_ratio Hazard ratio between cfDNA negative and positive status
#' @param cfDNA_detection_rate Proportion of cfDNA postivie who are detected

#'
#' @return Estimate of cfDNA negative survival
#' @examples
#' f_derive_surv_cfDNAstatus()
#' @export
f_derive_surv_cfDNAstatus <- function(survival,
                                         hazard_ratio,
                                         cfDNA_detection_rate){

        internal_function <- function(target_surv){

                (target_surv^hazard_ratio * cfDNA_detection_rate + target_surv * (1 - cfDNA_detection_rate) - survival)^2
        }

        output = optimize(internal_function, interval = c(0, 1))
        output$minimum
}

