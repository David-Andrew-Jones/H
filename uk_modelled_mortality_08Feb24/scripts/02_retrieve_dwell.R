
# 1.  Read in data, dwell time for sequential model --------------------------------------------------------
#1. seq model
dwell_all<-read_tsv("data/20210816_aggregate_dwell.tsv")

dwell_all = dwell_all %>%
  filter(Cancer != "CUP" & Cancer != "Brain")


# 2.  Define dwell times for stage skip models  ---------------------------

#2. nonseq no prior history (NPH)
dwell_all_nonseq_nph = dwell_all %>%
  mutate(dwell =case_when(
    (Cancer %in% PRE_SPEC & Stage %in% NO_PRIOR_HISTORY) ~ NPH_DWELL, #no time spent in stage I, II or III
    TRUE~dwell
  ))

#3. nonseq with some prior history (PH)
dwell_all_nonseq_ph = dwell_all %>%
  mutate(dwell =case_when(
    (Cancer %in% PRE_SPEC & Stage %in% PRIOR_HISTORY) ~ NPH_DWELL, #no time spent in stage II or III
    (Cancer %in% PRE_SPEC & Stage %in% E_PRIOR_HISTORY) ~ PH_DWELL, #small amount of time spent in stage I
    TRUE~dwell
  ))


# 3.  Slip rate computation -----------------------------------------------

source("R/slip_rate_from_dwell.R") # get slip rate computation


# 4.  Compute multiple_dwell_slip_rate for all models ---------------------

#screening interval set for which we are computing slip rates
SET_SCREEN_INTERVAL<-1:1

#screening at multiple potential intervals for PPV thresholds
#1. seq model
multiple_dwell_slip_rate<-bind_rows(sapply(SET_SCREEN_INTERVAL,function(z){
  exact_slip_rate_from_dwell(dwell_all,screen_interval=z,weibull_shape=1)
},simplify=FALSE)) %>%
  distinct() #handle the repeated MIS problem by ensuring only one copy

stopifnot(length(unique(multiple_dwell_slip_rate$Cancer))==25)

#2. nonseq no PH
# in this scenario
# there is no ability to intercept certain cancers before metastasis
# as there is no history in which they are detectable and not metastatic
multiple_dwell_slip_rate_X<-bind_rows(sapply(SET_SCREEN_INTERVAL,function(z){
  exact_slip_rate_from_dwell(dwell_all_nonseq_nph,screen_interval=z,weibull_shape=1)
},simplify=FALSE)) %>%
  distinct() #handle the repeated MIS problem by ensuring only one copy

#Set the slip rate for stages 1, 2 and 3 to 1, may not be needed as very short dwell will lead to  ~ 1 for slip rate
#i.e. no ability to intercept cancers in stage 1, 2 and 3 for those affected -  stage 4 only detection
multiple_dwell_slip_rate_nonseq_NPH = multiple_dwell_slip_rate_X %>%
  mutate(slip =case_when(
    (Cancer %in% PRE_SPEC & Stage %in% NO_PRIOR_HISTORY) ~ NPH_SLIP_RATE, 
    TRUE~slip
  ))
rm(multiple_dwell_slip_rate_X)
stopifnot(length(unique(multiple_dwell_slip_rate_nonseq_NPH$Cancer))==25)

#3. nonseq with PH
# in this scenario cancers may be detectable in stage 1 without metastasis
# however there is no ability to detect in stages 2 and 3 as they are assumed equivalent to metastatic
# in this scenario
multiple_dwell_slip_rate_Y<-bind_rows(sapply(SET_SCREEN_INTERVAL,function(z){
  exact_slip_rate_from_dwell(dwell_all_nonseq_ph,screen_interval=z,weibull_shape=1)
},simplify=FALSE)) %>%
  distinct() #handle the repeated MIS problem by ensuring only one copy

#Set the slip rate for stages 2 and 3 to 1, may not be needed as very short dwell will lead to  ~ 1 for slip rate
# i.e. no ability to intercept cancers in stages 2 and 3
multiple_dwell_slip_rate_nonseq_PH = multiple_dwell_slip_rate_Y %>%
  mutate(slip =case_when(
    (Cancer %in% PRE_SPEC & Stage %in% PRIOR_HISTORY) ~ NPH_SLIP_RATE, 
    TRUE~slip
  ))
rm(multiple_dwell_slip_rate_Y)
stopifnot(length(unique(multiple_dwell_slip_rate_nonseq_PH$Cancer))==25)

#filter down to only scenarios we're going to examine to reduce computation and size of data frames
multiple_dwell_slip_rate<- multiple_dwell_slip_rate %>%
  filter(scenario %in% DWELL_TIME)

multiple_dwell_slip_rate_nonseq_NPH<-multiple_dwell_slip_rate_nonseq_NPH %>%
  filter(scenario %in% DWELL_TIME)

multiple_dwell_slip_rate_nonseq_PH<-multiple_dwell_slip_rate_nonseq_PH %>%
  filter(scenario %in% DWELL_TIME)
