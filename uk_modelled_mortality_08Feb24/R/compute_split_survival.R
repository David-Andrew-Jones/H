#utility function: given that we are splitting survival into cfdna+ and cfdna-
# so that weighted average (detection rate) is the original survival
# and that we have a given hazard rate between the two cures

find_cfdna_negative_survival<-function(o_surv,haz_ratio,det_rate){ #unclear what this is doing - ie., when is survival negative
  tmp_fcn<-function(t_surv){
    (t_surv^haz_ratio*det_rate + t_surv*(1-det_rate)-o_surv)^2
  }
  par=optimize(tmp_fcn,interval=c(0,1))
  par$minimum
}