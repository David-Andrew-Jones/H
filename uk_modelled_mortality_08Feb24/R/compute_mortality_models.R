#some mortality models

#basic tools for combining mortality curve and density
# given lead time during which individuals should not die
# deaths by end of time interval 1
# are individuals who have passed through lead time in this interval (lead_dens)
# who then died in this interval (mortality curve)
# then sum over future time points the deaths that happen
# arguably need to offset by 1/2 time-period or fine-grain time intervals
# because not everyone passes through lead time at start of time interval
mortality_convolve<-function(lead_density,mortality_curve){
  zz<-rep(0,length(lead_density))
  for (tt in 1:length(lead_density)){
    zz[tt]<-0
    for (yt in 1:tt){
      zz[tt]<-zz[tt]+lead_density[yt]*mortality_curve[tt-yt+1]
    }
  }
  zz
}


#conditional mortality using one model
#conditional on immortal until a given time
#use conditional survival after that time to count deaths
mortality_conditional<-function(lead_density,mortality_curve){
  zz<-rep(0,length(lead_density))
  for (tt in 1:length(lead_density)){
    zz[tt]<-0
    for (yt in 1:tt){
      zz[tt]<-zz[tt]+lead_density[yt]*(1-(1-mortality_curve[tt])/(1-mortality_curve[yt])) #survival at tt is conditioned on reaching yt
    }
  }
  zz
}

#lead time only, original cure,original not cure, original cure fraction
#survival curve if 0 benefit from early detection
#death occurs on same date
compute_lead_time<-function(lead_dens,
                            prequel_curve,
                            clinical_curve){
  1-mortality_convolve(lead_dens,1-clinical_curve)
}

#just the curve expected with no lead time
#can lead to shortened lifespan because no lead time is accounted for
compute_prequel<-function(lead_dens,
                          prequel_curve,
                          clinical_curve){
  prequel_curve
}

#effectively: draw from same quantile of survival function
#take whichever is better
#effectively wever_2B model
# take the better of two survival curves with minimal assumptions
# satisfies no-shortening lifespan requirement
compute_noworse_B<-function(lead_dens,
                          prequel_curve,
                          clinical_curve){
  z_ld<-compute_lead_time(lead_dens,prequel_curve,clinical_curve)
  z_pq<-compute_prequel(lead_dens,prequel_curve,clinical_curve)
  pmax(z_ld,z_pq)
}

#optimistic: take independent draw from each survival function
#keep whichever is better
#computes wever_2A model
# effectively can double-count cure fraction
# by giving two opportunities to be cured, one at earlier, one at later
compute_noworse_A<-function(lead_dens,
                            prequel_curve,
                            clinical_curve){
  z_ld<-compute_lead_time(lead_dens,prequel_curve,clinical_curve)
  z_pq<-compute_prequel(lead_dens,prequel_curve,clinical_curve)
  1-(1-z_ld)*(1-z_pq) #survive to time t if either would survive
}

#shift the hazard of the prequel to the first time they can die
#that is, no benefit for surviving lead time
#but benefit for detection at earlier stage
# because hazard is decreasing through time
# hazard at time 0+x for prequel should be larger than hazard at time t+x that is survived
# providing a conservative bound
# although Wever reports for prostate PSA this is optimistic
# possibly due to PSA overdiagnosis over-estimating survival at earlier stage
compute_hazard<-function(lead_dens,
                         prequel_curve,
                         clinical_curve) {
  1-mortality_convolve(lead_dens,1-prequel_curve)
}

#compute conditional mortality given that you have survived to this point
# very optimistic in most cases
# double-counts cure fraction
# as long-term behavior is dominated by long-term survivors
# and are then counted as being the entirety of the population at long lead times
compute_conditional<-function(lead_dens,
                              prequel_curve,
                              clinical_curve){
  1-mortality_conditional(lead_dens,1-prequel_curve)
}

