#preprocess CCGA3 results into expected format for interception model
#include isotonic regression
#raw data generally preserved - remove unknown & multiple primaries
#keep sensitivities for effectively not-staged cancers

library(tidyverse)
library(Iso)

#raw data table from Klein et al
sens_simple<-read_tsv("data/20200121_SensitivitybyCancerTypeandStage.tsv")

#not expected to be staged in SEER
special_set<-c("Lymphoid Leukemia","Myeloid Neoplasm","Plasma Cell Neoplasm")

xStage<-c("I","II","III","IV","NotStaged")

full_ccga3_sens<-sens_simple %>%
  select(Cancer=cancer_type_tfl,Stage=cstage,c=detec_cancers,n=total_cancers) %>%
  filter(Stage!="Missing") %>%
  mutate(Stage=case_when(grepl("Not",Stage) ~ "NotStaged",
                         TRUE ~ Stage)) %>%
  complete(Cancer,Stage=xStage,fill=list(c=0,n=0))  %>%
  mutate(Cancer=case_when(Cancer=="Other" ~ "[OTHER]",
                          TRUE ~ Cancer)) %>%
  filter(Cancer!="Unknown Primary",Cancer!="Multiple Primaries")

full_ccga3_stageable<-full_ccga3_sens %>%
  filter(!Cancer %in% special_set) %>%
  filter(Stage!="NotStaged") 

full_ccga3_notstaged<-full_ccga3_sens %>%
  filter(Cancer %in% special_set | (Cancer=="[OTHER]" & Stage=="NotStaged")) %>%
  group_by(Cancer) %>%
  summarize(Stage="NotStaged", c=sum(c),n=sum(n)) %>%
  ungroup() 



#isotone regression for sensitivity by stage
isotone_fix<-function(sens,Stage,num){
  out_val<-sens
  ndx<-match(xStage,Stage) #numbers from 1:5, natural ordering
  good_ndx<-ndx[!is.na(ndx)]
  if (length(good_ndx)>1){
    #need stages I-IV only in order
    y<-sens[good_ndx]
    w<-num[good_ndx]
    val<-pava(y,w)
    out_val[good_ndx]<-val #put back
  } 
  out_val
}

ccga3_iso_stageable<-full_ccga3_stageable %>%
  mutate(sensitivity=case_when(n>0 ~ c/n,
                               TRUE ~ 0.0)) %>%
  group_by(Cancer) %>%
  mutate(original_sens = sensitivity,
         sens=isotone_fix(sensitivity,Stage,n)) %>%
  ungroup() 

ccga3_iso_notstaged<-full_ccga3_notstaged %>%
  mutate(sensitivity=case_when(n>0 ~ c/n,
                               TRUE ~ 0.0)) %>%
  group_by(Cancer) %>%
  mutate(original_sens = sensitivity,
         sens=sensitivity) %>%
  ungroup() 

full_ccga3_iso_sens<-bind_rows(ccga3_iso_stageable,ccga3_iso_notstaged) %>%
  select(Cancer,Stage,c,n,original_sens,sens)

#write out isotonic estimates for sensitivity to be used in modeling
write_tsv(full_ccga3_iso_sens,sprintf("generated_data/%s_ccga3_iso_sens.tsv",date_code))
