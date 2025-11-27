#big split survival

full_imputed_survival_by_age<-read_tsv(sprintf("generated_data/%s_full_imputed_survival_by_age.tsv", date_code))


xStage=c("I","II","III","IV","NotStaged")

# note: joining this to the full interception matrix may be hazardous to memory indexing
# also not complete if constructing full survival instead of 5-year only: 
# need to convolve with lead time to get survival curve
# this is just for plotting purposes to show differences in curves
full_split_survival<-full_imputed_survival_by_age %>%
  left_join(final_sens_table) %>%
  left_join(tibble(local_haz_ratio=c(1,2,3)),by=character()) %>%
  mutate(grail_negative=mapply(find_cfdna_negative_survival,CSS,local_haz_ratio,sens)) %>%
  mutate(grail_positive=grail_negative^local_haz_ratio) %>%
  select(Age,NCRAS_Draw,Stage,local_haz_ratio,Months,grail_negative,grail_positive) %>%
  pivot_longer(cols=c("grail_positive","grail_negative"),names_to="type",values_to="CSS") %>%
  mutate(cfdna_detectable=case_when(type=="grail_positive" ~ "yes",
                                    TRUE ~ "no")) %>%
  mutate(prequel=match(Stage,xStage)) %>% 
  left_join(final_sens_table %>% select(NCRAS_Draw,Stage,sens)) %>%
  select(NCRAS_Draw,Age,prequel,cfdna_detectable,local_haz_ratio,Months,CSS, sens) %>%
  mutate(sens=case_when(cfdna_detectable=="yes" ~ sens,
                        TRUE ~ 1-sens)) %>%
  rename(local_fraction=sens)


# generate df's for plotting ----------------------------------------------

disp_split_survival = full_split_survival %>%
  filter(Age == 65, local_haz_ratio==3) %>% #65 as mid-way through screening programme
  select(-Age, -local_haz_ratio, -local_fraction) %>%
  rename(Stage=prequel) 

surv_NCRAS_plot = full_imputed_survival_by_age %>%
  filter(Age==65) %>%
  mutate(Stage=case_when(
    Stage=="I" ~ 1, 
    Stage=="II" ~ 2,
    Stage=="III" ~ 3,
    Stage=="IV" ~ 4,
  )) %>%
  mutate(cfdna_detectable="observed") %>%
  select(-Age)

surv_plot_all = bind_rows(disp_split_survival, surv_NCRAS_plot) 

surv_plot_all = surv_plot_all %>%
  mutate(NCRAS_Draw=case_when(
    NCRAS_Draw %in% "Esophagus" ~ "Oesophagus",
    TRUE~NCRAS_Draw
  )) %>%
  mutate(`cfDNA detectable`=cfdna_detectable) %>%
  select(-cfdna_detectable) %>%
  arrange(NCRAS_Draw) 

# Plot example cancers surv by cfDNA status (Figure 2) --------------------
vis_eg=c("Breast","Colon/Rectum","Gallbladder","Head and Neck","Liver/Bile-duct","Lung","Melanoma","Prostate","Sarcoma") #subset of cancers which demonstrate what can happen with cfDNA status
surv_plot_vis_eg = surv_plot_all %>%
  filter(NCRAS_Draw %in% vis_eg) 

#write out excel file
write.xlsx(surv_plot_vis_eg, sprintf("generated_data/%s_fig_2_surv_eg_by_cfdna.xlsx", date_code))

foo = ggplot(data = surv_plot_vis_eg, aes(x=Months,y=CSS,group=`cfDNA detectable`, color=`cfDNA detectable`)) + 
  geom_point() +
  geom_smooth(se=FALSE) +
  scale_color_manual(values = c("#612B5B", "#B89466","#E0E0E0")) +
  facet_wrap(~NCRAS_Draw + Stage , ncol = 4, nrow = 22) +
  labs(
    x="Months",
    y="5 Year Net Survival",
    fill = "") +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(strip.background = element_blank(),
    strip.text.x = element_blank())  
print(foo)
ggsave(filename = sprintf("figs/%s_Figure2_surv_eg_by_cfdna.png",date_code), plot = foo, width = 30, height = 40, units = "cm")


# Plot all cancers surv by cfDNA status (S9) ------------------------------
not_vis=c("[OTHER]","Plasma Cell Neoplasm","Myeloid Neoplasm","Lymphoid Leukemia")
surv_plot_all_vis = surv_plot_all %>%
  filter(NCRAS_Draw %!in% not_vis) 
#batch these for the purposes of visualisation
batch_1=c("Anus","Bladder","Breast","Cervix","Colon/Rectum","Gallbladder","Head and Neck","Kidney","Liver/Bile-duct","Lung") 
batch_2=c("Lymphoma","Melanoma","Oesophagus","Ovary","Pancreas","Prostate","Sarcoma","Stomach","Thyroid","Urothelial Tract","Uterus")

surv_plot_all_vis_b1 = surv_plot_all %>%
  filter(NCRAS_Draw %in% batch_1)
#write out excel file
write.xlsx(surv_plot_all_vis_b1, sprintf("generated_data/%s_SF9_surv_b1_by_cfdna.xlsx", date_code))

surv_plot_all_vis_b2 = surv_plot_all %>%
  filter(NCRAS_Draw %in% batch_2)
#write out excel file
write.xlsx(surv_plot_all_vis_b2, sprintf("generated_data/%s_SF9_surv_b2_by_cfdna.xlsx", date_code))


foo = ggplot(data = surv_plot_all_vis_b1, aes(x=Months,y=CSS,group=`cfDNA detectable`, color=`cfDNA detectable`)) + 
  geom_point() +
  geom_smooth(se=FALSE) +
  scale_color_manual(values = c("#612B5B", "#B89466","#E0E0E0")) +
  facet_wrap(~NCRAS_Draw + Stage , ncol = 4, nrow = 22) +
  labs(
    x="Months",
    y="5 Year Net Survival",
    fill = "") +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())  
print(foo)
ggsave(filename = sprintf("figs/%s_S9_b1_surv_all_by_cfdna.png",date_code), plot = foo, width = 30, height = 40, units = "cm")

foo = ggplot(data = surv_plot_all_vis_b2, aes(x=Months,y=CSS,group=`cfDNA detectable`, color=`cfDNA detectable`)) + 
  geom_point() +
  geom_smooth(se=FALSE) +
  scale_color_manual(values = c("#612B5B", "#B89466","#E0E0E0")) +
  facet_wrap(~NCRAS_Draw + Stage , ncol = 4, nrow = 22) +
  labs(
    x="Months",
    y="5 Year Net Survival",
    fill = "") +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())  
print(foo)
ggsave(filename = sprintf("figs/%s_S9_b2_surv_all_by_cfdna.png",date_code), plot = foo, width = 30, height = 40, units = "cm")





