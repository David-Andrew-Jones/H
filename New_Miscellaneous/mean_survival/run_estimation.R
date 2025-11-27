library(survextrap)
library(ggplot2)
library(dplyr)
library(viridis) # for colour palettes


survminer::ggsurvplot(survfit(Surv(years, d) ~ treat, data=cetux)) + xlab("Years")