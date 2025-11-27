################################################################################
#______________________________________________________________________________#
#### Galleri sensitivity and 5 year net survival by stage
#______________________________________________________________________________#
################################################################################

library(tidyverse)
library(tidyr)

df_sens_5yrnetsurv <- read_csv(file = '~/Miscellaneous/gallerisens_5y_netsurv.csv')

temp <- df_sens_5yrnetsurv %>%
        filter(!is.na(cancer)) %>%
        #separate(sens_95, c("sens_low95", "sens_high95"), "-")
        separate_wider_delim(sens_95, delim = "-", names = c("sens_low95", "sens_high95")) %>% 
        mutate(sens = str_sub(sens, end = -2),
               sens_low95 = str_sub(sens_low95, start = 2 ),
               sens_high95 = str_sub(sens_high95, end = -3 )) %>%
        filter(cancer == "Anus") %>%
        mutate(across(starts_with("sens"), as.numeric))

ggplot(temp, aes(stage, sens)) +
        geom_point(size = 2) +
        geom_errorbar( aes(ymin = sens_low95, ymax = sens_high95),width = 0.2)



