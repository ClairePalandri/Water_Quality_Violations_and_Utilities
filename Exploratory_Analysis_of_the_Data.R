
####################Intialization##############
library(dplyr)
setwd("~/Sr Research Assistant/Fines and Bottle-Medicine Sales - Claires Dataset")
#The current model uses RTC dataset as a proxy
#Further we only also consider the first two weeks of the violations.


##################Data_Input##########################
fulls <- read.csv("cnty_wk_V2_FULLPANEL.csv")
full <- fulls %>% 
        select(county_FIPS, week_end, dum_1stwk_viol, dum_2ndwk_viol, defl_medsales_pc, defl_bottlesales_pc)
#The Na's of the share of the total population can be converted to zero.


##########Creating the Dataset for violation for first and second weeks################ 
full$dum_1stwk_viol[is.na(full$dum_1stwk_viol)] <- 0  
full$dum_2ndwk_viol[is.na(full$dum_2ndwk_viol)] <- 0
full$dum_2ndwk_viol <- full$dum_1stwk_viol + full$dum_2ndwk_viol
colSums(is.na(full))


#########Selecting counties with complete Med Sales Data############
macro_analysis <- full %>% 
                  group_by(county_FIPS) %>%
                  summarise(meds = sum(defl_medsales_pc))
macro_analysis <- macro_analysis %>% 
                  filter(meds > 0)


##################Merging###############
Merged_dataset <- merge(full,macro_analysis, by = "county_FIPS")
Merged_dataset$meds <- NULL
colSums(is.na(Merged_dataset))


##########Saving the Dataset################
write.table(Merged_dataset, "Dataset for Regression.txt", sep = " ")
