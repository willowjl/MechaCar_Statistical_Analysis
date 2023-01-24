#DELIVERABLE 1
library(tidyverse)
library(dplyr)

mc_mpg <- read.csv(file='MechaCar_mpg.csv', check.names=F, stringsAsFactors = F)

lm(mpg~vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mc_mpg)

summary(lm(mpg~vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mc_mpg))

#DELIVERABLE 2
s_coil <- read.csv(file='Suspension_Coil.csv', check.names=F, stringsAsFactors = F) #Read in dataset as a dataframe

total_summary <- s_coil %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI)) #create table for total summary
lot_summary <- s_coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep') #lot summary


#DELIVERABLE 3
t.test(s_coil$PSI, mu = 1500)

t.test(subset(s_coil,Manufacturing_Lot=="Lot1")$PSI, mu = 1500)
t.test(subset(s_coil,Manufacturing_Lot=="Lot2")$PSI, mu = 1500)
t.test(subset(s_coil,Manufacturing_Lot=="Lot3")$PSI, mu = 1500)
