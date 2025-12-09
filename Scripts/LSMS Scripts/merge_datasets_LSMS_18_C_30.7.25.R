#LSMS Senegal 2018 datasets merge to create final data
#30.7.2025
#Cristina Ruano Chamorro

library(readr)
library(tidyverse)
library(Hmisc)
library(plyr)
library(dplyr)
library(haven)
library(sf)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(survey)
library(magrittr)
library(ggplot2)
library(sjPlot)

setwd("C:/Users/crist/OneDrive - Lancaster University/Lancaster University_RuanoChamorro/Postdoc project")

hh_id_hh_clean2<-read_csv("DHS/hh_id_hh_clean2.csv")#7156 rows

food_consumption_lsms_18_clean.C<-read_csv("DHS/food_consumption_lsms_18_clean.C.csv")#  7101 rows. It has many rows because food items (colums) are located in different rows. Id on´t know how to fix this

LSMS_wealth_index_18_all_C<-read_csv("DHS/LSMS_wealth_index_18_all_C.csv")# 7156 Why does it have more rows than the rest?

#hh_food_comsumption_outside_b<- read_csv("LSMS/SEN_2018_LSMS/s07a1_me_sen2018.csv")#useful to check the id of the person who responded the consumption section
#nrow(hh_food_comsumption_outside_b)#7156 rows. 

hh_employment<- read_csv("DHS/hh_employment_edu.csv")
nrow(hh_employment)# 66120 rows. There are multiple individuals in 1 household.


#check who (the person ID) respondend the consumption survey. I don´t think this matter because the person is being asked at the household level.

#create individual id of the person who responded the survey
#hh_food_comsumption_outside_b$ind_id <- paste(hh_food_comsumption_outside_b$vague,hh_food_comsumption_outside_b$grappe, hh_food_comsumption_outside_b$menage,hh_food_comsumption_outside_b$s07aq00, sep = "_")

#create household id
#hh_food_comsumption_outside_b$hh_id <- paste(hh_food_comsumption_outside_b$vague,hh_food_comsumption_outside_b$grappe, hh_food_comsumption_outside_b$menage, sep = "_") 

#hh_food_comsumption_outside_b_clean<- hh_food_comsumption_outside_b[,c("hh_id","ind_id")]


#transform hh_id into factor
hh_id_hh_clean2$hh_id<-as.factor((hh_id_hh_clean2$hh_id))
LSMS_wealth_index_18_all_C$hh_id<-as.factor((LSMS_wealth_index_18_all_C$hh_id))
food_consumption_lsms_18_clean.C$hh_id<-as.factor((food_consumption_lsms_18_clean.C$hh_id))
hh_employment$hh_id<-as.factor((hh_employment$hh_id))


#merge
LSMS_18_clean<-merge(hh_id_hh_clean2,LSMS_wealth_index_18_all_C, by="hh_id", all.x=TRUE)#7156
LSMS_18_clean<-merge(LSMS_18_clean,food_consumption_lsms_18_clean.C, by="hh_id", all.x=TRUE)#7101
LSMS_18_clean<-merge(LSMS_18_clean,hh_employment, by="hh_id", all.x=TRUE)
nrow(LSMS_18_clean)# 7156

LSMS_18_clean <- LSMS_18_clean %>%
  dplyr::select(-hh_cluster.x) %>%               # Remove hh_cluster.x
  dplyr::rename(hh_cluster = hh_cluster.y)         # Rename hh_cluster.y to cluster

#Add travel time to factories data

#NearFact: the name of the nearest factory (based on tt)
#tt_NearFact = the travel time from the nearest factory (in minutes)
#NearFact_L: the nearest factory using linear distance (only when no factory is found within 500km)
#Ldist_NearFact_L: the corresponding linear distance (in meters) (only when no factory is found within 500km)

#if we only use tt_NearFact we are not taking into account hh beyond 500km

factories_lsms_18<- read_csv("DHS/Eva distanc/Eva distance LSMS james/Res_tt_NearFactory_Senegal.csv")
nrow(factories_lsms_18)# 6793 rows



LSMS_18_clean<-merge(LSMS_18_clean,factories_lsms_18, by="hh_id", all.x=TRUE)

#Include data on operational factories

operational_factories_lsms_18<- read_csv("DHS/factories_lsms_18_b_Sen_b_C.csv")

colnames(operational_factories_lsms_18)

anyNA(operational_factories_lsms_18$Min_tt_factories)
sum(is.na(operational_factories_lsms_18$Min_tt_factories))#318 NA
nrow(operational_factories_lsms_18)# 6793 rows


LSMS_18_clean<-merge(LSMS_18_clean,operational_factories_lsms_18, by="hh_id", all.x=TRUE)

#Add travel time to markets

markets_lsms_18<- read_csv("DHS/Eva distanc/Eva distance LSMS james/Res_tt_NearMarket_Senegal.csv")
nrow(markets_lsms_18)# 6793 rows

LSMS_18_clean<-merge(LSMS_18_clean,markets_lsms_18, by="hh_id", all.x=TRUE)


#distance and markets were calculated from the data used by James. there are less hh than in the original data (363 hh less).
#ask eva to update this and also get distances for 2021 data

#distance to water bodies

load("DHS/Eva distanc/Eva distance LSMS james/distance_waterbodies_LSMS_18.Rdata")
LSMS_18_clean<-merge(LSMS_18_clean,distance_waterbodies_LSMS_18, by="hh_cluster", all.x=TRUE)


LSMS_18_clean.3<-LSMS_18_clean

sum(is.na(LSMS_18_clean.3$Min_tt_factories))#681 NA


write.csv(LSMS_18_clean.3, "C:/Users/crist/OneDrive - Lancaster University/Lancaster University_RuanoChamorro/Postdoc project/DHS/LSMS_18_clean.3.csv", row.names = FALSE)


#END


