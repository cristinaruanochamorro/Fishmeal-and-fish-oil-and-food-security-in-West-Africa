#LSMS SenegaL 2018
#Fish consumption and sociodemographics (clean version of R script food consumption_sociodem_lsms_18_b)
#Previous version:  food consumption_sociodem_lsms_18_b
#Date of previous verison: 5.3.2025 and updated the 28.4.25
#current version: 30.7.25
#Cristina Ruano Chamorro


#Load packages
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

survey_info<-read_csv("LSMS/SEN_2018_LSMS/s00_me_sen2018.csv")
hh_id_info<-read_csv("LSMS/SEN_2018_LSMS/ehcvm_individu_sen2018.csv")
hh_food_comsumption_inside_b<- read_csv("LSMS/SEN_2018_LSMS/s07b_me_sen2018.csv")#this has info about food consumed including fish
hh_food_comsumption_a1<- read_csv("LSMS/SEN_2018_LSMS/s07a1_me_sen2018.csv")

hh_food_comsumption_a1$code_id<-hh_food_comsumption_a1$s07aq00#this is the code of the person who responded the consumption surveys.

#grappe = hh_cluster
#menage = household
#vague = sampling unit

#create hh_id and a hh_cluster in each dataset
hh_id_info

hh_id_info$hh_id <- paste(hh_id_info$vague,hh_id_info$grappe, hh_id_info$menage, sep = "_")
head(hh_id_info)
length(unique(hh_id_info$hh_id))# 7156

hh_id_info$hh_cluster <- paste(hh_id_info$vague,hh_id_info$grappe, sep = "_")
head(hh_id_info)

hh_food_comsumption_inside_b$hh_id <- paste(hh_food_comsumption_inside_b$vague, hh_food_comsumption_inside_b$grappe, hh_food_comsumption_inside_b$menage, sep = "_")
head(hh_food_comsumption_inside_b)

length(unique(hh_food_comsumption_inside_b$hh_id))# 7101

hh_food_comsumption_inside_b$hh_cluster <- paste(hh_food_comsumption_inside_b$vague,hh_food_comsumption_inside_b$grappe, sep = "_")
head(hh_food_comsumption_inside_b)


#food consumption inside????????????


#####Household id information#####

#region

hh_id_info$region

#departamento

hh_id_info$departement

#urban (1)/rural(2) = milieu

hh_id_info$urban_rural<-hh_id_info$milieu


#hh weight: hhweight

hh_id_info$hhweight

#select first id (numind). In some hh there is no numind =1, therefore, many hh would be eliminated. I am not sure which of these individuals is the one who was asked for food consumption


#household level
hh_id_hh_clean<-hh_id_info%>%
  dplyr::select(hh_id, hh_cluster, region, departement, urban_rural, hhweight)

#remove duplicates

hh_id_hh_clean2_C<-unique(hh_id_hh_clean)
nrow(hh_id_hh_clean2_C)#7156

#write.csv(hh_id_hh_clean2_C, "C:/Users/crist/OneDrive - Lancaster University/Lancaster University_RuanoChamorro/Postdoc project/DHS/hh_id_hh_clean2_C.csv", row.names = FALSE)

#Food consumption
#hh_food_comsumption_inside_b$code_id<-s07aq00


# s07bq01: food product

hh_food_comsumption_inside_b$food_item<-hh_food_comsumption_inside_b$s07bq01

#35 = Poisson frais yaboye ou obo (sardinelle). Fresh yaboi u ovo (sardinella)
#36 = Poisson frais thiof/ seudeu (baracouda). Fresh barracuda
#37 = Poisson frais wass 
#38 = Autre Poisson frais (dorade, youfouf, rouget, siket [capitaine], thiarumbekh [mollette], …..)
#39 = Poisson fumé Kethiakh (sardinelle): smoked sardinella
#40 = Autre Poisson fumé (Con fumé, yaboye ou obo fumé, …): somode yaboi u ovo
#41 = Poisson séché: dry fish
#42= Crabes, crevettes et autres fruits de mer
#43= Conserves de poisson 


#s07bq02: has your houshold consume this product on the past 7 days? (yes/no)
hh_food_comsumption_inside_b$fish_consumption_bi<-hh_food_comsumption_inside_b$s07bq02

hh_food_comsumption_inside_b$fish_consumption_bi


#keep only fish food items

#add all food item columns and add a zero to s07bq02 if these were not consumed (from 1 to 138)
#then select the fish products (35-43)

#Create a dataset with all possible combinations of hh_id and food_item (1 to 138)
all_food_items <- expand.grid(hh_id = unique(hh_food_comsumption_inside_b$hh_id), food_item = 1:138)
nrow(all_food_items)#979938

#Join the original table with this list of all food items
hh_food_consumption_complete <- all_food_items %>%
  left_join(hh_food_comsumption_inside_b, by = c("hh_id", "food_item"))

#Make sure that the fish_consumption_bi column has 0 where there is no data
hh_food_consumption_complete <- hh_food_consumption_complete %>%
  mutate(fish_consumption_bi= ifelse(is.na(fish_consumption_bi), 0, fish_consumption_bi))
nrow(hh_food_consumption_complete)#979938

#Select only relevant variables: hh_id, food_item,  fish_consumption_bi

hh_food_consumption_complete_clean<-hh_food_consumption_complete %>%
  dplyr:: select( hh_id, food_item, fish_consumption_bi)


#add cluster again because new rows won´t have them

hh_food_consumption_complete_clean$hh_cluster <- sub("(_[^_]*)$", "", hh_food_consumption_complete_clean$hh_id)

#Filter the data to keep only food_item values between 35 and 43
hh_fish_consumption <- hh_food_consumption_complete_clean%>%
  filter(food_item >= 35 & food_item <= 43)

hh_fish_consumption$food_item_fact<-as.factor(hh_fish_consumption$food_item)
levels(hh_fish_consumption$food_item_fact) #"35" "36" "37" "38" "39" "40" "41" "42" "43"



hh_fish_consumption <- hh_fish_consumption%>%
  mutate(food_item = recode_factor(food_item,
                                   "35" = "fresh_sardinella", 
                                   "36" = "fresh_barracuda", 
                                   "37" = "fresh_tilapia",
                                   "38" = "fresh_other",
                                   "39" = "smoked_keti_sardinella",
                                   "40" = "smoked_sardinella",
                                   "41" = "dried_fish",
                                   "42" = "crabs",
                                   "43" = "conserves"  ))

hh_fish_consumption$food_item


#hh_id_info$hhweight<-hh_fish_consumption%>%
 # dplyr::select(hh_id,hh_cluster, food_item,fish_consumption_bi)


#wide consumption data


hh_fish_consumption_wide <-hh_fish_consumption %>%
  dplyr::select(hh_id, hh_cluster, food_item, fish_consumption_bi) %>%
  pivot_wider(
    names_from = food_item, 
    values_from = fish_consumption_bi, values_fill = list(value = NA))

nrow(hh_fish_consumption_wide) # 7101

food_consumption_lsms_18_clean.C<-hh_fish_consumption_wide

write.csv(food_consumption_lsms_18_clean.C, "C:/Users/crist/OneDrive - Lancaster University/Lancaster University_RuanoChamorro/Postdoc project/DHS/food_consumption_lsms_18_clean.C.csv", row.names = FALSE)

#END