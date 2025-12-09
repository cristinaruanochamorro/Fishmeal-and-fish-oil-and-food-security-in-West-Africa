#lSMS wealth data set
#Cristina Ruano
#30.7.2025


# Load necessary libraries
library(tidyverse)
library(haven)
library(sf)
library(ggplot2)
library(sjPlot)
library(rnaturalearth)
library(rnaturalearthdata)
library(survey)
library(magrittr)

# Set working directory
setwd("C:/Users/crist/OneDrive - Lancaster University/Lancaster University_RuanoChamorro/Postdoc project")


# Read data
housing_df<-read_csv("LSMS/SEN_2018_LSMS/s11_me_sen2018.csv")
assets_df<-read_csv("LSMS/SEN_2018_LSMS/s12_me_sen2018.csv")

# Create household ID
housing_df$hh_id <- paste(housing_df$vague,housing_df$grappe, housing_df$menage, sep = "_") 
head(housing_df)

# Rename selected asset columns

#s11q03_1: Air conditioned 
housing_df$aircon_housing<-housing_df$s11q03__1
#s11q03_2: Water heater
housing_df$w_heater<-housing_df$s11q03__2
#s11q03_3: Ceiling-mounted fans
housing_df$c_fan<-housing_df$s11q03__3

#s11q53: Cooking (1. collected firewood, 2.purchased firewood, 3.charcoal, 4.gas, 5.electricity, 6. petrol, 7.animal waste, 8.other)
#Improved cooking: 3,4,5,6
#Unimproved cooking:1,2,7,8

#If a household has 3,4,5,and/or 6, it has improved cooking, otherwise it has unimproved cooking


housing_df$ s11q53__3_f<-as.factor(housing_df$ s11q53__3)
housing_df$ s11q53__4_f<-as.factor(housing_df$ s11q53__4)
housing_df$ s11q53__5_f<-as.factor(housing_df$ s11q53__5)
housing_df$ s11q53__6_f<-as.factor(housing_df$ s11q53__6)

housing_df <- housing_df %>% 
  mutate(imp_cooking  = case_when(
    s11q53__3 %in% c(1, 2) |s11q53__4  %in% c(1, 2) | s11q53__5  %in% c(1, 2) |s11q53__6  %in% c(1, 2) ~ 1,
    TRUE ~ 0
  ))

freq_table <- table(housing_df$imp_cooking)
print(freq_table)


# Housing materials

#s11q19:Walls (1.concrete/cement/dressed stones, 2.clay bricks, 3. aluminium trat, windows, 4.improved mud bricks/semi-durable, 
#5.recycled materials (boards, metal sheets), 6. traditional simple stones, 7.Straw, mud brick, clod of earth, 8.other)
housing_df$walls<-housing_df$s11q19
#Improved walls:1,2,3,4
#Unimproved walls:5,6,7,8

housing_df$ walls_f<-as.factor(housing_df$ walls)
sum(is.na(housing_df$walls_f))#no NAs

housing_df <- housing_df %>%
  mutate(imp_walls  = case_when(
    walls_f %in% c(1, 2, 3, 4) ~ 1,
    TRUE ~ 0
  ))


#s11q20: ceiling (1.concrete slab, 2.tile, 3.metal sheets, 4.straw, 5.adobe, 6.thatch (dried plant materials), 7.mats, 8. other  )
housing_df$ceiling<-housing_df$s11q20
#Improved roofs: 1,2,3
#Unimproved roofs: 4,5,6,7,8

housing_df$ ceiling_f<-as.factor(housing_df$ceiling)
sum(is.na(housing_df$ceiling_f))#no NAs

housing_df <- housing_df %>%
  mutate(imp_ceiling  = case_when(
    ceiling_f %in% c(1, 2, 3) ~ 1,
    TRUE ~ 0
  ))


#s11q21: floors (1.tiles/marble, 2.concrete, 3. Hard earth/sand, 4.Animal dung, 5.other)
housing_df$floors<-housing_df$s11q21
#Improved floors:1,2
#Unimproved floors: 3,4,5

housing_df$floors_f<-as.factor(housing_df$floors)
sum(is.na(housing_df$floors_f))# no NAs

housing_df <- housing_df %>%
  mutate(imp_floors  = case_when(
    floors_f %in% c(1, 2) ~ 1,
    TRUE ~ 0
  ))


####Access to water:

#LSMS: What is the main source of drinking water for the household?
#s11q27a (dry season)
housing_df$water_dry<-housing_df$s11q27a
#s11q27b (wet season)
housing_df$water_wet<-housing_df$s11q27b

#TAP WATER :1)in the house, 2)in the yard/compound, 3)neighbours´ tap 4) water fountain/ public tap (Improved water)
#oPEN WELL: 5)in the yard/compound  6)Open well elsewhere (Unimproved)
#COVERED WELL OR BOREHOLE: 7) Covered well in the yard/compound  8) Covered well elsewhere,  9)Borehole in the compound, 10) Borehole elsewhhere (Improved)
#SURFACE WATER: 11)Improved spring, (Improved) 12) Unimproved spring, (Unimproved) 13) River, stream, lake or dam (Unimproved)
#14) Water in bottle (Improved)
#15)Rainwater (improved)
#16)Street vendor (unimproved)
#17)Other

#Access to protected water sources (yes/no)
#Protected dry season:
#s11q27a: yes if s11q27a = 1,2,3,4, 7, 8,9,10, 11,14,15 / no if 11q27a = 5, 6, 12, 13, 16, 17

#Protected wet season:
#s11q27b: yes if s11q27b = 1,2,3,4, 7, 8,9,10, 11,14,15 / no if 11q27b = 5, 6, 12, 13, 16, 17

housing_df$water_dry<-as.factor(housing_df$water_dry)

sum(is.na(housing_df$water_dry))#no NAs

housing_df <- housing_df %>%
  mutate(imp_water_dry  = case_when(
    water_dry %in% c(1,2,3,4, 7, 8,9,10, 11,14,15) ~ 1,
    TRUE ~ 0
  ))

housing_df$water_wet<-as.factor(housing_df$water_wet)
sum(is.na(housing_df$water_wet))# 2 NAs
housing_df %>% filter(is.na(water_wet)) # 1_533_8 and 1_383_3

housing_df <- housing_df %>%
  mutate(imp_water_wet  = case_when(
    water_wet %in% c(1,2,3,4, 7, 8,9,10, 11,14,15) ~ 1,
    TRUE ~ 0
  ))


#####Toilets

#11.55 What type of sanitation does your household use?

housing_df$toilet<-housing_df$s11q55

#1)Indoor toilter with flish
#2) External toilt with flush (Improved)
#3) Internatl toilter with manual flush (Improved)
#4) External toilter with manual flush (Improved)
#5) Latrines vip (paved, ventilated) (Improved)
#6) Latrines ECOSAN (Improved)
#7) lATRINES SANPLAT (Improved)
#8) Simply paved latrines (Improved)
#9) Rudimentary pit/open hole (Unimproved)
#10) Public toilets (Unimproved)
#11) No toilet (in the open)  (Unimproved)
#12) Other  (Unimproved)

#Improved sanitation facilities (yes/no)
#s11q55: yes if s11q55 = 1, 2, 3, 4, 5, 6, 7, 8 / no if s11q55 = 9,10,11,12

housing_df$toilet<-as.factor(housing_df$toilet)
sum(is.na(housing_df$toilet))# zeros NAs

housing_df <- housing_df %>%
  mutate(toilet_imp = case_when(
    toilet %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ 1,
    TRUE ~ 0
  ))


#select variables of interest in housing_df

housing_df_select<-housing_df%>%
  dplyr::select(hh_id,imp_water_wet, imp_water_dry, imp_cooking, imp_walls, imp_ceiling, imp_floors, toilet_imp, aircon_housing, w_heater, c_fan)


# Process assets data

#12.02 (avoirs= assets)
#s12q01 = code of the asset
#1. Living room (Armchairs and coffee table) (DHS)
#2. Dining table (table + chairs)
#3. Bed
#4. Single mattress
#5 Wardrobes and other furniture
#6. Carpet
#7. Electric iron
#8. Charcoal iron
#9. Gas or electric stove
#10. Gas cylinder
#11. Gas or electric hot plate
#12. Microwave or electric oven (DHS)
#13. Improved stoves
#14. Electric food processor (Moulinex)
#15. Non-electric blender/fruit press
#16. Refrigerator (DHS)
#17. Freezer
#18. Pedestal fan
#19. Radio (DHS)
#20. TV (DHS)
#21. VCR/CD/DVD player (DHS)
#22. Satellite dish / decoder (DHS)
#23. Washing machine, dryer
#24. Vacuum cleaner
#25. Air conditioners/split systems
#26. Lawn mower and other gardening tools
#27. Generator (DHS Generator/solar panel)
#28. Personal car (DHS)
#29. Moped, scooter, motorcycle (DHS)
#30. Bicycle (DHS)
#31. Camera
#32. Camcorder
#33. Hi-Fi system
#34. Landline phone (DHS)
#35. Mobile phone (DHS)
#36. Tablet
#37. Computer (DHS)
#38. Printer/Fax
#39. Video camera
#40. Canoe and outboard motor (pleasure boats) (DHS)
#41. Hunting rifles
#42. guitar
#43. piano and other instruments
#44. Bulding/house
#45. unbiult land

assets_df$hh_id <- paste(assets_df$vague,assets_df$grappe, assets_df$menage, sep = "_")
head(assets_df)
assets_df$s12q01_f<-as.factor(assets_df$s12q01)

assets_df$assets <- recode(assets_df$s12q01_f , "1" = "armchairs", "2" = "table_chairs", "3" = "bed", "4"="s_mattres",
                           "5"="wardrobes", "6"="carpet", "7"="e_iron", "8"="c_iron", "9"="gas_elec_stove", "10"="gas_cylinder",
                           "11"="gas_e_hotplate", "12"="microwave","13" = "improved_stoves", "14"= "e_foodprocesor", "15"= "non_e_blender",
                           "16"="refrigerator", "17" = "freezer", "18"="p_fan", "19"="radio", "20"="tv", "21"="dvd",
                           "22"="sat_dish", "23"="washing_machine", "24"="v_cleaner", "25"= "aircon_asset", "26"="gard_tools",
                           "27"="generator", "28"="car", "29"="motorcycle", "30"="bicycle", "31"="camera", "32"="camcorder","33"="hifi",
                           "34"="land_phone", "35"= "mobile_p", "36"="tablet", "37"="computer","38"="printer", "39"="video_camera",
                           "40"="canoe", "41"="hunting_rif", "42"="guitar", "43"="piano", "44"="building_house", "45"="unbuilt_land")

# 1=yes and 2=no



assets_df_select<-assets_df%>%
  dplyr::select(hh_id,assets,s12q02)


duplicates<-assets_df_select %>%  #no duplicates
  filter(duplicated(assets_df_select))


assets_df_wide<-assets_df_select%>%
  pivot_wider(
    names_from=assets, 
    values_from =s12q02,
    values_fn = sum
  )

print(assets_df_wide)


sum(is.na(assets_df_wide$armchairs))#no NAs

# change 2 to 0

assets_df_wide[assets_df_wide == 2] <- 0


#Other potential varaibles:  bank account (DHS)domestic servant in household, owns a dwelling, Number of members per sleeping room
#cattle, cows, bulls, owns agricultural land
#Electricity

#merge datasets and create the index (pca)

LSMS_wealth_index_df_C<-merge(housing_df_select, assets_df_wide, by="hh_id", all.x=TRUE)


write.csv(LSMS_wealth_index_df_C, "C:/Users/crist/OneDrive - Lancaster University/Lancaster University_RuanoChamorro/Postdoc project/DHS/LSMS_wealth_index_df_C.csv", row.names = FALSE)

##END