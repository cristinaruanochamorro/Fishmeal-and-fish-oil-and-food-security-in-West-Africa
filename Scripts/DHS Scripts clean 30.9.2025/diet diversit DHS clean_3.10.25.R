# Children - Combine all DHS datasets
#Diet diversity
#clean version 30.9.35

library(tidyverse); theme_set(theme_classic())
library(Hmisc)
library(haven)
library(plyr)
library(dplyr)
library(sjPlot)
library(car)
library(sf)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(survey)
library(magrittr)
library(ggplot2)
library(sjPlot)

setwd("C:/Users/crist/OneDrive - Lancaster University/Lancaster University_RuanoChamorro/Postdoc project")

# Read in women's data for all countries/years:
SNkr18 <- readRDS("DHS/DHS Senegal/SN_2018_CONTINUOUSDHS/Kids_SNKR81FL.rds") #SN7: check version of the survey and variables
SNkr19 <- readRDS("DHS/DHS Senegal/SN_2019_CONTINUOUSDHS/Kids_SNKR8BFL.rds") 

k_DHS_SEN <- rbind.fill(SNkr18, SNkr19)

k_DHS_SEN $DHSCLUST = paste(substr(k_DHS_SEN$v000, start = 1, stop = 2), k_DHS_SEN$v001, k_DHS_SEN$v007, sep="_")

k_DHS_MAU <- readRDS("DHS/DHS Mauritania/Kids_MRKR71FL.rds")

#k_DHS_MAU$DHSCLUST <- paste(substr(k_DHS_MAU$v000, start = 1, stop = 2), k_DHS_MAU$v001, "2019_2021", sep = "_")

k_DHS_MAU$DHSCLUST = paste("MR", k_DHS_MAU$v001,"2019_2021", sep="_")

k_DHS_GAM <- readRDS("DHS/DHS Gambia/Kids_GMKR81FL.rds")

#k_DHS_GAM $DHSCLUST <- paste(substr(k_DHS_GAM $v000, start = 1, stop = 2), k_DHS_GAM $v001, "2019_2020", sep = "_")


k_DHS_GAM$DHSCLUST = paste("GM", k_DHS_GAM $v001,"2019_2020", sep="_")


#factories_df_b<- read_csv("DHS/Eva distanc/Eva distance V3/Res_tt_AllFactories.csv")


# check dimensions (rows, columns):
dim(SNkr18) # 6719  1155
dim(SNkr19) # 6125  1155
dim(k_DHS_MAU) # 11628  1121
dim(k_DHS_GAM ) # 8362  1217

####clean dataset########


# Use rbind.fill to merge datasets, accounting for differing columns:
#k_DHS <- rbind.fill(SNkr14, SNkr15, SNkr16, SNkr17, SNkr18, SNkr19, SNkr23)
#,
#MRkr19_21, GMkr19_20)


k_DHS <- rbind.fill(k_DHS_SEN ,k_DHS_MAU,k_DHS_GAM)


dim(k_DHS)  # 32834  1368

# First cut-down of the data - keep only variables with the following prefixes:
#k_DHS2 <- k_DHS %>% 
# dplyr::select(tidyr::contains(c("caseid","s","v0","v1","b","v2","m","v4","v5",
#"hw","v7","v8","g","ecd","chd", "h31")))

# Remove variables that are ALL NA
k_DHS <- k_DHS[,-which(sapply(k_DHS, function(x)all(is.na(x))))]
dim(k_DHS)
# 32834  1077


# Save variable labels:
#k_codes <- colnames(SN_DHSk)
#k_sub <- label(SN_DHSk)

#k_variables <- as.data.frame(k_codes,k_sub)
#head(k_variables)

#write.csv(k_variables, "outputs/DHS8_kids-variables.csv", row.names=T)



# add decimal places to women's sample weights:
k_DHS$v005i <- k_DHS$v005/1000000

# add decimal places to wealth index
k_DHS$v191i <- k_DHS$v191/100000

# add decimal places into birth weight:
plot(k_DHS$m19) # first replace flagged obs with NA

k_DHS$m19 <- ifelse(k_DHS$m19 > 9000, NA,k_DHS$m19)
k_DHS$m19i <- k_DHS$m19/1000

# add decimal place into weight:
plot(k_DHS$hw2) # first replace flagged obs with NA
k_DHS$hw2 <- ifelse(k_DHS$hw2 > 900, NA, k_DHS$hw2)
k_DHS$hw2i <- k_DHS$hw2/10

# add decimal place into height:
plot(k_DHS$hw3) # first replace flagged obs with NA
k_DHS$hw3 <- ifelse(k_DHS$hw3 > 9000, NA, k_DHS$hw3)
k_DHS$hw3i <-k_DHS$hw3/10

# rename all variables that may be needed for further analyses:
k_DHS_b<- k_DHS%>% 
  dplyr::rename(country_phase = v000,
                DHSCLUST_n = v001,
                hh_number = v002,
                respondent_line_no = v003,
                ult_area_unit = v004,
                wt = v005i,
                month = v006,                # month of interview
                year = v007,                 # year of interview
                interview_result = v015,
                strat = v023,
                region = v024,
                urban_rural = v025,
                usual_resident = v135,
                number_children0_5 = v137,   # de jure (usual resident)
                wealth_grouped = v190,
                wealth_index = v191i,
                birth_id = bidx,          # birth order of child present during survey (1 = youngest)
                birth_order = bord,       # birth order relative to mother's birth history (1 = oldest; includes children who have died)
                twin = b0,
                birth_month = b1,
                birth_year = b2,
                child_gender = b4,
                child_alive = b5,
                child_age_years = b8,
                child_age_months = hw1, #Age of child in months for children of respondents for whom anthropometric measures were taken
                child_age_months_all = b19,
                child_lives_with = b9,
                child_line_number = b16,
                size_at_birth = m18,
                birth_weight_kg = m19i,
                mum_breastfeeding = v404,
                child_breastfed = m4,      # 95 = exclusively breastfed
                child_milk = v411,
                child_formula = v411a,     # gave child baby formula
                child_babyfood = v412a,
                child_soup = v412c,
                child_grains = v414e,      # includes bread/noodles
                child_starchveg = v414f,
                child_eggs = v414g, # eggs 
                child_meat = v414h,# (beef, pork, lamb, chicken, etc.)
                child_processedmeat = v414b, #sausages, hot dogs, frankfurters, ham, bacon, salami, canned food
                child_yellowveg = v414i,
                child_greenveg = v414j,
                child_otherveg = v414a,
                child_vitAfruit = v414k,
                child_otherfruit = v414l,
                child_offal = v414m,#liver, heart, other organs
                child_fish = v414n,          # including shellfish
                child_pulses = v414o,
                # child_nutsseeds = v414c,
                child_dairy = v414p,
                child_yogurt = v414v,
                child_bottle = v415,
                child_weight_kg = hw2i,
                child_height_cm = hw3i,
                child_cough=h31)





# Keep only variables above (plus "caseid")

df_k2 <- k_DHS_b[,c("caseid", "DHSCLUST","country_phase","hh_number","respondent_line_no", "month", "birth_id",
                    "ult_area_unit","wt","month","year","interview_result","strat",
                    "region","urban_rural","usual_resident","number_children0_5",
                    "wealth_grouped","wealth_index","birth_id","birth_order","twin",
                    "birth_month","birth_year","child_gender","child_alive",
                    "child_age_years","child_age_months", "child_age_months_all","child_lives_with",
                    "child_line_number","size_at_birth","birth_weight_kg",
                    "mum_breastfeeding","child_breastfed","child_milk","child_formula","child_babyfood",
                    "child_soup","child_grains","child_starchveg","child_eggs",
                    "child_meat","child_processedmeat","child_offal","child_yellowveg",
                    "child_greenveg","child_otherveg","child_vitAfruit","child_otherfruit",
                    "child_fish","child_pulses","child_dairy",
                    "child_yogurt","child_bottle","child_weight_kg","child_height_cm", "child_cough")]
glimpse(df_k2)

# add country & year info to cluster number:

# extract first 2 characters of country_phase and paste before DHSCLUST
# add year after DHSCLUST
#df_k2$DHSCLUST = paste(substr(df_k2$country_phase, start = 1, stop = 2), df_k2$DHSCLUST, df_k2$year, sep="_")


##add women id (same as case ID but with country_year)

df_k2$women_id <- paste(df_k2$DHSCLUST, df_k2$hh_number,df_k2$respondent_line_no , sep = "_")


###add women data###
SN_women_2018<- readRDS("DHS/DHS Senegal/SN_2018_CONTINUOUSDHS/Women_SNIR81FL.rds")

SN_women_2018 $DHSCLUST = paste(substr(SN_women_2018$v000, start = 1, stop = 2), SN_women_2018$v001, SN_women_2018$v007, sep="_")

SN_women_2019<- readRDS("DHS/DHS Senegal/SN_2019_CONTINUOUSDHS/Women_SNIR8BFL.rds")
SN_women_2019 $DHSCLUST = paste(substr(SN_women_2019$v000, start = 1, stop = 2), SN_women_2019$v001, SN_women_2019$v007, sep="_")

GM_women<- readRDS("DHS/DHS Gambia/Women_GMIR81FL.rds")

GM_women$DHSCLUST = paste("GM", GM_women$v001,"2019_2020", sep="_")

MR_women<- readRDS("DHS/DHS Mauritania/Women_MRIR71FL.rds")

MR_women$DHSCLUST = paste("MR", MR_women$v001,"2019_2021", sep="_")

w_DHS <- rbind.fill(SN_women_2018,SN_women_2019,GM_women,MR_women)


dim(w_DHS)  # 45642  5893


# Remove variables that are ALL NA
w_DHS <- w_DHS[,-which(sapply(w_DHS, function(x)all(is.na(x))))]
dim(w_DHS)
#  45642  2928

# add decimal places to women's sample weights:
w_DHS$v005i <- w_DHS$v005/1000000

# add decimal places to wealth index
w_DHS$v191i <- w_DHS$v191/100000


w_codes <- colnames(w_DHS)#extract the names of the columns and store in w_codes


w_DHS  <- w_DHS %>% 
  dplyr::rename(country_phase = v000,
                DHSCLUST_n = v001, #max value is 281
                hh_number = v002, 
                respondent_line_no = v003, #household line number?
                ult_area_unit = v004,
                wt = v005,
                month = v006,               # month of interview
                year = v007,                # year of interview
                year_of_birth = v010,       #respondent´s year of birth
                age = v012,                 #respondent´s age
                interview_result = v015,    #1= completed, 2= not at home, 3= postponed, 4=refused, 5=partially completed, 6=respondent incapacitated, 7=other
                ever_married=v020,          #The ever-married sample indicator is a constant for all cases in the data file.  For all woman samples it is code 0, and for ever-married samples it is code 1. 
                psu = v021,
                strat = v023,               #14 levels
                region = v024,              #8 levels in Gambia: 1)Banjul, 2)Kaninfing, 3)Brikama, 4)Mansakonko,5)Kerewan, 6)Kuntaur, 7)Janjanbureh, 8)Basse
                urban_rural = v025,
                husband_line_no = v034,
                hh_hemoglobin = v042,        # household selected for hemoglobin (0=not selected, 1=selected, na=not applicanble)
                highest_edu = v106,          # (0= no education, 1= primary, 2=secondary, 3=higher, 9=missing)
                highest_edu_years = v107,
                edu_attainment = v149,       # (0= no education, 1= incomplete primary, 2=complete primary, 3=incomplete secondary,4=complete secondary, 5=higher, 9=missing)
                #education in years is v133
                #usual resident or visitor v135
                literacy = v155,
                source_drink_water = v113,
                religion = v130,
                ethnicity = v131,#missing in mauritania
                usual_resident = v135,
                num_child_hh = v137,#Number of children resident in the household and aged 5 and under.  Visiting children are  not included.
                gender_hh_head = v151,
                age_hh_head = v152,
                wealth_grouped = v190,
                wealth_index = v191,
                wealth_indez_urban = v190a, #I am not sure what this is. It is missing in senegal
                bank_account= v170, #this is missing in senegal
                #selfrep_health = v176,       # self-reported health status. Does not exist in Gambia?
                pregnant = v213,
                living_children = v218,
                breastfeeding = v404, #check if it is the same as m4=95
                #duration_breastfeeding = m4,#not in Senegal
                #child_line_no_diet = v400,   # line number of child used for diet questions
                marital_status = v501, #Agricultural categories also include fishermen, foresters and hunters and are not the basis for selection of agricultural/non agricultural workers.  
                women_occup_status= v714,
                women_occup_status_12m=v731,
                women_occupation=v717,
                husband_occu_status=v704a, #Current or last husband or partner worked in the past 7 days or 12 months 
                husband_occupation=v705, #Standardized partner's occupation groups.  Agricultural categories also include fishermen
                husband_educ_yr=v715,
                own_land= v745b
                
  )



w_DHS <- w_DHS[,c("caseid","country_phase","DHSCLUST","hh_number","respondent_line_no","ult_area_unit","wt","month","year","year_of_birth","age","interview_result",
                  "psu", "strat","region","urban_rural" ,"husband_line_no" ,"hh_hemoglobin" ,"highest_edu" ,"highest_edu_years","edu_attainment","literacy" ,
                  "source_drink_water","religion","ethnicity","usual_resident","num_child_hh", "gender_hh_head","age_hh_head","wealth_grouped","wealth_index","wealth_indez_urban", 
                  "bank_account","pregnant","living_children","breastfeeding", "marital_status",
                  "women_occup_status","women_occup_status_12m","women_occupation","husband_occu_status",
                  "husband_occupation",
                  "husband_educ_yr",
                  "own_land"  )]


##add women id (same as case ID but with country_year)

w_DHS $women_id <- paste(w_DHS $DHSCLUST, w_DHS $hh_number,w_DHS $respondent_line_no , sep = "_")


#merge children and women data

w_k_DHS<-merge(w_DHS, df_k2, by="women_id", all.x=TRUE)


#create a country variable

w_k_DHS<-w_k_DHS %>%
  mutate(
    country = case_when(
      startsWith(country_phase.x, "SN7") ~ "Senegal",
      startsWith(country_phase.x, "GM7") ~ "Gambia",
      startsWith(country_phase.x, "MR7") ~ "Mauritania",
      TRUE ~ NA_character_  # optional: NA for anything else
    )
  )



#Combine gambia years and Mauritania years (only one survey was done in Mauritania an gambia)


w_k_DHS<- w_k_DHS%>%
  mutate(year_2 = case_when(
    country == "Mauritania" & year.x %in% c(2019, 2020,2021) ~ "2019_2020_2021",  # Combine Mauritania's years
    country == "Gambia" & year.x %in% c(2019, 2020) ~ "2019_2020",      # Combine Gambia's years
    country == "Senegal" ~ as.character(year.x),  # Keep Senegal's years separate
    TRUE ~ NA_character_  # For any other countries or unexpected cases, set to NA
  ))


w_k_DHS$year_2_fact<-as.factor(w_k_DHS$year_2)

w_k_DHS$country_year<- paste(w_k_DHS$country, w_k_DHS$year_2, sep = "_")



w_k_DHS$DHSCLUST<-w_k_DHS$DHSCLUST.x


#include distante to markets and waterbodies


load("C:/Users/crist/OneDrive - Lancaster University/Lancaster University_RuanoChamorro/Postdoc project/DHS/Eva distanc/Eva distance V2/distance_waterbodies.RData")
#View(distance_waterbodies)

markets<-read_csv("DHS/Eva distanc/Eva distance V2/Res_tt_NearMarket.csv")


#merge datasets


w_k_DHS_2018_2020<-merge(w_k_DHS, distance_waterbodies, by="DHSCLUST", all.x=TRUE)

w_k_DHS_2018_2020<-merge(w_k_DHS_2018_2020,markets , by="DHSCLUST", all.x=TRUE)


#eliminate rows where caseid.y is NA (there is no data regarding this children if case.y is NA. There are more women than children)

w_k_DHS_2018_2020 <- w_k_DHS_2018_2020 %>%
  filter(!is.na(caseid.y))


# Data now contains variables of interest for all countries and years.

# Save combined dataset:
#saveRDS(df_k2, "DHS Senegal/DHS-Kids_ALL_2014-2023_crc_24.3.25.rds")

#write.csv(df_k2, "DHS/DHS Senegal/DHS-Kids_ALL_2014-2023_crc_24.3.25.csv", row.names = FALSE)

#saveRDS(DHS_WA_2018_2021_April_25, "DHS/DHS_WA_2018_2021_15_April_25.rds")
# updated 2/4/25

########## How important is fish consumption ####

colnames(w_k_DHS_2018_2020)


#eliminate children who are not alive: child_alive

w_k_DHS_2018_2020 <- w_k_DHS_2018_2020 %>%
  filter(child_alive != 0)

#keep children between 6-23 months:child_age_months_all

w_k_DHS_2018_2020$child_age_months_all

w_k_DHS_2018_2020<- w_k_DHS_2018_2020 %>%
  filter(child_age_months_all >= 6 & child_age_months_all <= 23)

#####Diet diversity in children#####

#Minimum diet diversity = consumption of 5 out of 8 food groups during the preceding day or night of the survey
# a) grains, roots and tubers (v412a or v414e or v414f)
#child_starchveg = v414f
w_k_DHS_2018_2020$child_starchveg
sum(w_k_DHS_2018_2020$child_starchveg == 8,na.rm = TRUE)#21

#child_grains = v414e
sum(w_k_DHS_2018_2020$child_grains == 8,na.rm = TRUE)#21
#child_babyfood
w_k_DHS_2018_2020$child_babyfood
sum(w_k_DHS_2018_2020$child_babyfood == 8,na.rm = TRUE)


# b) Legumes and nuts (v414o  v414c)
#child_pulses= v414o
sum(w_k_DHS_2018_2020$child_pulses == 8,na.rm = TRUE)#21

# c) Dairy products (milk, yogurt, eggs) (v411 = 1 or v411a = 1 or v414v = 1 or v414p = 1)
#child_milk = v411
sum(w_k_DHS_2018_2020$child_milk == 8,na.rm = TRUE)#25

#child_formula = v411a
sum(w_k_DHS_2018_2020$child_formula == 8,na.rm = TRUE)#17
#child_dairy = v414p
sum(w_k_DHS_2018_2020$child_dairy == 8,na.rm = TRUE)#18
#child_yogurt = v414v
sum(w_k_DHS_2018_2020$child_yogurt == 8,na.rm = TRUE)#20

# d) Flesh foods (meat, fish, poultry and liver/organ meats) (v414h = 1 or v414m = 1 or v414n =1,  v414b)

#child_offal = v414m,#liver, heart, other organs
sum(w_k_DHS_2018_2020$child_offal== 8, na.rm = TRUE)#18
#child_fish = v414n,          # including shellfish
sum(w_k_DHS_2018_2020$child_fish== 8, na.rm = TRUE)#19
#child_meat = v414h,# (beef, pork, lamb, chicken, etc.)
sum(w_k_DHS_2018_2020$child_meat== 8,na.rm = TRUE)#22
#child_processedmeat = v414b, #sausages, hot dogs, frankfurters, ham, bacon, salami, canned food: not included?
sum(w_k_DHS_2018_2020$child_processedmeat== 8, na.rm = TRUE)#10

# e) eggs   (v414g = 1) 
#child_eggs = v414g, # eggs 
sum(w_k_DHS_2018_2020$child_eggs== 8,na.rm = TRUE)#18

# f) vitamin A rich fruits and vegetables (v414i = 1 or v414j = 1 or v414k = 1) 
#child_yellowveg = v414i
sum(w_k_DHS_2018_2020$child_yellowveg== 8,na.rm = TRUE)#0

#child_greenveg = v414j
sum(w_k_DHS_2018_2020$child_greenveg== 8,na.rm = TRUE)#20

#child_vitAfruit = v414k
sum(w_k_DHS_2018_2020$child_vitAfruit== 8,na.rm = TRUE)#19


# g) other fruits and veg (v414l , v414a (included in  DHS 8) 
#child_otherfruit = v414l
sum(w_k_DHS_2018_2020$child_otherfruit== 8,na.rm = TRUE)#20

#child_otherveg = v414a
sum(w_k_DHS_2018_2020$child_otherveg== 8,na.rm = TRUE)#9

# h) breastmilk
#child_breastfed = m4,      # 95 = exclusively breastfed
sum(w_k_DHS_2018_2020$child_breastfed== 8,na.rm = TRUE)#0


#eliminate don´t knows (8)


#eliminate if the value is 8 in any of the previous columns

sum((is.na(w_k_DHS_2018_2020$child_starchveg)))#82
sum((is.na(w_k_DHS_2018_2020$child_grains)))#82
sum((is.na(w_k_DHS_2018_2020$child_babyfood)))#82
sum((is.na(w_k_DHS_2018_2020$child_pulses)))#82
sum((is.na(w_k_DHS_2018_2020$child_milk)))#82
sum((is.na(w_k_DHS_2018_2020$child_formula)))#82
sum((is.na(w_k_DHS_2018_2020$child_dairy)))#82
sum((is.na(w_k_DHS_2018_2020$child_yogurt)))#82
sum((is.na(w_k_DHS_2018_2020$child_offal)))#82
sum((is.na(w_k_DHS_2018_2020$child_fish)))#82
sum((is.na(w_k_DHS_2018_2020$child_meat)))#82
sum((is.na(w_k_DHS_2018_2020$child_processedmeat)))#5669, exclude
sum(is.na(w_k_DHS_2018_2020$child_eggs))#82
sum((is.na(w_k_DHS_2018_2020$child_yellowveg)))#82
sum((is.na(w_k_DHS_2018_2020$child_greenveg)))#82
sum((is.na(w_k_DHS_2018_2020$child_vitAfruit)))#82
sum((is.na(w_k_DHS_2018_2020$child_otherfruit)))#82
sum((is.na(w_k_DHS_2018_2020$child_otherveg)))# 5669, exclude
sum((is.na(w_k_DHS_2018_2020$child_breastfed)))# 0



w_k_DHS_2018_2020<-filter(w_k_DHS_2018_2020,child_starchveg !=8)
w_k_DHS_2018_2020<-filter(w_k_DHS_2018_2020,child_grains !=8)
w_k_DHS_2018_2020<-filter(w_k_DHS_2018_2020,child_babyfood !=8)
w_k_DHS_2018_2020<-filter(w_k_DHS_2018_2020,child_pulses !=8)
w_k_DHS_2018_2020<-filter(w_k_DHS_2018_2020,child_milk !=8)
w_k_DHS_2018_2020<-filter(w_k_DHS_2018_2020,child_formula!=8)
w_k_DHS_2018_2020<-filter(w_k_DHS_2018_2020,child_dairy !=8)
w_k_DHS_2018_2020<-filter(w_k_DHS_2018_2020,child_yogurt !=8)
w_k_DHS_2018_2020<-filter(w_k_DHS_2018_2020,child_offal !=8)#eliminate value 8 (don´t know)
w_k_DHS_2018_2020<-filter(w_k_DHS_2018_2020,child_fish!=8)#eliminate value 8 (don´t know)
w_k_DHS_2018_2020<-filter(w_k_DHS_2018_2020,child_meat!=8)#eliminate value 8 (don´t know)
#w_k_DHS_2018_2020<-filter(w_k_DHS_2018_2020,child_processedmeat!=8)#eliminate value 8 (don´t know)
w_k_DHS_2018_2020<-filter(w_k_DHS_2018_2020,child_eggs!=8)#eliminate value 8 (don´t know)
w_k_DHS_2018_2020<-filter(w_k_DHS_2018_2020,child_yellowveg!=8)
w_k_DHS_2018_2020<-filter(w_k_DHS_2018_2020,child_greenveg!=8)
w_k_DHS_2018_2020<-filter(w_k_DHS_2018_2020,child_vitAfruit!=8)
w_k_DHS_2018_2020<-filter(w_k_DHS_2018_2020,child_otherfruit!=8)
# w_k_DHS_2018_2020<-filter(w_k_DHS_2018_2020,child_otherveg!=8)

w_k_DHS_2018_2020<-filter(w_k_DHS_2018_2020,child_breastfed!=98)
w_k_DHS_2018_2020$child_breastfed

w_k_DHS_2018_2020$child_breastfed_fact<-as.factor(w_k_DHS_2018_2020$child_breastfed)

ggplot(w_k_DHS_2018_2020,aes(child_breastfed_fact))+ #93 = 0, 94=0, 95=1
  geom_bar()

#transform breastfeeding into a binomial variable
w_k_DHS_2018_2020$child_breastfed_fact_2 <- ifelse(
  w_k_DHS_2018_2020$child_breastfed_fact== "95",1,0)

#convert to numeric

w_k_DHS_2018_2020$child_breastfed_num_2<-as.numeric(w_k_DHS_2018_2020$child_breastfed_fact_2)

#after eliminating don´t knows there are not NAs


#build food groups

#grains, roots and tubers
w_k_DHS_2018_2020$child_starchveg_fact<- as.factor(w_k_DHS_2018_2020$child_starchveg)
w_k_DHS_2018_2020$child_grains_fact<- as.factor(w_k_DHS_2018_2020$child_grains)
w_k_DHS_2018_2020$child_babyfood_fact<- as.factor(w_k_DHS_2018_2020$child_babyfood)

w_k_DHS_2018_2020$child_grains_starch<- ifelse(
  w_k_DHS_2018_2020$child_starchveg == 1 | 
    w_k_DHS_2018_2020$child_babyfood == 1 |
    w_k_DHS_2018_2020$child_grains == 1,1, 0)

#check there is 1 column
w_k_DHS_2018_2020_a<- w_k_DHS_2018_2020%>%
  dplyr::select(women_id, child_grains_starch, child_starchveg, child_grains, child_babyfood)%>%
  filter(women_id == "GM_1_2019_2020_18_2")

# Legumes and nuts = child_pulses


# c) Dairy products (milk, yogurt, eggs) (v411 = 1 or v411a = 1 or v414v = 1 or v414p = 1)
#child_milk = v411

w_k_DHS_2018_2020$child_all_dairy<- ifelse(
  w_k_DHS_2018_2020$child_milk == 1 |
    w_k_DHS_2018_2020$child_formula == 1 | 
    w_k_DHS_2018_2020$child_dairy== 1 | 
    w_k_DHS_2018_2020$child_yogurt == 1,1, 0)



# d) Flesh foods (meat, fish, poultry and liver/organ meats) (v414h = 1 or v414m = 1 or v414n =1,  v414b)

w_k_DHS_2018_2020$child_flesh<- ifelse(
  w_k_DHS_2018_2020$child_offal == 1 |
    w_k_DHS_2018_2020$child_fish == 1 | 
    w_k_DHS_2018_2020$child_meat == 1,1, 0)


# e) eggs  = child_eggs 


# f) vitamin A rich fruits and vegetables (v414i = 1 or v414j = 1 or v414k = 1) 

w_k_DHS_2018_2020$child_vit_A<- ifelse(
  w_k_DHS_2018_2020$child_yellowveg == 1 |
    w_k_DHS_2018_2020$child_greenveg == 1 | 
    w_k_DHS_2018_2020$child_vitAfruit == 1,1, 0)



# g) other fruits and veg = child_otherfruit


# h) breastmilk = child_breastfed_fact2



w_k_DHS_2018_2020%>%
  dplyr::select(women_id, DHSCLUST,hh_number.x,
                child_grains_starch, 
                child_all_dairy,
                child_pulses,
                child_flesh,
                child_eggs,
                child_vit_A,
                child_otherfruit,
                child_breastfed_num_2
  )



w_k_DHS_2018_2020$diet_diversity <- rowSums(w_k_DHS_2018_2020[, #consider breasteeding
                                                              c("child_grains_starch", 
                                                                "child_all_dairy", 
                                                                "child_pulses",
                                                                "child_flesh",
                                                                "child_eggs",
                                                                "child_vit_A",
                                                                "child_otherfruit",
                                                                "child_breastfed_fact_2"
                                                              )], na.rm = TRUE)

###pivot_longer

w_k_DHS_2018_2020_long <- pivot_longer(
  w_k_DHS_2018_2020,
  cols = c(
    child_grains_starch, 
    child_all_dairy, 
    child_pulses,
    child_flesh,
    child_eggs,
    child_vit_A,
    child_otherfruit,
    child_breastfed_fact_2,
    child_fish
  ),
  
  names_to = "food_item",
  values_to = "Consumption"
)

w_k_DHS_2018_2020_long<-filter(w_k_DHS_2018_2020_long,Consumption!=8)

w_k_DHS_2018_2020_long%>%
  dplyr::select(women_id, food_item, Consumption)


w_k_DHS_2018_2020_long$Consumption_fact<-as.factor(w_k_DHS_2018_2020_long$Consumption)

desired_order <- c("child_breastfed_fact_2",
                   "child_grains_starch", 
                   "child_all_dairy", 
                   "child_flesh",
                   "child_vit_A",
                   "child_pulses",
                   "child_otherfruit",
                   "child_eggs")

# Reorder the factor levels in the data frame:
w_k_DHS_2018_2020_long$food_item <- factor(w_k_DHS_2018_2020_long$food_item, levels = desired_order)


w_k_DHS_2018_2020_long$food_item <- fct_recode(w_k_DHS_2018_2020_long$food_item,
                                               Breastfeed = "child_breastfed_fact_2",
                                               Grains_starch = "child_grains_starch",
                                               Dairy = "child_all_dairy", 
                                               Flesh_foods = "child_flesh",
                                               Vitamine_A_rich_FV = "child_vit_A",
                                               Legumes="child_pulses",
                                               Other_FV="child_otherfruit",
                                               Eggs= "child_eggs")

DHSdesign<-svydesign(id=~DHSCLUST, strata=~strat.y, weights=~wt.y, data=w_k_DHS_2018_2020_long)

table1<-svytable(~ Consumption_fact+ food_item,design=DHSdesign)%>%
  as.data.frame()


ggplot(table1, aes(x=food_item,y=Freq,
                   fill=Consumption_fact))+geom_bar(stat="identity",position = "fill")

# Calculate percentage by total
table_2 <- table1 %>%
  mutate(percent = Freq / sum(Freq) * 100)

# Or calculate percentage within each food_item
table3 <- table2 %>%
  dplyr::group_by(food_item) %>%
  dplyr::mutate(percent_within_food = Freq / sum(Freq) * 100) %>%
  ungroup()


#Min diet diveristy: 5 out of 8 if we consider breastfeeding is the min diet diversity, 4 if we do not consider breastfeeding

w_k_DHS_2018_2020$diet_diversity_min<-ifelse(#5 is min diet diversity, because it includes breastfeeding
  w_k_DHS_2018_2020$diet_diversity>4, 1,0)

w_k_DHS_2018_2020%>%
  dplyr::select(women_id, diet_diversity_min, diet_diversity)

w_k_DHS_2018_2020$diet_diversity_min_fact<-as.factor(w_k_DHS_2018_2020$diet_diversity_min)


#weights

DHSdesign<-svydesign(id=~DHSCLUST, strata=~strat.y, weights=~wt.y, data=w_k_DHS_2018_2020)

table1<-svytable(~ diet_diversity_min_fact, design=DHSdesign)%>%
  as.data.frame()

table1 <- table1 %>%
  mutate(Percent = Freq / sum(Freq) * 100)

ggplot(table1, aes(x = diet_diversity_min_fact, y = Percent)) +
  geom_bar(stat = "identity") +
  labs(x = "Minimum Diet Diversity", y = "Percentage", 
       title = "Diet Diversity (Weighted Percentages)") +
  theme_minimal()

ggplot(table1, aes(x = diet_diversity_min_fact, y = Percent)) +
  geom_bar(stat = "identity") +
  labs(x = "Minimum Diet Diversity", y = "Percentage") +
  theme_minimal()

ggplot(table1, aes(x = factor(diet_diversity_min_fact, levels = c(0, 1), labels = c("No", "Yes")), 
                   y = Percent)) +
  geom_bar(stat = "identity", fill = "grey") +  # white bars with black outline
  labs(x = "Minimum Diet Diversity", y = "Percentage", 
       title = "Children's (6-23 months)") +
  theme_classic(base_size = 16) +  # larger base font size
  theme(
    #panel.background = element_rect(fill = "transparent", color = NA),
    # plot.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_text(size = 18),     # larger tick labels
    axis.title = element_text(size = 20, face = "bold"),  # larger axis titles
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5)  # centered and bold title
  )

#select those with min diet diveristy

w_k_DHS_2018_2020$diet_diversity_min_fact<-as.factor(w_k_DHS_2018_2020$diet_diversity_min)

w_k_DHS_2018_2020_mdd1 <- subset(w_k_DHS_2018_2020,diet_diversity_min_fact == "1")

w_k_DHS_2018_2020_mdd1$diet_diversity_min_fact <- droplevels(w_k_DHS_2018_2020_mdd1$diet_diversity_min_fact)


###pivot_longer for only those with min dd

w_k_DHS_2018_2020_long <- pivot_longer(
  w_k_DHS_2018_2020_mdd1,
  cols = c(
    child_grains_starch, 
    child_all_dairy, 
    child_pulses,
    child_flesh,
    child_eggs,
    child_vit_A,
    child_otherfruit,
    child_breastfed_fact_2,
    child_fish
  ),
  
  names_to = "food_item",
  values_to = "Consumption"
)

w_k_DHS_2018_2020_long<-filter(w_k_DHS_2018_2020_long,Consumption!=8)

w_k_DHS_2018_2020_long%>%
  dplyr::select(women_id, food_item, Consumption)


w_k_DHS_2018_2020_long$Consumption_fact<-as.factor(w_k_DHS_2018_2020_long$Consumption)

w_k_DHS_2018_2020_long$food_item<-as.factor(w_k_DHS_2018_2020_long$food_item)

desired_order <- c("child_breastfed_fact_2",
                   "child_grains_starch", 
                   "child_all_dairy", 
                   "child_flesh",
                   "child_fish",
                   "child_vit_A",
                   "child_pulses",
                   "child_otherfruit",
                   "child_eggs")

# Reorder the factor levels in the data frame:
w_k_DHS_2018_2020_long$food_item <- factor(w_k_DHS_2018_2020_long$food_item, levels = desired_order)


w_k_DHS_2018_2020_long$food_item <- fct_recode(w_k_DHS_2018_2020_long$food_item,
                                               Breastfeed = "child_breastfed_fact_2",
                                               Grains_starch = "child_grains_starch",
                                               Dairy = "child_all_dairy", 
                                               Flesh_foods = "child_flesh",
                                               Fish  = "child_fish",
                                               Vitamine_A_rich_FV = "child_vit_A",
                                               Legumes="child_pulses",
                                               Other_FV="child_otherfruit",
                                               Eggs= "child_eggs")


#weights

DHSdesign<-svydesign(id=~DHSCLUST, strata=~strat.y, weights=~wt.y, data=w_k_DHS_2018_2020_long)

table1<-svytable(~ food_item + Consumption_fact, design=DHSdesign)%>%
  as.data.frame()

table1 <- table1 %>%
  mutate(Percent = Freq / sum(Freq) * 100)

table3 <- table1 %>%
  dplyr::group_by(food_item) %>%
  dplyr::mutate(percent_within_food = Freq / sum(Freq) * 100) %>%
  ungroup()


ggplot(table1, aes(x=food_item,y=Freq,
                   fill=Consumption_fact))+geom_bar(stat="identity",position = "fill")+
                   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                   labs(x = "Food Item", y = "Proportion", fill = "Consumption")



# Suppose you want this order:
desired_order <- c("child_breastfed_fact_2",
                   "child_grains_starch", 
                   "child_all_dairy", 
                   "child_flesh",
                   "child_vit_A",
                   "child_pulses",
                   "child_otherfruit",
                   "child_eggs")

# Reorder the factor levels in your data frame:

w_k_DHS_2018_2020_long_flesh<- subset(w_k_DHS_2018_2020_long, food_item != "child_fish")

w_k_DHS_2018_2020_long_flesh$food_item <- factor(w_k_DHS_2018_2020_long_flesh$food_item, levels = desired_order)


w_k_DHS_2018_2020_long_flesh$food_item <- fct_recode(w_k_DHS_2018_2020_long_flesh$food_item,
                                                     Breastfeed = "child_breastfed_fact_2",
                                                     Grains_starch = "child_grains_starch",
                                                     Dairy = "child_all_dairy", 
                                                     Flesh_foods = "child_flesh",
                                                     Vitamine_A_rich_FV = "child_vit_A",
                                                     Legumes="child_pulses",
                                                     Other_FV="child_otherfruit",
                                                     Eggs= "child_eggs")

ggplot(w_k_DHS_2018_2020_long_flesh, aes(x = food_item, fill = Consumption_fact)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Food Item", y = "Proportion", fill = "Consumption Fact")





