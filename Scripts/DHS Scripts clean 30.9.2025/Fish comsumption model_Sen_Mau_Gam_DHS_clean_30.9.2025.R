# Children - Combine all DHS datasets
#version with respiratory info

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
View(distance_waterbodies)


markets<-read_csv("DHS/Eva distanc/Eva distance V2/Res_tt_NearMarket.csv")




#merge datasets


w_k_DHS_2018_2020<-merge(w_k_DHS, distance_waterbodies, by="DHSCLUST", all.x=TRUE)

w_k_DHS_2018_2020<-merge(w_k_DHS_2018_2020,markets , by="DHSCLUST", all.x=TRUE)


#eliminate rows where caseid.y is NA (there is no data regarding this children if case.y is NA. There are more women than children)

w_k_DHS_2018_2020 <- w_k_DHS_2018_2020 %>%
  filter(!is.na(caseid.y))


# Data now contains variables of interest for all countries and years.

#eliminate years 2014-2017 and 2023 

pattern <- "^SN.*(2014|2015|2016|2017|2023)$"
# Subset to exclude matching categories
w_k_DHS_2018_2020<- subset(w_k_DHS_2018_2020, !grepl(pattern, DHSCLUST))

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


###Consumption####
#ASF consumption: child_eggs, child_meat (beef, pork, lamb, chicken, etc.) child_processed meat (sausages, hot dogs, frankfurters, ham, bacon, salami, canned food)
#child_offal (liver, heart, other organs), child_fish 

#eliminate don´t knows (8)

#how many don´t knows

sum(w_k_DHS_2018_2020$child_eggs== 8,na.rm = TRUE)#18

sum(w_k_DHS_2018_2020$child_meat== 8,na.rm = TRUE)#22

sum(w_k_DHS_2018_2020$child_processedmeat== 8, na.rm = TRUE)#10
sum(w_k_DHS_2018_2020$child_offal== 8, na.rm = TRUE)#18

sum(w_k_DHS_2018_2020$child_fish== 8, na.rm = TRUE)#19

#eliminate if the value is 8 in any of the previous columns

w_k_DHS_2018_2020<-filter(w_k_DHS_2018_2020,child_offal !=8)#eliminate value 8 (don´t know)
w_k_DHS_2018_2020<-filter(w_k_DHS_2018_2020,child_eggs!=8)#eliminate value 8 (don´t know)
w_k_DHS_2018_2020<-filter(w_k_DHS_2018_2020,child_meat!=8)#eliminate value 8 (don´t know)
w_k_DHS_2018_2020<-filter(w_k_DHS_2018_2020,child_fish!=8)#eliminate value 8 (don´t know)

sum(is.na(w_k_DHS_2018_2020$child_eggs))#0
sum((is.na(w_k_DHS_2018_2020$child_offal)))#0
sum((is.na(w_k_DHS_2018_2020$child_meat)))#0
sum((is.na(w_k_DHS_2018_2020$child_processedmeat)))#5669. We will exclude this one
sum((is.na(w_k_DHS_2018_2020$child_fish)))#0


#the same rows have NAs, eliminate these rows
#which(is.na(w_k_DHS_2018_2020$child_eggs))#
#which((is.na(DHS_WA_2018_2021_April_25$child_offal)))#
#which((is.na(DHS_WA_2018_2021_April_25$child_meat)))#82

#eliminate  rows with NAs

#DHS_WA_2018_2021_April_25_b <- DHS_WA_2018_2021_April_25_b%>%
#filter(!is.na(child_eggs) & !is.na(child_meat) & !is.na(child_offal) & !is.na(child_fish))

#calculate meat consumption (including fish) and excluding processed meat (thre were too many NAs)

w_k_DHS_2018_2020$child_ASF <- ifelse(
  w_k_DHS_2018_2020$child_eggs == 1 | 
    w_k_DHS_2018_2020$child_meat == 1 | 
    w_k_DHS_2018_2020$child_offal == 1 | 
    w_k_DHS_2018_2020$child_fish == 1, 
  1, 
  0
)


w_k_DHS_2018_2020$child_fish_fact<-as.factor(w_k_DHS_2018_2020$child_fish)


w_k_DHS_2018_2020$child_ASF_fact<-as.factor(w_k_DHS_2018_2020$child_ASF)

w_k_DHS_2018_2020$country_year<-as.factor(w_k_DHS_2018_2020$country_year)


ggplot(w_k_DHS_2018_2020, aes(x = country_year, fill = child_fish_fact)) +
  geom_bar(position = "fill")



ggplot(w_k_DHS_2018_2020, aes(x = country_year, fill = child_ASF_fact)) +
  geom_bar(position = "fill")

nrow(w_k_DHS_2018_2020)

#Plots with weights 

library(survey)

#I have used the weights in each country. check if this is ok!!!

DHSdesign<-svydesign(id=~DHSCLUST, strata=~strat.y, weights=~wt.y, data=w_k_DHS_2018_2020)

table1<-svytable(~ child_fish_fact+ country_year,design=DHSdesign)%>%
  as.data.frame()


ggplot(table1, aes(x=country_year,y=Freq,
                   fill=child_fish_fact))+geom_bar(stat="identity",position = "fill")

table2<-svytable(~ child_ASF_fact+ country_year,design=DHSdesign)%>%
  as.data.frame()


ggplot(table2, aes(x=country_year,y=Freq,
                   fill=child_ASF_fact))+geom_bar(stat="identity",position = "fill")


#inequalities in consumption

# mother education, hh wealth, proximity to markets, proximity to water bodies, geder head hh, ethnicity
#control for child age, child gender,

#Ethnicity
w_k_DHS_2018_2020$ethnicity_fact<-as.factor(w_k_DHS_2018_2020$ethnicity)

w_k_DHS_2018_2020$ethnicity_fact <- as.factor(
  ifelse(w_k_DHS_2018_2020$ethnicity_fact == 1,"Wolof",
         ifelse(w_k_DHS_2018_2020$ethnicity_fact == 2, "Poular",
                ifelse(w_k_DHS_2018_2020$ethnicity_fact == 3, "Serer",
                       ifelse(w_k_DHS_2018_2020$ethnicity_fact == 4, "Mandingue",
                              ifelse(w_k_DHS_2018_2020$ethnicity_fact == 5, "Diola",
                                     ifelse(w_k_DHS_2018_2020$ethnicity_fact == 6, "Soninke", "Other")))))))


ggplot(w_k_DHS_2018_2020, aes(x=ethnicity_fact, fill=country))+
  geom_bar()

#education
# (0= no education, 1= primary, 2=secondary, 3=higher, 9=missing)


w_k_DHS_2018_2020$highest_edu_fact<-as.factor(w_k_DHS_2018_2020$highest_edu)

ggplot(w_k_DHS_2018_2020, aes(x=highest_edu_fact))+
  geom_bar()

w_k_DHS_2018_2020$highest_edu_fact_b <- as.factor(
  ifelse(w_k_DHS_2018_2020$highest_edu_fact== 0,"0",#no education
         ifelse(w_k_DHS_2018_2020$highest_edu_fact == 1, "1",#primary
                ifelse(w_k_DHS_2018_2020$highest_edu_fact == 2, "2",#secondary and higher
                       ifelse(w_k_DHS_2018_2020$highest_edu_fact == 3, "2","6")))))


#there is no ethnicity information in Mauritania


#Fishing_ethnicity

unique(w_k_DHS_2018_2020$ethnicity) # could do wolof/serer (generally fishing communities) & other
table(w_k_DHS_2018_2020$ethnicity)
w_k_DHS_2018_2020$fishing_ethnicity <- ifelse(w_k_DHS_2018_2020$ethnicity==1 | w_k_DHS_2018_2020$ethnicity==3, 1, 0)

w_k_DHS_2018_2020$fishing_ethnicity_fact<-as.factor(w_k_DHS_2018_2020$fishing_ethnicity)


w_k_DHS_2018_2020$wealth_grouped_fact<-as.factor(w_k_DHS_2018_2020$wealth_grouped.x)
w_k_DHS_2018_2020$wealth_index_sc<-scale(w_k_DHS_2018_2020$wealth_index.x)
w_k_DHS_2018_2020$urban_rural<-as.factor(w_k_DHS_2018_2020$urban_rural.x)
w_k_DHS_2018_2020$child_gender<-as.factor(w_k_DHS_2018_2020$child_gender)
w_k_DHS_2018_2020$country_year<-as.factor(w_k_DHS_2018_2020$country_year)
w_k_DHS_2018_2020$year<-as.factor(w_k_DHS_2018_2020$year.x)
w_k_DHS_2018_2020$child_age_months_all_sc<-scale(w_k_DHS_2018_2020$child_age_months_all)#between 6 months and 23
w_k_DHS_2018_2020$highest_edu_fact<-as.factor(w_k_DHS_2018_2020$highest_edu)
w_k_DHS_2018_2020$distance_to_marine_km_sc<- scale(w_k_DHS_2018_2020$distance_to_marine_km)
w_k_DHS_2018_2020$distance_to_markets_sc<- scale(w_k_DHS_2018_2020$tt_NearMarket)
w_k_DHS_2018_2020$proximity_to_water_km_sc<- scale(w_k_DHS_2018_2020$proximity_to_water_km)
w_k_DHS_2018_2020$distance_to_inland_sc<- scale(w_k_DHS_2018_2020$distance_to_inland)
w_k_DHS_2018_2020$gender_hh_head_fact<-as.factor(w_k_DHS_2018_2020$gender_hh_head)
w_k_DHS_2018_2020$marital_status_fact<-as.factor(w_k_DHS_2018_2020$marital_status)#most women with children are married
w_k_DHS_2018_2020$religion_fact<-as.factor(w_k_DHS_2018_2020$religion)
w_k_DHS_2018_2020$age_hh_head_sc<- scale(w_k_DHS_2018_2020$age_hh_head)
w_k_DHS_2018_2020$urban_rural_fact <-as.factor(w_k_DHS_2018_2020$urban_rural)
w_k_DHS_2018_2020$birth_order_fact<-as.factor(w_k_DHS_2018_2020$birth_order)
w_k_DHS_2018_2020$birth_order_sc<-scale(w_k_DHS_2018_2020$birth_order)

ggplot(w_k_DHS_2018_2020,aes(x=urban_rural_fact))+
  geom_bar()


w_k_DHS_2018_2020<- w_k_DHS_2018_2020 %>%
  filter(highest_edu_fact_b != "6")

w_k_DHS_2018_2020$edu_attainment_fact<-as.factor(w_k_DHS_2018_2020$edu_attainment)

ggplot(w_k_DHS_2018_2020, aes(x=edu_attainment_fact))+
  geom_bar()


ggplot(w_k_DHS_2018_2020, aes(x=religion_fact))+ #moslty musleims
  geom_bar()

#combine higher (5) and complete secondary (4): low numbers = secondary education or more
#combinre 2 (complete primary), 3 (incomplete seconcary) = primary education


w_k_DHS_2018_2020$edu_attainment_fact_b <- as.factor(
  ifelse(w_k_DHS_2018_2020$edu_attainment_fact== 0,"0",#no education
         ifelse(w_k_DHS_2018_2020$edu_attainment_fact == 1, "1",#incomplete primary
                ifelse(w_k_DHS_2018_2020$edu_attainment_fact == 2, "2",#complete primary
                       ifelse(w_k_DHS_2018_2020$edu_attainment_fact == 3, "3", "4"))))) # 3 is incomplete secondary, 4 is complete secondary or higher

ggplot(w_k_DHS_2018_2020, aes(x=edu_attainment_fact_b))+
  geom_bar()


w_k_DHS_2018_2020$edu_attainment_fact_c <- as.factor(
  ifelse(w_k_DHS_2018_2020$edu_attainment_fact== 0,"0",#no education
         ifelse(w_k_DHS_2018_2020$edu_attainment_fact == 1, "1",# 1 is incomplete primary and complete primary
                ifelse(w_k_DHS_2018_2020$edu_attainment_fact == 2, "1", "2"))))# 2 is beyond primary


ggplot(w_k_DHS_2018_2020, aes(x=marital_status_fact))+ #moslty musleims
  geom_bar()

#check these variables

#women_occup_status= v714,
#women_occup_status_12m=v731,
#women_occupation=v717,
#husband_occup_status=v704a, #Current or last husband or partner worked in the past 7 days or 12 months 
#husband_occupation=v705, #Standardized partner's occupation groups.  Agricultural categories also include fishermen
#husband_educ_yr=v715,
#own_land= v745b


w_k_DHS_2018_2020$women_occup_status_fact<-as.factor(w_k_DHS_2018_2020$women_occup_status)#yes,no
w_k_DHS_2018_2020$women_occup_status_12m
w_k_DHS_2018_2020$women_occupation_fact<-as.factor(w_k_DHS_2018_2020$women_occupation)

ggplot(w_k_DHS_2018_2020, aes(x=women_occupation_fact))+
  geom_bar()

w_k_DHS_2018_2020$women_occupation_fact_b <- as.factor(
  ifelse(w_k_DHS_2018_2020$women_occupation_fact== 0,"not working",
         ifelse(w_k_DHS_2018_2020$women_occupation_fact == 3, "sales",
                ifelse(w_k_DHS_2018_2020$women_occupation_fact == 4, "agricultural",
                       ifelse(w_k_DHS_2018_2020$women_occupation_fact == 5, "agricultural","other"))))) 


ggplot(w_k_DHS_2018_2020, aes(x=women_occupation_fact_b))+
  geom_bar()

w_k_DHS_2018_2020$women_occupation_fact_c <- as.factor(
  ifelse(w_k_DHS_2018_2020$women_occupation_fact== 0,"not working",
         ifelse(w_k_DHS_2018_2020$women_occupation_fact == 4, "agricultural",
                ifelse(w_k_DHS_2018_2020$women_occupation_fact == 5, "agricultural","other"))))


w_k_DHS_2018_2020$husband_occu_status_fact<-as.factor( w_k_DHS_2018_2020$husband_occu_status)
ggplot(w_k_DHS_2018_2020, aes(husband_occu_status_fact))+
  geom_bar()




w_k_DHS_2018_2020$husband_occupation_fact<-as.factor(w_k_DHS_2018_2020$husband_occupation)
ggplot(w_k_DHS_2018_2020, aes(husband_occupation_fact))+
  geom_bar()

w_k_DHS_2018_2020$husband_educ_yr_fact<-as.factor(w_k_DHS_2018_2020$husband_educ_yr)
ggplot(w_k_DHS_2018_2020, aes(husband_educ_yr_fact))+
  geom_bar() #majority no education, many don´t knows


w_k_DHS_2018_2020$own_land_fact<-as.factor(w_k_DHS_2018_2020$own_land)
ggplot(w_k_DHS_2018_2020, aes(own_land_fact))+
  geom_bar()

w_k_DHS_2018_2020$own_land_fact_bi <- as.factor(
  ifelse(w_k_DHS_2018_2020$own_land_fact== 0,"0","1"))

ggplot(w_k_DHS_2018_2020, aes(own_land_fact_bi))+
  geom_bar()

w_k_DHS_2018_2020$bank_account_fact<-as.factor(w_k_DHS_2018_2020$bank_account)

ggplot(w_k_DHS_2018_2020, aes(bank_account_fact))+ #very few people with bank accounts
  geom_bar()

#literacy

w_k_DHS_2018_2020$literacy_fact<-as.factor(w_k_DHS_2018_2020$literacy)

ggplot(w_k_DHS_2018_2020, aes(literacy_fact))+
  geom_bar()

w_k_DHS_2018_2020 <-  w_k_DHS_2018_2020 %>% #I have removed 3 ="no card with required language" and 4= "blind/visually impaired"
  filter(!literacy_fact%in% c("3", "4"))

w_k_DHS_2018_2020$literacy_fact_b <- as.factor(
  ifelse(w_k_DHS_2018_2020$literacy_fact== 0,"0",#cannot read at all
         ifelse(w_k_DHS_2018_2020$literacy_fact == 1, "1","2")))# 1= able to read only parts of the sentence and 2= able to read a whole sentence

w_k_DHS_2018_2020$literacy_fact_c <- as.factor(
  ifelse(w_k_DHS_2018_2020$literacy_fact== 0,"0",
         ifelse(w_k_DHS_2018_2020$literacy_fact== 1,"0", "2")))# 0 = cannot read at all, or can read only parts of the sentence

w_k_DHS_2018_2020$literacy_fact_d<- as.factor(
  ifelse(w_k_DHS_2018_2020$literacy_fact== 0,"0","1"))

ggplot(w_k_DHS_2018_2020, aes(literacy_fact_b))+
  geom_bar()

ggplot(w_k_DHS_2018_2020, aes(literacy_fact_c))+
  geom_bar()
ggplot(w_k_DHS_2018_2020, aes(literacy_fact_d))+ #0 = cannot read at all, 1= can read parts of sentences or whole sentences
  geom_bar()



ggplot(w_k_DHS_2018_2020, aes(x = literacy_fact_d, fill = child_fish_fact)) +
  geom_bar(position = "fill") +
  theme_minimal()



##model

DHSdesign<-svydesign(id=~DHSCLUST, strata=~strat.y, weights=~wt.y, data=w_k_DHS_2018_2020)

child_fish_model_d<-svyglm(child_fish ~wealth_grouped_fact + 
                             #women_occupation_fact_c+
                             #age_hh_head_sc+
                             urban_rural_fact+
                             #literacy_fact_b+
                             women_occup_status_fact+ 
                             #highest_edu_fact_b+
                             #edu_attainment_fact_c+ 
                             #own_land_fact_bi+  
                             distance_to_inland_sc+ 
                             distance_to_marine_km_sc+ 
                             distance_to_markets_sc +
                             #gender_hh_head_fact +
                             child_gender+
                             child_age_months_all_sc+
                             birth_order_sc+
                             country_year, 
                           design= DHSdesign, family=quasibinomial(), data=w_k_DHS_2018_2020)
summary(child_fish_model_d)
plot_model(child_fish_model_d)

#####1.1.Diagnostics#####

pseudo_r2 <- 1 - (child_fish_model_d$deviance / child_fish_model_d$null.deviance)

#Residual analysis

#Pearson residuals
pearson_resid <- residuals(child_fish_model_d, type = "pearson")

#Deviance residuals
deviance_resid <- residuals(child_fish_model_d, type = "deviance")

#Fitted values
fitted_vals <- fitted(child_fish_model_d)

#Residual plot
par(mfrow = c(2, 2))

# Residuals vs fitted values
plot(fitted_vals, pearson_resid, 
     main = "Pearson residuals vs Fitted values",
     xlab = "Fitted values", ylab = "Pearson residuals",
     pch = 16, col = alpha("blue", 0.6))
abline(h = 0, col = "red", lty = 2)
lowess_line <- lowess(fitted_vals, pearson_resid)
lines(lowess_line, col = "red", lwd = 2)

# QQ plot de residuos
qqnorm(pearson_resid, main = "Q-Q Plot Pearson residuals")
qqline(pearson_resid, col = "red")

# Histograma de residuos
hist(pearson_resid, breaks = 30, 
     main = "Distribution Pearson residuals",
     xlab = "Pearson residuals", col = "lightblue")

# Residuos vs índice
plot(1:length(pearson_resid), pearson_resid,
     main = "Residuas vs Índice de Observación",
     xlab = "Índice", ylab = "Pearson residuals",
     pch = 16, col = alpha("blue", 0.6))
abline(h = 0, col = "red", lty = 2)

par(mfrow = c(1, 1))

# Check overdispersion estimate
dispersion_value <- summary(child_fish_model_d)$dispersion
print(paste("Dispersion parameter:", round(dispersion_value, 4)))



# # Predict fitted probabilities
predicted_prob <- predict(child_fish_model_d, type = "response")
head(predicted_prob)
# Binned residual plot: residuals vs predicted probabilities
residuals_pearson <- residuals(child_fish_model_d, type = "pearson")
binnedplot(
  x = predicted_prob,
  y = residuals_pearson,
  xlab = "Predicted probabilities",
  ylab = "Pearson residuals",
  main = "Binned Residual Plot"
)


car::dfbetaPlots(child_fish_model_d)

car::vif(child_fish_model_d)



library(broom)
# Tidy the model to extract coefficients and confidence intervals
coef_df <- tidy(child_fish_model_d, conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
# Reorder the terms to match the order in the model specification
coef_df$term <- factor(coef_df$term, levels = c("child_gender2",
                                                "child_age_months_all_sc",
                                                "birth_order_sc",
                                                "country_yearMauritania_2019_2020_2021",
                                                "country_yearSenegal_2018",
                                                "country_yearSenegal_2019",
                                                "distance_to_marine_km_sc",
                                                "distance_to_inland_sc",
                                                "distance_to_markets_sc",
                                                "urban_rural_fact2",
                                                "women_occup_status_fact1",
                                                #"literacy_fact_d1",
                                                "wealth_grouped_fact5",
                                                "wealth_grouped_fact4",
                                                "wealth_grouped_fact3",
                                                "wealth_grouped_fact2"))

coef_df$term <- 
  dplyr::recode(coef_df$term,
                "child_gender2"= "Child gender",
                "child_age_months_all_sc" = "Child age (months)",
                "birth_order_sc" = "Birth order",
                "country_yearMauritania_2019_2020_2021" = "Mauritania",
                "country_yearSenegal_2018" = "Senegal 18",
                "country_yearSenegal_2019" = "Senegal 19",
                "distance_to_marine_km_sc" = "Distance to Ocean",
                "distance_to_inland_sc" = "Distance to inland waterbodies",
                "distance_to_markets_sc" = "Travel Time to Markets",
                "urban_rural_fact2" = "Rural",
                "women_occup_status_fact1" = "Employment status women",
                #"literacy_fact_d1" = "Literacy",
                "wealth_grouped_fact5" = "Wealth Q5",
                "wealth_grouped_fact4" = "Wealth Q4",
                "wealth_grouped_fact3" = "Wealth Q3",
                "wealth_grouped_fact2" = "Wealth Q2")


# Create a new column to indicate positive or negative coefficients
coef_df$coef_color <- ifelse(coef_df$estimate > 0, "#6A4C93", "#4DB6AC")

#Create caterpillar plot with color based on coefficient sign
# Create caterpillar plot with color based on coefficient sign and no legend


ggplot(coef_df, aes(x = term, y = estimate, color = coef_color)) +
  geom_point(size = 3.3,stroke=0) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high),
                 linewidth = 1.4, alpha = 0.95) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8C8C8C", linewidth = 0.6) +
  coord_flip() +
  labs(title = "Children fish consumption",
       subtitle = "(Mauritania, Senegal, The Gambia)",
       tag= "a)",
       y = "Estimate (log-odds)") +
  scale_color_manual(values = c("#6A4C93", "#4DB6AC")) +  # Manually set color scale
  theme_minimal(base_size=12) +
  guides(color = "none") +# Remove the color legend
  theme(
    panel.grid.major.y = element_blank(),
    #panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold",size=16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.tag = element_text(face = "bold", size = 14),
    plot.tag.position = c(0.07, 0.98),
    axis.text.y = element_text(size = 12), 
    axis.text.x = element_text(size = 11),
    axis.title.y = element_blank())+
  scale_x_discrete(expand = expansion(mult = c(0.01, 0.01)))



#extract coef

coefs <- summary(child_fish_model_d)$coefficients

ORs <- exp(coefs[, "Estimate"])

CI_lower <- exp(coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"])
CI_upper <- exp(coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"])

OR_table <- data.frame(
  OR = ORs,
  CI_lower = CI_lower,
  CI_upper = CI_upper,
  p_value = coefs[, "Pr(>|t|)"]
)

