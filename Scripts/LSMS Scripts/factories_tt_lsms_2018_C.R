#Factories data lsms 2018
#Create a variable with min distance to the operational factory in Senegal (according to Greenpeace report 2019)


library(readr)
library(tidyverse)
library(ggplot2)

#set working directory and load data sets

setwd("C:/Users/crist/OneDrive - Lancaster University/Lancaster University_RuanoChamorro/Postdoc project")

factories_lsms_18_b<- read_csv("DHS/Eva distanc/Eva distance LSMS james/Res_tt_AllFactories_Senegal.csv")

#Select factories in Senegal
factories_lsms_18_b_Sen <- factories_lsms_18_b %>%
  select(hh_id, 
         `Sea Production`, 
         `Barna Senegal`, 
         `Afric Azote`, 
         `Senegal Protein`, 
         `Africa Feed`, 
         `Omega Pelagic`, 
         `Omega Fishing`, 
         `Societe Des Produits Halieutiques (Sph Sarl)`, 
         `Beal Meal`, 
         `NDAM (Nouvelles Démarches Avicoles et Maritimes)`)

#option a: include the 6 key factories
#option b: include the factories we think were operating in 2018/2019
#factories (likely) open in 2018/2019: Sea production, Africa Azote, Africa feed,Omega fishing

#select operating factories
factories_lsms_18_b_Sen_b <- factories_lsms_18_b %>%
  select(hh_id, 
         `Sea Production`, 
         `Afric Azote`, 
         `Africa Feed`, 
         `Omega Fishing`)

#replace zeros for NAs because the zeros in the data set are not real zeros, these are NAs

factories_lsms_18_b_Sen_b[] <- lapply(factories_lsms_18_b_Sen_b, function(x) replace(x, x == 0, NA))

#Find minimum travel time.
#It combines all the travel time columns into one long vector and finds the single smallest travel time across all households and all four factories.
min_distance=min(c(factories_lsms_18_b_Sen_b$`Sea Production`, 
                   factories_lsms_18_b_Sen_b$`Afric Azote`, 
                   factories_lsms_18_b_Sen_b$`Africa Feed`, 
                   factories_lsms_18_b_Sen_b$`Omega Fishing`), na.rm = TRUE)


#find nearest factory for each household

factories_lsms_18_b_Sen_b$Min_tt_factories <- #pmin() is a vectorized version of min() that works row by row.It looks across the four factory columns and picks the smallest value for each row.
  pmin(factories_lsms_18_b_Sen_b$'Sea Production', 
       factories_lsms_18_b_Sen_b$'Afric Azote', 
       factories_lsms_18_b_Sen_b$ 'Africa Feed', 
       factories_lsms_18_b_Sen_b$'Omega Fishing')

# Create a new column 'Min_Factory' with the name of the factory that has the minimum distance
factories <- c('Sea Production', 'Afric Azote', 'Africa Feed', 'Omega Fishing')
factories_lsms_18_b_Sen_b$Min_Factory_name <- apply(factories_lsms_18_b_Sen_b[, factories], 1, function(x) factories[which.min(x)])

head(factories_lsms_18_b_Sen_b)

#average distance:

factories_lsms_18_b_Sen_b$mean_Min_tt_factories<- rowMeans(factories_lsms_18_b_Sen_b[, 2:5])#This assumes columns 2 to 5 are all the factory distances. If column order changes or hh_id is missing, it could break.


head(factories_lsms_18_b_Sen_b)

factories_lsms_18_b_Sen_b_C<-factories_lsms_18_b_Sen_b#I have changed the name to differentiate this from previous versions

write.csv(factories_lsms_18_b_Sen_b_C, "C:/Users/crist/OneDrive - Lancaster University/Lancaster University_RuanoChamorro/Postdoc project/DHS/factories_lsms_18_b_Sen_b_C.csv", row.names = FALSE)

