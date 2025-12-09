#lSMS Wealth index
#Cristina Ruano
#30.7.25


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

LSMS_wealth_index_df_C<-read_csv("DHS/LSMS_wealth_index_df_C.csv")


#pca


wealth_rows_pca = LSMS_wealth_index_df_C [, c("imp_water_dry", "imp_water_wet", "imp_cooking", "imp_walls", "imp_ceiling", "imp_floors", "toilet_imp", 
                                         "w_heater", "c_fan", "armchairs", "table_chairs", "bed", "s_mattres", "e_iron", "gas_elec_stove", 
                                         "gas_cylinder", "non_e_blender", "refrigerator", "p_fan", "tv", "land_phone", "mobile_p", "wardrobes", 
                                         "carpet", "microwave", "freezer", "washing_machine", "aircon_asset", "car", "computer", "printer", "sat_dish", 
                                         "building_house", "c_iron", "radio", "camera", "tablet", "unbuilt_land", "e_foodprocesor", "dvd", 
                                         "motorcycle", "improved_stoves", "gas_e_hotplate", "canoe", "v_cleaner", "bicycle", "hunting_rif", 
                                         "camcorder", "guitar", "gard_tools", "video_camera", "hifi", "piano", "generator", "aircon_housing")]


wealth_rows_pca <- wealth_rows_pca %>%
  mutate(across(everything(), as.numeric))

#PCA model
xord<-prcomp(wealth_rows_pca, center=TRUE, scale=TRUE)

summary(xord)

biplot(xord)

loadings1<-xord$rotation #rotation is the matrix of variable loadings (columns are eigenvectors) in prcomp() function. 
#loadings describe how much each variable contributes to each PCA.
loadings1

#To create the wealth index, we will use the scores from the first principal component (PC1), which is typically the most informative 
#for wealth-related analyses.

#extract pc1

myscores<-xord$x # "x" are the coordinates of the individuals (observations) on the principal components in prcomp()


LSMS_wealth_index_df_C$wealth_index_all<-myscores[,"PC1"] #add this to the data

hist(LSMS_wealth_index_df_C$wealth_index_all)

LSMS_wealth_index_all_C<-LSMS_wealth_index_df_C

write.csv(LSMS_wealth_index_all_C,"C:/Users/crist/OneDrive - Lancaster University/Lancaster University_RuanoChamorro/Postdoc project/DHS/LSMS_wealth_index_18_all_C.csv", row.names = FALSE)


#EMD