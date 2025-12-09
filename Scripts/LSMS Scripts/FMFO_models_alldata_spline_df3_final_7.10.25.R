#Models assessing the relationship between FMFO proximity and fish consumption by wealth group
#models include all data
#models with splines df=3
#updated 7.10.25
#clean final version
#Cristina Ruano


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
library(splines)
library(arm)

setwd("C:/Users/crist/OneDrive - Lancaster University/Lancaster University_RuanoChamorro/Postdoc project")

LSMS_18_clean<-read_csv("DHS/LSMS_18_clean.3.csv")

sum(is.na(LSMS_18_clean$Min_tt_factories))#681 NA


#Combine all smoked sardinella
LSMS_18_clean <- LSMS_18_clean %>%
  mutate(all_smoked_sardinella = case_when(
    # If either (or both) variables is 1, mark as 1.
    smoked_keti_sardinella == 1 | smoked_sardinella == 1 ~ 1,
    # If both variables are NA, then mark as NA.
    is.na(smoked_keti_sardinella) & is.na(smoked_sardinella) ~ NA_real_,
    # If both variables are 0, mark as 0.
    smoked_keti_sardinella == 0 & smoked_sardinella== 0 ~ 0
  ))

#LSMS_18_clean$fish_consumption_quant_all_smoked_sardinella_pc <- ifelse(rowSums(!is.na(LSMS_18_clean[, c("fish_consumption_quant_smoked_sardinella_pc", "fish_consumption_quant_smoked_keti_sardinella_pc")])) > 0, 
#        rowSums(LSMS_18_clean[, c("fish_consumption_quant_smoked_sardinella_pc", "fish_consumption_quant_smoked_keti_sardinella_pc")], na.rm = TRUE), 
#       NA)#keep the NAs when both columns have NA.


#check

check_somoked_df<-LSMS_18_clean%>%
  dplyr::select(hh_id,smoked_keti_sardinella, smoked_sardinella,
                all_smoked_sardinella)



#Combine other fresh fish
LSMS_18_clean <- LSMS_18_clean %>%
  mutate(other_fresh = case_when( #evaluates in order
    # 1) If either any of the variables is 1, mark as 1.
    fresh_barracuda == 1 | 
      fresh_tilapia == 1 | 
      fresh_other == 1 ~ 1,
    # 2) If any variable is 0, mark as 
    fresh_barracuda == 0 |
      fresh_tilapia == 0     |
      fresh_other == 0        ~ 0,
    #3) Only if all three are NA, return NA
    is.na(fresh_barracuda)  &
      is.na(fresh_tilapia)  &
      is.na(fresh_other)    ~ NA_real_
    # (no TRUE ~ … needed unless you want a different default)
  ))


#LSMS_18_clean$fish_consumption_quant_all_other_fresh_pc <- ifelse(rowSums(!is.na(LSMS_18_clean[, c("fish_consumption_quant_fresh_tilapia_pc", "fish_consumption_quant_fresh_barracuda_pc", "fish_consumption_bi_fresh_other")])) > 0, 
# rowSums(LSMS_18_clean[, c("fish_consumption_quant_fresh_tilapia_pc", "fish_consumption_quant_fresh_barracuda_pc", "fish_consumption_bi_fresh_other")], na.rm = TRUE), 
#NA)#keep the NAs when both columns have NA.
#check

check_other_fresh_df<-LSMS_18_clean%>%
  dplyr::select(hh_id,fresh_barracuda, fresh_tilapia, fresh_other,
                other_fresh)


#Combine all fish
LSMS_18_clean <- LSMS_18_clean %>%
  mutate(all_fish = case_when(
    # If any of the fish consumption variables equals 1, mark as 1.
    (!is.na(all_smoked_sardinella) & all_smoked_sardinella == 1) |
      (!is.na(other_fresh) & other_fresh == 1) |
      (!is.na(fresh_sardinella) & fresh_sardinella == 1) |
      (!is.na(dried_fish) & dried_fish == 1) |
      (!is.na(conserves) & conserves == 1) |
      (!is.na(crabs) & crabs == 1) ~ 1,
    # If all fish variables are NA, then the result is NA.
    is.na(smoked_sardinella) & 
      is.na(other_fresh) & 
      is.na(fresh_sardinella) & 
      is.na(dried_fish) & 
      is.na(conserves) & 
      is.na(crabs) ~ NA_real_,
    
    
    # Otherwise, mark as 0.
    TRUE ~ 0
  ))

#check

check_df<-LSMS_18_clean%>%
  dplyr::select(hh_id,fresh_sardinella, fresh_barracuda, fresh_tilapia,
                fresh_other,smoked_keti_sardinella, smoked_sardinella,
                dried_fish, crabs, all_smoked_sardinella, all_fish )

#categorize binomial fish consumption
LSMS_18_clean$smoked_sardinella_fact<-as.factor(LSMS_18_clean$all_smoked_sardinella)
LSMS_18_clean$other_fresh_fact<-as.factor( LSMS_18_clean$other_fresh)
LSMS_18_clean$fresh_sardinella_fact<-as.factor(LSMS_18_clean$fresh_sardinella)
LSMS_18_clean$dried_fish_fact<-as.factor(LSMS_18_clean$dried_fish)
LSMS_18_clean$all_fish_fact<-as.factor(LSMS_18_clean$all_fish)

#categorical distance to factories (10 quintiles)
LSMS_18_clean <- LSMS_18_clean %>%
  mutate(Min_tt_factories_cat_10 = ntile(Min_tt_factories, 10))#Min_tt_factories is the distance to operating factories

LSMS_18_clean$Min_tt_factories_cat_10<-as.factor(LSMS_18_clean$Min_tt_factories_cat_10)


anyNA(LSMS_18_clean$Min_tt_factories)
sum(is.na(LSMS_18_clean$Min_tt_factories))#681

#Histogram travel time to factories

x <- LSMS_18_clean$Min_tt_factories
bw <- 2 * IQR(x, na.rm = TRUE) / (sum(is.finite(x))^(1/3))
if (!is.finite(bw) || bw <= 0) {
  rng <- range(x, na.rm = TRUE)
  bw <- (rng[2] - rng[1]) / 30
}


ggplot(LSMS_18_clean, aes(x = Min_tt_factories)) +
  geom_histogram(
    binwidth = bw, boundary = 0,
    fill = "#4C78A8", color = "white",
    linewidth = 0.6, alpha = 0.9, na.rm = TRUE
  ) +
  labs(
    title = "Distribution of travel time to factories",
    x = "Travel time to factories (minutes)",
    y = "Count"
  ) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())


#categorize wealth
LSMS_18_clean <- LSMS_18_clean %>%
  mutate(wealth_index_quantiles = ntile(wealth_index_all, 5))#wealth quintiles

LSMS_18_clean$wealth_index_quantiles_fact<-as.factor(LSMS_18_clean$wealth_index_quantiles)#as factor

#region and urban/rural
LSMS_18_clean$region_fact<-as.factor(LSMS_18_clean$region)
LSMS_18_clean$urban_rural_fact<-as.factor(LSMS_18_clean$urban_rural)
LSMS_18_clean$region_urban_rural <- interaction(LSMS_18_clean$region, LSMS_18_clean$urban_rural_fact, sep = "_")

#Predictors: scale and transfom into factors
LSMS_18_clean$wealth_index_sc <- scale( LSMS_18_clean$wealth_index_all)
LSMS_18_clean$tt_NearMarket_sc <- scale( LSMS_18_clean$tt_NearMarket)
LSMS_18_clean$Min_tt_factories_sc <- scale( LSMS_18_clean$Min_tt_factories)
LSMS_18_clean$distance_to_marine_km_sc <- scale( LSMS_18_clean$distance_to_marine_km)
LSMS_18_clean$distance_to_inland_km_sc <- scale( LSMS_18_clean$distance_to_inland_km)
LSMS_18_clean$proximity_to_water_km_sc <- scale( LSMS_18_clean$proximity_to_water_km)
LSMS_18_clean$wealth_index_quantiles_fact<-as.factor(LSMS_18_clean$wealth_index_quantiles)
LSMS_18_clean$urban_rural_fact<-as.factor(LSMS_18_clean$urban_rural)
LSMS_18_clean$wealth_index_quantiles_fact<-as.factor(LSMS_18_clean$wealth_index_quantiles_fact)


#reduce number of wealth quintiles

LSMS_18_clean$wealth_3 <- factor(
  ifelse(LSMS_18_clean$wealth_index_quantiles_fact=="1","1",
         ifelse(LSMS_18_clean$wealth_index_quantiles_fact=="2","1",
                ifelse(LSMS_18_clean$wealth_index_quantiles_fact=="3","2","3"))))

#Combine q1 and q2 because there very few respondents in Q1 close to the factories
LSMS_18_clean$wealth_4 <- factor(
  ifelse(LSMS_18_clean$wealth_index_quantiles_fact=="1","1",
         ifelse(LSMS_18_clean$wealth_index_quantiles_fact=="2","1",
                ifelse(LSMS_18_clean$wealth_index_quantiles_fact=="3","2",
                       ifelse(LSMS_18_clean$wealth_index_quantiles_fact=="4","3","4")))))


#Eliminate hh living further away than 70km from the coast
#LSMS_18_clean_70 <- LSMS_18_clean_c[LSMS_18_clean_c$distance_to_marine_km <70 , ]
#nrow(LSMS_18_clean_c)#  7156
#nrow(LSMS_18_clean_70)# 4708
#max(LSMS_18_clean$distance_to_marine_km, na.rm = TRUE)#393.568km
#plot(LSMS_18_clean_70$Min_tt_factories , LSMS_18_clean_70$distance_to_marine_km, main = "Scatter Plot with Correlation",
#    xlab = "Distance to factories", ylab = "Distance to the ocean", pch = 19, col = "blue")
#clean_data <- na.omit(LSMS_18_clean_70[, c("Min_tt_factories", "distance_to_marine_km")])
#cor(clean_data $Min_tt_factories, clean_data $distance_to_marine_km)#0.097

####Model fresh sardinella#####

LSMS_18_clean_70 <- LSMS_18_clean

#Elimnate NAs and create a data for fresh sardinella
LSMS_18_clean_70$fresh_sardinella_int <- as.integer(LSMS_18_clean_70$fresh_sardinella)

any(is.na(LSMS_18_clean_70$fresh_sardinella_int ))
which(any(is.na(LSMS_18_clean_70$fresh_sardinella_int )))

##create a dataset withouth na for fresh sardinella
LSMS_18_clean_d <- LSMS_18_clean_70%>%
  filter(!is.na(fresh_sardinella_int))

any(is.na(LSMS_18_clean_d$fresh_sardinella_int ))#no NAs


#define survey desing
DHSdesign_lsms <- svydesign(id = ~hh_cluster, 
                            strata= ~ region_urban_rural,
                            weights = ~hhweight, 
                            data = LSMS_18_clean_d)

#model with 3 df
model_fresh_sardinella_factory_3_int_4<- svyglm(fresh_sardinella_int ~ 
                                                  ns(Min_tt_factories_sc, df=3) *wealth_4+
                                                  urban_rural_fact+
                                                  tt_NearMarket_sc+
                                                  distance_to_inland_km_sc+
                                                  distance_to_marine_km_sc, 
                                                design = DHSdesign_lsms, 
                                                family = quasibinomial())


#compare if spline terms improve over linear. All models with splines are better than the linear model

regTermTest(model_fresh_sardinella_factory_3_int_4, 
            ~ns(Min_tt_factories_sc, df=3))


##The models with 4 splines has more multicollinearity and shows similar patters to model with 3 splines. We chose the simplest one
##the model with 2 splines does not capture the decresed of consumption at the begining like the models with spline 3 and 4. The multicollinearity is similar to the model with spline 3 (they look fine)




###Diagnostics####

#Residual analysis

#Pearson residuals
pearson_resid <- residuals(model_fresh_sardinella_factory_3_int_4, type = "pearson")

#Deviance residuals
deviance_resid <- residuals(model_fresh_sardinella_factory_3_int_4, type = "deviance")

#Fitted values
fitted_vals <- fitted(model_fresh_sardinella_factory_3_int_4)

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
dispersion_value <- summary(model_fresh_sardinella_factory_3_int_4)$dispersion
print(paste("Dispersion parameter:", round(dispersion_value, 4)))#no overdispersion



# # Predict fitted probabilities
predicted_prob <- predict(model_fresh_sardinella_factory_3_int_4, type = "response")
head(predicted_prob)
# Binned residual plot: residuals vs predicted probabilities
residuals_pearson <- residuals(model_fresh_sardinella_factory_3_int_4, type = "pearson")
binnedplot(
  x = predicted_prob,
  y = residuals_pearson,
  xlab = "Predicted probabilities",
  ylab = "Pearson residuals",
  main = "Binned Residual Plot"
)

#Diagnostics of these model not look amazing, but I think it is because there are too many 1 (80% of the data is 1)

car::dfbetaPlots(model_fresh_sardinella_factory_3_int_4)

car::vif(model_fresh_sardinella_factory_3_int_4)#some values are above 2, but this is not too bad and acceptable. 
#It is important to keep the splines and the interaction which are leading to numbers above 2

#results
summary(model_fresh_sardinella_factory_3_int_4 )#dispersion looks good
plot_model(model_fresh_sardinella_factory_3_int_4 )#fresh sardinella consumption increases with distance to water bodies

model_fresh_sardinella_factory_3_int_4 %>% plot_model(type='est')

#Coefficient plot

library(broom)
# Tidy the model to extract coefficients nd confidence intervals
coef_df <- tidy(model_fresh_sardinella_factory_3_int_4 , conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
# Reorder the terms to match the order in the model specification
coef_df$term <- factor(coef_df$term, levels = c(
  "ns(Min_tt_factories_sc, df = 3)3:wealth_44",
  "ns(Min_tt_factories_sc, df = 3)2:wealth_44",
  "ns(Min_tt_factories_sc, df = 3)1:wealth_44",
  "ns(Min_tt_factories_sc, df = 3)3:wealth_43",
  "ns(Min_tt_factories_sc, df = 3)2:wealth_43",
  "ns(Min_tt_factories_sc, df = 3)1:wealth_43",
  "ns(Min_tt_factories_sc, df = 3)3:wealth_42",
  "ns(Min_tt_factories_sc, df = 3)2:wealth_42",
  "ns(Min_tt_factories_sc, df = 3)1:wealth_42",
  "distance_to_marine_km_sc",
  "distance_to_inland_km_sc",
  "tt_NearMarket_sc",
  "urban_rural_fact2",
  "wealth_44",
  "wealth_43",
  "wealth_42",
  "ns(Min_tt_factories_sc, df = 3)3",
  "ns(Min_tt_factories_sc, df = 3)2", 
  "ns(Min_tt_factories_sc, df = 3)1"))


coef_df$term <- 
  dplyr::recode(coef_df$term,
                "ns(Min_tt_factories_sc, df = 3)3:wealth_44"="Travel time factories ns3 x wealth L4",
                "ns(Min_tt_factories_sc, df = 3)2:wealth_44"="Travel time factories ns2 x wealth L4",
                "ns(Min_tt_factories_sc, df = 3)1:wealth_44"="Travel time factories ns1 x wealth L4",
                "ns(Min_tt_factories_sc, df = 3)3:wealth_43"="Travel time factories ns3 x wealth L3",
                "ns(Min_tt_factories_sc, df = 3)2:wealth_43"="Travel time factories ns2 x wealth L3",
                "ns(Min_tt_factories_sc, df = 3)1:wealth_43"="Travel time factories ns1 x wealth L3",
                "ns(Min_tt_factories_sc, df = 3)3:wealth_42"="Travel time factories ns3 x wealth L2",
                "ns(Min_tt_factories_sc, df = 3)2:wealth_42"="Travel time factories ns2 x wealth L2",
                "ns(Min_tt_factories_sc, df = 3)1:wealth_42"="Travel time factories ns1 x wealth L2",
                "distance_to_marine_km_sc"= "Distance to Coast",
                "distance_to_inland_km_sc"= "Distance to inland waterbodies",
                "tt_NearMarket_sc"= "Travel Time to Market",
                "urban_rural_fact2"="Rural",
                "wealth_44" = "wealth L4",
                "wealth_43" = "wealth L3",
                "wealth_42" = "wealth L2",
                "ns(Min_tt_factories_sc, df = 3)3"="Travel time factories ns3",
                "ns(Min_tt_factories_sc, df = 3)2"="Travel time factories ns2",
                "ns(Min_tt_factories_sc, df = 3)1"="Travel time factories ns1")       



# Create a new column to indicate positive or negative coefficients
coef_df$coef_color <- ifelse(coef_df$estimate > 0, "#6A4C93", "#4DB6AC")

# Create caterpillar plot with color based on coefficient sign and no legend
plot_fresh_sardinella<-ggplot(coef_df, aes(x = term, y = estimate, color = coef_color)) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high),
                 linewidth = 1.4, alpha = 0.95) +
  geom_point(size = 3.3,stroke=0) +
  geom_hline(yintercept = 0, linetype = "dashed", 
             color = "#8C8C8C", linewidth= 0.6) +
  coord_flip() +
  labs(title = "Consumption of Fresh sardinella",
       y = "Estimate (log-odds)", x = NULL) +
  scale_color_manual(values = c("#6A4C93", "#4DB6AC")) +  # Manually set color scale
  theme_minimal(base_size = 12) +
  guides(color = "none") +# Remove the color legend
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10)
  ) 

plot_fresh_sardinella

#predictions
new_data <- data.frame(
  Min_tt_factories_sc = seq(min(DHSdesign_lsms$variables$Min_tt_factories_sc, na.rm = TRUE),#sequence of 100 values ranging from the min to the max
                            max(DHSdesign_lsms$variables$Min_tt_factories_sc, na.rm = TRUE),
                            length.out = 100),
  wealth_4 = factor("1", levels = levels(DHSdesign_lsms$variables$wealth_4)),
  tt_NearMarket_sc = mean(DHSdesign_lsms$variables$tt_NearMarket_sc, na.rm = TRUE),
  urban_rural_fact = factor("1", levels = levels(DHSdesign_lsms$variables$urban_rural_fact)),
  distance_to_inland_km_sc = mean(DHSdesign_lsms$variables$distance_to_inland_km_sc, na.rm = TRUE),
  distance_to_marine_km_sc = mean(DHSdesign_lsms$variables$distance_to_marine_km_sc, na.rm = TRUE)
)

new_data$predicted_log_odds <- predict(model_fresh_sardinella_factory_3_int_4, newdata = new_data, type = "link")

#convert log-odds to probabilities (logistic transformation)
new_data$predicted_prob <- plogis(new_data$predicted_log_odds)
# Calculate confidence intervals
# First, get the design matrix

X <- model.matrix(delete.response(terms(model_fresh_sardinella_factory_3_int_4 )), data = new_data)

#    Calculate standard errors using the variance-covariance matrix of the model
se_fit <- sqrt(diag(X %*% vcov(model_fresh_sardinella_factory_3_int_4 ) %*% t(X)))
#    Compute confidence intervals on the log-odds scale
new_data$lower_log_odds <- new_data$predicted_log_odds - 1.96 * se_fit
new_data$upper_log_odds <- new_data$predicted_log_odds + 1.96 * se_fit

#    Convert log-odds to probabilities (logistic transformation for confidence intervals)
new_data$lower_ci <- plogis(new_data$lower_log_odds)
new_data$upper_ci <- plogis(new_data$upper_log_odds)

p1<-ggplot(new_data, aes(x = Min_tt_factories_sc, y = predicted_prob)) +
  geom_line(color = "#6A4C93", linewidth = 1.4) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), 
              fill="#6A4C93", alpha = 0.2) +
  labs(title ="Fresh Sardinella",
       x = "Travel Time to Factories (Standardized)",
       y = "Probability of Consumption") +
  theme_classic() +
  theme(
    axis.text      = element_text(size = 12),      # numbers on axes
    axis.title     = element_text(size = 15),      # axis labels
    plot.title     = element_text(size = 18, face = "bold"),  # title
    plot.title.position = "plot"                  
  )

p1

#predictions for different wealth groups

new_data <- expand.grid(
  wealth_4 = factor(1:4, levels = 1:4, labels = c("1", "2", "3","4")),
  Min_tt_factories_sc = seq(min(DHSdesign_lsms$variables$Min_tt_factories_sc, na.rm = TRUE),
                            max(DHSdesign_lsms$variables$Min_tt_factories_sc, na.rm = TRUE),
                            length.out = 100),
  tt_NearMarket_sc = mean(DHSdesign_lsms$variables$tt_NearMarket_sc, na.rm = TRUE),
  urban_rural_fact = factor("1", levels = levels(DHSdesign_lsms$variables$urban_rural_fact)),
  distance_to_inland_km_sc = mean(DHSdesign_lsms$variables$distance_to_inland_km_sc, na.rm = TRUE),
  distance_to_marine_km_sc = mean(DHSdesign_lsms$variables$distance_to_marine_km_sc, na.rm = TRUE)
)

new_data$predicted_log_odds <- predict(model_fresh_sardinella_factory_3_int_4, newdata = new_data, type = "link")

#convert log-odds to probabilities (logistic transformation)
new_data$predicted_prob <- plogis(new_data$predicted_log_odds)
# Calculate confidence intervals
# First, get the design matrix

X <- model.matrix(delete.response(terms(model_fresh_sardinella_factory_3_int_4)), data = new_data)

#    Calculate standard errors using the variance-covariance matrix of the model
se_fit <- sqrt(diag(X %*% vcov(model_fresh_sardinella_factory_3_int_4) %*% t(X)))
#    Compute confidence intervals on the log-odds scale
new_data$lower_log_odds <- new_data$predicted_log_odds - 1.96 * se_fit
new_data$upper_log_odds <- new_data$predicted_log_odds + 1.96 * se_fit

#    Convert log-odds to probabilities (logistic transformation for confidence intervals)
new_data$lower_ci <- plogis(new_data$lower_log_odds)
new_data$upper_ci <- plogis(new_data$upper_log_odds)

#Extract values

target_wealth <- "4"
target_min_tt  <- 0.25049847
 #min value

out <- new_data %>%
  filter(wealth_4 == target_wealth) %>%
  filter(near(Min_tt_factories_sc, target_min_tt, tol = 1e-8)) %>%
  slice(1) %>%  # in case of duplicates
  select(predicted_prob, lower_ci, upper_ci)

out

#back transform

# Get the mean and standard deviation of the original 'Min_tt_factories'
mean_Min_tt_factories <- mean(LSMS_18_clean_d$Min_tt_factories, na.rm = TRUE)
sd_Min_tt_factories <- sd(LSMS_18_clean_d$Min_tt_factories, na.rm = TRUE)

# To convert the scaled 'Min_tt_factories_sc' back to the original scale:
LSMS_18_clean_d$Min_tt_factories_original <- (LSMS_18_clean_d$Min_tt_factories_sc * sd_Min_tt_factories) + mean_Min_tt_factories

#when the value of z is 1
Min_tt_factories_original_when_z_1 <- (1 * sd_Min_tt_factories) + mean_Min_tt_factories

#when z is -1.235 the tt is 0.01623096
(-1.22798662  * sd_Min_tt_factories) + mean_Min_tt_factories
(-0.02671748 * sd_Min_tt_factories) + mean_Min_tt_factories#171.564
(0.25049847* sd_Min_tt_factories) + mean_Min_tt_factories#210.025


cols4 <- c("#D32F2F",  # Level 1 - red
           "#E69F00",  # Level 2 - amber
           "#B97C5E",  # Level 3 - clay
           "#0072B2")  # Level 4 - blue

p1_all_wealth <- ggplot(new_data, aes(x = Min_tt_factories_sc, y = predicted_prob,
                                      color = wealth_4)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = wealth_4),
              alpha = 0.25, color = NA) +
  geom_line(linewidth = 1.3) +
  labs(title = "Fresh sardinella",
       x = "Travel Time to Factories (Standardized)",
       y = "Probability of Consumption",
       color = "Wealth", fill = "Wealth") +
  scale_color_manual(values = cols4, name= "Wealth") +
  scale_fill_manual(values  = cols4, name= "Wealth") +
  theme_classic()+
  theme(
    axis.text      = element_text(size = 12),      # numbers on axes
    axis.title     = element_text(size = 15),      # axis labels
    plot.title     = element_text(size = 18, face = "bold"),  # title
    plot.title.position = "plot"                  
  )

p1_all_wealth

plot_data <- DHSdesign_lsms$variables %>%
  mutate(
    wealth_4 = factor(wealth_4, levels = 1:4, labels = c("1","2","3","4"))
  )

p_points_sep <- p1_all_wealth +
  geom_point(
    data = plot_data,
    aes(x = Min_tt_factories_sc,
        y = fresh_sardinella_int,
        color = wealth_4),
    inherit.aes = FALSE,
    alpha = 0.25,
    position = position_jitter(width = 0, height = 0.02)
  ) +
  guides(size = "none") 

p_points_sep

p_points_facets <- p1_all_wealth +
  geom_point(
    data = plot_data,
    aes(x = Min_tt_factories_sc,
        y = fresh_sardinella_int,
        color = wealth_4),
    inherit.aes = FALSE,
    size = 0.8, alpha = 0.4,
    position = position_jitter(width = 0, height = 0.02)
  ) +
  guides(size = "none") +
  facet_wrap(~ wealth_4, ncol = 2)

p_points_facets



# Filter new_data to show only wealth categories 1 and 4

new_data14 <- new_data %>%
  filter(wealth_4 %in% c("1","4")) %>%
  mutate(wealth_4 = factor(wealth_4,
                           levels = c("1","4"),
                           labels = c("Poorest","Wealthiest")))

cols14 <- c("Poorest" = "#D32F2F",   # red
            "Wealthiest" = "#0072B2")# blue

p1_wealth14 <- ggplot(new_data14, aes(x = Min_tt_factories_sc, y = predicted_prob,
                                      color = wealth_4)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = wealth_4),
              alpha = 0.25, color = NA) +
  geom_line(linewidth = 1.3) +
  labs(title = "Fresh sardinella",
       x = "Travel Time to Factories (Standardized)",
       y = "Probability of Consumption",
       color = "Wealth", fill = "Wealth") +
  scale_color_manual(values = cols14) +
  scale_fill_manual(values  = cols14) +
  theme_classic() +
  theme(
    axis.text  = element_text(size = 12),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 18, face = "bold"),
    plot.title.position = "plot"
  )

p1_wealth14


###with raw data

# Data: keep only groups 1 & 4 and relabel
plot_data14 <- DHSdesign_lsms$variables %>%
  filter(wealth_4 %in% c(1, 4)) %>%
  mutate(
    wealth_4 = factor(ifelse(wealth_4 == 1, "Poorest", "Wealthiest"),
                      levels = c("Poorest", "Wealthiest"))
  )

# Palette (matches your lines/ribbons)
cols14 <- c(Poorest = "#D32F2F", Wealthiest = "#0072B2")

# Plot: overlay raw observed points
p_points <- p1_wealth14 +
  geom_point(
    data = plot_data14,
    aes(x = Min_tt_factories_sc, y = fresh_sardinella_int, color = wealth_4),
    inherit.aes = FALSE,
    size = 0.8, alpha = 0.4,
    position = position_jitter(width = 0, height = 0.02)  # tiny vertical jitter for 0/1
  ) +
  scale_color_manual(values = cols14, breaks = c("Poorest", "Wealthiest"))

p_points

p_points <- p1_wealth14 +
  geom_point(
    data = plot_data14,
    aes(x = Min_tt_factories_sc, 
        y =  pmin(fresh_sardinella_int + as.numeric(fresh_sardinella_int == 1) * 0.02, 1.02),#this is to add the points a bit above 1
        #y = fresh_sardinella_int, ´
        color = wealth_4),
    inherit.aes = FALSE,
    size = 1.2,          # increased from 0.6 → 1.2
    alpha = 0.3,         # slightly less transparent
    position = position_jitter(width = 0, height = 0.02)
  ) +
  guides(size = "none") +
  scale_color_manual(values = cols14, breaks = c("Poorest", "Wealthiest"))

p_points


########Models dried fish#############

LSMS_18_clean_70$dried_fish_int <- as.integer(LSMS_18_clean_70$dried_fish)

any(is.na(LSMS_18_clean_70$dried_fish_int))
which(any(is.na(LSMS_18_clean_70$dried_fish_int)))

any(is.na(LSMS_18_clean_70$Min_tt_factories_cat_10 ))#no NAs

LSMS_18_clean_f <- LSMS_18_clean_70 %>%
  filter(!is.na(dried_fish_int))
any(is.na(LSMS_18_clean_f$dried_fish_int ))#no NAs

LSMS_18_clean_f$wealth_index_quantiles_fact<-as.factor(LSMS_18_clean_f$wealth_index_quantiles_fact)

DHSdesign_lsms <- svydesign(id = ~hh_cluster, 
                            strata=~region_urban_rural,
                            weights = ~hhweight, 
                            data = LSMS_18_clean_f)
#model with 3 df
model_dried_fish_factory_3<- svyglm(dried_fish_int ~ 
                                      ns(Min_tt_factories_sc, df=3) *  wealth_4 +
                                      urban_rural_fact+
                                      tt_NearMarket_sc+
                                      distance_to_inland_km_sc+
                                      distance_to_marine_km_sc, 
                                    design = DHSdesign_lsms, 
                                    family = quasibinomial())

regTermTest(model_dried_fish_factory_3, 
            ~ns(Min_tt_factories_sc, df=3))

###Diagnostics####

#Residual analysis

#Pearson residuals
pearson_resid <- residuals(model_dried_fish_factory_3, type = "pearson")

#Deviance residuals
deviance_resid <- residuals(model_dried_fish_factory_3, type = "deviance")

#Fitted values
fitted_vals <- fitted(model_dried_fish_factory_3)

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
dispersion_value <- summary(model_dried_fish_factory_3)$dispersion
print(paste("Dispersion parameter:", round(dispersion_value, 4)))#no overdispersion



# # Predict fitted probabilities
predicted_prob <- predict(model_dried_fish_factory_3, type = "response")
head(predicted_prob)
# Binned residual plot: residuals vs predicted probabilities
residuals_pearson <- residuals(model_dried_fish_factory_3, type = "pearson")
binnedplot(
  x = predicted_prob,
  y = residuals_pearson,
  xlab = "Predicted probabilities",
  ylab = "Pearson residuals",
  main = "Binned Residual Plot"
)


car::dfbetaPlots(model_dried_fish_factory_3)

car::vif(model_dried_fish_factory_3)


#results
summary(model_dried_fish_factory_3 )
plot_model(model_dried_fish_factory_3 )

#Coefficient plot

library(broom)
# Tidy the model to extract coefficients nd confidence intervals
coef_df <- tidy(model_dried_fish_factory_3 , conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
# Reorder the terms to match the order in the model specification
coef_df$term <- factor(coef_df$term, levels = c(
  "ns(Min_tt_factories_sc, df = 3)3:wealth_44",  
  "ns(Min_tt_factories_sc, df = 3)2:wealth_44", 
  "ns(Min_tt_factories_sc, df = 3)1:wealth_44",
  "ns(Min_tt_factories_sc, df = 3)3:wealth_43",  
  "ns(Min_tt_factories_sc, df = 3)2:wealth_43", 
  "ns(Min_tt_factories_sc, df = 3)1:wealth_43",
  "ns(Min_tt_factories_sc, df = 3)3:wealth_42",  
  "ns(Min_tt_factories_sc, df = 3)2:wealth_42", 
  "ns(Min_tt_factories_sc, df = 3)1:wealth_42",
  "distance_to_marine_km_sc",
  "distance_to_inland_km_sc",
  "tt_NearMarket_sc",
  "urban_rural_fact2",
  "wealth_44",
  "wealth_43",
  "wealth_42",
  "ns(Min_tt_factories_sc, df = 3)3",
  "ns(Min_tt_factories_sc, df = 3)2", 
  "ns(Min_tt_factories_sc, df = 3)1"))

coef_df$term <- 
  dplyr::recode(coef_df$term,
                "ns(Min_tt_factories_sc, df = 3)3:wealth_44"="Travel time factories ns3 x wealth L4",
                "ns(Min_tt_factories_sc, df = 3)2:wealth_44"="Travel time factories ns2 x wealth L4",
                "ns(Min_tt_factories_sc, df = 3)1:wealth_44"="Travel time factories ns1 x wealth L4",
                "ns(Min_tt_factories_sc, df = 3)3:wealth_43"="Travel time factories ns3 x wealth L3",
                "ns(Min_tt_factories_sc, df = 3)2:wealth_43"="Travel time factories ns2 x wealth L3",
                "ns(Min_tt_factories_sc, df = 3)1:wealth_43"="Travel time factories ns1 x wealth L3",
                "ns(Min_tt_factories_sc, df = 3)3:wealth_42"="Travel time factories ns3 x wealth L2",
                "ns(Min_tt_factories_sc, df = 3)2:wealth_42"="Travel time factories ns2 x wealth L2",
                "ns(Min_tt_factories_sc, df = 3)1:wealth_42"="Travel time factories ns1 x wealth L2",
                "distance_to_marine_km_sc"= "Distance to Coast",
                "distance_to_inland_km_sc" = "Distance to inland waterbodies",
                "tt_NearMarket_sc" = "Travel Time to Market",
                "urban_rural_fact2"="Rural",
                "wealth_44"="wealth L4",
                "wealth_43"="wealth L3",
                "wealth_43"="wealth L2",
                "ns(Min_tt_factories_sc, df = 3)3" = "Travel Time to Factory ns3",
                "ns(Min_tt_factories_sc, df = 3)2" = "Travel Time to Factory ns2",
                "ns(Min_tt_factories_sc, df = 3)1" = "Travel Time to Factory ns1")

# Create a new column to indicate positive or negative coefficients
coef_df$coef_color <- ifelse(coef_df$estimate > 0, "#6A4C93", "#4DB6AC")

# Create caterpillar plot with color based on coefficient sign and no legend
plot_model_dried_fish<-ggplot(coef_df, aes(x = term, y = estimate, color = coef_color)) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high),
                 linewidth = 1.4, alpha = 0.95) +
  geom_point(size = 3.3,stroke=0) +
  geom_hline(yintercept = 0, linetype = "dashed", 
             color = "#8C8C8C", linewidth= 0.6) +
  coord_flip() +
  labs(title = "Consumption of Dried Fish",
       y = "Estimate (log-odds)", x = NULL) +
  scale_color_manual(values = c("#6A4C93", "#4DB6AC")) +  # Manually set color scale
  theme_minimal(base_size = 12) +
  guides(color = "none") +# Remove the color legend
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10)
  ) 

plot_model_dried_fish

#predictions
new_data <- data.frame(
  Min_tt_factories_sc = seq(min(DHSdesign_lsms$variables$Min_tt_factories_sc, na.rm = TRUE),#sequence of 100 values ranging from the min to the max
                            max(DHSdesign_lsms$variables$Min_tt_factories_sc, na.rm = TRUE),
                            length.out = 100),
  wealth_4 = factor("1", levels = levels(DHSdesign_lsms$variables$wealth_4)),
  tt_NearMarket_sc = mean(DHSdesign_lsms$variables$tt_NearMarket_sc, na.rm = TRUE),
  urban_rural_fact = factor("1", levels = levels(DHSdesign_lsms$variables$urban_rural_fact)),
  distance_to_inland_km_sc = mean(DHSdesign_lsms$variables$distance_to_inland_km_sc, na.rm = TRUE),
  distance_to_marine_km_sc = mean(DHSdesign_lsms$variables$distance_to_marine_km_sc, na.rm = TRUE)
)


new_data$predicted_log_odds <- predict(model_dried_fish_factory_3 , newdata = new_data, type = "link")

#convert log-odds to probabilities (logistic transformation)
new_data$predicted_prob <- plogis(new_data$predicted_log_odds)
# Calculate confidence intervals
# First, get the design matrix

X <- model.matrix(delete.response(terms(model_dried_fish_factory_3 )), data = new_data)

#    Calculate standard errors using the variance-covariance matrix of the model
se_fit <- sqrt(diag(X %*% vcov(model_dried_fish_factory_3 ) %*% t(X)))
#    Compute confidence intervals on the log-odds scale
new_data$lower_log_odds <- new_data$predicted_log_odds - 1.96 * se_fit
new_data$upper_log_odds <- new_data$predicted_log_odds + 1.96 * se_fit

#    Convert log-odds to probabilities (logistic transformation for confidence intervals)
new_data$lower_ci <- plogis(new_data$lower_log_odds)
new_data$upper_ci <- plogis(new_data$upper_log_odds)

p2<-ggplot(new_data, aes(x = Min_tt_factories_sc, y = predicted_prob)) +
  geom_line(color = "#6A4C93", linewidth = 1.4) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), 
              fill="#6A4C93", alpha = 0.2) +
  labs(title ="Dried fish",
       x = "Travel Time to Factories (Standardized)",
       y = "Probability of Consumption") +
  theme_classic() +
  theme(
    axis.text      = element_text(size = 12),      # numbers on axes
    axis.title     = element_text(size = 15),      # axis labels
    plot.title     = element_text(size = 18, face = "bold"),  # title
    plot.title.position = "plot"                  
  )
p2


#predictiions for different wealth groups

new_data <- expand.grid(
  wealth_4 = factor(1:4, levels = 1:4, labels = c("1", "2", "3", "4")),
  Min_tt_factories_sc = seq(min(DHSdesign_lsms$variables$Min_tt_factories_sc, na.rm = TRUE),
                            max(DHSdesign_lsms$variables$Min_tt_factories_sc, na.rm = TRUE),
                            length.out = 100),
  tt_NearMarket_sc = mean(DHSdesign_lsms$variables$tt_NearMarket_sc, na.rm = TRUE),
  urban_rural_fact = factor("1", levels = levels(DHSdesign_lsms$variables$urban_rural_fact)),
  distance_to_inland_km_sc = mean(DHSdesign_lsms$variables$distance_to_inland_km_sc, na.rm = TRUE),
  distance_to_marine_km_sc = mean(DHSdesign_lsms$variables$distance_to_marine_km_sc, na.rm = TRUE)
)

new_data$predicted_log_odds <- predict(model_dried_fish_factory_3, newdata = new_data, type = "link")

#convert log-odds to probabilities (logistic transformation)
new_data$predicted_prob <- plogis(new_data$predicted_log_odds)
# Calculate confidence intervals
# First, get the design matrix

X <- model.matrix(delete.response(terms(model_dried_fish_factory_3)), data = new_data)

#    Calculate standard errors using the variance-covariance matrix of the model
se_fit <- sqrt(diag(X %*% vcov(model_dried_fish_factory_3) %*% t(X)))
#    Compute confidence intervals on the log-odds scale
new_data$lower_log_odds <- new_data$predicted_log_odds - 1.96 * se_fit
new_data$upper_log_odds <- new_data$predicted_log_odds + 1.96 * se_fit

#    Convert log-odds to probabilities (logistic transformation for confidence intervals)
new_data$lower_ci <- plogis(new_data$lower_log_odds)
new_data$upper_ci <- plogis(new_data$upper_log_odds)


#Extract values

target_wealth <- "4"
target_min_tt  <- -0.02671748
#min value

out <- new_data %>%
  filter(wealth_4 == target_wealth) %>%
  filter(near(Min_tt_factories_sc, target_min_tt, tol = 1e-8)) %>%
  slice(1) %>%  # in case of duplicates
  select(predicted_prob, lower_ci, upper_ci)

out

#back transform

# Get the mean and standard deviation of the original 'Min_tt_factories'
mean_Min_tt_factories <- mean(LSMS_18_clean_f$Min_tt_factories, na.rm = TRUE)
sd_Min_tt_factories <- sd(LSMS_18_clean_f$Min_tt_factories, na.rm = TRUE)

# To convert the scaled 'Min_tt_factories_sc' back to the original scale:
LSMS_18_clean_f$Min_tt_factories_original <- (LSMS_18_clean_f$Min_tt_factories_sc * sd_Min_tt_factories) + mean_Min_tt_factories

#when the value of z is 1
Min_tt_factories_original_when_z_1 <- (1 * sd_Min_tt_factories) + mean_Min_tt_factories

#when z is -1.235 the tt is 0.01623096
(-1.22798662  * sd_Min_tt_factories) + mean_Min_tt_factories#0.9999996
(-0.02671748 * sd_Min_tt_factories) + mean_Min_tt_factories#171.564
(0.25049847* sd_Min_tt_factories) + mean_Min_tt_factories#210.925
(0* sd_Min_tt_factories) + mean_Min_tt_factories#175.3576


#average consumption of the welthiest hh from travel time to factories  -1.22798662  to -0.02671748
lower_limit <- -1.22798662 
upper_limit <- -0.02671748

avg_consumption_wealth4 <- new_data%>%
  filter(
    wealth_4== 4,  
    Min_tt_factories_sc >= lower_limit,
    Min_tt_factories_sc <= upper_limit
  ) %>%
  summarise(avg_consumption = mean(predicted_prob, na.rm = TRUE),
            n = sum(!is.na(predicted_prob)),
            approx_lci = mean(lower_ci, na.rm = TRUE),  # descriptive average of lower CI endpoints
            approx_uci = mean(upper_ci, na.rm = TRUE)   # descriptive average of upper CI endpoints
  )

print(avg_consumption_wealth4)

cols4 <- c("#D32F2F",  # Level 1 - red
           "#E69F00",  # Level 2 - amber
           "#B97C5E",  # Level 3 - clay
           "#0072B2")  # Level 4 - blue

p2_all_wealth <- ggplot(new_data, aes(x = Min_tt_factories_sc, y = predicted_prob,
                                      color = wealth_4)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = wealth_4),
              alpha = 0.25, color = NA) +
  geom_line(linewidth = 1.3) +
  labs(title = "Dried Fish",
       x = "Travel Time to Factories (Standardized)",
       y = "Probability of Consumption",
       color = "Wealth", fill = "Wealth") +
  scale_color_manual(values = cols4, name= "Wealth") +
  scale_fill_manual(values  = cols4, name= "Wealth") +
  theme_classic()+
  theme(
    axis.text      = element_text(size = 12),      # numbers on axes
    axis.title     = element_text(size = 15),      # axis labels
    plot.title     = element_text(size = 18, face = "bold"),  # title
    plot.title.position = "plot"                  
  )

p2_all_wealth

#aw data
plot2_data <- DHSdesign_lsms$variables %>%
  mutate(
    wealth_4 = factor(wealth_4, levels = 1:4, labels = c("1","2","3","4")))

p2_points_sep <- p2_all_wealth +
  geom_point(
    data = plot2_data,
    aes(x = Min_tt_factories_sc,
        y = dried_fish_int,
        color = wealth_4),
    inherit.aes = FALSE,
    alpha = 0.25,
    position = position_jitter(width = 0, height = 0.02)
  ) +
  guides(size = "none")

p2_points_sep

p2_points_facets <- p2_all_wealth +
  geom_point(
    data = plot2_data,
    aes(x = Min_tt_factories_sc,
        y = dried_fish_int,
        color = wealth_4),
    inherit.aes = FALSE,
    size = 0.8, alpha = 0.4,
    position = position_jitter(width = 0, height = 0.02)
  ) +
  guides(size = "none") +
  facet_wrap(~ wealth_4, ncol = 2)

p2_points_facets


# Filter new_data to show only wealth categories 1 and 4
new_data14 <- new_data %>%
  filter(wealth_4 %in% c("1","4")) %>%
  mutate(wealth_4 = factor(wealth_4,
                           levels = c("1","4"),
                           labels = c("Poorest","Wealthiest")))

cols14 <- c("Poorest" = "#D32F2F",   # red
            "Wealthiest" = "#0072B2")# blue

p2_wealth14 <- ggplot(new_data14, aes(x = Min_tt_factories_sc, y = predicted_prob,
                                      color = wealth_4)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = wealth_4),
              alpha = 0.25, color = NA) +
  geom_line(linewidth = 1.3) +
  labs(title = "Dried fish",
       x = "Travel Time to Factories (Standardized)",
       y = "Probability of Consumption",
       color = "Wealth", fill = "Wealth") +
  scale_color_manual(values = cols14) +
  scale_fill_manual(values  = cols14) +
  theme_classic() +
  theme(
    axis.text  = element_text(size = 12),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 18, face = "bold"),
    plot.title.position = "plot"
  )

p2_wealth14

###with raw data

# Data: keep only groups 1 & 4 and relabel
plot2_data14 <- DHSdesign_lsms$variables %>%
  filter(wealth_4 %in% c(1, 4)) %>%
  mutate(
    wealth_4 = factor(ifelse(wealth_4 == 1, "Poorest", "Wealthiest"),
                      levels = c("Poorest", "Wealthiest"))
  )

# Palette (matches your lines/ribbons)
cols14 <- c(Poorest = "#D32F2F", Wealthiest = "#0072B2")

# Plot: overlay raw observed points
p2_points <- p2_wealth14 +
  geom_point(
    data = plot2_data14,
    aes(x = Min_tt_factories_sc, y = dried_fish_int, color = wealth_4),
    inherit.aes = FALSE,
    size = 0.8, alpha = 0.4,
    position = position_jitter(width = 0, height = 0.02)  # tiny vertical jitter for 0/1
  ) +
  scale_color_manual(values = cols14, breaks = c("Poorest", "Wealthiest"))

p2_points


p_raw_dried <- p2_wealth14 +
  geom_point(
    data = plot2_data14,
    aes(x = Min_tt_factories_sc, 
        y =  pmin(dried_fish_int + as.numeric(dried_fish_int == 1) * 0.02, 1.02),#this is to add the points a bit above 1
        #y = dried_fish_int, 
        color = wealth_4),
    inherit.aes = FALSE,
    size = 1.2,          # increased from 0.6 → 1.2
    alpha = 0.3,         # slightly less transparent
    position = position_jitter(width = 0, height = 0.02)
  ) +
  guides(size = "none") +
  scale_color_manual(values = cols14, breaks = c("Poorest", "Wealthiest"))


p_raw_dried


########Models other fresh fish###########
LSMS_18_clean_70$other_fresh_int <- as.integer(LSMS_18_clean_70$other_fresh)

any(is.na(LSMS_18_clean_70$other_fresh_int))
which(any(is.na(LSMS_18_clean_70$other_fresh_int)))

any(is.na(LSMS_18_clean_70$Min_tt_factories_cat_10 ))#no NAs


LSMS_18_clean_g <- LSMS_18_clean_70%>%
  filter(!is.na(other_fresh_int))
any(is.na(LSMS_18_clean_g$other_fresh_int))#no NAs

LSMS_18_clean_g$wealth_index_quantiles_fact<-as.factor(LSMS_18_clean_g$wealth_index_quantiles_fact)


DHSdesign_lsms <- svydesign(id = ~hh_cluster, 
                            strata= ~region_urban_rural,
                            weights = ~hhweight, 
                            data = LSMS_18_clean_g)

#model with 3 df
model_other_fresh_factory_3<- svyglm(other_fresh_int ~ 
                                       ns(Min_tt_factories_sc, df=3) * wealth_4+
                                       urban_rural_fact+
                                       tt_NearMarket_sc+
                                       distance_to_inland_km_sc+
                                       distance_to_marine_km_sc, 
                                     design = DHSdesign_lsms, 
                                     family = quasibinomial())


regTermTest(model_other_fresh_factory_3, 
            ~ns(Min_tt_factories_sc, df=3))#Strong evidence that the relationship between Min_tt_factories_sc and the outcome is not zero (and likely non-linear), after accounting for the other covariates and the survey design.



###Diagnostics####


#Residual analysis

#Pearson residuals
pearson_resid <- residuals(model_other_fresh_factory_3, type = "pearson")

#Deviance residuals
deviance_resid <- residuals(model_other_fresh_factory_3, type = "deviance")

#Fitted values
fitted_vals <- fitted(model_other_fresh_factory_3)

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
dispersion_value <- summary(model_other_fresh_factory_3)$dispersion
print(paste("Dispersion parameter:", round(dispersion_value, 4)))



# # Predict fitted probabilities
predicted_prob <- predict(model_other_fresh_factory_3, type = "response")
head(predicted_prob)
# Binned residual plot: residuals vs predicted probabilities
residuals_pearson <- residuals(model_other_fresh_factory_3, type = "pearson")
binnedplot(
  x = predicted_prob,
  y = residuals_pearson,
  xlab = "Predicted probabilities",
  ylab = "Pearson residuals",
  main = "Binned Residual Plot"
)


car::dfbetaPlots(model_other_fresh_factory_3)

car::vif(model_other_fresh_factory_3)


#results
summary(model_other_fresh_factory_3 )#dispersion looks good
plot_model(model_other_fresh_factory_3 )

#Coefficient plot

library(broom)
# Tidy the model to extract coefficients nd confidence intervals
coef_df <- tidy(model_other_fresh_factory_3 , conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
# Reorder the terms to match the order in the model specification
coef_df$term <- factor(coef_df$term, levels = c(
  "ns(Min_tt_factories_sc, df = 3)3:wealth_44",  
  "ns(Min_tt_factories_sc, df = 3)2:wealth_44", 
  "ns(Min_tt_factories_sc, df = 3)1:wealth_44",
  "ns(Min_tt_factories_sc, df = 3)3:wealth_43",  
  "ns(Min_tt_factories_sc, df = 3)2:wealth_43", 
  "ns(Min_tt_factories_sc, df = 3)1:wealth_43",
  "ns(Min_tt_factories_sc, df = 3)3:wealth_42",  
  "ns(Min_tt_factories_sc, df = 3)2:wealth_42", 
  "ns(Min_tt_factories_sc, df = 3)1:wealth_42",
  "distance_to_marine_km_sc",
  "distance_to_inland_km_sc",
  "tt_NearMarket_sc",
  "urban_rural_fact2",
  "wealth_44",
  "wealth_43",
  "wealth_42",
  "ns(Min_tt_factories_sc, df = 3)3",
  "ns(Min_tt_factories_sc, df = 3)2", 
  "ns(Min_tt_factories_sc, df = 3)1"))

coef_df$term <- 
  dplyr::recode(coef_df$term,
                "ns(Min_tt_factories_sc, df = 3)3:wealth_44"="Travel time factories ns3 x wealth L4",
                "ns(Min_tt_factories_sc, df = 3)2:wealth_44"="Travel time factories ns2 x wealth L4",
                "ns(Min_tt_factories_sc, df = 3)1:wealth_44"="Travel time factories ns1 x wealth L4",
                "ns(Min_tt_factories_sc, df = 3)3:wealth_43"="Travel time factories ns3 x wealth L3",
                "ns(Min_tt_factories_sc, df = 3)2:wealth_43"="Travel time factories ns2 x wealth L3",
                "ns(Min_tt_factories_sc, df = 3)1:wealth_43"="Travel time factories ns1 x wealth L3",
                "ns(Min_tt_factories_sc, df = 3)3:wealth_42"="Travel time factories ns3 x wealth L2",
                "ns(Min_tt_factories_sc, df = 3)2:wealth_42"="Travel time factories ns2 x wealth L2",
                "ns(Min_tt_factories_sc, df = 3)1:wealth_42"="Travel time factories ns1 x wealth L2",
                "distance_to_marine_km_sc"= "Distance to Coast",
                "distance_to_inland_km_sc" = "Distance to inland waterbodies",
                "tt_NearMarket_sc" = "Travel Time to Market",
                "urban_rural_fact2"="Rural",
                "wealth_44"="wealth L4",
                "wealth_43"="wealth L3",
                "wealth_43"="wealth L2",
                "ns(Min_tt_factories_sc, df = 3)3" = "Travel Time to Factory ns3",
                "ns(Min_tt_factories_sc, df = 3)2" = "Travel Time to Factory ns2",
                "ns(Min_tt_factories_sc, df = 3)1" = "Travel Time to Factory ns1")

# Create a new column to indicate positive or negative coefficients
coef_df$coef_color <- ifelse(coef_df$estimate > 0, "#6A4C93", "#4DB6AC")

# Create caterpillar plot with color based on coefficient sign and no legend
plot_model_other_fresh<-ggplot(coef_df, aes(x = term, y = estimate, color = coef_color)) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high),
                 linewidth = 1.4, alpha = 0.95) +
  geom_point(size = 3.3,stroke=0) +
  geom_hline(yintercept = 0, linetype = "dashed", 
             color = "#8C8C8C", linewidth= 0.6) +
  coord_flip() +
  labs(title = "Consumption of Other fresh fish",
       y = "Estimate (log-odds)", x = NULL) +
  scale_color_manual(values = c("#6A4C93", "#4DB6AC")) +  # Manually set color scale
  theme_minimal(base_size = 12) +
  guides(color = "none") +# Remove the color legend
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10)
  ) 

plot_model_other_fresh

#predictions
new_data <- data.frame(
  Min_tt_factories_sc = seq(min(DHSdesign_lsms$variables$Min_tt_factories_sc, na.rm = TRUE),#sequence of 100 values ranging from the min to the max
                            max(DHSdesign_lsms$variables$Min_tt_factories_sc, na.rm = TRUE),
                            length.out = 100),
  wealth_4 = factor("1", levels = levels(DHSdesign_lsms$variables$wealth_4)),
  tt_NearMarket_sc = mean(DHSdesign_lsms$variables$tt_NearMarket_sc, na.rm = TRUE),
  urban_rural_fact = factor("1", levels = levels(DHSdesign_lsms$variables$urban_rural_fact)),
  distance_to_inland_km_sc = mean(DHSdesign_lsms$variables$distance_to_inland_km_sc, na.rm = TRUE),
  distance_to_marine_km_sc = mean(DHSdesign_lsms$variables$distance_to_marine_km_sc, na.rm = TRUE)
)


new_data$predicted_log_odds <- predict(model_other_fresh_factory_3 , newdata = new_data, type = "link")

#convert log-odds to probabilities (logistic transformation)
new_data$predicted_prob <- plogis(new_data$predicted_log_odds)
# Calculate confidence intervals
# First, get the design matrix

X <- model.matrix(delete.response(terms(model_other_fresh_factory_3 )), data = new_data)

#    Calculate standard errors using the variance-covariance matrix of the model
se_fit <- sqrt(diag(X %*% vcov(model_other_fresh_factory_3 ) %*% t(X)))
#    Compute confidence intervals on the log-odds scale
new_data$lower_log_odds <- new_data$predicted_log_odds - 1.96 * se_fit
new_data$upper_log_odds <- new_data$predicted_log_odds + 1.96 * se_fit

#    Convert log-odds to probabilities (logistic transformation for confidence intervals)
new_data$lower_ci <- plogis(new_data$lower_log_odds)
new_data$upper_ci <- plogis(new_data$upper_log_odds)

p3<-ggplot(new_data, aes(x = Min_tt_factories_sc, y = predicted_prob)) +
  geom_line(color = "#6A4C93", linewidth = 1.4) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), 
              fill="#6A4C93", alpha = 0.2) +
  labs(title ="Other fresh fish",
       x = "Travel Time to Factories (Standardized)",
       y = "Probability of Consumption") +
  theme_classic() +
  theme(
    axis.text      = element_text(size = 12),      # numbers on axes
    axis.title     = element_text(size = 15),      # axis labels
    plot.title     = element_text(size = 18, face = "bold"),  # title
    plot.title.position = "plot"                  
  )
p3
#predictiions for different wealth groups

new_data <- expand.grid(
  wealth_4 = factor(1:4, levels = 1:4, labels = c("1", "2", "3", "4")),
  Min_tt_factories_sc = seq(min(DHSdesign_lsms$variables$Min_tt_factories_sc, na.rm = TRUE),
                            max(DHSdesign_lsms$variables$Min_tt_factories_sc, na.rm = TRUE),
                            length.out = 100),
  tt_NearMarket_sc = mean(DHSdesign_lsms$variables$tt_NearMarket_sc, na.rm = TRUE),
  urban_rural_fact = factor("1", levels = levels(DHSdesign_lsms$variables$urban_rural_fact)),
  distance_to_inland_km_sc = mean(DHSdesign_lsms$variables$distance_to_inland_km_sc, na.rm = TRUE),
  distance_to_marine_km_sc = mean(DHSdesign_lsms$variables$distance_to_marine_km_sc, na.rm = TRUE)
)

new_data$predicted_log_odds <- predict(model_other_fresh_factory_3, newdata = new_data, type = "link")

#convert log-odds to probabilities (logistic transformation)
new_data$predicted_prob <- plogis(new_data$predicted_log_odds)
# Calculate confidence intervals
# First, get the design matrix

X <- model.matrix(delete.response(terms(model_other_fresh_factory_3)), data = new_data)

#    Calculate standard errors using the variance-covariance matrix of the model
se_fit <- sqrt(diag(X %*% vcov(model_other_fresh_factory_3) %*% t(X)))
#    Compute confidence intervals on the log-odds scale
new_data$lower_log_odds <- new_data$predicted_log_odds - 1.96 * se_fit
new_data$upper_log_odds <- new_data$predicted_log_odds + 1.96 * se_fit

#    Convert log-odds to probabilities (logistic transformation for confidence intervals)
new_data$lower_ci <- plogis(new_data$lower_log_odds)
new_data$upper_ci <- plogis(new_data$upper_log_odds)

#Extract values

target_wealth <- "1"
target_min_tt  <-  0.25049847
#min value

out <- new_data %>%
  filter(wealth_4 == target_wealth) %>%
  filter(near(Min_tt_factories_sc, target_min_tt, tol = 1e-8)) %>%
  slice(1) %>%  # in case of duplicates
  dplyr::select(predicted_prob, lower_ci, upper_ci)

out

#back transform

# Get the mean and standard deviation of the original 'Min_tt_factories'
mean_Min_tt_factories <- mean(LSMS_18_clean_f$Min_tt_factories, na.rm = TRUE)
sd_Min_tt_factories <- sd(LSMS_18_clean_f$Min_tt_factories, na.rm = TRUE)

# To convert the scaled 'Min_tt_factories_sc' back to the original scale:
LSMS_18_clean_f$Min_tt_factories_original <- (LSMS_18_clean_f$Min_tt_factories_sc * sd_Min_tt_factories) + mean_Min_tt_factories

#when the value of z is 1
Min_tt_factories_original_when_z_1 <- (1 * sd_Min_tt_factories) + mean_Min_tt_factories

#when z is -1.235 the tt is 0.01623096
(-1.22798662  * sd_Min_tt_factories) + mean_Min_tt_factories#0.9999996
(-0.02671748 * sd_Min_tt_factories) + mean_Min_tt_factories#171.564
(0.25049847* sd_Min_tt_factories) + mean_Min_tt_factories#210.925
(0* sd_Min_tt_factories) + mean_Min_tt_factories#175.3576





cols4 <- c("#D32F2F",  # Level 1 - red
           "#E69F00",  # Level 2 - amber
           "#B97C5E",  # Level 3 - clay
           "#0072B2")  # Level 4 - blue

p3_all_wealth <- ggplot(new_data, aes(x = Min_tt_factories_sc, y = predicted_prob,
                                      color = wealth_4)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = wealth_4),
              alpha = 0.25, color = NA) +
  geom_line(linewidth = 1.3) +
  labs(title = "Other fresh fish",
       x = "Travel Time to Factories (Standardized)",
       y = "Probability of Consumption",
       color = "Wealth", fill = "Wealth") +
  scale_color_manual(values = cols4, name= "Wealth") +
  scale_fill_manual(values  = cols4, name= "Wealth") +
  theme_classic()+
  theme(
    axis.text      = element_text(size = 12),      # numbers on axes
    axis.title     = element_text(size = 15),      # axis labels
    plot.title     = element_text(size = 18, face = "bold"),  # title
    plot.title.position = "plot"                  
  )

p3_all_wealth

#raw data

plot3_data <- DHSdesign_lsms$variables %>%
  mutate(
    wealth_4 = factor(wealth_4, levels = 1:4, labels = c("1","2","3","4")))

p3_points_sep <- p3_all_wealth +
  geom_point(
    data = plot3_data,
    aes(x = Min_tt_factories_sc,
        y = other_fresh_int,
        color = wealth_4),
    inherit.aes = FALSE,
    alpha = 0.25,
    position = position_jitter(width = 0, height = 0.02)
  ) +
  guides(size = "none")

p3_points_sep

p3_points_facets <- p3_all_wealth +
  geom_point(
    data = plot3_data,
    aes(x = Min_tt_factories_sc,
        y = other_fresh_int,
        color = wealth_4),
    inherit.aes = FALSE,
    size = 0.8, alpha = 0.4,
    position = position_jitter(width = 0, height = 0.02)
  ) +
  guides(size = "none") +
  facet_wrap(~ wealth_4, ncol = 2)

p3_points_facets

# Filter new_data to show only wealth categories 1 and 4
new_data14 <- new_data %>%
  filter(wealth_4 %in% c("1","4")) %>%
  mutate(wealth_4 = factor(wealth_4,
                           levels = c("1","4"),
                           labels = c("Poorest","Wealthiest")))

cols14 <- c("Poorest" = "#D32F2F",   # red
            "Wealthiest" = "#0072B2")# blue

p3_wealth14 <- ggplot(new_data14, aes(x = Min_tt_factories_sc, y = predicted_prob,
                                      color = wealth_4)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = wealth_4),
              alpha = 0.25, color = NA) +
  geom_line(linewidth = 1.3) +
  labs(title = "Other fresh fish",
       x = "Travel Time to Factories (Standardized)",
       y = "Probability of Consumption",
       color = "Wealth", fill = "Wealth") +
  scale_color_manual(values = cols14) +
  scale_fill_manual(values  = cols14) +
  theme_classic() +
  theme(
    axis.text  = element_text(size = 12),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 18, face = "bold"),
    plot.title.position = "plot"
  )

p3_wealth14

# Data: keep only groups 1 & 4 and relabel
plot3_data14 <- DHSdesign_lsms$variables %>%
  filter(wealth_4 %in% c(1, 4)) %>%
  mutate(
    wealth_4 = factor(ifelse(wealth_4 == 1, "Poorest", "Wealthiest"),
                      levels = c("Poorest", "Wealthiest"))
  )

# Palette (matches your lines/ribbons)
cols14 <- c(Poorest = "#D32F2F", Wealthiest = "#0072B2")

# Plot: overlay raw observed points
p3_points <- p3_wealth14 +
  geom_point(
    data = plot3_data14,
    aes(x = Min_tt_factories_sc, y = other_fresh_int, color = wealth_4),
    inherit.aes = FALSE,
    size = 0.8, alpha = 0.4,
    position = position_jitter(width = 0, height = 0.02)  # tiny vertical jitter for 0/1
  ) +
  scale_color_manual(values = cols14, breaks = c("Poorest", "Wealthiest"))

p3_points


p_raw_other_fresh <- p3_wealth14 +
  geom_point(
    data = plot3_data14,
    aes(x = Min_tt_factories_sc, 
        y =  pmin(other_fresh_int + as.numeric(other_fresh_int == 1) * 0.02, 1.02),#this is to add the points a bit above 1
        #y = other_fresh_int, 
        color = wealth_4),
    inherit.aes = FALSE,
    size = 1.2,          # increased from 0.6 → 1.2
    alpha = 0.3,         # slightly less transparent
    position = position_jitter(width = 0, height = 0.02)
  ) +
  guides(size = "none") +
  scale_color_manual(values = cols14, breaks = c("Poorest", "Wealthiest"))


p_raw_other_fresh





#########Models all fish##############

LSMS_18_clean_70$all_fish_int <- as.integer(LSMS_18_clean_70$all_fish)

any(is.na(LSMS_18_clean_70$allfish_int))
which(any(is.na(LSMS_18_clean_70$all_fish_int)))

any(is.na(LSMS_18_clean_70$Min_tt_factories_cat_10 ))#no NAs


LSMS_18_clean_h <- LSMS_18_clean_70 %>%
  filter(!is.na(all_fish_int))
any(is.na(LSMS_18_clean_h$all_fish_int))#no NAs

LSMS_18_clean_h$wealth_index_quantiles_fact<-as.factor(LSMS_18_clean_h$wealth_index_quantiles_fact)


DHSdesign_lsms <- svydesign(id = ~hh_cluster,
                            strata=~region_urban_rural,
                            weights = ~hhweight, 
                            data = LSMS_18_clean_h)

#model with 3 df
model_all_fish_factory_3<- svyglm(all_fish_int ~ 
                                    ns(Min_tt_factories_sc, df=3) *wealth_4+
                                    urban_rural_fact+
                                    tt_NearMarket_sc+
                                    distance_to_inland_km_sc+
                                    distance_to_marine_km_sc, 
                                  design = DHSdesign_lsms, 
                                  family = quasibinomial())

regTermTest(model_all_fish_factory_3, 
            ~ns(Min_tt_factories_sc, df=3))
#Residual analysis

#Pearson residuals
pearson_resid <- residuals(model_all_fish_factory_3, type = "pearson")

#Deviance residuals
deviance_resid <- residuals(model_all_fish_factory_3, type = "deviance")

#Fitted values
fitted_vals <- fitted(model_all_fish_factory_3)

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
dispersion_value <- summary(model_all_fish_factory_3)$dispersion
print(paste("Dispersion parameter:", round(dispersion_value, 4)))



# # Predict fitted probabilities
predicted_prob <- predict(model_all_fish_factory_3, type = "response")
head(predicted_prob)
# Binned residual plot: residuals vs predicted probabilities
residuals_pearson <- residuals(model_all_fish_factory_3, type = "pearson")
binnedplot(
  x = predicted_prob,
  y = residuals_pearson,
  xlab = "Predicted probabilities",
  ylab = "Pearson residuals",
  main = "Binned Residual Plot"
)


car::dfbetaPlots(model_all_fish_factory_3)

car::vif(model_all_fish_factory_3)


#results
summary(model_all_fish_factory_3 )#dispersion looks good
plot_model(model_all_fish_factory_3 )

#Coefficient plot

library(broom)
# Tidy the model to extract coefficients nd confidence intervals
coef_df <- tidy(model_all_fish_factory_3 , conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
# Reorder the terms to match the order in the model specification
coef_df$term <- factor(coef_df$term, levels = c(
  "ns(Min_tt_factories_sc, df = 3)3:wealth_44",  
  "ns(Min_tt_factories_sc, df = 3)2:wealth_44", 
  "ns(Min_tt_factories_sc, df = 3)1:wealth_44",
  "ns(Min_tt_factories_sc, df = 3)3:wealth_43",  
  "ns(Min_tt_factories_sc, df = 3)2:wealth_43", 
  "ns(Min_tt_factories_sc, df = 3)1:wealth_43",
  "ns(Min_tt_factories_sc, df = 3)3:wealth_42",  
  "ns(Min_tt_factories_sc, df = 3)2:wealth_42", 
  "ns(Min_tt_factories_sc, df = 3)1:wealth_42",
  "distance_to_marine_km_sc",
  "distance_to_inland_km_sc",
  "tt_NearMarket_sc",
  "urban_rural_fact2",
  "wealth_44",
  "wealth_43",
  "wealth_42",
  "ns(Min_tt_factories_sc, df = 3)3",
  "ns(Min_tt_factories_sc, df = 3)2", 
  "ns(Min_tt_factories_sc, df = 3)1"))

coef_df$term <- 
  dplyr::recode(coef_df$term,
                "ns(Min_tt_factories_sc, df = 3)3:wealth_44"="Travel time factories ns3 x wealth L4",
                "ns(Min_tt_factories_sc, df = 3)2:wealth_44"="Travel time factories ns2 x wealth L4",
                "ns(Min_tt_factories_sc, df = 3)1:wealth_44"="Travel time factories ns1 x wealth L4",
                "ns(Min_tt_factories_sc, df = 3)3:wealth_43"="Travel time factories ns3 x wealth L3",
                "ns(Min_tt_factories_sc, df = 3)2:wealth_43"="Travel time factories ns2 x wealth L3",
                "ns(Min_tt_factories_sc, df = 3)1:wealth_43"="Travel time factories ns1 x wealth L3",
                "ns(Min_tt_factories_sc, df = 3)3:wealth_42"="Travel time factories ns3 x wealth L2",
                "ns(Min_tt_factories_sc, df = 3)2:wealth_42"="Travel time factories ns2 x wealth L2",
                "ns(Min_tt_factories_sc, df = 3)1:wealth_42"="Travel time factories ns1 x wealth L2",
                "distance_to_marine_km_sc"= "Distance to Coast",
                "distance_to_inland_km_sc" = "Distance to inland waterbodies",
                "tt_NearMarket_sc" = "Travel Time to Market",
                "urban_rural_fact2"="Rural",
                "wealth_44"="wealth L4",
                "wealth_43"="wealth L3",
                "wealth_43"="wealth L2",
                "ns(Min_tt_factories_sc, df = 3)3" = "Travel Time to Factory ns3",
                "ns(Min_tt_factories_sc, df = 3)2" = "Travel Time to Factory ns2",
                "ns(Min_tt_factories_sc, df = 3)1" = "Travel Time to Factory ns1")


# Create a new column to indicate positive or negative coefficients
coef_df$coef_color <- ifelse(coef_df$estimate > 0, "#6A4C93", "#4DB6AC")

# Create caterpillar plot with color based on coefficient sign and no legend
plot_model_all_fish<-ggplot(coef_df, aes(x = term, y = estimate, color = coef_color)) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high),
                 linewidth = 1.4, alpha = 0.95) +
  geom_point(size = 3.3,stroke=0) +
  geom_hline(yintercept = 0, linetype = "dashed", 
             color = "#8C8C8C", linewidth= 0.6) +
  coord_flip() +
  labs(title = "Consumption of All fish",
       y = "Estimate (log-odds)", x = NULL) +
  scale_color_manual(values = c("#6A4C93", "#4DB6AC")) +  # Manually set color scale
  theme_minimal(base_size = 12) +
  guides(color = "none") +# Remove the color legend
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10)
  ) 

plot_model_all_fish

#predictions
new_data <- data.frame(
  Min_tt_factories_sc = seq(min(DHSdesign_lsms$variables$Min_tt_factories_sc, na.rm = TRUE),#sequence of 100 values ranging from the min to the max
                            max(DHSdesign_lsms$variables$Min_tt_factories_sc, na.rm = TRUE),
                            length.out = 100),
  wealth_4 = factor("1", levels = levels(DHSdesign_lsms$variables$wealth_4)),
  tt_NearMarket_sc = mean(DHSdesign_lsms$variables$tt_NearMarket_sc, na.rm = TRUE),
  urban_rural_fact = factor("1", levels = levels(DHSdesign_lsms$variables$urban_rural_fact)),
  distance_to_inland_km_sc = mean(DHSdesign_lsms$variables$distance_to_inland_km_sc, na.rm = TRUE),
  distance_to_marine_km_sc = mean(DHSdesign_lsms$variables$distance_to_marine_km_sc, na.rm = TRUE)
)


new_data$predicted_log_odds <- predict(model_all_fish_factory_3 , newdata = new_data, type = "link")

#convert log-odds to probabilities (logistic transformation)
new_data$predicted_prob <- plogis(new_data$predicted_log_odds)
# Calculate confidence intervals
# First, get the design matrix

X <- model.matrix(delete.response(terms(model_all_fish_factory_3 )), data = new_data)

#    Calculate standard errors using the variance-covariance matrix of the model
se_fit <- sqrt(diag(X %*% vcov(model_all_fish_factory_3 ) %*% t(X)))
#    Compute confidence intervals on the log-odds scale
new_data$lower_log_odds <- new_data$predicted_log_odds - 1.96 * se_fit
new_data$upper_log_odds <- new_data$predicted_log_odds + 1.96 * se_fit

#    Convert log-odds to probabilities (logistic transformation for confidence intervals)
new_data$lower_ci <- plogis(new_data$lower_log_odds)
new_data$upper_ci <- plogis(new_data$upper_log_odds)

p4<-ggplot(new_data, aes(x = Min_tt_factories_sc, y = predicted_prob)) +
  geom_line(color = "#6A4C93", linewidth = 1.4) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), 
              fill="#6A4C93", alpha = 0.2) +
  labs(title ="All fish",
       x = "Travel Time to Factories (Standardized)",
       y = "Probability of Consumption") +
  theme_classic() +
  theme(
    axis.text      = element_text(size = 12),      # numbers on axes
    axis.title     = element_text(size = 15),      # axis labels
    plot.title     = element_text(size = 18, face = "bold"),  # title
    plot.title.position = "plot"                  
  )
p4

#predictiions for different wealth groups

new_data <- expand.grid(
  wealth_4 = factor(1:4, levels = 1:4, labels = c("1", "2", "3", "4")),
  Min_tt_factories_sc = seq(min(DHSdesign_lsms$variables$Min_tt_factories_sc, na.rm = TRUE),
                            max(DHSdesign_lsms$variables$Min_tt_factories_sc, na.rm = TRUE),
                            length.out = 100),
  tt_NearMarket_sc = mean(DHSdesign_lsms$variables$tt_NearMarket_sc, na.rm = TRUE),
  urban_rural_fact = factor("1", levels = levels(DHSdesign_lsms$variables$urban_rural_fact)),
  distance_to_inland_km_sc = mean(DHSdesign_lsms$variables$distance_to_inland_km_sc, na.rm = TRUE),
  distance_to_marine_km_sc = mean(DHSdesign_lsms$variables$distance_to_marine_km_sc, na.rm = TRUE)
)

new_data$predicted_log_odds <- predict(model_all_fish_factory_3, newdata = new_data, type = "link")

#convert log-odds to probabilities (logistic transformation)
new_data$predicted_prob <- plogis(new_data$predicted_log_odds)
# Calculate confidence intervals
# First, get the design matrix

X <- model.matrix(delete.response(terms(model_all_fish_factory_3)), data = new_data)

#    Calculate standard errors using the variance-covariance matrix of the model
se_fit <- sqrt(diag(X %*% vcov(model_all_fish_factory_3) %*% t(X)))
#    Compute confidence intervals on the log-odds scale
new_data$lower_log_odds <- new_data$predicted_log_odds - 1.96 * se_fit
new_data$upper_log_odds <- new_data$predicted_log_odds + 1.96 * se_fit

#    Convert log-odds to probabilities (logistic transformation for confidence intervals)
new_data$lower_ci <- plogis(new_data$lower_log_odds)
new_data$upper_ci <- plogis(new_data$upper_log_odds)

#Extract values

target_wealth <- "4"
target_min_tt  <-  -1.22798662
#min value

out <- new_data %>%
  filter(wealth_4 == target_wealth) %>%
  filter(near(Min_tt_factories_sc, target_min_tt, tol = 1e-8)) %>%
  slice(1) %>%  # in case of duplicates
  dplyr::select(predicted_prob, lower_ci, upper_ci)

out

#back transform

# Get the mean and standard deviation of the original 'Min_tt_factories'
mean_Min_tt_factories <- mean(LSMS_18_clean_f$Min_tt_factories, na.rm = TRUE)
sd_Min_tt_factories <- sd(LSMS_18_clean_f$Min_tt_factories, na.rm = TRUE)

# To convert the scaled 'Min_tt_factories_sc' back to the original scale:
LSMS_18_clean_f$Min_tt_factories_original <- (LSMS_18_clean_f$Min_tt_factories_sc * sd_Min_tt_factories) + mean_Min_tt_factories

#when the value of z is 1
Min_tt_factories_original_when_z_1 <- (1 * sd_Min_tt_factories) + mean_Min_tt_factories

#when z is -1.235 the tt is 0.01623096
(-1.22798662  * sd_Min_tt_factories) + mean_Min_tt_factories#0.9999996
(-0.02671748 * sd_Min_tt_factories) + mean_Min_tt_factories#171.564
(0.25049847* sd_Min_tt_factories) + mean_Min_tt_factories#210.925
(0* sd_Min_tt_factories) + mean_Min_tt_factories#175.3576





cols4 <- c("#D32F2F",  # Level 1 - red
           "#E69F00",  # Level 2 - amber
           "#B97C5E",  # Level 3 - clay
           "#0072B2")  # Level 4 - blue

p4_all_wealth <- ggplot(new_data, aes(x = Min_tt_factories_sc, y = predicted_prob,
                                      color = wealth_4)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = wealth_4),
              alpha = 0.25, color = NA) +
  geom_line(linewidth = 1.3) +
  labs(title = "All fish",
       x = "Travel Time to Factories (Standardized)",
       y = "Probability of Consumption",
       color = "Wealth", fill = "Wealth") +
  scale_color_manual(values = cols4, name= "Wealth") +
  scale_fill_manual(values  = cols4, name= "Wealth") +
  theme_classic()+
  theme(
    axis.text      = element_text(size = 12),      # numbers on axes
    axis.title     = element_text(size = 15),      # axis labels
    plot.title     = element_text(size = 18, face = "bold"),  # title
    plot.title.position = "plot"                  
  )

p4_all_wealth

#raw data

plot4_data <- DHSdesign_lsms$variables %>%
  mutate(
    wealth_4 = factor(wealth_4, levels = 1:4, labels = c("1","2","3","4")))

p4_points_sep <- p4_all_wealth +
  geom_point(
    data = plot4_data,
    aes(x = Min_tt_factories_sc,
        y = all_fish_int,
        color = wealth_4),
    inherit.aes = FALSE,
    alpha = 0.25,
    position = position_jitter(width = 0, height = 0.02)
  ) +
  guides(size = "none")

p4_points_sep

p4_points_facets <- p4_all_wealth +
  geom_point(
    data = plot4_data,
    aes(x = Min_tt_factories_sc,
        y = all_fish_int,
        color = wealth_4),
    inherit.aes = FALSE,
    size = 0.8, alpha = 0.4,
    position = position_jitter(width = 0, height = 0.02)
  ) +
  guides(size = "none") +
  facet_wrap(~ wealth_4, ncol = 2)

p4_points_facets

# Filter new_data to show only wealth categories 1 and 4
new_data14 <- new_data %>%
  filter(wealth_4 %in% c("1","4")) %>%
  mutate(wealth_4 = factor(wealth_4,
                           levels = c("1","4"),
                           labels = c("Poorest","Wealthiest")))

cols14 <- c("Poorest" = "#D32F2F",   # red
            "Wealthiest" = "#0072B2")# blue

p4_wealth14 <- ggplot(new_data14, aes(x = Min_tt_factories_sc, y = predicted_prob,
                                      color = wealth_4)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = wealth_4),
              alpha = 0.25, color = NA) +
  geom_line(linewidth = 1.3) +
  labs(title = "All fish",
       x = "Travel Time to Factories (Standardized)",
       y = "Probability of Consumption",
       color = "Wealth", fill = "Wealth") +
  scale_color_manual(values = cols14) +
  scale_fill_manual(values  = cols14) +
  theme_classic() +
  theme(
    axis.text  = element_text(size = 12),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 18, face = "bold"),
    plot.title.position = "plot"
  )

p4_wealth14

#raw data

# Data: keep only groups 1 & 4 and relabel
plot4_data14 <- DHSdesign_lsms$variables %>%
  filter(wealth_4 %in% c(1, 4)) %>%
  mutate(
    wealth_4 = factor(ifelse(wealth_4 == 1, "Poorest", "Wealthiest"),
                      levels = c("Poorest", "Wealthiest"))
  )

# Palette (matches your lines/ribbons)
cols14 <- c(Poorest = "#D32F2F", Wealthiest = "#0072B2")

# Plot: overlay raw observed points
p4_points <- p4_wealth14 +
  geom_point(
    data = plot4_data14,
    aes(x = Min_tt_factories_sc, y = all_fish_int, color = wealth_4),
    inherit.aes = FALSE,
    size = 0.8, alpha = 0.4,
    position = position_jitter(width = 0, height = 0.02)  # tiny vertical jitter for 0/1
  ) +
  scale_color_manual(values = cols14, breaks = c("Poorest", "Wealthiest"))

p4_points


p_raw_all_fish <- p4_wealth14 +
  geom_point(
    data = plot4_data14,
    aes(x = Min_tt_factories_sc, 
        y =  pmin(all_fish_int + as.numeric(all_fish_int == 1) * 0.02, 1.02),#this is to add the points a bit above 1
        color = wealth_4),
    inherit.aes = FALSE,
    size = 1.2,          # increased from 0.6 → 1.2
    alpha = 0.3,         # slightly less transparent
    position = position_jitter(width = 0, height = 0.02)
  ) +
  guides(size = "none") +
  scale_color_manual(values = cols14, breaks = c("Poorest", "Wealthiest"))


p_raw_all_fish


######all plots together######
library(patchwork)

(p4 + p1 + p2 + p3) +
  plot_layout(ncol = 4)

# 1) Center each plot's title (already done with common_title_theme)
common_title_theme <- theme(
  plot.title = element_text(hjust = 0.5))

# 2) Remove x and y axis titles from p1-p3, keep only y-axis title on p4
p1.b <- p1 + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
p2.b <- p2 + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
p3.b <- p3 + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
p4.b <- p4 + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
# 4) Combine plots

combined <- (p4.b + p1.b + p2.b + p3.b) + 
  plot_layout(ncol = 4) & 
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) &
  theme(
    axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 16),
    axis.title.x = element_blank(),  # remove all individual x labels, we add a shared one later
    axis.text = element_text(size = 14)
  )

combined
# 5) Add a shared x-axis label centered under all plots
combined + plot_annotation(
  caption = "Travel Time to Factories (Standardized)"
) & theme(plot.caption = element_text(hjust = 0.5, size = 16, margin = margin(t = 10)))

#with observed data

(p_points + p_raw_dried + p_raw_other_fresh + p_raw_all_fish) +
  plot_layout(ncol = 4)

# 1) Center each plot's title (already done with common_title_theme)
common_title_theme <- theme(
  plot.title = element_text(hjust = 0.5))

# 2) Remove x and y axis titles from p1-p3, keep only y-axis title on p4
p1.b <- p_points + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")
p2.b <- p_raw_dried + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")
p3.b <- p_raw_other_fresh + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "right")
p4.b <- p_raw_all_fish +
  labs(y = "Probability of consumption") +
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 16))

# combine (note: no axis.title.y in the shared theme)
combined <- (p4.b + p1.b + p2.b + p3.b) +
  plot_layout(ncol = 4) &
  scale_y_continuous(limits = c(-0.05, 1.05), expand = c(0, 0)) &
  theme(
    axis.title.x = element_blank(),
    axis.text = element_text(size = 14)
  )

combined

# shared x label (caption trick)
combined + plot_annotation(caption = "Travel Time to Factories (Standardized)") &
  theme(plot.caption = element_text(hjust = 0.5, size = 16, margin = margin(t = 10)))



###wealth

(p4_wealth14 + p1_wealth14 + p2_wealth14 + p3_wealth14) +
  plot_layout(ncol = 4)

# 1) Center each plot's title (already done with common_title_theme)
common_title_theme <- theme(
  plot.title = element_text(hjust = 0.5))

# 2) Remove x and y axis titles from p1-p3, keep only y-axis title on p4

p1.b <- p1_wealth14 + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")
p2.b <- p2_wealth14 + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")
p3.b <- p2_wealth14 + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "right")
p4.b <- p4_wealth14 +
  labs(y = "Probability of consumption") +
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 16))

# combine (note: no axis.title.y in the shared theme)
combined <- (p4.b + p1.b + p2.b + p3.b) +
  plot_layout(ncol = 4) &
  scale_y_continuous(limits = c(-0.05, 1.05), expand = c(0, 0)) &
  theme(
    axis.title.x = element_blank(),
    axis.text = element_text(size = 14)
  )

combined

# shared x label (caption trick)
combined + plot_annotation(caption = "Travel Time to Factories (Standardized)") &
  theme(plot.caption = element_text(hjust = 0.5, size = 16, margin = margin(t = 10)))


##l wealth

(p4_all_wealth+ p1_all_wealth + p2_all_wealth + p3_all_wealth) +
  plot_layout(ncol = 4)

# 1) Center each plot's title (already done with common_title_theme)
common_title_theme <- theme(
  plot.title = element_text(hjust = 0.5))

# 2) Remove x and y axis titles from p1-p3, keep only y-axis title on p4

p1.b <- p1_all_wealth + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")
p2.b <- p2_all_wealth + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")
p3.b <- p3_all_wealth + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "right")
p4.b <- p4_all_wealth +
  labs(y = "Probability of consumption") +
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 16))

# combine (note: no axis.title.y in the shared theme)
combined <- (p4.b + p1.b + p2.b + p3.b) +
  plot_layout(ncol = 4) &
  scale_y_continuous(limits = c(-0.05, 1.05), expand = c(0, 0)) &
  theme(
    axis.title.x = element_blank(),
    axis.text = element_text(size = 14)
  )


# shared x label (caption trick)
combined + plot_annotation(caption = "Travel Time to Factories (Standardized)") &
  theme(plot.caption = element_text(hjust = 0.5, size = 16, margin = margin(t = 10)))

