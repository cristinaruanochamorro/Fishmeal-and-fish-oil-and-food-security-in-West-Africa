#LSMS consumption models
#3.10.25

library(readr)
library(tidyverse)
#library(Hmisc)
#library(plyr)
library(dplyr)
library(arm)
library(survey)
library(magrittr)
library(ggplot2)
library(sjPlot)
library(ggeffects)

setwd("C:/Users/crist/OneDrive - Lancaster University/Lancaster University_RuanoChamorro/Postdoc project")

LSMS_18_clean<-read_csv("DHS/LSMS_18_clean.3.csv")


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

#categorical distance to factories
#LSMS_18_clean <- LSMS_18_clean %>%
# mutate(Min_tt_factories_cat_10 = ntile(Min_tt_factories, 10))#Min_tt_factories is the distance to operating factories

#LSMS_18_clean$Min_tt_factories_cat_10<-as.factor(LSMS_18_clean$Min_tt_factories_cat_10)

#categorize wealth
LSMS_18_clean <- LSMS_18_clean %>%
  mutate(wealth_index_quantiles = ntile(wealth_index_all, 5))#wealth quintiles

LSMS_18_clean$wealth_index_quantiles_fact<-as.factor(LSMS_18_clean$wealth_index_quantiles)#as factor

#region and urban/rural
LSMS_18_clean$region_fact<-as.factor(LSMS_18_clean$region)
LSMS_18_clean$urban_rural_fact<-as.factor(LSMS_18_clean$urban_rural)
LSMS_18_clean$region_urban_rural <- interaction(LSMS_18_clean$region, LSMS_18_clean$urban_rural_fact, sep = "_")

######CONSUMPTION BY WEALTH GROUP###################

#scale numerical variables and transform categorical variables into factor

LSMS_18_clean$tt_NearMarket_sc <- scale( LSMS_18_clean$tt_NearMarket)
LSMS_18_clean$distance_to_marine_km_sc <- scale( LSMS_18_clean$distance_to_marine_km)
LSMS_18_clean$distance_to_inland_km_sc <- scale( LSMS_18_clean$distance_to_inland_km)
LSMS_18_clean$proximity_to_water_km_sc <- scale( LSMS_18_clean$proximity_to_water_km)
LSMS_18_clean$wealth_index_quantiles_fact<-as.factor(LSMS_18_clean$wealth_index_quantiles)
LSMS_18_clean$head_employment_fact<-as.factor(LSMS_18_clean$head_employment)
LSMS_18_clean$any_adult_employed_fact<-as.factor(LSMS_18_clean$any_adult_employed)
LSMS_18_clean$any_adult_woman_employed_fact<-as.factor(LSMS_18_clean$any_adult_woman_employed)
LSMS_18_clean$urban_rural_fact<-as.factor(LSMS_18_clean$urban_rural)
LSMS_18_clean$hh_size_sc<-scale(LSMS_18_clean$hh_size)
LSMS_18_clean$gender_head_hh_fact<-as.factor(LSMS_18_clean$gender_head_hh)
LSMS_18_clean$hh_cluster_fact<-as.factor(LSMS_18_clean$hh_cluster)
LSMS_18_clean$literacy_fact<-as.factor(LSMS_18_clean$any_adult_woman_literate)
LSMS_18_clean$hh_size_sc<-scale(LSMS_18_clean$hh_size)

#### 1.Model all fish#####

# Convert to integer
LSMS_18_clean$all_fish_int<-as.integer(LSMS_18_clean$all_fish)

# Check for missing values
any(is.na(LSMS_18_clean$all_fish_int))

# Remove rows with NA in response variable
LSMS_18_clean_all <- LSMS_18_clean %>%
  filter(!is.na(all_fish_int))

# Confirm removal
any(is.na(LSMS_18_clean_all$all_fish_int))#FALSE


#Survey desing
DHSdesign_lsms <- svydesign(id = ~hh_cluster, 
                            strata = ~region_urban_rural,
                            weights = ~hhweight, 
                            data = LSMS_18_clean_all)


fish_cons_mod_2.b_w_fact_all_fish <- svyglm(all_fish_int ~ 
                                              wealth_index_quantiles_fact + 
                                              tt_NearMarket_sc+
                                              urban_rural_fact+
                                              any_adult_woman_employed_fact+
                                              distance_to_inland_km_sc+
                                              distance_to_marine_km_sc,
                                            design = DHSdesign_lsms, 
                                            family = quasibinomial())


#####1.1.Diagnostics#####

pseudo_r2 <- 1 - (fish_cons_mod_2.b_w_fact_all_fish$deviance / fish_cons_mod_2.b_w_fact_all_fish$null.deviance)

#Residual analysis

#Pearson residuals
pearson_resid <- residuals(fish_cons_mod_2.b_w_fact_all_fish, type = "pearson")

#Deviance residuals
deviance_resid <- residuals(fish_cons_mod_2.b_w_fact_all_fish, type = "deviance")

#Fitted values
fitted_vals <- fitted(fish_cons_mod_2.b_w_fact_all_fish)

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
dispersion_value <- summary(fish_cons_mod_2.b_w_fact_all_fish)$dispersion
print(paste("Dispersion parameter:", round(dispersion_value, 4)))



# # Predict fitted probabilities
predicted_prob <- predict(fish_cons_mod_2.b_w_fact_all_fish, type = "response")
head(predicted_prob)
# Binned residual plot: residuals vs predicted probabilities
residuals_pearson <- residuals(fish_cons_mod_2.b_w_fact_all_fish, type = "pearson")
binnedplot(
  x = predicted_prob,
  y = residuals_pearson,
  xlab = "Predicted probabilities",
  ylab = "Pearson residuals",
  main = "Binned Residual Plot"
)


car::dfbetaPlots(fish_cons_mod_2.b_w_fact_all_fish)

car::vif(fish_cons_mod_2.b_w_fact_all_fish)

######1.2.Results######
summary(fish_cons_mod_2.b_w_fact_all_fish)
plot_model(fish_cons_mod_2.b_w_fact_all_fish)


ggpredict(fish_cons_mod_2.b_w_fact_all_fish,
          terms = c("distance_to_marine_km_sc [-1:3 by=0.05]")) %>%
  plot()

ggpredict(fish_cons_mod_2.b_w_fact_all_fish,
          terms = c("wealth_index_quantiles_fact")) %>%
  plot()



######1.2.1. wealth and all fish consumption plot######

#predictions
new_data <- data.frame(
  wealth_index_quantiles_fact = factor(1:5, levels = 1:5, labels = c("1", "2", "3", "4", "5")),
  urban_rural_fact = factor("1", levels = levels(DHSdesign_lsms$variables$urban_rural_fact)),
  tt_NearMarket_sc = mean(DHSdesign_lsms$variables$tt_NearMarket_sc, na.rm = TRUE),
  distance_to_inland_km_sc = mean(DHSdesign_lsms$variables$distance_to_inland_km_sc, na.rm = TRUE),
  distance_to_marine_km_sc = mean(DHSdesign_lsms$variables$distance_to_marine_km_sc, na.rm = TRUE),
  any_adult_woman_employed_fact= factor("1", levels = levels(DHSdesign_lsms$variables$any_adult_woman_employed_fact))
)


new_data$predicted_log_odds <- predict(fish_cons_mod_2.b_w_fact_all_fish, newdata = new_data, type = "link")

#convert log-odds to probabilities (logistic transformation)
new_data$predicted_prob <- plogis(new_data$predicted_log_odds)

# Calculate confidence intervals
# First, get the design matrix

X <- model.matrix(delete.response(terms(fish_cons_mod_2.b_w_fact_all_fish)), data = new_data)

#    Calculate standard errors using the variance-covariance matrix of the model
se_fit <- sqrt(diag(X %*% vcov(fish_cons_mod_2.b_w_fact_all_fish) %*% t(X)))
#    Compute confidence intervals on the log-odds scale
new_data$lower_log_odds <- new_data$predicted_log_odds - 1.96 * se_fit
new_data$upper_log_odds <- new_data$predicted_log_odds + 1.96 * se_fit

#    Convert log-odds to probabilities (logistic transformation for confidence intervals)
new_data$lower_ci <- plogis(new_data$lower_log_odds)
new_data$upper_ci <- plogis(new_data$upper_log_odds)

ggplot(new_data, aes(x =   wealth_index_quantiles_fact , y = predicted_prob)) +
  geom_point(size = 1.2) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
  labs(title ="All fish Consumption",
       x = "Wealth",
       y = "Predicted Probability of Consumption") +
  theme_minimal() 

new_data$lower_ci <- plogis(new_data$lower_log_odds)
new_data$upper_ci <- plogis(new_data$upper_log_odds)



p2 <- ggplot(new_data, aes(x = wealth_index_quantiles_fact, y = predicted_prob)) +
  geom_point(color = "#2e0854", size = 2) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                width = 0.15, color = "#2e0854", linewidth = 0.8) +
  labs(title = "All fish",
       x = "Wealth",
       y = "Probability of Consumption") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.title.position = "plot"
  )

p2



p2 <- ggplot(new_data, aes(x = wealth_index_quantiles_fact, y = predicted_prob, color = wealth_index_quantiles_fact)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                width = 0, linewidth = 1) +
  scale_color_manual(values = c(
    "1" = "#CC0000",   # rojo
    "2" = "#FF6666",   # menos rojo
    "3" = "#CCCCCC",   # gris
    "4" = "#6699FF",   # azul
    "5" = "#0036FF"    # muy azul
  )) +
  labs(title = "All fish",
       x = "Wealth",
       y = "Probability of Consumption",
       color = "Wealth") +  # optional: legend title
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.title.position = "plot"
  )

p2

p2 <- ggplot(new_data, aes(x = wealth_index_quantiles_fact, y = predicted_prob, color = wealth_index_quantiles_fact)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                width = 0, linewidth = 1) +
  scale_color_manual(values = c(
    "1" = "#CC0000",   # rojo
    "2" = "#FF6666",   # menos rojo
    "3" = "#CCCCCC",   # gris
    "4" = "#6699FF",   # azul
    "5" = "#0036FF"    # muy azul
  )) +
  labs(title = NULL,
       x = "Wealth",
       y = "Probability of Consumption",
       color = "Wealth") +  # optional: legend title
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.title.position = "plot",
    legend.position = "none"   
  )

p2


#plot consumption

DHSdesign_lsms <- svydesign(id = ~hh_cluster, 
                            strata = ~region_urban_rural,
                            weights = ~hhweight, 
                            data = LSMS_18_clean_all)


all_fish_table<-svytable(~ all_fish_fact, design=DHSdesign_lsms)%>%
  as.data.frame()

all_fish_table_b<- all_fish_table%>%
  mutate(percent = Freq / sum(Freq) * 100)


plot_all_fish_b<-ggplot(all_fish_table_b, aes(x = all_fish_fact, y = percent)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Consumption",
    y = "Percentage (%)",
    title = "All fish"
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_classic() +
  theme(
    plot.title = element_text(
      hjust = 0.5,          
      face = "bold",        
      size = 16             
    )
  )


#####1.2.2. Coefficient plot all fish####

library(broom)
# Tidy the model to extract coefficients and confidence intervals
coef_df <- tidy(fish_cons_mod_2.b_w_fact_all_fish, conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
# Reorder the terms to match the order in the model specification
coef_df$term <- factor(coef_df$term, levels = c("distance_to_marine_km_sc",
                                                "distance_to_inland_km_sc",
                                                "tt_NearMarket_sc",
                                                "urban_rural_fact2",
                                                "any_adult_woman_employed_fact1",
                                                "wealth_index_quantiles_fact5",
                                                "wealth_index_quantiles_fact4",
                                                "wealth_index_quantiles_fact3",
                                                "wealth_index_quantiles_fact2"
                                                
))

coef_df$term <- 
  dplyr::recode(coef_df$term,
                "distance_to_marine_km_sc" = "Distance to Ocean",
                "distance_to_inland_km_sc" = "Distance to indland waterbodies",
                "tt_NearMarket_sc" = "Travel Time to Markets",
                "urban_rural_fact2"="Rural",
                "any_adult_woman_employed_fact1"= "Employment status women",
                "wealth_index_quantiles_fact5" = "Wealth Q5",
                "wealth_index_quantiles_fact4" = "Wealth Q4",
                "wealth_index_quantiles_fact3" = "Wealth Q3",
                "wealth_index_quantiles_fact2" = "Wealth Q2")



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
  labs(title = "Household fish consumption",
       subtitle = "(Senegal)",
       tag= "b)",
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
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.05)))

#extract coef

coefs <- summary(fish_cons_mod_2.b_w_fact_all_fish)$coefficients
ORs <- exp(coefs[, "Estimate"])

CI_lower <- exp(coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"])
CI_upper <- exp(coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"])

OR_table <- data.frame(
  OR = ORs,
  CI_lower = CI_lower,
  CI_upper = CI_upper,
  p_value = coefs[, "Pr(>|t|)"]
)


#####1.3. Model all fish no comsumption#####

#We could alternatively model the prob of no consumption, but the interpretation is more confusing.

#the diagnostics of this model are better

LSMS_18_clean_all$no_fish <- 1 - LSMS_18_clean_all$all_fish_int

DHSdesign_lsms <- svydesign(id = ~hh_cluster, 
                            strata = ~region_urban_rural,
                            weights = ~hhweight, 
                            data = LSMS_18_clean_all)


fish_cons_mod_2.b_w_fact_NO_fish <- svyglm(no_fish ~ 
                                             wealth_index_quantiles_fact + 
                                             tt_NearMarket_sc +
                                             urban_rural_fact +
                                             any_adult_woman_employed_fact +
                                             distance_to_inland_km_sc +
                                             distance_to_marine_km_sc,
                                           design = DHSdesign_lsms, 
                                           family = quasibinomial())

######1.3.1. wealth and all fish consumption plot######

#predictions
new_data <- data.frame(
  wealth_index_quantiles_fact = factor(1:5, levels = 1:5, labels = c("1", "2", "3", "4", "5")),
  urban_rural_fact = factor("1", levels = levels(DHSdesign_lsms$variables$urban_rural_fact)),
  tt_NearMarket_sc = mean(DHSdesign_lsms$variables$tt_NearMarket_sc, na.rm = TRUE),
  distance_to_inland_km_sc = mean(DHSdesign_lsms$variables$distance_to_inland_km_sc, na.rm = TRUE),
  distance_to_marine_km_sc = mean(DHSdesign_lsms$variables$distance_to_marine_km_sc, na.rm = TRUE),
  any_adult_woman_employed_fact= factor("1", levels = levels(DHSdesign_lsms$variables$any_adult_woman_employed_fact))
)


new_data$predicted_log_odds <- predict(fish_cons_mod_2.b_w_fact_NO_fish, newdata = new_data, type = "link")

#convert log-odds to probabilities (logistic transformation)
new_data$predicted_prob <- plogis(new_data$predicted_log_odds)

# Calculate confidence intervals
# First, get the design matrix

X <- model.matrix(delete.response(terms(fish_cons_mod_2.b_w_fact_NO_fish)), data = new_data)

#    Calculate standard errors using the variance-covariance matrix of the model
se_fit <- sqrt(diag(X %*% vcov(fish_cons_mod_2.b_w_fact_NO_fish) %*% t(X)))
#    Compute confidence intervals on the log-odds scale
new_data$lower_log_odds <- new_data$predicted_log_odds - 1.96 * se_fit
new_data$upper_log_odds <- new_data$predicted_log_odds + 1.96 * se_fit

#    Convert log-odds to probabilities (logistic transformation for confidence intervals)
new_data$lower_ci <- plogis(new_data$lower_log_odds)
new_data$upper_ci <- plogis(new_data$upper_log_odds)

ggplot(new_data, aes(x =   wealth_index_quantiles_fact , y = predicted_prob)) +
  geom_point(size = 1.2) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
  labs(title ="All fish Consumption",
       x = "Wealth",
       y = "Predicted Probability of Consumption") +
  theme_minimal() 

new_data$lower_ci <- plogis(new_data$lower_log_odds)
new_data$upper_ci <- plogis(new_data$upper_log_odds)



p2_b<- ggplot(new_data, aes(x = wealth_index_quantiles_fact, y = predicted_prob)) +
  geom_point(color = "#2e0854", size = 2) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                width = 0.15, color = "#2e0854", linewidth = 0.8) +
  labs(title = "All fish",
       x = "Wealth",
       y = "Probability of Consumption") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.title.position = "plot"
  )

p2_b



p2_b <- ggplot(new_data, aes(x = wealth_index_quantiles_fact, y = predicted_prob, color = wealth_index_quantiles_fact)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                width = 0, linewidth = 1) +
  scale_color_manual(values = c(
    "1" = "#CC0000",   # rojo
    "2" = "#FF6666",   # menos rojo
    "3" = "#CCCCCC",   # gris
    "4" = "#6699FF",   # azul
    "5" = "#0036FF"    # muy azul
  )) +
  labs(title = "All fish",
       x = "Wealth",
       y = "Probability of Consumption",
       color = "Wealth") +  # optional: legend title
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.title.position = "plot"
  )

p2_b

p2_b <- ggplot(new_data, aes(x = wealth_index_quantiles_fact, y = predicted_prob, color = wealth_index_quantiles_fact)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                width = 0, linewidth = 1) +
  scale_color_manual(values = c(
    "1" = "#CC0000",   # rojo
    "2" = "#FF6666",   # menos rojo
    "3" = "#CCCCCC",   # gris
    "4" = "#6699FF",   # azul
    "5" = "#0036FF"    # muy azul
  )) +
  labs(title = NULL,
       x = "Wealth",
       y = "Probability of Consumption",
       color = "Wealth") +  # optional: legend title
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.title.position = "plot",
    legend.position = "none"   
  )

p2_b




#####1.3.2. Coefficient plot all fish####

library(broom)
# Tidy the model to extract coefficients and confidence intervals
coef_df <- tidy(fish_cons_mod_2.b_w_fact_NO_fish, conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
# Reorder the terms to match the order in the model specification
coef_df$term <- factor(coef_df$term, levels = c("distance_to_marine_km_sc",
                                                "distance_to_inland_km_sc",
                                                "tt_NearMarket_sc",
                                                "urban_rural_fact2",
                                                "any_adult_woman_employed_fact1",
                                                "wealth_index_quantiles_fact5",
                                                "wealth_index_quantiles_fact4",
                                                "wealth_index_quantiles_fact3",
                                                "wealth_index_quantiles_fact2"
                                                
))

coef_df$term <- 
  dplyr::recode(coef_df$term,
                "distance_to_marine_km_sc" = "Distance to Ocean",
                "distance_to_inland_km_sc" = "Distance to indland waterbodies",
                "tt_NearMarket_sc" = "Travel Time to Markets",
                "urban_rural_fact2"="Rural",
                "any_adult_woman_employed_fact1"= "Employment status women",
                "wealth_index_quantiles_fact5" = "Wealth Q5",
                "wealth_index_quantiles_fact4" = "Wealth Q4",
                "wealth_index_quantiles_fact3" = "Wealth Q3",
                "wealth_index_quantiles_fact2" = "Wealth Q2")



# Create a new column to indicate positive or negative coefficients
coef_df$coef_color <- ifelse(coef_df$estimate > 0, "#5F9EA0", "#CD3333")

#Create caterpillar plot with color based on coefficient sign
# Create caterpillar plot with color based on coefficient sign and no legend
ggplot(coef_df, aes(x = term, y = estimate, color = coef_color)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  coord_flip() +
  labs(title = "NO Consumption of Fish",
       y = "Estimate (log-odds scale)") +
  scale_color_manual(values = c("#5F9EA0", "#CD3333")) +  # Manually set color scale
  theme_classic() +
  guides(color = "none") +# Remove the color legend
  theme(axis.title.y = element_blank())  

#extract coef

coefs <- summary(fish_cons_mod_2.b_w_fact_all_fish)$coefficients
ORs <- exp(coefs[, "Estimate"])

CI_lower <- exp(coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"])
CI_upper <- exp(coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"])

OR_table <- data.frame(
  OR = ORs,
  CI_lower = CI_lower,
  CI_upper = CI_upper,
  p_value = coefs[, "Pr(>|t|)"]
)


####2.Models fresh sardinella#####

LSMS_18_clean$fresh_sardinella_int <- as.integer(LSMS_18_clean$fresh_sardinella)

any(is.na(LSMS_18_clean$fresh_sardinella_int ))
which(any(is.na(LSMS_18_clean$fresh_sardinella_int )))

##create a dataset withouth na for fresh sardinella
LSMS_18_clean_d <- LSMS_18_clean%>%
  filter(!is.na(fresh_sardinella_int))

any(is.na(LSMS_18_clean_d$fresh_sardinella_int ))#no NAs


LSMS_18_clean_d$wealth_index_quantiles_fact<-as.factor(LSMS_18_clean_d$wealth_index_quantiles_fact)

#Survey desing
DHSdesign_lsms <- svydesign(id = ~hh_cluster, 
                            strata = ~region_urban_rural,
                            weights = ~hhweight, 
                            data = LSMS_18_clean_d)

model_fresh_sardinella<- svyglm(fresh_sardinella_int ~ 
                                  wealth_index_quantiles_fact + 
                                  tt_NearMarket_sc+
                                  urban_rural_fact+
                                  any_adult_woman_employed_fact+
                                  distance_to_inland_km_sc+
                                  distance_to_marine_km_sc,
                                design = DHSdesign_lsms, 
                                family = quasibinomial())

#####2.1.Diagnostics#####
pseudo_r2 <- 1 - (model_fresh_sardinella$deviance / model_fresh_sardinella$null.deviance)


#Pearson residuals
pearson_resid <- residuals(model_fresh_sardinella, type = "pearson")

#Deviance residuals
deviance_resid <- residuals(model_fresh_sardinella, type = "deviance")

#Fitted values
fitted_vals <- fitted(model_fresh_sardinella)

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

# Residuals diagnostic plot
plot(
  residuals(model_fresh_sardinella, type="pearson"),
  ylab = "Residuals", 
  main = "Model Residuals Diagnostic"
)
abline(h = 0, col = "red", lty = 2)

# Check overdispersion estimate
dispersion_value <- summary(model_fresh_sardinella)$dispersion
print(paste("Dispersion parameter:", round(dispersion_value, 4)))

# Predict fitted probabilities
predicted_prob <- predict(model_fresh_sardinella, type = "response")
head(predicted_prob)
# Binned residual plot: residuals vs predicted probabilities
residuals_pearson <- residuals(model_fresh_sardinella, type = "pearson")
binnedplot(
  x = predicted_prob,
  y = residuals_pearson,
  xlab = "Predicted probabilities",
  ylab = "Pearson residuals",
  main = "Binned Residual Plot"
)

svymean(~fresh_sardinella_int, design = DHSdesign_lsms)

svyby(~fresh_sardinella_int, ~urban_rural_fact, design = DHSdesign_lsms, FUN = svymean)
svyby(~fresh_sardinella_int, ~wealth_index_quantiles_fact, design = DHSdesign_lsms, FUN = svymean)

# Check distance variables - are there extreme values?
summary(LSMS_18_clean_d$distance_to_marine_km_sc)
summary(LSMS_18_clean_d$distance_to_inland_km_sc)



####2.2.Results#####

summary(model_fresh_sardinella)

plot_model(model_fresh_sardinella)

ggpredict(model_fresh_sardinella,
          terms = c("distance_to_marine_km_sc [-1:3 by=0.05]")) %>%
  plot()

ggpredict(model_fresh_sardinella,
          terms = c("wealth_index_quantiles_fact")) %>%
  plot()

#wealth and fish consumption plot

#predictions
new_data <- data.frame(
  wealth_index_quantiles_fact = factor(1:5, levels = 1:5, labels = c("1", "2", "3", "4", "5")),
  urban_rural_fact = factor("1", levels = levels(DHSdesign_lsms$variables$urban_rural_fact)),
  tt_NearMarket_sc = mean(DHSdesign_lsms$variables$tt_NearMarket_sc, na.rm = TRUE),
  distance_to_inland_km_sc = mean(DHSdesign_lsms$variables$distance_to_inland_km_sc, na.rm = TRUE),
  distance_to_marine_km_sc = mean(DHSdesign_lsms$variables$distance_to_marine_km_sc, na.rm = TRUE),
  any_adult_woman_employed_fact= factor("1", levels = levels(DHSdesign_lsms$variables$any_adult_woman_employed_fact))
)


new_data$predicted_log_odds <- predict(model_fresh_sardinella, newdata = new_data, type = "link")

#convert log-odds to probabilities (logistic transformation)
new_data$predicted_prob <- plogis(new_data$predicted_log_odds)

# Calculate confidence intervals
# First, get the design matrix

X <- model.matrix(delete.response(terms(model_fresh_sardinella)), data = new_data)

#    Calculate standard errors using the variance-covariance matrix of the model
se_fit <- sqrt(diag(X %*% vcov(model_fresh_sardinella) %*% t(X)))
#    Compute confidence intervals on the log-odds scale
new_data$lower_log_odds <- new_data$predicted_log_odds - 1.96 * se_fit
new_data$upper_log_odds <- new_data$predicted_log_odds + 1.96 * se_fit

#    Convert log-odds to probabilities (logistic transformation for confidence intervals)
new_data$lower_ci <- plogis(new_data$lower_log_odds)
new_data$upper_ci <- plogis(new_data$upper_log_odds)

ggplot(new_data, aes(x =   wealth_index_quantiles_fact , y = predicted_prob)) +
  geom_point(size = 1.2) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
  labs(title ="Fresh sardinella",
       x = "Wealth",
       y = "Predicted Probability of Consumption") +
  theme_minimal() 

new_data$lower_ci <- plogis(new_data$lower_log_odds)
new_data$upper_ci <- plogis(new_data$upper_log_odds)



p3 <- ggplot(new_data, aes(x = wealth_index_quantiles_fact, y = predicted_prob)) +
  geom_point(color = "#2e0854", size = 2) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                width = 0.15, color = "#2e0854", linewidth = 0.8) +
  labs(title = "Fresh sardinella",
       x = "Wealth",
       y = "Probability of Consumption") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.title.position = "plot"
  )

p3



p3 <- ggplot(new_data, aes(x = wealth_index_quantiles_fact, y = predicted_prob, color = wealth_index_quantiles_fact)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                width = 0, linewidth = 1) +
  scale_color_manual(values = c(
    "1" = "#CC0000",   # rojo
    "2" = "#FF6666",   # menos rojo
    "3" = "#CCCCCC",   # gris
    "4" = "#6699FF",   # azul
    "5" = "#0036FF"    # muy azul
  )) +
  labs(title = "Fresh sardinella",
       x = "Wealth",
       y = "Probability of Consumption",
       color = "Wealth") +  # optional: legend title
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.title.position = "plot"
  )

p3

p3 <- ggplot(new_data, aes(x = wealth_index_quantiles_fact, y = predicted_prob, color = wealth_index_quantiles_fact)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                width = 0, linewidth = 1) +
  scale_color_manual(values = c(
    "1" = "#CC0000",   # rojo
    "2" = "#FF6666",   # menos rojo
    "3" = "#CCCCCC",   # gris
    "4" = "#6699FF",   # azul
    "5" = "#0036FF"    # muy azul
  )) +
  labs(title = NULL,
       x = "Wealth",
       y = NULL,
       color = NULL) +  # optional: legend title
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.title.position = "plot",
    legend.position = "none"   
  )


p3

#plot consumption

DHSdesign_lsms <- svydesign(id = ~hh_cluster, 
                            strata = ~region_urban_rural,
                            weights = ~hhweight, 
                            data = LSMS_18_clean_d)

fresh_sard_table<-svytable(~ fresh_sardinella_fact, design=DHSdesign_lsms)%>%
  as.data.frame()

fresh_sard_table_b<- fresh_sard_table%>%
  mutate(percent = Freq / sum(Freq) * 100)


plot_fresh_sard_b<-ggplot(fresh_sard_table_b, aes(x = fresh_sardinella_fact, y = percent)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Consumption",
    y = "Percentage (%)",
    title = "Fresh sardinella"
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_classic() +
  theme(
    plot.title = element_text(
      hjust = 0.5,          
      face = "bold",        
      size = 16             
    )
  )



####3.Dried fish#####

LSMS_18_clean$dried_fish_int <- as.integer(LSMS_18_clean$dried_fish)

any(is.na(LSMS_18_clean$dried_fish_int))

which(any(is.na(LSMS_18_clean$dried_fish_int)))

##create a dataset withouth na 
LSMS_18_clean_f <- LSMS_18_clean%>%
  filter(!is.na(dried_fish_int))

any(is.na(LSMS_18_clean_f$dried_fish_int))#no NAs

#Survey desing
DHSdesign_lsms <- svydesign(id = ~hh_cluster, 
                            strata = ~region_urban_rural,
                            weights = ~hhweight, 
                            data = LSMS_18_clean_f)


model_dried_fish<- svyglm(dried_fish_int ~ 
                            wealth_index_quantiles_fact + 
                            tt_NearMarket_sc+
                            urban_rural_fact+
                            any_adult_woman_employed_fact+
                            distance_to_inland_km_sc+
                            distance_to_marine_km_sc , 
                          design = DHSdesign_lsms, 
                          family = quasibinomial())



#####3.1.Diagnostics#####

pseudo_r2 <- 1 - (model_dried_fish$deviance / model_dried_fish$null.deviance)


#Pearson residuals
pearson_resid <- residuals(model_dried_fish, type = "pearson")

#Deviance residuals
deviance_resid <- residuals(model_dried_fish, type = "deviance")

#Fitted values
fitted_vals <- fitted(model_dried_fish)

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

# Residuals diagnostic plot
plot(
  residuals(model_dried_fish, type="pearson"),
  ylab = "Residuals", 
  main = "Model Residuals Diagnostic"
)
abline(h = 0, col = "red", lty = 2)

# Check overdispersion estimate
dispersion_value <- summary(model_dried_fish)$dispersion
print(paste("Dispersion parameter:", round(dispersion_value, 4)))

# Predict fitted probabilities
predicted_prob <- predict(model_dried_fish, type = "response")
head(predicted_prob)
# Binned residual plot: residuals vs predicted probabilities
residuals_pearson <- residuals(model_dried_fish, type = "pearson")
binnedplot(
  x = predicted_prob,
  y = residuals_pearson,
  xlab = "Predicted probabilities",
  ylab = "Pearson residuals",
  main = "Binned Residual Plot"
  
)

car::vif(model_dried_fish)

###3.2.Results####

summary(model_dried_fish)

plot_model(model_dried_fish)

ggpredict(model_dried_fish,
          terms = c("distance_to_marine_km_sc [-1:3 by=0.05]")) %>%
  plot()

ggpredict(model_dried_fish,
          terms = c("wealth_index_quantiles_fact")) %>%
  plot()


#wealth and fish consumption plot

#predictions
new_data <- data.frame(
  wealth_index_quantiles_fact = factor(1:5, levels = 1:5, labels = c("1", "2", "3", "4", "5")),
  urban_rural_fact = factor("1", levels = levels(DHSdesign_lsms$variables$urban_rural_fact)),
  tt_NearMarket_sc = mean(DHSdesign_lsms$variables$tt_NearMarket_sc, na.rm = TRUE),
  distance_to_inland_km_sc = mean(DHSdesign_lsms$variables$distance_to_inland_km_sc, na.rm = TRUE),
  distance_to_marine_km_sc = mean(DHSdesign_lsms$variables$distance_to_marine_km_sc, na.rm = TRUE),
  any_adult_woman_employed_fact= factor("1", levels = levels(DHSdesign_lsms$variables$any_adult_woman_employed_fact))
)


new_data$predicted_log_odds <- predict(model_dried_fish, newdata = new_data, type = "link")

#convert log-odds to probabilities (logistic transformation)
new_data$predicted_prob <- plogis(new_data$predicted_log_odds)

# Calculate confidence intervals
# First, get the design matrix

X <- model.matrix(delete.response(terms(model_dried_fish)), data = new_data)

#    Calculate standard errors using the variance-covariance matrix of the model
se_fit <- sqrt(diag(X %*% vcov(model_dried_fish) %*% t(X)))
#    Compute confidence intervals on the log-odds scale
new_data$lower_log_odds <- new_data$predicted_log_odds - 1.96 * se_fit
new_data$upper_log_odds <- new_data$predicted_log_odds + 1.96 * se_fit

#    Convert log-odds to probabilities (logistic transformation for confidence intervals)
new_data$lower_ci <- plogis(new_data$lower_log_odds)
new_data$upper_ci <- plogis(new_data$upper_log_odds)

ggplot(new_data, aes(x =   wealth_index_quantiles_fact , y = predicted_prob)) +
  geom_point(size = 1.2) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
  labs(title ="Dried fish",
       x = "Wealth",
       y = "Predicted Probability of Consumption") +
  theme_minimal() 

new_data$lower_ci <- plogis(new_data$lower_log_odds)
new_data$upper_ci <- plogis(new_data$upper_log_odds)



p4 <- ggplot(new_data, aes(x = wealth_index_quantiles_fact, y = predicted_prob)) +
  geom_point(color = "#2e0854", size = 2) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                width = 0.15, color = "#2e0854", linewidth = 0.8) +
  labs(title = "Dried fish",
       x = "Wealth",
       y = "Probability of Consumption") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.title.position = "plot"
  )

p4



p4 <- ggplot(new_data, aes(x = wealth_index_quantiles_fact, y = predicted_prob, color = wealth_index_quantiles_fact)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                width = 0, linewidth = 1) +
  scale_color_manual(values = c(
    "1" = "#CC0000",   # rojo
    "2" = "#FF6666",   # menos rojo
    "3" = "#CCCCCC",   # gris
    "4" = "#6699FF",   # azul
    "5" = "#0036FF"    # muy azul
  )) +
  labs(title = "Dried fish",
       x = "Wealth",
       y = "Probability of Consumption",
       color = "Wealth") +  # optional: legend title
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.title.position = "plot"
  )

p4

p4 <- ggplot(new_data, aes(x = wealth_index_quantiles_fact, y = predicted_prob, color = wealth_index_quantiles_fact)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                width = 0, linewidth = 1) +
  scale_color_manual(values = c(
    "1" = "#CC0000",   # rojo
    "2" = "#FF6666",   # menos rojo
    "3" = "#CCCCCC",   # gris
    "4" = "#6699FF",   # azul
    "5" = "#0036FF"    # muy azul
  )) +
  labs(title = NULL,
       x = "Wealth",
       y = NULL,
       color = NULL) +  # optional: legend title
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.title.position = "plot",
    legend.position = "none"   
  )
p4

#plot consumption

DHSdesign_lsms <- svydesign(id = ~hh_cluster, 
                            strata = ~region_urban_rural,
                            weights = ~hhweight, 
                            data = LSMS_18_clean_f)

dried_fish_table<-svytable(~ dried_fish_fact, design=DHSdesign_lsms)%>%
  as.data.frame()

dried_fish_table_b<- dried_fish_table%>%
  mutate(percent = Freq / sum(Freq) * 100)


plot_dried_fish_b<-ggplot(dried_fish_table_b, aes(x = dried_fish_fact, y = percent)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Consumption",
    y = "Percentage (%)",
    title = "Dried fish"
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_classic() +
  theme(
    plot.title = element_text(
      hjust = 0.5,          
      face = "bold",        
      size = 16             
    )
  )

######4.Other fresh fish####

LSMS_18_clean$fresh_other_fact<- as.factor(LSMS_18_clean$fresh_other)

LSMS_18_clean$fresh_other_int<- as.integer(LSMS_18_clean$fresh_other)

any(is.na(LSMS_18_clean$fresh_other_int))

which(any(is.na(LSMS_18_clean$fresh_other_int)))

##create a dataset withouth na for smoked sardinella
LSMS_18_clean_g <- LSMS_18_clean%>%
  filter(!is.na(fresh_other_int))

any(is.na(LSMS_18_clean_g$fresh_other_int))#no NAs


#Survey design
DHSdesign_lsms <- svydesign(id = ~hh_cluster, 
                            strata = ~region_urban_rural,
                            weights = ~hhweight, 
                            data = LSMS_18_clean_g)


model_fresh_other<- svyglm(fresh_other_int ~ 
                             wealth_index_quantiles_fact + 
                             tt_NearMarket_sc+
                             urban_rural_fact+
                             any_adult_woman_employed_fact+
                             distance_to_inland_km_sc+
                             distance_to_marine_km_sc , 
                           design = DHSdesign_lsms, 
                           family = quasibinomial())
######4.1.Diagnostics#####


pseudo_r2 <- 1 - (model_fresh_other$deviance / model_fresh_other$null.deviance)


#Pearson residuals
pearson_resid <- residuals(model_fresh_other, type = "pearson")

#Deviance residuals
deviance_resid <- residuals(model_fresh_other, type = "deviance")

#Fitted values
fitted_vals <- fitted(model_fresh_other)

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

# Residuals diagnostic plot
plot(
  residuals(model_dried_fish, type="pearson"),
  ylab = "Residuals", 
  main = "Model Residuals Diagnostic"
)
abline(h = 0, col = "red", lty = 2)

# Check overdispersion estimate
dispersion_value <- summary(model_fresh_other)$dispersion
print(paste("Dispersion parameter:", round(dispersion_value, 4)))

# Predict fitted probabilities
predicted_prob <- predict(model_fresh_other, type = "response")
head(predicted_prob)
# Binned residual plot: residuals vs predicted probabilities
residuals_pearson <- residuals(model_fresh_other, type = "pearson")
binnedplot(
  x = predicted_prob,
  y = residuals_pearson,
  xlab = "Predicted probabilities",
  ylab = "Pearson residuals",
  main = "Binned Residual Plot"
  
)

car::vif(model_fresh_other)

####5.2.Results########

summary(model_fresh_other)

plot_model(model_fresh_other)

car::vif(model_fresh_other)


ggpredict(model_fresh_other,
          terms = c("distance_to_marine_km_sc [-1:3 by=0.05]")) %>%
  plot()

ggpredict(model_fresh_other,
          terms = c("wealth_index_quantiles_fact")) %>%
  plot()



#wealth and fish consumption plot

#predictions
new_data <- data.frame(
  wealth_index_quantiles_fact = factor(1:5, levels = 1:5, labels = c("1", "2", "3", "4", "5")),
  urban_rural_fact = factor("1", levels = levels(DHSdesign_lsms$variables$urban_rural_fact)),
  tt_NearMarket_sc = mean(DHSdesign_lsms$variables$tt_NearMarket_sc, na.rm = TRUE),
  distance_to_inland_km_sc = mean(DHSdesign_lsms$variables$distance_to_inland_km_sc, na.rm = TRUE),
  distance_to_marine_km_sc = mean(DHSdesign_lsms$variables$distance_to_marine_km_sc, na.rm = TRUE),
  any_adult_woman_employed_fact= factor("1", levels = levels(DHSdesign_lsms$variables$any_adult_woman_employed_fact))
)


new_data$predicted_log_odds <- predict(model_fresh_other, newdata = new_data, type = "link")

#convert log-odds to probabilities (logistic transformation)
new_data$predicted_prob <- plogis(new_data$predicted_log_odds)

# Calculate confidence intervals
# First, get the design matrix

X <- model.matrix(delete.response(terms(model_fresh_other)), data = new_data)

#    Calculate standard errors using the variance-covariance matrix of the model
se_fit <- sqrt(diag(X %*% vcov(model_fresh_other) %*% t(X)))
#    Compute confidence intervals on the log-odds scale
new_data$lower_log_odds <- new_data$predicted_log_odds - 1.96 * se_fit
new_data$upper_log_odds <- new_data$predicted_log_odds + 1.96 * se_fit

#    Convert log-odds to probabilities (logistic transformation for confidence intervals)
new_data$lower_ci <- plogis(new_data$lower_log_odds)
new_data$upper_ci <- plogis(new_data$upper_log_odds)

ggplot(new_data, aes(x =   wealth_index_quantiles_fact , y = predicted_prob)) +
  geom_point(size = 1.2) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
  labs(title ="Other fresh fish",
       x = "Wealth",
       y = "Predicted Probability of Consumption") +
  theme_minimal() 

new_data$lower_ci <- plogis(new_data$lower_log_odds)
new_data$upper_ci <- plogis(new_data$upper_log_odds)



p5 <- ggplot(new_data, aes(x = wealth_index_quantiles_fact, y = predicted_prob)) +
  geom_point(color = "#2e0854", size = 2) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                width = 0.15, color = "#2e0854", linewidth = 0.8) +
  labs(title = "Other fresh fish",
       x = "Wealth",
       y = "Probability of Consumption") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.title.position = "plot"
  )

p5



p5 <- ggplot(new_data, aes(x = wealth_index_quantiles_fact, y = predicted_prob, color = wealth_index_quantiles_fact)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                width = 0, linewidth = 1) +
  scale_color_manual(values = c(
    "1" = "#CC0000",   # rojo
    "2" = "#FF6666",   # menos rojo
    "3" = "#CCCCCC",   # gris
    "4" = "#6699FF",   # azul
    "5" = "#0036FF"    # muy azul
  )) +
  labs(title = "Other fresh fish",
       x = "Wealth",
       y = "Probability of Consumption",
       color = "Wealth") +  # optional: legend title
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.title.position = "plot"
  )

p5

p5 <- ggplot(new_data, aes(x = wealth_index_quantiles_fact, y = predicted_prob, color = wealth_index_quantiles_fact)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                width = 0, linewidth = 1) +
  scale_color_manual(values = c(
    "1" = "#CC0000",   # rojo
    "2" = "#FF6666",   # menos rojo
    "3" = "#CCCCCC",   # gris
    "4" = "#6699FF",   # azul
    "5" = "#0036FF"    # muy azul
  )) +
  labs(title = NULL,
       x = "Wealth",
       y = NULL,
       color = "Wealth") +  # optional: legend title
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.title.position = "plot"
  )


p5


#plot consumption

DHSdesign_lsms <- svydesign(id = ~hh_cluster, 
                            strata = ~region_urban_rural,
                            weights = ~hhweight, 
                            data = LSMS_18_clean_g)


fresh_other_fish_table<-svytable(~ fresh_other_fact, design=DHSdesign_lsms)%>%
  as.data.frame()

fresh_other_fish_table_b<- fresh_other_fish_table%>%
  mutate(percent = Freq / sum(Freq) * 100)


plot_fresh_other_fish_b<-ggplot(fresh_other_fish_table_b, aes(x = fresh_other_fact, y = percent)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Consumption",
    y = "Percentage (%)",
    title = "Other fresh fish"
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_classic() +
  theme(
    plot.title = element_text(
      hjust = 0.5,          
      face = "bold",        
      size = 16             
    )
  )

## Consumption of different groups by wealth 

combined <- (p2 + p3 + p4+p5)+ 
  plot_layout(ncol = 4)&
  theme(
    axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 16),
    #axis.title.x = element_blank(),  # remove all individual x labels, we add a shared one later
    axis.text = element_text(size = 14)
  )


combined 


#combine percentage consumptions and consumption by wealth plots


combined <- plot_all_fish_b + plot_fresh_sard_b + plot_dried_fish_b + plot_fresh_other_fish_b+
  p2 + p3 + p4 + p5 +
  plot_layout(ncol = 4, nrow = 2) &   # 4 columns and 2 rows forced
  theme(
    axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 16),
    axis.text = element_text(size = 14)
  )

combined

