# Setting working directory

setwd("~/Library/CloudStorage/OneDrive-UniversityofStrathclyde/Fifth Year/MM916/Project 2")

# Loading necessary packages

library(ggplot2)
library(tidyverse)
library(MASS)
library(corrplot)
library(leaps)

# Loading in data

real <- read.csv("real_est.csv")
head(real)
view(real)

# Converting condtion to categorical variable

realestate <- real %>% 
  mutate(Condition = as.factor(ifelse(Condition == 1, "Good", "Otherwise")))
str(realestate)

# Transforming tax variable

realestate <- realestate %>% 
  mutate(log_Tax = log(Tax))

# Filtering to only include relevant predictors for leaps and bounds

leapsdata <- realestate %>%
  dplyr::select('Price', 'Bedroom', 'Space', 'Bathroom', 'Garage', 'log_Tax', 'Lot')
str(leapsdata)

# Leaps and bounds regression

best_subset <- leaps(x = leapsdata[,2:7], y = leapsdata[,1],
                     nbest=5, method="adjr2",
                     names=names(leapsdata)[-1])

# Converting to data frame

fit <- data.frame(Size = best_subset$size,
                  `Adjusted R-Squared` = round(best_subset$adjr2, 3),
                  best_subset$which, row.names = NULL)

# Plot of models by size and adjusted r squared

ggplot(fit, aes(Size, Adjusted.R.Squared)) +
  geom_point() +
  labs(title = "Best Subsets by Leaps and Bounds Regression",
       x = "Number of Predictors",
       y = "Adjusted R-Squared") +
  theme_minimal()

# Displaying the best models to be displayed as table

fit <- fit %>%
  arrange(desc(`Adjusted.R.Squared`)) %>%
  mutate('No. of predictors' = Size - 1) %>%
  rename('log(Tax)' = 'log_Tax',
         'Adjusted R-Squared' = 'Adjusted.R.Squared') %>%
  dplyr::select('No. of predictors', 'Adjusted R-Squared', 'Bedroom', 'Space', 
                'Bathroom', 'Garage', 'log(Tax)', 'Lot')
head(fit)

# Creating model from leaps

leaps_model <- lm(Price ~ Bedroom + Space + Bathroom + Garage + log_Tax + Lot, data = realestate)

# Summary and diagnostic plots for leaps

summary(leaps_model)
par(mfrow = c(2,2))
plot(leaps_model)

# Initial model setup for step wise selection

step_model <- lm(Price ~ 1, data = realestate)

summary(step_model)

# Step wise selection

best_step_model <- step(step_model, scope = ~ Bedroom + Space + Bathroom + Garage + log_Tax + Lot + Condition +
                          Bedroom:Condition + Space:Condition + Bathroom:Condition + Garage:Condition + 
                          log_Tax:Condition + Lot:Condition, direction = "both")

step_model <- lm(Price ~ Space + Garage + Lot + log_Tax + Bathroom + Condition + 
                   Bathroom:Condition + Lot:Condition, data = realestate)

summary(step_model)
par(mfrow = c(2,2))
plot(step_model)

# Comparing models

leaps <- summary(leaps_model)
step <- summary(step_model)

# Creating data frame for comparison

df1 <- data.frame(Model = 'Stepwise',
                  `Adjusted R-squared` = round(step$adj.r.squared, 3),
                  `AIC` = AIC(step_model), row.names = NULL)

df2 <- data.frame(Model = 'Leaps and Bounds',
                  `Adjusted R-squared` = round(leaps$adj.r.squared, 3),
                  `AIC` = AIC(leaps_model), row.names = NULL)

comparison <- rbind(df1, df2)
comparison

# From Residuals vs fitted we see these observations

par(mfrow = c(1,1))
plot(step_model)

outliers <- realestate[c(68,84,120),] %>%
  rename(`Log Tax` = log_Tax)

view(outliers)






