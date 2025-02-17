# Setting working directory

setwd("~/Library/CloudStorage/OneDrive-UniversityofStrathclyde/Fifth Year/MM916/Project 2")

# Loading necessary packages

library(ggplot2)
library(MASS)
library(tidyverse)
library(corrplot)
library(leaps)

# Loading in data

real <- read.csv("real_est.csv")
head(real)
view(real)
summary(real)

# Exploratory data analysis

par(mfrow = c(1,1))
corrplot(cor(real), method = 'number', bg = 'lightgrey') # Correlation plot

# Addressing potential multicollinearity

ggplot(realestate, aes(x = Room, y = Space)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(y = 'Space (sq ft)',
       x = 'No. of Rooms',
       title = 'No. of Rooms vs Space') +
  theme_minimal()

ggplot(realestate, aes(x = Bedroom, y = Room)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(y = 'No. of Rooms',
       x = 'No. of Bedrooms',
       title = 'No. of Rooms vs Bedrooms') +
  theme_minimal()

# Room is not considered as a potential predictor

# Looking at relationship of potential predictors

noroom <- real %>%
  dplyr::select(!Room)

plot(noroom)

# Converting condition to a categorical variable

realestate <- real %>% 
  mutate(Condition = as.factor(ifelse(Condition == 1, "Good", "Otherwise")))
str(realestate)

# Boxplot of Price vs Condition

ggplot(realestate, aes(x = Condition, y = Price, group = Condition)) +
  geom_boxplot() +
  labs(y = 'Price (10k US Dollars)',
       x = 'Condition',
       title = 'Price of House vs Condition') +
  theme_minimal()

# Comparing properties in good condition and otherwise

table(realestate$Condition)

# Now looking at each of the variables we will be looking at scatterplots and diagnostic plots

# Space

ggplot(realestate, aes(x = Space, y = Price, colour = Condition)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(y = 'Price (10k US Dollars)',
       x = 'Space (sq ft)',
       title = 'Price of House vs Size of House') +
  scale_colour_manual(values = c("Good" = "blue", "Otherwise" = "red")) +
  theme_minimal()

z <- lm(Price ~ Space, data = realestate)
par(mfrow = c(2,2))
plot(z)

# Transforming space with log transformation

realestate <- realestate %>% 
  mutate(log_Space = log(Space))

ggplot(realestate, aes(x = log_Space, y = Price, colour = Condition)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(y = 'Price (10k US Dollars)',
       x = 'Log of Space',
       title = 'Price of House vs The Log of Space') +
  scale_colour_manual(values = c("Good" = "blue", "Otherwise" = "red")) +
  theme_minimal()

z2 <- lm(Price ~ log_Space, data = realestate)
par(mfrow = c(2,2))
plot(z2)

# Garage

ggplot(realestate, aes(x = Garage, y = Price, colour = Condition)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(y = 'Price (10k US Dollars)',
       x = 'No. of Garages',
       title = 'Price of House vs No. of Garages') +
  scale_colour_manual(values = c("Good" = "blue", "Otherwise" = "red")) +
  theme_minimal()

z3 <- lm(Price ~ Garage, data = realestate)
par(mfrow = c(2,2))
plot(z3)

# Bathroom

ggplot(realestate, aes(x = Bathroom, y = Price, colour = Condition)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(y = 'Price (10k US Dollars)',
       x = 'No. of Bathrooms',
       title = 'Price of House vs No. of Bathrooms') +
  scale_colour_manual(values = c("Good" = "blue", "Otherwise" = "red")) +
  theme_minimal()

z4 <- lm(Price ~ Bathroom, data = realestate)
par(mfrow = c(2,2))
plot(z4)

# Tax

ggplot(realestate, aes(x = Tax, y = Price)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(y = 'Price (10k US Dollars)',
       x = 'Annual Tax (US Dollars)',
       title = 'Price of House vs Annual Tax') +
  theme_minimal()


ggplot(realestate, aes(x = Tax, y = Price, colour = Condition)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(y = 'Price (10k US Dollars)',
       x = 'Annual Tax (US Dollars)',
       title = 'Price of House vs Annual Tax') +
  scale_colour_manual(values = c("Good" = "blue", "Otherwise" = "red")) +
  theme_minimal()

z5 <- lm(Price ~ Tax, data = realestate)
par(mfrow = c(2,2))
plot(z5)

par(mfrow = c(1,1))
boxcox(z5)
title('Boxcox Transformation for Tax Model')

?boxcox

# Transforming tax variable with log transformation

realestate <- realestate %>% 
  mutate(log_Tax = log(Tax))

ggplot(realestate, aes(x = log_Tax, y = Price)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(y = 'Price (10k US Dollars)',
       x = 'Annual Tax (US Dollars)',
       title = 'Price of House vs Annual Tax') +
  theme_minimal()

ggplot(realestate, aes(x = log_Tax, y = Price, colour = Condition)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(y = 'Price (10k US Dollars)',
       x = 'Log of Annual Tax (US Dollars)',
       title = 'Price of House vs The Log of Annual Tax') +
  scale_colour_manual(values = c("Good" = "blue", "Otherwise" = "red")) +
  theme_minimal()

z6 <- lm(Price ~ log_Tax, data = realestate)
par(mfrow = c(2,2))


summary(z6)

# Removing tax outliers

taxless_realestate <- realestate %>%
  filter(Tax < 2000)

ggplot(taxless_realestate, aes(x = Tax, y = Price, colour = Condition)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(y = 'Price (10k US Dollars)',
       x = 'Annual Tax (US Dollars)',
       title = 'Price of House vs Annual Tax') +
  scale_colour_manual(values = c("Good" = "blue", "Otherwise" = "red")) +
  theme_minimal()

zz <- lm(Price ~ Tax, data = taxless_realestate)
par(mfrow = c(2,2))
plot(zz)

# Lot

ggplot(realestate, aes(x = Lot, y = Price, colour = Condition)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(y = 'Price (10k US Dollars)',
       x = 'Width of Lot',
       title = 'Price of House vs Width of Lot') +
  scale_colour_manual(values = c("Good" = "blue", "Otherwise" = "red")) +
  theme_minimal()

z7 <- lm(Price ~ Lot, data = realestate)
par(mfrow = c(2,2))
plot(z7)

# Bedroom

ggplot(realestate, aes(x = Bedroom, y = Price, colour = Condition)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(y = 'Price (10k US Dollars)',
       x = 'No. of Bedrooms',
       title = 'Price of House vs No. of Bedrooms') +
  scale_colour_manual(values = c("Good" = "blue", "Otherwise" = "red")) +
  theme_minimal()

z8 <- lm(Price ~ Bedroom, data = realestate)
par(mfrow = c(2,2))
plot(z8)






