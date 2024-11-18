# Load required libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(ggplot2)
library(nlme)
library(lattice)

# Read the data
data <- read.csv("Data.csv", header=TRUE)

# Subset to remove Enclosure values
data <- data[data$Enclosure == "0", ]

## DATA PREPARATION
# Convert categorical variables to factors
data$Field <- as.factor(data$Field)
data$Plot <- as.factor(data$Plot)
data$Treatment <- as.factor(data$Treatment)
data$Enclosure <- as.factor(data$Enclosure)

# Check the structure of the data
str(data)

# Remove rows with missing Height values
data <- data |>
  filter(!is.na(Height))

#-------------------------------------------------------------------------------
# Exploratory Data Analysis: Height
#-------------------------------------------------------------------------------

# Cleveland Dot Plot to identify outliers

dotchart(data$Height,
         groups = data$Treatment,
         ylab = "Treatment", xlab = "Height",
         main = "Cleveland dotplot", pch = data$WAT)

# Pair plot 

pairs(data,
      main = "Pair Plot of Numeric Variables",
      pch = 19, col = "blue")

# Box Plot looking at distribution

boxplot(Height ~ Treatment * WAT,
        data = data, 
        varwidth = TRUE,
        col = data$WAT, # Color each WAT group differently
        xlab = "Treatment and WAT",
        ylab = "Height",
        main = "Boxplot of Height by Treatment and WAT")

# Calculate mean Height for each Treatment and WAT combination
summary_data <- data |>
  group_by(Treatment, WAT) |>
  summarise(mean_height = mean(Height, na.rm = TRUE))

# Create a plot
ggplot(summary_data, aes(x = WAT, y = mean_height, color = Treatment, group = Treatment)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Height by Treatment over Time",
       x = "Weeks After Treatment",
       y = "Mean Height") +
  theme_minimal()

# Box plot to visualize distribution over time
# Box plot with centered, larger title and two legends: one for Treatment and one for WAT
ggplot(data, aes(x = Treatment, y = Height, fill = Treatment)) +
  geom_boxplot() +
  facet_wrap(~ WAT, nrow = 1) +  # Facet in a single row
  labs(title = "Height Distribution by Treatment over Time",
       x = "Treatment",
       y = "Height") +
  # Create a dummy aesthetic for WAT to generate a second legend
  geom_point(aes(color = factor(WAT)), alpha = 0) +  # Invisible points to create the WAT legend
  scale_color_discrete(name = "Weeks After Treatment") +  # Legend for WAT
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 14)  # Center title and increase font size by 2 points (default is ~12)
  )

#-------------------------------------------------------------------------------
# Linear Regression: Height
#-------------------------------------------------------------------------------

# Linear Regression for Height

lm_1 <- lm(formula = Height ~ Treatment, data = data)
summary(lm_1)

lm_2 <- lm(formula = Height ~ Treatment * WAT, data = data)
summary(lm_2) <- lm(formula = Height ~ Treatment * WAT, data = data)

lm_3 <- lm(formula = Height ~ Treatment * WAT * Field, data = data)
summary(lm_3)

#ANOVA for model simplicity
anova(lm_1, lm_2, lm_3)

# AIC to pick the best model
AIC(lm_1, lm_2, lm_3)

# Model lm_height_3 has the lowest AIC

# Model Validation

plot(lm_3)

#-------------------------------------------------------------------------------
# Mixed Effects Model: Height
#-------------------------------------------------------------------------------

# Linear Mixed Effects Model

# Fit the mixed-effects model
library(lme4)
library(lmerTest)

lmer_1 <- lmer(Height ~ Treatment + (1|Field), data = data)

lmer_2 <- lmer(Height ~ Treatment * WAT + (1|Field), data = data)

lmer_3 <- lmer(Height ~ Treatment + (1|Field/Plot), data = data)

lmer_4 <- lmer(Height ~ Treatment * WAT + (1|Field/Plot), data = data)

# Null model (only random effects)
lmer_null <- lmer(Height ~ 1 + (1|Field/Plot), data = data)

# Compare models using likelihood ratio test
anova(lmer_1, lmer_2, lmer_3, lmer_4, lmer_null)

# AIC comparison
AIC(lmer_1, lmer_2, lmer_3, lmer_4, lmer_null)

# BIC comparison
BIC(lmer_1, lmer_2, lmer_3, lmer_4, lmer_null)

# Summary of Best fitting model
summary(lmer_4)

# Model Diagnostics

# 1) Extract residuals
residuals <- resid(lmer_4)
fitted <- fitted(lmer_4)

# Plot residuals vs fitted values
plot(fitted, residuals, xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# 2) Normality in Residuals

qqnorm(residuals)
qqline(residuals, col = "red")

# 4) Random Effects Diagnoses 

ranef_diag <- ranef(lmer_4, condVar = TRUE)
dotplot(ranef_diag)  # Visualize random effects

print(ranef_diag)

#-------------------------------------------------------------------------------
# Mixed Effects Model with Log transformation: Height
#-------------------------------------------------------------------------------

# Linear Mixed Effects Model with Log transformation

# Log Transformation to account for outliers identified in QQ plot

# Fit the mixed-effects model with log transformation on best model

lmer_log <- lmer(log(Height) ~ Treatment * WAT + (1|Field/Plot), data = data)

# AIC comparison between log and non-log model
AIC(lmer_4, lmer_log)

# BIC comparison
BIC(lmer_4, lmer_log)

# Log transformed model has drastically lower AIC 
# Summary of Best fitting model 
summary(lmer_log)

# Model Diagnostics

# 1) Extract residuals
residuals_log <- resid(lmer_log)
fitted_log <- fitted(lmer_log)

# Plot residuals vs fitted values
plot(fitted_log, residuals_log, xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# 2) Normality in Residuals

qqnorm(residuals_log)
qqline(residuals_log, col = "red")

# 4) Random Effects Diagnoses 

ranef_diag_log <- ranef(lmer_log, condVar = TRUE)
dotplot(ranef_diag)  # Visualize random effects

print(ranef_diag)
