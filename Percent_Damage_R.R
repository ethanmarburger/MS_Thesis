# Load required libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(ggplot2)

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
# Exploratory Data Analysis: Percent Damage
#-------------------------------------------------------------------------------

# Calculate mean PercentDamage for each Treatment and WAT combination
summary_data2 <- data |>
  group_by(Treatment, WAT) |>
  summarise(mean_percent = mean(PercentDamage, na.rm = TRUE))

# Create a plot
ggplot(summary_data2, aes(x = WAT, y = mean_percent, color = Treatment, group = Treatment)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Percent Damage by Treatment over Time",
       x = "Weeks After Treatment",
       y = "Mean Percent") +
  theme_minimal()

# Box plot to visualize distribution over time
# Box plot with centered, larger title and two legends: one for Treatment and one for WAT
ggplot(data, aes(x = Treatment, y = PercentDamage, fill = Treatment)) +
  geom_boxplot() +
  facet_wrap(~ WAT, nrow = 1) +  # Facet in a single row
  labs(title = "Percent Damage Distribution by Treatment over Time",
       x = "Treatment",
       y = "Percent Damage") +
  # Create a dummy aesthetic for WAT to generate a second legend
  geom_point(aes(color = factor(WAT)), alpha = 0) +  # Invisible points to create the WAT legend
  scale_color_discrete(name = "Weeks After Treatment") +  # Legend for WAT
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 14)  # Center title and increase font size by 2 points (default is ~12)
  )

#-------------------------------------------------------------------------------
# Linear Regression: Percent Damage
#-------------------------------------------------------------------------------

# Linear Regression for Height

lm_pd_1 <- lm(formula = PercentDamage ~ Treatment, data = data)
summary(lm_pd_1)

lm_pd_2 <- lm(formula = PercentDamage ~ Treatment * WAT, data = data)
summary(lm_pd_2)

lm_pd_3 <- lm(formula = PercentDamage ~ Treatment * WAT * Field, data = data)
summary(lm_pd_3)

#ANOVA for model simplicity
anova(lm_pd_1, lm_pd_2, lm_pd_3)

# AIC to pick the best model
AIC(lm_pd_1, lm_pd_2, lm_pd_3)

# Model lm_pd_3 has the lowest AIC