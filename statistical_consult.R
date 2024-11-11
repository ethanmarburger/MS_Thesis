# Load required libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(glmmTMB)

# Read the data
data <- read.csv("Data.csv", header=TRUE)


## STEP 1: DATA PREPARATION
# Convert categorical variables to factors
data$Field <- as.factor(data$Field)
data$Plot <- as.factor(data$Plot)
data$Treatment <- as.factor(data$Treatment)

# Keep WAT as numeric (continuous)
data$WAT <- as.numeric(data$WAT)

# Check the structure of the data
str(data)

# Remove rows with missing Height values
data <- data %>% filter(!is.na(Height))



# EXPLORATORY DATA ANALYSIS -----------------------------------------------

# Calculate mean Height for each Treatment, WAT, and Field combination
summary_data <- data %>%
  group_by(Treatment, WAT, Field) %>%
  summarise(mean_height = mean(Height, na.rm = TRUE))

# Create a plot
ggplot(summary_data, aes(x = , y = mean_height, color = Treatment, group = Treatment)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Field) +
  labs(title = "Mean Height by Treatment over Time",
       x = "Weeks After Treatment",
       y = "Mean Height") +
  theme_minimal()

# Box plot to visualize distribution
ggplot(data, aes(x = Treatment, y = Height, fill = Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Field) +
  labs(title = "Height Distribution by Treatment over Time",
       x = "Weeks After Treatment",
       y = "Height") +
  theme_minimal()

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

library(ggplot2)

library(ggplot2)

library(ggplot2)

# Define custom labels for Treatment
custom_labels <- c("0" = "", "1" = "")  # Use empty strings to remove numeric values

# Box plot with centered, larger title and two legends: one for Treatment and one for WAT
ggplot(data, aes(x = Treatment, y = Height, fill = Treatment)) +
  geom_boxplot() +
  facet_wrap(~ WAT, nrow = 1) +  # Facet in a single row
  labs(title = "Height Distribution by Treatment Over Time",
       x = "Treatment",
       y = "Height") +
  # Create a dummy aesthetic for WAT to generate a second legend
  geom_point(aes(color = factor(WAT)), alpha = 0) +  # Invisible points to create the WAT legend
  scale_x_discrete(labels = custom_labels) +  # Remove numeric values from x-axis
  scale_color_discrete(name = "Weeks After Treatment") +  # Legend for WAT
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 14),  # Center title and increase font size by 2 points
    legend.title = element_text(size = 12, face = "bold"),  # Customize the legend title font size and style
    legend.text = element_text(size = 10)  # Customize the legend text size
  ) +
  guides(fill = guide_legend(title = "Treatment Type", labels = c("0" = "Control", "1" = "Condensed Tannins")))  # Customize legend labels

library(ggplot2)

# Define custom labels for Treatment
custom_labels <- c("0" = "Control", "1" = "Condensed Tannins")  # Customize legend labels

# Define custom labels for x-axis to remove numeric values
x_axis_labels <- c("0" = "", "1" = "")  # Use empty strings to remove numeric values

# Box plot with centered, larger title and two legends: one for Treatment and one for WAT
ggplot(data, aes(x = Treatment, y = Height, fill = Treatment)) +
  geom_boxplot() +
  facet_wrap(~ WAT, nrow = 1) +  # Facet in a single row
  labs(title = "Height Distribution by Treatment Over Time",
       x = "Treatment",
       y = "Height") +
  # Create a dummy aesthetic for WAT to generate a second legend
  geom_point(aes(color = factor(WAT)), alpha = 0) +  # Invisible points to create the WAT legend
  scale_x_discrete(labels = x_axis_labels) +  # Remove numeric values from x-axis
  scale_color_discrete(name = "Weeks After Treatment") +  # Legend for WAT
  scale_fill_discrete(name = "Treatment Type", labels = custom_labels) +  # Customize legend labels while keeping default colors
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 14),  # Center title and increase font size by 2 points
    legend.title = element_text(size = 12),  # Customize the legend title font size without bold
    legend.text = element_text(size = 10)  # Customize the legend text size
  )




# MIXED EFFECT MODEL ------------------------------------------------------

## general structure of a mixed effects model formula in R 
## response ~ fixed_effects + (random_effects | grouping_factor)

# Main effects:

# Treatment: This tests for overall differences in Height between treated and untreated plots.
# WAT (Weeks After Treatment): This tests for a linear trend in Height over time, regardless of Treatment.

# Interaction effect:
#Treatment * WAT: This tests whether the effect of Treatment on Height changes over time.

# The * operator in R model formulas expands to include both main effects and their interaction. So Treatment * WAT is equivalent to Treatment + WAT + Treatment:WAT

# Fit the mixed-effects model
library(lme4)
library(lmerTest)

# Full model (as before)
full_model <- lmer(Height ~ Treatment * WAT + (1|Field/Plot), data = data)

# Null model (only random effects)
null_model <- lmer(Height ~ 1 + (1|Field/Plot), data = data)

# partial model
partial_model <- lm(Height ~ Treatment * WAT, data = data)
summary(partial_model)


# Compare models using likelihood ratio test
anova(null_model, full_model)

# AIC comparison
AIC(null_model, full_model)

# BIC comparison
BIC(null_model, full_model)

# Summary of full model
summary(full_model)

# Summary of null model
summary(null_model)


# Model Interpretation ----------------------------------------------------
# Model diagnostics
library(ggplot2)

# Generate predictions from both models
data$pred_full <- predict(full_model)
data$pred_null <- predict(null_model)

# Create a plot
ggplot(data, aes(x = WAT, y = Height)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_full, color = Treatment), size = 1) +
  geom_line(aes(y = pred_null), color = "black", linetype = "dashed", size = 1) +
  facet_wrap(~ Field) +
  labs(title = "Comparison of Full and Null Models",
       subtitle = "Solid lines: Full model predictions, Dashed line: Null model prediction",
       x = "Weeks After Treatment",
       y = "Height") +
  theme_minimal()

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# PERCENTDAMAGE

summary_data_pd <- data %>%
  group_by(Field, Treatment, WAT) %>%
  summarise(mean_pd = mean(PercentDamage, na.rm = TRUE))

# Subset data to exclude values at WAT 0
pd_data <- data[data$WAT > "0", ]

# Create a plot
ggplot(pd_data, aes(x = WAT, y = mean_pd, color = Treatment, group = Treatment)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Field) +
  labs(title = "Mean pd by Treatment over Time",
       x = "Weeks After Treatment",
       y = "Mean pd") +
  theme_minimal()

# Box plot to visualize distribution by Field
ggplot(pd_data, aes(x = WAT, y = PercentDamage, fill = Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Field) +
  labs(title = "pd Distribution by Treatment over Time",
       x = "Weeks After Treatment",
       y = "Height") +
  theme_minimal()

# Box plot to visualize distribution by WAT
ggplot(pd_data, aes(x = Treatment, y = PercentDamage, fill = Treatment)) +
  geom_boxplot() +
  labs(title = "pd Distribution by Treatment over Time",
       x = "Treatment",
       y = "PD") +
  theme_minimal()

install.packages("DHARMa")
library(DHARMa)
news(package = "DHARMa")

fittedModel <- glmer(PercentDamage ~ Treatment + WAT + (1|Field/Plot), 
                     family = "poisson", data = pd_data)

simulationOutput <- simulateResiduals(fittedModel = fittedModel, plot = F)

residuals(simulationOutput)

plot(simulationOutput)

plotResiduals(simulationOutput, form = pd_data$PercentDamage)


library(ggplot2)

library(ggplot2)

library(ggplot2)

library(ggplot2)

library(ggplot2)

library(ggplot2)

library(ggplot2)

library(ggplot2)

# Define custom labels for Treatment
custom_labels <- c("0" = "Control", "1" = "Condensed Tannins")  # Customize legend labels

# Scatter plot with jitter, centered title, and two legends: one for Treatment and one for WAT
ggplot(pd_data, aes(x = factor(Treatment), y = PercentDamage, color = factor(Treatment))) +
  geom_jitter(size = 3, alpha = 0.7, width = 0.2, height = 0) +  # Add jitter to PercentDamage
  facet_wrap(~ WAT, nrow = 1) +  # Facet in a single row
  labs(title = "Percent Damage by Treatment Over Time",
       x = "Treatment",
       y = "Percent Damage") +
  scale_x_discrete(labels = c("0" = "", "1" = "")) +  # Remove numeric values from x-axis
  geom_point(aes(shape = factor(WAT)), alpha = 0, size = 0) +  # Invisible points to create the WAT legend
  scale_shape_discrete(name = "Weeks After Treatment") +  # Legend for WAT
  scale_color_discrete(name = "Treatment Type", labels = custom_labels) +  # Use default colors and customize legend labels
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 14)  # Center title and increase font size by 2 points
  )
