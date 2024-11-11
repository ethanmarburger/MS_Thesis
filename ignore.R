install.packages("betareg")
install.packages("statmod")
install.packages("glmmTMB")
install.packages("lme4")
install.packages("zoib")
install.packages("rjags")
install.packages("coda")

library(tidyverse)
library(glmmTMB)
library(dplyr)
library(lme4)
library(zoib)
library(rjags)
library(coda)
library(MASS)

# Read in data
data <- read.csv('Data.csv', header = TRUE)

head(data)

class(data)

# Checking for missing values
summary(data)

#Dropping rows with missing values
clean_data <- na.omit(data)
summary(clean_data)

# Subset data for exposed observations only
clean_data <- clean_data[clean_data$Enclosure == "0", ]

# PERCENTDAMAGE DATA FRAME
# Subset data to exclude values at WAT 0
clean_data_norm <- clean_data[clean_data$WAT > "0", ]
summary(clean_data_norm)
qqnorm(clean_data_norm$PercentDamage)
qqline(clean_data_norm$PercentDamage, col="red")
summary(clean_data_norm)

# use clean_data to model Height

hist(clean_data_norm$PercentDamage)
# Convert PercentDamage to binary (0 = undamaged, 1 = damaged)
clean_data_norm <- clean_data_norm |>
  mutate(pd_binary = ifelse(PercentDamage > 0, 1, 0))

unique(clean_data_norm$pd_binary)
hist(clean_data_norm$pd_binary)

# Meeting logit transformation specifications, only values between 0-1
clean_data_norm$pd_100 <- clean_data_norm$PercentDamage / 100

clean_data_norm$pd_100[clean_data_norm$pd_100 == "0"] <- 0.001
clean_data_norm$pd_100[clean_data_norm$pd_100 == "1"] <- 0.999

#-------------------------------------------------------------------------------

# Visualizing normality
qqnorm(clean_data$Height)
qqline(clean_data$Height, col="red")
shapiro.test(clean_data$Height)

# Box-cox transformation to meet normality assumption
# Box-cox was recommended by statistical consultant 

# Step 1: Find the optimal lambda using boxcox
bc <- boxcox(lm(clean_data$Height ~ 1))

# Step 2: Extract the lambda that maximizes the log-likelihood
lambda_optimal <- bc$x[which.max(bc$y)]

# Step 3: Apply the Box-Cox transformation to the Height variable
clean_data$Height_transformed <- (clean_data$Height^lambda_optimal - 1) / lambda_optimal

# Rechecking normality
hist(clean_data$Height_transformed)
qqnorm(clean_data$Height_transformed)
qqline(clean_data$Height_transformed, col="red")
shapiro.test(clean_data$Height_transformed)

# Two-AY ANOVA test for height and height transformation

clean_data_height <- aov(Height ~ Field * Treatment * WAT + Error(Plot), data = clean_data)
summary(clean_data_height)

clean_data__height_boxcox <- aov(Height_transformed ~ Field * Treatment * WAT + Error(Plot), data = clean_data)
summary(clean_data_boxcox)


#-------------------------------------------------------------------------------

# PercentDamage: zero-inflated beta regression using "zoib" package
# Zoib package recommended by committee stats chair 

# pd_100 is highly zero inflated

clean_data_norm$Treatment <- as.factor(clean_data_norm$Treatment)
clean_data_norm$WAT <- as.factor(clean_data_norm$WAT)
clean_data_norm$Field <- as.factor(clean_data_norm$Field)

str(clean_data_norm)

# Wanted to include Blocking by Field
# Wanted to include Plot as a random effect 
# Couldn't figure out model syntax or how to incorporate Field or Plot
# Create the model
zoib_model <- zoib(
  pd_100 ~ Treatment+WAT|1|Treatment+WAT|1  ,  # Beta component and intercepts for precision and zero-inflation
  data = clean_data_norm,
  zero.inflation = TRUE,  # Zero-inflation model
  one.inflation = FALSE,  # No one-inflation
  joint = FALSE,  # Separate models for beta and zero-inflation parts
  n.iter = 5000,  # Number of iterations
  n.thin = 5  # Thinning
)

summary(zoib_model)

zoib_1 <- zoib_model$coeff
summary(zoib_1)

# Extract credible intervals from the model
credible_intervals <- apply(zoib_1, 2, quantile, probs = c(0.025, 0.5, 0.975))

# Display the credible intervals
print(credible_intervals)

 #-------------------------------------------------------------------------------
