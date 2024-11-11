# set working direstory

# import libraries
library(glmmTMB)
library(Matrix)
library(lme4)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gamlss)
library(effects)

# Import Data
seq_data <- read.csv("sequences.csv")

# Transform start_time to year-month-day
seq_data <- seq_data |>
  mutate(start_time = as.Date(start_time))

#-------------------------------------------------------------------------------

# Sebsetting data 

# subsetting to only show only post treatment dates >= 2024-06-21
seq_data <- seq_data |>
  filter(identified_by == 'Ethan Marburger',
         start_time >= "2024-06-22" & start_time <= "2024-08-02",
         common_name == "White-tailed Deer")

unique(seq_data$deployment_id)

seq_data <- seq_data |>
  mutate(treatment = case_when(
    grepl("^WTD_2|^WTD_4|^WTD_5|^WTD_7|^WTD_8|^WTD_10|^WTD_14|^WTD_15|^WTD_17|^WTD_18", deployment_id) ~ 1,
    grepl("^WTD_1|^WTD_3|^WTD_6|^WTD_9|^WTD_11|^WTD_12|^WTD_13|^WTD_16|^WTD_19|^WTD_20", deployment_id) ~ 0,
    TRUE ~ NA_real_)) |>
  mutate(WAT = case_when(
    start_time >= "2024-06-22" & start_time <= "2024-06-28" ~ 1,
    start_time >= "2024-06-29" & start_time <= "2024-07-05" ~ 2,
    start_time >= "2024-07-06" & start_time <= "2024-07-12" ~ 3,
    start_time >= "2024-07-13" & start_time <= "2024-07-19" ~ 4,
    start_time >= "2024-07-20" & start_time <= "2024-07-26" ~ 5,
    start_time >= "2024-07-27" & start_time <= "2024-08-02" ~ 6,
    TRUE ~ NA_real_)) |>
  mutate(field = case_when(
    grepl("^WTD_1|^WTD_2|^WTD_3|^WTD_4|^WTD_5|^WTD_6", deployment_id) ~ 1,
    grepl("^WTD_7|^WTD_8|^WTD_9|^WTD_20", deployment_id) ~ 2,
    grepl("^WTD_10|^WTD_11|^WTD_12|^WTD_13|^WTD_14|^WTD_15|^WTD_16|^WTD_17|^WTD_18|^WTD_19", deployment_id) ~ 3,
    TRUE ~ NA_real_)) |>
  mutate(plot = case_when(
    grepl("^WTD_1_", deployment_id) ~ 1,
    grepl("^WTD_2_", deployment_id) ~ 2,
    grepl("^WTD_3_", deployment_id) ~ 3,
    grepl("^WTD_4_", deployment_id) ~ 4,
    grepl("^WTD_5_", deployment_id) ~ 5,
    grepl("^WTD_6_", deployment_id) ~ 6,
    grepl("^WTD_7_", deployment_id) ~ 7,
    grepl("^WTD_8_", deployment_id) ~ 8,
    grepl("^WTD_9_", deployment_id) ~ 9,
    grepl("^WTD_10_", deployment_id) ~ 10,
    grepl("^WTD_11_", deployment_id) ~ 11,
    grepl("^WTD_12_", deployment_id) ~ 12,
    grepl("^WTD_13_", deployment_id) ~ 13,
    grepl("^WTD_14_", deployment_id) ~ 14,
    grepl("^WTD_15_", deployment_id) ~ 15,
    grepl("^WTD_16_", deployment_id) ~ 16,
    grepl("^WTD_17_", deployment_id) ~ 17,
    grepl("^WTD_18_", deployment_id) ~ 18,
    grepl("^WTD_19_", deployment_id) ~ 19,
    grepl("^WTD_20_", deployment_id) ~ 20,
    TRUE ~ NA_real_))

unique(seq_data$plot)

# Creating a new tibble
data <- seq_data|>
  select(deployment_id, treatment, date = start_time, WAT, field, plot, group_size) |>
  group_by(WAT)
print(data)

qqnorm(data$group_size)
qqline(data$group_size)
hist(data$group_size)
shapiro.test(data$group_size)

#-------------------------------------------------------------------------------

# Weekly sums of group_size values 


# getting to daily sums of individuals per plot
data_daily_total <- aggregate(group_size ~ date + plot, data = data, sum, na.rm = TRUE)

# Creating treatment variable
data_daily_total$treatment <- ifelse(data_daily_total$plot %in% c(1, 3, 6, 9, 11, 12, 13, 16, 19, 20), 0,
                         ifelse(data_daily_total$plot %in% c(2, 4, 5, 7, 8, 10, 14, 15, 17, 18), 1,
                                NA))

# Creating a Field variable
data_daily_total$field <- ifelse(data_daily_total$plot %in% c(1, 2, 3, 4, 5, 6), 1,
                         ifelse(data_daily_total$plot %in% c(7, 8, 9, 20), 2,
                                3))

# Creating a WAT variable
data_daily_total <- data_daily_total |>
  mutate(WAT = case_when(
  date >= "2024-06-22" & date <= "2024-06-28" ~ 1,
  date >= "2024-06-29" & date <= "2024-07-05" ~ 2,
  date >= "2024-07-06" & date <= "2024-07-12" ~ 3,
  date >= "2024-07-13" & date <= "2024-07-19" ~ 4,
  date >= "2024-07-20" & date <= "2024-07-26" ~ 5,
  date >= "2024-07-27" & date <= "2024-08-02" ~ 6,
  TRUE ~ NA_real_))

summary(data_daily_total)

qqnorm(data_daily_total$group_size)
qqline(data_daily_total$group_size)
hist(data_daily_total$group_size)
shapiro.test(data_daily_total$group_size)

#-------------------------------------------------------------------------------

# Fitting a negative binomial distribution
# Model recommended by committee stats chair

data_daily_total$plot <- as.factor(data_daily_total$plot)
data_daily_total$field <- as.factor(data_daily_total$field)

nb_group_size <- gamlss(group_size ~ treatment * WAT + re(random= ~1|field/plot),
                family = NBII, #negative binomial type 2
                data = data_daily_total, 
                trace = FALSE)

# Summary of the model
summary(nb_group_size)

# Extract residuals and fitted values
residuals <- resid(nb_group_size)
fitted_values <- fitted(nb_group_size)

# Plot residuals vs. fitted values
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")

# QQ plot for checking normality of residuals
qqnorm(residuals, main = "QQ Plot of Residuals")
qqline(residuals, col = "red")

#-------------------------------------------------------------------------------

# Side-by-side bar graph with centered title and two legends: one for Treatment and one for WAT
ggplot(data_daily_total, aes(x = factor(treatment), fill = factor(treatment))) +
  geom_bar() +
  facet_wrap(~ WAT, nrow = 1) +  # Facet by WAT in a single row
  labs(title = "Observed Feeding Behavior by Treatment over Time",
       x = "Treatment",
       y = "Total Observations",
       fill = "Treatment") +  # Change fill legend title
  geom_point(aes(x = factor(treatment), y = 0, color = factor(WAT)), alpha = 0, size = 0) +  # Invisible points for WAT legend
  scale_color_discrete(name = "Weeks After Treatment") +  # Legend for WAT
  scale_fill_discrete(name = "Treatment") +  # Set the legend title for treatment
  theme(
    legend.position = "right",  # Adjust legend position
    plot.title = element_text(hjust = 0.5, size = 14)  # Center the title
  )


library(ggplot2)

library(ggplot2)

library(ggplot2)

# Define custom labels for Treatment
custom_labels <- c("0" = "Control", "1" = "Condensed Tannins")  # Customize legend labels

# Define custom labels for Treatment on the x-axis
x_axis_labels <- c("0" = "", "1" = "")  # Use empty strings to remove numeric values

# Side-by-side bar graph with centered title and two legends: one for Treatment and one for WAT
ggplot(data_daily_total, aes(x = factor(treatment), fill = factor(treatment))) +
  geom_bar() +
  facet_wrap(~ WAT, nrow = 1) +  # Facet by WAT in a single row
  labs(title = "Observed Feeding Behavior by Treatment Over Time",
       x = "Treatment",
       y = "Total Observations",
       fill = "Treatment") +  # Change fill legend title
  geom_point(aes(x = factor(treatment), y = 0, color = factor(WAT)), alpha = 0, size = 0) +  # Invisible points for WAT legend
  scale_color_discrete(name = "Weeks After Treatment") +  # Legend for WAT
  scale_fill_discrete(name = "Treatment Type", labels = custom_labels) +  # Set custom labels for the treatment legend
  scale_x_discrete(labels = x_axis_labels) +  # Remove numeric values from x-axis
  theme(
    legend.position = "right",  # Adjust legend position
    plot.title = element_text(hjust = 0.5, size = 14)  # Center the title
  )
