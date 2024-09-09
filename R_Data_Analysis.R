#Imported data via the global environment
head(Data)

#Ignoring rows with missing values
clean_data <- na.omit(Data)
summary(clean_data)

#Three-way ANOVA for dependent variable: Height
model_height <- aov(Height ~ Treatment * WAT * Enclosure + Error(Field), data = clean_data)
summary(model_height)

#Three-way ANOVA for dependent variable: Percent Damage
model_percentdamage <- aov(PercentDamage ~ Treatment * WAT * Enclosure + Error(Field), data = clean_data)
summary(model_percentdamage)
#Significance in treatment and percent damage!
#Date collected from inside an enclose was impervious to browsing damage: Precentdamage = 0

#-------------------------------------------------------------------------------

#Data set without Enclosure factor

#Ignoring rows with missing values
clean_data_no_enclosure <- na.omit(Data_No_Enclosure)
summary(clean_data_no_enclosure)

#Two-way ANOVA for dependent variable: Height
model_height_no_enclosure <- aov(Height ~ Treatment * WAT + Error(Field), data = clean_data_no_enclosure)
summary(model_height_no_enclosure)
#No significance in on plant height by treatment

#Two-way ANOVA for dependent variable: Percent Damage
model_percentdamage_no_enclosure <- aov(PercentDamage ~ Treatment * WAT + Error(Field), data = clean_data_no_enclosure)
summary(model_percentdamage_no_enclosure)
#Significance between Percent Damage and Treatment
#Significance between Percent Damage and Weeks After Treatment (WAT)

#-------------------------------------------------------------------------------

#Plotting PercentDamage by Treatment and Weeks after treatment

# Computing summary statistics for PercentDamage by Treatment and WAT
summary_data_treatment_wat <- aggregate(PercentDamage ~ Treatment + WAT, 
                                        data = clean_data_no_enclosure, 
                                        FUN = function(x) c(mean = mean(x, na.rm = TRUE), 
                                                            sd = sd(x, na.rm = TRUE), 
                                                            n = length(x)))

# Converting the matrix to a data frame
summary_data_treatment_wat <- data.frame(
  Treatment = summary_data_treatment_wat$Treatment,
  WAT = summary_data_treatment_wat$WAT,
  MeanDamage = summary_data_treatment_wat$PercentDamage[, "mean"],
  SD = summary_data_treatment_wat$PercentDamage[, "sd"],
  N = summary_data_treatment_wat$PercentDamage[, "n"]
)

# Calculating standard error
summary_data_treatment_wat$SE <- summary_data_treatment_wat$SD / sqrt(summary_data_treatment_wat$N)

# Creating a factor for grouping bars
summary_data_treatment_wat$Group <- with(summary_data_treatment_wat, 
                                         interaction(Treatment, WAT))

# Defining colors
bar_colors <- rainbow(length(unique(summary_data_treatment_wat$Group)))

# Creating the bar plot
bar_heights <- summary_data_treatment_wat$MeanDamage
bar_names <- as.character(summary_data_treatment_wat$Group)
bar_plot <- barplot(height = bar_heights, 
                    beside = TRUE,  # Group bars by Treatment and WAT
                    names.arg = bar_names,
                    col = bar_colors,
                    ylim = c(0, max(bar_heights + summary_data_treatment_wat$SE)),
                    main = "Mean Percent Damage by Treatment and Weeks After Treatment",
                    xlab = "Treatment and WAT",
                    ylab = "Mean Percent Damage",
                    las = 2)  # Rotate x-axis labels

#Adding error bars
arrows(x0 = bar_plot, 
       y0 = bar_heights - summary_data_treatment_wat$SE, 
       x1 = bar_plot, 
       y1 = bar_heights + summary_data_treatment_wat$SE, 
       angle = 90, code = 3, length = 0.1)

