# ============================================================
# Suicide Rates in Chile (2020): Economic and Meteorological Predictors
# Authors: Pelagia Kalpakidou & Myriana Miltiadous
# Course: Statistical Consulting, October 2023
# ============================================================

# ---- Libraries ----
library(readxl)
library(tidyverse)
library(mice)
library(lme4)
library(ggplot2)
library(naniar)
library(lubridate)

# ---- Read Data ----
original_data <- read_excel("data/Dataset_CS_2020_copia.xlsx", col_types = "text")
print(head(original_data))
nrow(original_data)

# ---- Preprocess Data ----

# Change column names
colnames(original_data) <- str_replace_all(colnames(original_data), " ", "_")

# Remove columns we don't need
columns_to_remove <- c("Year_of_death", "Municipality", "Diagnostic_2_code",
                       "Place_of_occurrence", "Highest_Temperature", "Seks",
                       "Municipality_code", "Province", "Diagnostic_1_(cause_of_death)",
                       "Diagnostic_2_(suicide_method)", "Lowest_Temperature")

data <- original_data[, !(names(original_data) %in% columns_to_remove)]

names(data) <- c("Age", "Day", "Month", "Region", "Temperature",
                 "AtmPressure", "Precipitations",
                 "Humidity", "Unemployementrate", "Inflationrate")

# Check for duplicates
duplicates <- duplicated(original_data)
print("Duplicated rows:")
print(original_data[duplicates, ])

# Convert and round numeric columns
convert_and_round <- function(col) {
  if (is.numeric(col)) {
    round(col, 1)
  } else {
    numeric_col <- as.numeric(as.character(col))
    if (any(is.na(numeric_col))) {
      message("Warning: NA values in column ", names(col))
    }
    round(numeric_col, 1)
  }
}

data[, c(1, 5:8)] <- sapply(data[, c(1, 5:8)], convert_and_round)
data$Unemployementrate <- round(as.numeric(data$Unemployementrate), 3)
data$Inflationrate <- round(as.numeric(data$Inflationrate), 3)

# ---- Explore Data ----

# Age distribution
min_age <- min(data$Age)
max_age <- max(data$Age)

hist(data$Age,
     main = "Age Distribution",
     xlab = "Age",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")
abline(v = min_age, col = "red", lwd = 2, lty = 2)
abline(v = max_age, col = "green", lwd = 2, lty = 2)
legend("topright", legend = c(paste("Min: ", min_age), paste("Max: ", max_age)),
       fill = c("red", "green"))

# Fix missing unemployment rate value provided by client
which(is.na(data$Unemployementrate))
data[is.na(data$Unemployementrate), "Unemployementrate"] <- 0.108
which(is.na(data$Unemployementrate))

# Add age group column
data$AgeGroup <- cut(
  data$Age,
  breaks = c(0, 26, 40, 60, Inf),
  labels = c("Adolescents", "Young_adults", "Middle_adults", "Elderly_adults"),
  include.lowest = TRUE
)

# Check observations per age group
sum(data$AgeGroup == "Elderly_adults")
sum(data$AgeGroup == "Adolescents")
sum(data$AgeGroup == "Young_adults")
sum(data$AgeGroup == "Middle_adults")

# Boxplots
data_plot <- data[, !names(data) %in% c("Day", "Month", "Region", "AgeGroup")]
data_long <- gather(data_plot, key = "Variable", value = "Value")

gg <- ggplot(data_long, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  labs(x = "Variable Names", y = "Values") +
  ggtitle("Boxplots of Variables") +
  facet_wrap(~ Variable, scales = "free_y", nrow = 3, ncol = 3)
gg + theme(axis.text.x = element_blank())

# Count outliers
count_outliers <- function(data) {
  outliers_count <- data.frame(Variable = character(), Outliers = numeric())
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      numeric_values <- na.omit(data[[col]])
      Q1 <- quantile(numeric_values, 0.25)
      Q3 <- quantile(numeric_values, 0.75)
      IQR <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      outliers <- numeric_values[numeric_values < lower_bound | numeric_values > upper_bound]
      num_outliers <- length(outliers)
      outliers_count <- rbind(outliers_count, data.frame(Variable = col, Outliers = num_outliers))
    }
  }
  return(outliers_count)
}

outliers_summary <- count_outliers(data)
print(outliers_summary)

# Correlations
pairs(data_plot)

# Missing values
miss_var_summary(data)

# ---- Multiple Imputation ----

data$Region <- as.factor(data$Region)

imp <- mice(
  data = data,
  method = 'rf',
  pred = quickpred(data, mincor = 0.1,
                   exclude = c("Age", "Unemploymentrate", "Inflationrate",
                               "AgeGroup", "Day", "Month"))
)

# ---- Helper Functions for Poisson Regression ----

# Z-score meteorological variables within each region
z_scores_fun <- function(imputed_data) {
  data_zscored <- imputed_data %>%
    group_by(Region) %>%
    mutate(
      zs_temp = scale(Temperature),
      zs_atm_press = scale(AtmPressure),
      zs_Preci = scale(Precipitations),
      zs_humi = scale(Humidity)
    )
  return(data_zscored)
}

# Aggregate to daily suicide counts per region and age group
process_suicide_data_by_day_r <- function(data) {
  data <- data %>%
    mutate(Date = as.Date(paste("2020", Month, Day, sep = "-"), format = "%Y-%m-%d"))

  suicides_by_day_r <- data %>%
    group_by(Date, Region, AgeGroup) %>%
    summarise(
      Number_of_suicides = n(),
      z_temperature = mean(zs_temp),
      z_Atmospheric_pressure = mean(zs_atm_press),
      z_Precipitations_mm = mean(zs_Preci),
      z_Humidity_percentage = mean(zs_humi),
      .groups = "drop"
    )
  return(suicides_by_day_r)
}

# Apply z-score and process data for each imputed dataset
processed_datasets <- lapply(1:5, function(i) {
  imp_day <- complete(imp, action = i)
  zscored_data <- z_scores_fun(imp_day)
  processed_data <- process_suicide_data_by_day_r(zscored_data)
  return(processed_data)
})

# ---- Poisson Regression: Meteorological Factors ----

# Prepare z-scored data (fix NaN z-scores for precipitation = 0)
data_z_scored <- z_scores_fun(complete(imp))
data_z_scored$zs_Preci[is.na(data_z_scored$zs_Preci)] <- 0

# Model without age group
model_meter_no_age <- glmer(Number_of_suicides ~ z_temperature +
                              z_Atmospheric_pressure +
                              z_Precipitations_mm +
                              z_Humidity_percentage +
                              (1 | Region),
                            family = poisson,
                            data = process_suicide_data_by_day_r(data_z_scored))
summary(model_meter_no_age)
ranef(model_meter_no_age)

# Model with age group interactions
model_meter <- glmer(Number_of_suicides ~ AgeGroup:(z_temperature
                                                     + z_Atmospheric_pressure
                                                     + z_Precipitations_mm
                                                     + z_Humidity_percentage)
                     + (1 | Region),
                     family = poisson,
                     data = process_suicide_data_by_day_r(data_z_scored))
summary(model_meter)
ranef(model_meter)

# ---- Poisson Regression: Economic Factors ----

# Aggregate to monthly suicide counts
process_suicide_data_by_month <- function(data) {
  data <- data %>%
    mutate(Date = as.Date(paste("2020", Month, "01", sep = "-"), format = "%Y-%m-%d"))

  suicides_by_month <- data %>%
    group_by(Year = lubridate::year(Date), Month = lubridate::month(Date), AgeGroup) %>%
    summarise(
      Number_of_suicides = n(),
      Inflation_rate_percentage = mean(Inflationrate),
      Unemployement_rate_percentage = mean(Unemployementrate),
      .groups = 'drop'
    )
  return(suicides_by_month)
}

suicides_by_month <- process_suicide_data_by_month(data)

# Model without age group
glm_model_econ_no_age <- glm(Number_of_suicides ~ Inflation_rate_percentage +
                               Unemployement_rate_percentage,
                             family = poisson,
                             data = suicides_by_month)
summary(glm_model_econ_no_age)

# Model with age group interactions
suicides_by_month$AgeGroup <- as.factor(suicides_by_month$AgeGroup)

glm_model_econ <- glm(Number_of_suicides ~ Inflation_rate_percentage +
                        Unemployement_rate_percentage +
                        AgeGroup:(Inflation_rate_percentage +
                                    Unemployement_rate_percentage),
                      family = poisson,
                      data = suicides_by_month)
summary(glm_model_econ)

# ---- Diagnostics ----

run_diagnostics <- function(model, observed) {
  df <- df.residual(model)
  expected_table <- predict(model, type = "response")

  if (length(expected_table) != length(observed)) {
    stop("Length mismatch between observed and expected values.")
  }

  chi_square_deviance <- deviance(model)
  p_value_deviance <- pchisq(chi_square_deviance, df, lower.tail = FALSE)

  pearson_residuals <- residuals(model, type = "pearson")
  pearson_statistic <- sum(pearson_residuals^2)
  p_value_pearson <- pchisq(pearson_statistic, df, lower.tail = FALSE)

  cat("Goodness-of-Fit Tests\n")
  cat(sprintf("Deviance:  chi2 = %.4f, df = %d, p = %.4f\n",
              chi_square_deviance, df, p_value_deviance))
  cat(sprintf("Pearson:   chi2 = %.4f, df = %d, p = %.4f\n",
              pearson_statistic, df, p_value_pearson))

  if (p_value_deviance > 0.05) cat("No evidence of lack-of-fit.\n") else cat("Statistically significant lack of fit.\n")
  if (p_value_pearson > 0.05) cat("No evidence of overdispersion.\n") else cat("Statistically significant overdispersion.\n")
}

# Meteorological models
cat("\n--- Meteorological model (without age) ---\n")
run_diagnostics(model_meter_no_age, process_suicide_data_by_day_r(data_z_scored)$Number_of_suicides)

cat("\n--- Meteorological model (with age) ---\n")
run_diagnostics(model_meter, process_suicide_data_by_day_r(data_z_scored)$Number_of_suicides)

# Economic models
cat("\n--- Economic model (without age) ---\n")
run_diagnostics(glm_model_econ_no_age, suicides_by_month$Number_of_suicides)

cat("\n--- Economic model (with age) ---\n")
run_diagnostics(glm_model_econ, suicides_by_month$Number_of_suicides)
