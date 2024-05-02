# Analysis of Youth Decline in Gallup Data for CHR 2024
# Author: Anthony McCanny
# Date: May 1, 2024
###############################################


# Load the Gallup data for Canada and perform initial filtering and renaming of variables
gallup_data_raw <- readRDS("Data/Gallup/GWP_021723_FullyCleaned_Canada.rds") %>%
  filter(COUNTRYNEW == "Canada") %>%
  rename(year = YEAR_WAVE, ls = WP16, age = WP1220, date = WP4, province = REGION2_CAN) %>%
  # Convert the date to Date format and extract month and day of the week
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         month = format(date, "%m"),
         day_of_week = weekdays(date)) %>%
  # Categorize age into ranges
  mutate(age_ranges = case_when(
            age >= 15 & age < 30 ~ "15-29",
            age >= 30 & age < 45 ~ "30-44",
            age >= 45 & age < 60 ~ "45-59",
            age >= 60 & age < 100 ~ "60+",
            TRUE ~ NA_character_)) %>%
  # Select columns where less than 50% of the data is missing
  select_if(~ sum(is.na(.)) / length(.) <= 0.5) %>%
  # Remove rows where life satisfaction (ls) is missing
  drop_na(ls)

# Identify determinant variables by excluding certain columns and those with 'RECODED' in their names
determinants <- setdiff(names(gallup_data_raw), c("ls", "WP5889", "WP5", "WAVE", "WPID", "WPID_RANDOM", "WGT", "FIELD_DATE", "COUNTRY_ISO2", "COUNTRY_ISO3", "COUNTRYNEW", "REG_GLOBAL",
                    "REG2_GLOBAL", "YEAR_CALENDAR", "year", "average_date", "date", "WP13156", "WP16590", "INDEX_LE", "INDEX_ST", "INDEX_TH")) %>%
                    setdiff(grep("RECODED", ., value = TRUE, invert = FALSE))

# Identify binary and categorical columns based on their values
binary_columns <- names(gallup_data_raw)[sapply(gallup_data_raw, function(column) all(column %in% c(0,1, NA)) && all(c(0,1) %in% column))]
categorical_columns <- names(gallup_data_raw)[sapply(gallup_data_raw, function(column) all(column %in% c(1:10, NA)))]

# Separate numeric and non-numeric determinants
numeric_determinants <- determinants[sapply(gallup_data_raw[determinants], is.numeric)]
non_numeric_determinants <- setdiff(determinants, numeric_determinants)

# Clean the data by handling missing values and converting columns to appropriate types
gallup_data_clean_total <- gallup_data_raw %>%
  # Create new columns to indicate non-missing (1) or missing (0) for each determinant
  mutate(
    across(all_of(determinants), ~if_else(is.na(.), 0, 1), .names = "{.col}_nm")) %>%
  # Handle missing values by replacing them with mean or "missing" depending on the column type
  mutate(
    across(all_of(numeric_determinants), ~replace_na(., mean(., na.rm = TRUE)), .names = "{.col}"),
    across(all_of(non_numeric_determinants), ~replace_na(as.character(.), "missing"), .names = "{.col}")) %>%
  # Convert binary and categorical columns to factors
  mutate(
    across(all_of(binary_columns), factor),
    across(all_of(categorical_columns), factor),
    across(all_of(intersect(names(gallup_data_raw)[ends_with("_nm")], 
        names(gallup_data_raw)[sapply(gallup_data_raw, function(column) all(column %in% c(0,1, NA)) && all(c(0,1) %in% column))])), 
        ~factor(.x, levels = c(0, 1))))
        
# Filter the cleaned data for specific age ranges or the year 2006
gallup_data_clean <- gallup_data_clean_total %>%
  filter(age_ranges %in% c("30-44", "45-59", "60+") | year == 2006)

# Filter the cleaned data for the young age group (15-29)
gallup_data_clean_young <- gallup_data_clean_total %>%
    filter(age_ranges == "15-29")

# Generate interaction terms for the linear model
interaction_terms <- paste0(determinants, ":", determinants, "_nm")
# Create the formula for the linear model using the interaction terms
lm_formula <- as.formula(paste("ls ~", paste(interaction_terms, collapse = " + "), " + year"))
# Fit the linear model to the cleaned data
lm_model <- lm(lm_formula, data = gallup_data_clean)
# Summarize the model to get an overview of the results
summary(lm_model)

# Calculate residuals from the linear model
residuals <- resid(lm_model)
# Add the residuals to the cleaned data
gallup_data_clean$residuals <- residuals

# Predict life satisfaction scores for the young age group using the model
gallup_data_clean_young$predicted_ls <- predict(lm_model, newdata = gallup_data_clean_young)
# Calculate residuals for the young age group by comparing actual and predicted life satisfaction
gallup_data_clean_young$residuals <- gallup_data_clean_young$ls - gallup_data_clean_young$predicted_ls



zero_variance <- sapply(gallup_data_clean_young[determinants], function(x) {
  length(unique(na.omit(x))) == 1
})
zero_variance_cols <- names(zero_variance[zero_variance])


gallup_data_clean_young_lasso <- gallup_data_clean_young %>%
  select(all_of(determinants)) %>%
  select(-all_of(zero_variance_cols))

# Run a lasso regression for gallup_data_clean_young using the previously determined interaction terms
library(glmnet)

# Prepare the matrix of predictors with just the determinants columns for the lasso regression
predictors_matrix <- model.matrix(~ . - 1, data = gallup_data_clean_young_lasso)

# Extract the outcome variable (residuals) from the data
outcome_vector <- gallup_data_clean_young$residuals

# Set alpha to 1 for lasso regression
lasso_model <- glmnet(predictors_matrix, outcome_vector, alpha = 1)

# Fit the lasso model using cross-validation
cv_lasso <- cv.glmnet(predictors_matrix, outcome_vector, alpha = 1)

# Get the lambda that gives minimum mean cross-validated error
optimal_lambda <- cv_lasso$lambda.min

# Refit the lasso model with the optimal lambda
lasso_model_optimal <- glmnet(predictors_matrix, outcome_vector, alpha = 1, lambda = optimal_lambda)

# Output the coefficients from the lasso regression and select non-zero coefficients
lasso_coefficients <- coef(lasso_model_optimal)
non_zero_coefficients <- lasso_coefficients[lasso_coefficients[, 1] != 0, ]

print(non_zero_coefficients)

# Calculate residuals from the optimal lasso model
lasso_predicted_values <- predict(lasso_model_optimal, newx = predictors_matrix, s = optimal_lambda)
lasso_residuals <- outcome_vector - lasso_predicted_values

# Add lasso residuals to the gallup_data_clean_young dataframe
gallup_data_clean_young$lasso_residuals <- lasso_residuals


# Calculate the average of residuals by year
average_residuals_by_year <- gallup_data_clean_young %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(average_resid = weighted.mean(residuals, w = WGT, na.rm = TRUE),
                    average_lasso_resid = weighted.mean(lasso_residuals, w = WGT, na.rm = TRUE))



library(ggplot2)
ggplot(average_residuals_by_year, aes(x = year)) +
  geom_line(aes(y = average_resid), color = "blue") +
  geom_point(aes(y = average_resid), color = "blue") +
  geom_line(aes(y = average_lasso_resid), color = "red") +
  geom_point(aes(y = average_lasso_resid), color = "red") +
  labs(title = "Average of Residuals by Year",
       x = "Year",
       y = "Average Residuals") +
  theme_minimal()
# End Generation Here


september_data <- gallup_data_clean_young %>%
  dplyr::filter(month == "09") %>%
  select(year)
  september_year_tally <- september_data %>%
    dplyr::group_by(year) %>%
    dplyr::tally()


