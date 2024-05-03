# Analysis of Youth Decline in Gallup Data for CHR 2024
# Author: Anthony McCanny
# Date: May 1, 2024
###############################################

library(dplyr)
library(tidyr)
library(Hmisc)
library(glmnet)
library(ggplot2)

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
  # Create new variables
  mutate( age2 = age^2,
          log_income = log(INCOME_2/sqrt(HHSIZE)+0.01)) %>%
  # Set attributes for new variables with short text descriptions
  mutate(across(c(age2, log_income), ~`attr<-`(.,"Short Text", c("Age squared", "Log of Equivalized Household Income")[match(cur_column(), c("age2", "log_income"))]))) %>%
  drop_na(ls)

# Identify determinant variables by excluding certain columns and those with 'RECODED' in their names
determinants <- setdiff(c(names(gallup_data_raw)), c("ls", "WP5889", "WP5", "WAVE", "WPID", "WPID_RANDOM", "WGT", "FIELD_DATE", "COUNTRY_ISO2", "COUNTRY_ISO3", "COUNTRYNEW", "REG_GLOBAL",
                    "REG2_GLOBAL", "YEAR_CALENDAR", "year", "average_date", "date", "WP13156", "WP16590", "INDEX_LE", "INDEX_ST", "INDEX_TH", "INDEX_OT", "INDEX_SU", "WP18", "INCOME_1", "INCOME_2", "INCOME_3", "INCOME_4", "INCOME_7")) %>%
                    setdiff(grep("RECODED", ., value = TRUE, invert = FALSE))

# Identify binary and categorical columns based on their values
binary_columns <- names(gallup_data_raw)[sapply(gallup_data_raw, function(column) all(column %in% c(0,1, NA)) && all(c(0,1) %in% column))]
categorical_columns <- names(gallup_data_raw)[sapply(gallup_data_raw, function(column) all(column %in% c(1:10, NA)))]

# Separate numeric and non-numeric determinants
numeric_determinants <- determinants[sapply(gallup_data_raw[determinants], is.numeric)]
non_numeric_determinants <- setdiff(determinants, numeric_determinants)

# Clean the data by handling missing values and converting columns to appropriate types
gallup_data_clean_total <- gallup_data_raw %>%
  mutate(WP1233 = recode(WP1233,
                         `0` = "Other Religions",
                         `1` = "Catholic",
                         `2` = "Protestant",
                         `3` = "Orthodox",
                         `4` = "Islam",
                         `5` = "Islam",
                         `6` = "Islam",
                         `7` = "Other Religions",
                         `8` = "Hinduism",
                         `9` = "Buddhism",
                         `10` = "Primal-Indigenous",
                         `11` = "Other Religions",
                         `12` = "Sikhism",
                         `13` = "Other Religions",
                         `14` = "Other Religions",
                         `15` = "Judaism",
                         `16` = "Other Religions",
                         `17` = "Other Religions",
                         `18` = "Other Religions",
                         `19` = "Other Religions",
                         `20` = "Other Religions",
                         `21` = "Other Religions",
                         `22` = "Other Religions",
                         `23` = "Other Religions",
                         `24` = "Other Religions",
                         `25` = "Other Religions",
                         `26` = "Secular/Nonreligious/Agnostic/Atheist/None",
                         `28` = "Other Religions",
                         `29` = "Other Religions",
                         `97` = "No response",
                         `98` = "No response",
                         `99` = "No response",
                         .default = as.character(WP1233))) %>%
  # Create new columns to indicate non-missing (1) or missing (0) for each determinant
  mutate(
    across(all_of(determinants), ~if_else(is.na(.), 0, 1), .names = "{.col}_nm")) %>%
  # Handle missing values by replacing them with mean or "missing" depending on the column type
  mutate(
    across(all_of(numeric_determinants), ~{ existing_vals <- na.omit(.); replace_na(., sample(existing_vals, size = 1, replace = TRUE)) }, .names = "{.col}"),
    across(all_of(non_numeric_determinants), ~{ existing_vals <- na.omit(.); replace_na(., sample(existing_vals, size = 1,  replace = TRUE)) }, .names = "{.col}")) %>%
  # Convert binary and categorical columns to factors
  mutate(
    across(all_of(binary_columns), factor),
    across(all_of(c(categorical_columns, "year", "province", "WP1233")), factor),
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


# Select the proper set of columns before splitting the sample
gallup_data_clean_young_selected <- gallup_data_clean_young %>%
  select(all_of(c(determinants, "residuals", "year", "WGT"))) %>%
  select(-all_of(zero_variance_cols)) %>%
  select(-month)

# Split the sample by age into two groups: 15-21 and 22-29
gallup_data_clean_young_15_21 <- gallup_data_clean_young_selected %>%
  filter(age >= 15 & age <= 21)

gallup_data_clean_young_22_29 <- gallup_data_clean_young_selected %>%
  filter(age > 21 & age <= 29)


get_readable_label <- function(variable_name) {
  # Create a named vector of readable labels from gallup_data_clean_young_lasso
  readable_labels <- sapply(names(gallup_data_raw), function(column_name) {
    label <- attr(gallup_data_raw[[column_name]], "Short Text")
    if (is.null(label)) {
      return(column_name)
    } else {
      return(label)
    }
  })

  # Check if the variable is a factor by seeing if the base name without the last digit is in the list of variables turned into factors
  base_variable_name <- sub("\\d$", "", variable_name)
  is_factor <- base_variable_name %in% c(binary_columns, categorical_columns, "year", "province")
  # Find the readable label that matches the variable name
  readable_label <- readable_labels[names(readable_labels) == base_variable_name]
  province_names <- c("10" = "Newfoundland and Labrador", "11" = "Prince Edward Island", "12" = "Nova Scotia", "13" = "New Brunswick", "24" = "Quebec", "35" = "Ontario", "46" = "Manitoba", "47" = "Saskatchewan", "48" = "Alberta", "59" = "British Columbia", "98" = "(DK)", "99" = "(Refused)")
  religion_names <- c("0" = "Other", "1" = "Christianity: Roman Catholic", "2" = "Christianity: Protestant", "3" = "Christianity: Eastern Orthodox", "4" = "Islam/Muslim", "5" = "Islam/Muslim (Shiite)", "6" = "Islam/Muslim (Sunni)", "7" = "Druze", "8" = "Hinduism", "9" = "Buddhism", "10" = "Primal-indigenous", "11" = "Chinese Traditional Religion", "12" = "Sikhism", "13" = "Juche", "14" = "Spiritism", "15" = "Judaism", "16" = "Baha'i", "17" = "Jainism", "18" = "Shinto", "19" = "Cao Dai", "20" = "Zoroastrianism", "21" = "Tenrikyo", "22" = "Neo-Paganism", "23" = "Unitarian-Universalism", "24" = "Rastafarianism", "25" = "Scientology", "26" = "Secular/Nonreligious/Agnostic/Atheist/None", "28" = "Christian (not specified)", "29" = "Taoism/Daoism", "97" = "(No response)", "98" = "(DK)", "99" = "(Refused)")
  if (is_factor) {
      factor_level <- sub(".*?(\\d)$", "\\1", variable_name)
      readable_label <- paste(readable_label, factor_level, sep = "_")
  } else if (grepl("^province\\d{2}$", variable_name)) {
      province_code <- substring(variable_name, 9, 10)
      readable_label <- province_names[province_code]
  } else if (startsWith(variable_name, "WP1233")) {
      religion_code <- substring(variable_name, 7)
      readable_label <- religion_code
  } else if (variable_name == "(Intercept)") {
      readable_label <- "Intercept"
  } else {
      readable_label <- readable_labels[names(readable_labels) == variable_name]
  }
  return(readable_label)
}

run_lasso_average <- function(data, num_runs = 100) {
  
  # Initialize a list to store coefficients from each run
  coefficients_list <- list()
  
  for (i in 1:num_runs) {
    set.seed(i)  # Set a different seed for each run
    
    predictors_matrix <- model.matrix(~ . - 1 - residuals - year - WGT, data = data)
    outcome_vector <- data$residuals
    
    lasso_model <- glmnet(predictors_matrix, outcome_vector, alpha = 1)
    cv_lasso <- cv.glmnet(predictors_matrix, outcome_vector, alpha = 1)
    optimal_lambda <- cv_lasso$lambda.min
    lasso_model_optimal <- glmnet(predictors_matrix, outcome_vector, alpha = 1, lambda = optimal_lambda)
    
    lasso_coefficients <- coef(lasso_model_optimal)
    non_zero_coefficients <- lasso_coefficients[lasso_coefficients[, 1] != 0, ]
    
    # Store the non-zero coefficients
    coefficients_list[[i]] <- non_zero_coefficients
  }
  
  # Assuming 'all_coefficients' contains all possible coefficient names
  all_coefficients <- unique(unlist(lapply(coefficients_list, names)))

  standardized_coefficients_list <- lapply(coefficients_list, function(coefs) {
    standardized_coefs <- rep(0, length(all_coefficients))
    names(standardized_coefs) <- all_coefficients
    standardized_coefs[names(coefs)] <- coefs
    return(standardized_coefs)
  })

  # Now use 'Reduce' to average the coefficients
  averaged_coefficients <- Reduce("+", standardized_coefficients_list) / length(coefficients_list)
  
  # Copy the optimal lasso model to a new variable for averaging coefficients
  averaged_coef_model <- lasso_model_optimal
  # Extract the coefficient matrix from the optimal lasso model
  coef_matrix <- averaged_coef_model$beta
  # Remove the intercept from the averaged coefficients as we only want to modify the predictors' coefficients
  averaged_coefficients_red <- averaged_coefficients[names(averaged_coefficients) != "(Intercept)"]
  # Set the intercept of the averaged_coef_model
  averaged_coef_model$a0 <- setNames(averaged_coefficients[names(averaged_coefficients) == "(Intercept)"], "s0")
  # Find the positions of the averaged coefficients in the coefficient matrix
  index <- match(names(averaged_coefficients_red), rownames(coef_matrix))
  # Update the coefficient matrix with the averaged coefficients
  coef_matrix[index] <- averaged_coefficients_red
  # Assign the updated coefficient matrix back to the model
  averaged_coef_model$beta <- coef_matrix
  # Use the updated model to predict values based on the predictors matrix
  lasso_predicted_values <- predict(averaged_coef_model, newx = predictors_matrix)
  # Calculate the residuals by subtracting the predicted values from the actual outcome vector
  lasso_residuals <- outcome_vector - lasso_predicted_values
  # Add the calculated residuals as a new column in the data
  data$lasso_residuals <- lasso_residuals
  
  non_zero_averaged_coefficients_df <- data.frame(
    readable_name = as.character(unname(sapply(names(averaged_coefficients), get_readable_label))),
    name = names(averaged_coefficients),
    value = as.numeric(unname(averaged_coefficients))
  )
    
  non_zero_averaged_coefficients_df <- non_zero_averaged_coefficients_df[order(-abs(non_zero_averaged_coefficients_df$value)), ]
  
  # Return a list with data and non_zero_averaged_coefficients_df
  return(list(data = data, coef = non_zero_averaged_coefficients_df))
}

# Run the averaged lasso regression for each age group
results_15_21 <- run_lasso_average(gallup_data_clean_young_15_21)
data_15_21 <- results_15_21$data %>% mutate(age_group = "15-21")
coefficients_15_21 <- results_15_21$coef


results_22_29 <- run_lasso_average(gallup_data_clean_young_22_29)
data_22_29 <- results_22_29$data %>% mutate(age_group = "22-29")
coefficients_22_29 <- results_22_29$coef


combined_data <- rbind(data_15_21, data_22_29) %>%
  dplyr::group_by(year, age_group) %>%
  dplyr::summarise(average_resid = weighted.mean(residuals, w = WGT, na.rm = TRUE),
                    average_lasso_resid = weighted.mean(lasso_residuals, w = WGT, na.rm = TRUE))

ggplot(combined_data, aes(x = year, group = age_group)) +
  geom_line(aes(y = average_resid, color = age_group), linetype = "solid") +
  geom_point(aes(y = average_resid, color = age_group)) +
  geom_line(aes(y = average_lasso_resid, color = age_group), linetype = "dashed") +
  geom_point(aes(y = average_lasso_resid, color = age_group)) +
  labs(title = "Average of Residuals by Year for Each Age Group",
       x = "Year",
       y = "Average Residuals",
       color = "Age Group") +
  scale_color_manual(values = c("15-21" = "blue", "22-29" = "red")) +
  theme_minimal()
