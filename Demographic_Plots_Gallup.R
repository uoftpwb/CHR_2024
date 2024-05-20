# Demographic Visualizations (Gallup) for CHR 2024
# Author: Anthony McCanny
# Date: May 3, 2024
###############################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(stringr)



theme_chr <- function() {
  theme_minimal(base_family = "Open Sans") %+replace%
    theme(
          plot.title = element_text(size = rel(1.2)),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = rel(1), angle = 90, margin = margin(t = 0, r = 10, b = 0, l = 0)),
          legend.title = element_text(size = rel(0.8)),
          legend.text = element_text(size = rel(0.8)),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x = element_line(color = "black"),
          axis.ticks.x.top = element_line(color = "black"),
          axis.line.x = element_line(color = "black", lineend = "square"),
          axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0), hjust = -0.25),
          plot.background = element_rect(fill = background_colour, color = NA),
          axis.ticks.length = unit(-0.4, "lines")
          )
}

wellbeing_scale_colours <- c("#794979", "#8d4c7c", "#a1507d", "#b4547b", "#c55a77", "#d46171", "#e16b6a", "#eb7761", "#f28557", "#f5944c", "#f5a542")
wellbeing_scale_colours_separate <- c("#f5a542", "#e3c51e", "#83b13e", "#40af5d", "#00a780", "#00999a", "#008dab", "#007fb8", "#496cae", "#6b5a97", "#794979")
background_colour <- "#fef8f9"
accent_background_colour <- "#fdeeef"
four_tone_scale_colours <- c("#da7170", "#eea449", "#b6bd58", "#8a5c7c")
six_tone_scale_colours <- c("#da7170", "#eea449", "#b6bd58", "#8a5c7c", "#006ba2", "#3ebcd2")

############## GALLUP DATA ####################

# Load the Gallup data for Canada and perform initial filtering and renaming of variables
gallup_data_raw <- readRDS("Data/Gallup/GWP_021723_FullyCleaned_Canada.rds") %>%
  filter(COUNTRYNEW == "Canada") %>%
  rename(year = YEAR_WAVE, ls = WP16, age = WP1220, date = WP4, province = REGION2_CAN) %>%
  # Convert the date to Date format and extract month and day of the week
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         month = format(date, "%m"),
         day_of_week = weekdays(date)) %>%
  group_by(year) %>%
  mutate(average_date = mean(date, na.rm = TRUE)) %>%
  mutate(average_date = case_when(
    year <= 2007 ~ as.Date(paste(year, "-07-01", sep="")),
    TRUE ~ average_date
  )) %>%
  mutate(year = as.numeric(format(average_date, "%Y")) + (as.numeric(format(average_date, "%j")) - 1) / 365) %>%
  ungroup() %>%
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
  drop_na(ls) %>%
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
                         .default = as.character(WP1233)))

############## YOUTH-SPECIFIC DEMOGRAPHIC PLOTS  ####################

# Load candidate variables from Lasso
coefficients_15_21 <- read.csv("Output/lasso_variables_15_21.csv")
coefficients_22_29 <- read.csv("Output/lasso_variables_22_29.csv")





##### Trajectory by Age-group Overtime

create_trajectory_plot(gallup_data_raw, "province == 11", "P.E.I.", "Rest of Canada")
create_trajectory_plot(gallup_data_raw, "REGION_CAN == 3", "Quebec Residents (not Montreal)", "Rest of Canada")
create_trajectory_plot(gallup_data_raw, "WP17625 == 1", "Landline Phone at Home", "No Landline Phone")
create_trajectory_plot(gallup_data_raw, "WP118 == 1", "Assaulted Past Year", "Others")
create_trajectory_plot(gallup_data_raw, "WP1219 == 1", "Men", "Women")
create_trajectory_plot(gallup_data_raw, "log_income <= 11", "Below Poverty Line", "Above Poverty Line")

create_trajectory_plot(gallup_data_raw, "province == 35", "Ontario Residents", "Rest of Canada")

create_trajectory_plot(gallup_data_raw, "WP4657 == 1", "Born in Canada", "Not Born in Canada")

create_trajectory_plot(gallup_data_raw, "WP31 == 3", "SoL Decreasing", "SoL not Decreasing")

create_trajectory_plot(gallup_data_raw, "INCOME_5 <= 2 ", "Bottom 40%", "Other Quintiles")

create_trajectory_plot(gallup_data_raw, "EMP_FTEMP == 1", "Employed Full Time", "Not")

create_trajectory_plot(gallup_data_raw, "REGION_CAN == 4", "Toronto", "Rest of Canada")

create_trajectory_plot_cchs(cchs, "language == 'French'", "French Speakers", "Rest of Canada")



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

create_trajectory_plot <- function(data, condition, true_label, false_label) {
  # Create a new binary variable based on the condition
  data <- data %>%
    mutate(binary_var = if_else(eval(parse(text = condition)), true_label, false_label)) %>%
    mutate(age_group = case_when(
      age >= 15 & age <= 21 ~ "15-21",
      age > 21 & age <= 29 ~ "22-29",
      TRUE ~ NA_character_
    )) %>% 
    filter(!is.na(age_group)) # Filter out NA age groups
  
  # Calculate average life satisfaction for each age group, by year and binary variable
  gallup_avg_ls <- data %>%
    group_by(year, age_group, binary_var) %>%
    dplyr::summarize(average_ls = weighted.mean(ls, WGT, na.rm = TRUE), .groups = "drop")  %>%
    drop_na(binary_var)
  
  # Automatically determine min_y and max_y based on the data
  min_y <- floor(min(gallup_avg_ls$average_ls, na.rm = TRUE))
  max_y <- ceiling(max(gallup_avg_ls$average_ls, na.rm = TRUE))
  y_breaks <- seq(min_y, max_y, by = 0.5)
  rect_data <- data.frame(ymin = head(y_breaks, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks, -1)[c(TRUE, FALSE)])
  
  # Create the plot
age_group_ls_plot_gallup <- ggplot() +
  geom_rect(data = rect_data, 
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
            fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
  geom_line(data = filter(gallup_avg_ls, age_group == "15-21"), aes(x = year, y = average_ls, group = binary_var, color = binary_var, linetype = "15-21")) +
  geom_line(data = filter(gallup_avg_ls, age_group == "22-29"), aes(x = year, y = average_ls, group = binary_var, color = binary_var, linetype = "22-29")) +
  geom_point(data = filter(gallup_avg_ls, age_group == "15-21"), aes(x = year, y = average_ls, group = binary_var, color = binary_var, shape = "15-21")) +
  geom_point(data = filter(gallup_avg_ls, age_group == "22-29"), aes(x = year, y = average_ls, group = binary_var, color = binary_var, shape = "22-29")) +
  labs(title = paste("Youth Life Satisfaction for", tools::toTitleCase(true_label), "(Gallup)"),
       y = "Average Life Satisfaction",
       color = "Group",
       linetype = "Age Group",
       shape = "Age Group") +
  theme_chr() +
  theme(aspect.ratio = 1/3, plot.background = element_rect(fill = background_colour, color = background_colour), axis.title.y = element_text(angle = 90), legend.key.width = unit(1, "cm")) +  # Adjusted legend key width
  scale_y_continuous(limits = c(min_y, max_y), breaks = y_breaks) +
  scale_x_continuous(limits = c(floor(min(sample_size$year, na.rm = TRUE)), ceiling(max(sample_size$year, na.rm = TRUE))), breaks = seq(floor(min(sample_size$year, na.rm = TRUE)), ceiling(max(sample_size$year, na.rm = TRUE)), by = 1), labels = c(seq(floor(min(sample_size$year, na.rm = TRUE)), ceiling(max(sample_size$year, na.rm = TRUE))-1, by = 1), ""), 
                      expand = c(0, 0)) +
  scale_color_manual(values = setNames(c(six_tone_scale_colours[1], six_tone_scale_colours[3]), c(true_label, false_label))) +
  scale_linetype_manual(values = c("15-21" = "dashed", "22-29" = "solid")) +
  scale_shape_manual(values = c("15-21" = 17, "22-29" = 16))

  # Print the plot
  print(age_group_ls_plot_gallup)
  
  # Save the plot in different formats
  true_label_formatted <- tolower(gsub("[[:punct:]]", "", gsub(" ", "_", true_label)))
  ggsave(paste0("Output/Plots/Trajectory/PNG/trajectory_by_age_and_", true_label_formatted, "_Canada_Gallup.png"), plot = age_group_ls_plot_gallup + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
  ggsave(paste0("Output/Plots/Trajectory/JPG/trajectory_by_age_and_", true_label_formatted, "_Canada_Gallup.jpg"), plot = age_group_ls_plot_gallup, width = 9, height = 3, dpi = 300)
  ggsave(paste0("Output/Plots/Trajectory/SVG/trajectory_by_age_and_", true_label_formatted, "_Canada_Gallup.svg"), plot = age_group_ls_plot_gallup + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
}

create_trajectory_plot_cchs <- function(data, condition, true_label, false_label) {
  # Create a new binary variable based on the condition
  data <- data %>%
    mutate(binary_var = if_else(eval(parse(text = condition)), true_label, false_label)) %>%
    mutate(age_group = case_when(
      age >= 15 & age <= 21 ~ "15-21",
      age > 21 & age <= 29 ~ "22-29",
      TRUE ~ NA_character_),
      year = numeric_year) %>%
    filter(!is.na(age_group)) # Filter out NA age groups
  
  # Calculate average life satisfaction for each age group, by year and binary variable
  cchs_avg_ls <- data %>%
    group_by(year, age_group, binary_var) %>%
    dplyr::summarize(average_ls = weighted.mean(ls, weight, na.rm = TRUE), .groups = "drop")  %>%
    drop_na(binary_var)
  
  # Automatically determine min_y and max_y based on the data
  min_y <- floor(min(cchs_avg_ls$average_ls, na.rm = TRUE))
  max_y <- ceiling(max(cchs_avg_ls$average_ls, na.rm = TRUE))
  y_breaks <- seq(min_y, max_y, by = 0.5)
  rect_data <- data.frame(ymin = head(y_breaks, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks, -1)[c(TRUE, FALSE)])
  
  # Create the plot
age_group_ls_plot_cchs <- ggplot() +
  geom_rect(data = rect_data, 
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
            fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
  geom_line(data = filter(cchs_avg_ls, age_group == "15-21"), aes(x = year, y = average_ls, group = binary_var, color = binary_var, linetype = "15-21")) +
  geom_line(data = filter(cchs_avg_ls, age_group == "22-29"), aes(x = year, y = average_ls, group = binary_var, color = binary_var, linetype = "22-29")) +
  geom_point(data = filter(cchs_avg_ls, age_group == "15-21"), aes(x = year, y = average_ls, group = binary_var, color = binary_var, shape = "15-21")) +
  geom_point(data = filter(cchs_avg_ls, age_group == "22-29"), aes(x = year, y = average_ls, group = binary_var, color = binary_var, shape = "22-29")) +
  labs(title = paste("Youth Life Satisfaction for", tools::toTitleCase(true_label), "(CCHS)"),
       y = "Average Life Satisfaction",
       color = "Group",
       linetype = "Age Group",
       shape = "Age Group") +
  theme_chr() +
  theme(aspect.ratio = 1/3, plot.background = element_rect(fill = background_colour, color = background_colour), axis.title.y = element_text(angle = 90), legend.key.width = unit(1, "cm")) +  # Adjusted legend key width
  scale_y_continuous(limits = c(min_y, max_y), breaks = y_breaks) +
  scale_x_continuous(limits = c(floor(min(data$year, na.rm = TRUE)), ceiling(max(data$year, na.rm = TRUE))), breaks = seq(floor(min(data$year, na.rm = TRUE)), ceiling(max(data$year, na.rm = TRUE)), by = 1), labels = c(seq(floor(min(data$year, na.rm = TRUE)), ceiling(max(data$year, na.rm = TRUE))-1, by = 1), ""), 
                      expand = c(0, 0)) +
  scale_color_manual(values = setNames(c(six_tone_scale_colours[1], six_tone_scale_colours[3]), c(true_label, false_label))) +
  scale_linetype_manual(values = c("15-21" = "dashed", "22-29" = "solid")) +
  scale_shape_manual(values = c("15-21" = 17, "22-29" = 16))

  # Print the plot
  print(age_group_ls_plot_cchs)
  
  # Save the plot in different formats
  true_label_formatted <- tolower(gsub("[[:punct:]]", "", gsub(" ", "_", true_label)))
  ggsave(paste0("Output/Plots/Trajectory/PNG/trajectory_by_age_and_", true_label_formatted, "_Canada_CCHS.png"), plot = age_group_ls_plot_cchs + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
  ggsave(paste0("Output/Plots/Trajectory/JPG/trajectory_by_age_and_", true_label_formatted, "_Canada_CCHS.jpg"), plot = age_group_ls_plot_cchs, width = 9, height = 3, dpi = 300)
  ggsave(paste0("Output/Plots/Trajectory/SVG/trajectory_by_age_and_", true_label_formatted, "_Canada_CCHS.svg"), plot = age_group_ls_plot_cchs + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
}





create_sample_size_plot <- function(data, condition, true_label, false_label, age_range1, age_range2) {
  age_range1_bounds <- if (str_detect(age_range1, "\\+")) {
    list(as.numeric(str_remove(age_range1, "\\+")), Inf)
  } else {
    as.numeric(str_split(age_range1, "-")[[1]])
  }
  
  age_range2_bounds <- if (str_detect(age_range2, "\\+")) {
    list(as.numeric(str_remove(age_range2, "\\+")), Inf)
  } else {
    as.numeric(str_split(age_range2, "-")[[1]])
  }

  # Create a new binary variable based on the condition
  data <- data %>%
    mutate(binary_var = if_else(eval(parse(text = condition)), true_label, false_label)) %>%
    mutate(age_group = case_when(
      age >= age_range1_bounds[[1]] & (is.infinite(age_range1_bounds[[2]]) | age <= age_range1_bounds[[2]]) ~ age_range1,
      age >= age_range2_bounds[[1]] & (is.infinite(age_range2_bounds[[2]]) | age <= age_range2_bounds[[2]]) ~ age_range2,
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(age_group)) # Filter out NA age groups
  # Calculate the sample size for each age group, by year and binary variable
  sample_size <- data %>%
    group_by(year, age_group, binary_var) %>%
    dplyr::summarize(n = n(), .groups = "drop") %>%
    drop_na(binary_var)
  
  # Automatically determine min_y and max_y based on the data
  min_y <- 0
  max_sample_size <- max(sample_size$n, na.rm = TRUE)
  if (max_sample_size > 900) {
    y_increment <- 200
  } else if (max_sample_size > 500) {
    y_increment <- 100
  } else if (max_sample_size > 150) {
    y_increment <- 50
  } else if (max_sample_size < 50) {
    y_increment <- 10
  } else {
    y_increment <- 20
  }
  max_y <- ceiling(max_sample_size / y_increment) * y_increment
  y_breaks <- seq(min_y, max_y, by = y_increment)
  rect_data <- data.frame(ymin = head(y_breaks, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks, -1)[c(TRUE, FALSE)])
  
  # Create the plot
age_group_sample_size_plot <- ggplot() +
  geom_rect(data = rect_data, 
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
            fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
  geom_line(data = filter(sample_size, age_group == age_range1), aes(x = year, y = n, group = binary_var, color = binary_var, linetype = age_range1)) +
  geom_line(data = filter(sample_size, age_group == age_range2), aes(x = year, y = n, group = binary_var, color = binary_var, linetype = age_range2)) +
  geom_point(data = filter(sample_size, age_group == age_range1), aes(x = year, y = n, group = binary_var, color = binary_var, shape = age_range1)) +
  geom_point(data = filter(sample_size, age_group == age_range2), aes(x = year, y = n, group = binary_var, color = binary_var, shape = age_range2)) +
  labs(title = paste("Sample Size by Age Group for", tools::toTitleCase(true_label), "(Gallup)"),
       y = "Sample Size",
       color = "Group",
       linetype = "Age Group",
       shape = "Age Group") +
  theme_chr() +
  theme(aspect.ratio = 1/3, plot.background = element_rect(fill = background_colour, color = background_colour), axis.title.y = element_text(angle = 90), legend.key.width = unit(1, "cm")) +  # Adjusted legend key width
  scale_y_continuous(limits = c(min_y, max_y), breaks = y_breaks) +
  scale_x_continuous(limits = c(floor(min(sample_size$year, na.rm = TRUE)), ceiling(max(sample_size$year, na.rm = TRUE))), breaks = seq(floor(min(sample_size$year, na.rm = TRUE)), ceiling(max(sample_size$year, na.rm = TRUE)), by = 1), labels = c(seq(floor(min(sample_size$year, na.rm = TRUE)), ceiling(max(sample_size$year, na.rm = TRUE))-1, by = 1), ""), 
                      expand = c(0, 0)) +
  scale_color_manual(values = setNames(c(six_tone_scale_colours[1], six_tone_scale_colours[3]), c(true_label, false_label))) +
  scale_linetype_manual(values = setNames(c("dashed", "solid"), c(age_range1, age_range2))) +
  scale_shape_manual(values = setNames(c(17, 16), c(age_range1, age_range2)))
  
  # Print the plot
  print(age_group_sample_size_plot)
  
  # Save the plot in different formats
  true_label_formatted <- tolower(gsub("[[:punct:]]", "", gsub(" ", "_", true_label)))
  age_range1_formatted <- tolower(gsub("[[:punct:]]", "", gsub(" ", "_", age_range1)))
  age_range2_formatted <- tolower(gsub("[[:punct:]]", "", gsub(" ", "_", age_range2)))


  ggsave(paste0("Output/Plots/Sample Size/PNG/sample_size_", true_label_formatted, "_ages_", age_range1_formatted, "_and_", age_range2_formatted, ".png"), plot = age_group_sample_size_plot + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
  ggsave(paste0("Output/Plots/Sample Size/JPG/sample_size_", true_label_formatted, "_ages_", age_range1_formatted, "_and_", age_range2_formatted, ".jpg"), plot = age_group_sample_size_plot, width = 9, height = 3, dpi = 300)
  ggsave(paste0("Output/Plots/Sample Size/SVG/sample_size_", true_label_formatted, "_ages_", age_range1_formatted, "_and_", age_range2_formatted, ".svg"), plot = age_group_sample_size_plot + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
}

create_sample_size_plot(gallup_data_raw, "log_income <= 10.14741", "Below Poverty Line", "Above Poverty Line", "15-29", "30-44")
create_sample_size_plot(gallup_data_raw, "province == 24", "Quebec", "Not", "15-29", "30-44")
create_sample_size_plot(gallup_data_raw, "WP118 == 1", "Assaulted Past Year", "Not Assaulted Past Year", "15-21", "22-29")
# Access to Internet
create_sample_size_plot(gallup_data_raw, "WP39 == 1", "Internet at Home", "No Internet at Home", "15-21", "22-29")
# Received Money or Goods
create_sample_size_plot(gallup_data_raw, "WP39 == 1", "Internet at Home", "No Internet at Home", "15-21", "22-29")
#Landline in Home
create_sample_size_plot(gallup_data_raw, "WP17625 == 1", "Landline", "No Landline", "15-21", "22-29")
#Smile or Laugh
create_sample_size_plot(gallup_data_raw, "WP63 == 1", "Smiled or Laughed", "Did Not Smile or Laugh", "15-21", "22-29")
# Born in Canada
create_sample_size_plot(gallup_data_raw, "WP4657 == 1", "Born in Canada", "Not Born in Canada", "15-21", "22-29")
# Move Permanently
create_sample_size_plot(gallup_data_raw, "WP1325 == 1", "Like to Move", "Like to Stay", "15-21", "22-29")


create_proportion_plot(gallup_data_raw, "WP1325 == 1", "Like to Move Permanently", "15-29", "30+")

create_proportion_plot(gallup_data_raw, "log_income <= 9.0", "Below Poverty Line", "15-29", "30+")

create_proportion_plot(gallup_data_raw, "WP31 == 3", "SoL Decreasing", "15-29", "30+")
create_proportion_plot(gallup_data_raw, "WP31 == 1", "SoL Increasing", "15-29", "30+")
create_proportion_plot(gallup_data_raw, "WP95 == 1", "Quality of Water Good", "15-29", "30+")
create_proportion_plot(gallup_data_raw, "INCOME_5 == 1", "Lowest Quintile", "15-29", "30+")
create_proportion_plot(gallup_data_raw, "WP134==1", "Freedom in Life", "15-29", "30+")
create_proportion_plot(gallup_data_raw, "WP63 == 1", "Smiled or Laughed", "15-29", "30+")
create_proportion_plot(gallup_data_raw, "EMP_FTEMP == 1", "Employed Full Time", "15-29", "30+")

assault <- create_proportion_plot(filter(gallup_data_raw, WP1219 == 0), "WP118 == 1", "Women Assaulted Past Year", "15-21", "22-29")

create_sample_size_plot(filter(gallup_data_raw, WP1219 == 0), "WP118 == 1", "Women Assaulted Past Year", "Women Not Assaulted Past Year", "15-21", "22-29")

data = gallup_data_raw; condition = "log_income <= 10.14741"; true_label = "Below Poverty Line"; false_label = "Above Poverty Line"; age_range1 = "15-29"; age_range2 = "30+"
# Example usage:
# create_trajectory_plot(gallup_data_raw, "employment_status")


create_proportion_plot <- function(data, condition, true_label, age_range1, age_range2) {
  age_range1_bounds <- if (str_detect(age_range1, "\\+")) {
    list(as.numeric(str_remove(age_range1, "\\+")), Inf)
  } else {
    as.numeric(str_split(age_range1, "-")[[1]])
  }
  
  age_range2_bounds <- if (str_detect(age_range2, "\\+")) {
    list(as.numeric(str_remove(age_range2, "\\+")), Inf)
  } else {
    as.numeric(str_split(age_range2, "-")[[1]])
  }

  # Create a new binary variable based on the condition
  data <- data %>%
    mutate(condition_met = eval(parse(text = condition))) %>%
    mutate(age_group = case_when(
      age >= age_range1_bounds[[1]] & (is.infinite(age_range1_bounds[[2]]) | age <= age_range1_bounds[[2]]) ~ age_range1,
      age >= age_range2_bounds[[1]] & (is.infinite(age_range2_bounds[[2]]) | age <= age_range2_bounds[[2]]) ~ age_range2,
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(age_group)) # Filter out NA age groups

  # Calculate the proportion for each age group, by year
  proportions <- data %>%
    group_by(year, age_group) %>%
    dplyr::summarize(proportion = mean(condition_met, na.rm = TRUE), .groups = "drop")

  min_y <- 0
  max_proportion <- max(proportions$proportion, na.rm = TRUE)
  y_increment <- if (max_proportion > 0.5) 0.25 else 0.05
  max_y <- ceiling(max_proportion / y_increment) * y_increment
  y_breaks <- seq(min_y, max_y, by = y_increment)
  rect_data <- data.frame(ymin = head(y_breaks, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks, -1)[c(TRUE, FALSE)])
  

  # Create the plot
  proportion_plot <- ggplot(proportions, aes(x = year, y = proportion, group = age_group, color = age_group)) +
  geom_rect(data = rect_data, 
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
            fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
    geom_line() +
    geom_point() +
    labs(title = paste("Proportion Meeting Condition '", tools::toTitleCase(true_label), "' by Age Group (Gallup)"),
         y = "Proportion",
         x = "Year",
         color = "Age Group") +
    theme_chr() +
    theme(aspect.ratio = 1/3, plot.background = element_rect(fill = background_colour, color = background_colour), axis.title.y = element_text(angle = 90), legend.key.width = unit(1, "cm")) +
    scale_y_continuous(limits = c(min_y, max_y), breaks = y_breaks) +
    scale_x_continuous(limits = c(floor(min(sample_size$year, na.rm = TRUE)), ceiling(max(sample_size$year, na.rm = TRUE))), breaks = seq(floor(min(sample_size$year, na.rm = TRUE)), ceiling(max(sample_size$year, na.rm = TRUE)), by = 1), labels = c(seq(floor(min(sample_size$year, na.rm = TRUE)), ceiling(max(sample_size$year, na.rm = TRUE))-1, by = 1), ""), 
                      expand = c(0, 0)) +
    scale_color_manual(values = setNames(c(six_tone_scale_colours[1], six_tone_scale_colours[3]), c(age_range1, age_range2)))

  # Print the plot
  print(proportion_plot)
  
  # Save the plot in different formats
  true_label_formatted <- tolower(gsub("[[:punct:]]", "", gsub(" ", "_", true_label)))
  age_range1_formatted <- tolower(gsub("[[:punct:]]", "", gsub(" ", "_", age_range1)))
  age_range2_formatted <- tolower(gsub("[[:punct:]]", "", gsub(" ", "_", age_range2)))

  ggsave(paste0("Output/Plots/Proportion/PNG/proportion_", true_label_formatted, "_ages_", age_range1_formatted, "_and_", age_range2_formatted, ".png"), plot = proportion_plot + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
  ggsave(paste0("Output/Plots/Proportion/JPG/proportion_", true_label_formatted, "_ages_", age_range1_formatted, "_and_", age_range2_formatted, ".jpg"), plot = proportion_plot, width = 9, height = 3, dpi = 300)
  ggsave(paste0("Output/Plots/Proportion/SVG/proportion_", true_label_formatted, "_ages_", age_range1_formatted, "_and_", age_range2_formatted, ".svg"), plot = proportion_plot + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")

  return(proportions)

}


torontonians2018 <- filter(gallup_data_raw, REGION_CAN == 4 & year >=2018 & year <= 2019 & age_ranges == "15-29")
