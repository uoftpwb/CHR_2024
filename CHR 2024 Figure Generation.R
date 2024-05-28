# CHR 2024 Figure Generation
# Author: Anthony McCanny
# Date: May 28, 2024
###############################################

######### LOAD PACKAGES #########
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(hmisc)


######### DECLARE THEMES AND COLOURS #########
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


######### DECLARE FUNCTIONS #########
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

generate_trajectory_plot <- function(data, condition, true_label = " ", false_label = " ", single_line = FALSE, colour = 1, override_filename = FALSE, override_title = NULL, file_path = NULL) {
  if (data == "from tables" && !is.null(file_path)) {
    data <- list.files(path = file_path, pattern = "*.csv", full.names = TRUE) %>%
      purrr::map_dfr(~ read.csv(.x) %>%
        mutate(year = as.numeric(paste0("20", sub(paste0(file_path, "/.*_(.*)_CCHS.csv"), "\\1", .x)))))
    
    plot_data <- data %>%
      mutate(display_year = case_when(as.numeric(year) <= 2023 ~ as.numeric(year),
                                      year == 201516 ~ 2016,
                                      year == 201718 ~ 2018,
                                      year == 201920 ~ 2020)) %>%
      drop_na(age_ranges) %>%
      mutate(binary_var = if_else(eval(parse(text = condition)), true_label, false_label)) %>%
      mutate(young = age_ranges == "15-29") %>%
      group_by(display_year, young, binary_var) %>%
      mutate(proportion = frequency / sum(frequency)) %>%
      summarize(average_ls = sum(proportion * as.numeric(ls)), .groups = 'drop')
  } else {
    plot_data <- data %>%
      mutate(binary_var = if_else(eval(parse(text = condition)), true_label, false_label)) %>%
      group_by(year, binary_var) %>%
      dplyr::summarize(
        average_ls = weighted.mean(ls, WGT, na.rm = TRUE),
        var_ls = wtd.var(ls, weights = WGT, na.rm = TRUE),
        n = sum(!is.na(ls)),
        .groups = "drop"
      ) %>%
      mutate(
        se_ls = sqrt(var_ls / n),
        ci_lower = average_ls - qt(0.975, df = n - 1) * se_ls,
        ci_upper = average_ls + qt(0.975, df = n - 1) * se_ls
      ) %>%
      drop_na(binary_var)
  }
  
  # Automatically determine min_y and max_y based on the confidence intervals
  min_y <- floor(min(plot_data$ci_lower, na.rm = TRUE)*2)/2
  max_y <- ceiling(max(plot_data$ci_upper, na.rm = TRUE)*2)/2
  y_breaks <- seq(min_y, max_y, by = 0.5)
  rect_data <- data.frame(ymin = head(y_breaks, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks, -1)[c(TRUE, FALSE)])
  
  # Create the plot
  age_group_ls_plot_gallup <- ggplot() +
    geom_rect(data = rect_data, 
              aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
              fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE)
  
  if (single_line) {
    age_group_ls_plot_gallup <- age_group_ls_plot_gallup +
      geom_ribbon(data = plot_data, aes(x = year, ymin = ci_lower, ymax = ci_upper), fill = four_tone_scale_colours[colour], alpha = 0.2) +
      geom_line(data = plot_data, aes(x = year, y = average_ls), color = four_tone_scale_colours[colour]) +
      geom_point(data = plot_data, aes(x = year, y = average_ls), color = four_tone_scale_colours[colour])
  } else {
    age_group_ls_plot_gallup <- age_group_ls_plot_gallup +
      geom_ribbon(data = plot_data, aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = binary_var), alpha = 0.2) +
      geom_line(data = plot_data, aes(x = year, y = average_ls, group = binary_var, color = binary_var)) +
      geom_point(data = plot_data, aes(x = year, y = average_ls, group = binary_var, color = binary_var))
  }
  
  plot_title <- if (!is.null(override_title)) override_title else paste("Life Satisfaction for", tools::toTitleCase(true_label), "(Gallup)")
  
  age_group_ls_plot_gallup <- age_group_ls_plot_gallup +
    labs(title = plot_title,
         y = "Average Life Evaluation",
         color = "Group") +
    theme_chr() +
    theme(aspect.ratio = 1/3, plot.background = element_rect(fill = background_colour, color = background_colour), axis.title.y = element_text(angle = 90), legend.key.width = unit(1, "cm")) +  # Adjusted legend key width
    scale_y_continuous(limits = c(min_y, max_y), breaks = y_breaks) +
    scale_x_continuous(limits = c(floor(min(plot_data$year, na.rm = TRUE)), ceiling(max(plot_data$year, na.rm = TRUE))), breaks = seq(floor(min(plot_data$year, na.rm = TRUE)), ceiling(max(plot_data$year, na.rm = TRUE)), by = 1), labels = c(seq(floor(min(plot_data$year, na.rm = TRUE)), ceiling(max(plot_data$year, na.rm = TRUE))-1, by = 1), ""), 
                        expand = c(0, 0)) +
    scale_color_manual(values = setNames(c(six_tone_scale_colours[1], six_tone_scale_colours[3]), c(true_label, false_label)))
  # Print the plot
  print(age_group_ls_plot_gallup)
  
  # Save the plot in different formats
  true_label_formatted <- tolower(gsub("[[:punct:]]", "", gsub(" ", "_", true_label)))
  base_filename <- if (is.character(override_filename)) override_filename else paste0("trajectory_by_", true_label_formatted, "_Canada_Gallup")
  
  ggsave(paste0("Output/Plots/Trajectory/PNG/", base_filename, ".png"), plot = age_group_ls_plot_gallup + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
  ggsave(paste0("Output/Plots/Trajectory/JPG/", base_filename, ".jpg"), plot = age_group_ls_plot_gallup, width = 9, height = 3, dpi = 300)
  ggsave(paste0("Output/Plots/Trajectory/SVG/", base_filename, ".svg"), plot = age_group_ls_plot_gallup + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
}

######## LOAD GALLUP DATA #########
gallup <- readRDS("Data/Gallup/GWP_cleaned_CHR2024_240430.rds") %>%
  filter(COUNTRYNEW == "Canada") %>%
  select(year = YEAR_WAVE, ls = WP16, age = WP1220, WGT, date = WP4, province = REGION2_CAN) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  group_by(year) %>%
  mutate(average_date = mean(date, na.rm = TRUE)) %>%
  mutate(average_date = case_when(
    year <= 2007 ~ as.Date(paste(year, "-07-01", sep="")),
    TRUE ~ average_date
  )) %>%
  mutate(year = as.numeric(format(average_date, "%Y")) + (as.numeric(format(average_date, "%j")) - 1) / 365) %>%
  mutate(age_ranges = case_when(
            age >= 15 & age < 30 ~ "15-29",
            age >= 30 & age < 45 ~ "30-44",
            age >= 45 & age < 60 ~ "45-59",
            age >= 60 & age < 100 ~ "60+",
            TRUE ~ NA_character_))

######## GENERATE PLOTS #########
gallup_trajectory_plot(gallup, TRUE, single_line=TRUE, colour=4, override_filename = "gallup_ls_trajectory_canada", override_title = "Average Evaluations of Life in Canada (Gallup)")

