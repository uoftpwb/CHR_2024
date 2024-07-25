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
library(Hmisc)


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

generate_trajectory_plot <- function(data=NULL, condition, true_label = " ", false_label = " ", single_line = FALSE, colour = 1, override_filename = FALSE, override_title = NULL, from_tables=FALSE, file_path = NULL) {
  data_source_caption <- ""
  
  if (from_tables && !is.null(file_path)) {
    data <- list.files(path = file_path, pattern = "*.csv", full.names = TRUE) %>%
      purrr::map_dfr(~ read.csv(.x) %>%
        mutate(year = as.numeric(paste0("20", sub(paste0(file_path, "/.*_(.*)_CCHS.csv"), "\\1", .x))))) %>%
      mutate(year = case_when(as.numeric(year) <= 2023 ~ as.numeric(year)+0.5,
                                      year == 201516 ~ 2016,
                                      year == 201718 ~ 2018,
                                      year == 201920 ~ 2020),
            ls = as.numeric(ls)) %>%
      group_by(year) %>%
      mutate(WGT = frequency / sum(frequency) * 65000) %>%
      ungroup() %>%
      drop_na(age_ranges)
    data_source_caption <- "Data Source: Canadian Community Health Survey"
  } 
  
  if (!is.null(data) && !from_tables) {
    data_source_caption <- "Data Source: Gallup World Poll"
  }
  
  if (!is.null(data) && from_tables) {
    data_source_caption <- "Data Source: Gallup World Poll, Canadian Community Health Survey"
  }
  
  plot_data <- data %>%
    mutate(binary_var = if_else(eval(parse(text = condition)), true_label, false_label)) %>%
    mutate(young = age_ranges == "15-29") %>%
    group_by(year, binary_var) %>%
    dplyr::summarize(
      average_ls = weighted.mean(ls, WGT, na.rm = TRUE),
      var_ls = wtd.var(ls, weights = WGT, na.rm = TRUE),
      n = sum(WGT, na.rm=TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      se_ls = sqrt(var_ls / n),
      ci_lower = average_ls - qt(0.975, df = n - 1) * se_ls,
      ci_upper = average_ls + qt(0.975, df = n - 1) * se_ls
    ) %>%
    drop_na(binary_var)

  
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
         color = "Group",
         caption = data_source_caption) +
    theme_chr() +
    theme(aspect.ratio = 1/3, plot.background = element_rect(fill = background_colour, color = background_colour), axis.title.y = element_text(angle = 90), legend.key.width = unit(1, "cm"), plot.caption = element_text(hjust = 0, size = 8, face = "italic")) +  # Adjusted legend key width
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

  return(plot_data)
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





######## LOAD CCHS DATA #########
# Read all the PUMF CSV files and combine them into one dataframe
PUMF_list <- list.files(path = "Output/PROVxAGExLS Tables CCHS", pattern = "*.csv", full.names = TRUE)
provincePUMF <- PUMF_list %>%
  purrr::map_dfr(~ read.csv(.x) %>%
    mutate(year = as.numeric(paste0("20", sub("Output/PROVxAGExLS Tables CCHS/PROVxAGExLS_(.*)_CCHS.csv", "\\1", .x)))))

# Read all the RTRA CSV files and combine them into one dataframe
RTRA_list <- list.files(path = "Data/CCHS RTRA", pattern = "^PROVxAGExLS.*\\.csv$", full.names = TRUE)
provinceRTRA <- RTRA_list %>%  # Start a chain of commands using the file_list vector
  purrr::map_dfr(~read.csv(.x) %>%  # Read each file in the list into a dataframe and row-bind them together
    rename_with(~if_else(.x %in% c("LSM_01", "GEN_010"), "ls", .x)) %>%  # Rename columns 'LSM_01' and 'GEN_010' to 'ls'
    mutate(year = as.numeric( paste0("20", sub("Data/CCHS RTRA/PROVxAGExLS_(.*)_CCHS.csv", "\\1", .x))))) %>%  # Extract the year from the filename and add it as a new column
  mutate(province = case_when(
    GEO_PRV == 10 ~ "Newfoundland and Labrador",
    GEO_PRV == 11 ~ "Prince Edward Island",
    GEO_PRV == 12 ~ "Nova Scotia",
    GEO_PRV == 13 ~ "New Brunswick",
    GEO_PRV == 24 ~ "Quebec",
    GEO_PRV == 35 ~ "Ontario",
    GEO_PRV == 46 ~ "Manitoba",
    GEO_PRV == 47 ~ "Saskatchewan",
    GEO_PRV == 48 ~ "Alberta",
    GEO_PRV == 59 ~ "British Columbia",
    GEO_PRV == 60 ~ "Yukon",
    GEO_PRV == 61 ~ "Northwest Territories",
    GEO_PRV == 62 ~ "Nunavut",
    is.na(GEO_PRV) ~ "Canada",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(ls) & ls <= 10) %>%  # Drop rows with NA in 'ls' or 'ls' values higher than 10
  mutate(AGEGROUP = case_when(
    AGEGROUP == ".toless15" ~ NA_character_,
    AGEGROUP == "15toless3" ~ "15-29",
    AGEGROUP == "30toless4" ~ "30-44",
    AGEGROUP == "45toless6" ~ "45-59",
    AGEGROUP == "60andup" ~ "60+",
    TRUE ~ "All ages")) %>%
  select(province, year, ls, age_ranges=AGEGROUP, frequency = X_COUNT_)

# Combine PUMF and RTRA dataframes
province <- rbind(provincePUMF, provinceRTRA)

canada <- province %>%
  filter(province == "Canada")

            
######## GENERATE PLOTS #########


#### GENERATE TRAJECTORY PLOTS #####
# Generate the first plot using Gallup data
gallup_filename <- "gallup_ls_trajectory_canada"
gallup_title <- "Evaluations of Life in Canada"

plot_data <- gallup %>%
  mutate(young = age_ranges == "15-29") %>%
  group_by(year) %>%
  dplyr::summarize(
    average_ls = weighted.mean(ls, WGT, na.rm = TRUE),
    var_ls = wtd.var(ls, weights = WGT, na.rm = TRUE),
    n = sum(WGT, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    se_ls = sqrt(var_ls / n),
    ci_lower = average_ls - qt(0.975, df = n - 1) * se_ls,
    ci_upper = average_ls + qt(0.975, df = n - 1) * se_ls
  )

min_y <- floor(min(plot_data$ci_lower, na.rm = TRUE)*2)/2
max_y <- ceiling(max(plot_data$ci_upper, na.rm = TRUE)*2)/2
y_breaks <- seq(min_y, max_y, by = 0.5)
rect_data <- data.frame(ymin = head(y_breaks, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks, -1)[c(TRUE, FALSE)])

gallup_trajectory_ls <- ggplot() +
  geom_rect(data = rect_data, 
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
            fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
  geom_ribbon(data = plot_data, aes(x = year, ymin = ci_lower, ymax = ci_upper), fill = six_tone_scale_colours[5], alpha = 0.2) +
  geom_line(data = plot_data, aes(x = year, y = average_ls), color = six_tone_scale_colours[5]) +
  geom_point(data = plot_data, aes(x = year, y = average_ls), color = six_tone_scale_colours[5]) +
  labs(title = gallup_title,
       y = "Average Life Evaluation",
       color = "Group",
       caption = "Data Source: Gallup World Poll") +
  theme_chr() +
  theme(aspect.ratio = 1/3, plot.background = element_rect(fill = background_colour, color = background_colour), axis.title.y = element_text(angle = 90), legend.key.width = unit(1, "cm"), plot.caption = element_text(hjust = 0, size = 8, face = "italic")) +
  scale_y_continuous(limits = c(min_y, max_y), breaks = y_breaks) +
  scale_x_continuous(limits = c(floor(min(plot_data$year, na.rm = TRUE)), ceiling(max(plot_data$year, na.rm = TRUE))), breaks = seq(floor(min(plot_data$year, na.rm = TRUE)), ceiling(max(plot_data$year, na.rm = TRUE)), by = 1), labels = c(seq(floor(min(plot_data$year, na.rm = TRUE)), ceiling(max(plot_data$year, na.rm = TRUE))-1, by = 1), ""), 
                    expand = c(0, 0))
# Print the plot
print(gallup_trajectory_ls)

# Save the plot in different formats
base_filename <- gallup_filename

ggsave(paste0("Output/Final Plots2/PNG/", base_filename, ".png"), plot = gallup_trajectory_ls + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 8, height = 3, dpi = 900, bg = "transparent")
ggsave(paste0("Output/Final Plots2/JPG/", base_filename, ".jpg"), plot = gallup_trajectory_ls, width = 8, height = 3, dpi = 300)
ggsave(paste0("Output/Final Plots2/SVG/", base_filename, ".svg"), plot = gallup_trajectory_ls + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 8, height = 3, dpi = 300, bg = "transparent")

# Generate the CCHS plot data
plot_data_cchs <- canada %>%
  mutate(year = case_when(as.numeric(year) <= 2023 ~ as.numeric(year)+0.5,
                          year == 201516 ~ 2016,
                          year == 201718 ~ 2018,
                          year == 201920 ~ 2020),
         ls = as.numeric(ls)) %>%
  group_by(year) %>%
  mutate(WGT = frequency / sum(frequency) * 65000) %>%
  ungroup() %>%
  drop_na(age_ranges) %>%
  mutate(young = age_ranges == "15-29") %>%
  group_by(year) %>%
  dplyr::summarize(
    average_ls = weighted.mean(ls, WGT, na.rm = TRUE),
    var_ls = wtd.var(ls, weights = WGT, na.rm = TRUE),
    n = sum(WGT, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    se_ls = sqrt(var_ls / n),
    ci_lower = average_ls - qt(0.975, df = n - 1) * se_ls,
    ci_upper = average_ls + qt(0.975, df = n - 1) * se_ls
  )

# Create Combined Plot
combined_plot_data <- bind_rows(
  plot_data_cchs %>% mutate(source = "Life Satisfaction - CCHS"),
  plot_data %>% mutate(source = "Life Evaluation - Gallup")
)

min_y_combined <- floor(min(combined_plot_data$ci_lower, na.rm = TRUE) * 2) / 2
max_y_combined <- ceiling(max(combined_plot_data$ci_upper, na.rm = TRUE) * 2) / 2
y_breaks_combined <- seq(min_y_combined, max_y_combined, by = 0.5)
rect_data_combined <- data.frame(ymin = head(y_breaks_combined, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks_combined, -1)[c(TRUE, FALSE)])

combined_trajectory_ls <- ggplot() +
  geom_rect(data = rect_data_combined, 
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
            fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
  geom_vline(xintercept = c(2015, 2022), linetype = "dashed", color = six_tone_scale_colours[1]) +
  # Gallup data
  geom_ribbon(data = combined_plot_data %>% filter(source == "Life Evaluation - Gallup"), 
              aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = source), alpha = 0.2) +
  geom_line(data = combined_plot_data %>% filter(source == "Life Evaluation - Gallup"), 
            aes(x = year, y = average_ls, color = source)) +
  # CCHS data
  geom_ribbon(data = combined_plot_data %>% filter(source == "Life Satisfaction - CCHS" & year >= 2009 & year <= 2015), 
              aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = source), alpha = 0.2) +
  geom_line(data = combined_plot_data %>% filter(source == "Life Satisfaction - CCHS" & year >= 2009 & year <= 2015), 
            aes(x = year, y = average_ls, color = source)) +
  geom_ribbon(data = combined_plot_data %>% filter(source == "Life Satisfaction - CCHS" & year >= 2015 & year <= 2022), 
              aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = source), alpha = 0.2) +
  geom_line(data = combined_plot_data %>% filter(source == "Life Satisfaction - CCHS" & year >= 2015 & year <= 2022), 
            aes(x = year, y = average_ls, color = source)) +
  
  geom_point(data = combined_plot_data, aes(x = year, y = average_ls, color = source)) +
  labs(title = "Life Evaluations and Satisfaction in Canada",
       y = "Average Response",
       color = "Question",
       fill = "Question",
       caption = "Data Source: Gallup World Poll and Canadian Community Health Survey") +
  theme_chr() +
  theme(aspect.ratio = 1/3, plot.background = element_rect(fill = background_colour, color = background_colour), axis.title.y = element_text(angle = 90), legend.key.width = unit(1, "cm"), plot.caption = element_text(hjust = 0, size = 8, face = "italic")) +
  scale_y_continuous(limits = c(min_y_combined, max_y_combined), breaks = y_breaks_combined) +
  scale_x_continuous(limits = c(floor(min(combined_plot_data$year, na.rm = TRUE)), ceiling(max(combined_plot_data$year, na.rm = TRUE))), breaks = seq(floor(min(combined_plot_data$year, na.rm = TRUE)), ceiling(max(combined_plot_data$year, na.rm = TRUE)), by = 1), labels = c(seq(floor(min(combined_plot_data$year, na.rm = TRUE)), ceiling(max(combined_plot_data$year, na.rm = TRUE))-1, by = 1), ""), 
                    expand = c(0, 0)) +
  scale_color_manual(values = six_tone_scale_colours[c(5, 1)]) +
  scale_fill_manual(values = six_tone_scale_colours[c(5, 1)])

# Print the combined plot
print(combined_trajectory_ls)

base_filename <- "combined_trajectory_ls"
ggsave(paste0("Output/Final Plots/PNG/", base_filename, ".png"), plot = combined_trajectory_ls + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
ggsave(paste0("Output/Final Plots/JPG/", base_filename, ".jpg"), plot = combined_trajectory_ls, width = 9, height = 3, dpi = 300)
ggsave(paste0("Output/Final Plots/SVG/", base_filename, ".svg"), plot = combined_trajectory_ls + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")

###### AGE GROUP TRAJECTORY PLOTS ######

# Generate the first plot using Gallup data
gallup_filename <- "gallup_ls_trajectory_canada_by_age"
gallup_title <- "Evaluations of Life in Canada"

plot_data <- gallup %>%
  drop_na(age_ranges) %>%
  group_by(year, age_ranges) %>%
  dplyr::summarize(
    average_ls = weighted.mean(ls, WGT, na.rm = TRUE),
    var_ls = wtd.var(ls, weights = WGT, na.rm = TRUE),
    n = sum(WGT, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    se_ls = sqrt(var_ls / n),
    ci_lower = average_ls - qt(0.975, df = n - 1) * se_ls,
    ci_upper = average_ls + qt(0.975, df = n - 1) * se_ls
  )

min_y <- floor(min(plot_data$ci_lower, na.rm = TRUE)*2)/2
max_y <- ceiling(max(plot_data$ci_upper, na.rm = TRUE)*2)/2
y_breaks <- seq(min_y, max_y, by = 0.5)
rect_data <- data.frame(ymin = head(y_breaks, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks, -1)[c(TRUE, FALSE)])

gallup_trajectory_ls <- ggplot() +
  geom_rect(data = rect_data, 
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
            fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
  geom_ribbon(data = plot_data, aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = age_ranges), alpha = 0.2) +
  geom_line(data = plot_data, aes(x = year, y = average_ls, color = age_ranges)) +
  geom_point(data = plot_data, aes(x = year, y = average_ls, color = age_ranges)) +
  labs(title = gallup_title,
       y = "Average Life Evaluation",
       color = "Age Range",
       fill = "Age Range",
       caption = "Data Source: Gallup World Poll") +
  theme_chr() +
  theme(aspect.ratio = 1/3, plot.background = element_rect(fill = background_colour, color = background_colour), axis.title.y = element_text(angle = 90), legend.key.width = unit(1, "cm"), plot.caption = element_text(hjust = 0, size = 8, face = "italic")) +
  scale_y_continuous(limits = c(min_y, max_y), breaks = y_breaks) +
  scale_x_continuous(limits = c(floor(min(plot_data$year, na.rm = TRUE)), ceiling(max(plot_data$year, na.rm = TRUE))), breaks = seq(floor(min(plot_data$year, na.rm = TRUE)), ceiling(max(plot_data$year, na.rm = TRUE)), by = 1), labels = c(seq(floor(min(plot_data$year, na.rm = TRUE)), ceiling(max(plot_data$year, na.rm = TRUE))-1, by = 1), ""), 
                    expand = c(0, 0)) +
  scale_color_manual(values = four_tone_scale_colours) +
  scale_fill_manual(values = four_tone_scale_colours)
# Print the plot
print(gallup_trajectory_ls)

# Save the plot in different formats
base_filename <- gallup_filename

ggsave(paste0("Output/Final Plots2/PNG/", base_filename, ".png"), plot = gallup_trajectory_ls + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 8, height = 3, dpi = 900, bg = "transparent")
ggsave(paste0("Output/Final Plots2/JPG/", base_filename, ".jpg"), plot = gallup_trajectory_ls, width = 8, height = 3, dpi = 300)
ggsave(paste0("Output/Final Plots2/SVG/", base_filename, ".svg"), plot = gallup_trajectory_ls + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 8, height = 3, dpi = 300, bg = "transparent")

# Generate the CCHS plot data
plot_data_cchs <- canada %>%
  mutate(year = case_when(as.numeric(year) <= 2023 ~ as.numeric(year)+0.5,
                          year == 201516 ~ 2016,
                          year == 201718 ~ 2018,
                          year == 201920 ~ 2020),
         ls = as.numeric(ls)) %>%
  group_by(year, age_ranges) %>%
  mutate(WGT = frequency / sum(frequency) * 65000) %>%
  ungroup() %>%
  drop_na(age_ranges) %>%
  filter(age_ranges != "All ages") %>%
  group_by(year, age_ranges) %>%
  dplyr::summarize(
    average_ls = weighted.mean(ls, WGT, na.rm = TRUE),
    var_ls = wtd.var(ls, weights = WGT, na.rm = TRUE),
    n = sum(WGT, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    se_ls = sqrt(var_ls / n),
    ci_lower = average_ls - qt(0.975, df = n - 1) * se_ls,
    ci_upper = average_ls + qt(0.975, df = n - 1) * se_ls
  )

# Create Combined Plot
combined_plot_data <- bind_rows(
  plot_data_cchs %>% mutate(source = "Life Satisfaction - CCHS"),
  plot_data %>% mutate(source = "Life Evaluation - Gallup")
)

min_y_combined <- floor(min(combined_plot_data$ci_lower, na.rm = TRUE) * 2) / 2
max_y_combined <- ceiling(max(combined_plot_data$ci_upper, na.rm = TRUE) * 2) / 2
y_breaks_combined <- seq(min_y_combined, max_y_combined, by = 0.5)
rect_data_combined <- data.frame(ymin = head(y_breaks_combined, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks_combined, -1)[c(TRUE, FALSE)])

combined_trajectory_ls <- ggplot() +
  geom_rect(data = rect_data_combined, 
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
            fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
  geom_vline(xintercept = c(2015, 2022), linetype = "dashed", color = "black") +
  # Gallup data
  geom_ribbon(data = combined_plot_data %>% filter(source == "Life Evaluation - Gallup"), 
              aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = age_ranges), alpha = 0.2) +
  geom_line(data = combined_plot_data %>% filter(source == "Life Evaluation - Gallup"), 
            aes(x = year, y = average_ls, color = age_ranges, linetype = source)) +
  # CCHS data
  geom_ribbon(data = combined_plot_data %>% filter(source == "Life Satisfaction - CCHS" & year >= 2009 & year <= 2015), 
              aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = age_ranges), alpha = 0.2) +
  geom_line(data = combined_plot_data %>% filter(source == "Life Satisfaction - CCHS" & year >= 2009 & year <= 2015), 
            aes(x = year, y = average_ls, color = age_ranges, linetype = source)) +
  geom_ribbon(data = combined_plot_data %>% filter(source == "Life Satisfaction - CCHS" & year >= 2015 & year <= 2022), 
              aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = age_ranges), alpha = 0.2) +
  geom_line(data = combined_plot_data %>% filter(source == "Life Satisfaction - CCHS" & year >= 2015 & year <= 2022), 
            aes(x = year, y = average_ls, color = age_ranges, linetype = source)) +
  
  geom_point(data = combined_plot_data, aes(x = year, y = average_ls, color = age_ranges, shape = source)) +
  labs(title = "Life Evaluations and Satisfaction in Canada",
       y = "Average Response",
       color = "Age Range",
       fill = "Age Range",
       linetype = "Data Source",
       shape = "Data Source",
       caption = "Data Source: Gallup World Poll and Canadian Community Health Survey") +
  theme_chr() +
  theme(aspect.ratio = 1/3, plot.background = element_rect(fill = background_colour, color = background_colour), axis.title.y = element_text(angle = 90), legend.key.width = unit(1, "cm"), plot.caption = element_text(hjust = 0, size = 8, face = "italic")) +
  scale_y_continuous(limits = c(min_y_combined, max_y_combined), breaks = y_breaks_combined) +
  scale_x_continuous(limits = c(floor(min(combined_plot_data$year, na.rm = TRUE)), ceiling(max(combined_plot_data$year, na.rm = TRUE))), breaks = seq(floor(min(combined_plot_data$year, na.rm = TRUE)), ceiling(max(combined_plot_data$year, na.rm = TRUE)), by = 1), labels = c(seq(floor(min(combined_plot_data$year, na.rm = TRUE)), ceiling(max(combined_plot_data$year, na.rm = TRUE))-1, by = 1), ""), 
                    expand = c(0, 0)) +
  scale_color_manual(values = four_tone_scale_colours) +
  scale_fill_manual(values = four_tone_scale_colours)
  
# Print the combined plot
print(combined_trajectory_ls)

base_filename <- "combined_trajectory_ls"
ggsave(paste0("Output/Final Plots/PNG/", base_filename, ".png"), plot = combined_trajectory_ls + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
ggsave(paste0("Output/Final Plots/JPG/", base_filename, ".jpg"), plot = combined_trajectory_ls, width = 9, height = 3, dpi = 300)
ggsave(paste0("Output/Final Plots/SVG/", base_filename, ".svg"), plot = combined_trajectory_ls + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")




###### AGE-STANDARDIZED PROVINCIAL MAP #######

library(sf)
library(canadianmaps)
library(dplyr)
library(ggplot2)

# Get the provincial boundary data
data("PROV")

years_to_plot <- c(2009, 2018, 2022)
province_map_data <- province %>%
  mutate(year = if_else(year==201718, 2018, year)) %>%
  filter(year %in% years_to_plot) %>%
  group_by(province, year) %>%
  dplyr::summarize(weighted_avg_ls = weighted.mean(ls, w = frequency, na.rm = TRUE))

# Handle the special case for territories in 2009
territories_2009 <- province_map_data %>%
  filter(year == 2009 & province == "Yukon/Northwest Territories/Nunavut") %>%
  mutate(province = list(c("Yukon", "Northwest Territories", "Nunavut"))) %>%
  unnest(province)

# Handle the special case for territories in 2022
territories_2022 <- tibble(
  province = c("Yukon", "Northwest Territories", "Nunavut"),
  year = 2022,
  weighted_avg_ls = NA_real_
)
# Combine the modified data
province_map_data <- province_map_data %>%
  filter(!(year == 2009 & province == "Yukon/Northwest Territories/Nunavut")) %>%
  bind_rows(territories_2009, territories_2022)

# Merge the weighted averages with the provincial boundary data
PROV <- PROV %>%
  left_join(province_map_data, by = c("PRENAME" = "province"))

# Plot the maps with each province colored by the weighted average of 'ls' for the specified years
ggplot(data = PROV) +
  theme_minimal() + 
  geom_sf(aes(fill = weighted_avg_ls), color = "black") +
  scale_fill_gradientn(colors = wellbeing_scale_colours, na.value = "grey50", limits = c(7.8, 8.3)) +
  labs(title = "Average Life Satisfaction by Province",
      fill = "Average Life Satisfaction",
      caption = "Data Source: Canadian Community Health Survey") +
  coord_sf(crs = st_crs(3347)) +  # Using the Lambert Conformal Conic projection
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        strip.text = element_text(size = 14),
        plot.caption = element_text(hjust = 0, size = 8, face = "italic")) + 
  facet_wrap(~ year, ncol = 1)
# Export the map to different formats
map_plot <- last_plot()
ggsave("Output/Final Plots/PNG/province_map.png", plot = map_plot, width = 10, height = 15, dpi = 300)
ggsave("Output/Final Plots/JPG/province_map.jpg", plot = map_plot, width = 10, height = 15, dpi = 300)
ggsave("Output/Final Plots/SVG/province_map.svg", plot = map_plot, width = 10, height = 15, dpi = 300)

# Duplicate territories in 2009, duplicate territories with NA in 2022, 
# Remove labels
# Remove y axis labels


######## DEMOGRAPHIC PLOTS #########


demographic_distribution_plot <- function(file_path, condition1, condition2, group1_label, group2_label, plot_title) {
  # Import Data
  data <- list.files(path = file_path, pattern = "*.csv", full.names = TRUE) %>%
    purrr::map_dfr(~ read.csv(.x) %>%
      mutate(year = as.numeric(sub(".*_([0-9]+)_CCHS\\.csv$", "20\\1", .x))))
  
  plot_data <- data %>%
    mutate(condition_met = case_when(
      eval(parse(text = condition1)) ~ 1,
      eval(parse(text = condition2)) ~ 0,
      TRUE ~ NA_real_
    )) %>%
    group_by(year, condition_met) %>%
    mutate(proportion = frequency / sum(frequency, na.rm = TRUE)) %>%
    mutate(average_ls = wtd.mean(as.numeric(ls), frequency, na.rm = TRUE))
  
  min_y <- 0
  max_y <- ceiling(max(plot_data$proportion, na.rm = TRUE) * 20) / 20
  y_breaks <- seq(min_y, max_y, by = 0.1)
  rect_data <- data.frame(ymin = head(y_breaks, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks, -1)[c(TRUE, FALSE)])

  # Create the bar chart with a completely custom legend
  demographic_plot <- ggplot() +
      geom_rect(data = rect_data, 
          aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
          fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
      geom_bar(data = plot_data %>% filter(year >= 2022), 
               stat = "identity", 
               width = 0.95, 
               aes(x = as.factor(ls), y = proportion, fill = as.factor(condition_met)), 
               position = position_dodge(width = 0.95), 
               show.legend = NA) +
      geom_vline(aes(xintercept = {plot_data %>% filter(year >= 2022 & condition_met == 1) %>% group_by(condition_met) %>% dplyr::summarize(average_ls = first(average_ls))}[[2]]), color = six_tone_scale_colours[1], linetype = "solid", linewidth = 0.5) +
      geom_vline(aes(xintercept = {plot_data %>% filter(year >= 2022 & condition_met == 0) %>% group_by(condition_met) %>% dplyr::summarize(average_ls = first(average_ls))}[[2]]), color = six_tone_scale_colours[3], linetype = "solid", linewidth = 0.5) +
      labs(title = plot_title,
          x = "Life Satisfaction Score",
          y = "Proportion") +
      theme_chr() +
      theme(aspect.ratio = 1, legend.position = "right") + 
      scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0), limits = c(min_y, max_y)) +
      scale_fill_manual(values = c(six_tone_scale_colours[1], six_tone_scale_colours[3]), guide = FALSE) +
      scale_color_identity(name = "Group", labels = c(group1_label, group2_label), 
                           guide = 'legend', breaks = c(six_tone_scale_colours[1], six_tone_scale_colours[3])) +
      geom_point(aes(x = 3, y = -5, color = six_tone_scale_colours[1]), size = 10, shape = 15, show.legend = TRUE) +
      geom_point(aes(x = 3, y = -5, color = six_tone_scale_colours[3]), size = 10, shape = 15, show.legend = TRUE)
  print(demographic_plot)

  # Save plot
  group1_label_sanitized <- tolower(gsub(" ", "_", group1_label))
  ggsave(paste0("Output/Final Plots/PNG/", group1_label_sanitized, "_distribution.png"), plot = demographic_plot + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 8, height = 6, dpi = 300, bg = "transparent")
  ggsave(paste0("Output/Final Plots/JPG/", group1_label_sanitized, "_distribution.svg"), plot = demographic_plot + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 8, height = 6, dpi = 300, bg = "transparent")
  ggsave(paste0("Output/Final Plots/SVG/", group1_label_sanitized, "_distribution.jpg"), plot = demographic_plot, width = 8, height = 6, dpi = 300)

  return(plot_data)
}


test <- demographic_distribution_plot("Data/Tables/INDIGENOUSxLS Tables CCHS", "indigenous == 'Yes'", "indigenous == 'No'", "Indigenous", "Non-indigenous", "Indigenous Life Satisfaction 2015-2018")

test <- demographic_distribution_plot("Data/Tables/IMMIGRATIONxLS Tables CCHS", "immigration == '0-9 years'", "immigration == 'Canadian born'", "New Immigrants", "Born in Canada", "Life Satisfaction of New Immigrants in Canada 2015-2018")

demographic_distribution_plot("Output/IMMIGRATIONxLS Tables CCHS", "time_in_canada == '10 or more years'", "In Canada 10 or more years", "Other Canadians", "Life Satisfaction of Established Immigrants in Canada 2015-2018")

demographic_distribution_plot("Output/MINORITYxLS Tables CCHS", "minority == 'Non-white'", "Visible Minority", "Other Canadians", "Life Satisfaction of Visible Minorities in Canada 2015-2018")

mh <- demographic_distribution_plot("Output/MENTALHEALTHxLS Tables CCHS", "mental_health <= 0", "Poor Mental Health", "Other Canadians", "Life Satisfaction of Canadians with Poor Mental Health 2015-2018")

demographic_distribution_plot("Output/LANGUAGExLS Tables CCHS", "language %in% c('French', 'English and French')", "French Speakers", "Other Canadians", "Life Satisfaction of Canadians who speak French at Home 2015-2018")

demographic_distribution_plot("Output/SEXxLS Tables CCHS", "sex == 'Female'", "Female", "Male", "Life Satisfaction of Canadians by Sex at Birth 2015-2018")

demographic_distribution_plot("Output/SEXUALORIENTATIONxLS Tables CCHS", "sex_diversity == 'Sexual minorities'", "Sexual Minorities", "Heterosexual Canadians", "Life Satisfaction of Sexual Minorities in Canada 2015-2018")

demographic_distribution_plot("Output/POVERTYxLS Tables CCHS", "poverty == TRUE", "Low Income (<$25,252)", "Other Canadians", "Life Satisfaction of Low Income Canadians 2015-2018")

demographic_distribution_plot("Output/HOMEOWNERxLS Tables CCHS", "home_owner == FALSE", "Non-Home Owners", "Home Owner", "Life Satisfaction of Non-Home Owning Canadians 2015-2018")

demographic_distribution_plot("Output/SCREENTIMEWExLS Tables CCHS", "screentime_weekend %in% c('6 hours to less than 8 hours', '8 hours or more per day')", "6 hours or more", "Less than 6 hours", "Life Satisfaction of Canadians by Screentime on Weekends")

demographic_distribution_plot("Output/SCREENTIMEWDxLS Tables CCHS", "screentime_weekday %in% c('6 hours to less than 8 hours', '8 hours or more per day')", "6 hours or more", "Less than 6 hours", "Life Satisfaction of Canadians by Screentime on Weekdays")

demographic_distribution_plot("Output/YOUTH-SCREENTIMEWDxLS Tables CCHS", "screentime_weekday %in% c('6 hours to less than 8 hours', '8 hours or more per day')", "6 hours or more", "Less than 6 hours", "Life Satisfaction of 15-29 Year Old Canadians by Screentime on Weekdays")


demographic_trajectory_plot(
  condition = "home_owner == FALSE",
  true_label = "Non-Home Owners",
  false_label = "Home Owners",
  colour = 1,
  override_filename = FALSE,
  override_title = "Life Satisfaction of Homeowners vs Non-Homeowners, 2020-2022",
  file_path = "Data/Tables/HOMEOWNERxLS Tables CCHS"
)


###### Demographic Trajectory Plots ######

demographic_trajectory_plot <- function(condition, true_label = " ", false_label = " ", colour = 1, override_filename = FALSE, override_title = NULL, file_path) {
  data_source_caption <- "Data Source: Canadian Community Health Survey"
  
  data <- list.files(path = file_path, pattern = "*.csv", full.names = TRUE) %>%
    purrr::map_dfr(~ read.csv(.x) %>%
      mutate(year = as.numeric(paste0("20", sub(paste0(file_path, "/.*_(.*)_CCHS.csv"), "\\1", .x))))) %>%
    mutate(year = case_when(as.numeric(year) <= 2023 ~ as.numeric(year) + 0.5,
                            year == 201516 ~ 2016,
                            year == 201718 ~ 2018,
                            year == 201920 ~ 2020),
           ls = as.numeric(ls)) %>%
    group_by(year) %>%
    mutate(WGT = frequency / sum(frequency) * 65000) %>%
    ungroup()
  
  plot_data <- data %>%
    mutate(binary_var = if_else(eval(parse(text = condition)), true_label, false_label)) %>%
    group_by(year, binary_var) %>%
    dplyr::summarize(
      average_ls = weighted.mean(ls, WGT, na.rm = TRUE),
      var_ls = wtd.var(ls, weights = WGT, na.rm = TRUE),
      n = sum(WGT, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      se_ls = sqrt(var_ls / n),
      ci_lower = average_ls - qt(0.975, df = n - 1) * se_ls,
      ci_upper = average_ls + qt(0.975, df = n - 1) * se_ls
    ) %>%
    drop_na(binary_var)
  
  # Automatically determine min_y and max_y based on the confidence intervals
  min_y <- floor(min(plot_data$ci_lower, na.rm = TRUE) * 2) / 2
  max_y <- ceiling(max(plot_data$ci_upper, na.rm = TRUE) * 2) / 2
  y_breaks <- seq(min_y, max_y, by = 0.5)
  rect_data <- data.frame(ymin = head(y_breaks, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks, -1)[c(TRUE, FALSE)])
  
  # Create the plot
  age_group_ls_plot <- ggplot() +
    geom_rect(data = rect_data, 
              aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
              fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
    geom_ribbon(data = plot_data, aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = binary_var), alpha = 0.2) +
    geom_line(data = plot_data, aes(x = year, y = average_ls, group = binary_var, color = binary_var)) +
    geom_point(data = plot_data, aes(x = year, y = average_ls, group = binary_var, color = binary_var))
  
  plot_title <- if (!is.null(override_title)) override_title else paste("Life Satisfaction for", tools::toTitleCase(true_label))
  
  age_group_ls_plot <- age_group_ls_plot +
    labs(title = plot_title,
         y = "Average Life Evaluation",
         color = "Group",
         caption = data_source_caption) +
    theme_chr() +
    theme(aspect.ratio = 1/3, plot.background = element_rect(fill = background_colour, color = background_colour), axis.title.y = element_text(angle = 90), legend.key.width = unit(1, "cm"), plot.caption = element_text(hjust = 0, size = 8, face = "italic")) +  # Adjusted legend key width
    scale_y_continuous(limits = c(min_y, max_y), breaks = y_breaks) +
    scale_x_continuous(limits = c(floor(min(plot_data$year, na.rm = TRUE)), ceiling(max(plot_data$year, na.rm = TRUE))), breaks = seq(floor(min(plot_data$year, na.rm = TRUE)), ceiling(max(plot_data$year, na.rm = TRUE)), by = 1), labels = c(seq(floor(min(plot_data$year, na.rm = TRUE)), ceiling(max(plot_data$year, na.rm = TRUE)) - 1, by = 1), ""), 
                        expand = c(0, 0)) +
    scale_color_manual(values = setNames(c(six_tone_scale_colours[1], six_tone_scale_colours[3]), c(true_label, false_label)))
  
  # Print the plot
  print(age_group_ls_plot)
  
  # Save the plot in different formats
  true_label_formatted <- tolower(gsub("[[:punct:]]", "", gsub(" ", "_", true_label)))
  base_filename <- if (is.character(override_filename)) override_filename else paste0("trajectory_by_", true_label_formatted, "_Canada")
  
  ggsave(paste0("Output/Plots/Trajectory/PNG/", base_filename, ".png"), plot = age_group_ls_plot + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
  ggsave(paste0("Output/Plots/Trajectory/JPG/", base_filename, ".jpg"), plot = age_group_ls_plot, width = 9, height = 3, dpi = 300)
  ggsave(paste0("Output/Plots/Trajectory/SVG/", base_filename, ".svg"), plot = age_group_ls_plot + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
  
  return(plot_data)
}








############# Immigration Plots ##################

  file_path <- "Data/Tables/IMMIGRATIONxLS Tables CCHS"
  plot_title <- "Life Satisfaction of Immigrants in Canada, 2021-2022"

  data <- list.files(path = file_path, pattern = "*.csv", full.names = TRUE) %>%
    purrr::map_dfr(~ read.csv(.x) %>%
      mutate(year = as.numeric(sub(".*_([0-9]+)_CCHS\\.csv$", "20\\1", .x))))
  
  plot_data <- data %>%
    mutate(condition_met = case_when(
      immigration == "0-9 years" ~ "0-9 years",
      immigration == "10 or more years" ~ "10 or more years",
      immigration == "Canadian born" ~ "Canadian born",
      TRUE ~ NA_character_
    )) %>%
    group_by(year, condition_met) %>%
    mutate(proportion = frequency / sum(frequency, na.rm = TRUE)) %>%
    mutate(average_ls = wtd.mean(as.numeric(ls), frequency, na.rm = TRUE)) %>%
    mutate(respondents = (frequency / 32535400) * 35000) %>%
    group_by(year, condition_met) %>%
    mutate(total_respondents = sum(respondents))
  
  min_y <- 0
  max_y <- 0.35
  y_breaks <- seq(min_y, max_y, by = 0.1)
  rect_data <- data.frame(ymin = head(y_breaks, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks, -1)[c(TRUE, FALSE)])

  # Create the bar chart with a completely custom legend
  demographic_plot <- ggplot() +
      geom_rect(data = rect_data, 
          aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
          fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
      geom_bar(data = plot_data %>% filter(year >= 2021), 
               stat = "identity", 
               width = 0.95, 
               aes(x = as.factor(ls), y = proportion, fill = as.factor(condition_met)), 
               position = position_dodge(width = 0.95), 
               show.legend = NA) +
      # geom_vline(aes(xintercept = {plot_data %>% filter(year >= 2021 & condition_met == "0-9 years") %>% 
      #                             group_by(condition_met) %>% dplyr::summarize(average_ls = first(average_ls))}[[2]]), 
      #            color = six_tone_scale_colours[1], linetype = "solid", linewidth = 0.5) +
      # geom_vline(aes(xintercept = {plot_data %>% filter(year >= 2021 & condition_met == "10 or more years") %>% 
      #                             group_by(condition_met) %>% dplyr::summarize(average_ls = first(average_ls))}[[2]]), 
      #            color = six_tone_scale_colours[2], linetype = "solid", linewidth = 0.5) +
      # geom_vline(aes(xintercept = {plot_data %>% filter(year >= 2021 & condition_met == "Canadian born") %>% 
      #                             group_by(condition_met) %>% dplyr::summarize(average_ls = first(average_ls))}[[2]]), 
      #            color = six_tone_scale_colours[5], linetype = "solid", linewidth = 0.5) +
geom_segment(data = plot_data %>% filter(year >= 2021 & condition_met == "0-9 years") %>% 
               group_by(condition_met) %>% dplyr::summarize(average_ls = first(average_ls)),
             aes(x = average_ls, xend = average_ls, y = 0, yend = 0.33), 
             color = six_tone_scale_colours[1], linetype = "solid", linewidth = 0.5) +
geom_segment(data = plot_data %>% filter(year >= 2021 & condition_met == "10 or more years") %>% 
               group_by(condition_met) %>% dplyr::summarize(average_ls = first(average_ls)),
             aes(x = average_ls, xend = average_ls, y = 0, yend = 0.33), 
             color = six_tone_scale_colours[2], linetype = "solid", linewidth = 0.5) +
geom_segment(data = plot_data %>% filter(year >= 2021 & condition_met == "Canadian born") %>% 
               group_by(condition_met) %>% dplyr::summarize(average_ls = first(average_ls)),
             aes(x = average_ls, xend = average_ls, y = 0, yend = 0.33), 
             color = six_tone_scale_colours[5], linetype = "solid", linewidth = 0.5) +
      labs(title = plot_title,
          x = "Life Satisfaction Score",
          y = "Proportion",
          group = "Time in Canada") +
      theme_chr() +
      theme(aspect.ratio = 1, legend.position = "right") + 
      scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0), limits = c(min_y, max_y)) +
      scale_fill_manual(values = c(six_tone_scale_colours[1], six_tone_scale_colours[2], six_tone_scale_colours[5]), guide = FALSE) +
      scale_color_identity(name = "Time in Canada", labels = c("0-9 years", "10 or more years", "Canadian born"), 
                           guide = 'legend', breaks = c(six_tone_scale_colours[1], six_tone_scale_colours[2], six_tone_scale_colours[5])) +
      geom_point(aes(x = 3, y = -5, color = six_tone_scale_colours[1]), size = 10, shape = 15, show.legend = TRUE) +
      geom_point(aes(x = 3, y = -5, color = six_tone_scale_colours[2]), size = 10, shape = 15, show.legend = TRUE) +
      geom_point(aes(x = 3, y = -5, color = six_tone_scale_colours[5]), size = 10, shape = 15, show.legend = TRUE)
  print(demographic_plot)

  # Save plot
  group1_label_sanitized <- tolower(gsub(" ", "_", group1_label))
  ggsave(paste0("Output/Final Plots/PNG/immigration_distribution.png"), plot = demographic_plot + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 8, height = 6, dpi = 300, bg = "transparent")
  ggsave(paste0("Output/Final Plots/JPG/immigration_distribution.svg"), plot = demographic_plot + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 8, height = 6, dpi = 300, bg = "transparent")
  ggsave(paste0("Output/Final Plots/SVG/immigration_distribution.jpg"), plot = demographic_plot, width = 8, height = 6, dpi = 300)

  # Calculate the percentage of respondents in >= 2021 that fall into each of the three categories
  percentage_data <- plot_data %>%
    filter(year >= 2021) %>%
    group_by(condition_met) %>%
    dplyr::summarize(total_count = sum(frequency, na.rm = TRUE)) %>%
    mutate(percentage = total_count / sum(total_count) * 100)
  
  # Print the percentage data
  print(percentage_data)


  return(plot_data)



###########################################################################################################
############################################    APPENDIX    ###############################################
###########################################################################################################


###### Age-Standardized Provincial Map #######

data("PROV")

# Assuming 'provinces' is a dataframe with columns 'province' and 'ls'
# Calculate the weighted average of 'ls' for each province for the years 2009, 201718, and 2022
years_to_plot <- c(2009, 2018, 2022)

# Calculate the overall age makeup of Canada in 2022
overall_age_makeup <- province %>%
  filter(year == 2022 & age_ranges %in% c("15-29", "30-44", "45-59", "60+")) %>%
  group_by(age_ranges) %>%
  dplyr::summarize(total_population = sum(frequency, na.rm = TRUE)) %>%
  mutate(overall_weight = total_population / sum(total_population))

# Calculate the weighted average of ls in each age range in each year and province
province_age_group_data <- province %>%
  mutate(year = if_else(year==201718, 2018, year)) %>%
  filter(year %in% years_to_plot & age_ranges %in% c("15-29", "30-44", "45-59", "60+")) %>%
  group_by(province, year, age_ranges) %>%
  dplyr::summarize(weighted_avg_ls_age_group = weighted.mean(ls, w = frequency, na.rm = TRUE)) %>%
  drop_na(age_ranges)

# Calculate the age-standardized average of ls for each province and year
province_map_data <- province_age_group_data %>%
  left_join(overall_age_makeup, by = "age_ranges") %>%
  group_by(province, year) %>%
  dplyr::summarize(weighted_avg_ls = weighted.mean(weighted_avg_ls_age_group, w = overall_weight, na.rm = TRUE)) 

# Handle the special case for territories in 2009
territories_2009 <- province_map_data %>%
  filter(year == 2009 & province == "Yukon/Northwest Territories/Nunavut") %>%
  mutate(province = list(c("Yukon", "Northwest Territories", "Nunavut"))) %>%
  unnest(province)

# Handle the special case for territories in 2022
territories_2022 <- tibble(
  province = c("Yukon", "Northwest Territories", "Nunavut"),
  year = 2022,
  weighted_avg_ls = NA_real_
)

# Combine the modified data
province_map_data <- province_map_data %>%
  filter(!(year == 2009 & province == "Yukon/Northwest Territories/Nunavut")) %>%
  bind_rows(territories_2009, territories_2022)

# Merge the weighted averages with the provincial boundary data
PROV <- PROV %>%
  left_join(province_map_data, by = c("PRENAME" = "province"))

# Plot the maps with each province colored by the weighted average of 'ls' for the specified years
ggplot(data = PROV) +
  theme_minimal() + 
  geom_sf(aes(fill = weighted_avg_ls), color = "black") +
  scale_fill_gradientn(colors = wellbeing_scale_colours, na.value = "grey50", limits = c(7.8, 8.2)) +
  labs(title = "Age-Standardized Average Life Satisfaction by Province",
      fill = "Average Life Satisfaction",
      caption = "Data Source: Canadian Community Health Survey") +
  coord_sf(crs = st_crs(3347)) +  # Using the Lambert Conformal Conic projection
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        strip.text = element_text(size = 14),
        plot.caption = element_text(hjust = 0, size = 8, face = "italic")) + 
  facet_wrap(~ year, ncol = 1)
# Export the map to different formats
map_plot <- last_plot()
ggsave("Output/Final Plots/Appendix/PNG/age_standardized_province_map.png", plot = map_plot, width = 10, height = 15, dpi = 300)
ggsave("Output/Final Plots/Appendix/JPG/age_standardized_province_map.jpg", plot = map_plot, width = 10, height = 15, dpi = 300)
ggsave("Output/Final Plots/Appendix/SVG/age_standardized_province_map.svg", plot = map_plot, width = 10, height = 15, dpi = 300)
