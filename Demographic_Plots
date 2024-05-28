# Demographic Visualizations (CCHS) for CHR 2024
# Author: Anthony McCanny
# Date: May 6, 2024
###############################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)



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


########### Indigenous Identity Plots ############

## Import Data

indigenous <- list.files(path = "Output/INDIGENOUSxLS Tables CCHS", pattern = "*.csv", full.names = TRUE) %>%
  purrr::map_dfr(~ read.csv(.x) %>%
    mutate(year = as.numeric(paste0("20", sub("Output/INDIGENOUSxLS Tables CCHS/INDIGENOUSxLS_(.*)_CCHS.csv", "\\1", .x)))))

## Process Data
indigenous_plot_data <- indigenous %>%
    mutate(display_year = case_when(as.numeric(year) <= 2023 ~ as.numeric(year),
                                    year == 201516 ~ 2016,
                                    year == 201718 ~ 2018,
                                    year == 201920 ~ 2020)) %>%
    group_by(year, indigenous) %>%
    mutate(proportion = frequency / sum(frequency)) %>%
    ungroup() %>%
    # Calculate the weighted average life satisfaction for each year
    group_by(year, indigenous) %>%
    mutate(average_ls = sum(proportion * as.numeric(ls)))

min_y <- 0
max_y <- ceiling(max(indigenous_plot_data$proportion*2, na.rm = TRUE) * 20) / 20
y_breaks <- seq(min_y, max_y, by = 0.2)
rect_data <- data.frame(ymin = head(y_breaks, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks, -1)[c(TRUE, FALSE)])

# Create the bar chart with a completely custom legend
indigenous_plot <- ggplot() +
    geom_rect(data = rect_data, 
        aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
        fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
    geom_bar(data = indigenous_plot_data %>% filter(year >= 201516 & indigenous == "Yes"), stat = "identity", width = 1, aes(x = as.factor(ls), y = proportion, fill = as.factor(ls)), alpha = 1, show.legend = NA) +
    geom_bar(data = indigenous_plot_data %>% filter(year >= 201516 & indigenous == "No"), stat = "identity", width = 1, aes(x = as.factor(ls), y = proportion), fill = "#006ba2", alpha = 0.2, show.legend = NA) +
    geom_vline(aes(xintercept = {indigenous_plot_data %>% filter(year >= 201516 & indigenous == "Yes") %>% group_by(indigenous) %>% summarize(average_ls = first(average_ls))}[[2]]), color = "#d46171", linetype = "solid", linewidth = 0.5) +
    geom_vline(aes(xintercept = {indigenous_plot_data %>% filter(year >= 201516 & indigenous == "No") %>% group_by(indigenous) %>% summarize(average_ls = first(average_ls))}[[2]]), color = "#006ba2", linetype = "solid", linewidth = 0.5) +
    labs(title = "First Nation, Metis and Inuit Life Satisfaction",
        x = "Life Satisfaction Score",
        y = "Proportion") +
    theme_chr() +
    theme(aspect.ratio = 1, legend.position = "right") + 
    scale_y_continuous(labels = scales::percent_format(scale = 50), expand = c(0, 0), limits = c(min_y, max_y)) +
    scale_fill_manual(values = wellbeing_scale_colours, guide = FALSE) +
    scale_color_identity(name = "Group", labels = c("First Nations, Metis and Inuit", "Other Canadians"), 
                         guide = 'legend', breaks = c("#d46171", "#006ba2")) +
    geom_point(aes(x = 3, y = -5, color = "#d46171"), size = 10, shape = 15, show.legend = TRUE) +
    geom_point(aes(x = 3, y = -5, color = "#006ba2"), size = 10, shape = 15, show.legend = TRUE)
print(indigenous_plot)

ggsave("Output/Plots/Demographics/indigenous_distribution_2015_2018.png", plot = indigenous_plot + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 4, height = 4, dpi = 300, bg = "transparent")
ggsave("Output/Plots/Demographics/indigenous_distribution_2015_2018.svg", plot = indigenous_plot + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 4, height = 4, dpi = 300, bg = "transparent")
ggsave("Output/Plots/Demographics/indigenous_distribution_2015_2018.jpg", plot = indigenous_plot, width = 4, height = 4, dpi = 300)


create_demographic_plot <- function(file_path, condition, group1_label, group2_label, plot_title) {
  # Import Data
  data <- list.files(path = file_path, pattern = "*.csv", full.names = TRUE) %>%
    purrr::map_dfr(~ read.csv(.x) %>%
      mutate(year = as.numeric(sub(".*_([0-9]+)_CCHS\\.csv$", "20\\1", .x))))
  plot_data <- data %>%
    mutate(display_year = case_when(as.numeric(year) <= 2023 ~ as.numeric(year),
                                    year == 201516 ~ 2016,
                                    year == 201718 ~ 2018,
                                    year == 201920 ~ 2020),
           condition_met = ifelse(eval(parse(text = condition)), 1, 0)) %>%
    group_by(year, condition_met) %>%
    mutate(proportion = frequency / sum(frequency)) %>%
    ungroup() %>%
    # Calculate the weighted average life satisfaction for each year
    group_by(year, condition_met) %>%
    mutate(average_ls = sum(proportion * as.numeric(ls)))
  min_y <- 0
  max_y <- ceiling(max(plot_data$proportion*2, na.rm = TRUE) * 20) / 20
  y_breaks <- seq(min_y, max_y, by = 0.2)
  rect_data <- data.frame(ymin = head(y_breaks, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks, -1)[c(TRUE, FALSE)])

  # Create the bar chart with a completely custom legend
  demographic_plot <- ggplot() +
      geom_rect(data = rect_data, 
          aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
          fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
      geom_bar(data = plot_data %>% filter(year >= 201516 & condition_met), stat = "identity", width = 1, aes(x = as.factor(ls), y = proportion, fill = as.factor(ls)), alpha = 1, show.legend = NA) +
      geom_bar(data = plot_data %>% filter(year >= 201516 & !condition_met), stat = "identity", width = 1, aes(x = as.factor(ls), y = proportion), fill = "#006ba2", alpha = 0.2, show.legend = NA) +
      geom_vline(aes(xintercept = {plot_data %>% filter(year >= 201516 & condition_met) %>% group_by(condition_met) %>% summarize(average_ls = first(average_ls))}[[2]]), color = "#d46171", linetype = "solid", linewidth = 0.5) +
      geom_vline(aes(xintercept = {plot_data %>% filter(year >= 201516 & !condition_met) %>% group_by(condition_met) %>% summarize(average_ls = first(average_ls))}[[2]]), color = "#006ba2", linetype = "solid", linewidth = 0.5) +
      labs(title = plot_title,
          x = "Life Satisfaction Score",
          y = "Proportion") +
      theme_chr() +
      theme(aspect.ratio = 1, legend.position = "right") + 
      scale_y_continuous(labels = scales::percent_format(scale = 50), expand = c(0, 0), limits = c(min_y, max_y)) +
      scale_fill_manual(values = wellbeing_scale_colours, guide = FALSE) +
      scale_color_identity(name = "Group", labels = c(group1_label, group2_label), 
                           guide = 'legend', breaks = c("#d46171", "#006ba2")) +
      geom_point(aes(x = 3, y = -5, color = "#d46171"), size = 10, shape = 15, show.legend = TRUE) +
      geom_point(aes(x = 3, y = -5, color = "#006ba2"), size = 10, shape = 15, show.legend = TRUE)
print(demographic_plot)

# Save plot
group1_label_sanitized <- tolower(gsub(" ", "_", group1_label))
ggsave(paste0("Output/Plots/Demographics/", group1_label_sanitized, "_distribution.png"), plot = demographic_plot + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 8, height = 6, dpi = 300, bg = "transparent")
ggsave(paste0("Output/Plots/Demographics/", group1_label_sanitized, "_distribution.svg"), plot = demographic_plot + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 8, height = 6, dpi = 300, bg = "transparent")
ggsave(paste0("Output/Plots/Demographics/", group1_label_sanitized, "_distribution.jpg"), plot = demographic_plot, width = 8, height = 6, dpi = 300)

return(plot_data)
}


create_demographic_plot("Output/INDIGENOUSxLS Tables CCHS", "indigenous == 'Yes'", "Indigenous", "Non-indigenous", "Indigenous Life Satisfaction 2015-2018")

create_demographic_plot("Output/IMMIGRATIONxLS Tables CCHS", "time_in_canada == '0-9 years'", "New Immigrants", "Other Canadians", "Life Satisfaction of New Immigrants in Canada 2015-2018")

create_demographic_plot("Output/IMMIGRATIONxLS Tables CCHS", "time_in_canada == '10 or more years'", "In Canada 10 or more years", "Other Canadians", "Life Satisfaction of Established Immigrants in Canada 2015-2018")

create_demographic_plot("Output/MINORITYxLS Tables CCHS", "minority == 'Non-white'", "Visible Minority", "Other Canadians", "Life Satisfaction of Visible Minorities in Canada 2015-2018")

mh <- create_demographic_plot("Output/MENTALHEALTHxLS Tables CCHS", "mental_health <= 0", "Poor Mental Health", "Other Canadians", "Life Satisfaction of Canadians with Poor Mental Health 2015-2018")

create_demographic_plot("Output/LANGUAGExLS Tables CCHS", "language %in% c('French', 'English and French')", "French Speakers", "Other Canadians", "Life Satisfaction of Canadians who speak French at Home 2015-2018")

create_demographic_plot("Output/SEXxLS Tables CCHS", "sex == 'Female'", "Female", "Male", "Life Satisfaction of Canadians by Sex at Birth 2015-2018")

create_demographic_plot("Output/SEXUALORIENTATIONxLS Tables CCHS", "sex_diversity == 'Sexual minorities'", "Sexual Minorities", "Heterosexual Canadians", "Life Satisfaction of Sexual Minorities in Canada 2015-2018")

create_demographic_plot("Output/POVERTYxLS Tables CCHS", "poverty == TRUE", "Low Income (<$25,252)", "Other Canadians", "Life Satisfaction of Low Income Canadians 2015-2018")

create_demographic_plot("Output/HOMEOWNERxLS Tables CCHS", "home_owner == FALSE", "Non-Home Owners", "Home Owner", "Life Satisfaction of Non-Home Owning Canadians 2015-2018")

create_demographic_plot("Output/SCREENTIMEWExLS Tables CCHS", "screentime_weekend %in% c('6 hours to less than 8 hours', '8 hours or more per day')", "6 hours or more", "Less than 6 hours", "Life Satisfaction of Canadians by Screentime on Weekends")

create_demographic_plot("Output/SCREENTIMEWDxLS Tables CCHS", "screentime_weekday %in% c('6 hours to less than 8 hours', '8 hours or more per day')", "6 hours or more", "Less than 6 hours", "Life Satisfaction of Canadians by Screentime on Weekdays")

create_demographic_plot("Output/YOUTH-SCREENTIMEWDxLS Tables CCHS", "screentime_weekday %in% c('6 hours to less than 8 hours', '8 hours or more per day')", "6 hours or more", "Less than 6 hours", "Life Satisfaction of 15-29 Year Old Canadians by Screentime on Weekdays")



file_path = "Output/INDIGENOUSxLS Tables CCHS"; condition =  "indigenous == 'Yes"; group1_label = "Indigenous"; group2_label =  "Non-indigenous"; plot_title = "Indigenous Life Satisfaction 2015-2018"



mental_health_data <- list.files(path = "Output/AGExMENTALHEALTH Tables CCHS", pattern = "*.csv", full.names = TRUE) %>%
  purrr::map_dfr(~ read.csv(.x) %>%
    mutate(year = as.numeric(paste0("20", sub("Output/AGExMENTALHEALTH Tables CCHS/AGExMENTALHEALTH_(.*)_CCHS.csv", "\\1", .x)))))

## Process Data
mental_health_plot_data <- mental_health_data %>%
    mutate(display_year = case_when(as.numeric(year) <= 2023 ~ as.numeric(year),
                                    year == 201516 ~ 2016,
                                    year == 201718 ~ 2018,
                                    year == 201920 ~ 2020)) %>%
    group_by(year, age_ranges) %>%
    mutate(proportion = frequency / sum(frequency)) %>%
    ungroup() %>%
    # Calculate the weighted average life satisfaction for each age range and year
    group_by(year, age_ranges) %>%
    mutate(average_mh = sum(proportion * as.numeric(mental_health))) %>%
    drop_na(age_ranges)

min_y <- 2.75
max_y <- 3.25
y_breaks <- seq(min_y, max_y, by = 0.25)
rect_data <- data.frame(ymin = head(y_breaks, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks, -1)[c(TRUE, FALSE)])

mental_health_age_plot <- ggplot(mental_health_plot_data, aes(x = display_year, y = average_mh, group = age_ranges, color = age_ranges)) +
  geom_rect(data = rect_data, 
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
            fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2020.25, color = "#b41f1f", linetype = "solid", size = 0.5) +
  geom_text(aes(x = 2020.25, y = 7.4, label = "Pandemic Begins"), color = "#b41f1f", vjust = -0.5, hjust = 0.5, angle = 90, size = 2.5) +
  labs(title = "Average Mental Health (on a 0-4 scale) across Age Groups in Canada (CCHS)",
       y = "Average Self-Reported Mental Health",
       color = "Age Ranges") +
  theme_chr() + 
  theme(aspect.ratio = 1/3, axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0), hjust = 0.5)) +
  scale_y_continuous(limits = c(min_y, max_y), breaks = y_breaks) +
  scale_x_continuous(breaks = seq(min(mental_health_plot_data$display_year, na.rm = TRUE), max(mental_health_plot_data$display_year, na.rm = TRUE), by = 1)) +
  scale_color_manual(values = six_tone_scale_colours)

print(mental_health_age_plot)

ggsave("Output/Plots/trajectory_by_mental_health_age_ranges_Canada_CCHS.png", plot = mental_health_age_plot + theme_void() + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
ggsave("Output/Plots/trajectory_by_mental_health_age_ranges_Canada_CCHS.svg", plot = mental_health_age_plot + theme_void() + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
ggsave("Output/Plots/trajectory_by_mental_health_age_ranges_Canada_CCHS.jpg", plot = mental_health_age_plot, width = 9, height = 3, dpi = 300)

## Calculate statistics on mental_health decline
mental_health_data %>% filter(age_ranges == "15-29") %>% group_by(year) %>% summarize(low_mh = sum(frequency[mental_health %in% 0:1]) / sum(frequency))
filter(year %in% c(2013, 201718), age_ranges == "15-29", mental_health %in% 0:3) %>% group_by(year) %>% summarize(proportion = sum(frequency))

generate_trajectory_plot_non_ls <- function(file_path, variable_name, y_axis_name, plot_title, save_tag = NULL) {
  # Load the data
  data <- list.files(path = file_path, pattern = "*.csv", full.names = TRUE) %>%
    purrr::map_dfr(~ read.csv(.x) %>%
      mutate(year = as.numeric(paste0("20", sub(paste0(file_path, "/.*_(.*)_CCHS.csv"), "\\1", .x)))))

  # Process Data
  plot_data <- data %>%
    mutate(display_year = case_when(as.numeric(year) <= 2023 ~ as.numeric(year),
                                    year == 201516 ~ 2016,
                                    year == 201718 ~ 2018,
                                    year == 201920 ~ 2020)) %>%
    group_by(year, age_ranges) %>%
    mutate(proportion = frequency / sum(frequency)) %>%
    ungroup() %>%
    # Calculate the weighted average for the specified variable for each age range and year
    group_by(year, age_ranges) %>%
    mutate(average_value = sum(proportion * as.numeric(get(variable_name)))) %>%
    drop_na(age_ranges)

  # Define plot parameters
  min_y <- floor(min(plot_data$average_value, na.rm = TRUE) * 2) / 2
  max_y <- ceiling(max(plot_data$average_value, na.rm = TRUE) * 2) / 2
  y_breaks <- seq(min_y, max_y, by = 0.25)
  rect_data <- data.frame(ymin = head(y_breaks, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks, -1)[c(TRUE, FALSE)])

  # Create the plot
  plot <- ggplot(plot_data, aes(x = display_year, y = average_value, group = age_ranges, color = age_ranges)) +
    geom_rect(data = rect_data, 
              aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
              fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = 2020.25, color = "#b41f1f", linetype = "solid", size = 0.5) +
    geom_text(aes(x = 2020.25, y = max_y, label = "Pandemic Begins"), color = "#b41f1f", vjust = -0.5, hjust = 0.5, angle = 90, size = 2.5) +
    labs(title = plot_title,
         y = y_axis_name,
         color = "Age Ranges") +
    theme_chr() + 
    theme(aspect.ratio = 1/3, axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0), hjust = 0.5)) +
    scale_y_continuous(limits = c(min_y, max_y), breaks = y_breaks) +
    scale_x_continuous(breaks = seq(min(plot_data$display_year, na.rm = TRUE), max(plot_data$display_year, na.rm = TRUE), by = 1)) +
    scale_color_manual(values = six_tone_scale_colours)

  # Print the plot
  print(plot)

  # Define the save tag if provided
  save_tag <- ifelse(is.null(save_tag), "", paste0("_", save_tag))

  # Save the plot in different formats
  ggsave(paste0("Output/Plots/trajectory_by_", variable_name, "_age_ranges_Canada_CCHS", save_tag, ".png"), plot = plot + theme_void() + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
  ggsave(paste0("Output/Plots/trajectory_by_", variable_name, "_age_ranges_Canada_CCHS", save_tag, ".svg"), plot = plot + theme_void() + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
  ggsave(paste0("Output/Plots/trajectory_by_", variable_name, "_age_ranges_Canada_CCHS", save_tag, ".jpg"), plot = plot, width = 9, height = 3, dpi = 300)
}

generate_trajectory_plot_non_ls("Output/JOBSATxLS Tables CCHS", "job_sat", "Job Satisfaction", "Job Satisfaction Over Time")
generate_trajectory_plot_non_ls("Output/LEISURESATxLS Tables CCHS", "leisure_sat", "Leisure Satisfaction", "Leisure Satisfaction Over Time")
generate_trajectory_plot_non_ls("Output/FINANCESATxLS Tables CCHS", "finance_sat", "Finance Satisfaction", "Finance Satisfaction Over Time")
generate_trajectory_plot_non_ls("Output/YOUSATxLS Tables CCHS", "you_sat", "Self Satisfaction", "Self Satisfaction Over Time")
generate_trajectory_plot_non_ls("Output/BODYSATxLS Tables CCHS", "body_sat", "Body Satisfaction", "Body Satisfaction Over Time")
generate_trajectory_plot_non_ls("Output/FAMILYSATxLS Tables CCHS", "family_sat", "Family Satisfaction", "Family Satisfaction Over Time")
generate_trajectory_plot_non_ls("Output/FRIENDSSATxLS Tables CCHS", "friends_sat", "Friends Satisfaction", "Friends Satisfaction Over Time")
generate_trajectory_plot_non_ls("Output/HOUSINGSATxLS Tables CCHS", "housing_sat", "Housing Satisfaction", "Housing Satisfaction Over Time")
generate_trajectory_plot_non_ls("Output/NEIGHBOURHOODSATxLS Tables CCHS", "neighbourhood_sat", "Neighbourhood Satisfaction", "Neighbourhood Satisfaction Over Time")
generate_trajectory_plot_non_ls("Output/PHQ9xLS Tables CCHS", "phq9", "PHQ-9 Score", "PHQ-9 Scores Over Time")
generate_trajectory_plot_non_ls("Output/AGExSCREENTIME Tables CCHS", "screentime_num", "Screentime", "Screentime Over Time")
generate_trajectory_plot_non_ls("Output/AGExSCREENTIMEWEEKEND Tables CCHS", "screentime_we", "Screentime", "Weekend Screentime Over Time")
generate_trajectory_plot_non_ls("Output/AGExSCREENTIMEWEEKDAY Tables CCHS", "screentime_wd", "Screentime", "Weekday  Screentime Over Time")
generate_trajectory_plot_non_ls("Output/QUEBEC-AGExMENTALHEALTH Tables CCHS", "mental_health", "Average Self-Reported Mental Health", "Average Mental Health (on a 0-4 scale) across Age Groups in Quebec (CCHS)", save_tag="QC")
generate_trajectory_plot_non_ls("Output/AGExMENTALHEALTH Tables CCHS", "mental_health", "Average Self-Reported Mental Health", "Average Mental Health (on a 0-4 scale) across Age Groups in Canada (CCHS)")
generate_trajectory_plot_non_ls("Output/FRENCH-AGExMENTALHEALTH Tables CCHS", "mental_health", "Average Self-Reported Mental Health", "Average Mental Health (on a 0-4 scale) across Age Groups for French Speakers (CCHS)", save_tag="FR")
generate_trajectory_plot_non_ls("Output/ENGLISH-QUEBEC-AGExMENTALHEALTH Tables CCHS", "mental_health", "Average Self-Reported Mental Health", "Average Mental Health (on a 0-4 scale) across Age Groups for English Speaking Quebec Residents (CCHS)", save_tag="EN_QC")
generate_trajectory_plot_non_ls("Output/ENGLISH-AGExMENTALHEALTH Tables CCHS", "mental_health", "Average Self-Reported Mental Health", "Average Mental Health (on a 0-4 scale) across Age Groups for English Speakers in Canada (CCHS)", save_tag="EN_CA")
generate_trajectory_plot_non_ls("Output/HOMEOWNERxAGExLS Tables CCHS", "home_owner", "Proportion Homeowners", "Average Rate of Homeownership Over Time")

file_path = "Output/HOMEOWNERxAGExLS Tables CCHS"; condition = "home_owner == TRUE"; true_label = "Homeowners"; false_label="Non-homeowners"

create_trajectory_plot_cchs <- function(file_path, condition, true_label, false_label) {
  # Load the data
  data <- list.files(path = file_path, pattern = "*.csv", full.names = TRUE) %>%
    purrr::map_dfr(~ read.csv(.x) %>%
      mutate(year = as.numeric(paste0("20", sub(paste0(file_path, "/.*_(.*)_CCHS.csv"), "\\1", .x)))))

  # Process Data
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

  # Automatically determine min_y and max_y based on the data
  min_y <- floor(min(plot_data$average_ls, na.rm = TRUE))
  max_y <- ceiling(max(plot_data$average_ls, na.rm = TRUE))
  y_breaks <- seq(min_y, max_y, by = 0.5)
  rect_data <- data.frame(ymin = head(y_breaks, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks, -1)[c(TRUE, FALSE)])

  # Create the plot
  age_group_ls_plot_cchs <- ggplot(plot_data, aes(x = display_year, y = average_ls, group = binary_var, color = binary_var)) +
    geom_rect(data = rect_data, 
              aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
              fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
    geom_line(data = filter(plot_data, young == TRUE), aes(x = display_year, y = average_ls, group = binary_var, color = binary_var, linetype = "15-29")) +
    geom_line(data = filter(plot_data, young == FALSE), aes(x = display_year, y = average_ls, group = binary_var, color = binary_var, linetype = "30+")) +
    geom_point(data = filter(plot_data, young == TRUE), aes(x = display_year, y = average_ls, group = binary_var, color = binary_var, shape = "15-29")) +
    geom_point(data = filter(plot_data, young == FALSE), aes(x = display_year, y = average_ls, group = binary_var, color = binary_var, shape = "30+")) +
    labs(title = paste("Youth Life Satisfaction for", tools::toTitleCase(true_label), "(CCHS)"),
         y = "Average Life Satisfaction",
         color = "Group",
         linetype = "Age Ranges",
         shape = "Age Ranges") +
    theme_chr() +
    theme(aspect.ratio = 1/3, plot.background = element_rect(fill = background_colour, color = background_colour), axis.title.y = element_text(angle = 90), legend.key.width = unit(1, "cm")) +  # Adjusted legend key width
    scale_y_continuous(limits = c(min_y, max_y), breaks = y_breaks) +
    scale_x_continuous(limits = c(floor(min(plot_data$display_year, na.rm = TRUE)), ceiling(max(plot_data$display_year, na.rm = TRUE))), breaks = seq(floor(min(plot_data$display_year, na.rm = TRUE)), ceiling(max(plot_data$display_year, na.rm = TRUE)), by = 1), labels = c(seq(floor(min(plot_data$display_year, na.rm = TRUE)), ceiling(max(plot_data$display_year, na.rm = TRUE))-1, by = 1), ""), 
                      expand = c(0, 0)) +
    scale_color_manual(values = setNames(c(six_tone_scale_colours[1], six_tone_scale_colours[3]), c(true_label, false_label))) +
    scale_linetype_manual(values = c("30+" = "solid", "15-29" = "dashed")) +
    scale_shape_manual(values = c("30+" = 16, "15-29" = 17))

  # Print the plot
  print(age_group_ls_plot_cchs)
  
  # Save the plot in different formats
  true_label_formatted <- tolower(gsub("[[:punct:]]", "", gsub(" ", "_", true_label)))
  ggsave(paste0("Output/Plots/Trajectory/PNG/trajectory_by_age_and_", true_label_formatted, "_Canada_CCHS.png"), plot = age_group_ls_plot_cchs + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
  ggsave(paste0("Output/Plots/Trajectory/JPG/trajectory_by_age_and_", true_label_formatted, "_Canada_CCHS.jpg"), plot = age_group_ls_plot_cchs, width = 9, height = 3, dpi = 300)
  ggsave(paste0("Output/Plots/Trajectory/SVG/trajectory_by_age_and_", true_label_formatted, "_Canada_CCHS.svg"), plot = age_group_ls_plot_cchs + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
}


create_trajectory_plot_cchs(
  file_path = "Output/HOMEOWNERxAGExLS Tables CCHS",
  condition = "home_owner == TRUE",
  true_label = "Homeowners",
  false_label = "Non-homeowners"
)

create_trajectory_plot_cchs(
  file_path = "Output/SCREENTIMExAGExLS Tables CCHS",
  condition = "screentime_week %in% c('From 35 to 39 hours', 'From 30 to 34 hours', 'From 40 to 44 hours', '45 hours or more')",
  true_label = "30 or more hours",
  false_label = "Less than 30 hours"
)
