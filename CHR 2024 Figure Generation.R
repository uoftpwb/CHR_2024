# CHR 2024 Figure Generation
# Author: Anthony McCanny
# Date: May 28, 2024
##############################################################################################################

######################################## LOAD PACKAGES #######################################################

library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(Hmisc)
library(patchwork)
library(dplyr)
summarize <- dplyr::summarize

######################################## DECLARE THEMES AND COLOURS ##########################################
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


######################################## LOAD GALLUP DATA ##########################################
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


######################################## LOAD CCHS DATA ##########################################
# Read all the PUMF CSV files and combine them into one dataframe
PUMF_list <- list.files(path = "Data/Tables/PROVxAGExLS Tables CCHS", pattern = "*.csv", full.names = TRUE)
provincePUMF <- PUMF_list %>%
  purrr::map_dfr(~ read.csv(.x) %>%
    mutate(year = as.numeric(paste0("20", sub("Data/Tables/PROVxAGExLS Tables CCHS/PROVxAGExLS_(.*)_CCHS.csv", "\\1", .x)))))

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

            
###########################################################################################################
########################################### TRAJECTORY PLOTS ##############################################
###########################################################################################################

################################ CANADA-WIDE TRAJECTORY PLOTS ################################

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

ggsave(paste0("Output/Final Plots/PNG/", base_filename, ".png"), plot = gallup_trajectory_ls + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 8, height = 3, dpi = 900, bg = "transparent")
ggsave(paste0("Output/Final Plots/JPG/", base_filename, ".jpg"), plot = gallup_trajectory_ls, width = 8, height = 3, dpi = 300)
ggsave(paste0("Output/Final Plots/SVG/", base_filename, ".svg"), plot = gallup_trajectory_ls + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 8, height = 3, dpi = 300, bg = "transparent")

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

################################ LOAD CSS DATA ################################

CSS_list <- list.files(path = "Data/CSS RTRA", pattern = "^PROVxAGExLS.*\\.csv$", full.names = TRUE)
provinceXageCSS <- CSS_list %>%  # Start a chain of commands using the file_list vector
  purrr::map_dfr(~read.csv(.x) %>%  # Read each file in the list into a dataframe and row-bind them together
    rename_with(~if_else(.x %in% c("LSM_01", "GEN_010"), "ls", .x)) %>%  # Rename columns 'LSM_01' and 'GEN_010' to 'ls'
    mutate(year = paste0("20", sub("Data/CSS RTRA/PROVxAGExLS_(.*)_CSS.csv", "\\1", .x)))) %>%  # Extract the year from the filename and add it as a new column
  mutate(province = case_when(
    PR == 10 ~ "Newfoundland and Labrador",
    PR == 11 ~ "Prince Edward Island",
    PR == 12 ~ "Nova Scotia",
    PR == 13 ~ "New Brunswick",
    PR == 24 ~ "Quebec",
    PR == 35 ~ "Ontario",
    PR == 46 ~ "Manitoba",
    PR == 47 ~ "Saskatchewan",
    PR == 48 ~ "Alberta",
    PR == 59 ~ "British Columbia",
    PR == 60 ~ "Yukon",
    PR == 61 ~ "Northwest Territories",
    PR == 62 ~ "Nunavut",
    is.na(PR) ~ "Canada",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(ls) & ls <= 10) %>%  # Drop rows with NA in 'ls' or 'ls' values higher than 10
  mutate(AGEGROUP = case_when(
    AGEGROUP == 2 ~ "15-29",
    AGEGROUP == 3 ~ "30-44",
    AGEGROUP == 4 ~ "45-59",
    AGEGROUP == 5 ~ "60+",
    TRUE ~ "All ages")) %>%
  drop_na(AGEGROUP) %>%
  mutate(year = case_when(
    year == "2021W1" ~ 2021.25,
    year == "2021W2" ~ 2021.50,
    year == "2021W3" ~ 2021.75,
    year == "2021W4" ~ 2022.00,
    year == "2022W1" ~ 2022.5,
    year == "2022W2" ~ 2022.75,
    year == "2022W3" ~ 2023,
    year == "2023W1" ~ 2023.5,
    year == "2023W2" ~ 2023.75
  )) %>%
  select(province, year, age_ranges = AGEGROUP, ls, frequency = X_COUNT_)

# Format data
canadaXageCSS_avg_ls <- provinceXageCSS %>%
  filter(province == "Canada") %>%
  group_by(year, age_ranges) %>%
  mutate(proportion = frequency / sum(frequency)) %>%    
  mutate(average_ls = sum(proportion * as.numeric(ls))) %>%
  filter(age_ranges != "All ages") 

################################ AGE GROUP TRAJECTORY PLOTS ################################

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

ggsave(paste0("Output/Final Plots/PNG/", base_filename, ".png"), plot = gallup_trajectory_ls + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 8, height = 3, dpi = 900, bg = "transparent")
ggsave(paste0("Output/Final Plots/JPG/", base_filename, ".jpg"), plot = gallup_trajectory_ls, width = 8, height = 3, dpi = 300)
ggsave(paste0("Output/Final Plots/SVG/", base_filename, ".svg"), plot = gallup_trajectory_ls + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 8, height = 3, dpi = 300, bg = "transparent")

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

base_filename <- "combined_ls_age_trajectory"
ggsave(paste0("Output/Final Plots/PNG/", base_filename, ".png"), plot = combined_trajectory_ls + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
ggsave(paste0("Output/Final Plots/JPG/", base_filename, ".jpg"), plot = combined_trajectory_ls, width = 9, height = 3, dpi = 300)
ggsave(paste0("Output/Final Plots/SVG/", base_filename, ".svg"), plot = combined_trajectory_ls + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")

### Combined CCHS, CSS, Gallup Age Trajectory Plot ###

# Calculate standard error and confidence intervals for CSS data
canadaXageCSS_avg_ls <- canadaXageCSS_avg_ls %>%
  group_by(year, age_ranges) %>%
  mutate(
    var_ls = wtd.var(ls, weights = frequency, na.rm = TRUE),
    n = sum(frequency, na.rm = TRUE),
    se_ls = sqrt(var_ls / n),
    ci_lower = average_ls - qt(0.975, df = n - 1) * se_ls,
    ci_upper = average_ls + qt(0.975, df = n - 1) * se_ls
  ) %>%
  ungroup()

# Combine CCHS, CSS, and Gallup data
combined_plot_data_all <- bind_rows(
  combined_plot_data,
  canadaXageCSS_avg_ls %>% 
    mutate(source = "Life Satisfaction - CSS") %>%
    select(province, year, age_ranges, average_ls, ci_lower, ci_upper, source)
)

min_y_combined <- floor(min(combined_plot_data_all$ci_lower, na.rm = TRUE) * 2) / 2
max_y_combined <- ceiling(max(combined_plot_data_all$ci_upper, na.rm = TRUE) * 2) / 2
y_breaks_combined <- seq(min_y_combined, max_y_combined, by = 0.5)
rect_data_combined <- data.frame(ymin = head(y_breaks_combined, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks_combined, -1)[c(TRUE, FALSE)])


# Create the combined plot
combined_trajectory_ls_all <- ggplot() +
  geom_rect(data = rect_data_combined, 
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
            fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
  geom_vline(xintercept = c(2015, 2022), linetype = "dashed", color = "black") +
  # Gallup data
  geom_ribbon(data = combined_plot_data_all %>% filter(source == "Life Evaluation - Gallup"), 
              aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = age_ranges), alpha = 0.1) +
  geom_line(data = combined_plot_data_all %>% filter(source == "Life Evaluation - Gallup"), 
            aes(x = year, y = average_ls, color = age_ranges, linetype = source)) +
  geom_point(data = combined_plot_data_all %>% filter(source == "Life Evaluation - Gallup"), 
             aes(x = year, y = average_ls, color = age_ranges, shape = source), size = 2) +
  # CCHS data (separated by time periods)
  geom_ribbon(data = combined_plot_data_all %>% filter(source == "Life Satisfaction - CCHS" & year >= 2009 & year <= 2015), 
              aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = age_ranges), alpha = 0.2) +
  geom_line(data = combined_plot_data_all %>% filter(source == "Life Satisfaction - CCHS" & year >= 2009 & year <= 2015), 
            aes(x = year, y = average_ls, color = age_ranges, linetype = source)) +
  geom_point(data = combined_plot_data_all %>% filter(source == "Life Satisfaction - CCHS"), 
             aes(x = year, y = average_ls, color = age_ranges, shape = source), size = 2) +
  geom_ribbon(data = combined_plot_data_all %>% filter(source == "Life Satisfaction - CCHS" & year >= 2015 & year <= 2022), 
              aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = age_ranges), alpha = 0.2) +
  geom_line(data = combined_plot_data_all %>% filter(source == "Life Satisfaction - CCHS" & year >= 2015 & year <= 2022), 
            aes(x = year, y = average_ls, color = age_ranges, linetype = source)) +
  # CSS data
  geom_ribbon(data = combined_plot_data_all %>% filter(source == "Life Satisfaction - CSS"), 
              aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = age_ranges), alpha = 0.2) +
  geom_line(data = combined_plot_data_all %>% filter(source == "Life Satisfaction - CSS"), 
            aes(x = year, y = average_ls, color = age_ranges, linetype = source)) +
  geom_point(data = combined_plot_data_all %>% filter(source == "Life Satisfaction - CSS"), 
             aes(x = year, y = average_ls, color = age_ranges, shape = source), size = 3) +
  labs(title = "Life Evaluations and Satisfaction in Canada",
       y = "Average Response",
       color = "Age Range",
       fill = "Age Range",
       linetype = "Data Source",
       shape = "Data Source",
       caption = "Data Source: Gallup World Poll, Canadian Community Health Survey, and Canadian Social Survey") +
  theme_chr() +
  theme(aspect.ratio = 1/3, 
        plot.background = element_rect(fill = background_colour, color = background_colour), 
        axis.title.y = element_text(angle = 90), 
        legend.key.width = unit(1, "cm"), 
        plot.caption = element_text(hjust = 0, size = 8, face = "italic")) +
  scale_y_continuous(limits = c(min_y_combined, max_y_combined), breaks = y_breaks_combined) +
  scale_x_continuous(limits = c(floor(min(combined_plot_data_all$year, na.rm = TRUE)), 
                                ceiling(max(combined_plot_data_all$year, na.rm = TRUE))), 
                     breaks = seq(floor(min(combined_plot_data_all$year, na.rm = TRUE)), 
                                  ceiling(max(combined_plot_data_all$year, na.rm = TRUE)), by = 1), 
                     labels = c(seq(floor(min(combined_plot_data_all$year, na.rm = TRUE)), 
                                    ceiling(max(combined_plot_data_all$year, na.rm = TRUE))-1, by = 1), ""), 
                     expand = c(0, 0)) +
  scale_color_manual(values = four_tone_scale_colours) +
  scale_fill_manual(values = four_tone_scale_colours) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid")) +
  scale_shape_manual(values = c(16, 17, 18))

# Print the combined plot
print(combined_trajectory_ls_all)

# Save the plot
base_filename <- "combined_ls_age_trajectory_all_sources"
ggsave(paste0("Output/Final Plots/PNG/", base_filename, ".png"), 
       plot = combined_trajectory_ls_all + theme(plot.background = element_rect(fill = "transparent", color = NA)), 
       width = 12, height = 4, dpi = 300, bg = "transparent")
ggsave(paste0("Output/Final Plots/JPG/", base_filename, ".jpg"), 
       plot = combined_trajectory_ls_all, 
       width = 12, height = 4, dpi = 300)
ggsave(paste0("Output/Final Plots/SVG/", base_filename, ".svg"), 
       plot = combined_trajectory_ls_all + theme(plot.background = element_rect(fill = "transparent", color = NA)), 
       width = 12, height = 4, dpi = 300, bg = "transparent")




######################################## PROVINCIAL MAP ##########################################

library(sf)
library(canadianmaps)
library(dplyr)
library(ggplot2)

# Get the provincial boundary data
data("PROV")

years_to_plot <- c(2009, 2018, 2021)
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

# Handle the special case for territories in 2021
territories_2021 <- tibble(
  province = c("Yukon", "Northwest Territories", "Nunavut"),
  year = 2021,
  weighted_avg_ls = 5
)
# Combine the modified data
province_map_data <- province_map_data %>%
  filter(!(year == 2009 & province == "Yukon/Northwest Territories/Nunavut")) %>%
  bind_rows(territories_2009, territories_2021)

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


###########################################################################################################
########################################### DEMOGRAPHIC PLOTS #############################################
###########################################################################################################


##################################### DEMOGRAPHIC DISTRIBUTION PLOTS ######################################

demographic_distribution_plot <- function(file_path, condition1, condition2, group1_label, group2_label, plot_title, year_choice = 2022) {
  # Import Data
  data <- list.files(path = file_path, pattern = "*.csv", full.names = TRUE) %>%
    purrr::map_dfr(~ read.csv(.x) %>%
      mutate(year = as.numeric(sub(".*_([0-9]+)_CCHS\\.csv$", "20\\1", .x))))
  
  plot_data <- data %>%
    filter(year == year_choice) %>%
    mutate(group = case_when(
      eval(parse(text = condition1)) ~ group1_label,
      eval(parse(text = condition2)) ~ group2_label,
      TRUE ~ NA_character_
    )) %>%
    drop_na(group) %>%
    group_by(year, group, ls) %>%
    summarise(frequency = sum(frequency, na.rm = TRUE)) %>%
    group_by(year, group) %>%
    mutate(proportion = frequency / sum(frequency, na.rm = TRUE)) %>%
    mutate(average_ls = wtd.mean(as.numeric(ls), frequency, na.rm = TRUE))
  
  min_y <- 0
  max_y <- 0.35
  y_breaks <- seq(min_y, max_y, by = 0.1)
  rect_data <- data.frame(ymin = head(y_breaks, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks, -1)[c(TRUE, FALSE)])

  # Create a custom color mapping
  color_map <- setNames(c(six_tone_scale_colours[1], six_tone_scale_colours[5]), c(group1_label, group2_label))

  # Create the bar chart with a completely custom legend
  demographic_plot <- ggplot() +
      geom_rect(data = rect_data, 
          aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
          fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
      geom_bar(data = plot_data, 
               stat = "identity", 
               width = 0.95, 
               aes(x = as.factor(ls), y = proportion, fill = group), 
               position = position_dodge(width = 0.95), 
               show.legend = NA) +
      geom_segment(data = plot_data %>% filter(group == group1_label) %>% group_by(group) %>% dplyr::summarize(average_ls = first(average_ls)),
                   aes(x = average_ls, xend = average_ls, y = 0, yend = max_y * 0.95),
                   color = colorspace::darken(six_tone_scale_colours[1], amount = 0.3), 
                   linetype = "solid", 
                   linewidth = 0.5) +
      geom_segment(data = plot_data %>% filter(group == group2_label) %>% group_by(group) %>% dplyr::summarize(average_ls = first(average_ls)),
                   aes(x = average_ls, xend = average_ls, y = 0, yend = max_y * 0.95),
                   color = colorspace::darken(six_tone_scale_colours[5], amount = 0.3), 
                   linetype = "solid", 
                   linewidth = 0.5) +
      labs(title = plot_title,
          x = "Life Satisfaction Score",
          y = "Proportion") +
      theme_chr() +
      theme(aspect.ratio = 1, legend.position = "right", axis.title.x = element_text(margin = margin(t = 5)),
            axis.text.x = element_text(hjust = 0.5),
            axis.ticks.x = element_blank()) +  # Remove x-axis ticks
      scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0), limits = c(min_y, max_y)) +
      #scale_x_discrete(breaks = unique(plot_data$ls), labels = unique(plot_data$ls)) +  # Explicitly set x-axis breaks and labels
      scale_fill_manual(values = color_map, guide = 'none') +
      scale_color_identity(name = "Group", labels = c(str_wrap(group1_label, width = 15), str_wrap(group2_label, width = 15)), 
                           guide = 'legend', breaks = c(six_tone_scale_colours[1], six_tone_scale_colours[5])) +
      geom_point(aes(x = 3, y = -5, color = six_tone_scale_colours[1]), size = 10, shape = 15, show.legend = TRUE) +
      geom_point(aes(x = 3, y = -5, color = six_tone_scale_colours[5]), size = 10, shape = 15, show.legend = TRUE) +
      geom_text(aes(x = Inf, y = 0.32, label = year_choice), 
                hjust = 1.5, size = 5, 
                inherit.aes = FALSE, show.legend = FALSE)
  print(demographic_plot)

  # Save plot
  group1_label_sanitized <- tolower(gsub(" ", "_", group1_label))
  # ggsave(paste0("Output/Final Plots/PNG/", group1_label_sanitized, "_distribution.png"), plot = demographic_plot + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 8, height = 6, dpi = 300, bg = "transparent")
  # ggsave(paste0("Output/Final Plots/JPG/", group1_label_sanitized, "_distribution.svg"), plot = demographic_plot + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 8, height = 6, dpi = 300, bg = "transparent")
  # ggsave(paste0("Output/Final Plots/SVG/", group1_label_sanitized, "_distribution.jpg"), plot = demographic_plot, width = 8, height = 6, dpi = 300)

  return(demographic_plot)
}

# demographic_distribution_plot("Data/Tables/INDIGENOUSxLS Tables CCHS", "indigenous == 'Yes'", "indigenous == 'No'", "First Nation, Metis and Inuit", "Other Canadians", "First Nation, Metis and Inuit Life Satisfaction in 2022")

# # CHANGE year_choice VARIABLE IN THIS CALL IF UPDATED DATA IS RECEIVED:
# demographic_distribution_plot("Data/Tables/MINORITYxLS Tables CCHS", "minority == 'Non-white'", "minority == 'White'", "Visible Minority", "Other Canadians", "Life Satisfaction of Visible Minorities in Canada in 2017-2018", year_choice = 2018)

# demographic_distribution_plot("Data/Tables/MENTALHEALTHxLS Tables CCHS", "mental_health <= 1", "mental_health > 1", "Poor or Fair Mental Health", "Good or Better Mental Health", "Life Satisfaction of Canadians by Self-Perceived Mental Health in 2022")

# demographic_distribution_plot("Data/Tables/LANGUAGExLS Tables CCHS", "language %in% c('French', 'English and French')", "language == 'English'", "French Speakers", "Other Canadians", "Life Satisfaction of Canadians who Learned French before English in 2022")

# demographic_distribution_plot("Data/Tables/SEXxLS Tables CCHS", "sex == 'Female'", "sex == 'Male'", "Female", "Male", "Life Satisfaction of Canadians by Sex at Birth in 2022")

# demographic_distribution_plot("Data/Tables/SEXUALORIENTATIONxLS Tables CCHS", "sexual_orientation == 'Sexual minorities'", "sexual_orientation == 'Heterosexual'", "Sexual Minorities", "Heterosexual Canadians", "Life Satisfaction of Sexual Minorities in Canada in 2022")

# demographic_distribution_plot("Data/Tables/POVERTYxLS Tables CCHS", "poverty == TRUE", "poverty == FALSE", "Low Income (<$25,252)", "Other Canadians", "Life Satisfaction of Low Income Canadians in 2022")

# demographic_distribution_plot("Data/Tables/HOMEOWNERxLS Tables CCHS", "home_owner == FALSE", "home_owner == TRUE", "Non-Homeowners", "Homeowners", "Life Satisfaction of Non-homeowners in Canada in 2022")

# demographic_distribution_plot("Data/Tables/SCREENTIMEWExLS Tables CCHS", "screentime_weekend %in% c('6 hours to less than 8 hours', '8 hours or more per day')", "screentime_weekend < '6 hours'", "6 hours or more", "Less than 6 hours", "Life Satisfaction of Canadians by Screentime on Weekends")

# demographic_distribution_plot("Data/Tables/SCREENTIMEWDxLS Tables CCHS", "screentime_weekday %in% c('6 hours to less than 8 hours', '8 hours or more per day')", "screentime_weekday < '6 hours'", "6 hours or more", "Less than 6 hours", "Life Satisfaction of Canadians by Screentime on Weekdays")

# demographic_distribution_plot("Data/Tables/YOUTH-SCREENTIMEWDxLS Tables CCHS", "screentime_weekday %in% c('6 hours to less than 8 hours', '8 hours or more per day')", "screentime_weekday < '6 hours'", "6 hours or more", "Less than 6 hours", "Life Satisfaction of 15-29 Year Old Canadians by Screentime on Weekdays")


##################################### DEMOGRAPHIC TRAJECTORY PLOTS #####################################

demographic_trajectory_plot <- function(file_path, condition1, condition2, group1_label, group2_label, plot_title, override_filename = FALSE, include_title = TRUE, show_legend = TRUE) {
  data_source_caption <- "Data Source: Canadian Community Health Survey"
  
  data <- list.files(path = file_path, pattern = "*.csv", full.names = TRUE) %>%
    purrr::map_dfr(~ read.csv(.x) %>%
      mutate(year = as.numeric(paste0("20", sub(paste0(file_path, "/.*_(.*)_CCHS.csv"), "\\1", .x))))) %>%
    mutate(year = case_when(year %in% c(2016, 2018, 2020) ~ as.numeric(year),
                            as.numeric(year) <= 2023 ~ as.numeric(year) + 0.5),
           ls = as.numeric(ls)) %>%
    group_by(year) %>%
    mutate(WGT = frequency / sum(frequency) * 65000) %>%
    ungroup()
  
  plot_data <- data %>%
    mutate(binary_var = if_else(eval(parse(text = condition1)), group1_label, if_else(eval(parse(text = condition2)), group2_label, NA_character_))) %>%
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
  trajectory_plot <- ggplot() +
    geom_rect(data = rect_data, 
              aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
              fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
    geom_segment(aes(x = 2015, xend = 2015, y = min_y, yend = max_y), linetype = "dashed", color = "gray") +
    geom_segment(aes(x = 2022, xend = 2022, y = min_y, yend = max_y), linetype = "dashed", color = "gray") +
    geom_ribbon(data = plot_data %>% filter(year >= 2009 & year <= 2015), 
                aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = binary_var), alpha = 0.2, show.legend = show_legend) +
    geom_line(data = plot_data %>% filter(year >= 2009 & year <= 2015), 
              aes(x = year, y = average_ls, group = binary_var, color = binary_var), show.legend = show_legend) +
    geom_ribbon(data = plot_data %>% filter(year >= 2015 & year <= 2022), 
                aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = binary_var), alpha = 0.2, show.legend = show_legend) +
    geom_line(data = plot_data %>% filter(year >= 2015 & year <= 2022), 
              aes(x = year, y = average_ls, group = binary_var, color = binary_var), show.legend = show_legend) +
    geom_point(data = plot_data, aes(x = year, y = average_ls, group = binary_var, color = binary_var), show.legend = show_legend) +
    labs(title = if (include_title) plot_title else NULL,
         y = "Average Life Satisfaction",
         color = "Group",
         fill = "Group",
         caption = data_source_caption) +
    theme_chr() +
    theme(aspect.ratio = 1/3, plot.background = element_rect(fill = background_colour, color = background_colour), axis.title.y = element_text(angle = 90), legend.key.width = unit(1, "cm"), plot.caption = element_text(hjust = 0, size = 8, face = "italic")) +  # Adjusted legend key width
    scale_y_continuous(limits = c(min_y, max_y), breaks = y_breaks) +
    scale_x_continuous(limits = c(floor(min(plot_data$year, na.rm = TRUE)-0.01), ceiling(max(plot_data$year, na.rm = TRUE)+0.01)), breaks = seq(floor(min(plot_data$year, na.rm = TRUE)-0.01), ceiling(max(plot_data$year, na.rm = TRUE)+0.01), by = 1), labels = c(seq(floor(min(plot_data$year, na.rm = TRUE)-0.01), ceiling(max(plot_data$year, na.rm = TRUE)+0.01) - 1, by = 1), ""), 
                        expand = c(0, 0)) +
    scale_color_manual(values = setNames(c(six_tone_scale_colours[1], six_tone_scale_colours[5]), c(group1_label, group2_label))) +
    scale_fill_manual(values = setNames(c(six_tone_scale_colours[1], six_tone_scale_colours[5]), c(group1_label, group2_label)))
  # Print the plot
  print(trajectory_plot)
  
  # Save the plot in different formats
  group1_label_sanitized <- tolower(gsub(" ", "_", group1_label))
  base_filename <- if (is.character(override_filename)) override_filename else paste0(group1_label_sanitized, "_trajectory")
  
  # ggsave(paste0("Output/Final Plots/PNG/", base_filename, ".png"), plot = trajectory_plot + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
  # ggsave(paste0("Output/Final Plots/JPG/", base_filename, ".jpg"), plot = trajectory_plot, width = 9, height = 3, dpi = 300)
  # ggsave(paste0("Output/Final Plots/SVG/", base_filename, ".svg"), plot = trajectory_plot + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
  return(trajectory_plot)
}

# demographic_trajectory_plot("Data/Tables/MINORITYxLS Tables CCHS", "minority == 'Non-white'", "minority == 'White'", "Visible Minority", "Other Canadians", "Life Satisfaction of Visible Minorities over Time")

# demographic_trajectory_plot("Data/Tables/MENTALHEALTHxLS Tables CCHS", "mental_health <= 1", "mental_health > 1", "Poor or Fair Mental Health", "Good or Better Mental Health", "Life Satisfaction of Canadians by Self-Perceived Mental Health over Time")

# demographic_trajectory_plot("Data/Tables/LANGUAGExLS Tables CCHS", "language %in% c('French', 'English and French')", "language == 'English'", "French Speakers", "Other Canadians", "Life Satisfaction of Canadians who Learned French before English over Time")

# demographic_trajectory_plot("Data/Tables/SEXxLS Tables CCHS", "sex == 'Female'", "sex == 'Male'", "Female", "Male", "Life Satisfaction of Canadians by Sex at Birth over Time")

# demographic_trajectory_plot("Data/Tables/SEXUALORIENTATIONxLS Tables CCHS", "sexual_orientation == 'Sexual minorities'", "sexual_orientation == 'Heterosexual'", "Sexual Minorities", "Heterosexual Canadians", "Life Satisfaction of Sexual Minorities in Canada over Time")

# demographic_trajectory_plot("Data/Tables/POVERTYxLS Tables CCHS", "poverty == TRUE", "poverty == FALSE", "Low Income (<$25,252)", "Other Canadians", "Life Satisfaction of Low Income Canadians over Time")

# demographic_trajectory_plot("Data/Tables/HOMEOWNERxLS Tables CCHS", "home_owner == FALSE", "home_owner == TRUE", "Non-Homeowners", "Homeowners", "Life Satisfaction of Non-homeowners in Canada over Time")


##################################### DEMOGRAPHIC COMBINED PLOTS #####################################

generate_combined_plot <- function(file_path, group1_condition, group2_condition, group1_label, group2_label, distribution_plot_title, trajectory_plot_title, include_title = TRUE, show_legend = TRUE, override_filename = NULL, year_choice = 2022) {
  # Ensure required packages are loaded
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required but not installed.")
  if (!requireNamespace("patchwork", quietly = TRUE)) stop("Package 'patchwork' is required but not installed.")
  
  # Generate the distribution plot
  distribution_plot <- demographic_distribution_plot(file_path, group1_condition, group2_condition, group1_label, group2_label, distribution_plot_title, year_choice = year_choice) +
    theme(plot.background = element_rect(fill = "transparent", color = NA))
  
  # Generate the trajectory plot with the appropriate title
  trajectory_plot <- demographic_trajectory_plot(file_path, group1_condition, group2_condition, group1_label, group2_label, trajectory_plot_title, include_title = include_title, show_legend = show_legend) +
    theme(plot.background = element_rect(fill = "transparent", color = NA))

  # Combine the plots using patchwork with a small space between the two plots
  combined_plot <- free(distribution_plot) + trajectory_plot + plot_layout(ncol = 1, heights = c(0.88, 0.33))

  # Sanitize the filename
  group1_label_sanitized <- tolower(gsub(" ", "_", group1_label))
  base_filename <- if (is.character(override_filename)) override_filename else paste0(group1_label_sanitized, "_demographic_combined")
  
  print(combined_plot & theme(plot.background = element_rect(fill = "transparent", color = NA)))

  # Save the combined plot in different formats
  ggsave(paste0("Output/Final Plots/PNG/", base_filename, ".png"), plot = combined_plot & theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 8, height = 9, dpi = 300, bg = "transparent")
  ggsave(paste0("Output/Final Plots/JPG/", base_filename, ".jpg"), plot = combined_plot & theme(plot.background = element_rect(fill = background_colour, color = NA)), width = 8, height = 9, dpi = 300)
  ggsave(paste0("Output/Final Plots/SVG/", base_filename, ".svg"), plot = combined_plot & theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 8, height = 9, dpi = 300, bg = "transparent")
}

generate_combined_plot(
  file_path = "Data/Tables/INDIGENOUSxLS Tables CCHS",
  group1_condition = "indigenous == 'Yes'",
  group2_condition = "indigenous == 'No'",
  group1_label = "First Nation, Metis and Inuit",
  group2_label = "Other Canadians",
  distribution_plot_title = "Distribution and Trend of First Nation, Metis and Inuit Life Satisfaction",
  trajectory_plot_title = "Life Satisfaction of First Nation, Metis and Inuit over Time",
  show_legend = FALSE,
  include_title = FALSE
)

generate_combined_plot(
  file_path = "Data/Tables/MINORITYxLS Tables CCHS",
  group1_condition = "minority == 'Non-white'",
  group2_condition = "minority == 'White'",
  group1_label = "Visible Minority",
  group2_label = "Other Canadians",
  distribution_plot_title = "Distribution and Trend of Life Satisfaction by Visible Minority Status",
  trajectory_plot_title = "Life Satisfaction of Visible Minorities over Time",
  show_legend = FALSE,
  include_title = FALSE
)

generate_combined_plot(
  file_path = "Data/Tables/MENTALHEALTHxLS Tables CCHS",
  group1_condition = "mental_health <= 1",
  group2_condition = "mental_health > 1",
  group1_label = "Poor or Fair Mental Health",
  group2_label = "Good or Better Mental Health",
  distribution_plot_title = "Distribution and Trend of Life Satisfaction by Mental Health Status",
  trajectory_plot_title = "Life Satisfaction of Canadians by Self-Perceived Mental Health over Time",
  show_legend = FALSE,
  include_title = FALSE
)

generate_combined_plot(
  file_path = "Data/Tables/LANGUAGExLS Tables CCHS",
  group1_condition = "language %in% c('French', 'English and French')",
  group2_condition = "language == 'English'",
  group1_label = "French Speakers",
  group2_label = "Other Canadians",
  distribution_plot_title = "Distribution and Trend of Life Satisfaction by Language",
  trajectory_plot_title = "Life Satisfaction of Canadians who Learned French before English over Time",
  show_legend = FALSE,
  include_title = FALSE
)

generate_combined_plot(
  file_path = "Data/Tables/SEXxLS Tables CCHS",
  group1_condition = "sex == 'Female'",
  group2_condition = "sex == 'Male'",
  group1_label = "Female",
  group2_label = "Male",
  distribution_plot_title = "Distribution and Trend of Life Satisfaction by Sex",
  trajectory_plot_title = "Life Satisfaction of Canadians by Sex at Birth over Time",
  show_legend = FALSE,
  include_title = FALSE
)

generate_combined_plot(
  file_path = "Data/Tables/SEXUALORIENTATIONxLS Tables CCHS",
  group1_condition = "sexual_orientation == 'Sexual minorities'",
  group2_condition = "sexual_orientation == 'Heterosexual'",
  group1_label = "Sexual Minorities",
  group2_label = "Heterosexual Canadians",
  distribution_plot_title = "Distribution and Trend of Life Satisfaction by Sexual Orientation",
  trajectory_plot_title = "Life Satisfaction of Sexual Minorities in Canada over Time",
  show_legend = FALSE,
  include_title = FALSE
)

generate_combined_plot(
  file_path = "Data/Tables/POVERTYxLS Tables CCHS",
  group1_condition = "poverty == TRUE",
  group2_condition = "poverty == FALSE",
  group1_label = "Low Income (<$25,252)",
  group2_label = "Other Canadians",
  distribution_plot_title = "Distribution and Trend of Life Satisfaction by Income",
  trajectory_plot_title = "Life Satisfaction of Low Income Canadians over Time",
  show_legend = FALSE,
  include_title = FALSE
)

generate_combined_plot(
  file_path = "Data/Tables/HOMEOWNERxLS Tables CCHS",
  group1_condition = "home_owner == FALSE",
  group2_condition = "home_owner == TRUE",
  group1_label = "Non-Homeowners",
  group2_label = "Homeowners",
  distribution_plot_title = "Distribution and Trend of Life Satisfaction by Homeownership",
  trajectory_plot_title = "Life Satisfaction of Non-homeowners in Canada over Time",
  show_legend = FALSE,
  include_title = FALSE
)


######################################## IMMIGRATION PLOTS ##########################################

################################### IMMIGRATION DISTRIBUTION PLOT ###################################
  file_path <- "Data/Tables/IMMIGRATIONxLS Tables CCHS"
  plot_title <- "Distribution and Trend of Life Satisfaction by Time in Canada"

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
      geom_segment(data = plot_data %>% filter(year >= 2021 & condition_met == "0-9 years") %>% 
                   group_by(condition_met) %>% dplyr::summarize(average_ls = first(average_ls)),
                   aes(x = average_ls, xend = average_ls, y = 0, yend = 0.33), 
                   color = colorspace::darken(six_tone_scale_colours[1], amount = 0.3), linetype = "solid", linewidth = 0.5) +
      geom_segment(data = plot_data %>% filter(year >= 2021 & condition_met == "10 or more years") %>% 
                   group_by(condition_met) %>% dplyr::summarize(average_ls = first(average_ls)),
                   aes(x = average_ls, xend = average_ls, y = 0, yend = 0.33), 
                   color = colorspace::darken(six_tone_scale_colours[2], amount = 0.3), linetype = "solid", linewidth = 0.5) +
      geom_segment(data = plot_data %>% filter(year >= 2021 & condition_met == "Canadian born") %>% 
                   group_by(condition_met) %>% dplyr::summarize(average_ls = first(average_ls)),
                   aes(x = average_ls, xend = average_ls, y = 0, yend = 0.33), 
                   color = colorspace::darken(six_tone_scale_colours[5], amount = 0.3), linetype = "solid", linewidth = 0.5) +
      labs(title = plot_title,
          x = "Life Satisfaction Score",
          y = "Proportion",
          group = "Time in Canada") +
      theme_chr() +
      theme(aspect.ratio = 1, legend.position = "right", axis.title.x = element_text(margin = margin(t = 5))) + 
      scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0), limits = c(min_y, max_y)) +
      scale_fill_manual(values = c(six_tone_scale_colours[1], six_tone_scale_colours[2], six_tone_scale_colours[5]), guide = FALSE) +
      scale_color_identity(name = "Time in Canada", labels = c("0-9 years", "10 or more years", "Canadian born"), 
                           guide = 'legend', breaks = c(six_tone_scale_colours[1], six_tone_scale_colours[2], six_tone_scale_colours[5])) +
      geom_point(aes(x = 3, y = -5, color = six_tone_scale_colours[1]), size = 10, shape = 15, show.legend = TRUE) +
      geom_point(aes(x = 3, y = -5, color = six_tone_scale_colours[2]), size = 10, shape = 15, show.legend = TRUE) +
      geom_point(aes(x = 3, y = -5, color = six_tone_scale_colours[5]), size = 10, shape = 15, show.legend = TRUE) + 
      geom_text(aes(x = Inf, y = 0.32, label = "2022"), 
          hjust = 1.5, size = 5, 
          inherit.aes = FALSE, show.legend = FALSE)
  print(demographic_plot)

  # Save plot -- Commented out so that only combined versions of plots are saved
  # ggsave(paste0("Output/Final Plots/PNG/immigration_distribution.png"), plot = demographic_plot + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 8, height = 6, dpi = 300, bg = "transparent")
  # ggsave(paste0("Output/Final Plots/JPG/immigration_distribution.svg"), plot = demographic_plot + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 8, height = 6, dpi = 300, bg = "transparent")
  # ggsave(paste0("Output/Final Plots/SVG/immigration_distribution.jpg"), plot = demographic_plot, width = 8, height = 6, dpi = 300)

  # Calculate the percentage of respondents in >= 2021 that fall into each of the three categories
  percentage_data <- plot_data %>%
    filter(year >= 2021) %>%
    group_by(condition_met) %>%
    dplyr::summarize(total_count = sum(frequency, na.rm = TRUE)) %>%
    mutate(percentage = total_count / sum(total_count) * 100)
  
  # Print the percentage data
  print(percentage_data)


###################################### IMMIGRATION TREND PLOT #######################################

data <- list.files(path = file_path, pattern = "*.csv", full.names = TRUE) %>%
  purrr::map_dfr(~ read.csv(.x) %>%
    mutate(year = as.numeric(paste0("20", sub(paste0(file_path, "/.*_(.*)_CCHS.csv"), "\\1", .x))))) %>%
  mutate(year = case_when(year %in% c(2016, 2018, 2020) ~ as.numeric(year),
                          as.numeric(year) <= 2023 ~ as.numeric(year) + 0.5),
          ls = as.numeric(ls)) %>%
  group_by(year) %>%
  mutate(WGT = frequency / sum(frequency) * 65000) %>%
  ungroup()

plot_data <- data %>%
    mutate(condition_met = case_when(
      immigration == "0-9 years" ~ "0-9 years",
      immigration == "10 or more years" ~ "10 or more years",
      immigration == "Canadian born" ~ "Canadian born",
      TRUE ~ NA_character_
    )) %>%
  group_by(year, condition_met) %>%
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
  drop_na(condition_met)

# Automatically determine min_y and max_y based on the confidence intervals
min_y <- floor(min(plot_data$ci_lower, na.rm = TRUE) * 2) / 2
max_y <- ceiling(max(plot_data$ci_upper, na.rm = TRUE) * 2) / 2
y_breaks <- seq(min_y, max_y, by = 0.5)
rect_data <- data.frame(ymin = head(y_breaks, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks, -1)[c(TRUE, FALSE)])
show_legend <- FALSE

# Create the plot
trajectory_plot <- ggplot() +
  geom_rect(data = rect_data, 
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
            fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
  geom_segment(aes(x = 2015, xend = 2015, y = min_y, yend = max_y), linetype = "dashed", color = "gray") +
  geom_segment(aes(x = 2022, xend = 2022, y = min_y, yend = max_y), linetype = "dashed", color = "gray") +
  geom_ribbon(data = plot_data %>% filter(year >= 2009 & year <= 2015 & condition_met == "0-9 years"), 
              aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = condition_met), alpha = 0.2, show.legend = show_legend) +
  geom_line(data = plot_data %>% filter(year >= 2009 & year <= 2015 & condition_met == "0-9 years"), 
            aes(x = year, y = average_ls, group = condition_met, color = condition_met), show.legend = show_legend) +
  geom_ribbon(data = plot_data %>% filter(year >= 2009 & year <= 2015 & condition_met == "10 or more years"), 
              aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = condition_met), alpha = 0.2, show.legend = show_legend) +
  geom_line(data = plot_data %>% filter(year >= 2009 & year <= 2015 & condition_met == "10 or more years"), 
            aes(x = year, y = average_ls, group = condition_met, color = condition_met), show.legend = show_legend) +
  geom_ribbon(data = plot_data %>% filter(year >= 2009 & year <= 2015 & condition_met == "Canadian born"), 
              aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = condition_met), alpha = 0.2, show.legend = show_legend) +
  geom_line(data = plot_data %>% filter(year >= 2009 & year <= 2015 & condition_met == "Canadian born"), 
            aes(x = year, y = average_ls, group = condition_met, color = condition_met), show.legend = show_legend) +
  geom_ribbon(data = plot_data %>% filter(year >= 2015 & year <= 2022 & condition_met == "0-9 years"), 
              aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = condition_met), alpha = 0.2, show.legend = show_legend) +
  geom_line(data = plot_data %>% filter(year >= 2015 & year <= 2022 & condition_met == "0-9 years"), 
            aes(x = year, y = average_ls, group = condition_met, color = condition_met), show.legend = show_legend) +
  geom_ribbon(data = plot_data %>% filter(year >= 2015 & year <= 2022 & condition_met == "10 or more years"), 
              aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = condition_met), alpha = 0.2, show.legend = show_legend) +
  geom_line(data = plot_data %>% filter(year >= 2015 & year <= 2022 & condition_met == "10 or more years"), 
            aes(x = year, y = average_ls, group = condition_met, color = condition_met), show.legend = show_legend) +
  geom_ribbon(data = plot_data %>% filter(year >= 2015 & year <= 2022 & condition_met == "Canadian born"), 
              aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = condition_met), alpha = 0.2, show.legend = show_legend) +
  geom_line(data = plot_data %>% filter(year >= 2015 & year <= 2022 & condition_met == "Canadian born"), 
            aes(x = year, y = average_ls, group = condition_met, color = condition_met), show.legend = show_legend) +
  geom_point(data = plot_data, aes(x = year, y = average_ls, group = condition_met, color = condition_met), show.legend = show_legend) +
  labs(title =  NULL,
        y = "Average Life Satisfaction",
        color = "Group",
        fill = "Group",
        caption = "Data Source: Canadian Community Health Survey") +
  theme_chr() +
  theme(aspect.ratio = 1/3, plot.background = element_rect(fill = background_colour, color = background_colour), axis.title.y = element_text(angle = 90), legend.key.width = unit(1, "cm"), plot.caption = element_text(hjust = 0, size = 8, face = "italic")) +  # Adjusted legend key width
  scale_y_continuous(limits = c(min_y, max_y), breaks = y_breaks) +
  scale_x_continuous(limits = c(floor(min(plot_data$year, na.rm = TRUE)-0.01), ceiling(max(plot_data$year, na.rm = TRUE)+0.01)), breaks = seq(floor(min(plot_data$year, na.rm = TRUE)-0.01), ceiling(max(plot_data$year, na.rm = TRUE)+0.01), by = 1), labels = c(seq(floor(min(plot_data$year, na.rm = TRUE)-0.01), ceiling(max(plot_data$year, na.rm = TRUE)+0.01) - 1, by = 1), ""), 
                      expand = c(0, 0)) +
  scale_color_manual(values = setNames(c(six_tone_scale_colours[1], six_tone_scale_colours[2], six_tone_scale_colours[5]), c("0-9 years", "10 or more years", "Canadian born"))) +
  scale_fill_manual(values = setNames(c(six_tone_scale_colours[1], six_tone_scale_colours[2], six_tone_scale_colours[5]), c("0-9 years", "10 or more years", "Canadian born")))
# Print the plot
print(trajectory_plot)

# Combine the original and special immigration plots
combined_immigration_plot <- free(demographic_plot) + trajectory_plot + plot_layout(ncol = 1, heights = c(0.88, 0.33))

# Print the combined plot
print(combined_immigration_plot & theme(plot.background = element_rect(fill = background_colour, color = NA)))

# Save the combined plot in different formats
ggsave(paste0("Output/Final Plots/PNG/immigration_distribution_combined.png"), plot = combined_immigration_plot & theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 8, height = 9, dpi = 300, bg = "transparent")
ggsave(paste0("Output/Final Plots/JPG/immigration_distribution_combined.jpg"), plot = combined_immigration_plot & theme(plot.background = element_rect(fill = background_colour, color = NA)), width = 8, height = 9, dpi = 300)
ggsave(paste0("Output/Final Plots/SVG/immigration_distribution_combined.svg"), plot = combined_immigration_plot & theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 8, height = 9, dpi = 300, bg = "transparent")



###########################################################################################################
########################################### MENTAL HEALTH PLOTS ###########################################
###########################################################################################################

################################### MENTAL HEALTH AGE RANGE PLOT ####################################

# Load and process data from CSV files
data <- list.files(path = "Data/Tables/ENGLISHxQUEBECxAGExMENTALHEALTH Tables CCHS", pattern = "*.csv", full.names = TRUE) %>%
  purrr::map_dfr(~ read.csv(.x) %>%
    mutate(year = as.numeric(paste0("20", sub(".*_(.*)_CCHS.csv", "\\1", .x))))) %>%  # Extract year from filename and convert to numeric
  mutate(year = case_when(year %in% c(2016, 2018, 2020) ~ year,  # Adjust year for specific cases
                          year <= 2023 ~ year + 0.5),
         mental_health = as.numeric(mental_health)) %>%  # Convert mental_health to numeric
  group_by(year) %>%
  mutate(WGT = frequency / sum(frequency) * 65000) %>%  # Calculate weights
  ungroup()

# Summarize and calculate statistics for plot data
plot_data <- data %>%
  drop_na(age_ranges) %>%  # Drop rows with NA in age_ranges
  group_by(year, age_ranges) %>%  # Group by year and age_ranges
  dplyr::summarize(
    average_mh = weighted.mean(mental_health, WGT, na.rm = TRUE),  # Calculate weighted mean of mental_health
    n = sum(WGT, na.rm = TRUE),  # Sum of weights
    var_mh = wtd.var(mental_health, weights = WGT, na.rm = TRUE),  # Calculate weighted variance of mental_health
    .groups = "drop"
  ) %>%
  mutate(
    se_mh = sqrt(var_mh / n),  # Calculate standard error
    ci_lower = average_mh - qt(0.975, df = n - 1) * se_mh,  # Calculate lower bound of confidence interval
    ci_upper = average_mh + qt(0.975, df = n - 1) * se_mh  # Calculate upper bound of confidence interval
  )

data_source_caption <- "Data Source: Canadian Community Health Survey"  # Data source caption

# Automatically determine min_y and max_y based on the confidence intervals
min_y <- floor(min(plot_data$ci_lower, na.rm = TRUE) * 2) / 2
max_y <- ceiling(max(plot_data$ci_upper, na.rm = TRUE) * 2) / 2
y_breaks <- seq(min_y, max_y, by = 0.5)
rect_data <- data.frame(ymin = head(y_breaks, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks, -1)[c(TRUE, FALSE)])

# Create the mental health trajectory plot
mental_health_trajectory <- ggplot() +
  geom_rect(data = rect_data, 
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
            fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +  # Add background rectangles
  geom_segment(aes(x = 2015, xend = 2015, y = min_y, yend = max_y), linetype = "dashed", color = "gray") +  # Add dashed line for 2015
  geom_segment(aes(x = 2022, xend = 2022, y = min_y, yend = max_y), linetype = "dashed", color = "gray") +  # Add dashed line for 2022
  geom_ribbon(data = plot_data %>% filter(year >= 2009 & year <= 2015), 
              aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = age_ranges), alpha = 0.2, show.legend = TRUE) +  # Add confidence interval ribbons for 2009-2015
  geom_line(data = plot_data %>% filter(year >= 2009 & year <= 2015), 
            aes(x = year, y = average_mh, group = age_ranges, color = age_ranges), show.legend = TRUE) +  # Add lines for 2009-2015
  geom_ribbon(data = plot_data %>% filter(year >= 2015 & year <= 2022), 
              aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = age_ranges), alpha = 0.2, show.legend = TRUE) +  # Add confidence interval ribbons for 2015-2022
  geom_line(data = plot_data %>% filter(year >= 2015 & year <= 2022), 
            aes(x = year, y = average_mh, group = age_ranges, color = age_ranges), show.legend = TRUE) +  # Add lines for 2015-2022
  geom_point(data = plot_data, 
             aes(x = year, y = average_mh, group = age_ranges, color = age_ranges), show.legend = TRUE) +  # Add points for all years
  labs(title = "Self-Reported Mental Health by Age Range",
       y = "Average Mental Health",
       color = "Age Range",
       fill = "Age Range",
       caption = data_source_caption) +  # Add labels
  theme_chr() +
  theme(aspect.ratio = 1/3, plot.background = element_rect(fill = background_colour, color = background_colour), axis.title.y = element_text(angle = 90), legend.key.width = unit(1, "cm"), plot.caption = element_text(hjust = 0, size = 8, face = "italic")) +  # Adjusted legend key width
  scale_y_continuous(limits = c(min_y, max_y), breaks = y_breaks) +  # Set y-axis limits and breaks
  scale_x_continuous(limits = c(floor(min(plot_data$year, na.rm = TRUE)-0.01), ceiling(max(plot_data$year, na.rm = TRUE)+0.01)), breaks = seq(floor(min(plot_data$year, na.rm = TRUE)-0.01), ceiling(max(plot_data$year, na.rm = TRUE)+0.01), by = 1), labels = c(seq(floor(min(plot_data$year, na.rm = TRUE)-0.01), ceiling(max(plot_data$year, na.rm = TRUE)+0.01) - 1, by = 1), ""), 
                      expand = c(0, 0)) +  # Set x-axis limits and breaks
  scale_color_manual(values = setNames(six_tone_scale_colours[1:4], unique(plot_data$age_ranges))) +  # Set color scale
  scale_fill_manual(values = setNames(six_tone_scale_colours[1:4], unique(plot_data$age_ranges)))  # Set fill scale

# Print the plot
print(mental_health_trajectory)

# Save the plot in different formats
ggsave("Output/Final Plots/PNG/mental_health_trajectory.png", plot = mental_health_trajectory + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
ggsave("Output/Final Plots/JPG/mental_health_trajectory.jpg", plot = mental_health_trajectory, width = 9, height = 3, dpi = 300)
ggsave("Output/Final Plots/SVG/mental_health_trajectory.svg", plot = mental_health_trajectory + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")


################################### MENTAL HEALTH COMPARISON PLOT ###################################

# Create plot_data_comparison by mutating and summarizing the data
plot_data_comparison <- data %>%
mutate(
  queb_franc = ifelse(is.na(quebec) | is.na(anglophone), NA, ifelse(quebec == 1 & anglophone == 0, 1, ifelse(quebec == 1 & anglophone == 1, 2, 0))),  # Create queb_franc variable
  young = ifelse(is.na(age_ranges), NA, age_ranges %in% c("15-29"))  # Create young variable
) %>%
drop_na(age_ranges, queb_franc) %>%  # Drop rows with NA in age_ranges or queb_franc
filter(young == 1) %>%  # Filter for young == 1
group_by(year, queb_franc) %>%  # Group by year and queb_franc
dplyr::summarize(
  average_mh = weighted.mean(mental_health, WGT, na.rm = TRUE),  # Calculate weighted mean of mental_health
  n = sum(WGT, na.rm = TRUE),  # Sum of weights
  var_mh = wtd.var(mental_health, weights = WGT, na.rm = TRUE),  # Calculate weighted variance of mental_health
  .groups = "drop"
) %>%
mutate(
  se_mh = sqrt(var_mh / n),  # Calculate standard error
  ci_lower = average_mh - qt(0.975, df = n - 1) * se_mh,  # Calculate lower bound of confidence interval
  ci_upper = average_mh + qt(0.975, df = n - 1) * se_mh  # Calculate upper bound of confidence interval
)

# Convert queb_franc to a factor with specific labels
plot_data_comparison$queb_franc <- factor(plot_data_comparison$queb_franc, levels = c(0, 1, 2), labels = c("Canadians\noutside Quebec", "Quebec Francophones", "Quebec Anglophones"))

# Calculate y-axis limits and breaks
min_y <- floor(min(plot_data_comparison$ci_lower, na.rm = TRUE) * 2) / 2
max_y <- ceiling(max(plot_data_comparison$ci_upper, na.rm = TRUE) * 2) / 2
y_breaks <- seq(min_y, max_y, by = 0.5)

# Create data for background rectangles
rect_data <- data.frame(ymin = head(y_breaks, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks, -1)[c(TRUE, FALSE)])

# Create the mental health comparison plot
mental_health_comparison <- ggplot() +
    geom_rect(data = rect_data, 
              aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
              fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +  # Add background rectangles
    geom_segment(aes(x = 2015, xend = 2015, y = min_y, yend = max_y), linetype = "dashed", color = "gray") +  # Add dashed line for 2015
    geom_segment(aes(x = 2022, xend = 2022, y = min_y, yend = max_y), linetype = "dashed", color = "gray") +  # Add dashed line for 2022
    geom_ribbon(data = plot_data_comparison %>% filter(year >= 2009 & year <= 2015), 
                aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = as.character(queb_franc)), alpha = 0.2, show.legend = TRUE) +  # Add confidence interval ribbons for 2009-2015
    geom_line(data = plot_data_comparison %>% filter(year >= 2009 & year <= 2015), 
              aes(x = year, y = average_mh, color = as.character(queb_franc)), show.legend = TRUE) +  # Add lines for 2009-2015
    geom_ribbon(data = plot_data_comparison %>% filter(year >= 2015 & year <= 2022), 
                aes(x = year, ymin = ci_lower, ymax = ci_upper, fill = as.character(queb_franc)), alpha = 0.2, show.legend = TRUE) +  # Add confidence interval ribbons for 2015-2022
    geom_line(data = plot_data_comparison %>% filter(year >= 2015 & year <= 2022), 
              aes(x = year, y = average_mh, color = as.character(queb_franc)), show.legend = TRUE) +  # Add lines for 2015-2022
    geom_point(data = plot_data_comparison, 
               aes(x = year, y = average_mh, color = as.character(queb_franc), shape = as.character(queb_franc)), show.legend = TRUE) +  # Add points
    labs(title = "Youth Mental Health in Quebec and the Rest of Canada",
         y = "Average Mental Health for Ages 15-29",
         color = "Group",
         fill = "Group",
         shape = "Group",
         caption = data_source_caption) +  # Add labels
    theme_chr() +
    theme(aspect.ratio = 1/3, plot.background = element_rect(fill = background_colour, color = background_colour), axis.title.y = element_text(angle = 90), legend.key.width = unit(1, "cm"), plot.caption = element_text(hjust = 0, size = 8, face = "italic")) +  # Adjusted legend key width
    scale_y_continuous(limits = c(min_y, max_y), breaks = y_breaks) +  # Set y-axis limits and breaks
    scale_x_continuous(limits = c(floor(min(plot_data_comparison$year, na.rm = TRUE)-0.01), ceiling(max(plot_data_comparison$year, na.rm = TRUE)+0.01)), breaks = seq(floor(min(plot_data_comparison$year, na.rm = TRUE)-0.01), ceiling(max(plot_data_comparison$year, na.rm = TRUE)+0.01), by = 1), labels = c(seq(floor(min(plot_data_comparison$year, na.rm = TRUE)-0.01), ceiling(max(plot_data_comparison$year, na.rm = TRUE)+0.01) - 1, by = 1), ""), 
                      expand = c(0, 0)) +  # Set x-axis limits and breaks
    scale_color_manual(values = setNames(six_tone_scale_colours[1:3], unique(plot_data_comparison$queb_franc))) +  # Set color scale
    scale_fill_manual(values = setNames(six_tone_scale_colours[1:3], unique(plot_data_comparison$queb_franc))) +  # Set fill scale
    scale_shape_manual(values = c(16, 17, 18))  # Set shape scale

# Print the plot
print(mental_health_comparison)

# Save the plot in different formats
ggsave(paste0("Output/Final Plots/PNG/mental_health_comparison_trajectory.png"), plot = mental_health_comparison + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
ggsave(paste0("Output/Final Plots/JPG/mental_health_comparison_trajectory.jpg"), plot = mental_health_comparison, width = 9, height = 3, dpi = 300)
ggsave(paste0("Output/Final Plots/SVG/mental_health_comparison_trajectory.svg"), plot = mental_health_comparison + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")


###########################################################################################################
############################################    APPENDIX    ###############################################
###########################################################################################################


###### Age-Standardized Provincial Map #######

data("PROV")

# Assuming 'provinces' is a dataframe with columns 'province' and 'ls'
# Calculate the weighted average of 'ls' for each province for the years 2009, 201718, and 2021
years_to_plot <- c(2009, 2018, 2021)

# Calculate the overall age makeup of Canada in 2021
overall_age_makeup <- province %>%
  filter(year == 2021 & age_ranges %in% c("15-29", "30-44", "45-59", "60+")) %>%
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

# Handle the special case for territories in 2021
territories_2021 <- tibble(
  province = c("Yukon", "Northwest Territories", "Nunavut"),
  year = 2021,
  weighted_avg_ls = NA_real_
)

# Combine the modified data
province_map_data <- province_map_data %>%
  filter(!(year == 2009 & province == "Yukon/Northwest Territories/Nunavut")) %>%
  bind_rows(territories_2009, territories_2021)

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
