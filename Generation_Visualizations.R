# Generation Visualizations for CHR 2024
# Author: Anthony McCanny
# Date: April 24, 2024
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


########### VISUALIZATIONS FOR TABLES GROUPED BY PROVINCE  ###########

## READ IN DATA

# Read all the PUMF CSV files and combine them into one dataframe
PUMF_list <- list.files(path = "Output/PROVxLS Tables CCHS", pattern = "*.csv", full.names = TRUE)
provincePUMF <- PUMF_list %>%
  purrr::map_dfr(~ read.csv(.x) %>%
    mutate(year = as.numeric(paste0("20", sub("Output/PROVxLS Tables CCHS/PROVxLS_(.*)_CCHS.csv", "\\1", .x)))))


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
    AGEGROUP == "45toless6" ~ "45-60",
    AGEGROUP == "60andup" ~ "60+",
    TRUE ~ "All ages")) %>%
  filter(AGEGROUP == "All ages") %>%
  select(province, year, ls, frequency = X_COUNT_)

# Combine PUMF and RTRA dataframes
province <- rbind(provincePUMF, provinceRTRA)

canada <- province %>%
  filter(province == "Canada")


##### Visualization for country-wide life satisfaction over time

# Define ordered life satisfaction levels
ls_levels_ordered <- c("10", "9", "8", "7", "6", "5", "4", "3", "2", "1", "0")

# Create a color palette and assign to life satisfaction levels
wellbeing_scale_separate <- wellbeing_scale_colours_separate %>%
                   setNames(ls_levels_ordered)

spectral_colors <- RColorBrewer::brewer.pal(11, "Spectral")[11:1] %>%
  setNames(ls_levels_ordered)

# Update 'ls' column in new 'canada_by_year' data frame to ordered factor
canada_by_year <- canada
canada_by_year$ls <- factor(canada_by_year$ls, levels = ls_levels_ordered)

# Calculate proportions and cumulative sums for each year and life satisfaction level
canada_by_year <- canada_by_year %>%
  group_by(year) %>%
  mutate(proportion = frequency / sum(frequency)) %>%
  arrange(year, desc(ls)) %>%
  mutate(cumsum = cumsum(proportion),
         label_pos = cumsum - 0.5 * proportion) %>%
  mutate(year_string = case_when(
                          year <= 2023 ~ as.character(year),
                          year == 201516 ~ "2015/2016",
                          year == 201718 ~ "2017/2018",
                          year == 201920 ~ "2019/2020",
                          TRUE ~ NA_character_
  ))

# Prepare label data for life satisfaction levels 5 or below in the year 2014
label_data <- canada_by_year %>%
  filter(as.numeric(ls) <= 5, year == 2014)

# Construct the bar plot with filled bars and labels
ggplot(canada_by_year, aes(x = factor(year_string), y = frequency, fill = ls)) +
  geom_bar(stat = "identity", position = "fill", width = 1) +
  scale_fill_manual(values = wellbeing_scale_separate, name = "Life Satisfaction\nScore") +
  labs(title = "Distribution of Life Satisfaction Scores in Canada, 2009-2018",
       x = "Year",
       y = "Proportion") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  geom_text(data = label_data, aes(label = ls, y = label_pos),
            size = 8, 
            color = "white")

ggsave("Output/Plots/year_distributions.png", width = 8, height = 15, units = "in")
ggsave("Output/Plots/year_distributions.jpg", width = 8, height = 15, units = "in")


###### Visualizations for province-specific life satisfaction distributions in 2017/2018

# Filter for 2019 and 2020 data from the provinces dataframe
province_20192020 <- province %>%
  filter(year == 201920 & province != "Canada")

province_averages <- province_20192020 %>%
  group_by(province) %>%
  summarise(average_ls = weighted.mean(as.numeric(ls), frequency, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(average_ls))

province_20192020 <- province_20192020 %>%
  left_join(province_averages, by = "province")
# Update 'ls' column in 'province_20192020' data frame to ordered factor
province_20192020$ls <- factor(province_20192020$ls, levels = ls_levels_ordered)

# Calculate proportions and cumulative sums for each province and life satisfaction level
provinces_20192020 <- province_20192020 %>%
  group_by(province) %>%
  mutate(proportion = frequency / sum(frequency)) %>%
  arrange(province, desc(ls)) %>%
  mutate(cumsum = cumsum(proportion),
         label_pos = cumsum - 0.5 * proportion)

# Prepare label data for life satisfaction levels 5 or below in the years 2019 and 2020
# and create a new column for text color based on life satisfaction levels
label_data_provinces <- provinces_20192020 %>%
  filter(as.numeric(ls) <=6, province == "Nunavut") %>%
  mutate(text_color = ifelse(as.numeric(ls) %in% 5:6, "black", "white"))

# Construct the bar plot with filled bars for each province, ordered by average life satisfaction
ggplot(provinces_20192020, aes(x = reorder(factor(province), average_ls), y = frequency, fill = ls)) +
  geom_bar(stat = "identity", position = "fill", width = 1) +
  scale_fill_manual(values = wellbeing_scale_separate, name = "Life Satisfaction\nScore") +
  labs(title = "Distribution of Life Satisfaction Scores by Province, 2019-2020",
       x = "Province",
       y = "Proportion") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = rel(2)),
        axis.title.x = element_text(size = rel(2)),
        axis.text.y = element_text(size = rel(2)),
        axis.title.y = element_text(size = rel(2)),
        axis.ticks = element_blank()) +  # Remove ticks above and below the plot
  geom_text(data = label_data_provinces, aes(label = ls, y = label_pos, color = text_color),
            size = 8) +
  scale_color_manual(values = c("black", "white"), guide = FALSE)  

  ggsave("Output/Plots/province_distributions_2019_2020.png", width = 8, height = 15, units = "in")
  ggsave("Output/Plots/province_distributions_2019_2020.jpg", width = 8, height = 15, units = "in")



############### Animated Plots of Temporal Change in Distribtuions for Canada #########3

library(gganimate)
library(gapminder)
library(grid)


canada_anim <- canada %>%
    mutate(display_year = case_when(as.numeric(year) <= 2023 ~ as.numeric(year),
                                    year == 201516 ~ 2016,
                                    year == 201718 ~ 2018,
                                    year == 201920 ~ 2020)) %>%
    group_by(year) %>%
    mutate(proportion = frequency / sum(frequency)) %>%
    ungroup() %>%
    # Calculate the weighted average life satisfaction for each year
    group_by(display_year) %>%
    mutate(average_ls = sum(proportion * as.numeric(ls)))

# Create the bar chart
canada_life_satisfaction_plot <- ggplot(canada_anim, aes(x = as.factor(ls), y = proportion, fill = as.factor(ls))) +
    geom_bar(stat = "identity", width = 1) +
    geom_vline(aes(xintercept = average_ls+1), color = "#602582", linetype = "solid", linewidth = 0.5) +
    #geom_text(aes(label = sprintf("Avg: %.2f", average_ls)), x = Inf, y = 1, vjust = -0.5, hjust = 1, color = "#602582", size = 6) +
    labs(title = "Canadian Life Satisfaction 2009-2022",
        x = "Life Satisfaction Score",
        y = "Proportion") +
    theme_minimal() +
    theme(title = element_text(size = rel(2)),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),  # Remove the box around the plot
          axis.text.x = element_text(size = rel(2)),
          axis.text.y = element_text(size = rel(2)),
          legend.position = "none") +  # Remove the legend
    scale_y_continuous(labels = scales::percent_format(scale = 100), expand = c(0, 0)) +   # Remove space between 0% and the x-axis
    scale_fill_manual(values = wellbeing_scale_colours) +
    transition_time(display_year) +
    labs(subtitle = 'Year: {as.integer(frame_time)}') +  # Use labs to add a subtitle that changes with the frame time
    ease_aes('linear')
#annotation_custom(grob = textGrob(as.character('{as.integer(frame_time)}'), gp = gpar(col = "black", fontsize = 80)), xmin = -4, xmax = Inf, ymin = 1, ymax = Inf) + 

# Create the animated plot
anim <- animate(canada_life_satisfaction_plot, nframes = 50 * (2022 - 2009), fps = 25, width = 800, height = 1000, units = 'px', end_pause = 100)

# Save the animation
anim_save("Output/Plots/animated_canada_plot.gif", animation = anim)



### ANIMATED PROVINCE PLOT

province_anim <- province %>%
    mutate(display_year = case_when(as.numeric(year) <= 2023 ~ as.numeric(year),
                                    year == 201516 ~ 2016,
                                    year == 201718 ~ 2018,
                                    year == 201920 ~ 2020)) %>%
    bind_rows(
      filter(., province %in% c("Yukon", "Northwest Territories", "Nunavut")) %>%
      group_by(year, display_year, ls) %>%
      summarize(frequency = sum(frequency, na.rm = TRUE), .groups = 'drop') %>%
      mutate(province = "Yukon/Northwest Territories/Nunavut")
    ) %>%
    filter(!province %in% c("Yukon", "Northwest Territories", "Nunavut")) %>%
    bind_rows(
      filter(., province == "Yukon/Northwest Territories/Nunavut" & year== 201920) %>%
      mutate(year = 2021, display_year = 2021) %>%
      bind_rows(
        mutate(., year = 2022, display_year = 2022)
      )
    ) %>%
    group_by(display_year, province) %>%
    mutate(proportion = frequency / sum(frequency)) %>%    
    mutate(average_ls = sum(proportion * as.numeric(ls))) %>%
    ungroup() %>%
    arrange(year, province, ls)

test <- province_anim %>% filter(province == "Yukon/Northwest Territories/Nunavut")

# Create the bar chart
all_provinces_plot <- ggplot(province_anim, aes(x = as.factor(ls), y = proportion, fill = as.factor(ls))) +
    geom_bar(stat = "identity", width = 1) +
    geom_vline(aes(xintercept = average_ls+1), color = "#602582", linetype = "solid", linewidth = 0.5) +
    labs(title = "Canadian Life Satisfaction 2009-2022",
        x = "Life Satisfaction Score",
        y = "Proportion") +
    theme_minimal() +
    theme(title = element_text(size = rel(2)),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),  # Remove the box around the plot
          axis.text.x = element_text(size = rel(2)),
          axis.text.y = element_text(size = rel(2)),
          legend.position = "none") +  # Remove the legend
    scale_y_continuous(labels = scales::percent_format(scale = 100), expand = c(0, 0)) +   # Remove space between 0% and the x-axis
    scale_fill_manual(values = wellbeing_scale_colours) +
    facet_wrap(~province, ncol = 3) +  # Tiled layout with two identical plots side by side
    transition_time(display_year) +
    labs(subtitle = 'Year: {as.integer(frame_time)}') +  # Use labs to add a subtitle that changes with the frame time
    ease_aes('linear')

# Create the animated plot
all_prov_anim <- animate(all_provinces_plot, nframes = 50 * (2022 - 2009), fps = 25, width = 800, height = 1000, units = 'px', end_pause = 100)

# Save the animation
anim_save("Output/Plots/animated_provinces_plot.gif", animation = all_prov_anim)






########### VISUALIZATIONS FOR TABLES GROUPED BY PROVINCE and AGE CCHS ###########

## Import Data

# Read all the PUMF CSV files and combine them into one dataframe
PUMF_list_generation <- list.files(path = "Output/PROVxGENERATIONxLS Tables CCHS", pattern = "*.csv", full.names = TRUE)
provinceXgenerationPUMF <- PUMF_list_generation %>%
  purrr::map_dfr(~ read.csv(.x) %>%
    mutate(year = as.numeric(paste0("20", sub("Output/PROVxGENERATIONxLS Tables CCHS/PROVxGENERATIONxLS_(.*)_CCHS.csv", "\\1", .x)))))

# Read all the RTRA CSV files and combine them into one dataframe
RTRA_list <- list.files(path = "Data/CCHS RTRA", pattern = "^PROVxAGExLS.*\\.csv$", full.names = TRUE)
provinceXgenerationRTRA <- RTRA_list %>%  # Start a chain of commands using the file_list vector
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
  mutate(generation = case_when(
    AGEGROUP == ".toless15" ~ "Generation Z",
    AGEGROUP == "15toless3" ~ "Generation Z",
    AGEGROUP == "30toless4" ~ "Millennials",
    AGEGROUP == "45toless6" ~ "Generation X",
    AGEGROUP == "60andup" ~ "Baby Boomers",
    TRUE ~ "All generations")) %>%
  select(province, year, generation, ls, frequency = X_COUNT_)

# Combine PUMF and RTRA dataframes
provinceXgenerationCCHS <- rbind(provinceXgenerationPUMF, provinceXgenerationRTRA)

canadaXgenerationCCHS <- provinceXgenerationCCHS %>%
  filter(province == "Canada")



##################### Canada-wide age-group animated plots over time #####################

# Prepare the data for animation
canadaXage_anim <- canadaXage %>%
  mutate(display_year = case_when(as.numeric(year) <= 2023 ~ as.numeric(year),
                                  year == 201516 ~ 2016,
                                  year == 201718 ~ 2018,
                                  year == 201920 ~ 2020)) %>%
  group_by(display_year, age_ranges) %>%
  mutate(proportion = frequency / sum(frequency)) %>%    
  mutate(average_ls = sum(proportion * as.numeric(ls))) %>%
  ungroup() %>%
  arrange(year, age_ranges, ls)

# Create the bar chart
age_ranges_plot <- ggplot(canadaXage_anim, aes(x = as.factor(ls), y = proportion, fill = as.factor(ls))) +
  geom_bar(stat = "identity", width = 1) +
  geom_vline(aes(xintercept = average_ls+1), color = "#602582", linetype = "solid", linewidth = 0.5) +
  labs(title = "Canadian Life Satisfaction by Age Range 2009-2022",
      x = "Life Satisfaction Score",
      y = "Proportion") +
  theme_minimal() +
  theme(title = element_text(size = rel(2)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),  # Remove the box around the plot
        axis.text.x = element_text(size = rel(2)),
        axis.text.y = element_text(size = rel(2)),
        legend.position = "none") +  # Remove the legend
  scale_y_continuous(labels = scales::percent_format(scale = 100), expand = c(0, 0)) +   # Remove space between 0% and the x-axis
  scale_fill_viridis_d(option = "D", begin = 0, end = 1, direction = 1) +   # Apply a discrete spectral colour gradient to the bars with 11 values and remove the legend
  facet_wrap(~age_ranges, ncol = 3) +  # Tiled layout with one plot per age range
  transition_time(display_year) +
  labs(subtitle = 'Year: {as.integer(frame_time)}') +  # Use labs to add a subtitle that changes with the frame time
  ease_aes('linear')

# Create the animated plot
age_ranges_anim <- animate(age_ranges_plot, nframes = 50 * (2022 - 2009), fps = 25, width = 800, height = 1000, units = 'px', end_pause = 100)

# Save the animation
anim_save("Output/Plots/animated_age_ranges_plot.gif", animation = age_ranges_anim)


########## Youth Satisfaction Across Provinces, Animated Over Time ############

province_youth_anim <- provinceXage %>%
    filter(age_ranges == "15-29") %>%
    mutate(display_year = case_when(as.numeric(year) <= 2023 ~ as.numeric(year),
                                    year == 201516 ~ 2016,
                                    year == 201718 ~ 2018,
                                    year == 201920 ~ 2020)) %>%
    bind_rows(
      filter(., province %in% c("Yukon", "Northwest Territories", "Nunavut")) %>%
      group_by(year, display_year, ls) %>%
      summarize(frequency = sum(frequency, na.rm = TRUE), .groups = 'drop') %>%
      mutate(province = "Yukon/Northwest Territories/Nunavut")
    ) %>%
    filter(!province %in% c("Yukon", "Northwest Territories", "Nunavut")) %>%
    bind_rows(
      filter(., province == "Yukon/Northwest Territories/Nunavut" & year== 201920) %>%
      mutate(year = 2021, display_year = 2021) %>%
      bind_rows(
        mutate(., year = 2022, display_year = 2022)
      )
    ) %>%
    group_by(display_year, province) %>%
    mutate(proportion = frequency / sum(frequency)) %>%    
    mutate(average_ls = sum(proportion * as.numeric(ls))) %>%
    ungroup() %>%
    select(-age_ranges) %>%
    arrange(year, province, ls)

# Create the bar chart
all_provinces_plot_youth <- ggplot(province_youth_anim, aes(x = as.factor(ls), y = proportion, fill = as.factor(ls))) +
    geom_bar(stat = "identity", width = 1) +
    geom_vline(aes(xintercept = average_ls+1), color = "#602582", linetype = "solid", linewidth = 0.5) +
    labs(title = "Canadian Life Satisfaction 2009-2022",
        x = "Life Satisfaction Score",
        y = "Proportion") +
    theme_minimal() +
    theme(title = element_text(size = rel(2)),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),  # Remove the box around the plot
          axis.text.x = element_text(size = rel(2)),
          axis.text.y = element_text(size = rel(2)),
          legend.position = "none") +  # Remove the legend
    scale_y_continuous(labels = scales::percent_format(scale = 100), expand = c(0, 0)) +   # Remove space between 0% and the x-axis
    scale_fill_viridis_d(option = "D", begin = 0, end = 1, direction = 1) +   # Apply a discrete spectral colour gradient to the bars with 11 values and remove the legend
    facet_wrap(~province, ncol = 3) +  # Tiled layout with two identical plots side by side
    transition_time(display_year) +
    labs(subtitle = 'Year: {as.integer(frame_time)}') +  # Use labs to add a subtitle that changes with the frame time
    ease_aes('linear')

# Create the animated plot
all_prov_youth_anim <- animate(all_provinces_plot_youth, nframes = 50 * (2022 - 2009), fps = 25, width = 800, height = 1000, units = 'px', end_pause = 100)

# Save the animation
anim_save("Output/Plots/animated_provinces_youth_plot.gif", animation = all_prov_youth_anim)




###################### Canada-wide Trajectory Plots by Age Group ########################

canadaXgenerationCCHS_avg_ls <- canadaXgenerationCCHS %>%
  group_by(year, generation) %>%
  mutate(proportion = frequency / sum(frequency)) %>%    
  mutate(average_ls = sum(proportion * as.numeric(ls))) %>%
  filter(generation != "All generations") %>%
  mutate(display_year = case_when(
                          year <= 2023 ~ year,
                          year == 201516 ~ 2015.5,
                          year == 201718 ~ 2017.5,
                          year == 201920 ~ 2019.5,
                          TRUE ~ NA_integer_
  ))

min_y <- 7.25
max_y <- 8.75
y_breaks <- seq(min_y, max_y, by = 0.25)
rect_data <- data.frame(ymin = head(y_breaks, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks, -1)[c(TRUE, FALSE)])

generation_ls_plot <- ggplot(canadaXgenerationCCHS_avg_ls, aes(x = display_year, y = average_ls, group = generation, color = generation)) +
  geom_rect(data = rect_data, 
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
            fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2020.25, color = "#b41f1f", linetype = "solid", size = 0.5) +
  geom_text(aes(x = 2020.25, y = 7.4, label = "Pandemic Begins"), color = "#b41f1f", vjust = -0.5, hjust = 0.5, angle = 90, size = 2.5) +
  labs(title = "Canadian Life Satisfaction by Generation (from CCHS Data)",
       y = "Average Life Satisfaction",
       color = "Generations") +
  theme_chr() + 
  theme(aspect.ratio = 1/3, axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0), hjust = 0.5)) +
  scale_y_continuous(limits = c(min_y, max_y), breaks = y_breaks) +
  scale_x_continuous(breaks = seq(min(canadaXgenerationCCHS_avg_ls$display_year, na.rm = TRUE), max(canadaXgenerationCCHS_avg_ls$display_year, na.rm = TRUE), by = 1)) +
  scale_color_manual(values = six_tone_scale_colours[1:5], 
                     breaks = c("Silent Generation", "Baby Boomers", "Generation X", "Millennials", "Generation Z"))

ggsave("Output/Plots/trajectory_by_generation_Canada_CCHS.png", plot = generation_ls_plot + theme_void() + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
ggsave("Output/Plots/trajectory_by_generation_Canada_CCHS.svg", plot = generation_ls_plot + theme_void() + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
ggsave("Output/Plots/trajectory_by_generation_Canada_CCHS.jpg", plot = generation_ls_plot, width = 9, height = 3, dpi = 300)

########### VISUALIZATIONS FOR TABLES GROUPED BY PROVINCE and AGE - CSS ###########

## Import CSS Data

CSS_list <- list.files(path = "Data/CSS RTRA", pattern = "^PROVxAGExLS.*\\.csv$", full.names = TRUE)
provinceXgenerationCSS <- CSS_list %>%  # Start a chain of commands using the file_list vector
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
  drop_na(AGEGROUP) %>%
  mutate(generation = case_when(
    AGEGROUP == 2 ~ "Generation Z",
    AGEGROUP == 3 ~ "Millennials",
    AGEGROUP == 4 ~ "Generation X",
    AGEGROUP == 5 ~ "Baby Boomers",
    TRUE ~ "Generation Z")) %>%
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
  select(province, year, generation, ls, frequency = X_COUNT_)

## GENERATE THE PLOT

# Format data
canadaXgenerationCSS_avg_ls <- provinceXgenerationCSS %>%
  filter(province == "Canada") %>%
  group_by(year, generation) %>%
  mutate(proportion = frequency / sum(frequency)) %>%    
  mutate(average_ls = sum(proportion * as.numeric(ls))) %>%
  filter(generation != "All generations") 


# Custom function to convert decimal years to year and month format
decimalYearToLabel <- function(years) {
  print(years)
  sapply(years, function(year) {
    if(is.na(year) || year == ""){
      return(" ")
    } else {
      base_year <- floor(year)
      month_num <- floor((year - base_year) * 12)
      if (month_num == 0) {
        month_num <- 12
        base_year <- base_year - 1
      }
      month_label <- format(as.Date(paste0(base_year, "-", month_num, "-01")), "%b")
      return(paste0(month_label, " ", base_year))
    }
  })
}


# Create the line plot
min_y <- 6.5
max_y <- 8.0
y_breaks <- seq(min_y, max_y, by = 0.25)
rect_data <- data.frame(ymin = head(y_breaks, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks, -1)[c(TRUE, FALSE)])

generation_ls_plot_CSS <- ggplot(canadaXgenerationCSS_avg_ls, aes(x = year, y = average_ls, group = generation, color = generation)) +
  geom_rect(data = rect_data, 
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
            fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
  geom_line() +
  geom_point() +
  labs(title = "Canadian Life Satisfaction by Age",
       y = "Average Life Satisfaction",
       color = "Age Groups") +
  theme_chr() +
  theme(aspect.ratio = 1/3, plot.background = element_rect(fill = background_colour, color = NA)) +
  scale_y_continuous(limits = c(min_y, max_y), breaks = y_breaks) +
  scale_x_continuous(breaks = seq(floor(min(canadaXageCSS_avg_ls$year)), ceiling(max(canadaXageCSS_avg_ls$year)), by = 0.5),
                    labels = decimalYearToLabel) +
  scale_color_manual(values = six_tone_scale_colours[1:5], 
                     breaks = c("Silent Generation", "Baby Boomers", "Generation X", "Millennials", "Generation Z"))

# Save the plot
ggsave("Output/Plots/trajectory_by_generation_Canada_CSS.png", plot = generation_ls_plot_CSS + theme_void() + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
ggsave("Output/Plots/trajectory_by_generation_Canada_CSS.svg", plot = generation_ls_plot_CSS  + theme_void() + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
ggsave("Output/Plots/trajectory_by_generation_Canada_CSS.jpg", plot = generation_ls_plot_CSS , width = 9, height = 3, dpi = 300)

############## GALLUP DATA ####################

gallup_data_raw <- readRDS("Data/Gallup/GWP_cleaned_CHR2024_240430.rds") %>%
  filter(COUNTRYNEW == "Canada") %>%
  select(year = YEAR_WAVE, ls = WP16, age = WP1220, WGT, date = WP4, province = REGION2_CAN) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  group_by(year) %>%
  mutate(average_date = mean(date, na.rm = TRUE)) %>%
  mutate(average_date = case_when(
    year <= 2007 ~ as.Date(paste(year, "-07-01", sep="")),
    TRUE ~ average_date
  )) %>%
  mutate(birth_year = year - age,
          generation = case_when(
            birth_year <= 1945 ~ "Silent Generation",
            birth_year >= 1946 & birth_year <= 1964 ~ "Baby Boomers",
            birth_year >= 1965 & birth_year <= 1980 ~ "Generation X",
            birth_year >= 1981 & birth_year <= 1996 ~ "Millennials",
            birth_year >= 1997 & birth_year <= 2012 ~ "Generation Z",
            TRUE ~ NA_character_),
          year = as.numeric(format(average_date, "%Y")) + (as.numeric(format(average_date, "%j")) - 1) / 365)
  
gallup <- gallup_data_raw %>%
  group_by(year, generation) %>%
  summarize(average_ls = weighted.mean(ls, WGT, na.rm=TRUE)) %>%
  drop_na(generation)

min_y <- 6.0
max_y <- 8.1
y_breaks <- seq(min_y, max_y, by = 0.5)
rect_data <- data.frame(ymin = head(y_breaks, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks, -1)[c(TRUE, FALSE)])


generation_ls_plot_gallup <- ggplot(gallup, aes(x = year, y = average_ls, group = generation, color = generation)) +
  geom_rect(data = rect_data, 
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
            fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2020.25, color = "#b41f1f", linetype = "solid", size = 0.5) +
  geom_text(aes(x = 2020.25, y = 6.0, label = "Pandemic Begins"), color = "#b41f1f", vjust = -0.5, hjust = 0, angle = 90, size = 2.5) +
  labs(title = "Canadian Life Satisfaction by Generation (Gallup)",
       y = "Average Life Satisfaction",
       color = "Age Groups") +
  theme_chr() +
  theme(aspect.ratio = 1/3, plot.background = element_rect(fill = background_colour, color = NA)) +
  scale_y_continuous(limits = c(min_y, max_y), breaks = y_breaks) +
  scale_x_continuous(limits = c(2006, 2024), breaks = seq(2006, 2024, by = 1), labels = c(seq(2006, 2023, by = 1), ""), 
                     expand = c(0, 0)) +
  scale_color_manual(values = six_tone_scale_colours[1:5], 
                     breaks = c("Silent Generation", "Baby Boomers", "Generation X", "Millennials", "Generation Z"))
print(generation_ls_plot_gallup)

ggsave("Output/Plots/trajectory_by_generation_Canada_Gallup.png", plot = generation_ls_plot_gallup + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
ggsave("Output/Plots/trajectory_by_generation_Canada_Gallup.svg", plot = generation_ls_plot_gallup + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
ggsave("Output/Plots/trajectory_by_generation_Canada_Gallup.jpg", plot = generation_ls_plot_gallup , width = 9, height = 3, dpi = 300)
############ COMBINATION of CCHS, CSS and GALLUP DATA - TRAJECTORIES by AGE GROUP ###################

canadaXgenerationCCHS_avg_ls <- canadaXgenerationCCHS %>%
  group_by(year, generation) %>%
  mutate(proportion = frequency / sum(frequency)) %>%    
  mutate(average_ls = sum(proportion * as.numeric(ls))) %>%
  filter(generation != "All generations") %>%
  mutate(year = case_when(
                          year <= 2023 ~ year + 0.5,
                          year == 201516 ~ 2016,
                          year == 201718 ~ 2018,
                          year == 201920 ~ 2020,
                          TRUE ~ NA_integer_
  ))

canadaXgenerationCSS_avg_ls <- provinceXgenerationCSS %>%
  filter(province == "Canada") %>%
  group_by(year, generation) %>%
  mutate(proportion = frequency / sum(frequency)) %>%    
  mutate(average_ls = sum(proportion * as.numeric(ls))) %>%
  filter(generation != "All generations") %>%
  mutate(year = year-0.125)

gallup <- gallup_data_raw %>%
  group_by(year, generation) %>%
  summarize(average_ls = weighted.mean(ls, WGT, na.rm=TRUE)) %>%
  drop_na(generation)

# Combine the datasets for the two sets of lines
combined_data <- canadaXgenerationCCHS_avg_ls %>%
  full_join(canadaXgenerationCSS_avg_ls, by = c("year", "generation")) %>%
  full_join(gallup, by = c("year", "generation")) %>%
  rename(average_ls_CCHS = average_ls.x, average_ls_CSS = average_ls.y, average_ls_Gallup = average_ls) %>%
  group_by(year, generation) %>%
  summarize(average_ls_CCHS = first(na.omit(average_ls_CCHS)), 
            average_ls_CSS = first(na.omit(average_ls_CSS)), 
            average_ls_Gallup = first(na.omit(average_ls_Gallup)))


# Create the combined line plot



min_y <- 6.0
max_y <- 8.75
y_breaks <- seq(min_y, max_y, by = 0.5)
rect_data <- data.frame(ymin = head(y_breaks, -1)[c(TRUE, FALSE)], ymax = tail(y_breaks, -1)[c(TRUE, FALSE)])


# Filter the data to remove rows with NA values for average_ls_CCHS
CCHS_data_filtered <- combined_data %>%
  filter(!is.na(average_ls_CCHS))

Gallup_data_filtered <- combined_data %>%
  filter(!is.na(average_ls_Gallup))

CSS_data_filtered <- combined_data %>%
  filter(!is.na(average_ls_CSS))

label_at <- function(n) function(x) ifelse(x %% n == 0, "", as.numeric(x)-0.5)


generation_ls_plot_combined <- ggplot() +
  geom_rect(data = rect_data, 
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
            fill = accent_background_colour, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
  geom_line(data = CCHS_data_filtered, aes(x = year, y = average_ls_CCHS, group = generation, color = generation, linetype = "CCHS")) +
  geom_line(data = CSS_data_filtered, aes(x = year, y = average_ls_CSS, group = generation, color = generation, linetype = "CSS")) +
  geom_line(data = Gallup_data_filtered, aes(x = year, y = average_ls_Gallup, group = generation, color = generation, linetype = "Gallup")) +
  labs(title = "Canadian Life Satisfaction by Generation",
       y = "Average Life Satisfaction",
       color = "Generations",
       linetype = "Data Source") +
  theme_chr() +
  theme(aspect.ratio = 1/3) + 
  scale_y_continuous(limits = c(min_y, max_y), breaks = y_breaks) +
  scale_x_continuous(limits = c(2006, 2024), breaks = seq(2006, 2024, by = 1), labels = c(seq(2006, 2023, by = 1), ""),  # Set minor breaks halfway between the labels
                     expand = c(0, 0)) +
  scale_color_manual(values = six_tone_scale_colours[1:5], 
                     breaks = c("Silent Generation", "Baby Boomers", "Generation X", "Millennials", "Generation Z")) +
  scale_linetype_manual(values = c("CCHS" = "solid", "CSS" = "longdash", "Gallup" = "twodash"))



# Print the combined line plot
print(generation_ls_plot_combined)

ggsave("Output/Plots/trajectory_by_generation_Canada_combined.png", plot = generation_ls_plot_combined + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
ggsave("Output/Plots/trajectory_by_generation_Canada_combined.svg", plot = generation_ls_plot_combined + theme(plot.background = element_rect(fill = "transparent", color = NA)), width = 9, height = 3, dpi = 300, bg = "transparent")
ggsave("Output/Plots/trajectory_by_generation_Canada_combined.jpg", plot = generation_ls_plot_combined, width = 9, height = 3, dpi = 300)
