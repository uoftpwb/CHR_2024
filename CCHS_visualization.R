# CCHS Visualizations for CHR 2024
# Author: Anthony McCanny
# Date: April 24, 2024
###############################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)





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
spectral_colors <- brewer.pal(length(ls_levels_ordered), "Spectral") %>% 
                   rev() %>% 
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
  scale_fill_manual(values = spectral_colors, name = "Life Satisfaction\nScore") +
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
  scale_fill_manual(values = spectral_colors, name = "Life Satisfaction\nScore") +
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
    scale_fill_viridis_d(option = "D", begin = 0, end = 1, direction = 1) +   # Apply a discrete spectral colour gradient to the bars with 11 values and remove the legend
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
    scale_fill_viridis_d(option = "D", begin = 0, end = 1, direction = 1) +   # Apply a discrete spectral colour gradient to the bars with 11 values and remove the legend
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
PUMF_list_age <- list.files(path = "Output/PROVxAGExLS Tables CCHS", pattern = "*.csv", full.names = TRUE)
provinceXagePUMF <- PUMF_list_age %>%
  purrr::map_dfr(~ read.csv(.x) %>%
    mutate(year = as.numeric(paste0("20", sub("Output/PROVxAGExLS Tables CCHS/PROVxAGExLS_(.*)_CCHS.csv", "\\1", .x)))))


# Read all the RTRA CSV files and combine them into one dataframe
RTRA_list <- list.files(path = "Data/CCHS RTRA", pattern = "^PROVxAGExLS.*\\.csv$", full.names = TRUE)
provinceXageRTRA <- RTRA_list %>%  # Start a chain of commands using the file_list vector
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
  drop_na(AGEGROUP) %>%
  select(province, year, age_ranges = AGEGROUP, ls, frequency = X_COUNT_)

# Combine PUMF and RTRA dataframes
provinceXage <- rbind(provinceXagePUMF, provinceXageRTRA)

canadaXage <- provinceXage %>%
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

canadaXage_avg_ls <- canadaXage %>%
  group_by(year, age_ranges) %>%
  mutate(proportion = frequency / sum(frequency)) %>%    
  mutate(average_ls = sum(proportion * as.numeric(ls))) %>%
  filter(age_ranges != "All ages") %>%
  mutate(display_year = case_when(
                          year <= 2023 ~ year,
                          year == 201516 ~ 2015.5,
                          year == 201718 ~ 2017.5,
                          year == 201920 ~ 2019.5,
                          TRUE ~ NA_integer_
  ))


# Create the line plot
 ggplot(canadaXage_avg_ls, aes(x = display_year, y = average_ls, group = age_ranges, color = age_ranges)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 7, ymax = 7.25, fill = "white", color = NA) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 7.25, ymax = 7.5, fill = "grey98", color = NA) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 7.5, ymax = 7.75, fill = "white", color = NA) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 7.75, ymax = 8, fill = "grey98", color = NA) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8, ymax = 8.25, fill = "white", color = NA) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.25, ymax = 8.5, fill = "grey98", color = NA) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2020.25, color = "#b41f1f", linetype = "solid", size = 0.5) +
  geom_text(aes(x = 2020.25, y = 7.4, label = "Pandemic Begins"), color = "#b41f1f", vjust = -0.5, hjust = 0.5, angle = 90, size = 4) +
  labs(title = "Average Canadian Life Satisfaction by Age Group",
       color = "Age Groups") +
  theme_minimal(base_family = "Helvetica") +
  theme(plot.title = element_text(size = rel(1.5), family = "Helvetica"),
        #axis.title.x = element_text(size = rel(1.2), family = "Helvetica"),
        #axis.title.y = element_text(size = rel(1.2), family = "Helvetica"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size = rel(1.2), family = "Helvetica"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        aspect.ratio = 1/3) +
  scale_y_continuous(limits = c(7.2, 8.4), breaks = seq(7.25, 8.4, by = 0.25)) +
  scale_x_continuous(breaks = seq(min(canadaXage_avg_ls$display_year, na.rm = TRUE), max(canadaXage_avg_ls$display_year, na.rm = TRUE), by = 1)) +
  scale_color_brewer(palette = "Set2", type = "qual")
# Print the plot
print(age_group_ls_plot)


age_group_ls_plot <- ggplot(canadaXage_avg_ls, aes(x = display_year, y = average_ls, group = age_ranges, color = age_ranges)) +
  geom_rect(data = data.frame(ymin = c(7.0, 7.25, 7.5, 7.75, 8, 8.25), ymax = c(7.25, 7.5, 7.75, 8, 8.25, 8.4)), 
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = factor(rep(c("white", "grey98"), 3))), 
            color = NA, inherit.aes = FALSE, show.legend = FALSE) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2020.25, color = "#b41f1f", linetype = "solid", size = 0.5) +
  geom_text(aes(x = 2020.25, y = 7.4, label = "Pandemic Begins"), color = "#b41f1f", vjust = -0.5, hjust = 0.5, angle = 90, size = 2.5) +
  labs(title = "Canadian Life Satisfaction by Age",
       y = "Average Life Satisfaction",
       color = "Age Groups") +
  theme_minimal(base_family = "Helvetica") +
  theme(plot.title = element_text(size = rel(1.2), family = "Helvetica"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = rel(1), family = "Helvetica", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        legend.title = element_text(size = rel(0.8), family = "Helvetica"),
        legend.text = element_text(size = rel(0.8), family = "Helvetica"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        aspect.ratio = 1/3) +
  scale_y_continuous(limits = c(7.25, 8.4), breaks = seq(7.25, 8.4, by = 0.25)) +
  scale_x_continuous(breaks = seq(min(canadaXage_avg_ls$display_year, na.rm = TRUE), max(canadaXage_avg_ls$display_year, na.rm = TRUE), by = 1)) +
  scale_color_brewer(palette = "Set2", type = "qual") +
  scale_fill_manual(values = c("white", "grey98"))

ggsave("Output/Plots/trajectory_by_age_group_Canada.png", plot = age_group_ls_plot, width = 9, height = 3, dpi = 300)
ggsave("Output/Plots/trajectory_by_age_group_Canada.jpg", plot = age_group_ls_plot, width = 9, height = 3, dpi = 300)
ggsave("Output/Plots/trajectory_by_age_group_Canada.svg", plot = age_group_ls_plot, width = 9, height = 3, dpi = 300)


########### VISUALIZATIONS FOR TABLES GROUPED BY PROVINCE and AGE - CSS ###########

## Import CSS Data

