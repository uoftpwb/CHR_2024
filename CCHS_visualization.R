# CCHS Visualizations for CHR 2024
# Author: Anthony McCanny
# Date: April 24, 2024
###############################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)





########### VISUALIZATIONS FOR TABLES GROUPED BY PROVINCE  ###########

# Read all the PUMF CSV files and combine them into one dataframe
PUMF_list <- list.files(path = "Output/PROVxLS Tables CCHS", pattern = "*.csv", full.names = TRUE)
provincePUMF <- PUMF_list %>%
  purrr::map_dfr(~ read.csv(.x) %>%
  mutate(year = as.numeric(paste0("20", sub("Output/PROVxLS Tables CCHS/PROVxLS_(.*)_CCHS.csv", "\\1", .x))))) %>%
  rename(frequency = weighted_freq) %>%
  bind_rows(
      group_by(., year, ls) %>%
      summarise(frequency = sum(frequency, na.rm = TRUE), .groups = 'drop') %>%
      mutate(province = "Canada")
  )

# Read all the RTRA CSV files and combine them into one dataframe
file_list <- list.files(path = "Data/CCHS RTRA", pattern = "^PROVxAGExLS.*\\.csv$", full.names = TRUE)
provinceRTRA <- file_list %>%  # Start a chain of commands using the file_list vector
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
    AGEGROUP == ".toless15" ~ "15-29",
    AGEGROUP == "15toless3" ~ "30-44",
    AGEGROUP == "30toless4" ~ "45-60",
    AGEGROUP == "45toless6" ~ "60+",
    TRUE ~ "All ages")) %>%
  select(province, year, age = AGEGROUP, ls, frequency = X_COUNT_)





for (i in seq_along(file_list)) {
  assign(paste0("cchs_", gsub("Data/CCHS RTRA/PROVxAGExLS_(.*)_CCHS.csv", "\\1", file_list[i])), read.csv(file_list[i]))
}
# Compare the names of cchs_1920, cchs_21, and cchs_22 and check for non-matches
cchs_1920_columns <- names(cchs_1920)
cchs_21_columns <- names(cchs_21)
cchs_22_columns <- names(cchs_22)

# Find common columns
common_columns <- Reduce(intersect, list(cchs_1920_columns, cchs_21_columns, cchs_22_columns))

# Find non-matching columns for each dataframe
non_matching_1920 <- setdiff(cchs_1920_columns, common_columns)
non_matching_21 <- setdiff(cchs_21_columns, common_columns)
non_matching_22 <- setdiff(cchs_22_columns, common_columns)

# Print non-matching columns
print("Non-matching columns in cchs_1920:")
print(non_matching_1920)
print("Non-matching columns in cchs_21:")
print(non_matching_21)
print("Non-matching columns in cchs_22:")
print(non_matching_22)




provinceRTRA <- do.call(rbind, lapply(file_list, function(file) {
  year_value <- paste0("20", sub("Data/CCHS RTRA/PROVxAGExLS_(.*)_CCHS.csv", "\\1", file))
  graphic_year_value <- ifelse(nchar(as.character(year_value)) == 4, as.numeric(year_value), 
                               ifelse(year_value == "201920", 2019.5))
  read.csv(file) %>%
    mutate(year = year_value, graphic_year = as.numeric(graphic_year_value))
}))




##### Visualization for country-wide life satisfaction over time

# Aggregating data for all provinces to get Canada-wide frequencies for each LS value
canada <- province %>%
  group_by(year, ls) %>%
  summarise(total_weighted_freq = sum(weighted_freq, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(year, desc(ls))



ggplot(canada, aes(x = as.factor(ls), y = total_weighted_freq, fill = factor(year), group = factor(year))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
    scale_fill_viridis_d() +
    labs(title = "Ridge-style Distribution of Life Satisfaction (ls) Over Years",
        x = "Life Satisfaction Score",
        y = "Total Weighted Frequency",
        fill = "Year") +
    theme_minimal() +
    theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    facet_wrap(~year, scales = "free_y", ncol = 1)



# Define ordered life satisfaction levels
ls_levels_ordered <- c("10", "9", "8", "7", "6", "5", "4", "3", "2", "1", "0")

# Create a color palette and assign to life satisfaction levels
spectral_colors <- brewer.pal(length(ls_levels_ordered), "Spectral") %>% 
                   rev() %>% 
                   setNames(ls_levels_ordered)

# Update 'ls' column in 'canada' data frame to ordered factor
canada$ls <- factor(canada$ls, levels = ls_levels_ordered)

# Calculate proportions and cumulative sums for each year and life satisfaction level
canada <- canada %>%
  group_by(year) %>%
  mutate(proportion = total_weighted_freq / sum(total_weighted_freq)) %>%
  arrange(year, desc(ls)) %>%
  mutate(cumsum = cumsum(proportion),
         label_pos = cumsum - 0.5 * proportion)

# Prepare label data for life satisfaction levels 5 or below in the year 2013
label_data <- canada %>%
  filter(as.numeric(ls) <= 5, year == 2013)

# Construct the bar plot with filled bars and labels
ggplot(canada, aes(x = factor(year), y = total_weighted_freq, fill = ls)) +
  geom_bar(stat = "identity", position = "fill", width = 1) +
  scale_fill_manual(values = ls_colors, name = "Life Satisfaction\nScore") +
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


# Filter for 2017 and 2018 data from the provinces dataframe
province_20172018 <- province %>%
  filter(year %in% c("20172018"))

province_averages <- province_20172018 %>%
  group_by(province) %>%
  summarise(average_ls = weighted.mean(as.numeric(ls), weighted_freq, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(average_ls))

province_20172018 <- province_20172018 %>%
  left_join(province_averages, by = "province")
# Update 'ls' column in 'provinces_20172018' data frame to ordered factor
province_20172018$ls <- factor(province_20172018$ls, levels = ls_levels_ordered)

# Calculate proportions and cumulative sums for each province and life satisfaction level
provinces_20172018 <- provinces_20172018 %>%
  group_by(province) %>%
  mutate(proportion = weighted_freq / sum(weighted_freq)) %>%
  arrange(province, desc(ls)) %>%
  mutate(cumsum = cumsum(proportion),
         label_pos = cumsum - 0.5 * proportion)

# Prepare label data for life satisfaction levels 5 or below in the years 2017 and 2018
# and create a new column for text color based on life satisfaction levels
label_data_provinces <- provinces_20172018 %>%
  filter(as.numeric(ls) <=6, province == "Ontario") %>%
  mutate(text_color = ifelse(as.numeric(ls) %in% 5:6, "black", "white"))

# Construct the bar plot with filled bars for each province, ordered by average life satisfaction
ggplot(provinces_20172018, aes(x = reorder(factor(province), average_ls), y = weighted_freq, fill = ls)) +
  geom_bar(stat = "identity", position = "fill", width = 1) +
  scale_fill_manual(values = spectral_colors, name = "Life Satisfaction\nScore") +
  labs(title = "Distribution of Life Satisfaction Scores by Province, 2017-2018",
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

  ggsave("Output/Plots/province distributions.png", width = 8, height = 15, units = "in")
  ggsave("Output/Plots/province distributions.jpg", width = 8, height = 15, units = "in")


############### Animated Plots of Temporal Change in Distribtuions for Canada #########3

library(ggplot2)
library(gganimate)
library(gapminder)


canada <- canada %>%
    mutate(display_year = case_when(as.numeric(year) <= 2014 ~ as.numeric(year),
                                    year == "20152016" ~ 2016,
                                    year == "20172018" ~ 2018)) %>%
    group_by(year) %>%
    mutate(proportion = total_weighted_freq / sum(total_weighted_freq))


# Create the bar chart
canada_life_satisfaction_plot <- ggplot(canada, aes(x = factor(ls), y = proportion, fill = factor(ls))) +
  geom_bar(stat = "identity") +
  labs(title = 'Life Satisfaction in Canada (2009-2018)',
       x = 'Life Satisfaction Score',
       y = 'Proportion') +
  theme_minimal() +
  transition_time(display_year) +
  labs(subtitle = 'Year: {frame_time}') +
  ease_aes('linear')

canada_life_satisfaction_plot <- ggplot(canada, aes(x = as.factor(ls), y = proportion)) +
    geom_bar(stat = "identity", width = 1) +
    scale_fill_gradient(low = "lightblue", high = "yellow") +
    labs(title = "Canadian Life Satisfaction 2009-2018",
        x = "Life Satisfaction Score",
        y = "Proportion") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),  # Remove the box around the plot
          axis.text.x = element_text(size = rel(1.5)),
          plot.subtitle = element_text(size = rel(3), hjust = 0, vjust = -3)) +
    scale_y_continuous(labels = scales::percent_format(scale = 10), expand = c(0, 0)) +  # Remove space between 0% and the x-axis
    transition_time(display_year) +
    labs(subtitle = 'Year: {as.integer(frame_time)}') +
    ease_aes('linear')

# Create the animated plot
anim <- animate(canada_life_satisfaction_plot, nframes = 25 * (2018 - 2009), fps = 25, width = 800, height = 600, units = 'px')

# Save the animation
anim_save("Output/Plots/life_satisfaction_canada.gif", animation = anim)