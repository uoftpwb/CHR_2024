# CCHS Visualizations for CHR 2024
# Author: Anthony McCanny
# Date: April 24, 2024
###############################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)





########### VISUALIZATIONS FOR TABLES GROUPED BY PROVINCE  ###########

# Read all the CSV files and combine them into one dataframe
file_list <- list.files(path = "Output/province_tables", pattern = "*.csv", full.names = TRUE)
province <- do.call(rbind, lapply(file_list, function(file) {
  year_value <- sub("Output/province_tables/province_(.*).csv", "\\1", file)
  graphic_year_value <- ifelse(nchar(as.character(year_value)) == 4, year_value, 
                               ifelse(year_value == "20152016", 2015.5, 
                                      ifelse(year_value == "20172018", 2017.5, NA)))
  read.csv(file) %>%
    mutate(year = year_value, graphic_year = graphic_year_value)
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
