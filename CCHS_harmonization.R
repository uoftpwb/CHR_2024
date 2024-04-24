# CCHS Analyses for Canadian Happiness Report
# Author: Phyllis Lun and Anthony McCanny
# Date: April 23, 2024
###############################################

#Loading libraries and setting directory -----------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(jtools)
library(patchwork)
library(ggbump)
library(tidyr)
library(cchsflow)

setwd("~/Documents/*UoT/PWB/YMH/CCHS") # Change this to your own directory

Reading CCHS data----------
# Set the path to the directory containing the CSV files
directory <- "Data"

# Get the list of CSV files in the directory
csv_files <- list.files(path = directory, pattern = "\\.csv$", full.names = TRUE)

# Read all CSV files into a list of data frames with names based on the file names
data_frames <- setNames(lapply(csv_files, read.csv), sapply(csv_files, function(f) {
  file_name <- tools::file_path_sans_ext(basename(f))
  gsub("-", "_", file_name) # Replace hyphens with underscores for valid R variable names
}))

# Assign each data frame from the list to a separate variable in the global environment
list2env(data_frames, envir = .GlobalEnv)

# # Get the list of CSV files in the directory
# csv_files <- list.files(path = directory, pattern = "\\.csv$", full.names = TRUE)

# # Loop through each file and read it into a data frame
# for (file in csv_files) {
#   # Extract the file name without the extension
#   file_name <- tools::file_path_sans_ext(basename(file))
  
#   # Read the CSV file into a data frame
#   df <- read.csv(file)
  
#   # Assign the data frame to a variable with the file name as the prefix
#   assign(file_name, df, envir = .GlobalEnv)
# }

cchs2001<-`cchs-pumf-2001`
cchs2003<-`cchs-pumf-2003`
cchs2005<-`cchs-pumf-2005`
cchs2007_2008<-`cchs-pumf-2007-2008`
cchs2009_2010<-`cchs-pumf-2009-2010`
cchs2011_2012<-`cchs-pumf-2011-2012`
cchs2013_2014<-`cchs-pumf-2013-2014`
cchs2015_2016<-`cchs-pumf-2015-2016`
cchs2017_2018<-`cchs-pumf-2017-2018`

rm(`cchs-pumf-2001`, `cchs-pumf-2003`,`cchs-pumf-2005`,`cchs-pumf-2007_2008`,`cchs-pumf-2009-2010`,
   `cchs-pumf-2011-2012`,`cchs-pumf-2013-2014`,`cchs-pumf-2015-2016`,`cchs-pumf-2017-2018`)

save.image("CCHS_harmonization.RData") 
load("CCHS_harmonization.RData")


#Transform variables into harmonized versions------------

##Province: GEO_PRV----
GEO_PRV_MAPPING <- tibble(
  PRV_NAME = c(
    "Newfoundland and Labrador",
    "Prince Edward Island",
    "Nova Scotia",
    "New Brunswick",
    "Quebec",
    "Ontario",
    "Manitoba",
    "Saskatchewan",
    "Alberta",
    "British Columbia",
    "Yukon",
    "Northwest Territories",
    "Nunavut"),
  PRV_NUM = c(10, 11, 12, 13, 24, 35, 46, 47, 48, 59, 60, 61,62))

### Used in 2013-2014
GEO_PRV_MAPPING_2 <- tibble(
  PRV_NAME = c(
    "Newfoundland and Labrador",
    "Prince Edward Island",
    "Nova Scotia",
    "New Brunswick",
    "Quebec",
    "Ontario",
    "Manitoba",
    "Saskatchewan",
    "Alberta",
    "British Columbia",
    "Yukon/Northwest Territories/Nunavut"),
  PRV_NUM = c(10, 11, 12, 13, 24, 35, 46, 47, 48, 59, 60))

## Age: DHHGAGE
DHHGAGE_mapping <- tibble(
  DHHGAGE_NAME = c(
    "12-14", "15-17", "18-19", "20-24", "25-29", "30-34",
    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
    "65-69", "70-74", "75-79", "80+"),
  DHHGAGE_NUM = c(1:16))

##Marital Status: DHHGMS
INCDGHH_mapping <- tibble(
  INCDGHH_NAME = c(
    "≤$20,000", "$20,000-$39,999", "$40,000-$59,999",
    "$60,000-$79,999", "$80,000 or more", NA_character_),
  INCDGHH_NUM = c(1:5, 9))

#CCHS_2017-2018----------
cchs2017_2018_extracted <- cchs_pumf_2017_2018 %>%
  select(
    ls = GEN_010,
    province = GEO_PRV,
    sex = DHH_SEX,
    age = DHHGAGE,
    marital = DHHGMS,
    hh_income = INCDGHH,
    hh_size = DHHDGHSZ,
    language = SDCDVFLS,
    immigration_status = SDCDVIMM,
    time_in_canada = SDCDGRES,
    mental_health = GEN_015,
    own_home = DHH_OWN,
    live_arrange = DHHDGLVG,
    indigenous = SDC_015,
    minority = SDCDGCGT,
    weight = WTS_M,
    sex_diversity = SDC_035,
    education = DHHDG611,
    health = GEN_005,
    live_stress = GEN_020,
    work_stress = GEN_025,
    belonging = GEN_030,
    sleep_hours = SLPG005,
    sleep_refreshing = SLP_015,
    smoke = SMK_010,
    alt_tobacco = TALDVUSE,
    alcohol = ALCDVTTM,
    job_sat = SWL_005,
    leisure_sat = SWL_010,
    finance_sat = SWL_015,
    you_sat = SWL_020,
    body_sat = SWL_025,
    family_sat = SWL_030,
    friends_sat = SWL_035,
    housing_sat = SWL_040,
    neighbourhood_sat = SWL_045,
    phq9 = DEPDVPHQ,
    employed = LBFG10,
    student = MAC_015
  ) %>% 
  mutate(ls = case_when(ls %in% c(97, 98, 99) ~ NA_integer_, TRUE ~ ls),
         province = GEO_PRV_MAPPING$PRV_NAME[match(province, GEO_PRV_MAPPING$PRV_NUM)],
         sex = case_when(sex == 1 ~ "Male", sex == 2 ~ "Female", TRUE ~ NA_character_), 
         age = DHHGAGE_mapping$DHHGAGE_NAME[match(age, DHHGAGE_mapping$DHHGAGE_NUM)],
         marital = case_when(marital == 1 ~ "Married/Common-law", marital == 2 ~ "Married/Common-law", 
                             marital == 3 ~ "Widowed/Divorced/Separated", marital == 4 ~ "Single", TRUE ~ NA_character_),
         hh_income = INCDGHH_mapping$INCDGHH_NAME[match(hh_income, INCDGHH_mapping$INCDGHH_NUM)],
         hh_size = case_when(hh_size == 9 ~ NA_integer_, TRUE ~ hh_size), 
         language = case_when(language == 1 ~ "English", language == 2 ~ "French", 
                              language == 3 ~ "English and French", language == 4 ~ "Neither English nor French", TRUE ~ NA_character_),
         immigration_status = case_when(immigration_status == 1 ~ "Landed immigrant/non-permanent resident", immigration_status == 2 ~ "Canadian born", TRUE ~ NA_character_),
         time_in_canada = case_when(time_in_canada == 1 ~ "0-9 years", time_in_canada == 2 ~ "10 or more years", 
                                    time_in_canada == 6 ~ "Canadian born", TRUE ~ NA_character_),
         mental_health = case_when(mental_health == 1 ~ "Excellent", mental_health == 2 ~ "Very good", 
                                   mental_health == 3 ~ "Good", mental_health == 4 ~ "Fair",
                                   mental_health == 5 ~ "Poor", TRUE ~ NA_character_),
         own_home = case_when(own_home == 1 ~ "Owned by household members", own_home == 2 ~ "Rented by household members", TRUE ~ NA_character_),
         indigenous = case_when(indigenous == 1 ~ "Yes", indigenous == 2 ~ "No", indigenous == 6 ~ "No", TRUE ~ NA_character_),
         minority = case_when(minority == 1 ~ "White", minority == 2 ~ "Non-white", TRUE ~ NA_character_), # Visible Minority: check what valid skip (6) stands for here
         sex_diversity = case_when(sex_diversity == 1 ~ "Heterosexual", sex_diversity == 2 ~ "Sexual minorities", 
                                   sex_diversity == 3 ~ "Sexual minorities", TRUE ~ NA_character_))


### CCHS 2015-2016
cchs2015_2016_extracted <- cchs2015_2016 %>%
  select(
    ls = GEN_010,
    province = GEO_PRV,
    sex = DHH_SEX,
    age = DHHGAGE,
    marital = DHHGMS,
    hh_income = INCDGHH,
    hh_size = DHHDGHSZ,
    language = SDCDVFLS,
    immigration_status = SDCDVIMM,
    time_in_canada = SDCDGRES,
    mental_health = GEN_015,
    own_home = DHH_OWN,
    live_arrange = DHHDGLVG,
    indigenous = SDC_015,
    minority = SDCDGCGT,
    weight = WTS_M,
    sex_diversity = SDC_035,
    education = DHHDG611,
    health = GEN_005,
    live_stress = GEN_020,
    work_stress = GEN_025,
    belonging = GEN_030,
    sleep_hours = SLPG005,
    sleep_refreshing = SLP_015,
    smoke = SMK_010,
    alt_tobacco = TALDVUSE,
    alcohol = ALCDVTTM,
    job_sat = SWL_005,
    leisure_sat = SWL_010,
    finance_sat = SWL_015,
    you_sat = SWL_020,
    body_sat = SWL_025,
    family_sat = SWL_030,
    friends_sat = SWL_035,
    housing_sat = SWL_040,
    neighbourhood_sat = SWL_045,
    employed = LBFG10,
    student = MAC_015
  ) %>% 
  mutate(ls = case_when(ls %in% c(97, 98, 99) ~ NA_integer_, TRUE ~ ls),
         province = GEO_PRV_MAPPING_2$PRV_NAME[match(province, GEO_PRV_MAPPING$PRV_NUM)],
         sex = case_when(sex == 1 ~ "Male", sex == 2 ~ "Female", TRUE ~ NA_character_), 
         age = DHHGAGE_mapping$DHHGAGE_NAME[match(age, DHHGAGE_mapping$DHHGAGE_NUM)],
         marital = case_when(marital == 1 ~ "Married/Common-law", marital == 2 ~ "Married/Common-law", 
                             marital == 3 ~ "Widowed/Divorced/Separated", marital == 4 ~ "Single", TRUE ~ NA_character_),
         hh_income = INCDGHH_mapping$INCDGHH_NAME[match(hh_income, INCDGHH_mapping$INCDGHH_NUM)],
         hh_size = case_when(hh_size == 9 ~ NA_integer_, TRUE ~ hh_size), 
         language = case_when(language == 1 ~ "English", language == 2 ~ "French", 
                              language == 3 ~ "English and French", language == 4 ~ "Neither English nor French", TRUE ~ NA_character_),
         immigration_status = case_when(immigration_status == 1 ~ "Landed immigrant/non-permanent resident", immigration_status == 2 ~ "Canadian born", TRUE ~ NA_character_),
         time_in_canada = case_when(time_in_canada == 1 ~ "0-9 years", time_in_canada == 2 ~ "10 or more years", 
                                    time_in_canada == 6 ~ "Canadian born", TRUE ~ NA_character_),
         mental_health = case_when(mental_health == 1 ~ "Excellent", mental_health == 2 ~ "Very good", 
                                   mental_health == 3 ~ "Good", mental_health == 4 ~ "Fair",
                                   mental_health == 5 ~ "Poor", TRUE ~ NA_character_),
         own_home = case_when(own_home == 1 ~ "Owned by household members", own_home == 2 ~ "Rented by household members", TRUE ~ NA_character_),
         indigenous = case_when(indigenous == 1 ~ "Yes", indigenous == 2 ~ "No", indigenous == 6 ~ "No", TRUE ~ NA_character_),
         minority = case_when(minority == 1 ~ "White", minority == 2 ~ "Non-white", TRUE ~ NA_character_), # Visible Minority: check what valid skip (6) stands for here
         sex_diversity = case_when(sex_diversity == 1 ~ "Heterosexual", sex_diversity == 2 ~ "Sexual minorities", 
                                   sex_diversity == 3 ~ "Sexual minorities", TRUE ~ NA_character_))

### CCHS 2013-2014
cchs2013_2014_extracted <- cchs2013_2014 %>%
  mutate(alt_tobacco = case_when(
    1 %in% c(TAL_1, TAL_2, TAL_3, TAL_4) ~ 1,
    all(c(TAL_1, TAL_2, TAL_3, TAL_4) == 2) ~ 2,
    TRUE ~ NA_real_
  )) %>%
  select(
    ls = GEN_02A2,
    province = GEOGPRV,
    sex = DHH_SEX,
    age = DHHGAGE,
    marital = DHHGMS,
    hh_income = INCGHH,
    hh_size = DHHGHSZ,
    immigration_status = SDCFIMM,
    time_in_canada = SDCGRES,
    mental_health = GEN_02B,
    own_home = DHH_OWN,
    live_arrange = DHHGLVG, # Assuming DHHGLVG is the correct variable name
    minority = SDCGCGT,
    weight = WTS_M,
    education = EDUDR04,
    health = GEN_01,
    live_stress = GEN_07,
    work_stress = GEN_09,
    belonging = GEN_10,
    sleep_hours = SLPG01, # Assuming SLPG01 is the correct variable name
    sleep_refreshing = SLP_03, # Assuming SLP_03 is the correct variable name
    smoke = SMK_05C, # Additional logic might be needed for '> 1'
    alcohol = ALCDTTM,
    employed = LBSDPFT,
    student = SDC_8 # Assuming SDC_8 is the correct variable name
  ) %>% 
  mutate(ls = case_when(ls %in% c(97, 98, 99) ~ NA_integer_, TRUE ~ ls),
         province = GEO_PRV_MAPPING_2$PRV_NAME[match(province, GEO_PRV_MAPPING$PRV_NUM)],
         sex = case_when(sex == 1 ~ "Male", sex == 2 ~ "Female", TRUE ~ NA_character_), 
         hh_income = DHHGAGE_mapping$DHHGAGE_NAME[match(hh_income, DHHGAGE_mapping$DHHGAGE_NUM)],
         marital = case_when(marital == 1 ~ "Married/Common-law", marital == 2 ~ "Married/Common-law", 
                             marital == 3 ~ "Widowed/Divorced/Separated", marital == 4 ~ "Single", TRUE ~ NA_character_),
         hh_size = case_when(hh_size == 9 ~ NA_integer_, TRUE ~ hh_size), 
         language = case_when(language == 1 ~ "English", language == 2 ~ "French", 
                              language == 3 ~ "English and French", language == 4 ~ "Neither English nor French", TRUE ~ NA_character_),
         immigration_status = case_when(immigration_status == 1 ~ "Landed immigrant/non-permanent resident", immigration_status == 2 ~ "Canadian born", TRUE ~ NA_character_),
         time_in_canada = case_when(time_in_canada == 1 ~ "0-9 years", time_in_canada == 2 ~ "10 or more years", 
                                    time_in_canada == 6 ~ "Canadian born", TRUE ~ NA_character_),
         mental_health = case_when(mental_health == 1 ~ "Excellent", mental_health == 2 ~ "Very good", 
                                   mental_health == 3 ~ "Good", mental_health == 4 ~ "Fair",
                                   mental_health == 5 ~ "Poor", TRUE ~ NA_character_),
         own_home = case_when(own_home == 1 ~ "Owned by household members", own_home == 2 ~ "Rented by household members", TRUE ~ NA_character_),
         minority = case_when(minority == 1 ~ "White", minority == 2 ~ "Non-white", TRUE ~ NA_character_) # Visible Minority: check what valid skip (6) stands for here
         # No equivalent found for SDC_015 and SDC_035 in the provided variable names above
)


# CCHS 2011-2012
cchs2011_2012_extracted <- cchs2011_2012 %>%
  mutate(alt_tobacco = case_when(
    1 %in% c(TAL_1, TAL_2, TAL_3, TAL_4) ~ 1,
    all(c(TAL_1, TAL_2, TAL_3, TAL_4) == 2) ~ 2,
    TRUE ~ NA_real_
  )) %>%
  select(
    ls = GEN_02A2,
    province = GEOGPRV,
    sex = DHH_SEX,
    age = DHHGAGE,
    marital = DHHGMS,
    hh_income = INCGHH,
    hh_size = DHHGHSZ,
    immigration_status = SDCFIMM,
    time_in_canada = SDCGRES,
    mental_health = GEN_02B,
    own_home = DHH_OWN,
    minority = SDCGCGT,
    weight = WTS_M,
    education = EDUDR04,
    health = GEN_01,
    live_stress = GEN_07,
    work_stress = GEN_09,
    belonging = GEN_10,
    job_sat = SWL_02,
    leisure_sat = SWL_03,
    finance_sat = SWL_04,
    you_sat = SWL_05,
    body_sat = SWL_06,
    family_sat = SWL_07,
    friends_sat = SWL_08,
    housing_sat = SWL_09,
    neighbourhood_sat = SWL_10,
    employed = LBSDPFT,
    student = SDC_8
  ) %>% 
  mutate(ls = case_when(ls %in% c(97, 98, 99) ~ NA_integer_, TRUE ~ ls),
         province = GEO_PRV_MAPPING_2$PRV_NAME[match(province, GEO_PRV_MAPPING_2$PRV_NUM)],
         sex = case_when(sex == 1 ~ "Male", sex == 2 ~ "Female", TRUE ~ NA_character_), 
         hh_income = INCDGHH_mapping$INCDGHH_NAME[match(hh_income, INCDGHH_mapping$INCDGHH_NUM)],
         marital = case_when(marital == 1 ~ "Married/Common-law", marital == 2 ~ "Married/Common-law", 
                             marital == 3 ~ "Widowed/Divorced/Separated", marital == 4 ~ "Single", TRUE ~ NA_character_),
         hh_size = case_when(hh_size == 9 ~ NA_integer_, TRUE ~ hh_size), 
         language = case_when(language == 1 ~ "English", language == 2 ~ "French", 
                              language == 3 ~ "English and French", language == 4 ~ "Neither English nor French", TRUE ~ NA_character_),
         immigration_status = case_when(immigration_status == 1 ~ "Landed immigrant/non-permanent resident", immigration_status == 2 ~ "Canadian born", TRUE ~ NA_character_),
         time_in_canada = case_when(time_in_canada == 1 ~ "0-9 years", time_in_canada == 2 ~ "10 or more years", 
                                    time_in_canada == 6 ~ "Canadian born", TRUE ~ NA_character_),
         mental_health = case_when(mental_health == 1 ~ "Excellent", mental_health == 2 ~ "Very good", 
                                   mental_health == 3 ~ "Good", mental_health == 4 ~ "Fair",
                                   mental_health == 5 ~ "Poor", TRUE ~ NA_character_),
         own_home = case_when(own_home == 1 ~ "Owned by household members", own_home == 2 ~ "Rented by household members", TRUE ~ NA_character_),
         minority = case_when(minority == 1 ~ "White", minority == 2 ~ "Non-white", TRUE ~ NA_character_) # Visible Minority: check what valid skip (6) stands for here
         # No equivalent found for SDC_015 and SDC_035 in the provided variable names above
  )

# CCHS 2009-2010

cchs2009_2010_extracted <- cchs2009_2010 %>%
  mutate(alt_tobacco = case_when(
      1 %in% c(TAL_1, TAL_2, TAL_3, TAL_4) ~ 1,
      all(c(TAL_1, TAL_2, TAL_3, TAL_4) == 2) ~ 2,
      TRUE ~ NA_real_
    )) %>%
  select(
    ls = GEN_02A2,
    province = GEOGPRV,
    sex = DHH_SEX,
    age = DHHGAGE,
    marital = DHHGMS,
    hh_income = INCGHH,
    hh_size = DHHGHSZ,
    immigration_status = SDCFIMM,
    time_in_canada = SDCGRES,
    mental_health = GEN_02B,
    own_home = DHH_OWN,
    minority = SDCGCGT,
    weight = WTS_M,
    education = EDUDR04,
    health = GEN_01,
    live_stress = GEN_07,
    work_stress = GEN_09,
    belonging = GEN_10,
    job_sat = SWL_02,
    leisure_sat = SWL_03,
    finance_sat = SWL_04,
    you_sat = SWL_05,
    body_sat = SWL_06,
    family_sat = SWL_07,
    friends_sat = SWL_08,
    housing_sat = SWL_09,
    neighbourhood_sat = SWL_10,
    employed = LBSDPFT,
    student = SDC_8
  ) %>% 
  mutate(ls = case_when(ls %in% c(97, 98, 99) ~ NA_integer_, TRUE ~ ls),
         province = GEO_PRV_MAPPING_2$PRV_NAME[match(province, GEO_PRV_MAPPING$PRV_NUM)],
         sex = case_when(sex == 1 ~ "Male", sex == 2 ~ "Female", TRUE ~ NA_character_), 
         hh_income = DHHGAGE_mapping$DHHGAGE_NAME[match(hh_income, DHHGAGE_mapping$DHHGAGE_NUM)],
         marital = case_when(marital == 1 ~ "Married/Common-law", marital == 2 ~ "Married/Common-law", 
                             marital == 3 ~ "Widowed/Divorced/Separated", marital == 4 ~ "Single", TRUE ~ NA_character_),
         hh_size = case_when(hh_size == 9 ~ NA_integer_, TRUE ~ hh_size), 
         immigration_status = case_when(immigration_status == 1 ~ "Landed immigrant/non-permanent resident", immigration_status == 2 ~ "Canadian born", TRUE ~ NA_character_),
         time_in_canada = case_when(time_in_canada == 1 ~ "0-9 years", time_in_canada == 2 ~ "10 or more years", 
                                    time_in_canada == 6 ~ "Canadian born", TRUE ~ NA_character_),
         mental_health = case_when(mental_health == 1 ~ "Excellent", mental_health == 2 ~ "Very good", 
                                   mental_health == 3 ~ "Good", mental_health == 4 ~ "Fair",
                                   mental_health == 5 ~ "Poor", TRUE ~ NA_character_),
         own_home = case_when(own_home == 1 ~ "Owned by household members", own_home == 2 ~ "Rented by household members", TRUE ~ NA_character_),
         minority = case_when(minority == 1 ~ "White", minority == 2 ~ "Non-white", TRUE ~ NA_character_) # Visible Minority: check what valid skip (6) stands for here
  )

save.image("CCHS_harmonization.RData") 
load("CCHS_harmonization.RData")

######################### Generate a simulated table #############################
# Define the parameters for the simulated data
provinces <- c("NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "YT", "NT", "NU")
sex <- c("Male", "Female")
age_groups <- c("15-29", "30-44", "45-59", "60+")
life_satisfaction_scores <- 0:10

# Approximate population weights by province (in thousands for simplicity)
province_populations <- c(520, 160, 990, 790, 8600, 14700, 1380, 1180, 4400, 5100, 40, 45, 40)

# Create a data frame by expanding the parameters
simulated_data <- expand.grid(
  GEO_PRV = provinces,
  DHH_SEX = sex,
  DHH_AGE = age_groups,
  GEN_010 = life_satisfaction_scores
)

# Initialize a column for weighted frequencies
simulated_data$Weighted_Frequency <- NA_integer_

# Assign random weighted frequencies to each combination of province, age group, and life satisfaction score
# Ensuring that the sum of weights for each province roughly adds up to its population
set.seed(123) # For reproducibility
for (prov in provinces) {
  # Calculate the number of rows for the current province
  province_rows <- nrow(simulated_data[simulated_data$GEO_PRV == prov, ])
  
  # Calculate the total population for the current province
  province_population <- province_populations[provinces == prov]
  
  # Generate random weights that sum up to the province's population
  # The weights are distributed across the number of rows for the province
  weights <- runif(province_rows, min = 0, max = 1)
  weights <- round(weights / sum(weights) * province_population*1000)
  
  # Assign the weights to the simulated data
  simulated_data$Weighted_Frequency[simulated_data$GEO_PRV == prov] <- weights
}

# Order the data frame by province, sex, and age group
simulated_data <- simulated_data[order(simulated_data$GEO_PRV, simulated_data$DHH_SEX, simulated_data$DHH_AGE, simulated_data$GEN_010), ]

# View the first few rows of the simulated data frame
head(simulated_data)

averages <- sapply(cchs_pumf_2015_2016, function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else NA)
averages[averages > 1500]

