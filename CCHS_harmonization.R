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
library(tidyr)

#setwd("~/Documents/*UoT/PWB/YMH/CCHS") # Change this to your own directory

########### Reading CCHS data #############

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


##### Declare Useful Mappings #####

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

## Declare list of variables for identifying the rows in the two-year files which are present in the one-year files
matching_variables <- c(
  "GEOGPRV", "DHHGAGE", "DHH_SEX", "GEODPMF", "DHHGLE5", "DHHG611", "DHHGL12", 
  "DHHGLVG", "DHHGHSZ", "GEN_01", "GEN_02", "GEN_02A2", "GEN_02B", "GEN_07", 
  "GEN_08", "GEN_09", "GEN_10", "GENDHDI", "GENDMHI", "GENGSWL",  
  "CIH_1", "CIH_2", "CIH_3", "CIH_4", "CIH_5", "CIH_6A", "CIH_6I", "CIH_6B", 
  "CIH_6J", "CIH_6K", "CIH_6G", "CIH_6F", "CIH_6E", "CIH_6L", "CIH_6M", "CIH_6N", 
  "CIH_6H", "CIH_7", "CIH_8A", "CIH_8B", "CIH_8C", "CIH_8J", "CIH_8K", "CIH_8G", 
  "CIH_8L", "CIH_8H", "CIH_8I", "HWT_4", 
  "HWTGHTM", "HWTGWTK", "HWTGBMI", "HWTGISW", "HWTDCOL", "CCC_031", "CCC_035", 
  "CCC_036", "CCC_051", "CCC_061", "CCC_071", "CCC_072", "CCC_073", "CCC_073A", 
  "CCC_073B", "CCC_081", "CCC_091", "CCC_101", "CCCG102", "CCC_10A", "CCC_10B", 
  "CCC_10C", "CCC_105", "CCC_106", "CCC_121", "CCC_131", "CCC_31A", "CCC_141", 
  "CCC_151", "CCC_161", "CCC_171", "CCC_17A", "CCC_280", "CCC_290", "CCCDDIA",
  "INCG2", "INCG7", "INCGHH", "INCGPER", "FSC_010", "FSC_020", 
  "FSC_030", "FSC_040", "FSC_050", "FSC_060", "FSC_070", "FSC_080", 
  "FSC_081", "FSC_090", "FSC_100", "FSC_110", "FSC_120", "FSC_121", 
  "FSC_130", "FSC_140", "FSC_141", "FSC_150", "FSC_160", "LBSGHPW"
)

######### VARIABLE LABELS #########
health_labels <- c("Poor" = 0, "Fair" = 1, "Good" = 2, "Very good" = 3, "Excellent" = 4) # For health and mental_health variables
income_labels <- c("≤$20,000" = 1, "$20,000-$39,999" = 2, "$40,000-$59,999" = 3, "$60,000-$79,999" = 4, "$80,000 or more" = 5)
stress_labels <- c("Not at all stressful" = 0, "Not very stressful" = 1, "A bit stressful" = 2, "Quite a bit stressful" = 3, "Extremely stressful" = 4) # For work and life stress
belonging_labels <- c("Very weak" = 0, "Somewhat weak" = 1, "Somewhat strong" = 2, "Very strong" = 3)
sleep_hours_labels <- c("Less than 3 hours" = 1.5, "3-4 hours" = 3, "4-5 hours" = 4, "5-6 hours" = 5, "6-7 hours" = 6, "7-8 hours" = 7, "8-9 hours" = 8, "9-10 hours" = 9, "10-11 hours" = 10, "11-12 hours" = 11, "12 hours or more" = 12)
sleep_refreshing_labels <- c("Never" = 0, "Rarely" = 1, "Sometimes" = 2, "Most of the time" = 3, "All of the time" = 4)
satisfaction_labels <- c("Very dissatisfied" = 0, "Dissatisfied" = 1, "Neither satisfied nor dissatisfied" = 2, "Satisfied" = 3, "Very satisfied" = 4)
live_arrange_labels <- c(
  "Unattached individual living alone" = 1,
  "Unattached individual living with others" = 2,
  "Individual living with spouse/partner" = 3,
  "Parent living with spouse/partner and child(ren)" = 4,
  "Single parent living with children" = 5,
  "Child living with a single parent with or without siblings" = 6,
  "Child living with two parents with or without siblings" = 7,
  "Other" = 8
)

######### CCHS_2017-2018 ~ Renaming and Cleaning ##########

cchs_2017_2018 <- cchs_pumf_2017_2018 %>%
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
    education = EHG2DVR3,
    health = GEN_005,
    life_stress = GEN_020,
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
    student = MAC_015,
    screentime_weekday=SBE_005, 
    screentime_weekend=SBE_010, 
    sps_attachment=SPSDVATT,
    sps_integration=SPSDVINT
  ) %>% 
  mutate( ls = case_when(ls <= 10 ~ ls, TRUE ~ NA_integer_),
          province = GEO_PRV_MAPPING$PRV_NAME[match(province, GEO_PRV_MAPPING$PRV_NUM)],
          sex = case_when(sex == 1 ~ "Male", sex == 2 ~ "Female", TRUE ~ NA_character_), 
          age = DHHGAGE_mapping$DHHGAGE_NAME[match(age, DHHGAGE_mapping$DHHGAGE_NUM)],
          marital = case_when(marital == 1 ~ "Married/Common-law", marital == 2 ~ "Married/Common-law", 
                              marital == 3 ~ "Widowed/Divorced/Separated", marital == 4 ~ "Single", TRUE ~ NA_character_),
          hh_income = case_when(hh_income <= 5 ~ hh_income, TRUE ~ NA_integer_),
          hh_size = case_when(hh_size == 9 ~ NA_integer_, TRUE ~ hh_size), 
          language = case_when(language == 1 ~ "English", language == 2 ~ "French", 
                                language == 3 ~ "English and French", language == 4 ~ "Neither English nor French", TRUE ~ NA_character_),
          immigration_status = case_when(immigration_status == 1 ~ "Landed immigrant/non-permanent resident", immigration_status == 2 ~ "Canadian born", TRUE ~ NA_character_),
          time_in_canada = case_when(time_in_canada == 1 ~ "0-9 years", time_in_canada == 2 ~ "10 or more years", 
                                      time_in_canada == 6 ~ "Canadian born", TRUE ~ NA_character_),
          mental_health = case_when(mental_health <= 5 ~ 5 - mental_health, TRUE ~ NA_integer_),
          live_arrange = case_when(live_arrange >= 1 & live_arrange <= 8 ~ as.numeric(live_arrange), TRUE ~ NA_integer_),
          own_home = case_when(
            live_arrange >= 1 & live_arrange <= 5 & own_home == 1 ~ "Owns home",
            is.na(live_arrange) | (own_home != 1 & own_home != 2) ~ NA_character_,
            TRUE ~ "Does not own home"
          ),
          indigenous = case_when(indigenous == 1 ~ "Yes", indigenous == 2 ~ "No", indigenous == 6 ~ "No", TRUE ~ NA_character_),
          minority = case_when(minority == 1 ~ "White", minority == 2 ~ "Non-white", minority == 6 ~ "Non-white", TRUE ~ NA_character_),
          sex_diversity = case_when(sex_diversity == 1 ~ "Heterosexual", sex_diversity == 2 ~ "Sexual minorities", 
                                   sex_diversity == 3 ~ "Sexual minorities", TRUE ~ NA_character_),
          education = case_when(education == 1 ~ "Less than secondary", education == 2 ~ "Secondary", education == 3 ~ "Post-secondary", TRUE ~ NA_character_),
          health = case_when(health <= 5 ~ 5 - health, TRUE ~ NA_integer_),
          life_stress = case_when(life_stress <= 5 ~ life_stress - 1, TRUE ~ NA_integer_),
          work_stress = case_when(work_stress <= 5 ~ work_stress - 1, TRUE ~ NA_integer_),
          belonging = case_when(belonging <= 4 ~ 4 - belonging, TRUE ~ NA_integer_),
          sleep_hours = case_when(sleep_hours == 1 ~ 1.5, (sleep_hours >= 2 & sleep_hours <=11) ~ sleep_hours + 1, TRUE ~ NA_integer_),
          sleep_refreshing = case_when(sleep_refreshing <= 5 ~ sleep_refreshing - 1, TRUE ~ NA_integer_),
          nicotine = case_when(smoke == 1 | alt_tobacco == 1 ~ "Yes", smoke == 2 & alt_tobacco == 2 ~ "No", TRUE ~ NA_character_), #CHANGE TO smoke > 1 for 2014 and earlier
          alcohol = case_when(alcohol == 1 ~ "Regular", alcohol == 2 ~ "Occasional", alcohol == 3 ~ "Did not drink in 12 mos"),
          job_sat = case_when(job_sat <= 5 ~ 5 - job_sat, TRUE ~ NA_integer_),
          leisure_sat = case_when(leisure_sat <= 5 ~ 5 - leisure_sat, TRUE ~ NA_integer_),
          finance_sat = case_when(finance_sat <= 5 ~ 5 - finance_sat, TRUE ~ NA_integer_),
          you_sat = case_when(you_sat <= 5 ~ 5 - you_sat, TRUE ~ NA_integer_),
          body_sat = case_when(body_sat <= 5 ~ 5 - body_sat, TRUE ~ NA_integer_),
          family_sat = case_when(family_sat <= 5 ~ 5 - family_sat, TRUE ~ NA_integer_),
          friends_sat = case_when(friends_sat <= 5 ~ 5 - friends_sat, TRUE ~ NA_integer_),
          housing_sat = case_when(housing_sat <= 5 ~ 5 - housing_sat, TRUE ~ NA_integer_),
          neighbourhood_sat = case_when(neighbourhood_sat <= 5 ~ 5 - neighbourhood_sat, TRUE ~ NA_integer_),
          phq9 = case_when(phq9 <= 27 ~ phq9, TRUE ~ NA_integer_),
          employed = case_when(employed == 1 ~ "Employed", employed == 2 ~ "Self-employed", employed == 6 ~ "Not employed", TRUE ~ NA_character_),
          student = case_when(student == 1 ~ "Student", student == 2 ~ "Not a student", TRUE ~ NA_character_) ,
          screentime_weekday=case_when(screentime_weekday==1~"2 hours or less per day",screentime_weekday==2~"More than 2 hours but less than 4 hours",screentime_weekday==3~"4 hours to less than 6 hours",
                                       screentime_weekday==4~ "6 hours to less than 8 hours",screentime_weekday==5~"8 hours or more per day",screentime_weekday==6~"Was not at work or school",TRUE ~ NA_character_),
          screentime_weekend=case_when(screentime_weekend==1~"2 hours or less per day",screentime_weekend==2~"More than 2 hours but less than 4 hours",screentime_weekend==3~"4 hours to less than 6 hours",
                                       screentime_weekend==4~ "6 hours to less than 8 hours",screentime_weekend==5~"8 hours or more per day",screentime_weekend==6~"Was not at work or school",TRUE ~ NA_character_),
          sps_attachment=case_when(sps_attachment<=8~sps_attachment, TRUE ~ NA_integer_),
          sps_integration=case_when(sps_integration<=8~sps_integration, TRUE ~ NA_integer_))


cchs_2017_2018$sps_attachment
cchs_2017_2018$sps_integration

######### CCHS_2015-2016 ~ Renaming and Cleaning ##########

cchs_2015_2016 <- cchs_pumf_2015_2016 %>%
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
    education = EHG2DVR3,
    health = GEN_005,
    life_stress = GEN_020,
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
    student = MAC_015,
    screentime_week=SACDVTER,
    sps_attachment=SPSDVATT,
    sps_integration=SPSDVINT
  ) %>% 
  mutate( ls = case_when(ls <= 10 ~ ls, TRUE ~ NA_integer_),
          province = GEO_PRV_MAPPING$PRV_NAME[match(province, GEO_PRV_MAPPING$PRV_NUM)],
          sex = case_when(sex == 1 ~ "Male", sex == 2 ~ "Female", TRUE ~ NA_character_), 
          age = DHHGAGE_mapping$DHHGAGE_NAME[match(age, DHHGAGE_mapping$DHHGAGE_NUM)],
          marital = case_when(marital == 1 ~ "Married/Common-law", marital == 2 ~ "Married/Common-law", 
                              marital == 3 ~ "Widowed/Divorced/Separated", marital == 4 ~ "Single", TRUE ~ NA_character_),
          hh_income = case_when(hh_income <= 5 ~ hh_income, TRUE ~ NA_integer_),
          hh_size = case_when(hh_size == 9 ~ NA_integer_, TRUE ~ hh_size), 
          language = case_when(language == 1 ~ "English", language == 2 ~ "French", 
                                language == 3 ~ "English and French", language == 4 ~ "Neither English nor French", TRUE ~ NA_character_),
          immigration_status = case_when(immigration_status == 1 ~ "Landed immigrant/non-permanent resident", immigration_status == 2 ~ "Canadian born", TRUE ~ NA_character_),
          time_in_canada = case_when(time_in_canada == 1 ~ "0-9 years", time_in_canada == 2 ~ "10 or more years", 
                                      time_in_canada == 6 ~ "Canadian born", TRUE ~ NA_character_),
          mental_health = case_when(mental_health <= 5 ~ 5 - mental_health, TRUE ~ NA_integer_),
          live_arrange = case_when(live_arrange >= 1 & live_arrange <= 8 ~ as.numeric(live_arrange), TRUE ~ NA_integer_),
          own_home = case_when(
            live_arrange >= 1 & live_arrange <= 5 & own_home == 1 ~ "Owns home",
            is.na(live_arrange) | (own_home != 1 & own_home != 2) ~ NA_character_,
            TRUE ~ "Does not own home"
          ),
          indigenous = case_when(indigenous == 1 ~ "Yes", indigenous == 2 ~ "No", indigenous == 6 ~ "No", TRUE ~ NA_character_),
          minority = case_when(minority == 1 ~ "White", minority == 2 ~ "Non-white", minority == 6 ~ "Non-white", TRUE ~ NA_character_),
          sex_diversity = case_when(sex_diversity == 1 ~ "Heterosexual", sex_diversity == 2 ~ "Sexual minorities", 
                                   sex_diversity == 3 ~ "Sexual minorities", TRUE ~ NA_character_),
          education = case_when(education == 1 ~ "Less than secondary", education == 2 ~ "Secondary", education == 3 ~ "Post-secondary", TRUE ~ NA_character_),
          health = case_when(health <= 5 ~ 5 - health, TRUE ~ NA_integer_),
          life_stress = case_when(life_stress <= 5 ~ life_stress - 1, TRUE ~ NA_integer_),
          work_stress = case_when(work_stress <= 5 ~ work_stress - 1, TRUE ~ NA_integer_),
          belonging = case_when(belonging <= 4 ~ 4 - belonging, TRUE ~ NA_integer_),
          sleep_hours = case_when(sleep_hours == 1 ~ 1.5, (sleep_hours >= 2 & sleep_hours <=11) ~ sleep_hours + 1, TRUE ~ NA_integer_),
          sleep_refreshing = case_when(sleep_refreshing <= 5 ~ sleep_refreshing - 1, TRUE ~ NA_integer_),
          nicotine = case_when(smoke == 1 | alt_tobacco == 1 ~ "Yes", smoke == 2 & alt_tobacco == 2 ~ "No", TRUE ~ NA_character_), #CHANGE TO smoke > 1 for 2014 and earlier
          alcohol = case_when(alcohol == 1 ~ "Regular", alcohol == 2 ~ "Occasional", alcohol == 3 ~ "Did not drink in 12 mos"),
          job_sat = case_when(job_sat <= 5 ~ 5 - job_sat, TRUE ~ NA_integer_),
          leisure_sat = case_when(leisure_sat <= 5 ~ 5 - leisure_sat, TRUE ~ NA_integer_),
          finance_sat = case_when(finance_sat <= 5 ~ 5 - finance_sat, TRUE ~ NA_integer_),
          you_sat = case_when(you_sat <= 5 ~ 5 - you_sat, TRUE ~ NA_integer_),
          body_sat = case_when(body_sat <= 5 ~ 5 - body_sat, TRUE ~ NA_integer_),
          family_sat = case_when(family_sat <= 5 ~ 5 - family_sat, TRUE ~ NA_integer_),
          friends_sat = case_when(friends_sat <= 5 ~ 5 - friends_sat, TRUE ~ NA_integer_),
          housing_sat = case_when(housing_sat <= 5 ~ 5 - housing_sat, TRUE ~ NA_integer_),
          neighbourhood_sat = case_when(neighbourhood_sat <= 5 ~ 5 - neighbourhood_sat, TRUE ~ NA_integer_),
          phq9 = case_when(phq9 <= 27 ~ phq9, TRUE ~ NA_integer_),
          employed = case_when(employed == 1 ~ "Employed", employed == 2 ~ "Self-employed", employed == 6 ~ "Not employed", TRUE ~ NA_character_),
          student = case_when(student == 1 ~ "Student", student == 2 ~ "Not a student", TRUE ~ NA_character_),
          screentime_week=case_when(screentime_week==1~"Less than 5 hours",screentime_week==2~"From 5 to 9 hours",screentime_week==3~"From 10 to 14 hours",screentime_week==4~"From 15 to 19 hours",
                                    screentime_week==5~"From 20 to 24 hours",screentime_week==6~"From 25 to 29 hours",screentime_week==7~"From 30 to 34 hours",screentime_week==8~"From 35 to 39 hours",
                                    screentime_week==9~"From 40 to 44 hours",screentime_week==10~"45 hours or more",TRUE ~ NA_character_),
          sps_attachment=case_when(sps_attachment<=8~sps_attachment, TRUE ~ NA_integer_),
          sps_integration=case_when(sps_integration<=8~sps_integration, TRUE ~ NA_integer_))



######### CCHS_2013-2014 ~ Renaming, Separating and Cleaning ##########

### Separate 2013 and 2014 observations before continuing

# Extract just the rows in the two-year file that aren't present in the one-year file to make the first one-year file
cchs_pumf_2013 <- anti_join(cchs_pumf_2013_2014, cchs_pumf_2014, by = matching_variables)

cchs_2013 <- cchs_pumf_2013 %>%
  rowwise() %>%
  mutate(alt_tobacco = case_when(
    1 %in% c(TAL_1, TAL_2, TAL_3, TAL_4) ~ 1,
    all(c(TAL_1, TAL_2, TAL_3, TAL_4) == 2) ~ 2,
    TRUE ~ NA_real_
  )) %>%
  ungroup() %>%
  select(
    ls = GEN_02A2,
    province = GEOGPRV,
    sex = DHH_SEX,
    age = DHHGAGE,
    marital = DHHGMS,
    hh_income = INCGHH,
    hh_size = DHHGHSZ,
    language = SDCDFOLS,
    immigration_status = SDCFIMM,
    time_in_canada = SDCGRES,
    mental_health = GEN_02B,
    own_home = DHH_OWN,
    live_arrange = DHHGLVG, 
    minority = SDCGCGT,
    weight = WTS_M,
    education = EDUDR04,
    health = GEN_01,
    life_stress = GEN_07,
    work_stress = GEN_09,
    belonging = GEN_10,
    sleep_hours = SLPG01, 
    sleep_refreshing = SLP_03, 
    smoke = SMK_05C,
    alt_tobacco,
    TAL_1, TAL_2, TAL_3, TAL_4,
    alcohol = ALCDTTM,
    employed = LBSG31,
    student = SDC_8,
    screentime_week=SACDTER,
    sps_attachment=SPSDATT,
    sps_integration=SPSDINT
  ) %>% 
  mutate( ls = case_when(ls <= 10 ~ ls, TRUE ~ NA_integer_),
          province = GEO_PRV_MAPPING_2$PRV_NAME[match(province, GEO_PRV_MAPPING$PRV_NUM)],
          sex = case_when(sex == 1 ~ "Male", sex == 2 ~ "Female", TRUE ~ NA_character_), 
          age = DHHGAGE_mapping$DHHGAGE_NAME[match(age, DHHGAGE_mapping$DHHGAGE_NUM)],
          marital = case_when(marital == 1 ~ "Married/Common-law", marital == 2 ~ "Married/Common-law", 
                              marital == 3 ~ "Widowed/Divorced/Separated", marital == 4 ~ "Single", TRUE ~ NA_character_),
          hh_income = case_when(hh_income <= 5 ~ hh_income, TRUE ~ NA_integer_),
          hh_size = case_when(hh_size == 9 ~ NA_integer_, TRUE ~ hh_size), 
          language = case_when(language == 1 ~ "English", language == 2 ~ "French", 
                                language == 3 ~ "English and French", language == 4 ~ "Neither English nor French", TRUE ~ NA_character_),
          immigration_status = case_when(immigration_status == 1 ~ "Landed immigrant/non-permanent resident", immigration_status == 2 ~ "Canadian born", TRUE ~ NA_character_),
          time_in_canada = case_when(time_in_canada == 1 ~ "0-9 years", time_in_canada == 2 ~ "10 or more years", 
                                      time_in_canada == 6 ~ "Canadian born", TRUE ~ NA_character_),
          mental_health = case_when(mental_health <= 5 ~ 5 - mental_health, TRUE ~ NA_integer_),
          live_arrange = case_when(live_arrange >= 1 & live_arrange <= 8 ~ as.numeric(live_arrange), TRUE ~ NA_integer_),
          own_home = case_when(
            live_arrange >= 1 & live_arrange <= 5 & own_home == 1 ~ "Owns home",
            is.na(live_arrange) | (own_home != 1 & own_home != 2) ~ NA_character_,
            TRUE ~ "Does not own home"
          ),
          minority = case_when(minority == 1 ~ "White", minority == 2 ~ "Non-white", minority == 6 ~ "Non-white", TRUE ~ NA_character_),
          education = case_when(education == 1 ~ "Less than secondary", education == 2 ~ "Secondary", education == 3 | education == 4 ~ "Post-secondary", TRUE ~ NA_character_),
          health = case_when(health <= 5 ~ 5 - health, TRUE ~ NA_integer_),
          life_stress = case_when(life_stress <= 5 ~ life_stress - 1, TRUE ~ NA_integer_),
          work_stress = case_when(work_stress <= 5 ~ work_stress - 1, TRUE ~ NA_integer_),
          belonging = case_when(belonging <= 4 ~ 4 - belonging, TRUE ~ NA_integer_),
          sleep_hours = case_when(sleep_hours == 1 ~ 1.5, (sleep_hours >= 2 & sleep_hours <=11) ~ sleep_hours + 1, TRUE ~ NA_integer_),
          sleep_refreshing = case_when(sleep_refreshing <= 5 ~ sleep_refreshing - 1, TRUE ~ NA_integer_),
          smoke = case_when(smoke > 0 & smoke <= 41 ~ 1, smoke == 0 ~ 2, smoke == 96 ~ 2, TRUE ~ NA_integer_),
          nicotine = case_when(smoke == 1 | alt_tobacco == 1 ~ "Yes", smoke == 2 & alt_tobacco == 2 ~ "No", TRUE ~ NA_character_),
          alcohol = case_when(alcohol == 1 ~ "Regular", alcohol == 2 ~ "Occasional", alcohol == 3 ~ "Did not drink in 12 mos"),
          employed = case_when(employed == 1 ~ "Employed", employed == 2 ~ "Self-employed", employed == 6 ~ "Not employed", TRUE ~ NA_character_),
          student = case_when(student == 1 ~ "Student", student == 2 ~ "Not a student", TRUE ~ NA_character_),
          screentime_week=case_when(screentime_week==1~"Less than 5 hours",screentime_week==2~"From 5 to 9 hours",screentime_week==3~"From 10 to 14 hours",screentime_week==4~"From 15 to 19 hours",
                                    screentime_week==5~"From 20 to 24 hours",screentime_week==6~"From 25 to 29 hours",screentime_week==7~"From 30 to 34 hours",screentime_week==8~"From 35 to 39 hours",
                                    screentime_week==9~"From 40 to 44 hours",screentime_week==10~"45 hours or more",TRUE ~ NA_character_),
          sps_attachment=case_when(sps_attachment<=8~sps_attachment, TRUE ~ NA_integer_),
          sps_integration=case_when(sps_integration<=8~sps_integration, TRUE ~ NA_integer_))


cchs_2014 <- cchs_pumf_2014 %>%
  rowwise() %>%
  mutate(alt_tobacco = case_when(
    1 %in% c(TAL_1, TAL_2, TAL_3, TAL_4) ~ 1,
    all(c(TAL_1, TAL_2, TAL_3, TAL_4) == 2) ~ 2,
    TRUE ~ NA_real_
  )) %>%
  ungroup() %>%
  select(
    ls = GEN_02A2,
    province = GEOGPRV,
    sex = DHH_SEX,
    age = DHHGAGE,
    marital = DHHGMS,
    hh_income = INCGHH,
    hh_size = DHHGHSZ,
    language = SDCDFOLS,
    immigration_status = SDCFIMM,
    time_in_canada = SDCGRES,
    mental_health = GEN_02B,
    own_home = DHH_OWN,
    live_arrange = DHHGLVG, 
    minority = SDCGCGT,
    weight = WTS_M,
    education = EDUDR04,
    health = GEN_01,
    life_stress = GEN_07,
    work_stress = GEN_09,
    belonging = GEN_10,
    sleep_hours = SLPG01, 
    sleep_refreshing = SLP_03, 
    smoke = SMK_05C, 
    alt_tobacco,
    alcohol = ALCDTTM,
    employed = LBSDPFT,
    student = SDC_8,
    screentime_week=SACDTER,
    sps_attachment=SPSDATT,
    sps_integration=SPSDINT
  ) %>% 
  mutate( ls = case_when(ls <= 10 ~ ls, TRUE ~ NA_integer_),
          province = GEO_PRV_MAPPING_2$PRV_NAME[match(province, GEO_PRV_MAPPING$PRV_NUM)],
          sex = case_when(sex == 1 ~ "Male", sex == 2 ~ "Female", TRUE ~ NA_character_), 
          age = DHHGAGE_mapping$DHHGAGE_NAME[match(age, DHHGAGE_mapping$DHHGAGE_NUM)],
          marital = case_when(marital == 1 ~ "Married/Common-law", marital == 2 ~ "Married/Common-law", 
                              marital == 3 ~ "Widowed/Divorced/Separated", marital == 4 ~ "Single", TRUE ~ NA_character_),
          hh_income = case_when(hh_income <= 5 ~ hh_income, TRUE ~ NA_integer_),
          hh_size = case_when(hh_size == 9 ~ NA_integer_, TRUE ~ hh_size), 
          language = case_when(language == 1 ~ "English", language == 2 ~ "French", 
                                language == 3 ~ "English and French", language == 4 ~ "Neither English nor French", TRUE ~ NA_character_),
          immigration_status = case_when(immigration_status == 1 ~ "Landed immigrant/non-permanent resident", immigration_status == 2 ~ "Canadian born", TRUE ~ NA_character_),
          time_in_canada = case_when(time_in_canada == 1 ~ "0-9 years", time_in_canada == 2 ~ "10 or more years", 
                                      time_in_canada == 6 ~ "Canadian born", TRUE ~ NA_character_),
          mental_health = case_when(mental_health <= 5 ~ 5 - mental_health, TRUE ~ NA_integer_),
          live_arrange = case_when(live_arrange >= 1 & live_arrange <= 8 ~ as.numeric(live_arrange), TRUE ~ NA_integer_),
          own_home = case_when(
            live_arrange >= 1 & live_arrange <= 5 & own_home == 1 ~ "Owns home",
            is.na(live_arrange) | (own_home != 1 & own_home != 2) ~ NA_character_,
            TRUE ~ "Does not own home"
          ),          
          minority = case_when(minority == 1 ~ "White", minority == 2 ~ "Non-white", minority == 6 ~ "Non-white", TRUE ~ NA_character_),
          education = case_when(education == 1 ~ "Less than secondary", education == 2 ~ "Secondary", education == 3 | education == 4 ~ "Post-secondary", TRUE ~ NA_character_),
          health = case_when(health <= 5 ~ 5 - health, TRUE ~ NA_integer_),
          life_stress = case_when(life_stress <= 5 ~ life_stress - 1, TRUE ~ NA_integer_),
          work_stress = case_when(work_stress <= 5 ~ work_stress - 1, TRUE ~ NA_integer_),
          belonging = case_when(belonging <= 4 ~ 4 - belonging, TRUE ~ NA_integer_),
          sleep_hours = case_when(sleep_hours == 1 ~ 1.5, (sleep_hours >= 2 & sleep_hours <=11) ~ sleep_hours + 1, TRUE ~ NA_integer_),
          sleep_refreshing = case_when(sleep_refreshing <= 5 ~ sleep_refreshing - 1, TRUE ~ NA_integer_),
          smoke = case_when(smoke > 0 & smoke <= 41 ~ 1, smoke == 0 ~ 2, smoke == 96 ~ 2, TRUE ~ NA_integer_),
          nicotine = case_when(smoke == 1 | alt_tobacco == 1 ~ "Yes", smoke == 2 & alt_tobacco == 2 ~ "No", TRUE ~ NA_character_),
          alcohol = case_when(alcohol == 1 ~ "Regular", alcohol == 2 ~ "Occasional", alcohol == 3 ~ "Did not drink in 12 mos"),
          employed = case_when(employed == 1 ~ "Employed", employed == 2 ~ "Self-employed", employed == 6 ~ "Not employed", TRUE ~ NA_character_),
          student = case_when(student == 1 ~ "Student", student == 2 ~ "Not a student", TRUE ~ NA_character_),
          screentime_week=case_when(screentime_week==1~"Less than 5 hours",screentime_week==2~"From 5 to 9 hours",screentime_week==3~"From 10 to 14 hours",screentime_week==4~"From 15 to 19 hours",
                                    screentime_week==5~"From 20 to 24 hours",screentime_week==6~"From 25 to 29 hours",screentime_week==7~"From 30 to 34 hours",screentime_week==8~"From 35 to 39 hours",
                                    screentime_week==9~"From 40 to 44 hours",screentime_week==10~"45 hours or more",TRUE ~ NA_character_),
          sps_attachment=case_when(sps_attachment<=8~sps_attachment, TRUE ~ NA_integer_),
          sps_integration=case_when(sps_integration<=8~sps_integration, TRUE ~ NA_integer_))



######### CCHS_2011-2012 ~ Separating, Renaming and Cleaning ##########

# Extract just the rows in the two-year file that aren't present in the one-year file to make the first one-year file
cchs_pumf_2011 <- anti_join(cchs_pumf_2011_2012, cchs_pumf_2012, by = matching_variables)

cchs_2011 <- cchs_pumf_2011 %>%
  rowwise() %>%
  mutate(alt_tobacco = case_when(
    1 %in% c(TAL_1, TAL_2, TAL_3, TAL_4) ~ 1,
    all(c(TAL_1, TAL_2, TAL_3, TAL_4) == 2) ~ 2,
    TRUE ~ NA_real_
  )) %>%
  ungroup() %>%
  select(
    ls = GEN_02A2,
    province = GEOGPRV,
    sex = DHH_SEX,
    age = DHHGAGE,
    marital = DHHGMS,
    hh_income = INCGHH,
    hh_size = DHHGHSZ,
    language = SDCDFOLS,
    immigration_status = SDCFIMM,
    time_in_canada = SDCGRES,
    mental_health = GEN_02B,
    own_home = DHH_OWN,
    live_arrange = DHHGLVG, 
    minority = SDCGCGT,
    weight = WTS_M,
    education = EDUDR04,
    health = GEN_01,
    life_stress = GEN_07,
    work_stress = GEN_09,
    belonging = GEN_10,
    sleep_hours = SLPG01,
    sleep_refreshing = SLP_03,
    smoke = SMK_05C,
    alt_tobacco,
    alcohol = ALCDTTM,
    job_sat = SWL_02,
    leisure_sat = SWL_03,
    finance_sat = SWL_04,
    you_sat = SWL_05,
    body_sat = SWL_06,
    family_sat = SWL_07,
    friends_sat = SWL_08,
    housing_sat = SWL_09,
    neighbourhood_sat = SWL_10,
    employed = LBSG31,
    student = SDC_8,
    screentime_week=SACDTER,
    sps_attachment=SPSDATT,
    sps_integration=SPSDINT
  ) %>% 
  mutate( ls = case_when(ls <= 10 ~ ls, TRUE ~ NA_integer_),
          province = GEO_PRV_MAPPING_2$PRV_NAME[match(province, GEO_PRV_MAPPING$PRV_NUM)],
          sex = case_when(sex == 1 ~ "Male", sex == 2 ~ "Female", TRUE ~ NA_character_), 
          age = DHHGAGE_mapping$DHHGAGE_NAME[match(age, DHHGAGE_mapping$DHHGAGE_NUM)],
          marital = case_when(marital == 1 ~ "Married/Common-law", marital == 2 ~ "Married/Common-law", 
                              marital == 3 ~ "Widowed/Divorced/Separated", marital == 4 ~ "Single", TRUE ~ NA_character_),
          hh_income = case_when(hh_income <= 5 ~ hh_income, TRUE ~ NA_integer_),
          hh_size = case_when(hh_size == 9 ~ NA_integer_, TRUE ~ hh_size), 
          language = case_when(language == 1 ~ "English", language == 2 ~ "French", 
                                language == 3 ~ "English and French", language == 4 ~ "Neither English nor French", TRUE ~ NA_character_),
          immigration_status = case_when(immigration_status == 1 ~ "Landed immigrant/non-permanent resident", immigration_status == 2 ~ "Canadian born", TRUE ~ NA_character_),
          time_in_canada = case_when(time_in_canada == 1 ~ "0-9 years", time_in_canada == 2 ~ "10 or more years", 
                                      time_in_canada == 6 ~ "Canadian born", TRUE ~ NA_character_),
          mental_health = case_when(mental_health <= 5 ~ 5 - mental_health, TRUE ~ NA_integer_),
          live_arrange = case_when(live_arrange >= 1 & live_arrange <= 8 ~ as.numeric(live_arrange), TRUE ~ NA_integer_),
          own_home = case_when(
            live_arrange >= 1 & live_arrange <= 5 & own_home == 1 ~ "Owns home",
            is.na(live_arrange) | (own_home != 1 & own_home != 2) ~ NA_character_,
            TRUE ~ "Does not own home"
          ),
          minority = case_when(minority == 1 ~ "White", minority == 2 ~ "Non-white", minority == 6 ~ "Non-white", TRUE ~ NA_character_),
          education = case_when(education == 1 ~ "Less than secondary", education == 2 ~ "Secondary", education == 3 | education == 4 ~ "Post-secondary", TRUE ~ NA_character_),
          health = case_when(health <= 5 ~ 5 - health, TRUE ~ NA_integer_),
          life_stress = case_when(life_stress <= 5 ~ life_stress - 1, TRUE ~ NA_integer_),
          work_stress = case_when(work_stress <= 5 ~ work_stress - 1, TRUE ~ NA_integer_),
          belonging = case_when(belonging <= 4 ~ 4 - belonging, TRUE ~ NA_integer_),
          sleep_hours = case_when(sleep_hours == 1 ~ 1.5, (sleep_hours >= 2 & sleep_hours <=11) ~ sleep_hours + 1, TRUE ~ NA_integer_),
          sleep_refreshing = case_when(sleep_refreshing <= 5 ~ sleep_refreshing - 1, TRUE ~ NA_integer_),
          smoke = case_when(smoke > 0 & smoke <= 41 ~ 1, smoke == 0 ~ 2, smoke == 96 ~ 2, TRUE ~ NA_integer_),
          nicotine = case_when(smoke == 1 | alt_tobacco == 1 ~ "Yes", smoke == 2 & alt_tobacco == 2 ~ "No", TRUE ~ NA_character_),
          alcohol = case_when(alcohol == 1 ~ "Regular", alcohol == 2 ~ "Occasional", alcohol == 3 ~ "Did not drink in 12 mos"),
          job_sat = case_when(job_sat <= 5 ~ 5 - job_sat, TRUE ~ NA_integer_),
          leisure_sat = case_when(leisure_sat <= 5 ~ 5 - leisure_sat, TRUE ~ NA_integer_),
          finance_sat = case_when(finance_sat <= 5 ~ 5 - finance_sat, TRUE ~ NA_integer_),
          you_sat = case_when(you_sat <= 5 ~ 5 - you_sat, TRUE ~ NA_integer_),
          body_sat = case_when(body_sat <= 5 ~ 5 - body_sat, TRUE ~ NA_integer_),
          family_sat = case_when(family_sat <= 5 ~ 5 - family_sat, TRUE ~ NA_integer_),
          friends_sat = case_when(friends_sat <= 5 ~ 5 - friends_sat, TRUE ~ NA_integer_),
          housing_sat = case_when(housing_sat <= 5 ~ 5 - housing_sat, TRUE ~ NA_integer_),
          neighbourhood_sat = case_when(neighbourhood_sat <= 5 ~ 5 - neighbourhood_sat, TRUE ~ NA_integer_),
          employed = case_when(employed == 1 ~ "Employed", employed == 2 ~ "Self-employed", employed == 6 ~ "Not employed", TRUE ~ NA_character_),
          student = case_when(student == 1 ~ "Student", student == 2 ~ "Not a student", TRUE ~ NA_character_),
          screentime_week=case_when(screentime_week==1~"Less than 5 hours",screentime_week==2~"From 5 to 9 hours",screentime_week==3~"From 10 to 14 hours",screentime_week==4~"From 15 to 19 hours",
                                    screentime_week==5~"From 20 to 24 hours",screentime_week==6~"From 25 to 29 hours",screentime_week==7~"From 30 to 34 hours",screentime_week==8~"From 35 to 39 hours",
                                    screentime_week==9~"From 40 to 44 hours",screentime_week==10~"45 hours or more",TRUE ~ NA_character_),
          sps_attachment=case_when(sps_attachment<=8~sps_attachment, TRUE ~ NA_integer_),
          sps_integration=case_when(sps_integration<=8~sps_integration, TRUE ~ NA_integer_))


cchs_2012 <- cchs_pumf_2012 %>%
  rowwise() %>%
  mutate(alt_tobacco = case_when(
    1 %in% c(TAL_1, TAL_2, TAL_3, TAL_4) ~ 1,
    all(c(TAL_1, TAL_2, TAL_3, TAL_4) == 2) ~ 2,
    TRUE ~ NA_real_
  )) %>%
  ungroup() %>%
  select(
    ls = GEN_02A2,
    province = GEOGPRV,
    sex = DHH_SEX,
    age = DHHGAGE,
    marital = DHHGMS,
    hh_income = INCGHH,
    hh_size = DHHGHSZ,
    language = SDCDFOLS,
    immigration_status = SDCFIMM,
    time_in_canada = SDCGRES,
    mental_health = GEN_02B,
    own_home = DHH_OWN,
    live_arrange = DHHGLVG, 
    minority = SDCGCGT,
    weight = WTS_M,
    education = EDUDR04,
    health = GEN_01,
    life_stress = GEN_07,
    work_stress = GEN_09,
    belonging = GEN_10,
    sleep_hours = SLPG01,
    sleep_refreshing = SLP_03,
    smoke = SMK_05C,
    alt_tobacco,
    alcohol = ALCDTTM,
    job_sat = SWL_02,
    leisure_sat = SWL_03,
    finance_sat = SWL_04,
    you_sat = SWL_05,
    body_sat = SWL_06,
    family_sat = SWL_07,
    friends_sat = SWL_08,
    housing_sat = SWL_09,
    neighbourhood_sat = SWL_10,
    employed = LBSG31,
    student = SDC_8,
    screentime_week=SACDTER,
    sps_attachment=SPSDATT,
    sps_integration=SPSDINT
  ) %>% 
  mutate( ls = case_when(ls <= 10 ~ ls, TRUE ~ NA_integer_),
          province = GEO_PRV_MAPPING_2$PRV_NAME[match(province, GEO_PRV_MAPPING$PRV_NUM)],
          sex = case_when(sex == 1 ~ "Male", sex == 2 ~ "Female", TRUE ~ NA_character_), 
          age = DHHGAGE_mapping$DHHGAGE_NAME[match(age, DHHGAGE_mapping$DHHGAGE_NUM)],
          marital = case_when(marital == 1 ~ "Married/Common-law", marital == 2 ~ "Married/Common-law", 
                              marital == 3 ~ "Widowed/Divorced/Separated", marital == 4 ~ "Single", TRUE ~ NA_character_),
          hh_income = case_when(hh_income <= 5 ~ hh_income, TRUE ~ NA_integer_),
          hh_size = case_when(hh_size == 9 ~ NA_integer_, TRUE ~ hh_size), 
          language = case_when(language == 1 ~ "English", language == 2 ~ "French", 
                                language == 3 ~ "English and French", language == 4 ~ "Neither English nor French", TRUE ~ NA_character_),
          immigration_status = case_when(immigration_status == 1 ~ "Landed immigrant/non-permanent resident", immigration_status == 2 ~ "Canadian born", TRUE ~ NA_character_),
          time_in_canada = case_when(time_in_canada == 1 ~ "0-9 years", time_in_canada == 2 ~ "10 or more years", 
                                      time_in_canada == 6 ~ "Canadian born", TRUE ~ NA_character_),
          mental_health = case_when(mental_health <= 5 ~ 5 - mental_health, TRUE ~ NA_integer_),
          live_arrange = case_when(live_arrange >= 1 & live_arrange <= 8 ~ as.numeric(live_arrange), TRUE ~ NA_integer_),
          own_home = case_when(
            live_arrange >= 1 & live_arrange <= 5 & own_home == 1 ~ "Owns home",
            is.na(live_arrange) | (own_home != 1 & own_home != 2) ~ NA_character_,
            TRUE ~ "Does not own home"
          ),
          minority = case_when(minority == 1 ~ "White", minority == 2 ~ "Non-white", minority == 6 ~ "Non-white", TRUE ~ NA_character_),
          education = case_when(education == 1 ~ "Less than secondary", education == 2 ~ "Secondary", education == 3 | education == 4 ~ "Post-secondary", TRUE ~ NA_character_),
          health = case_when(health <= 5 ~ 5 - health, TRUE ~ NA_integer_),
          life_stress = case_when(life_stress <= 5 ~ life_stress - 1, TRUE ~ NA_integer_),
          work_stress = case_when(work_stress <= 5 ~ work_stress - 1, TRUE ~ NA_integer_),
          belonging = case_when(belonging <= 4 ~ 4 - belonging, TRUE ~ NA_integer_),
          sleep_hours = case_when(sleep_hours == 1 ~ 1.5, (sleep_hours >= 2 & sleep_hours <=11) ~ sleep_hours + 1, TRUE ~ NA_integer_),
          sleep_refreshing = case_when(sleep_refreshing <= 5 ~ sleep_refreshing - 1, TRUE ~ NA_integer_),
          smoke = case_when(smoke > 0 & smoke <= 41 ~ 1, smoke == 0 ~ 2, smoke == 96 ~ 2, TRUE ~ NA_integer_),
          nicotine = case_when(smoke == 1 | alt_tobacco == 1 ~ "Yes", smoke == 2 & alt_tobacco == 2 ~ "No", TRUE ~ NA_character_),
          alcohol = case_when(alcohol == 1 ~ "Regular", alcohol == 2 ~ "Occasional", alcohol == 3 ~ "Did not drink in 12 mos"),
          job_sat = case_when(job_sat <= 5 ~ 5 - job_sat, TRUE ~ NA_integer_),
          leisure_sat = case_when(leisure_sat <= 5 ~ 5 - leisure_sat, TRUE ~ NA_integer_),
          finance_sat = case_when(finance_sat <= 5 ~ 5 - finance_sat, TRUE ~ NA_integer_),
          you_sat = case_when(you_sat <= 5 ~ 5 - you_sat, TRUE ~ NA_integer_),
          body_sat = case_when(body_sat <= 5 ~ 5 - body_sat, TRUE ~ NA_integer_),
          family_sat = case_when(family_sat <= 5 ~ 5 - family_sat, TRUE ~ NA_integer_),
          friends_sat = case_when(friends_sat <= 5 ~ 5 - friends_sat, TRUE ~ NA_integer_),
          housing_sat = case_when(housing_sat <= 5 ~ 5 - housing_sat, TRUE ~ NA_integer_),
          neighbourhood_sat = case_when(neighbourhood_sat <= 5 ~ 5 - neighbourhood_sat, TRUE ~ NA_integer_),
          employed = case_when(employed == 1 ~ "Employed", employed == 2 ~ "Self-employed", employed == 6 ~ "Not employed", TRUE ~ NA_character_),
          student = case_when(student == 1 ~ "Student", student == 2 ~ "Not a student", TRUE ~ NA_character_),
          screentime_week=case_when(screentime_week==1~"Less than 5 hours",screentime_week==2~"From 5 to 9 hours",screentime_week==3~"From 10 to 14 hours",screentime_week==4~"From 15 to 19 hours",
                                    screentime_week==5~"From 20 to 24 hours",screentime_week==6~"From 25 to 29 hours",screentime_week==7~"From 30 to 34 hours",screentime_week==8~"From 35 to 39 hours",
                                    screentime_week==9~"From 40 to 44 hours",screentime_week==10~"45 hours or more",TRUE ~ NA_character_),
          sps_attachment=case_when(sps_attachment<=8~sps_attachment, TRUE ~ NA_integer_),
          sps_integration=case_when(sps_integration<=8~sps_integration, TRUE ~ NA_integer_))


######### CCHS_2009-2010 ~ Separating, Renaming and Cleaning ##########

# Extract just the rows in the two-year file that aren't present in the one-year file to make the first one-year file
cchs_pumf_2009 <- anti_join(cchs_pumf_2009_2010, cchs_pumf_2010, by = matching_variables)

cchs_2009 <- cchs_pumf_2009 %>%
  rowwise() %>%
  mutate(alt_tobacco = case_when(
    1 %in% c(TAL_1, TAL_2, TAL_3, TAL_4) ~ 1,
    all(c(TAL_1, TAL_2, TAL_3, TAL_4) == 2) ~ 2,
    TRUE ~ NA_real_
  )) %>%
  ungroup() %>%
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
    live_arrange = DHHGLVG, 
    minority = SDCGCGT,
    weight = WTS_M,
    education = EDUDR04,
    health = GEN_01,
    life_stress = GEN_07,
    work_stress = GEN_09,
    belonging = GEN_10,
    smoke = SMK_05C,
    alt_tobacco,
    alcohol = ALCDTTM,
    job_sat = SWL_02,
    leisure_sat = SWL_03,
    finance_sat = SWL_04,
    you_sat = SWL_05,
    body_sat = SWL_06,
    family_sat = SWL_07,
    friends_sat = SWL_08,
    housing_sat = SWL_09,
    neighbourhood_sat = SWL_10,
    employed = LBSG31,
    student = SDC_8,
    screentime_week=SACDTER
  ) %>% 
  mutate( ls = case_when(ls <= 10 ~ ls, TRUE ~ NA_integer_),
          province = GEO_PRV_MAPPING_2$PRV_NAME[match(province, GEO_PRV_MAPPING$PRV_NUM)],
          sex = case_when(sex == 1 ~ "Male", sex == 2 ~ "Female", TRUE ~ NA_character_), 
          age = DHHGAGE_mapping$DHHGAGE_NAME[match(age, DHHGAGE_mapping$DHHGAGE_NUM)],
          marital = case_when(marital == 1 ~ "Married/Common-law", marital == 2 ~ "Married/Common-law", 
                              marital == 3 ~ "Widowed/Divorced/Separated", marital == 4 ~ "Single", TRUE ~ NA_character_),
          hh_income = case_when(hh_income <= 5 ~ hh_income, TRUE ~ NA_integer_),
          hh_size = case_when(hh_size == 9 ~ NA_integer_, TRUE ~ hh_size), 
          immigration_status = case_when(immigration_status == 1 ~ "Landed immigrant/non-permanent resident", immigration_status == 2 ~ "Canadian born", TRUE ~ NA_character_),
          time_in_canada = case_when(time_in_canada == 1 ~ "0-9 years", time_in_canada == 2 ~ "10 or more years", 
                                      time_in_canada == 6 ~ "Canadian born", TRUE ~ NA_character_),
          mental_health = case_when(mental_health <= 5 ~ 5 - mental_health, TRUE ~ NA_integer_),
          live_arrange = case_when(live_arrange >= 1 & live_arrange <= 8 ~ as.numeric(live_arrange), TRUE ~ NA_integer_),
          own_home = case_when(
            live_arrange >= 1 & live_arrange <= 5 & own_home == 1 ~ "Owns home",
            is.na(live_arrange) | (own_home != 1 & own_home != 2) ~ NA_character_,
            TRUE ~ "Does not own home"
          ),
          minority = case_when(minority == 1 ~ "White", minority == 2 ~ "Non-white", minority == 6 ~ "Non-white", TRUE ~ NA_character_),
          education = case_when(education == 1 ~ "Less than secondary", education == 2 ~ "Secondary", education == 3 | education == 4 ~ "Post-secondary", TRUE ~ NA_character_),
          health = case_when(health <= 5 ~ 5 - health, TRUE ~ NA_integer_),
          life_stress = case_when(life_stress <= 5 ~ life_stress - 1, TRUE ~ NA_integer_),
          work_stress = case_when(work_stress <= 5 ~ work_stress - 1, TRUE ~ NA_integer_),
          belonging = case_when(belonging <= 4 ~ 4 - belonging, TRUE ~ NA_integer_),
          smoke = case_when(smoke > 0 & smoke <= 41 ~ 1, smoke == 0 ~ 2, smoke == 96 ~ 2, TRUE ~ NA_integer_),
          nicotine = case_when(smoke == 1 | alt_tobacco == 1 ~ "Yes", smoke == 2 & alt_tobacco == 2 ~ "No", TRUE ~ NA_character_),
          alcohol = case_when(alcohol == 1 ~ "Regular", alcohol == 2 ~ "Occasional", alcohol == 3 ~ "Did not drink in 12 mos"),
          job_sat = case_when(job_sat <= 5 ~ 5 - job_sat, TRUE ~ NA_integer_),
          leisure_sat = case_when(leisure_sat <= 5 ~ 5 - leisure_sat, TRUE ~ NA_integer_),
          finance_sat = case_when(finance_sat <= 5 ~ 5 - finance_sat, TRUE ~ NA_integer_),
          you_sat = case_when(you_sat <= 5 ~ 5 - you_sat, TRUE ~ NA_integer_),
          body_sat = case_when(body_sat <= 5 ~ 5 - body_sat, TRUE ~ NA_integer_),
          family_sat = case_when(family_sat <= 5 ~ 5 - family_sat, TRUE ~ NA_integer_),
          friends_sat = case_when(friends_sat <= 5 ~ 5 - friends_sat, TRUE ~ NA_integer_),
          housing_sat = case_when(housing_sat <= 5 ~ 5 - housing_sat, TRUE ~ NA_integer_),
          neighbourhood_sat = case_when(neighbourhood_sat <= 5 ~ 5 - neighbourhood_sat, TRUE ~ NA_integer_),
          employed = case_when(employed == 1 ~ "Employed", employed == 2 ~ "Self-employed", employed == 6 ~ "Not employed", TRUE ~ NA_character_),
          student = case_when(student == 1 ~ "Student", student == 2 ~ "Not a student", TRUE ~ NA_character_),
          screentime_week=case_when(screentime_week==1~"Less than 5 hours",screentime_week==2~"From 5 to 9 hours",screentime_week==3~"From 10 to 14 hours",screentime_week==4~"From 15 to 19 hours",
                                    screentime_week==5~"From 20 to 24 hours",screentime_week==6~"From 25 to 29 hours",screentime_week==7~"From 30 to 34 hours",screentime_week==8~"From 35 to 39 hours",
                                    screentime_week==9~"From 40 to 44 hours",screentime_week==10~"45 hours or more",TRUE ~ NA_character_))

cchs_2010 <- cchs_pumf_2010 %>%
  rowwise() %>%
  mutate(alt_tobacco = case_when(
    1 %in% c(TAL_1, TAL_2, TAL_3, TAL_4) ~ 1,
    all(c(TAL_1, TAL_2, TAL_3, TAL_4) == 2) ~ 2,
    TRUE ~ NA_real_
  )) %>%
  ungroup() %>%
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
    live_arrange = DHHGLVG, 
    minority = SDCGCGT,
    weight = WTS_M,
    education = EDUDR04,
    health = GEN_01,
    life_stress = GEN_07,
    work_stress = GEN_09,
    belonging = GEN_10,
    smoke = SMK_05C,
    alt_tobacco,
    alcohol = ALCDTTM,
    job_sat = SWL_02,
    leisure_sat = SWL_03,
    finance_sat = SWL_04,
    you_sat = SWL_05,
    body_sat = SWL_06,
    family_sat = SWL_07,
    friends_sat = SWL_08,
    housing_sat = SWL_09,
    neighbourhood_sat = SWL_10,
    employed = LBSG31,
    student = SDC_8,
    screentime_week=SACDTER
  ) %>% 
  mutate( ls = case_when(ls <= 10 ~ ls, TRUE ~ NA_integer_),
          province = GEO_PRV_MAPPING_2$PRV_NAME[match(province, GEO_PRV_MAPPING$PRV_NUM)],
          sex = case_when(sex == 1 ~ "Male", sex == 2 ~ "Female", TRUE ~ NA_character_), 
          age = DHHGAGE_mapping$DHHGAGE_NAME[match(age, DHHGAGE_mapping$DHHGAGE_NUM)],
          marital = case_when(marital == 1 ~ "Married/Common-law", marital == 2 ~ "Married/Common-law", 
                              marital == 3 ~ "Widowed/Divorced/Separated", marital == 4 ~ "Single", TRUE ~ NA_character_),
          hh_income = case_when(hh_income <= 5 ~ hh_income, TRUE ~ NA_integer_),
          hh_size = case_when(hh_size == 9 ~ NA_integer_, TRUE ~ hh_size), 
          immigration_status = case_when(immigration_status == 1 ~ "Landed immigrant/non-permanent resident", immigration_status == 2 ~ "Canadian born", TRUE ~ NA_character_),
          time_in_canada = case_when(time_in_canada == 1 ~ "0-9 years", time_in_canada == 2 ~ "10 or more years", 
                                      time_in_canada == 6 ~ "Canadian born", TRUE ~ NA_character_),
          mental_health = case_when(mental_health <= 5 ~ 5 - mental_health, TRUE ~ NA_integer_),
          live_arrange = case_when(live_arrange >= 1 & live_arrange <= 8 ~ as.numeric(live_arrange), TRUE ~ NA_integer_),
          own_home = case_when(
            live_arrange >= 1 & live_arrange <= 5 & own_home == 1 ~ "Owns home",
            is.na(live_arrange) | (own_home != 1 & own_home != 2) ~ NA_character_,
            TRUE ~ "Does not own home"
          ),
          minority = case_when(minority == 1 ~ "White", minority == 2 ~ "Non-white", minority == 6 ~ "Non-white", TRUE ~ NA_character_),
          education = case_when(education == 1 ~ "Less than secondary", education == 2 ~ "Secondary", education == 3 | education == 4 ~ "Post-secondary", TRUE ~ NA_character_),
          health = case_when(health <= 5 ~ 5 - health, TRUE ~ NA_integer_),
          life_stress = case_when(life_stress <= 5 ~ life_stress - 1, TRUE ~ NA_integer_),
          work_stress = case_when(work_stress <= 5 ~ work_stress - 1, TRUE ~ NA_integer_),
          belonging = case_when(belonging <= 4 ~ 4 - belonging, TRUE ~ NA_integer_),
          smoke = case_when(smoke > 0 & smoke <= 41 ~ 1, smoke == 0 ~ 2, smoke == 96 ~ 2, TRUE ~ NA_integer_),
          nicotine = case_when(smoke == 1 | alt_tobacco == 1 ~ "Yes", smoke == 2 & alt_tobacco == 2 ~ "No", TRUE ~ NA_character_),
          alcohol = case_when(alcohol == 1 ~ "Regular", alcohol == 2 ~ "Occasional", alcohol == 3 ~ "Did not drink in 12 mos"),
          job_sat = case_when(job_sat <= 5 ~ 5 - job_sat, TRUE ~ NA_integer_),
          leisure_sat = case_when(leisure_sat <= 5 ~ 5 - leisure_sat, TRUE ~ NA_integer_),
          finance_sat = case_when(finance_sat <= 5 ~ 5 - finance_sat, TRUE ~ NA_integer_),
          you_sat = case_when(you_sat <= 5 ~ 5 - you_sat, TRUE ~ NA_integer_),
          body_sat = case_when(body_sat <= 5 ~ 5 - body_sat, TRUE ~ NA_integer_),
          family_sat = case_when(family_sat <= 5 ~ 5 - family_sat, TRUE ~ NA_integer_),
          friends_sat = case_when(friends_sat <= 5 ~ 5 - friends_sat, TRUE ~ NA_integer_),
          housing_sat = case_when(housing_sat <= 5 ~ 5 - housing_sat, TRUE ~ NA_integer_),
          neighbourhood_sat = case_when(neighbourhood_sat <= 5 ~ 5 - neighbourhood_sat, TRUE ~ NA_integer_),
          employed = case_when(employed == 1 ~ "Employed", employed == 2 ~ "Self-employed", employed == 6 ~ "Not employed", TRUE ~ NA_character_),
          student = case_when(student == 1 ~ "Student", student == 2 ~ "Not a student", TRUE ~ NA_character_),
          screentime_week=case_when(screentime_week==1~"Less than 5 hours",screentime_week==2~"From 5 to 9 hours",screentime_week==3~"From 10 to 14 hours",screentime_week==4~"From 15 to 19 hours",
                                    screentime_week==5~"From 20 to 24 hours",screentime_week==6~"From 25 to 29 hours",screentime_week==7~"From 30 to 34 hours",screentime_week==8~"From 35 to 39 hours",
                                    screentime_week==9~"From 40 to 44 hours",screentime_week==10~"45 hours or more",TRUE ~ NA_character_))


######### Combine the Dataframes and Save ########

# Combine the data frames for the years 2009 to 2014 with the combined data frames for 2015/2016 and 2017/2018 into one data frame
cchs <- bind_rows(
  cchs_2009 %>% mutate(year = "2009", graphic_year = 2009),
  cchs_2010 %>% mutate(year = "2010", graphic_year = 2010),
  cchs_2011 %>% mutate(year = "2011", graphic_year = 2011),
  cchs_2012 %>% mutate(year = "2012", graphic_year = 2012),
  cchs_2013 %>% mutate(year = "2013", graphic_year = 2013),
  cchs_2014 %>% mutate(year = "2014", graphic_year = 2014),
  cchs_2015_2016 %>% mutate(year = "2015/2016", graphic_year = 2015.5),
  cchs_2017_2018 %>% mutate(year = "2017/2018", graphic_year = 2017.5)
)
write.csv(cchs, file = "Data/combined_cchs_data.csv", row.names = FALSE)

rm(list=ls(pattern="^cchs_"))

save.image("CCHS_harmonization.RData") 
load("CCHS_harmonization.RData")



######################### Variable Processing #############################

income_labels <- c("≤$20,000" = 1, "$20,000-$39,999" = 2, "$40,000-$59,999" = 3, "$60,000-$79,999" = 4, "$80,000 or more" = 5)

cchs<- read.csv("Data/combined_cchs_data.csv") %>%
  # Process income variables, by assigning the median income value to each range, then calculating equivalized household income
  mutate( hh_income_medians = case_when(hh_income == 1 ~ 10000, 
                                        hh_income == 2 ~ 30000,
                                        hh_income == 3 ~ 50000,
                                        hh_income == 4 ~ 70000,
                                        hh_income == 5 ~ 100000),
          equiv_income = hh_income_medians / sqrt(hh_size)) %>%
  # Process ages into pre-specified age ranges
  mutate(age_ranges = case_when(
    age %in% c("15-17", "18-19", "20-24", "25-29") ~ "15-29",
    age %in% c("30-34", "35-39", "40-44") ~ "30-44",
    age %in% c("45-49", "50-54", "55-59") ~ "45-59",
    age %in% c("60-64", "65-69", "70-74", "75-79", "80+") ~ "60+",
    TRUE ~ NA_character_
  ))

######################### Province Table Generation #############################

# Group by year, province, and each individual value of ls (life satisfaction)
# Then calculate the weighted frequency of each 0-10 value of ls in each year and each province
province <- cchs %>%
  drop_na(ls) %>%
  group_by(year, province, ls) %>%
  summarise(frequency = sum(weight, na.rm = TRUE)) %>%
  ungroup() %>% 
  bind_rows(
      group_by(., year, ls) %>%
      summarise(frequency = sum(frequency, na.rm = TRUE), .groups = 'drop') %>%
      mutate(province = "Canada")
  )

years <- unique(province$year)

for (yr in years) {
  filename_year <- gsub("/", "", yr) # Remove "/" from yr
  filename_year <- gsub("20", "", filename_year, fixed = TRUE) # Remove "20" from yr
  subset_data <- province %>% filter(year == yr)
  write.csv(subset_data, file = paste0("Output/PROVxLS Tables CCHS/PROVxLS_", filename_year, "_CCHS.csv"), row.names = FALSE)
}

######################### Province X Age Table Generation #############################

provinceXage <- cchs %>%
  drop_na(ls, age_ranges) %>%
  group_by(year, province, ls, age_ranges) %>%
  summarise(frequency = sum(weight, na.rm = TRUE)) %>%
  ungroup() %>% 
  bind_rows(
    group_by(., year, province, ls) %>%
    summarise(frequency = sum(frequency, na.rm = TRUE), .groups = 'drop') %>%
    mutate(age_ranges = "All ages")
  ) %>%
  bind_rows(
      group_by(., year, age_ranges, ls) %>%
      summarise(frequency = sum(frequency, na.rm = TRUE), .groups = 'drop') %>%
      mutate(province = "Canada")
  ) %>%
  arrange(year, province, age_ranges, ls)

years <- unique(provinceXage$year)

for (yr in years) {
  filename_year <- gsub("/", "", yr) # Remove "/" from yr
  filename_year <- gsub("20", "", filename_year, fixed = TRUE) # Remove "20" from yr
  subset_data <- provinceXage %>% filter(year == yr)
  write.csv(subset_data, file = paste0("Output/PROVxAGExLS Tables CCHS/PROVxAGExLS_", filename_year, "_CCHS.csv"), row.names = FALSE)
}










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

# Assuming 'dataframe1' and 'dataframe2' are the two dataframes we want to compare
matching_columns <- intersect(names(cchs_pumf_2013_2014), names(cchs_pumf_2014))

# Print out the matching column names
print(matching_columns)

matching_columns <- matching_columns[!matching_columns %in% c("WTS_M", "VERDATE")]

# Find all rows that match on all identified columns
matching_rows <- inner_join(cchs_pumf_2009_2010, cchs_pumf_2010, by = c("GEOGPRV", "DHHGAGE", "DHH_SEX", "GEODPMF", "DHHGLE5", 
"DHHG611", "DHHGL12", "DHHGLVG", "DHHGHSZ", "GEN_01", "GEN_02", "GEN_02A2", "GEN_02B", "GEN_07", "GEN_08", "GEN_09", "GEN_10", 
"GENDHDI", "GENDMHI", "GENGSWL", 
"CIH_1", "CIH_2", "CIH_3", "CIH_4", "CIH_5", "CIH_6A", "CIH_6I", 
"CIH_6B", "CIH_6J", "CIH_6K", "CIH_6G", "CIH_6F", "CIH_6E", "CIH_6L", 
"CIH_6M", "CIH_6N", "CIH_6H", "CIH_7", "CIH_8A", "CIH_8B", "CIH_8C", 
"CIH_8J", "CIH_8K", "CIH_8G", "CIH_8L", "CIH_8H", "CIH_8I", 
"HCS_1", "HCS_2", "HCS_3", "HCS_4", "HWT_4", "HWTGHTM", "HWTGWTK", 
"HWTGBMI", "HWTGISW", "HWTDCOL", "CCC_031", "CCC_035", "CCC_036", 
"CCC_051", "CCC_061", "CCC_071", "CCC_072", "CCC_073", "CCC_073A", 
"CCC_073B", "CCC_081", "CCC_091", "CCC_101", "CCCG102", "CCC_10A", 
"CCC_10B", "CCC_10C", "CCC_105", "CCC_106", "CCC_121", "CCC_131", 
"CCC_31A", "CCC_141", "CCC_151", "CCC_161", "CCC_171", "CCC_17A", 
"CCC_280", "CCC_290", "CCCDDIA"))
# Save the result in a new dataframe
matched_dataframe <- matching_rows
cchs