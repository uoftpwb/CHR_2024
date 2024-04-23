# CCHS Analyses for Canadian Happiness Report
# Author: Phyllis Lun 
# Date: April 19, 2024
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

# Reading CCHS data----------
# # Set the path to the directory containing the CSV files
# directory <- "~/Documents/*UoT/PWB/YMH/CCHS/Data"
# 
# # Get the list of CSV files in the directory
# csv_files <- list.files(path = directory, pattern = "\\.csv$", full.names = TRUE)
# 
# # Loop through each file and read it into a data frame
# for (file in csv_files) {
#   # Extract the file name without the extension
#   file_name <- tools::file_path_sans_ext(basename(file))
#   
#   # Read the CSV file into a data frame
#   df <- read.csv(file)
#   
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
cchs2017_2018_extracted<-cchs2017_2018 %>% 
  select(GEN_010,GEO_PRV,DHH_SEX,DHHGAGE,DHHGMS,INCDGHH,DHHDGHSZ,SDCDVFLS,SDCDVIMM,SDCDGRES,GEN_015, DHH_OWN,SDC_015,
    SDCDGCGT,WTS_M,SDC_035)

cchs2017_2018_extracted<-cchs2017_2018_extracted %>% 
  mutate (GEN_010 = case_when(GEN_010 %in% c(97, 98, 99) ~ NA_integer_,TRUE ~ GEN_010),
          GEO_PRV=GEO_PRV_MAPPING$PRV_NAME[match(GEO_PRV,GEO_PRV_MAPPING$PRV_NUM)],
          DHH_SEX = case_when(DHH_SEX == 1 ~ "Male", DHH_SEX == 2 ~ "Female", TRUE ~ NA_character_), 
          DHHGAGE=DHHGAGE_mapping$DHHGAGE_NAME[match(DHHGAGE,DHHGAGE_mapping$DHHGAGE_NUM)],
          DHHGMS = case_when(DHHGMS == 1 ~ "Married/Common-law", DHHGMS == 2 ~ "Married/Common-law", 
                             DHHGMS == 3 ~ "Widowed/Divorced/Separated", DHHGMS == 4 ~ "Single",TRUE ~ NA_character_),
          INCDGHH=INCDGHH_mapping$INCDGHH_NAME[match(INCDGHH,INCDGHH_mapping$INCDGHH_NUM)],
          DHHDGHSZ=case_when(DHHDGHSZ ==9 ~ NA_integer_,TRUE ~ DHHDGHSZ), 
          SDCDVFLS= case_when(SDCDVFLS == 1 ~ "English", SDCDVFLS == 2 ~ "French", 
                               SDCDVFLS == 3 ~ "English and French", SDCDVFLS == 4 ~ "Neither English nor French",TRUE ~ NA_character_) ,
          SDCDVIMM= case_when(SDCDVIMM == 1 ~ "Landed immigrant/non-permanent resident", SDCDVIMM == 2 ~ "Canadian born", TRUE ~ NA_character_),
          SDCDGRES=case_when(SDCDGRES == 1 ~ "0-9 years", SDCDGRES == 2 ~ "10 or more years", 
                             SDCDGRES == 6 ~ "Canadian born", TRUE ~ NA_character_),
          GEN_015=case_when(GEN_015 == 1 ~ "Excellent", GEN_015 == 2 ~ "Very good", 
                             GEN_015 == 3 ~ "Good", GEN_015 == 4 ~ "Fair",
                             GEN_015 == 5 ~ "Poor",TRUE ~ NA_character_),
          DHH_OWN= case_when(DHH_OWN == 1 ~ "Owned by household members", DHH_OWN == 2 ~ "Rented by household members", TRUE ~ NA_character_),
          SDC_015=case_when(SDC_015 == 1 ~ "Yes", SDC_015 == 2 ~ "No", SDC_015 == 6 ~ "No", TRUE ~ NA_character_),
          SDCDGCGT= case_when(SDCDGCGT == 1 ~ "White", SDCDGCGT == 2 ~ "Non-white", TRUE ~ NA_character_),# Visible Minority: check what valid skip (6) stands for here
          SDC_035= case_when(SDC_035 == 1 ~ "Heterosexual", SDC_035 == 2 ~ "Sexual minorities", 
                             SDC_035 == 3 ~ "Sexual minorities", TRUE ~ NA_character_))

#CCHS_2015-2016-----------
cchs2015_2016_extracted<-cchs2015_2016 %>% 
  select(GEN_010,GEO_PRV,DHH_SEX,DHHGAGE,DHHGMS,INCDGHH,DHHDGHSZ,SDCDVFLS,SDCDVIMM,SDCDGRES,GEN_015, DHH_OWN,SDC_015,
         SDCDGCGT,WTS_M,SDC_035)

cchs2015_2016_extracted<-cchs2015_2016_extracted %>% 
  mutate (GEN_010 = case_when(GEN_010 %in% c(97, 98, 99) ~ NA_integer_,TRUE ~ GEN_010),
          GEO_PRV=GEO_PRV_MAPPING_2$PRV_NAME[match(GEO_PRV,GEO_PRV_MAPPING$PRV_NUM)],
          DHH_SEX = case_when(DHH_SEX == 1 ~ "Male", DHH_SEX == 2 ~ "Female", TRUE ~ NA_character_), 
          DHHGAGE=DHHGAGE_mapping$DHHGAGE_NAME[match(DHHGAGE,DHHGAGE_mapping$DHHGAGE_NUM)],
          DHHGMS = case_when(DHHGMS == 1 ~ "Married/Common-law", DHHGMS == 2 ~ "Married/Common-law", 
                             DHHGMS == 3 ~ "Widowed/Divorced/Separated", DHHGMS == 4 ~ "Single",TRUE ~ NA_character_),
          INCDGHH=INCDGHH_mapping$INCDGHH_NAME[match(INCDGHH,INCDGHH_mapping$INCDGHH_NUM)],
          DHHDGHSZ=case_when(DHHDGHSZ ==9 ~ NA_integer_,TRUE ~ DHHDGHSZ), 
          SDCDVFLS= case_when(SDCDVFLS == 1 ~ "English", SDCDVFLS == 2 ~ "French", 
                              SDCDVFLS == 3 ~ "English and French", SDCDVFLS == 4 ~ "Neither English nor French",TRUE ~ NA_character_) ,
          SDCDVIMM= case_when(SDCDVIMM == 1 ~ "Landed immigrant/non-permanent resident", SDCDVIMM == 2 ~ "Canadian born", TRUE ~ NA_character_),
          SDCDGRES=case_when(SDCDGRES == 1 ~ "0-9 years", SDCDGRES == 2 ~ "10 or more years", 
                             SDCDGRES == 6 ~ "Canadian born", TRUE ~ NA_character_),
          GEN_015=case_when(GEN_015 == 1 ~ "Excellent", GEN_015 == 2 ~ "Very good", 
                            GEN_015 == 3 ~ "Good", GEN_015 == 4 ~ "Fair",
                            GEN_015 == 5 ~ "Poor",TRUE ~ NA_character_),
          DHH_OWN= case_when(DHH_OWN == 1 ~ "Owned by household members", DHH_OWN == 2 ~ "Rented by household members", TRUE ~ NA_character_),
          SDC_015=case_when(SDC_015 == 1 ~ "Yes", SDC_015 == 2 ~ "No", SDC_015 == 6 ~ "No", TRUE ~ NA_character_),
          SDCDGCGT= case_when(SDCDGCGT == 1 ~ "White", SDCDGCGT == 2 ~ "Non-white", TRUE ~ NA_character_),# Visible Minority: check what valid skip (6) stands for here
          SDC_035= case_when(SDC_035 == 1 ~ "Heterosexual", SDC_035 == 2 ~ "Sexual minorities", 
                             SDC_035 == 3 ~ "Sexual minorities", TRUE ~ NA_character_))

#CCHS_2013-2014----------
cchs2013_2014_extracted<-cchs2013_2014 %>% 
  select(GEN_02A2,GEOGPRV,DHH_SEX,DHHGAGE,DHHGMS,INCGHH,DHHGHSZ,SDCDFOLS,SDCFIMM,SDCGRES,GEN_02B, DHH_OWN,
         SDCGCGT,WTS_M)

cchs2013_2014_extracted<-cchs2013_2014_extracted %>% 
  mutate (GEN_02A2 = case_when(GEN_02A2 %in% c(97, 98, 99) ~ NA_integer_,TRUE ~ GEN_02A2),
          GEOGPRV=GEO_PRV_MAPPING_2$PRV_NAME[match(GEOGPRV,GEO_PRV_MAPPING$PRV_NUM)],
          DHH_SEX = case_when(DHH_SEX == 1 ~ "Male", DHH_SEX == 2 ~ "Female", TRUE ~ NA_character_), 
          INCGHH=DHHGAGE_mapping$DHHGAGE_NAME[match(INCGHH,DHHGAGE_mapping$DHHGAGE_NUM)],
          DHHGMS = case_when(DHHGMS == 1 ~ "Married/Common-law", DHHGMS == 2 ~ "Married/Common-law", 
                             DHHGMS == 3 ~ "Widowed/Divorced/Separated", DHHGMS == 4 ~ "Single",TRUE ~ NA_character_),
          DHHGHSZ=case_when(DHHGHSZ ==9 ~ NA_integer_,TRUE ~ DHHGHSZ), 
          SDCDFOLS= case_when(SDCDFOLS == 1 ~ "English", SDCDFOLS == 2 ~ "French", 
                              SDCDFOLS == 3 ~ "English and French", SDCDFOLS == 4 ~ "Neither English nor French",TRUE ~ NA_character_) ,
          SDCFIMM= case_when(SDCFIMM == 1 ~ "Landed immigrant/non-permanent resident", SDCFIMM == 2 ~ "Canadian born", TRUE ~ NA_character_),
          SDCGRES=case_when(SDCGRES == 1 ~ "0-9 years", SDCGRES == 2 ~ "10 or more years", 
                            SDCGRES == 6 ~ "Canadian born", TRUE ~ NA_character_),
          GEN_02B=case_when(GEN_02B == 1 ~ "Excellent", GEN_02B == 2 ~ "Very good", 
                            GEN_02B == 3 ~ "Good", GEN_02B == 4 ~ "Fair",
                            GEN_02B == 5 ~ "Poor",TRUE ~ NA_character_),
          DHH_OWN= case_when(DHH_OWN == 1 ~ "Owned by household members", DHH_OWN == 2 ~ "Rented by household members", TRUE ~ NA_character_),
          #SDC_015=case_when(SDC_015 == 1 ~ "Yes", SDC_015 == 2 ~ "No", SDC_015 == 6 ~ "No", TRUE ~ NA_character_),
          SDCGCGT= case_when(SDCGCGT == 1 ~ "White", SDCGCGT == 2 ~ "Non-white", TRUE ~ NA_character_),# Visible Minority: check what valid skip (6) stands for here
          #SDC_035= case_when(SDC_035 == 1 ~ "Heterosexual", SDC_035 == 2 ~ "Sexual minorities", 
          #                   SDC_035 == 3 ~ "Sexual minorities", TRUE ~ NA_character_)
)

##CCHS_2011-2012----------
cchs2011_2012_extracted<-cchs2011_2012 %>% 
  select(GEN_02A2,GEOGPRV,DHH_SEX,DHHGAGE,DHHGMS,INCGHH,DHHGHSZ,SDCDFOLS,SDCFIMM,SDCGRES,GEN_02B, DHH_OWN,#SDC_015,
         SDCGCGT,WTS_M)#SDC_035

cchs2011_2012_extracted<-cchs2011_2012_extracted %>% 
  mutate (GEN_02A2 = case_when(GEN_02A2 %in% c(97, 98, 99) ~ NA_integer_,TRUE ~ GEN_02A2),
          GEOGPRV=GEO_PRV_MAPPING_2$PRV_NAME[match(GEOGPRV,GEO_PRV_MAPPING$PRV_NUM)],
          DHH_SEX = case_when(DHH_SEX == 1 ~ "Male", DHH_SEX == 2 ~ "Female", TRUE ~ NA_character_), 
          INCGHH=DHHGAGE_mapping$DHHGAGE_NAME[match(INCGHH,DHHGAGE_mapping$DHHGAGE_NUM)],
          DHHGMS = case_when(DHHGMS == 1 ~ "Married/Common-law", DHHGMS == 2 ~ "Married/Common-law", 
                             DHHGMS == 3 ~ "Widowed/Divorced/Separated", DHHGMS == 4 ~ "Single",TRUE ~ NA_character_),
          DHHGHSZ=case_when(DHHGHSZ ==9 ~ NA_integer_,TRUE ~ DHHGHSZ), 
          SDCDFOLS= case_when(SDCDFOLS == 1 ~ "English", SDCDFOLS == 2 ~ "French", 
                              SDCDFOLS == 3 ~ "English and French", SDCDFOLS == 4 ~ "Neither English nor French",TRUE ~ NA_character_) ,
          SDCFIMM= case_when(SDCFIMM == 1 ~ "Landed immigrant/non-permanent resident", SDCFIMM == 2 ~ "Canadian born", TRUE ~ NA_character_),
          SDCGRES=case_when(SDCGRES == 1 ~ "0-9 years", SDCGRES == 2 ~ "10 or more years", 
                            SDCGRES == 6 ~ "Canadian born", TRUE ~ NA_character_),
          GEN_02B=case_when(GEN_02B == 1 ~ "Excellent", GEN_02B == 2 ~ "Very good", 
                            GEN_02B == 3 ~ "Good", GEN_02B == 4 ~ "Fair",
                            GEN_02B == 5 ~ "Poor",TRUE ~ NA_character_),
          DHH_OWN= case_when(DHH_OWN == 1 ~ "Owned by household members", DHH_OWN == 2 ~ "Rented by household members", TRUE ~ NA_character_),
          #SDC_015=case_when(SDC_015 == 1 ~ "Yes", SDC_015 == 2 ~ "No", SDC_015 == 6 ~ "No", TRUE ~ NA_character_),
          SDCGCGT= case_when(SDCGCGT == 1 ~ "White", SDCGCGT == 2 ~ "Non-white", TRUE ~ NA_character_),# Visible Minority: check what valid skip (6) stands for here
          #SDC_035= case_when(SDC_035 == 1 ~ "Heterosexual", SDC_035 == 2 ~ "Sexual minorities", 
          #                   SDC_035 == 3 ~ "Sexual minorities", TRUE ~ NA_character_)
  )


##CCHS_2009-2010----------
cchs2009_2010_extracted<-cchs2009_2010 %>% 
  select(GEN_02A2,GEOGPRV,DHH_SEX,DHHGAGE,DHHGMS,INCGHH,DHHGHSZ,
         SDCFIMM,SDCGRES,GEN_02B, DHH_OWN,
         SDCGCGT,WTS_M)

cchs2009_2010_extracted<-cchs2009_2010_extracted %>% 
  mutate (GEN_02A2 = case_when(GEN_02A2 %in% c(97, 98, 99) ~ NA_integer_,TRUE ~ GEN_02A2),
          GEOGPRV=GEO_PRV_MAPPING_2$PRV_NAME[match(GEOGPRV,GEO_PRV_MAPPING$PRV_NUM)],
          DHH_SEX = case_when(DHH_SEX == 1 ~ "Male", DHH_SEX == 2 ~ "Female", TRUE ~ NA_character_), 
          INCGHH=DHHGAGE_mapping$DHHGAGE_NAME[match(INCGHH,DHHGAGE_mapping$DHHGAGE_NUM)],
          DHHGMS = case_when(DHHGMS == 1 ~ "Married/Common-law", DHHGMS == 2 ~ "Married/Common-law", 
                             DHHGMS == 3 ~ "Widowed/Divorced/Separated", DHHGMS == 4 ~ "Single",TRUE ~ NA_character_),
          DHHGHSZ=case_when(DHHGHSZ ==9 ~ NA_integer_,TRUE ~ DHHGHSZ), 
          #SDCDFOLS= case_when(SDCDFOLS == 1 ~ "English", SDCDFOLS == 2 ~ "French", 
          #                    SDCDFOLS == 3 ~ "English and French", SDCDFOLS == 4 ~ "Neither English nor French",TRUE ~ NA_character_) ,
          SDCFIMM= case_when(SDCFIMM == 1 ~ "Landed immigrant/non-permanent resident", SDCFIMM == 2 ~ "Canadian born", TRUE ~ NA_character_),
          SDCGRES=case_when(SDCGRES == 1 ~ "0-9 years", SDCGRES == 2 ~ "10 or more years", 
                            SDCGRES == 6 ~ "Canadian born", TRUE ~ NA_character_),
          GEN_02B=case_when(GEN_02B == 1 ~ "Excellent", GEN_02B == 2 ~ "Very good", 
                            GEN_02B == 3 ~ "Good", GEN_02B == 4 ~ "Fair",
                            GEN_02B == 5 ~ "Poor",TRUE ~ NA_character_),
          DHH_OWN= case_when(DHH_OWN == 1 ~ "Owned by household members", DHH_OWN == 2 ~ "Rented by household members", TRUE ~ NA_character_),
          #SDC_015=case_when(SDC_015 == 1 ~ "Yes", SDC_015 == 2 ~ "No", SDC_015 == 6 ~ "No", TRUE ~ NA_character_),
          SDCGCGT= case_when(SDCGCGT == 1 ~ "White", SDCGCGT == 2 ~ "Non-white", TRUE ~ NA_character_),# Visible Minority: check what valid skip (6) stands for here

  )


save.image("CCHS_harmonization.RData") 
load("CCHS_harmonization.RData")

