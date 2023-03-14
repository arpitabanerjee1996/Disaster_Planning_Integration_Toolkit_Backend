# Install all required packages if not already present
# Remove the "#" from the lines below beginning with "#install.packages" and press enter to execute each line
# If already installed in the past, please ignore

#install.packages("stringi")
#install.packages("stringr")
#install.packages("tidyverse")
#install.packages("tidycensus")

# Adding libraries to the code and cleaning environment
library(stringr)
library(tidyverse)
library(readr)
library(tidycensus)
library(readxl)
rm(list=ls())

# Environmental Quality Index (EQI) provide nuanced analysis of added Economy, Housing, Infrastructure and
# Environmental Information
# For EQI, data is only available at the County Level
# This data can then be generalized at the tract level
# All data can be obtained from the link https://edg.epa.gov/data/public/ORD/CPHEA/EQI_2006_2010/2006_2010_EQI_2Jan2018_VC.csv
# The link above contains data for the years 2006-2010, updated in 2018 for counties in all states
# Data documentation can be found here https://edg.epa.gov/data/public/ORD/CPHEA/EQI_2006_2010/Data%20Dictionary%20Variables%20EQI%202006_2010lsx 

# Mention State as Illinois
stateID <-  "IL"

# The latest version of this data as of September, 2022 is from 2018
# This can be updated as per need by accessing https://www.epa.gov/healthresearch/environmental-quality-index-eqi#downloads 
# All data corresponding to the 2006-2010 dataset can be accessed at https://edg.epa.gov/EPADataCommons/public/ORD/CPHEA/EQI_2006_2010/
# Directly downloading EQI Data at the National Level, for all Counties in the US

directory <- getwd()
directory <- str_replace_all(paste(directory, "/Data")," ","")
setwd(directory)
directory <- str_replace_all(paste(directory, "/Raw")," ","")
setwd(directory)

# Copy the console output of path, and add /data/CHS_2022lsx at the end
# The destination should look like the filename below
# We use the PCA Input variables non-transformed for our analysis
destfile <- str_replace_all(paste(directory, "/EQI_2018_PCA.csv")," ","")
url <- "https://edg.epa.gov/data/public/ORD/CPHEA/EQI_2006_2010/PCA_Input_Variables_Non_Transformed.csv"
download.file(url, destfile)
setwd(str_replace_all(directory, "Data/Raw", ""))

# Once the State is specified and File is downloaded
# Select the entire code below the double # and press enter
# To generate all CHS related indicators for the Specified State and Year

#
#
# Start Selection Here
#
#
# Reading the data in R
EQI<- read_csv("Data/Raw/EQI_2018_PCA.csv")
EQI <- EQI %>% filter(State==stateID)
#
#
# Indicator Group 2- Economy
EQI_vars <- c("County_Name","al_pwn_gm_env_rate",
              "civic_env_rate", "ed_env_rate", "hc_env_rate", "rec_env_rate",
              "CommuteTime", "sum_NWIBG")
EQI_E <- EQI[which(colnames(EQI)%in%EQI_vars)]
colnames(EQI_E) <- c("County","CommuteTime","Walkability_Score","Vice_related_business_rate",
                     "Civic_related_business_rate","Education_related_business_rate",
                     "Healthcare_related_business_rate","Recreation_related_business_rate")
write_csv(EQI_E, "Data/Economy_Indicators/Economy_EQIIndicators_county.csv")
rm(EQI_E, EQI_vars)
#
#
# Indicator Group 3- Housing
EQI_vars <- c("County_Name","total_units")
EQI_H <- EQI[which(colnames(EQI)%in%EQI_vars)]
colnames(EQI_H) <- c("County","Percent_Section8")
write_csv(EQI_H, "Data/Housing_Indicators/Housing_EQIIndicators_county.csv")
rm(EQI_H, EQI_vars)
#
#
# Indicator Group 5- Environment
EQI_vars <- c("County_Name",	"pm10","pm25","o3","so2","nox","co",
              "PCT_IRRIGATED_ACRES","pct_nematode_acres","pct_manure_acres","pct_disease_acres",
              "pct_defoliate_acres","pct_harvested_acres","pct_au","fungicide","herbicide",
              "insecticide","std_coal_prim_pop"         
              ,"std_metal_prim_pop","std_nonmetal_prim_pop","std_sandandgravel_prim_pop","std_stone_prim_pop",
              "Radon","Facilities_Rate","Pct_BS",
              "Pct_Unemp_total","Pct_Fam_Pov","Per_TotPopSS",
              "Per_PSWithSW","D303_Percent","ALLNPDESperKM",
              "CaAve","Kave","NO3Ave","ClAve","SO4Ave","HgAve",
              "AvgOfD3_ave","W_As","W_Ba","W_Cd","W_Cr","W_CN","W_FL","W_HG",                      
              "W_NO3","W_NO2","W_SE","W_SB","W_ENDRIN","W_METHOXYCHLOR",
              "W_DALAPON","W_DEHA","W_SIMAZINE","W_DEHP","W_PICLORAM","W_DINOSEB",
              "W_ATRAZINE","W_24D","W_BENZOAP","W_PCP","W_PCB","W_DBCP",
              "W_EDB","W_XYLENES","W_CHLORDANE","W_DCM" )
EQI_E <- EQI[which(colnames(EQI) %in% EQI_vars)]
colnames(EQI_E) <- c("County",	"pm10","pm25","o3","so2","nox","co",
                     "PCT_IRRIGATED_ACRES","pct_nematode_acres","pct_manure_acres","pct_disease_acres",
                     "pct_defoliate_acres","pct_harvested_acres","pct_au","fungicide","herbicide",
                     "insecticide","std_coal_prim_pop"         
                     ,"std_metal_prim_pop","std_nonmetal_prim_pop","std_sandandgravel_prim_pop","std_stone_prim_pop",
                     "Radon","Facilities_Rate","Pct_BS",
                     "Pct_Unemp_total","Pct_Fam_Pov","Per_TotPopSS",
                     "Per_PSWithSW","D303_Percent","ALLNPDESperKM",
                     "CaAve","Kave","NO3Ave","ClAve","SO4Ave","HgAve",
                     "AvgOfD3_ave","W_As","W_Ba","W_Cd","W_Cr","W_CN","W_FL","W_HG",                      
                     "W_NO3","W_NO2","W_SE","W_SB","W_ENDRIN","W_METHOXYCHLOR",
                     "W_DALAPON","W_DEHA","W_SIMAZINE","W_DEHP","W_PICLORAM","W_DINOSEB",
                     "W_ATRAZINE","W_24D","W_BENZOAP","W_PCP","W_PCB","W_DBCP",
                     "W_EDB","W_XYLENES","W_CHLORDANE","W_DCM")
write_csv(EQI_E, "Data/Environment_Indicators/Environment_EQIIndicators_county.csv")
rm(EQI_E, EQI_vars)
#
#
# End Selection Here
# Press Enter
# All required indicator data will be generated
# Considering the structure of EQI data remains the same
#
# 
