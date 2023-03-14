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

# County Health Rankings (CHR) provide nuanced analysis of added Community, Economy, Housing, Infrastructure Information
# For County Health Rankings, 
# Data is only available at the County Level
# This data can then be generalized at the tract level
# All data can be obtained from the link https://www.countyhealthrankings.org/explore-health-rankings/rankings-data-documentation
# The link above contains data for the years 2021 and 2022
# It also contains links to specific states
# Data documentation can be found here https://www.countyhealthrankings.org/explore-health-rankings/rankings-data-documentation 

# Mention State as Illinois
stateName <-  "Illinois"

# The latest version of this data as of September, 2022 is from 2022 itself
# This can be updated as per need by changing the link used below
# Directly downloading CHR Data at the National Level, for all Counties in the US
directory <- getwd()
directory <- str_replace_all(paste(directory, "/Data")," ","")
setwd(directory)
dir.create("Raw")
directory <- str_replace_all(paste(directory, "/Raw")," ","")
setwd(directory)

# Copy the console output of path, and add /data/CHS_2022.xlsx at the end
# The destination should look like the filename below
destfile <- str_replace_all(paste(directory, "/CHS_2022.xlsx")," ","")
url <- "https://www.countyhealthrankings.org/sites/default/files/media/document/2022%20County%20Health%20Rankings%20Data%20-%20v1.xlsx"
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
# Loading CHS Data in R
# and Data Cleaning to make it usable
CHS_2022 <- read_excel("Data/raw/CHS_2022.xlsx", 
                       sheet = "Ranked Measure Data")
columns <- unlist(CHS_2022[1,])
colnames(CHS_2022) <- columns
CHS_2022 <- CHS_2022[2:nrow(CHS_2022),]
CHS_2022_Additional <- read_excel("Data/raw/CHS_2022.xlsx", 
                       sheet = "Additional Measure Data")
columns <- unlist(CHS_2022_Additional[1,])
colnames(CHS_2022_Additional) <- columns
CHS_2022_Additional <- CHS_2022_Additional[2:nrow(CHS_2022_Additional),]
CHS_2022_Additional$State <- NULL
CHS_2022_Additional$County <- NULL
CHS_2022_Additional$FIPS <- NULL
CHS <- bind_cols(CHS_2022, CHS_2022_Additional)
CHS <- CHS %>% filter(State==stateName)
rm(CHS_2022, CHS_2022_Additional)
#
#
# Indicator Group 1- Community
CHS_Community_vars <- c("County","% rural",
                             "Social Association Rate",
                             "Violent Crime Rate" , "% Disconnected Youth" ,
                             "Segregation Index",  "Inadequate Facilities",
                             "COVID-19 death rate", "Primary Care Physicians Rate",
                             "Mental Health Provider Rate",
                             "Food Environment Index",
                             "% Food Insecure",
                             "% Limited Access to Healthy Foods",
                             "Average Number of Physically Unhealthy Days" ,
                             "% With Access to Exercise Opportunities",
                             "% Vaccinated")
CHS_C <- CHS[which(colnames(CHS)%in% CHS_Community_vars)]
colnames(CHS_C) <- c("County",
                  "Physically_Unhealthy_Days",
                  "Food_Environment_Index", "Percent_Exercise_Access",
                  "PrimaryCare_Physicians_Rate",              
                  "MentalHealth_Provider_Rate","Percent_Vaccinated",
                  "Social_Association_Rate","Violent_Crime_Rate","Inadequate_Facilities",
                  "COVID-19_death_rate","Percent_Food_Insecure","Percent_Limited_Access_to_Healthy_Foods",
                  "Percent_Disconnected_Youth","Segregation_index","Percent_rural")
# Saving the values in Community_Indicators folder
write_csv(CHS_C, "Data/Community_Indicators/Community_CHSIndicators_county.csv")
rm(CHS_C, CHS_Community_vars)
#
#
# Indicator Group 2- Economy
CHS_Economy_vars <- c("County","% Children in Poverty",
                      "Gender Pay Gap" , "% household income required for childcare expenses")
CHS_E <- CHS[which(colnames(CHS)%in% CHS_Economy_vars)]
colnames(CHS_E) <- c("County","Percent_Children_in_Poverty",                             
                     "Gender_Pay_Gap",
                     "Percent_income_required_for_childcare_expenses")
# Saving the values in Economy_Indicators folder
write_csv(CHS_E, "Data/Economy_Indicators/Economy_CHSIndicators_county.csv")
rm(CHS_E, CHS_Economy_vars)
#
#
# Indicator Group 3- Housing
CHS_Housing_vars <- c("County","% Severe Housing Problems","Inadequate Facilities 95% CI - Low", 
                      "Segregation Index")
CHS_H <- CHS[which(colnames(CHS)%in% CHS_Housing_vars)]
colnames(CHS_H) <- c("County","Percent_Children_in_Poverty",                             
                     "Gender_Pay_Gap",
                     "Percent_income_required_for_childcare_expenses")
# Saving the values in Economy_Indicators folder
write_csv(CHS_H, "Data/Housing_Indicators/Housing_CHSIndicators_county.csv")
rm(CHS_H, CHS_Housing_vars)
#
#
# Indicator Group 4- Infrastructure
CHS_Infra_vars <- c("County","% Broadband Access")
CHS_I <- CHS[which(colnames(CHS)%in% CHS_Infra_vars)]
colnames(CHS_I) <- c("County","Percent_BroadbandAccess")
# Saving the values in Economy_Indicators folder
write_csv(CHS_I, "Data/Infrastructure_Indicators/Infrastructure_CHSIndicators_county.csv")
rm(CHS_I, CHS_Infra_vars)
#
#
# End Selection Here
# Press Enter
# All required indicator data will be generated
# Considering the structure of CHS data remains the same
#
# 

