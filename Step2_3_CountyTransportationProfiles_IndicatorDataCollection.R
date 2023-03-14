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

# Mention State as Illinois
stateName <-  "Illinois"

directory <- getwd()
directory <- str_replace_all(paste(directory, "/Data")," ","")
setwd(directory)
directory <- str_replace_all(paste(directory, "/Raw")," ","")
setwd(directory)

# Copy the console output of path, and add /data/CHS_2022lsx at the end
# The destination should look like the filename below
# We use the PCA Input variables non-transformed for our analysis
destfile <- str_replace_all(paste(directory, "/County_Transportation_Profiles.csv")," ","")
url <- "https://data.bts.gov/api/views/qdmf-cxm3/rows.csv?accessType=DOWNLOAD"
download.file(url, destfile)
setwd(str_replace_all(directory, "Data/Raw", ""))

CTP <- read_csv("Data/Raw/County_Transportation_Profiles.csv")
CTP <- CTP %>% filter(`State Name`==stateName)
#
#
# Indicator Group 4- Infrastructure
CTP_vars <- c("County Name", "Primary and Commercial Airports", "Number of Bridges",
              "% of Poor Condition Bridges",
              "Number of business establishments",
              "Percent of resident workers who commute by transit",
              "Number of resident workers who work at home",
              "Number of workers from other counties who commute to work in the county",
              "Number of resident workers who commute to work in other counties",
              "Number of resident workers who commute within county",
              "Number of resident workers","Total Docks","Total Marinas","Route miles of freight railroad",
              "% of Medium to Fair Condition Bridges",
              "Route miles of passenger railroad and rail transit")
CTP_I <- CTP[which(colnames(CTP)%in%CTP_vars)]
colnames(CTP_I) <- c("Name", "Airports", "Bridges",
                     "Percent_PoorCondition_Bridges",
                     "Business_establishments",
                     "Percent_commuters_by_transit",
                     "Percent_work_at_home",
                     "Other_county_workers",
                     "Workers_to_OtherCounties",
                     "Commuters_withinCounty",
                     "Resident_Workers","Docks","Marinas","Freight_railroad_miles",
                     "Percent_ MediumFairCondition_Bridges",
                     "Passenger_railroad_miles")
write_csv(CTP_I, "Data/Infrastructure_Indicators/Infrastructure_CTPIndicators_county.csv")
rm(CTP_I, CTP_vars)

#
#
# End Selection Here
# Press Enter
# All required indicator data will be generated
# Considering the structure of CTP data remains the same
#
# 
