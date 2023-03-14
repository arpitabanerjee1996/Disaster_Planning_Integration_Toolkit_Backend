rm(list=ls())
library(readr)
library(tidyverse)
library(stringr)


# Community Indicators
Comm_ACS <- read_csv("Data/Community_Indicators/Community_ACSIndicators_county.csv")
State <- str_replace_all(str_split(Comm_ACS$Name[1], ",")[[1]][2]," ","")
Comm_CHS <- read_csv("Data/Community_Indicators/Community_CHSIndicators_county.csv")
Comm_CHS <- Comm_CHS[2:nrow(Comm_CHS),]

Comm_CHS$Name <- paste(Comm_CHS$County, paste("County,", State))
Comm_CHS$County <- NULL

Comm <- left_join(Comm_ACS, Comm_CHS, by="Name")
rm(Comm_CHS, Comm_ACS)
GEOID_Name <- Comm %>% 
  select(GEOID, Name)

# Economy Indicators
Econ_EQI <- read_csv("Data/Economy_Indicators/Economy_EQIIndicators_county.csv")
Econ_EQI$Name <- paste(str_replace(paste(Econ_EQI$County, ",")," ,",","), State)
Econ_EQI$County <- NULL

Econ_ACS <- read_csv("Data/Economy_Indicators/Economy_ACSIndicators_county.csv")
Econ <- left_join(Econ_ACS, GEOID_Name, by="GEOID")
Econ_CHS <- read_csv("Data/Economy_Indicators/Economy_CHSIndicators_county.csv")
Econ_CHS <- Econ_CHS[2:nrow(Econ_CHS),]
Econ_CHS$Name <- paste(Econ_CHS$County, paste("County,", State ))
Econ_CHS$County <- NULL
Econ <- left_join(Econ, Econ_CHS, by="Name")
Econ <- left_join(Econ, Econ_EQI, by="Name")
rm(Econ_ACS, Econ_CHS, Econ_EQI)

# Housing Indicators
H_ACS <- read_csv("Data/Housing_Indicators/Housing_ACSIndicators_county.csv")
Hous <- left_join(H_ACS, GEOID_Name, by="GEOID")
H_CHS <- read_csv("Data/Housing_Indicators/Housing_CHSIndicators_county.csv")
H_CHS <- H_CHS[2:nrow(H_CHS),]
H_CHS$Name <- paste(H_CHS$County, paste("County,", State ))
H_CHS$County <- NULL
H_EQI <- read_csv("Data/Housing_Indicators/Housing_EQIIndicators_county.csv")
H_EQI$Name <- paste(str_replace(paste(H_EQI$County, ",")," ,",","), State)
H_EQI$County <- NULL
Hous <- left_join(Hous, H_CHS, by="Name")
Hous <- left_join(Hous, H_EQI, by="Name")
rm(H_ACS, H_CHS, H_EQI)

# Infrastructure Indicators
Infra_ACS <- read_csv("Data/Infrastructure_Indicators/Infrastructure_ACSIndicators_county.csv")
Infra <- left_join(Infra_ACS, GEOID_Name, by="GEOID")
Infra_CHS <- read_csv("Data/Infrastructure_Indicators/Infrastructure_CHSIndicators_county.csv")
Infra_CHS <- Infra_CHS[2:nrow(Infra_CHS),]
Infra_CHS$Name <- paste(Infra_CHS$County, paste("County,",State))
Infra_CHS$County <- NULL
Infra <- left_join(Infra, Infra_CHS, by="Name")
rm(Infra_ACS, Infra_CHS)

# Environmental Indicators
Envi <- read_csv("Data/Environment_Indicators/Environment_EQIIndicators_county.csv")
Envi$Name <- paste(str_replace(paste(Envi$County, ",")," ,",","), State)
Envi$County <- NULL

# Combing and Saving
dir.create("Data/Indicators_CountyLevel")

write_csv(Comm, "Data/Indicators_CountyLevel/Community_Indicators_County.csv")
write_csv(Econ, "Data/Indicators_CountyLevel/Economy_Indicators_County.csv")
write_csv(Hous, "Data/Indicators_CountyLevel/Housing_Indicators_County.csv")
write_csv(Infra, "Data/Indicators_CountyLevel/Infrastructure_Indicators_County.csv")
write_csv(Envi, "Data/Indicators_CountyLevel/Environment_Indicators_County.csv")

Econ$Name <- NULL
All_Combined <- left_join(Comm, Econ, by="GEOID")
Hous$Name <- NULL
All_Combined <- left_join(All_Combined, Hous, by="GEOID")
Infra$Name <- NULL
All_Combined<- left_join(All_Combined, Infra, by="GEOID")
All_Combined <- left_join(All_Combined, Envi, by="Name")

write_csv(All_Combined, "Data/Indicators_CountyLevel/All_Indicators_County.csv")

