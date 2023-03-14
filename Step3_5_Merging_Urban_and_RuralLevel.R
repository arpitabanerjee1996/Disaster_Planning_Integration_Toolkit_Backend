rm(list=ls())
library(readr)
library(tidyverse)
library(stringr)
library(readxl)

directory <- getwd()
destfile <- str_replace_all(paste(directory, "/Data/Raw/RUCA_Codes.xlsx")," ","")
url <- "https://www.ers.usda.gov/webdocs/DataFiles/53241/ruca2010revised.xlsx?v=3567.7"
download.file(url, destfile)
destfile <- str_replace_all(paste(directory, "/Data/Raw/State_Codes.csv")," ","")
url <- "https://gist.githubusercontent.com/dantonnoriega/bf1acd2290e15b91e6710b6fd3be0a53/raw/11d15233327c8080c9646c7e1f23052659db251d/us-state-ansi-fips.csv"
download.file(url, destfile)

Tract_indicators <- read_csv("Data/Indicators_TractLevel/All_Indicators_Tract.csv")
State_codes <- read_csv("Data/Raw/State_Codes.csv")
Ruca <- read_excel("Data/Raw/Ruca_Codes.xlsx")
colnames(Ruca) <- c("FIPS", "State", "County", "Tract_FIPS", "Primary_RUCA",
                    "Secondary_Ruca", "Population", "Land_Area_Miles", "Pop_Density_2010")
Ruca <- Ruca[2:nrow(Ruca),]
State_name <- str_replace_all(str_split(Tract_indicators$Name, ",")[[1]][3]," ","")
State_code <- State_codes$stusps[which(State_codes$stname==State_name)]
Ruca <- Ruca %>% 
  filter(State == State_code)
Ruca$GEOID <- Ruca$Tract_FIPS
Ruca$FIPS <- NULL
Ruca$County <- NULL
Tract_indicators$GEOID <- as.character(Tract_indicators$GEOID)

x <- left_join(Tract_indicators, Ruca, by="GEOID")
Urban <- x %>% 
  filter(Primary_RUCA==1)
dir.create("Data/Indicators_UrbanLevel")
write_csv(Urban, "Data/Indicators_UrbanLevel/All_Indicators_Urban.csv")

Rural <- x %>% 
  filter(Primary_RUCA!=1)
dir.create("Data/Indicators_RuralLevel")
write_csv(Rural, "Data/Indicators_RuralLevel/All_Indicators_Rural.csv")


# Community Indicators
Tract_indicators <- read_csv("Data/Indicators_TractLevel/Community_Indicators_Tract.csv")
Tract_indicators$GEOID <- as.character(Tract_indicators$GEOID)
x <- left_join(Tract_indicators, Ruca, by="GEOID")
Urban <- x %>% 
  filter(Primary_RUCA==1)
write_csv(Urban, "Data/Indicators_UrbanLevel/Community_Indicators_Urban.csv")
Rural <- x %>% 
  filter(Primary_RUCA!=1)
write_csv(Rural, "Data/Indicators_RuralLevel/Community_Indicators_Rural.csv")

# Economy Indicators
Tract_indicators <- read_csv("Data/Indicators_TractLevel/Economy_Indicators_Tract.csv")
Tract_indicators$GEOID <- as.character(Tract_indicators$GEOID)
x <- left_join(Tract_indicators, Ruca, by="GEOID")
Urban <- x %>% 
  filter(Primary_RUCA==1)
write_csv(Urban, "Data/Indicators_UrbanLevel/Economy_Indicators_Urban.csv")
Rural <- x %>% 
  filter(Primary_RUCA!=1)
write_csv(Rural, "Data/Indicators_RuralLevel/Economy_Indicators_Rural.csv")

# Housing Indicators
Tract_indicators <- read_csv("Data/Indicators_TractLevel/Housing_Indicators_Tract.csv")
Tract_indicators$GEOID <- as.character(Tract_indicators$GEOID)
x <- left_join(Tract_indicators, Ruca, by="GEOID")
Urban <- x %>% 
  filter(Primary_RUCA==1)
write_csv(Urban, "Data/Indicators_UrbanLevel/Housing_Indicators_Urban.csv")
Rural <- x %>% 
  filter(Primary_RUCA!=1)
write_csv(Rural, "Data/Indicators_RuralLevel/Housing_Indicators_Rural.csv")

# Infrastructure Indicators
Tract_indicators <- read_csv("Data/Indicators_TractLevel/Infrastructure_Indicators_Tract.csv")
Tract_indicators$GEOID <- as.character(Tract_indicators$GEOID)
x <- left_join(Tract_indicators, Ruca, by="GEOID")
Urban <- x %>% 
  filter(Primary_RUCA==1)
write_csv(Urban, "Data/Indicators_UrbanLevel/Infrastructure_Indicators_Urban.csv")
Rural <- x %>% 
  filter(Primary_RUCA!=1)
write_csv(Rural, "Data/Indicators_RuralLevel/Infrastructure_Indicators_Rural.csv")

# Environment Indicators
Tract_indicators <- read_csv("Data/Indicators_TractLevel/Environment_Indicators_Tract.csv")
Tract_indicators$GEOID <- as.character(Tract_indicators$GEOID)
x <- left_join(Tract_indicators, Ruca, by="GEOID")
Urban <- x %>% 
  filter(Primary_RUCA==1)
write_csv(Urban, "Data/Indicators_UrbanLevel/Environment_Indicators_Urban.csv")
Rural <- x %>% 
  filter(Primary_RUCA!=1)
write_csv(Rural, "Data/Indicators_RuralLevel/Environment_Indicators_Rural.csv")

