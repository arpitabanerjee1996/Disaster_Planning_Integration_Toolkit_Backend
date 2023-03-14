rm(list = ls())
library(readr)
library(tidyverse)

#Tract Level
#Calculating Resilience
Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_TractLevel/All_Indicators_Tract.csv")
Resilience <- rowSums(Indicators[ , c(4:ncol(Indicators))])
x <- Indicators %>% 
  select(GEOID, Tract_Name=Name, County)
x$Resilience <- Resilience
x <- x %>% mutate(Resilience_Quantile = ntile(Resilience, 5)) %>% 
  mutate(Resilience_Category = case_when(Resilience_Quantile==5~"Very High",
                                         Resilience_Quantile==4~"High",
                                         Resilience_Quantile==3~"Moderate",
                                         Resilience_Quantile==2~"Low",
                                         Resilience_Quantile==1~"Very Low"))


Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_TractLevel/Community_Indicators_Tract.csv")
Comm_Resilience <- rowSums(Indicators[ , c(4:ncol(Indicators))])
x$Comm_Resilience <- Comm_Resilience
x <- x %>% mutate(Comm_Resilience_Quantile = ntile(Comm_Resilience, 5)) %>% 
  mutate(Comm_Resilience_Category = case_when(Comm_Resilience_Quantile==5~"Very High",
                                         Comm_Resilience_Quantile==4~"High",
                                         Comm_Resilience_Quantile==3~"Moderate",
                                         Comm_Resilience_Quantile==2~"Low",
                                         Comm_Resilience_Quantile==1~"Very Low"))

Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_TractLevel/Economy_Indicators_Tract.csv")
Econ_Resilience <- rowSums(Indicators[ , c(4:ncol(Indicators))])
x$Econ_Resilience <- Econ_Resilience
x <- x %>% mutate(Econ_Resilience_Quantile = ntile(Econ_Resilience, 5)) %>% 
  mutate(Econ_Resilience_Category = case_when(Econ_Resilience_Quantile==5~"Very High",
                                              Econ_Resilience_Quantile==4~"High",
                                              Econ_Resilience_Quantile==3~"Moderate",
                                              Econ_Resilience_Quantile==2~"Low",
                                              Econ_Resilience_Quantile==1~"Very Low"))

Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_TractLevel/Housing_Indicators_Tract.csv")
Hous_Resilience <- rowSums(Indicators[ , c(4:ncol(Indicators))])
x$Hous_Resilience <- Hous_Resilience
x <- x %>% mutate(Hous_Resilience_Quantile = ntile(Hous_Resilience, 5)) %>% 
  mutate(Hous_Resilience_Category = case_when(Hous_Resilience_Quantile==5~"Very High",
                                              Hous_Resilience_Quantile==4~"High",
                                              Hous_Resilience_Quantile==3~"Moderate",
                                              Hous_Resilience_Quantile==2~"Low",
                                              Hous_Resilience_Quantile==1~"Very Low"))

Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_TractLevel/Infrastructure_Indicators_Tract.csv")
Infra_Resilience <- rowSums(Indicators[ , c(4:ncol(Indicators))])
x$Infra_Resilience <- Infra_Resilience
x <- x %>% mutate(Infra_Resilience_Quantile = ntile(Infra_Resilience, 5)) %>% 
  mutate(Infra_Resilience_Category = case_when(Infra_Resilience_Quantile==5~"Very High",
                                               Infra_Resilience_Quantile==4~"High",
                                               Infra_Resilience_Quantile==3~"Moderate",
                                               Infra_Resilience_Quantile==2~"Low",
                                               Infra_Resilience_Quantile==1~"Very Low"))
Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_TractLevel/Environment_Indicators_Tract.csv")
Envi_Resilience <- rowSums(Indicators[ , c(4:ncol(Indicators))])
x$Envi_Resilience <- Envi_Resilience
x <- x %>% mutate(Envi_Resilience_Quantile = ntile(Envi_Resilience, 5)) %>% 
  mutate(Envi_Resilience_Category = case_when(Envi_Resilience_Quantile==5~"Very High",
                                              Envi_Resilience_Quantile==4~"High",
                                              Envi_Resilience_Quantile==3~"Moderate",
                                              Envi_Resilience_Quantile==2~"Low",
                                              Envi_Resilience_Quantile==1~"Very Low"))

write.csv(x, "Data/Resilience/Resilience_Categories_Tract.csv")
