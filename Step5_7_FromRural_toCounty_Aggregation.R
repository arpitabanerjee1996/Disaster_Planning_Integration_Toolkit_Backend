rm(list = ls())
library(readr)
library(tidyverse)

# County by Rural 
Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_RuralLevel/All_Indicators_Rural.csv")
Indicators$GEOID <- NULL
Indicators$Name <- NULL
y <- Indicators %>% 
  group_by(County) %>%
  summarise_if(is.numeric, median, na.rm = TRUE)

x <- y %>% select(County)
Resilience <- rowSums(y[ , c(2:ncol(y))])
x$Resilience <- Resilience
x <- x %>% mutate(Resilience_Quantile = ntile(Resilience, 5)) %>% 
  mutate(Resilience_Category = case_when(Resilience_Quantile==5~"Very High",
                                         Resilience_Quantile==4~"High",
                                         Resilience_Quantile==3~"Moderate",
                                         Resilience_Quantile==2~"Low",
                                         Resilience_Quantile==1~"Very Low"))
# Community
Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_RuralLevel/Community_Indicators_Rural.csv")
Indicators$GEOID <- NULL

Indicators$Name <- NULL
Indicators <- Indicators %>% 
  filter(!is.na(County))
y <- Indicators %>% 
  group_by(County) %>%
  summarise_if(is.numeric, median, na.rm = TRUE)
Comm_Resilience <- rowSums(y[ , c(2:ncol(Indicators))])
x$Comm_Resilience <- Comm_Resilience
x <- x %>% mutate(Comm_Resilience_Quantile = ntile(Comm_Resilience, 5)) %>% 
  mutate(Comm_Resilience_Category = case_when(Comm_Resilience_Quantile==5~"Very High",
                                              Comm_Resilience_Quantile==4~"High",
                                              Comm_Resilience_Quantile==3~"Moderate",
                                              Comm_Resilience_Quantile==2~"Low",
                                              Comm_Resilience_Quantile==1~"Very Low"))

Indicators_Ranked <- y
for (i in 2:ncol(y)) {
  Indicators_Ranked[,i] <- ntile(y[,i],10)
}
Start <- colnames(Indicators_Ranked[,2])
End <- colnames(Indicators_Ranked[,ncol(Indicators_Ranked)])
Indicators_Ranked <- Indicators_Ranked %>% pivot_longer(all_of(Start):all_of(End),names_to = "Indicators", values_to = "Ranks")

x$Comm_HighRisk <- ""
x$Comm_LowRisk <- ""

for (i in 1:nrow(x)) {
  County_name <- y$County[i]
  
  dummy <- Indicators_Ranked %>% 
    filter(Ranks == 10) %>% 
    filter(County == County_name) %>% 
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))

  x$Comm_HighRisk[i] <- dummy

  dummy <- Indicators_Ranked %>%
    filter(Ranks == 5) %>%
    filter(County == County_name) %>%
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  x$Comm_LowRisk[i] <- dummy
}



# Economy
Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_RuralLevel/Economy_Indicators_Rural.csv")
Indicators$GEOID <- NULL

Indicators$Name <- NULL
Indicators <- Indicators %>% 
  filter(!is.na(County))
y <- Indicators %>% 
  group_by(County) %>%
  summarise_if(is.numeric, median, na.rm = TRUE)

Econ_Resilience <- rowSums(y[ , c(2:ncol(Indicators))])
x$Econ_Resilience <- Econ_Resilience
x <- x %>% mutate(Econ_Resilience_Quantile = ntile(Econ_Resilience, 5)) %>% 
  mutate(Econ_Resilience_Category = case_when(Econ_Resilience_Quantile==5~"Very High",
                                              Econ_Resilience_Quantile==4~"High",
                                              Econ_Resilience_Quantile==3~"Moderate",
                                              Econ_Resilience_Quantile==2~"Low",
                                              Econ_Resilience_Quantile==1~"Very Low"))

Indicators_Ranked <- y
for (i in 2:ncol(y)) {
  Indicators_Ranked[,i] <- ntile(y[,i],10)
}
Start <- colnames(Indicators_Ranked[,2])
End <- colnames(Indicators_Ranked[,ncol(Indicators_Ranked)])
Indicators_Ranked <- Indicators_Ranked %>% pivot_longer(all_of(Start):all_of(End),names_to = "Indicators", values_to = "Ranks")

x$Econ_HighRisk <- ""
x$Econ_LowRisk <- ""

for (i in 1:nrow(x)) {
  County_name <- y$County[i]
  
  dummy <- Indicators_Ranked %>% 
    filter(Ranks == 10) %>% 
    filter(County == County_name) %>% 
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  
  x$Econ_HighRisk[i] <- dummy
  
  dummy <- Indicators_Ranked %>%
    filter(Ranks == 5) %>%
    filter(County == County_name) %>%
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  x$Econ_LowRisk[i] <- dummy
}

# Housing
Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_RuralLevel/Housing_Indicators_Rural.csv")
Indicators$GEOID <- NULL

Indicators$Name <- NULL
Indicators <- Indicators %>% 
  filter(!is.na(County))
y <- Indicators %>% 
  group_by(County) %>%
  summarise_if(is.numeric, median, na.rm = TRUE)

Hous_Resilience <- rowSums(y[ , c(2:ncol(Indicators))])
x$Hous_Resilience <- Hous_Resilience
x <- x %>% mutate(Hous_Resilience_Quantile = ntile(Hous_Resilience, 5)) %>% 
  mutate(Hous_Resilience_Category = case_when(Hous_Resilience_Quantile==5~"Very High",
                                              Hous_Resilience_Quantile==4~"High",
                                              Hous_Resilience_Quantile==3~"Moderate",
                                              Hous_Resilience_Quantile==2~"Low",
                                              Hous_Resilience_Quantile==1~"Very Low"))
Indicators_Ranked <- y
for (i in 2:ncol(y)) {
  Indicators_Ranked[,i] <- ntile(y[,i],10)
}
Start <- colnames(Indicators_Ranked[,2])
End <- colnames(Indicators_Ranked[,ncol(Indicators_Ranked)])
Indicators_Ranked <- Indicators_Ranked %>% pivot_longer(all_of(Start):all_of(End),names_to = "Indicators", values_to = "Ranks")

x$Hous_HighRisk <- ""
x$Hous_LowRisk <- ""

for (i in 1:nrow(x)) {
  County_name <- y$County[i]
  
  dummy <- Indicators_Ranked %>% 
    filter(Ranks == 10) %>% 
    filter(County == County_name) %>% 
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  
  x$Hous_HighRisk[i] <- dummy
  
  dummy <- Indicators_Ranked %>%
    filter(Ranks == 5) %>%
    filter(County == County_name) %>%
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  x$Hous_LowRisk[i] <- dummy
}

#Infrastructure
Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_RuralLevel/Infrastructure_Indicators_Rural.csv")
Indicators$GEOID <- NULL

Indicators$Name <- NULL
Indicators <- Indicators %>% 
  filter(!is.na(County))
y <- Indicators %>% 
  group_by(County) %>%
  summarise_if(is.numeric, median, na.rm = TRUE)

Infra_Resilience <- rowSums(y[ , c(2:ncol(Indicators))])
x$Infra_Resilience <- Infra_Resilience
x <- x %>% mutate(Infra_Resilience_Quantile = ntile(Infra_Resilience, 5)) %>% 
  mutate(Infra_Resilience_Category = case_when(Infra_Resilience_Quantile==5~"Very High",
                                               Infra_Resilience_Quantile==4~"High",
                                               Infra_Resilience_Quantile==3~"Moderate",
                                               Infra_Resilience_Quantile==2~"Low",
                                               Infra_Resilience_Quantile==1~"Very Low"))
Indicators_Ranked <- y
for (i in 2:ncol(y)) {
  Indicators_Ranked[,i] <- ntile(y[,i],10)
}
Start <- colnames(Indicators_Ranked[,2])
End <- colnames(Indicators_Ranked[,ncol(Indicators_Ranked)])
Indicators_Ranked <- Indicators_Ranked %>% pivot_longer(all_of(Start):all_of(End),names_to = "Indicators", values_to = "Ranks")

x$Infra_HighRisk <- ""
x$Infra_LowRisk <- ""

for (i in 1:nrow(x)) {
  County_name <- y$County[i]
  
  dummy <- Indicators_Ranked %>% 
    filter(Ranks == 10) %>% 
    filter(County == County_name) %>% 
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  
  x$Infra_HighRisk[i] <- dummy
  
  dummy <- Indicators_Ranked %>%
    filter(Ranks == 5) %>%
    filter(County == County_name) %>%
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  x$Infra_LowRisk[i] <- dummy
}

# Environment
Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_RuralLevel/Environment_Indicators_Rural.csv")
Indicators$GEOID <- NULL

Indicators$Name <- NULL
Indicators <- Indicators %>% 
  filter(!is.na(County))
y <- Indicators %>% 
  group_by(County) %>%
  summarise_if(is.numeric, median, na.rm = TRUE)

Envi_Resilience <- rowSums(y[ , c(2:ncol(Indicators))])
x$Envi_Resilience <- Envi_Resilience
x <- x %>% mutate(Envi_Resilience_Quantile = ntile(Envi_Resilience, 5)) %>% 
  mutate(Envi_Resilience_Category = case_when(Envi_Resilience_Quantile==5~"Very High",
                                              Envi_Resilience_Quantile==4~"High",
                                              Envi_Resilience_Quantile==3~"Moderate",
                                              Envi_Resilience_Quantile==2~"Low",
                                              Envi_Resilience_Quantile==1~"Very Low"))

Indicators_Ranked <- y
for (i in 2:ncol(y)) {
  Indicators_Ranked[,i] <- ntile(y[,i],10)
}
Start <- colnames(Indicators_Ranked[,2])
End <- colnames(Indicators_Ranked[,ncol(Indicators_Ranked)])
Indicators_Ranked <- Indicators_Ranked %>% pivot_longer(all_of(Start):all_of(End),names_to = "Indicators", values_to = "Ranks")

x$Envi_HighRisk <- ""
x$Envi_LowRisk <- ""

for (i in 1:nrow(x)) {
  County_name <- y$County[i]
  
  dummy <- Indicators_Ranked %>% 
    filter(Ranks == 10) %>% 
    filter(County == County_name) %>% 
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  
  x$Envi_HighRisk[i] <- dummy
  
  dummy <- Indicators_Ranked %>%
    filter(Ranks == 5) %>%
    filter(County == County_name) %>%
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  x$Envi_LowRisk[i] <- dummy
}



write_csv(x,
          "Data/Resilience/Resilience_Categories_Rural_by_County.csv")

