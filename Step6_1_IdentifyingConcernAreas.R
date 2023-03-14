library(tidyverse)
library(readr)
dir.create("Data/DPIT Resources")

# Discovering Concern Areas
County <- read_csv("Data/Resilience/Resilience_Categories_County.csv")
x <- County %>% select(GEOID, County_Name,Comm_Resilience,
                       Econ_Resilience,
                       Hous_Resilience,
                       Infra_Resilience,
                       Envi_Resilience)
County_Rank <- x

for (i in 3:ncol(x)) {
  County_Rank[,i] <- ntile(x[,i],nrow(x))
}

County_Rank <- County_Rank %>% 
  pivot_longer(Comm_Resilience:Envi_Resilience,
               names_to = "Area",
               values_to = "Rank")
x <- County_Rank %>% 
  group_by(County_Name) %>% 
  summarise(n=max(Rank)) %>% mutate(key = paste(County_Name, n))
County_Rank$key <- paste(County_Rank$County_Name, County_Rank$Rank)

Concern_Areas <- left_join(x, County_Rank, by="key")
Concern_Areas <-Concern_Areas %>% select(County_Name = County_Name.x, GEOID, Area) %>% 
  relocate(GEOID, County_Name, Area)

Concern_Areas <- 
  Concern_Areas %>% 
  mutate(Area_of_Concern = 
           case_when(Area == "Comm_Resilience"~ "Community and Health",
                     Area == "Econ_Resilience"~ "Economy",
                     Area== "Hous_Resilience" ~ "Housing",
                     Area == "Infra_Resilience" ~ "Infrastructure",
                     Area == "Envi_Resilience" ~ "Environment"))

write_csv(Concern_Areas,
          "Data/DPIT Resources/Concern_Areas.csv")

