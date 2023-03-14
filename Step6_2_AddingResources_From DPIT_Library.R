# Download Case Study File and save it as Data/Raw/CaseStudies.csv
# Download General Resources File and save it as Data/Raw/GeneralResources.csv
rm(list = ls())
library(tidyverse)
library(readr)
library(tidytext)

DPIT <- read_csv("Data/Raw/GeneralResources.csv")
DPIT <- DPIT %>% select(Title, WebLink = `Web Link`,
                        #BoxLink = `Box Link`,
                        Year = `Year Published/Adopted`,
                        Author, Organization,
                        Document_type=`Document Type`,
                        Abstract,
                        Area=`This resource applies to the following planning areas:`,
                        Disasters=`This resource applies to the following disaster types:`,
                        Plan_Components=`This resource Includes the following components:`,
                        Keyword=`Keywords (Separated by commas)`)
DPIT <- DPIT %>% filter(!is.na(WebLink))
DPIT$ID <- seq.int(nrow(DPIT))


DPIT_token <- DPIT %>%    
  unnest_tokens(term, Area,token = "words")
unique(DPIT_token$term)
DPIT <- DPIT_token %>% filter(term%in%c("housing","infrastructure","economic","disaster","community","natural"))

# Housing Resources
Housing <- DPIT %>% filter(term=="housing")
Housing_token <- Housing %>% 
  unnest_tokens(term, Keyword, token = "words")
unique(Housing_token$term)

Housing_Keywords <-  c("roofs","building","codes","code","city","floodplain","individual","residents","households","ordinances" ,
                       "land","affordability","tools", "families","vulnerable","rural","housing","residential","homeowners","fair","individuals",
                       "septic","municipalities","park" ,"districts","townships","model","operations")
x <- Housing_token %>% filter(term %in% Housing_Keywords)
y <- x %>% group_by(term) %>% summarise(count=n())
y <- y[order(-y$count),]
Housing_MainKeywords <- y$term[which(y$count>1)]
Housing_SecondaryKeywords <- y$term[which(y$count<=1)]
Housing_MainResources <- x %>% filter(term %in% Housing_MainKeywords)
Housing_MainResources <- Housing_MainResources %>% filter(!duplicated(Title))
unique(Housing_MainResources$term)
Housing_MainResources$term[which(Housing_MainResources$term == "codes")] <- "Buildings and Codes"
Housing_MainResources$term[which(Housing_MainResources$term == "code")] <- "Buildings and Codes"
Housing_MainResources$term[which(Housing_MainResources$term == "building")] <- "Buildings and Codes"
Housing_MainResources$term[which(Housing_MainResources$term == "floodplain")] <- "Floods and Disaster Mitigation"
Housing_MainResources$term[which(Housing_MainResources$term == "tools")] <- "Floods and Disaster Mitigation"
Housing_MainResources$term[which(Housing_MainResources$term == "residents")] <- "Floods and Disaster Mitigation"
Housing_MainResources$term[which(Housing_MainResources$term == "land")] <- "Floods and Disaster Mitigation"
BC <- Housing_MainResources %>% filter(term == "Buildings and Codes")
FDM <- Housing_MainResources %>% filter(term == "Floods and Disaster Mitigation")
rm(Housing, Housing_Keywords, Housing_MainKeywords, Housing_SecondaryKeywords, Housing_token, Housing_MainResources, x, y)

x <- BC[1,]
x$ID <- NULL
x$term <-  NULL
x$Year <- as.character(x$Year)
x <- x %>% pivot_longer(Year:Plan_Components, names_to = "Information", values_to = "Description")
x <- x %>% filter(!is.na(Description))
x$Info_Des <- paste(x$Information, ":", x$Description, "\n")
x$Description <- NULL
x <- x %>% pivot_wider(names_from=Information, values_from = Info_Des)
#x$BoxLink <- NULL
# Make sure you print it with cat
cat(x$Info_Des[1])
x$Para1 <- x$Title
x$Hyperlink_forPara1 <- x$WebLink
x$Para2 <- paste( x$Organization,
                 x$Year,
                 x$Document_type,
                 x$Disasters,
                 x$Abstract,
                 x$Plan_Components)
cat(x$Para2)
x <- x %>% select(Para1, Hyperlink_forPara1, Para2)
Housing_Resources <- x

for (i in 2:nrow(BC)) {
  x <- BC[i,]
  x$ID <- NULL
  x$term <-  NULL
  x$Year <- as.character(x$Year)
  x <- x %>% pivot_longer(Year:Plan_Components, names_to = "Information", values_to = "Description")
  x <- x %>% filter(!is.na(Description))
  x$Info_Des <- paste(x$Information, ":", x$Description, "\n")
  x$Description <- NULL
  x <- x %>% pivot_wider(names_from=Information, values_from = Info_Des)
  #x$BoxLink <- NULL
  # Make sure you print it with cat
  cat(x$Info_Des[1])
  x$Para1 <- x$Title
  x$Hyperlink_forPara1 <- x$WebLink
  x$Para2 <- paste( x$Organization,
                    x$Year,
                    x$Document_type,
                    x$Disasters,
                    x$Abstract,
                    x$Plan_Components)
  cat(x$Para2)
  x <- x %>% select(Para1, Hyperlink_forPara1, Para2)
  Housing_Resources <- bind_rows(Housing_Resources,x)
}

Housing_Resources$Categories <- "Buildings and Codes"

for (i in 1:nrow(FDM)) {
  x <- BC[i,]
  x$ID <- NULL
  x$term <-  NULL
  x$Year <- as.character(x$Year)
  x <- x %>% pivot_longer(Year:Plan_Components, names_to = "Information", values_to = "Description")
  x <- x %>% filter(!is.na(Description))
  x$Info_Des <- paste(x$Information, ":", x$Description, "\n")
  x$Description <- NULL
  x <- x %>% pivot_wider(names_from=Information, values_from = Info_Des)
  #x$BoxLink <- NULL
  # Make sure you print it with cat
  cat(x$Info_Des[1])
  x$Para1 <- x$Title
  x$Hyperlink_forPara1 <- x$WebLink
  x$Para2 <- paste( x$Organization,
                    x$Year,
                    x$Document_type,
                    x$Disasters,
                    x$Abstract,
                    x$Plan_Components)
  cat(x$Para2)
  x <- x %>% select(Para1, Hyperlink_forPara1, Para2) %>% mutate(Categories="Floods and Disaster Mitigation")
  Housing_Resources <- bind_rows(Housing_Resources,x)
}

Housing_Resources$Para1 <- str_to_title(Housing_Resources$Para1)
write_csv(Housing_Resources, "Data/DPIT Resources/Housing_recommendations.csv")
rm(BC, FDM, Housing_Resources, x)


# Economy Resources

Economy <- DPIT_token %>% filter(term=="economic")
Economy_token <- Economy %>% 
  unnest_tokens(term, Keyword, token = "words")
unique(Economy_token$term)
x <- Economy_token %>% group_by(term) %>% 
  summarise(count=n())
x <- x[order(-x$count),]

Economy_Keywords <-  c("resiliency","recovery","restoration","development","resilience","agriculture", "cost",
                       "financial", "economic", "economy", "farm", "grants", "livestock", "agroforestry", "affordability", 
                       "animal", "aquaculture", "business", "businesses", "cattle", "costs", "crops", "dairy", "equity",
                       "farmers", "financing", "fisheries", "forestry", "grazing", "incentives", "opportunities", "opportunity")
x <- Economy_token %>% filter(term %in% Economy_Keywords)
y <- x %>% group_by(term) %>% summarise(count=n())
y <- y[order(-y$count),]
Economy_MainKeywords <- y$term[which(y$count>1)]
Economy_SecondaryKeywords <- y$term[which(y$count<=1)]
Economy_MainResources <- x %>% filter(term %in% Economy_MainKeywords)
Economy_MainResources <- Economy_MainResources %>% filter(!duplicated(Title))
Economy_MainResources <- Economy_MainResources %>% filter(!is.na(Title))
unique(Economy_MainResources$term)

Economy_MainResources$term[which(Economy_MainResources$term == "agriculture")] <- "Agriculture and Rural Economy"
Economy_MainResources$term[which(Economy_MainResources$term == "resilience")] <- "Economic Recovery, Restoration and Resilience"
Economy_MainResources$term[which(Economy_MainResources$term == "resiliency")] <- "Economic Recovery, Restoration and Resilience"
Economy_MainResources$term[which(Economy_MainResources$term == "restoration")] <- "Economic Recovery, Restoration and Resilience"
Economy_MainResources$term[which(Economy_MainResources$term == "recovery")] <- "Economic Recovery, Restoration and Resilience"
Economy_MainResources$term[which(Economy_MainResources$term == "economy")] <- "Economic Recovery, Restoration and Resilience"
Economy_MainResources$term[which(Economy_MainResources$term == "economic")] <- "Economic Recovery, Restoration and Resilience"
Economy_MainResources$term[which(Economy_MainResources$term == "livestock")] <- "Agriculture and Rural Economy"
Economy_MainResources$term[which(Economy_MainResources$term == "cost")] <- "Development and Finances"
Economy_MainResources$term[which(Economy_MainResources$term == "development")] <- "Development and Finances"
Economy_MainResources$term[which(Economy_MainResources$term == "financial")] <- "Development and Finances"

Ag <- Economy_MainResources %>% filter(term == "Agriculture and Rural Economy")
Ec <- Economy_MainResources %>% filter(term == "Economic Recovery, Restoration and Resilience")
Fin <- Economy_MainResources %>% filter(term == "Development and Finances")
rm(Economy, Economy_Keywords, Economy_MainKeywords, Economy_SecondaryKeywords, Economy_token, Economy_MainResources, x, y)

x <- Ag[1,]
x$ID <- NULL
x$term <-  NULL
x$Year <- as.character(x$Year)
x <- x %>% pivot_longer(Year:Plan_Components, names_to = "Information", values_to = "Description")
x <- x %>% filter(!is.na(Description))
x$Info_Des <- paste(x$Information, ":", x$Description, "\n")
x$Description <- NULL
x <- x %>% pivot_wider(names_from=Information, values_from = Info_Des)
#x$BoxLink <- NULL
# Make sure you print it with cat
cat(x$Info_Des[1])
x$Para1 <- x$Title
x$Hyperlink_forPara1 <- x$WebLink
x$Para2 <- paste( x$Organization,
                  x$Year,
                  x$Document_type,
                  x$Disasters,
                  x$Abstract,
                  x$Plan_Components)
cat(x$Para2)
x <- x %>% select(Para1, Hyperlink_forPara1, Para2)
Economy_Resources <- x

for (i in 2:nrow(Ag)) {
  x <- Ag[i,]
  x$ID <- NULL
  x$term <-  NULL
  x$Year <- as.character(x$Year)
  x <- x %>% pivot_longer(Year:Plan_Components, names_to = "Information", values_to = "Description")
  x <- x %>% filter(!is.na(Description))
  x$Info_Des <- paste(x$Information, ":", x$Description, "\n")
  x$Description <- NULL
  x <- x %>% pivot_wider(names_from=Information, values_from = Info_Des)
  #x$BoxLink <- NULL
  # Make sure you print it with cat
  cat(x$Info_Des[1])
  x$Para1 <- x$Title
  x$Hyperlink_forPara1 <- x$WebLink
  x$Para2 <- paste( x$Organization,
                    x$Year,
                    x$Document_type,
                    x$Disasters,
                    x$Abstract,
                    x$Plan_Components)
  cat(x$Para2)
  x <- x %>% select(Para1, Hyperlink_forPara1, Para2)
  Economy_Resources <- bind_rows(Economy_Resources,x)
}

Economy_Resources$Categories <- "Agriculture and Rural Economy"

for (i in 1:nrow(Ec)) {
  x <- Ec[i,]
  x$ID <- NULL
  x$term <-  NULL
  x$Year <- as.character(x$Year)
  x <- x %>% pivot_longer(Year:Plan_Components, names_to = "Information", values_to = "Description")
  x <- x %>% filter(!is.na(Description))
  x$Info_Des <- paste(x$Information, ":", x$Description, "\n")
  x$Description <- NULL
  x <- x %>% pivot_wider(names_from=Information, values_from = Info_Des)
  #x$BoxLink <- NULL
  # Make sure you print it with cat
  cat(x$Info_Des[1])
  x$Para1 <- x$Title
  x$Hyperlink_forPara1 <- x$WebLink
  x$Para2 <- paste( x$Organization,
                    x$Year,
                    x$Document_type,
                    x$Disasters,
                    x$Abstract,
                    x$Plan_Components)
  cat(x$Para2)
  x <- x %>% select(Para1, Hyperlink_forPara1, Para2) %>% mutate(Categories="Economic Recovery, Restoration and Resilience")
  Economy_Resources <- bind_rows(Economy_Resources,x)
}

for (i in 1:nrow(Fin)) {
  x <- Fin[i,]
  x$ID <- NULL
  x$term <-  NULL
  x$Year <- as.character(x$Year)
  x <- x %>% pivot_longer(Year:Plan_Components, names_to = "Information", values_to = "Description")
  x <- x %>% filter(!is.na(Description))
  x$Info_Des <- paste(x$Information, ":", x$Description, "\n")
  x$Description <- NULL
  x <- x %>% pivot_wider(names_from=Information, values_from = Info_Des)
  #x$BoxLink <- NULL
  # Make sure you print it with cat
  cat(x$Info_Des[1])
  x$Para1 <- x$Title
  x$Hyperlink_forPara1 <- x$WebLink
  x$Para2 <- paste( x$Organization,
                    x$Year,
                    x$Document_type,
                    x$Disasters,
                    x$Abstract,
                    x$Plan_Components)
  cat(x$Para2)
  x <- x %>% select(Para1, Hyperlink_forPara1, Para2) %>% mutate(Categories="Development and Finances")
  Economy_Resources <- bind_rows(Economy_Resources,x)
}

Economy_Resources$Para1 <- str_to_title(Economy_Resources$Para1)
write_csv(Economy_Resources, "Data/DPIT Resources/Economy_recommendations.csv")
rm(Ag, Ec, Fin, Economy_Resources, x)

# Community Resources
Community <- DPIT_token %>% filter(term=="community")
Community_token <- Community %>% 
  unnest_tokens(term, Keyword, token = "words")
unique(Community_token$term)
x <- Community_token %>% group_by(term) %>% 
  summarise(count=n())
x <- x[order(-x$count),]

Community_Keywords <-  c("emergency","risk", "preparedness", "community", "communities", "health")
x <- Community_token %>% filter(term %in% Community_Keywords)
y <- x %>% group_by(term) %>% summarise(count=n())
y <- y[order(-y$count),]
Community_MainKeywords <- y$term
Community_MainResources <- x %>% filter(term %in% Community_MainKeywords)
Community_MainResources <- Community_MainResources %>% filter(!duplicated(Title))
Community_MainResources <- Community_MainResources %>% filter(!is.na(Title))
unique(Community_MainResources$term)

Community_MainResources$term[which(Community_MainResources$term == "preparedness")] <- "Emergency Preparedness"
Community_MainResources$term[which(Community_MainResources$term == "risk")] <- "Emergency Preparedness"
Community_MainResources$term[which(Community_MainResources$term == "emergency")] <- "Emergency Preparedness"
Community_MainResources$term[which(Community_MainResources$term == "communities")] <- "Community and Health"
Community_MainResources$term[which(Community_MainResources$term == "health")] <- "Community and Health"
Community_MainResources$term[which(Community_MainResources$term == "community")] <- "Community and Health"

EP <- Community_MainResources %>% filter(term == "Emergency Preparedness")
CH <- Community_MainResources %>% filter(term == "Community and Health")
rm(Community, Community_Keywords, Community_MainKeywords, Community_token, Community_MainResources, x, y)

x <- EP[1,]
x$ID <- NULL
x$term <-  NULL
x$Year <- as.character(x$Year)
x <- x %>% pivot_longer(Year:Plan_Components, names_to = "Information", values_to = "Description")
x <- x %>% filter(!is.na(Description))
x$Info_Des <- paste(x$Information, ":", x$Description, "\n")
x$Description <- NULL
x <- x %>% pivot_wider(names_from=Information, values_from = Info_Des)
#x$BoxLink <- NULL
# Make sure you print it with cat
cat(x$Info_Des[1])
x$Para1 <- x$Title
x$Hyperlink_forPara1 <- x$WebLink
x$Para2 <- paste( x$Organization,
                  x$Year,
                  x$Document_type,
                  x$Disasters,
                  x$Abstract,
                  x$Plan_Components)
cat(x$Para2)
x <- x %>% select(Para1, Hyperlink_forPara1, Para2)
Community_Resources <- x

for (i in 2:nrow(EP)) {
  x <- EP[i,]
  x$ID <- NULL
  x$term <-  NULL
  x$Year <- as.character(x$Year)
  x <- x %>% pivot_longer(Year:Plan_Components, names_to = "Information", values_to = "Description")
  x <- x %>% filter(!is.na(Description))
  x$Info_Des <- paste(x$Information, ":", x$Description, "\n")
  x$Description <- NULL
  x <- x %>% pivot_wider(names_from=Information, values_from = Info_Des)
  #x$BoxLink <- NULL
  # Make sure you print it with cat
  cat(x$Info_Des[1])
  x$Para1 <- x$Title
  x$Hyperlink_forPara1 <- x$WebLink
  x$Para2 <- paste( x$Organization,
                    x$Year,
                    x$Document_type,
                    x$Disasters,
                    x$Abstract,
                    x$Plan_Components)
  cat(x$Para2)
  x <- x %>% select(Para1, Hyperlink_forPara1, Para2)
  Community_Resources <- bind_rows(Community_Resources,x)
}

Community_Resources$Categories <- "Emergency Preparedness"

for (i in 1:nrow(CH)) {
  x <- CH[i,]
  x$ID <- NULL
  x$term <-  NULL
  x$Year <- as.character(x$Year)
  x <- x %>% pivot_longer(Year:Plan_Components, names_to = "Information", values_to = "Description")
  x <- x %>% filter(!is.na(Description))
  x$Info_Des <- paste(x$Information, ":", x$Description, "\n")
  x$Description <- NULL
  x <- x %>% pivot_wider(names_from=Information, values_from = Info_Des)
  #x$BoxLink <- NULL
  # Make sure you print it with cat
  cat(x$Info_Des[1])
  x$Para1 <- x$Title
  x$Hyperlink_forPara1 <- x$WebLink
  x$Para2 <- paste( x$Organization,
                    x$Year,
                    x$Document_type,
                    x$Disasters,
                    x$Abstract,
                    x$Plan_Components)
  cat(x$Para2)
  x <- x %>% select(Para1, Hyperlink_forPara1, Para2) %>% mutate(Categories="Community and Health")
  Community_Resources <- bind_rows(Community_Resources,x)
}


Community_Resources$Para1 <- str_to_title(Community_Resources$Para1)
write_csv(Community_Resources, "Data/DPIT Resources/Community_recommendations.csv")
rm(CH, EP, Community_Resources, x)

# Infrastructure Resources

Infrastructure <- DPIT_token %>% filter(term=="infrastructure")
Infrastructure_token <- Infrastructure %>% 
  unnest_tokens(term, Keyword, token = "words")
unique(Infrastructure_token$term)
x <- Infrastructure_token %>% group_by(term) %>% 
  summarise(count=n())
x <- x[order(-x$count),]

Infrastructure_Keywords <-  c("infrastructure")
x <- Infrastructure_token %>% filter(term %in% Infrastructure_Keywords)
x_token <- x %>% unnest_tokens(Term, Title, token = "words")
x <- x_token %>% group_by(Term) %>% 
  summarise(count=n())
x <- x[order(-x$count),]
x <- x_token %>% filter(Term == "infrastructure") 

Infrastructure_MainID <- x$ID
Infrastructure_MainResources <- DPIT_token %>% filter(ID %in% Infrastructure_MainID)
Infrastructure_MainResources <- Infrastructure_MainResources %>% filter(!duplicated(Title))
IM <- Infrastructure_MainResources
rm(Infrastructure, Infrastructure_Keywords, Infrastructure_MainID, Infrastructure_token,
   Infrastructure_MainResources, x, y, x_token)

x <- IM[1,]
x$ID <- NULL
x$term <-  NULL
x$Year <- as.character(x$Year)
x <- x %>% pivot_longer(Year:Plan_Components, names_to = "Information", values_to = "Description")
x <- x %>% filter(!is.na(Description))
x$Info_Des <- paste(x$Information, ":", x$Description, "\n")
x$Description <- NULL
x <- x %>% pivot_wider(names_from=Information, values_from = Info_Des)
#x$BoxLink <- NULL
# Make sure you print it with cat
cat(x$Info_Des[1])
x$Para1 <- x$Title
x$Hyperlink_forPara1 <- x$WebLink
x$Para2 <- paste( x$Organization,
                  x$Year,
                  x$Document_type,
                  x$Disasters,
                  x$Abstract,
                  x$Plan_Components)
cat(x$Para2)
x <- x %>% select(Para1, Hyperlink_forPara1, Para2)
Infrastructure_Resources <- x

for (i in 2:nrow(IM)) {
  x <- IM[i,]
  x$ID <- NULL
  x$term <-  NULL
  x$Year <- as.character(x$Year)
  x <- x %>% pivot_longer(Year:Plan_Components, names_to = "Information", values_to = "Description")
  x <- x %>% filter(!is.na(Description))
  x$Info_Des <- paste(x$Information, ":", x$Description, "\n")
  x$Description <- NULL
  x <- x %>% pivot_wider(names_from=Information, values_from = Info_Des)
  #x$BoxLink <- NULL
  # Make sure you print it with cat
  cat(x$Info_Des[1])
  x$Para1 <- x$Title
  x$Hyperlink_forPara1 <- x$WebLink
  x$Para2 <- paste( x$Organization,
                    x$Year,
                    x$Document_type,
                    x$Disasters,
                    x$Abstract,
                    x$Plan_Components)
  cat(x$Para2)
  x <- x %>% select(Para1, Hyperlink_forPara1, Para2)
  Infrastructure_Resources <- bind_rows(Infrastructure_Resources,x)
}

Infrastructure_Resources$Categories <- "Green Infrastructure"
Infrastructure_Resources$Para1 <- str_to_title(Infrastructure_Resources$Para1)
write_csv(Infrastructure_Resources, "Data/DPIT Resources/Infrastructure_recommendations.csv")
rm(IM, Infrastructure_Resources, x)

# Environment Resources

Environment <- DPIT_token %>% filter(term=="natural")
Environment_token <- Environment %>% 
  unnest_tokens(term, Keyword, token = "words")
unique(Environment_token$term)
x <- Environment_token %>% group_by(term) %>% 
  summarise(count=n())
x <- x[order(-x$count),]

Environment_MainResources <- Environment_token %>% filter(term=="natural")
Environment_MainResources <- Environment_MainResources %>% filter(!duplicated(Title))
IM <- Environment_MainResources
rm(Environment, Environment_token,
   Environment_MainResources, x, y, x_token)

x <- IM[1,]
x$ID <- NULL
x$term <-  NULL
x$Year <- as.character(x$Year)
x <- x %>% pivot_longer(Year:Plan_Components, names_to = "Information", values_to = "Description")
x <- x %>% filter(!is.na(Description))
x$Info_Des <- paste(x$Information, ":", x$Description, "\n")
x$Description <- NULL
x <- x %>% pivot_wider(names_from=Information, values_from = Info_Des)
#x$BoxLink <- NULL
# Make sure you print it with cat
cat(x$Info_Des[1])
x$Para1 <- x$Title
x$Hyperlink_forPara1 <- x$WebLink
x$Para2 <- paste( x$Organization,
                  x$Year,
                  x$Document_type,
                  x$Disasters,
                  x$Abstract,
                  x$Plan_Components)
cat(x$Para2)
x <- x %>% select(Para1, Hyperlink_forPara1, Para2)
Environment_Resources <- x

for (i in 2:nrow(IM)) {
  x <- IM[i,]
  x$ID <- NULL
  x$term <-  NULL
  x$Year <- as.character(x$Year)
  x <- x %>% pivot_longer(Year:Plan_Components, names_to = "Information", values_to = "Description")
  x <- x %>% filter(!is.na(Description))
  x$Info_Des <- paste(x$Information, ":", x$Description, "\n")
  x$Description <- NULL
  x <- x %>% pivot_wider(names_from=Information, values_from = Info_Des)
  #x$BoxLink <- NULL
  # Make sure you print it with cat
  cat(x$Info_Des[1])
  x$Para1 <- x$Title
  x$Hyperlink_forPara1 <- x$WebLink
  x$Para2 <- paste( x$Organization,
                    x$Year,
                    x$Document_type,
                    x$Disasters,
                    x$Abstract,
                    x$Plan_Components)
  cat(x$Para2)
  x <- x %>% select(Para1, Hyperlink_forPara1, Para2)
  Environment_Resources <- bind_rows(Environment_Resources,x)
}

Environment_Resources$Categories <- "Environmental Resources"
Environment_Resources$Para1 <- str_to_title(Environment_Resources$Para1)
Environment_Resources <- Environment_Resources %>% filter(!is.na(Para1))
write_csv(Environment_Resources, "data/DPIT Resources/Environment_recommendations.csv")
rm(IM, Environment_Resources, x)

  
  
