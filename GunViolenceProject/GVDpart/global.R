library(shinydashboard)
library(shiny)
library(leaflet)
library(dplyr)
library(stringr)
library(maptools)
library(tibble)
library(purrr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggiraph)
library(sf)

########## Static Data

#Data <- readRDS("Data.rds")
Data <- readRDS("newdata2.rds")
Data_Incident_Characteristics <- readRDS("Data_Incident_Characteristics.rds")
#Data_Guns <- readRDS("Data_Guns.rds")
Data_Participants <- readRDS("Data_Participants.rds")
#state_pop <- readRDS("state_pop")
#state_pop$NAME <-  str_to_lower(state_pop$NAME)
#colnames(state_pop) <- c("States", "Pop_Estimates_2018")
#wgt_tib <- readRDS("wgt_tib.rds")

### States
Data_states <- readRDS("Data_states.rds")
cookmap <- readRDS("cooks_colors.rds")
pal <- colorBin("RdBu", domain = c(-25, 25), bins = seq(-25, 25, length.out = 11))


labels <- sprintf(
  "<strong>%s</strong><br/>Cook Partisan Voter Index: %g<br/>Party %s</sup>",
  cookmap$name, abs(cookmap$index), cookmap$party
) %>% lapply(htmltools::HTML)

crimes_state <- readRDS("estimated_crimes20162017.rds")


state_sss <- Data %>%
  mutate(year = year(Date)) %>%
  filter(year == "2017") %>%
  group_by(States, year) %>%
  count() %>%
  mutate(n = n*100000) %>%
  inner_join(crimes_state, by = c("States" = "state_name", "year")) %>%
  select(States, n, population, year) %>%
  transmute(rate = round(n/population, 1)) %>%
  full_join(cookmap, by = c("States" = "name")) %>%
  select(-geometry)

###
### State Laws
state_laws <- readRDS("gun_laws_by_state.rds")
look <- enframe(state_laws, name = "state", value = "laws") %>%
  unnest() %>%
  mutate(Subjectlaw = str_to_sentence(Subjectlaw))

look[str_detect(look$Subjectlaw, pattern = "Assault weapon"),"Subjectlaw"] <- "Assault weapon law?"
look[str_detect(look$Subjectlaw, pattern = "Castle"),"Subjectlaw"] <- "Castle Law/Stand Your Ground"
look[str_detect(look$Subjectlaw, pattern = "Concealed carry on college"),"Subjectlaw"] <- "Concealed carry on college campuses"
look[str_detect(look$Subjectlaw, pattern = "Constitutional right to bear arms"),"Subjectlaw"] <- "Constitutional right to bear arms?"
look[str_detect(look$Subjectlaw, pattern = "Firearm registration"),"Subjectlaw"] <- "Firearm registration required?"
look[str_detect(look$Subjectlaw, pattern = "Magazine capacity restriction"),"Subjectlaw"] <- "Magazine capacity restriction?"
look[str_detect(look$Subjectlaw, pattern = "Magazine restriction"),"Subjectlaw"] <- "Magazine capacity restriction?"
look[str_detect(look$Subjectlaw, pattern = "Owner permit required"),"Subjectlaw"] <- "Owner license required?"
look[str_detect(look$Subjectlaw, pattern = "Peaceable journey law"),"Subjectlaw"] <- "Peaceable journey law?"
look[str_detect(look$Subjectlaw, pattern = "Permit required to purchase"),"Subjectlaw"]
#look[str_detect(look$Subjectlaw, pattern = "Shall certify"),"Subjectlaw"] 
look[str_detect(look$Subjectlaw, pattern = "Stand your ground"),"Subjectlaw"] <-"Castle Law/Stand Your Ground"
look[str_detect(look$Subjectlaw, pattern = "State pre"),"Subjectlaw"] <- "State preemption of local restrictions?"
look[str_detect(look$Subjectlaw, pattern = "Title ii"), "Subjectlaw"] <- "Nfa weapons restricted?"
look[str_detect(look$Subjectlaw, pattern = "Vehicle carry"),"Subjectlaw"] <- "Vehicle carry permitted?"
look[str_detect(look$Subjectlaw, pattern = "mental health"),"Subjectlaw"] <- "Lose possession over mental health?"

look <- map_dfc(look, as.factor)

longguns <- list(
  yes <- c("18", "Yes"),
  partially <- c("Partial", "Yes, with exceptions", "Yes*"),
  dontknow <- c("N/A", "Not defined - de facto", NA),
  no <- c("No[53], No")
)

lawtomatrix <- function(col, listchange){
  
  ifelse(col %in% longguns[[1]], "Yes", 
         ifelse(col %in% longguns[[2]], "Partial",
                ifelse(col %in% longguns[[3]], "Unknown", "No")))
}

handguns <- list(
  yes = c("Yes", "21[127]"),
  partially = c("Partial", "Vehicle carry only", "Yes*", "Yes, with exceptions"),
  dontknow = c("N/A", NA)
)

look2 <- look %>%
  mutate(Longguns = lawtomatrix(look$Longguns, longguns)) %>%
  select(state, Subjectlaw, Longguns) %>%
  distinct()




###

### filter functions

incident_type <- function(incident){
  Data %>%
    semi_join(distinct(Data_Incident_Characteristics[str_which(Data_Incident_Characteristics$Type, pattern = incident),"Incident"]),
              by = "Incident")
}

Data_Participants <- Data_Participants %>%
  mutate(Incident = as.character(Incident))

age_involved <- function(age){
  Data %>%
    semi_join(distinct(Data_Participants[str_which(Data_Participants$`Age Group`, pattern = age),"Incident"]),
              by = "Incident")
}


shinyApp(ui, server)

