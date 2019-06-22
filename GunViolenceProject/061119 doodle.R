

state_laws[["California"]] %>%
  View()

lubridate::as_date(17498)

crimes_state <- readRDS("estimated_crimes20162017.rds")

paste0("Incidents: ", nrow(Data[Data$States == "Georgia",1]))
  
paste0("Population: " ,crimes_state[crimes_state$state_name == "Georgia" & crimes_state$year == 2016,"population"])

(nrow(Data[Data$States == "Georgia",1])/crimes_state[crimes_state$state_name == "Georgia" & crimes_state$year == 2016,"population"])*100000


crimes_state[crimes_state$state_name == "Georgia" & crimes_state$year == 2017,"population"]
crimes_state[crimes_state$state_name == "Georgia" & crimes_state$year == 2016,"crime_rate"]

length(which(Data$States == "Georgia"))

library(tidyverse)

cal_check <- Data %>%
  filter(States == "California") %>% 
  nrow() * 100000/(crimes_state %>%
  filter(state_name == "California",
         year == 2017) %>%
  select(population))

usa_check <- Data %>%
  nrow() * 100000/(crimes_state %>%
  filter(state_name == "USA",
         year == 2017) %>%
  select(population))

Data %>%
  group_by(States) %>%
  count() %>%
  arrange(desc(n)) 

Data %>%
  select(States) %>%
  distinct() 

Data$Incident 
library(rebus)
pattern = or1(cooks_data$states)
str_match(Data$Incident, pattern = pattern) %>% View()

Data <- Data %>%
  mutate(States = str_match(Data$Incident, pattern = pattern))

saveRDS(Data, "newdata2.rds")
Data <- readRDS("newdata2.rds")

state_reactive <- Data %>%
  filter(States == "Utah") %>% 
  nrow() * 100000/(crimes_state %>%
                     filter(state_name == "Utah",
                            year == 2017) %>%
                     select(population))

usa_reactive <- Data %>%
  nrow() * 100000/(crimes_state %>%
                     filter(state_name == "USA",
                            year == 2017) %>%
                     select(population))
38.9
donut_data$state_usa <- factor(donut_data$state_usa, levels = donut_data$state_usa) 
22.9/163.9
163.9/22.9
donut_data <- data.frame(state_usa = c("Utah", "USA"),
                         rates = c(state_reactive$population, usa_reactive$population)) %>%
              mutate(percentage = state_reactive$population / usa_reactive$population) %>%
  mutate(percentage_label = paste0(round(100 * percentage, 1), "%"))
                                     
library(tidyverse)
library(ggplot2)
install.packages("ggiraph")
library(ggiraph)

#ggobj <- 
ggplot(donut_data, aes(y = rates, fill = state_usa)) + 
  geom_bar_interactive(
    aes(x = 1),
    width = 0.1,
    stat = "identity",
    show.legend = T
  ) +
  annotate(
    geom = "text",
    x = 0,
    y = 0,
    label = donut_data$percentage_label[1],
    size = 20,
    color = "grey"
  ) +
  scale_fill_manual(values = c("grey", "magenta")) +
  coord_polar(theta = "y") +
  theme_void()

ggiraph(ggobj = ggobj)

Data %>%
  filter(Date < "2019-03-31" & Date >= "2019-03-01") %>%
  group_by(Date) %>%
  count() %>%
  ggplot(aes(x = Date, y = n)) + 
  geom_point() + 
  geom_segment(aes(x = Date, xend = Date, y = 0, yend = n)) +
  theme(axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())


state_sss <- Data %>%
  group_by(States) %>%
  count() %>%
  mutate(n = n*100000) %>%
  inner_join(crimes_state, by = c("States" = "state_name")) %>%
                     filter(year == 2017) %>%
                     select(States, n, population) %>%
  transmute(rate = round(n/population, 1)) %>%
  full_join(cooks_data, by = c("States" = "states")) %>%
  arrange(desc(rate))

usa_reactive <- Data %>%
  nrow() * 100000/(crimes_state %>%
                     filter(state_name == "USA",
                            year == 2017) %>%
                     select(population))
38.9
donut_data$state_usa <- factor(donut_data$state_usa, levels = donut_data$state_usa) 
22.9/163.9
163.9/22.9
donut_data <- data.frame(state_usa = c("Utah", "USA"),
                         rates = c(state_reactive$population, usa_reactive$population)) %>%
  mutate(percentage = state_reactive$population / usa_reactive$population) %>%
  mutate(percentage_label = paste0(round(100 * percentage, 1), "%"))

pal <- colorBin("RdBu", domain = c(-25, 25), bins = seq(-25, 25, length.out = 11))

ggplot(state_sss, aes(x = reorder(States, rate), y = rate, fill = index)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  scale_fill_distiller(palette = "RdBu", trans = "reverse") +
  geom_label(aes(label = round(rate, 3)), color = "white", fontface = "bold") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.2),
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 13))+
  labs(x = NULL,
       y = NULL, 
       title = "Rate of Incident Per 100,000 People") 

paste0("National Incident Rate (per 100,000): ", round(nrow(Data)/crimes_state[crimes_state$state_name == "USA" & crimes_state$year == 2017, "population"]*100000, 3))

paste0("Incidents: ", length(which(Data$States == "California" &  year(Data$Date) == 2016)))

?heatmap
look <- enframe(state_laws, name = "state", value = "laws")
look2 <- unnest(look) %>% map_dfc(as.factor)
str(look2)
table(look2$`Long guns`)
look2$`Long guns`=="18"

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

cbind(lawtomatrix(look2$`Long guns`), as.character(look2$`Long guns`)) %>% View()

table(look2$Handguns)
handguns <- list(
  yes = c("Yes", "21[127]"),
  partially = c("Partial", "Vehicle carry only", "Yes*", "Yes, with exceptions"),
  dontknow = c("N/A", NA)
)

cbind(lawtomatrix(look2$Handguns), as.character(look2$Handguns)) %>% View()
?heatmap
state_laws[["Oklahoma"]] %>% View()

look3 <- look2 %>%
  mutate(Longguns = lawtomatrix(look2$Longguns, longguns), 
         Handguns = lawtomatrix(look2$Handguns, handguns)) %>%
  select(state, Subjectlaw, Longguns) %>%
  distinct()
library(tidyverse)
heatmap(look3)
row.names(look3) <- look3$state
look4 <- spread(look3[,1:3], key = Subjectlaw, value = Longguns, fill = "0")
row.names(look4) <- look4$state
look4$state = NULL

look.matrix <- t(data.matrix(look4))
my_col = brewer.pal(4, "Set1")
heatmap(look.matrix, Rowv = NA, Colv = NA, scale = "column", margins = c(5, 10))

ggplot(look.matrix, aes())

lattice::levelplot(look.matrix)
plotly::plot_ly(y = rownames(look.matrix), x = colnames(look.matrix), z = look.matrix, type = "heatmap")

#look4 <- data.table::melt(look3)

nba.m <- plyr::ddply(wat, (Subjectlaw), transform, rescale = rescale(value))



look <- enframe(state_laws, name = "state", value = "laws") %>%
  unnest() %>% 
  map_dfc(as.factor) %>%
  mutate(Subjectlaw = str_to_sentence(Subjectlaw))

look[look$Subjectlaw == "Assault weapon prohibition or restrictions?", "Subjectlaw"]
"Assault weapons law?"

awb <- c("Assault weapon prohibition or restrictions?", "Assault weapons law?")

look[which(look$Subjectlaw %in% awb),"Subjectlaw"] <- "Assault weapon law?"

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
look[str_detect(look$Subjectlaw, pattern = "Shall certify"),"Subjectlaw"] 
look[str_detect(look$Subjectlaw, pattern = "Stand your ground"),"Subjectlaw"] <-"Castle Law/Stand Your Ground"
look[str_detect(look$Subjectlaw, pattern = "State pre"),"Subjectlaw"] <- "State preemption of local restrictions?"
look[str_detect(look$Subjectlaw, pattern = "Title ii"), "Subjectlaw"] <- "Nfa weapons restricted?"
look[str_detect(look$Subjectlaw, pattern = "Vehicle carry"),"Subjectlaw"] <- "Vehicle carry permitted?"
lookgood <- look
#look[str_detect(look$Notes, pattern = "Ownership of machine") & str_detect(look$Subjectlaw, pattern = "Assault weapon law?") & look$state == "New York", c("Subjectlaw")] <- "Nfa weapons restricted?" 

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
  mutate(Longguns = lawtomatrix(look2$Longguns, longguns), 
         Handguns = lawtomatrix(look2$Handguns, handguns)) %>%
  select(state, Subjectlaw, Longguns) %>%
  distinct()

ggplot(look2, aes(Subjectlaw, state)) + 
  geom_tile(aes(fill = Longguns),
            color = "white") + 
  scale_fill_gradient(low = "white",
                      high = "steelblue") +
  coord_flip() + 
  theme_bw(base_size = 9) + 
  labs(x = "", y = "") + 
  scale_fill_brewer(palette = "BrBG") + 
  theme(#legend.position = "none",
        #axis.ticks = theme_blank(), 
        axis.text.x = element_text(size = base_size * 0.8, angle = 330, hjust = 0, colour = "grey50"))



unique(look$Subjectlaw) %>%
  View()



