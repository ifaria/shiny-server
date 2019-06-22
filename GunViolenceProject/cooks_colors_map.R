library(rebus)
library(stringr)
library(RColorBrewer)

state_lines <- USAboundaries::us_states() 

state_lines <- state_lines %>%
  filter(statefp != "72")

cooks_data <- readRDS("cooksdata.rds")

cooks_data <- bind_rows(cooks_data, tibble(states = "District of Columbia", party = "D", index = 5))

cooks_data <- cooks_data %>% mutate(index = ifelse(party == "R", -index, index))

cooks_colors <- state_lines %>%
  select(name, geometry) %>%
  left_join(cooks_data, by = c("name" = "states"))

saveRDS(wat, "cooks_colors.rds")

cooks_map <- readRDS("cooks_colors.rds")

pal <- colorBin("RdBu", domain = c(-25, 25), bins = seq(-25, 25, length.out = 11))

labels <- sprintf(
  "<strong>%s</strong><br/>Cook Partisan Voter Index: %g</sup>",
  cooks_map$name, cooks_map$index
) %>% lapply(htmltools::HTML)

leaflet() %>%
    addPolygons(cooks_map, color = "#444444", weight = 1, smoothFactor = 0.5,
      opacity = .5, fillOpacity = 0.5,
      fillColor = ~pal(index),
      highlightOptions = highlightOptions(color = "white", weight = 2,
      bringToFront = FALSE),
      group = "Incidents Per State",
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) 
  
  addCircleMarkers(data = incident_spdf,
                   radius = 5,
                   label = ~Incident,
                   popup = paste(Data$Sources, "<br>",
                                 Data$Name, "<br>",
                                 Data$Street, "<br>",
                                 Data$City, "<br>",
                                 Data$Notes , "<br>"),
                   group = "Incidents") %>%
  
  setView(lng = -97, lat = 38, zoom = 4)#  %>%

#  addLayersControl(
#    baseGroups = c("Population Density Per State","Incidents Per State"),
#    overlayGroups = c("Incidents")
#  )