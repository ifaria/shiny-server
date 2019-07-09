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

ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body
)

server <- function(input, output, session) {
  
  ### Creating a reactive filtering dataset  
  
  Data_react <- reactive({
    if(!is.null(input$checkbox) & is.null(input$checkbox2)){
      x = incident_type(input$checkbox)
    }else if (is.null(input$checkbox) & !is.null(input$checkbox2)){
      x = age_involved(input$checkbox2)
    }else if (!is.null(input$checkbox) & !is.null(input$checkbox2)){
      x = rbind(incident_type(input$checkbox), age_involved(input$checkbox2))
    } else{x = Data} 
    x %>% filter(Date >= min(input$date_slider) & Date <= max(input$date_slider))
  })
  
  ### Create summary box
  
  output$date_range  <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("\n")
      paste0("Date: ", as.character(as_date(round(e$x))), " Incidents: ", round(e$y), "\n")
    }
    paste("Day Range: ",  
          input$date_slider[2]-input$date_slider[1],
          "<br>", 
          "Incidents: ", NROW(Data_react()), 
          "<br>",
          xy_str(input$hoverdates))
    
  })
  
  output$date_plot <- renderPlot({
    Data_react() %>%
      group_by(Date) %>%
      count() %>%
      ggplot(aes(x = Date, y = n)) + 
      geom_point(size = 0.15) + 
      geom_segment(aes(x = Date, xend = Date, y = 0, yend = n), size = .05) +
      theme_bw() +
      theme(plot.background = element_rect(fill = "transparent", color = NA),
            axis.title.x = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank())}
    ,bg = "transparent"
  )
  
  
  #absolutePanel  
  
  click <- reactiveValues(clickedShape = NULL)
  
  output$click_a_state <- renderText({
    if(is.null(input$mainmap_shape_click)){
      "Click a state for summary information"}
    else(return())
  })
  
  
  observe({
    click<-input$mainmap_shape_click
    if(is.null(click))
      return()
    
    output$state_name <- renderText({
      paste0(input$mainmap_shape_click$id)
    })
    
    output$number_of_incidents_state_selected <- renderText({
      paste0("Incidents By Selected Date: ", length(which(Data_react()$States == click$id)))
    })
    
    output$number_of_incidents_state <- renderText({
      paste0("Incidents Summarized: ", length(which(Data$States == click$id &  year(Data$Date) == input$year_pick)))
    })
    
    output$state_population <- renderText({
      paste0("Population: " ,crimes_state[crimes_state$state_name == click$id & crimes_state$year == input$year_pick, "population"])
    })
    
    output$state_incident_rate <- renderText({
      paste0("State Incident Rate (per 100,000): ", round(nrow(Data[Data$States == click$id & year(Data$Date) == input$year_pick ,1])/crimes_state[crimes_state$state_name == click$id & crimes_state$year == input$year_pick, "population"]*100000, 3))
    })
    
    output$national_incident_rate <- renderText({
      paste0("National Incident Rate (per 100,000): ", round(length(which(year(Data$Date) == input$year_pick))/crimes_state[crimes_state$state_name == "USA" & crimes_state$year == input$year_pick, "population"]*100000, 3))
    })
    
    state_reactive <- Data %>%
      filter(States == input$mainmap_shape_click$id,
             year(Date) == input$year_pick) %>% 
      nrow() * 100000/(crimes_state %>%
                         filter(state_name == input$mainmap_shape_click$id,
                                year == input$year_pick) %>%
                         select(population))
    usa_reactive <- Data %>%
      filter(year(Date) == input$year_pick) %>%
      nrow() * 100000/(crimes_state %>%
                         filter(state_name == "USA",
                                year == input$year_pick) %>%
                         select(population))
    
    donut_data <- data.frame(state_usa = c(input$mainmap_shape_click$id, "USA"),
                             rates = c(state_reactive$population, usa_reactive$population)) %>%
      mutate(percentage = state_reactive$population / usa_reactive$population,
             percentage_label = paste0(round(100 * percentage, 1), "%"))
    donut_data$state_usa <- factor(donut_data$state_usa, levels = donut_data$state_usa) 
    output$state_usa_rates <- renderPlot({
      ggplot(donut_data, aes(y = rates, fill = state_usa, order = state_usa)) + 
        geom_bar(
          aes(x = 1),
          width = .3,
          stat = "identity",
          show.legend = TRUE
        ) +
        annotate(
          geom = "text",
          x = 0,
          y = 0,
          label = donut_data$percentage_label[1],
          size = 15,
          color = "white"
        ) +
        coord_polar(theta = "y") +
        theme_void(base_size = 20) +
        theme(plot.background = element_rect(fill = "transparent", color = NA),
              legend.title = element_blank(),
              legend.text = element_text(color = "white"),
              plot.title = element_text(color = "white")) +
        ggtitle("State vs National Rate")
      #labs(fill = "")
    }, bg = "transparent")
  })
  
  
  ### Create a reactive Map
  
  
  output$mainmap <- renderLeaflet({
    leaflet("mainmap") %>%
      addTiles() %>%
      setView(lng = -97, lat = 38, zoom = 4)  %>%
      
      addMarkers(data = Data_react(),
                 lng = ~Longitude,
                 lat = ~Latitude,
                 clusterOptions = markerClusterOptions(),
                 group = "Incidents (clustered)") %>%
      
      addCircles(data = Data_react(),
                 lng = ~Longitude,
                 lat = ~Latitude,
                 radius = .125,
                 color = "bdbdbd",
                 group = "Incidents (individual)") %>%
      
      addPolygons(data = cookmap, color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = .5, fillOpacity = 0.3,
                  fillColor = ~pal(index),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = FALSE),
                  group = "Partisan Voter Index",
                  layerId = ~name,
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
      ) %>%
      
      addLayersControl(
        baseGroups = c("Incidents (individual)", "Incidents (clustered)"),
        overlayGroups = c("Partisan Voter Index"),
        options = layersControlOptions(collapsed = FALSE)
      ) 
    
    
  })
  
  ### Observe shape clicks
  click <- reactiveValues()
  
  observe({
    click<-input$mainmap_shape_click
    if(is.null(click))
      return()
    output$state_name_laws <- renderText({
      print(click$id)
    })
    output$gun_laws <- renderTable({
      state_laws[[click$id]]
    })
    
    observeEvent(input$mainmap_shape_click, {
      updateTabItems(session, inputId = "tabs",  selected = "State Gun Laws")
    })
    
  })
  
  ### Observe marker clicks
  
  click <- reactiveValues(clickedMarker=NULL)
  
  output$click_an_incident <- renderText({
    if(is.null(input$mainmap_marker_click)){
      "Switch map to Incidents (clustered) to select an incident"}
    else(return())
  })
  
  observe({
    click<-input$mainmap_marker_click
    if(is.null(click))
      return()
    incident_key <- distinct(Data_react()[which(click$lat == Data_react()$Latitude & click$lng == Data_react()$Longitude),"Incident"])
    
    output$inc_loc <- renderTable({
      Data_react()[which(click$lat == Data_react()$Latitude & click$lng == Data_react()$Longitude), c("Incident", "Name", "Num_Participiants", "Injury")]
    })
    
    output$notes <- renderText({
      paste0("Notes: ", as.character(Data_react()[which(click$lat == Data_react()$Latitude & click$lng == Data_react()$Longitude), "Notes"]))
    })
    
    output$location <- renderTable({
      Data_react()[which(click$lat == Data_react()$Latitude & click$lng == Data_react()$Longitude), c("Street", "City", "Counties", "Longitude", "Latitude")]
    })
    
    output$participants<-renderTable({
      Data_Participants %>% semi_join(incident_key, by = "Incident") %>%
        mutate(Incident = NULL)
    })
    
    observeEvent(input$mainmap_marker_click, {
      updateTabItems(session, inputId = "tabs",  selected = "Incidents")
      
    })
  })
  
  output$state_by_rate <- renderPlot({
    state_sss %>%
      filter(year == input$year_pick) %>%
      ggplot(aes(x = reorder(States, rate), y = rate, fill = index)) + 
      geom_bar(stat = "identity") + 
      coord_flip() +
      scale_fill_distiller(palette = "RdBu", trans = "reverse") +
      #geom_label(aes(label = round(rate, 3)), color = "white", fontface = "bold") +
      theme_minimal() +
      theme(legend.position = c(0.8, 0.2),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 13))+
      labs(x = NULL,
           y = NULL, 
           title = "Rate of Incident Per 100,000 People",
           fill = "Voter Partisan Index") 
  }, height = 750)
  
  output$gun_law_heatmap <- renderPlot({
    ggplot(look2, aes(Subjectlaw, state)) + 
      geom_tile(aes(fill = Longguns),
                color = "white") + 
      scale_fill_gradient(low = "white",
                          high = "steelblue") +
      coord_flip() + 
      theme_bw(base_size = 20) + 
      labs(x = "", y = "") + 
      scale_fill_brewer(palette = "BrBG") + 
      theme(#legend.position = "none",
        #axis.ticks = theme_blank(), 
        axis.text.x = element_text(size = 20, angle = 300, hjust = 0, colour = "grey50"))
  })
  
  output$hover_states  <- renderText({
    if(is.null(input$heatmap_hover$x)) return("") 
    else {
      state_levels <- levels(look2$state)
      state_names <- state_levels[round(input$heatmap_hover$x)]
      clicked_state <- state_names <- state_levels[round(input$click_compare$x)]
      
      law_levels <- levels(look2$Subjectlaw)
      laws_state <- law_levels[round(input$heatmap_hover$y)]
      
      shiny::HTML("State: ", state_names, "<br><br>",
                  "Law :", laws_state, "  ", clicked_state
      )
    }
  })
  
  map_hover <- reactiveValues()
  
  observe({
    click2<-input$mainmap_shape_click
    if(is.null(click2))
      return()
    
    map_hover <- input$mainmap_shape_mouseover
    if(is.null(map_hover))
      return()
    
    output$compare_laws <- renderTable({
      
      look2 %>%
        filter(state == input$mainmap_shape_click$id | state == input$mainmap_shape_mouseover$id) %>%
        spread(state, Longguns)
      
    })
    
  })
  
}
shinyApp(ui, server)
