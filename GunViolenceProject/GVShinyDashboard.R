library(shinydashboard)
library(shiny)
library(leaflet)
library(dplyr)
library(stringr)
library(maptools)
library(tibble)
library(purrr)
library(tidyr)

## To begin with 
#Data <- readRDS("Data.rds")
Data <- readRDS("newdata1.rds")
Data_Incident_Characteristics <- readRDS("Data_Incident_Characteristics.rds")
#Data_Incident_Characteristics <- saveRDS("Data_Incident_Characteristics.rds")
Data_Guns <- readRDS("Data_Guns.rds")
Data_Participants <- readRDS("Data_Participants.rds")
state_pop <- readRDS("state_pop")
state_pop$NAME <-  str_to_lower(state_pop$NAME)
colnames(state_pop) <- c("States", "Pop_Estimates_2018")
wgt_tib <- readRDS("wgt_tib.rds")
Data_states <- readRDS("Data_states.rds")

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


### 


###


header <- dashboardHeader(title = "Gun Violence Incidents")

sidebar <- dashboardSidebar(
  sidebarMenu(
        dateRangeInput("date_slider",
                   label = "Select dates",
                   min = min(Data$Date),
                   max = max(Data$Date),
                   start = min(Data$Date), 
                   end = max(Data$Date)
                      ),
      
    checkboxGroupInput("checkbox",
                         label = "Incident Types",
                         choices = 
                           c("Officer Involved" = "Officer",
                             "Defensive Use" = "Defensive",
                             "Accident" = "Accidental",
                             "School" = "School",
                             "Robbery" = "robbery",
                             "Domestic" = "Domestic",
                             "Drive-by" = "Drive-by",
                             "Home Invasion" = "Home",
                             "Kidnapping" = "Kidnapping",
                             "Bar related" = "Bar",
                             "Mass Shooting" = "Mass",
                             "Suicide" = "Suicide"
                           )),
      
    checkboxGroupInput("checkbox2",
                         label = "Age Group",
                         choices =
                           c("Adult" = "Adult",
                             "Teen" = "Teen",
                             "Child" = "Child"))
    )
)
  
body <- dashboardBody(
  tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")),
    fluidRow(
      column(width = 6,
        box(width = NULL, background = "blue",
          leafletOutput(outputId = "mainmap", height = 500)
            ),
        box(width = NULL, background = "black", height = 245,
          title = "Map Summary", "Box content here." 
            )
            ),
      column(width = 6,

          tabBox(height = 780, width = NULL, id = "tabs",
              selected = "Data & Definitions",
              tabPanel(title = "Data & Definitions", 
                       div(style = 'overflow-y:scroll;height:700px;',
                       includeMarkdown("GVDtext.Rmd")
                          )),
              tabPanel(title = "Incidents",
                       tags$br(),
                       tableOutput("inc_loc"),
                       textOutput("notes"),
                       tags$hr(),
                       tableOutput("location"),
                       tableOutput("participants"),
                       tableOutput("inc_details")
                       ),
              tabPanel("States", "Tab content 3"),
              tabPanel("Characteristics", "Tab content 3"),
              tabPanel("Participants", "Tab content 3"),
              tabPanel("Guns", "Tab content 3"))
             )
            )
)


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
  
### Create a reactive Map
  
  output$mainmap <- renderLeaflet({
    leaflet("mainmap") %>%
      addTiles() %>%
      setView(lng = -97, lat = 38, zoom = 4)  %>%
      addCircleMarkers(data = Data_react(),
                       lng = ~Longitude,
                       lat = ~Latitude,
                       radius = 1,
                       label = ~Incident)
      
  })
  
### Observe marker clicks
  click <- reactiveValues(clickedMarker=NULL)

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

  

}
shinyApp(ui, server)
