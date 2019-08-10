

header <- dashboardHeader(title = "Gun Violence Project",
                          dropdownMenu(type = "messages",
                                       messageItem(
                                           from = "Ignacio",
                                           message = "Welcome to the Gun Violence Project",
                                           icon = icon("comment-alt")
                                           
                                       )))

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Date Range", 
                 startExpanded = TRUE,
                 dateRangeInput("date_slider",
                                label = "Select dates",
                                min = min(Data$Date),
                                max = max(Data$Date),
                                start = max(Data$Date-30), 
                                end = max(Data$Date)
                 )),
        selectInput(inputId = "year_pick",
                    label = "Summarized Year",
                    multiple = FALSE,
                    choices = c(2017,
                                2016),
                    selected = 2017),
        menuItem("Incident Filter",
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
                                          "Child" = "Child")))
    )
)

body <- dashboardBody(
    tabBox(width = NULL, 
           #title = "Tabs again",
           id = "tabs2",
           tabPanel("Map",
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
                               # Map ###########################################################
                               box(width = NULL, background = "blue",
                                   tags$style(type = "text/css", "html,
                       body {width:100%;height:100%"),
                                   leafletOutput(outputId = "mainmap", height = 375)
                               ),
                               # Map Value Boxes
                               box(width = NULL, background = "black", height = 80,
                                   tags$head(tags$style("#date_range{color: white;
                                 font-size: 15px;
                                 font-style: bold;
                                 }")),
                                   column(width = 4, 
                                          htmlOutput("date_range")),
                                   column(width = 8, 
                                          plotOutput("date_plot",
                                                     hover = "hoverdates", height = "60px", width = "385px"
                                          ))),
                               # Map Summary Box ##############################################
                               box(width = NULL, background = "black", height = 235,
                                   #title = "Summary By State", 
                                   column(width = 6,
                                          tags$style("#click_a_state {font-size:30px;
                            color:white;
                            display:block; }"),
                                          textOutput("click_a_state"),
                                          tags$style("#state_name {font-size:50px;
                            color:white;
                            display:block; }"),
                                          textOutput("state_name"),
                                          tags$br(),
                                          tags$style("#number_of_incidents_state_selected {font-size:15px;
                            color:white;
                            display:block; }"),
                                          textOutput("number_of_incidents_state_selected"),
                                          tags$style("#number_of_incidents_state {font-size:15px;
                            color:white;
                            display:block; }"),
                                          textOutput("number_of_incidents_state"),
                                          tags$style("#state_population {font-size:15px;
                            color:white;
                     display:block; }"),
                                          textOutput("state_population"),
                                          tags$style("#state_incident_rate {font-size:15px;
                            color:white;
                     display:block; }"),
                                          textOutput("state_incident_rate"),
                                          tags$style("#national_incident_rate {font-size:15px;
                            color:white;
                     display:block; }"),
                                          textOutput("national_incident_rate")),
                                   column(width = 6,
                                          style = "background-color:black",
                                          plotOutput("state_usa_rates",
                                                     width = "420px",
                                                     height = "200px"))
                               )),
                        column(width = 6,
                               
                               # Tabs ########################################################
                               tabBox(height = 740, width = NULL, id = "tabs",
                                      selected = "Data & Definitions",
                                      tabPanel(title = "Data & Definitions", 
                                               div(style = 'overflow-y:scroll;height:700px;'
                                                   #includeMarkdown("GVDtext.Rmd")
                                               )),
                                      tabPanel(title = "Incidents",
                                               tags$style("#click_an_incident {font-size:30px;
                            color:black;
                            display:block; }"),
                                               textOutput("click_an_incident"),
                                               tags$br(),
                                               tableOutput("inc_loc"),
                                               textOutput("notes"),
                                               tags$hr(),
                                               tableOutput("location"),
                                               tableOutput("participants"),
                                               tableOutput("inc_details")
                                      ),
                                      tabPanel(title = "State Gun Laws",
                                               div(style = 'overflow-y:scroll;height:700px;',
                                                   tags$style("#state_name_laws {font-size:30px;
                            color:black;
                                      display:block; }"),
                                                   textOutput("state_name_laws"),
                                                   tags$br(),
                                                   tableOutput("gun_laws"))),
                                      tabPanel("States by Incident Rate",
                                               column(width = 12, 
                                                      plotOutput("state_by_rate"))),
                                      tabPanel("Law Comparison",
                                               column(width = 12, 
                                                      tableOutput("compare_laws")))
                                      
                               )
                        )
                    )
           ), tabPanel("Gun Law Heatmap", 
                       fluidRow(
                           tabPanel("Gun Law Heatmap", 
                                    column(width = 12,
                                           box(width = NULL, background = "black", height = 200,
                                               textOutput("hover_states")),
                                           column(width = 12, 
                                                  
                                                  div(style = 'height:700px;overflow-y:scroll;overflow-x:scroll;',
                                                      plotOutput("gun_law_heatmap",
                                                                 height = "1000px",
                                                                 width = "1500px",
                                                                 hover = hoverOpts(id = "heatmap_hover", delay = 0),
                                                                 click = "click_compare"))
                                           )
                                    )))
           )
    ))


ui <- dashboardPage(
    header = header,
    sidebar = sidebar,
    body = body
)
