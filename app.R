library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggiraph)
library(JAOPuTo)

# Define UI for application that draws a histogram
ui <- fluidPage(
  setBackgroundColor(
    color = "#f5f2e9",
    shinydashboard = FALSE
  ),
  titlePanel(tags$p(
    tags$h1("Visualizing the Core day-ahead flow-based market coupling domain"),
                    tags$em("or: slicing 14-dimensional potatoes, see also this "),
                    tags$em(tags$a("LinkedIn article", 
                                   href = "https://www.linkedin.com/pulse/slicing-12-dimensional-potatoes-visualization-core-nico-schoutteet")))
  ),
  fluidRow(
    column(width = 3,
           h3("Input parameters")),
    column(width = 6, 
           h3("Domain visualization")),
    column(width = 3,
           h3("Active constraints"))
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      dateInput("Date", "Business day (for delivery):", 
                min = as.Date("2022-06-09"),
                max = Sys.Date() + lubridate::days(1),
                value = Sys.Date()),
      sliderInput("Hour", "Hour:", value = lubridate::hour(Sys.time()),
                  min = 0, max = 23),
      selectInput("BiddingZone1", "Bidding zone 1 (shown horizontally):",
                  choices = list("Austria", "Belgium", "Croatia", "Czech Republic", "France", "Germany/Luxembourg",
                                 "Hungary", "Netherlands", "Poland", "Romania", "Slovakia", "Slovenia",
                                 "ALEGrO Belgium", "ALEGrO Germany"),
                  selected = "Belgium"),
      selectInput("BiddingZone2", "Bidding Zone 2 (shown vertically):",
                  choices = list("Austria", "Belgium", "Croatia", "Czech Republic", "France", "Germany/Luxembourg",
                                 "Hungary", "Netherlands", "Poland", "Romania", "Slovakia", "Slovenia",
                                 "ALEGrO Belgium", "ALEGro Germany"),
                  selected = "Netherlands"),
      awesomeRadio("Reference", "Reference for cross-section of the flow-based domain:", 
                   choices = c("Zero-balanced", "MCP")),
      tags$p("The reference reflects which assumptions have been taken for the non-visualized bidding zones.",
             tags$br(),
             "Zero-balanced assumes every other zone to be at net position = 0,",
             "while MCP assumes net position = Core net position after market coupling."),
      awesomeRadio("Category", "Highlight CNECs according to:",
                   choices = c("Presolved / Active", "Cross-border / Internal", "RAM  above / below 20% minRAM"))
    ),
    mainPanel(
      width = 9,
      fluidRow(
        column(width = 8,
               girafeOutput("Plot",
                            width = "90%", 
                            height = "90%")),
        column(width = 4,
               tableOutput("ActiveConstraints"))
      )
    )
  ),
  tags$p(tags$h3("Disclaimer"),
         "This application is created as a personal project, based on a publicly available data and a public description of the functioning of the Core DA FBMC Project. It is in no way an official tool, associated to the Core DA FBMC Project, or my employer.",
         tags$br(),
         "It is created with the intention to be an educative tool, to increase the understanding on the functioning of the Core DA FBMC mechanism. It should not be used for decision-making and no rights can be deduced from its interpretation.",
         tags$br(),
         "The source code is publicly available on my GitHub page and questions or comments can be shared with my through through my ",
         tags$a("GitHub", href = "https://github.com/nicoschoutteet/"),
         " or my ",
         tags$a("LinkedIn", href = "https://www.linkedin.com/in/nicoschoutteet/"), " account.",
         tags$br(),
         tags$br(),
         "Last update of the application: 18 July 2023.")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  source("global.R")
  
  # summary input
  output$Plot <- renderGirafe({
    
    domainvisualization(as.POSIXct(paste0(input$Date, " ", input$Hour, ":00"), tz = "CET"),
                        input$BiddingZone1,
                        input$BiddingZone2,
                        input$Reference,
                        input$Category)
    
  })
  # shadow prices
  output$ActiveConstraints <- renderTable({
    
    JAOPuTo::JAOPuTo_Core_shadowprices(as.POSIXct(paste0(input$Date, " ", input$Hour, ":00"), tz = "CET"),
                                  as.POSIXct(paste0(input$Date, " ", input$Hour, ":00"), tz = "CET")) %>% 
      arrange(-ShadowPrice) %>% 
      mutate(RAM = scales::percent(RAM / Fmax, accuracy = .1),
             "Shadow Price" = paste0(round(ShadowPrice, 1), " €/MWh")) %>% 
      select(TSO, CNEC = CNE_Name, RAM, "Shadow Price") 
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
