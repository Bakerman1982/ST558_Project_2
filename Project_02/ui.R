#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel("Aviation Dashboard"),
  
  # Navigation bar with seven tabs
  navbarPage(
    title = "Dashboard",
    
    # First tab: Wind
    tabPanel("Wind",
             sidebarLayout(
               sidebarPanel(
                 
                 # Dropdown menu for choosing endpoint
                 selectInput("endpoint", "Choose Endpoint:",
                             choices = c("metar", "taf", "airport"),
                             selected = "metar"),
                 
                 # Text input for icaoID
                 conditionalPanel(
                   condition = "input.endpoint != 'airport'",
                   textInput("icaoID", "ICAO ID:", placeholder = "Enter ICAO ID")
                 ),
                 
                 # Text input for hours or time based on endpoint
                 conditionalPanel(
                   condition = "input.endpoint == 'metar'",
                   textInput("hours", "Hours:", placeholder = "Enter hours")
                 ),
                 conditionalPanel(
                   condition = "input.endpoint == 'taf'",
                   textInput("time", "Time:", placeholder = "Enter time")
                 )
                 
               ),
               mainPanel(
                 plotOutput("windPlot")
               )
             )
    ),
    
    # Second tab: Visibility
    tabPanel("Visibility",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("visibilityBins",
                             "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30)
               ),
               mainPanel(
                 plotOutput("visibilityPlot")
               )
             )
    ),
    
    # Third tab: Temperature
    tabPanel("Temperature",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("temperatureBins",
                             "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30)
               ),
               mainPanel(
                 plotOutput("temperaturePlot")
               )
             )
    ),
    
    # Fourth tab: Airport Statistics
    tabPanel("Airport Statistics",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("airportStatsBins",
                             "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30)
               ),
               mainPanel(
                 plotOutput("airportStatsPlot")
               )
             )
    ),
    
    # Fifth tab: Data Download
    tabPanel("Data Download",
             sidebarLayout(
               sidebarPanel(
                 downloadButton("downloadData", "Download Data")
               ),
               mainPanel(
                 tableOutput("dataTable")
               )
             )
    ),
    
    # Sixth tab: Data Exploration
    tabPanel("Data Exploration",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file1", "Choose CSV File",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                 )
               ),
               mainPanel(
                 tableOutput("contents")
               )
             )
    ),
    
    # Seventh tab: About
    tabPanel("About",
             mainPanel(
               h3("About This Dashboard"),
               p("This dashboard provides insights into aviation data, including wind, visibility, temperature, airport statistics, data download, and exploration features.")
             )
    )
  )
)





