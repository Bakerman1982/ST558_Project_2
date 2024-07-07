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
  titlePanel("Aviation Weather Dashboard"),
  
  # Navigation bar with seven tabs
  navbarPage(
    title = "Dashboard",
    
    # First tab: Wind
    tabPanel("Wind",
             sidebarLayout(
               sidebarPanel(
                 # Text input for icaoID
                 textInput("icaoID", "ICAO ID:", placeholder = "Enter valid icaoID"),
                 tags$small("See Valid icaoID tab for acceptable entries"),
                 
                 # Text input for hours
                 textInput("hours", "Hours:", placeholder = "Enter hours"),
                 tags$small("Enter a number corresponding to how far back for data retrieval. Leave blank for 24 hours."),
                 
                 # Button to update plot and table
                 actionButton("updateButton", "Update")
               ),
               mainPanel(
                 plotOutput("windPlot"),
                 uiOutput("windTableTitle"),
                 tableOutput("windTable")
               )
             )
    ),
    
    # Second tab: Visibility
    tabPanel("Temperature",
             sidebarLayout(
               sidebarPanel(
               ),
               mainPanel(
               )
             )
    ),
    
    
    # Third tab: Temperature
    tabPanel("Visib",
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
    ),
    

    tabPanel("Valid icaoIDs",
             sidebarLayout(
               sidebarPanel(
                 selectInput("icao_category", "Select ICAO Category",
                             choices = c("@TOP", "@TOPE", "@TOPC", "@TOPW", "@USN", "@USS", "@USE", "@USW", "#US", "<state>")),
                 uiOutput("state_dropdown")  # For dynamically generating the state dropdown
               ),
               mainPanel(
                 textOutput("category_description"),
                 tableOutput("icao_table")
               )
             )
    ) 
    
    
    
)
)