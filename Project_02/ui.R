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
    tabPanel("About", fluidPage(
      fluidRow(
        column(width = 6,
               img(src = "NOAA_Logo.png", height = "200px", width = "200px")
        ),
        column(width = 6,
               img(src = "NWS_Logo.png", height = "200px", width = "200px")
        )
      ),
      h3("About This App"),
      p("The purpose of this app is to provide comprehensive aviation weather data analysis and visualization. It is designed to help users explore various aspects of aviation weather through interactive and informative tabs."),
      h4("Data Source"),
      p("The data used in this app is sourced from various aviation weather APIs endpoints. These endpoints include:",
        tags$ul(
          tags$li("metar - Metoerological Aerodrome Report. This report contains current weather conditions and a log of conditions."),
          tags$li("taf - Terminal Area Forecast. This report is a forecast of the weather conditions."),
          tags$li("airport - Airport statistics. This report contains general statistics about an airport; including surface type, runway length etc.")
        )
      ),
      tags$a(href="https://aviationweather.gov", "Aviation Weather Center"),
      h4("Purpose of Each Tab"),

      tags$ul(
        tags$li("Wind: Analyze and visualize wind speed data by state. 
                Select a valid ICAO ID to analyze. A list of ICAO IDs can be found on the \"Valid icaoIDs\" tab. Optionally, you can leave the field blank to view all airports."),
        tags$li("You can also set the look-back hours. If you would like to see the last 24 hours of data, type \"24\" into the hours field."),
        tags$li("When finished, press the `update` button to display a plot showing the mean descending of the wind values by state. A table will accompany the plot, ordered by state. It is a frequency table with the headers being the wind speed groupings."),),
        
        tags$ul(tags$li("Visibility: Examine visibility data across different airports."),),

        tags$ul(tags$li("Temperature: Explore temperature variations and trends."),),
        tags$ul(tags$li("Airport Statistics: View various statistics related to airports."),),
        tags$ul(tags$li("Data Download: Download aviation weather data for offline analysis."),),
        tags$ul(tags$li("Data Exploration: Interactively explore the aviation weather dataset."),),
        tags$ul(tags$li("Valid icaoIDs: Reference different ICAO ID codes and their associated states.")
      ))),
    

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