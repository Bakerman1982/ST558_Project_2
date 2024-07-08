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
library(DT)
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
    
    # Second tab: Temperature
    tabPanel("Temperature",
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
                 plotOutput("tempPlot"),
                 uiOutput("tempTableTitle"),
                 tableOutput("tempTable"),
                 # Add a plot for temperature bar chart
                 plotOutput("tempBarChart"),
                 # Add title and table for temperature summary
                 uiOutput("tempSummaryTitle"),
                 tableOutput("tempSummaryTable")
               )
             )
    ),
    
    # Third tab: Visibility
    tabPanel("Visibility",
             sidebarLayout(
               sidebarPanel(
                 # Text input for icaoID
                 textInput("icaoID_visib", "ICAO ID:", placeholder = "Enter valid icaoID"),
                 tags$small("See Valid icaoID tab for acceptable entries"),
                 
                 # Text input for hours
                 textInput("hours_visib", "Hours:", placeholder = "Enter hours"),
                 tags$small("Enter a number corresponding to how far back for data retrieval. Leave blank for 24 hours."),
                 
                 # Button to update plot and table
                 actionButton("updateButton_visib", "Update")
               ),
               mainPanel(
                 plotOutput("visibHeatmap"),
                 uiOutput("visibSummaryTitle"),
                 tableOutput("visibSummaryTable")
               )
             )
    ),
    
    # Fourth tab: Airport Statistics
      tabPanel("Airport Statistics",
               sidebarLayout(
                 sidebarPanel(
                   # Text input for icaoID
                   textInput("icaoID", "ICAO ID:", placeholder = "Enter valid icaoID"),
                   tags$small("See Valid icaoID tab for acceptable entries"),
                   
                   # Button to update plot and table
                   actionButton("updateButton", "Update")
                 ),
                 mainPanel(
                   # CSS style to position the plot at the top
                   style = "position: relative; top: 0px; height: calc(100vh - 60px);",
                   plotOutput("airportStatsPlot", height = "400px"),  # Adjust height as needed
                   plotOutput("airport_plot", height = "600px")  # Adjust height as needed
                 )
               )
      ),
    

    # Fifth tab: Data Download
    # ui.R
    
    tabPanel("Data Download",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   # Input fields for API querying
                   selectInput("endpoint", "Select Endpoint",
                               choices = c("metar", "taf", "airport")),
                   textInput("icaoID", "Enter ICAO ID (optional)"),
                   numericInput("hours", "Lookback Hours", value = 24),
                   actionButton("updateButton", "Update"),
                   actionButton("downloadButton", "Save to file"),
                   selectInput("fileFormat", "File Format", 
                               choices = c(".csv" = "csv", ".xls" = "xls")),
                   downloadButton("downloadSubsetButton", "Download Subset Data")
                 ),
                 mainPanel(
                   # Display of returned data
                   verbatimTextOutput("downloadedDataSummary"),
                   DTOutput("downloadedDataTable"),
                   tableOutput("first10Rows")  # Display first 10 rows here
                 )
               )
             )
    )
    ,
    
    # Sixth tab: Data Exploration
    # ui.R
    
    tabPanel("Data Download",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   # Input fields for API querying
                   selectInput("endpoint", "Select Endpoint",
                               choices = c("metar", "taf", "airport")),
                   textInput("icaoID", "Enter ICAO ID (optional)"),
                   numericInput("hours", "Lookback Hours", value = 24),
                   actionButton("updateButton", "Update"),
                   actionButton("downloadButton", "Save to file"),
                   selectInput("fileFormat", "File Format", 
                               choices = c(".csv" = "csv", ".xls" = "xls")),
                   downloadButton("downloadSubsetButton", "Download Subset Data")
                 ),
                 mainPanel(
                   # Display of returned data
                   verbatimTextOutput("downloadedDataSummary"),
                   DTOutput("downloadedDataTable"),
                   tableOutput("first10Rows")  # Display first 10 rows here
                 )
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
        tags$li("Wind: Analyze and visualize wind speed data by state.", 
                "The output consists of a collection of density plots that show the distribution of wind speed observations ordered by mean descending.",
                "The output also contains a frequency table grouped by wind speeds summed over the state and by state and ordered by state.",
                tags$ul(
                  tags$li("How To use:"),
                  tags$li("Select a valid ICAO ID to analyze. A comprehensive list of icaoIDs can be found on the \"Valid icaoIDs\" tab. Optionally this field can be left blank to view all airports."),
                  tags$li("The hours field allows you to collect the records going backward from present to however many hours you set this field to."),
                  tags$li("When finished, press the `update` button to display a plot and table.")
                )
        ),
        tags$li("Visibility: Examine visibility data across different airports."),
        tags$li("Temperature: Explore temperature variations and trends."),
        tags$li("Airport Statistics: View various statistics related to airports."),
        tags$li("Data Download: Download aviation weather data for offline analysis."),
        tags$li("Data Exploration: Interactively explore the aviation weather dataset."),
        tags$li("Valid icaoIDs: Reference different ICAO ID codes and their associated states.",
                tags$ul(
                  tags$li("There are multiple icaoIDs you can use throughout this dashboard but only the following are allowed."),
                    tags$li("@TOP - `top 39 airports in the US`"),
                    tags$li("@TOPE - `top airports in the eastern US`"),
                    tags$li("@TOPC - `top airports in the central US`"),
                    tags$li("@TOPW - `top airports in the western US`"),
                    tags$li("@USN - `major airports in the northern US region`"),
                    tags$li("@USS - `major airports in the southern US region`"),
                    tags$li("@USE - `major airports in the eastern US region`"),
                    tags$li("@USW - `major airports in the western US region`"),
                    tags$li("#US - `all airports in the US`"),
                    tags$li("@<state_abbrev.> - `all airports by state`"),
                    tags$li("Individual icaoIDs.  Examples include single entries: `KAPF` or multiple in the format `KAPF,KATL,KRDU`"),
                  tags$li("The AWC website provides the different cases for what is acceptable text entries. This tab is a quick reference guide to show you what is acceptable and what the different categories capture."),
                  tags$li("You can find more information about these categories at the website ",
                          tags$a(href="https://aviationweather.gov/data/api/help/", "Aviation Weather Center"),
                          " beneath the section Metar > IDs.")
                )
        )
      )
    )),
    

    
    
    
    
    
    
    
    
    
# Eighth tab: Valid icaoIDs
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