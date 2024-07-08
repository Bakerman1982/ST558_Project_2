
library(shiny)
library(tidyverse)
library(dplyr)
library(httr)
library(jsonlite)
library(stringr)
library(purrr)
library(gapminder)
library(shinydashboard)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(lwgeom)
library(viridis)
library(ggridges)

# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel("Aviation Weather Dashboard"),
  
    # Navigation bar with seven tabs
    navbarPage(
      title = "Dashboard",



  tabPanel("Wind",
      sidebarLayout(
        sidebarPanel(
          textInput("icaoID", "ICAO ID:", placeholder = "Enter valid icaoID"),
          tags$small("See Valid icaoID tab for acceptable entries"),
          textInput("hours", "Hours:", placeholder = "Enter hours"),
          tags$small("Enter a number corresponding to how far back for data retrieval. Leave blank for 24 hours."),
            actionButton("updateButton", "Update")
        ),
        mainPanel(
          plotOutput("windPlot"),
          uiOutput("windTableTitle"),
          tableOutput("windTable")
        )
      )
  ),



  tabPanel("Temperature",
      sidebarLayout(
        sidebarPanel(
          textInput("icaoID", "ICAO ID:", placeholder = "Enter valid icaoID"),
          tags$small("See Valid icaoID tab for acceptable entries"),
          textInput("hours", "Hours:", placeholder = "Enter hours"),
          tags$small("Enter a number corresponding to how far back for data retrieval. Leave blank for 24 hours."),
            actionButton("updateButton", "Update")
        ),
        mainPanel(
          plotOutput("tempPlot"),
          uiOutput("tempTableTitle"),
          tableOutput("tempTable"),
          plotOutput("tempBarChart"),
          uiOutput("tempSummaryTitle"),
          tableOutput("tempSummaryTable")
        )
      )
  ),



  tabPanel("Visibility",
      sidebarLayout(
        sidebarPanel(
           textInput("icaoID_visib", "ICAO ID:", placeholder = "Enter valid icaoID"),
           tags$small("See Valid icaoID tab for acceptable entries"),
           textInput("hours_visib", "Hours:", placeholder = "Enter hours"),
           tags$small("Enter a number corresponding to how far back for data retrieval. Leave blank for 24 hours."),
            actionButton("updateButton_visib", "Update")
        ),
        mainPanel(
          plotOutput("visibHeatmap"),
          uiOutput("visibSummaryTitle"),
          tableOutput("visibSummaryTable")
        )
      )
  ),



  tabPanel("Airport Statistics",
      sidebarLayout(
        sidebarPanel(
          textInput("icaoID", "ICAO ID:", placeholder = "Enter valid icaoID"),
          tags$small("See Valid icaoID tab for acceptable entries"),
          actionButton("updateButton", "Update")
        ),
        mainPanel(
          style = "position: relative; top: 0px; height: calc(100vh - 60px);",
          plotOutput("airportStatsPlot", height = "400px"),  # Adjust height as needed
          plotOutput("airport_plot", height = "600px")  # Adjust height as needed
        )
      )
  ),



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



  tabPanel("About", fluidPage(
      fluidRow(
        column(width = 6,
               img(src = "NOAA_Logo.png", height = "200px", width = "200px")
        ),
        column(width = 6,
               img(src = "NWS_Logo.png", height = "200px", width = "200px")
        )
      ),

      h3("About This App"))),
    
    
    
    
    
    
    
    
    
    
    
    
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

    
  ) #NAVBAR
  
) #FLUIDPAGE

















###BELOW NEEDS TO BE FORMATTED AND ADDED TO ABOUT PAGE. 
,
p("The purpose of this app is to provide comprehensive aviation weather data analysis and visualization. It is designed to help users explore various aspects of aviation weather through interactive and informative tabs."),
h4("Data Source"),
p("The data used in this app is sourced from various aviation weather APIs endpoints from the Aviation Weather Center by NOAA. These endpoints include:"),
tags$ul(
  tags$li("metar - Metoerological Aerodrome Report. This report contains current weather conditions and a log of conditions."),
  tags$li("taf - Terminal Area Forecast. This report is a forecast of the weather conditions."),
  tags$li("airport - Airport statistics. This report contains general statistics about an airport; including surface type, runway length etc.")
),

tags$a(href="https://aviationweather.gov", "Aviation Weather Center"),

h4("Purpose of Each Tab"),
tags$ul(
  h5("Wind: Analyze and visualize wind speed data by state."), 
  tags$ul(
    tags$li("The output consists of a collection of density plots that show the distribution of wind speed observations ordered by mean descending."),
    tags$li("The output also contains a frequency table grouped by wind speeds summed over the state and by state and ordered by state."),
    tags$li("How To use:"),
    tags$li("Select a valid ICAO ID to analyze. A comprehensive list of icaoIDs can be found on the \"Valid icaoIDs\" tab. Optionally this field can be left blank to view all airports."),
    tags$li("The hours field allows you to collect the records going backward from present to however many hours you set this field to."),
    tags$li("When finished, press the `update` button to display a plot and table.")
  ),
),




h5("Visibility: Examine visibility data across different airports."),
h5("Temperature: Explore temperature variations and trends."),
h5("Airport Statistics: View various statistics related to airports."),
h5("Data Download: Download aviation weather data for offline analysis."),
h5("Data Exploration: Interactively explore the aviation weather dataset."),
h5("Valid icaoIDs: Reference different ICAO ID codes and their associated states.",
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
))




