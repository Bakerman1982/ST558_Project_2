
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
  titlePanel("Airport Weather Dashboard"),
  
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
          #style = "position: relative; top: 0px; height: calc(100vh - 60px);",
          
          plotOutput("airport_plot", width = "900px", height = "600")  # Adjust height as needed
        )
      )
  ),



  tabPanel("Data Download",
           sidebarLayout(
             sidebarPanel(
               selectInput("endpoint_dd", "Select Endpoint:",
                           choices = c("metar", "taf", "airport")),
               selectInput("format_dd", "Select Format:",
                           choices = c(".csv", ".xls")),
               downloadButton("downloadData", "Download Data")
             ),
             mainPanel(
               tableOutput("dataTableDownload")
             )
           )
  ),



  tabPanel("Data Exploration",
       sidebarLayout(
         sidebarPanel(
           selectInput("data_source", "Select Data Source:",
                       choices = c("metar", "airport")),
           uiOutput("var_select_ui"),
           selectInput("plot_type", "Select Plot Type:",
                       choices = c("Scatter Plot", "Bar Plot", "Histogram", "Box Plot")),
           selectInput("summary_type", "Select Summary Type:",
                       choices = c("Mean", "Median", "Standard Deviation", "Count")),
           uiOutput("facet_var_ui")
         ),
         mainPanel(
           plotOutput("data_plot"),
           verbatimTextOutput("data_summary")
         )
       )
  ),



  tabPanel("About",
      fluidPage(
        titlePanel("About this app"),
        fluidRow(
          column(width = 4,
                 img(src = "NOAA_Logo.png", height = "400px", width = "400px")
          ),
          column(width = 4,
                 img(src = "NWS_Logo.png", height = "400px", width = "400px")
          ),
          column(width = 4,
                 img(src = "AWC.jfif", height = "400px", width = "400px")
          )
        ),
        fluidRow(
          column(
            width = 12,
            p("The purpose of this app is to provide a national and regional view of the airport wind, temperature, visibility weather statistics through interactive tabs. There is also a tab that shows at a regional level the various lengths and widths of airport runways."),
            br(),
            p("The data used in this app is sourced from three API endpoints found at the Aviation Weather Center (AWC). The AWC is a component of the Federal Department of Commerce’ National Weather Service Agency (NWS) and the National Oceanic and Atmospheric Administration (NOAA)."),
            tags$ul(
              tags$li("METAR – Meteorological Aerodrome Report. This API reports current weather conditions and retains a log of recent weather."),
              tags$li("TAF – Terminal Area Forecast. This API reports future forecast weather conditions. This API does not keep record of historical forecasts made."),
              tags$li("AIRPORT – This API contains general airport statistics; runway length, width, surface type, elevation, global positioning, orientation to true and magnetic north, etc.")
            ),
            p("Reference: ", a(href="https://aviationweather.gov", "Aviation Weather Center")),
            br(),
            h3("Purpose of Tabs"),
            p("Each tab accepts reactive inputs that you can manipulate to contextualize the data you are viewing. How to use each of the interactive tabs:"),
            tags$ol(
              tags$li("Every tab has an icaoID field. You can optionally leave this blank to capture a national snapshot or you can enter any one of the valid icaoIDs found on that namesakes tab."),
              tags$li("There is also a second text field that states hours. This allows you to look back in the METAR report for however long you set it.")
            ),
            h4("Wind: visualize and summarize wind speed data by icaoID category."),
            tags$ul(
              tags$li("The output consists of a collection of density plots that show the distribution of wind speed observations ordered by mean descending. The icaoID selected will determine what is produced."),
              tags$li("There should also be a frequency table grouped by wind speed increments summed over the state and by state and ordered by state.")
            ),
            h4("Temperature: Explore temperature variations and trends."),
            tags$ul(
              tags$li("A vertical bar chart showing the min and max temperature observations by icaoID category."),
              tags$li("Will be accompanied by a summary table of the min, max and range ordered by state.")
            ),
            h4("Visibility: Examine visibility data across different airports."),
            tags$ul(
              tags$li("I tried implementing a heat map to show the different visibility min/max by icaoID category."),
              tags$li("The summary table will display min/max and a range ordered by state.")
            ),
            h4("Airport Statistics: View various statistics related to airports."),
            tags$ul(
              tags$li("This tab provides a graphical summary of runway length/width. I chose a vertical violin plot because it visually captures the width and length of runways if the plot is set to be proportional to each other. In keeping with aeronautical themes, the violin plot often produces a silhouette in the shape of an airplane.")
            ),
            h4("Data Download"),
              tags$ul(
                tags$li("This tab lets the user download the data found in this app. A convenience table populates to the right to give the user a look-before-you-buy opportunity.  Finally a drop down box affords you the option of chosing between a .csv or .xls format."),
              ),
            h4("Data Exploration"),
              tags$ul(
                tags$li("The data exploration tab provides a summary look at the data used throughout this app.  On the left are fields for placing parameters on the dataset.  The familiar data source allows for the choice between `metar` and `airport` data.  Variables will populate the fields below based on the condition set be the data source.  You will have the option of choosing between a scatter plot, bar plot, box plot or a histogram.  Summary type changes the summary table beneath the plots and finally a faceting option for faceting the plots created."),
              ),
            h4("Valid icaoIDs: Reference different ICAO ID codes and their associated states."),
            p("There are multiple icaoIDs you can use throughout this dashboard but only the following are allowed."),
            p("@TOP - `top 39 airports in the US`"),
            p("@TOPE - `top airports in the eastern US`"),
            p("@TOPC - `top airports in the central US`"),
            p("@TOPW - `top airports in the western US`"),
            p("@USN - `major airports in the northern US region`"),
            p("@USS - `major airports in the southern US region`"),
            p("@USE - `major airports in the eastern US region`"),
            p("@USW - `major airports in the western US region`"),
            p("#US - `all airports in the US`"),
            p("@<state_abbrev.> - `all airports by state`"),
            p("In addition to all of the above icaoIDs allow, you can also add single or multiple concatenated icaoIDs of the airport codes from any of the lists displayed."),
            p("Example 1. If you wanted to look at Naples, FL airport, you can simply enter KAPF into the field."),
            p("Example 2. If you wanted to look at Naples, FL, Atlanta, GA, and Raleigh, NC, you can enter comma separated values like this: KAPF,KATL,KRDU."),
            p("Do not enter spaces anywhere inside the concatenation of the app will throw an error."),
            p("The AWC website provides a comprehensive list of the different cases for acceptable text entries. This tab is a quick reference guide to show you what is acceptable and what the different categories capture. You can find more information about these icaoIDs at ", a(href="https://aviationweather.gov/data/api/help/", "https://aviationweather.gov/data/api/help/"), " beneath the section METAR > IDs.")
          )
        )
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

  ) #NAVBAR_brace
) #FLUIDPAGE_brace
