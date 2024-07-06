library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Aviation Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Wind", tabName = "wind"),
      menuItem("Visibility", tabName = "visibility"),
      menuItem("Temperature", tabName = "temperature"),
      menuItem("Airport Statistics", tabName = "airport_stats"),
      menuItem("Data Download", tabName = "data_download"),
      menuItem("Data Exploration", tabName = "data_exploration"),
      menuItem("About", tabName = "about")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "wind",
              h2("Wind Data")),
      tabItem(tabName = "visibility",
              h2("Visibility Data")),
      tabItem(tabName = "temperature",
              h2("Temperature Data")),
      tabItem(tabName = "airport_stats",
              h2("Airport Statistics")),
      tabItem(tabName = "data_download",
              h2("Data Download")),
      tabItem(tabName = "data_exploration",
              h2("Data Exploration")),
      tabItem(tabName = "about",
              h2("About This Dashboard"))
    )
  )
)

shinyApp(ui = ui, server = function(input, output) {})