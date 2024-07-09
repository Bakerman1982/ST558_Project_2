# ST558_Project_2

PURPOSE:  

The purpose of this app is to provide a national and regional view of the airport wind, temperature, visibility weather statistics through interactive tabs. There is also a tab that shows at a regional level the various lengths and widths of airport runways.


The data used in this app is sourced from three API endpoints found at the Aviation Weather Center (AWC). The AWC is a component of the Federal Department of Commerce’ National Weather Service Agency (NWS) and the National Oceanic and Atmospheric Administration (NOAA).

METAR – Meteorological Aerodrome Report. This API reports current weather conditions and retains a log of recent weather.
TAF – Terminal Area Forecast. This API reports future forecast weather conditions. This API does not keep record of historical forecasts made.
AIRPORT – This API contains general airport statistics; runway length, width, surface type, elevation, global positioning, orientation to true and magnetic north, etc.
Reference: Aviation Weather Center



PACKAGES:

install.packages(c("shiny", "tidyverse", "dplyr", "httr", "jsonlite", "stringr", "purrr", "gapminder", "shinydashboard", "ggplot2", "sf", "rnaturalearth", "rnaturalearthdata", "lwgeom", "viridis", "ggridges", "writexl"))

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
#library(rnaturalearth)
#library(rnaturalearthdata)
#library(lwgeom)
library(viridis)
library(ggridges)

*commented out were packages I was going to use but did not have time to make the geographical mapping that I wanted to do.



PRODUCT:

ui.R code:


server.R code: