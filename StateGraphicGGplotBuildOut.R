#Creating a dummy tibble to use to build out the ggplot
metar_samp <- build_url(endpoint = "metar", icaoIDs = "@USS")


#the setup
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("maps")
library("tools")
theme_set(theme_bw())


world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
(sites <- data.frame(longitude = c(-80.144005, -80.109), latitude = c(26.479005, 
                                                                    26.83)))



(sites <- st_as_sf(sites, coords = c("longitude", "latitude"), 
                   crs = 4326, agr = "constant"))

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)

sf::sf_use_s2(FALSE)
states <- cbind(states, st_coordinates(st_centroid(states)))


states$ID <- toTitleCase(states$ID)
head(states)

states$nudge_y <- -1
states$nudge_y[states$ID == "Florida"] <- 0.5
states$nudge_y[states$ID == "South Carolina"] <- -1.5

counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("florida", counties$ID))
counties$area <- as.numeric(st_area(counties))
head(counties)


ggplot(data = world) +
  geom_sf() +
  geom_sf(data = counties, fill = NA, color = gray(.5)) +
  geom_point(metar_samp,mapping = aes(x = lon, y = lat), size =4, shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-79, -110), ylim = c(23, 40), expand = FALSE)+
  theme_set(theme_bw())

https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html

