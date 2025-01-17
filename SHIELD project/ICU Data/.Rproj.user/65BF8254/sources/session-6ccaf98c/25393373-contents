install.packages("ggplot2")
install.packages("sf")
install.packages("tidyverse")
install.packages("tigris")
install.packages("leaflet")

library(ggplot2)
library(ggmap)
library(sf)
library(tidyverse)
library(zipcodeR)
library(tigris)
library(leaflet)
library(dplyr)

Data = read.csv(file.choose(), sep=",", header=T)
my_addresses = Data$Full.Address


register_google(key = "AIzaSyAkwdJeM7LFVthD5em9YkYchOADPuupai0")


geo_data = map_df(my_addresses, geocode, .id = "address")\

options(tigris_class = "sf")

# Download IL state boundary
il_state_boundary = states(cb = TRUE) %>% 
  filter(STUSPS == "IL")

data = il_state_boundary


map = leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  setView(lng = -89.3985, lat = 40.6331, zoom = 6)%>%
  addPolygons(data = il_state_boundary, fill = FALSE, color = "black", weight = 4)



if(nrow(geo_data) > 0) {
  for(i in 1:nrow(geo_data)) {
    map = map %>% addCircleMarkers(lng = geo_data$lon[i], lat = geo_data$lat[i], radius = 1, popup = my_addresses[i], fillColor = "blue", color = "blue", fillOpacity = 0.8)
  }
}

map


options(tigris_use_cache = TRUE)

# Download ZIP code boundaries for Illinois and filter for 60153
zip_code_boundary = zctas(cb = FALSE, state = "IL", year = 2010) %>% 
  filter(ZCTA5CE10 = c("60153","60804"))

leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  setView(lng = -87.841695, lat = 41.879198, zoom = 10) %>%
  addPolygons(data = il_state_boundary, fill = FALSE, color = "black", weight = 2, smoothFactor = 0.5) %>%
  addPolygons(data = zip_code_boundary, fillColor = "blue", fillOpacity = 0.2, color = "black", weight = 2, smoothFactor = 0.5)

#************************************************************************************************************************************************************************************




          