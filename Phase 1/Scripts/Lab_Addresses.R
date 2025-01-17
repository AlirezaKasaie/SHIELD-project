install.packages("readxl")
install.packages(c("ggplot2", "maps", "ggmap", "sf"))
install.packages("leaflet")

library(readxl)
library(ggplot2)
library(maps)
library(ggmap)
library(sf)
library(dplyr)
library(leaflet)

Data = read_excel(path = file.choose(), sheet = "Labs")
my_addresses = Data$`Full Address`


register_google(key = "AIzaSyAkwdJeM7LFVthD5em9YkYchOADPuupai0")


geo_data = map_df(my_addresses, geocode, .id = "address")


map = leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  setView(lng = -89.3985, lat = 40.6331, zoom = 6)%>%
  addPolygons(data = il_state_boundary, fill = FALSE, color = "black", weight = 4)



if(nrow(geo_data) > 0) {
  for(i in 1:nrow(geo_data)) {
    map = map %>% addMarkers(lng = geo_data$lon[i], lat = geo_data$lat[i], popup = addresses[i])
  }
}

map
