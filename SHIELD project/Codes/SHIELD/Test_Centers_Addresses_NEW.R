install.packages("ggplot2")
install.packages("sf")
install.packages("tidyverse")
install.packages("tigris")
install.packages("leaflet")

library(ggplot2)
library(sf)
library(tidyverse)
library(zipcodeR)
library(tigris)
library(leaflet)
library(dplyr)

Data = read_excel(path = file.choose(), sheet = "Sheet2")
my_addresses = Data$`Full Address`


register_google(key = "AIzaSyAkwdJeM7LFVthD5em9YkYchOADPuupai0")


geo_data = map_df(my_addresses, geocode, .id = "address")


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
