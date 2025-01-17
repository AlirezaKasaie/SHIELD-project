# Load necessary libraries
library(ggmap)
library(tigris)
library(sf)
library(leaflet)
library(dplyr)

register_google(key = "AIzaSyAkwdJeM7LFVthD5em9YkYchOADPuupai0")

# Get the Illinois map
illinois_map <- get_map(location = 'Illinois', zoom = 6, maptype = "roadmap")
ggmap(illinois_map)

# Read the updated zip codes data
ZIPCODES_UPDATED <- read.csv(file.choose(), sep = ",", header = TRUE)
ZIPCODES_UPDATED$zipcode <- as.character(ZIPCODES_UPDATED$zipcode)  # Ensure zipcodes are character

# Set tigris options to cache data
options(tigris_use_cache = TRUE)

# Get the zip code boundaries
zip_code_boundary <- zctas(cb = FALSE, state = "IL", year = 2010)

zip_code_boundary_filtered <- zip_code_boundary[zip_code_boundary$ZCTA5CE10 %in% ZIPCODES_UPDATED$zipcode, ]

zip_code_boundary_wgs84 <- st_transform(zip_code_boundary_filtered, crs = 4326)


# Get the Illinois state boundary
il_boundary <- states(cb = TRUE) %>%
  filter(STUSPS == "IL")

str(il_boundary)  # Inspect the structure of il_boundary

# Transform the Illinois state boundary to WGS84 coordinate reference system
il_boundary_wgs84 <- st_transform(il_boundary, crs = 4326)

ZIPCODES_UPDATED$zipcode <- as.character(ZIPCODES_UPDATED$zipcode)  # Ensure zipcodes are character

# Get the zip code boundaries
zip_code_boundary <- zctas(cb = FALSE, state = "IL", year = 2010)

# Filter the zip code boundaries
zip_code_boundary_filtered <- zip_code_boundary[zip_code_boundary$ZCTA5CE10 %in% ZIPCODES_UPDATED$zipcode, ]

# Transform the coordinate reference system
zip_code_boundary_wgs84 <- st_transform(zip_code_boundary_filtered, crs = 4326)

leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  setView(lng = -89.3985, lat = 40.6331, zoom = 6) %>%  # Centered on Illinois
  addPolygons(data = il_boundary_wgs84, fillColor = "transparent", color = "black", weight = 5, smoothFactor = 0.5) %>%  # Add Illinois state boundary
  addPolygons(data = zip_code_boundary_wgs84, fillColor = "red", fillOpacity = 0.2, color = "black", weight = 2, smoothFactor = 0.5)  # Add zip code boundaries

################################################################################
################################################################################
################################################################################

ZIPCODES_ADI <- read.csv(file.choose(), sep = ",", header = TRUE)

ZIPCODES_UPDATED_NEW = merge(ZIPCODES_UPDATED,ZIPCODES_ADI, by = c('zipcode'))

ZIPCODES_UPDATED_NEW$zipcode <- as.character(ZIPCODES_UPDATED_NEW$zipcode)  # Ensure zipcodes are character

# Ensure zipcodes are character
ZIPCODES_UPDATED_NEW$zipcode <- as.character(ZIPCODES_UPDATED_NEW$zipcode)  

# Set tigris options to cache data
options(tigris_use_cache = TRUE)

# Get the zip code boundaries
zip_code_boundary <- zctas(cb = FALSE, state = "IL", year = 2010)

# Filter the zip code boundaries based on the zip codes in your data
zip_code_boundary_filtered <- zip_code_boundary[zip_code_boundary$ZCTA5CE10 %in% ZIPCODES_UPDATED_NEW$zipcode, ]

# Merge the boundary data with your data containing ADI scores
zip_code_boundary_filtered <- merge(zip_code_boundary_filtered, ZIPCODES_UPDATED_NEW, by.x = "ZCTA5CE10", by.y = "zipcode")

# Transform the coordinate reference system
zip_code_boundary_wgs84 <- st_transform(zip_code_boundary_filtered, crs = 4326)

# Add color column based on ADI_score
zip_code_boundary_wgs84$color <- ifelse(zip_code_boundary_wgs84$ADI_score > 4, "#FFCCCB", "#ADD8E6")

# Create a custom yellow marker icon
yellowPinIcon <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-yellow.png",
  iconWidth = 25, iconHeight = 41,  # Adjust the size of the pin
  iconAnchorX = 12, iconAnchorY = 41  # Positioning of the pin icon
)


# Extract the coordinates for zip code "60153"
zipcode_60153 <- zip_code_boundary_wgs84[zip_code_boundary_wgs84$ZCTA5CE10 == "60153", ]

# If the zip code is not found, print a message
if (nrow(zipcode_60153) == 0) {
  stop("Zip code 60153 not found in the data!")
}

# Get the centroid (or center point) of the zip code polygon to place the pin
# Extract the geometry of the zip code to avoid warnings
geometry_60153 <- st_geometry(zipcode_60153)

# Calculate the centroid based on the geometry only
centroid_60153 <- st_centroid(geometry_60153)

# Check if centroid is computed correctly
print(st_coordinates(centroid_60153))

manual_coords_60153 <- c(-87.8438, 41.8809)  # Approximate coordinates for Maywood, IL (60153)

# Get the Illinois state boundary
il_boundary <- states(cb = TRUE) %>%
  filter(STUSPS == "IL")

# Transform the Illinois state boundary to WGS84 coordinate reference system
il_boundary_wgs84 <- st_transform(il_boundary, crs = 4326)

# Custom CSS for the Legend
library(htmltools) 
# Custom CSS for the Legend
# Custom CSS for the Legend
custom_css <- "
  <style>
    .leaflet-left .leaflet-control {
      left: 10px !important; /* Move legend to the left */
      bottom: 10px !important; /* Position legend at the bottom */
    }
    .custom-legend {
      background: white;
      padding: 20px; /* Increase padding for a bigger legend box */
      border-radius: 10px;
      box-shadow: 0 0 10px rgba(0, 0, 0, 0.2);
      font-size: 20px;
      width: 350px; /* Increase the width of the legend box */
      line-height: 2; /* Double-spacing for legend content */
    }
    .custom-legend span {
      display: inline-block;
      width: 20px; /* Set width to make the box square */
      height: 20px; /* Set height to make the box square */
      border: 1px solid black; /* Add border for better visibility */
    }
    .custom-legend img {
      vertical-align: middle;
      margin-right: 5px;
    }
  </style>
"

# Custom HTML Legend
custom_legend <- HTML("
  <div class='custom-legend'>
    <b>ADI Score Classification</b><br>
    <div><span style='background-color: #FFCCCB;'></span> High Disadvantaged Zip Codes</div>
    <div><span style='background-color: #ADD8E6;'></span> Low Disadvantaged Zip Codes</div>
    <div><img src='https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-yellow.png' width='20' height='30'> Location of the LUMC</div>
  </div>
")

# Leaflet map
leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  setView(lng = -89.3985, lat = 40.6331, zoom = 6) %>%  # Centered on Illinois
  addPolygons(data = il_boundary_wgs84, fillColor = "transparent", color = "black", weight = 5, smoothFactor = 0.5) %>%
  addPolygons(data = zip_code_boundary_wgs84, fillColor = ~color, fillOpacity = 0.5, color = "black", weight = 2, smoothFactor = 0.5,
              popup = ~paste("Zipcode:", ZCTA5CE10, "<br>ADI Score:", ADI_score)) %>%
  addMarkers(lng = -87.8431, lat = 41.8795,  # Coordinates for zip code 60153 (Maywood, IL)
             popup = "Zipcode: 60153 (Maywood, IL)",
             icon = yellowPinIcon) %>%
  addControl(custom_legend, position = "bottomleft") %>%  # Add the custom legend
  htmlwidgets::prependContent(HTML(custom_css))  # Add custom CSS to the HTML


