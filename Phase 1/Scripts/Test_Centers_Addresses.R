# Install and load necessary packages
if (!requireNamespace("ggmap", quietly = TRUE)) install.packages("ggmap")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
install.packages("sf")
if (!requireNamespace("tigris", quietly = TRUE)) install.packages("tigris")

library(ggmap)
library(sf)
library(tigris)
library(dplyr)


# Set your Google API key
api_key = "AIzaSyAkwdJeM7LFVthD5em9YkYchOADPuupai0"
register_google(key = api_key)

# Function to get real addresses for a vector of locations
get_real_addresses <- function(locations) {
  addresses <- sapply(locations, function(location) {
    # Attempt to geocode the location, catching any errors
    result <- tryCatch({
      geocode(location, output = "more")
    }, error = function(e) {
      # Return NA in a data frame format to match expected structure
      data.frame(address = NA, stringsAsFactors = FALSE)
    })
    
    # Check if the result is a data frame and has at least one row
    if (is.data.frame(result) && nrow(result) > 0) {
      # Ensure the 'address' column exists before attempting to access it
      if ("address" %in% names(result)) {
        return(result$address[1])
      }
    }
    # Return NA if the above conditions are not met
    return(NA)
  }, USE.NAMES = FALSE)
  
  return(addresses)
}

test_centers = read.csv(file.choose(), sep=",", header=T)

unique_values = unique(test_centers$Account.Name)
unique_values = as.data.frame(unique_values)

# Assuming the locations are in a column named 'Lab Center Location'
Test_Center_Locations = test_centers$Zip

# Get addresses
addresses = get_real_addresses(Test_Center_Locations)

# Combine locations and addresses into a data frame
results = data.frame(Test_Center_Location = Test_Center_Locations, Test_Center_Address = addresses)

# Path to the output Excel file
write.csv(results, file = "C:/Users/skasaiesharifi/Documents/Test Centers Addresses_NEW.csv", row.names = FALSE)





options(tigris_use_cache = TRUE, tigris_class = "sf")

# Register Google API key
register_google(key = "AIzaSyAkwdJeM7LFVthD5em9YkYchOADPuupai0") 

# Read data
Data = read.csv(file.choose(), sep=",", header=TRUE)

# Geocode addresses
addresses = Data$Test_Center_Address
geocoded_addresses = geocode(addresses)

# Download the shapefile for Illinois
illinois_shape = states(cb = TRUE) %>% 
  filter(STUSPS == "IL")

# Adjust the zoom level here; a smaller number zooms out
illinois_map = get_map(location = 'Illinois', zoom = 6, size = 4)

# Prepare the map with ggmap
base_map = ggmap(illinois_map)

# Add the boundaries of Illinois with significantly bolder lines
boundaries_map = base_map +
  geom_sf(data = illinois_shape, inherit.aes = FALSE, fill=NA, color="black", size=4) + # Significantly increase size for bolder lines
  geom_point(data = geocoded_addresses, aes(x = lon, y = lat), color = "red", size = 3, alpha = 0.5) +
  labs(title = "Test Centers Location")

# Plot the map with Illinois boundaries and geocoded addresses
print(boundaries_map)










