library(tidyverse)
library(tidygeocoder)
library(sf)

# Read data
df <- read_csv("projects/dollarStores.csv")
indiana <- df %>% filter (state == "MA")
indiana <- indiana %>% mutate(fulladdress = paste(address, city, state, zip, sep = ", "))

# Set API key
Sys.setenv(GOOGLEGEOCODE_API_KEY="AIzaSyDlyjiA43X2yEuaBld_oiW6DK7F4ZoeP_U")

# Geocode addresses
addresses <- as_tibble(indiana)
geocode_google <- addresses %>%
  tidygeocoder::geocode(fulladdress,
                        method = "google")

# Remove point outside state
geocode_google <- geocode_google %>% filter(zip != 47850)

# Download Indiana county data
census_sf <- st_read("https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json") %>% 
  filter(STATE == "25")

# Convert geocoded data to shapefile
geocode_google_sf <- geocode_google %>%
  filter(!is.na(lat), !is.na(long)) %>%
  st_as_sf(coords = c("long", "lat"), crs = st_crs(census_sf))

# Find intersection with county data
intersected <- st_intersects(geocode_google_sf, census_sf)

# Add FIPS codes to geocoded data
geocode_google_sf <- geocode_google_sf %>%
  mutate(intersection = as.integer(intersected),
         fips = if_else(is.na(intersection), "",
                        census_sf$id[intersection]))

geocode_google <- merge(geocode_google, geocode_google_sf[,c("address", "fips")], by = "address")

write_csv(geocode_google, "projects/dollarStores_MA.csv")
