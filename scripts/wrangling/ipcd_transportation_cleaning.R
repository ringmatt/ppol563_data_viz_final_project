## Import data and packages

library(tidyverse)

df <- 
  read.csv("data/raw/ipcd_transportation.csv") %>%
  filter(  (POINT_ID != "") &
           (FAC_NAME != "") &
           (!grepl("AK", FAC_NAME))) %>%
  select(c(id = POINT_ID, lat = LATITUDE, lon = LONGITUDE, bus = MODE_BUS,
           air = MODE_AIR, train = MODE_RAIL, ferry = MODE_FERRY,
           bike = MODE_BIKE)) %>%
  distinct(id, .keep_all = TRUE)

## Functions

latlong2fips <- function(latitude, longitude) {
  url <- "https://geo.fcc.gov/api/census/block/find?format=json&latitude=%f&longitude=%f"
  url <- sprintf(url, latitude, longitude)
  
  # Extracts the URL as a json file
  json <- curl::curl(url)
  
  # Sets up to close the URL connection once the function finishes
  on.exit(close(json))
  
  # Reads the lines
  lines <- readLines(json)
  
  # Converts the lines to readable results
  result <- rjson::fromJSON(lines)
  
  # Returns the county fips
  return(result$Block$FIPS)
}

## To Tracts

# Solution using Census API
tract_fips <- vector(length = nrow(df))
for (i in 1:nrow(df)){
  tract_fips[i] <- substr(latlong2fips(df$lat[i], df$lon[i]), 1, 11)
}

# Saves as a column in the dataframe
df$GEOID <- tract_fips

# groups to tracts, counting the number of each type of stop per tract
df_grouped <- df %>%
  group_by(GEOID, year) %>%
  summarize(bike = sum(bike),
            bus = sum(bus),
            ferry = sum(ferry),
            train = sum(train),
            air = sum(air))

## Save

df_grouped %>%
  write.csv("data/processed/ipcd_transportation.csv")



