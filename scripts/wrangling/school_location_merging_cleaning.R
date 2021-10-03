## Data and package imports

library(tidyverse)

# Data

# Initializes dataframe
df <- data.frame()

# Public Schools
for (i in 2015:2019){
  df <- df %>%
    rbind(
      read.csv(paste0("data/raw/school_locations/public_school_locations_", as.character(i), ".csv")) %>%
        select(c(lat = colnames(.)[grepl("LAT",colnames(.))], lon = colnames(.)[grepl("LON",colnames(.))])) %>%
        mutate(year = i,
               school_type = "public")
    )
}

# Private Schools
for (i in c(2015, 2017)){
  df <- df %>%
    rbind(
      read.csv(paste0("data/raw/school_locations/private_school_locations_", as.character(i), ".csv")) %>%
        select(c(lat = colnames(.)[grepl("LAT",colnames(.))], lon = colnames(.)[grepl("LON",colnames(.))])) %>%
        mutate(year = i,
               school_type = "private")
    )
}

# Postsecondary Schools
for (i in 2015:2019){
  df <- df %>%
    rbind(
      read.csv(paste0("data/raw/school_locations/postsecondary_school_locations_", as.character(i), ".csv")) %>%
        select(c(lat = colnames(.)[grepl("LAT",colnames(.))], lon = colnames(.)[grepl("LON",colnames(.))])) %>%
        mutate(year = i,
               school_type = "postsecondary")
    )
}

## Location to Census

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

# Solution using Census API
tract_fips <- vector(length = nrow(df))
for (i in 1:nrow(df)){
  ## Check "Replacement has length zero" issue
  tract_fips[i] <- substr(latlong2fips(df$lat[i], df$lon[i]), 1, 11)
  if (i/500-as.integer(i/500) == 0){
    print(round(i/nrow(df)*100,2))
  }
}

# Getting the error "Error in tract_fips[i] <- substr(latlong2fips(df$lat[i], df$lon[i]), 1,  : 
# replacement has length zero
# at i = 100498


# Saves as a column in the dataframe
df$GEOID <- tract_fips

## Grouping by Tract & year

df_grouped <- df %>%
  mutate(value = 1) %>%
  pivot_wider(id_cols = c(lat, lon, year), names_from = school_type, values_from = value) %>%
  group_by(GEOID, year) %>%
  summarize(public_schools = sum(public, na.rm = T),
            private_schools = sum(private, na.rm = T),
            postsecondary_schools = sum(postsecondary, na.rm = T))

## Save

df_grouped %>%
  write.csv("data/processed/schools.csv")






