## Data and package imports

# package

library(tidyverse)
library(sf)

# data

# Load the below if editting existing 

df_census <-
  read.csv("data/processed/census.csv") %>%
  as_tibble() %>%
  select(-c(X))

df_sf <- 
  rgdal::readOGR('data/processed/spatial/census/census.shp') %>% 
  st_as_sf() %>%
  mutate(GEOID = as.integer(GEOID)) %>%
  select(GEOID, geometry)

df_homelessness <-
  read.csv("data/processed/hud_homelessness.csv") %>%
  as_tibble()

# calculates CoC population
df_homelessness_cocPop <-
  df_homelessness %>%
  left_join(df_census %>%
              select(c(GEOID, year, pop))) %>%
  group_by(coc_number, year) %>%
  summarize(coc_pop = sum(pop*pct_cnty_pop_coc, na.rm = T))

# weights homelessness counts by a county's population within a CoC
df_homelessness <-
  df_homelessness %>%
  left_join(df_census %>%
              select(c(GEOID, year, pop))) %>%
  left_join(df_homelessness_cocPop) %>%
  mutate(exp_homelessness = ifelse(!is.na(pop),
                                       coc_exp_homelessness*(pop*pct_cnty_pop_coc/coc_pop),
                                       NA)) %>%
  select(c(GEOID, year, exp_homelessness)) %>%
  filter(!is.na(exp_homelessness))

df_transit <-
  read.csv("data/processed/ipcd_transportation.csv") %>%
  as_tibble() %>%
  # Converts tract to county
  mutate(GEOID = as.integer(substr(as.character(GEOID), 1,
                        nchar(as.character(GEOID))-6))) %>%
  select(-c(X)) %>%
  # Groups by county
  group_by(GEOID) %>%
  summarize(bike_2021 = sum(bike),
            bus_2021 = sum(bus),
            ferry_2021 = sum(ferry),
            train_2021 = sum(train),
            air_2021 = sum(air))

# Only contains schools in the DMV area

df_schools <-
  read.csv("data/processed/schools.csv") %>%
  as_tibble() %>%
  # Converts tract to county
  mutate(GEOID = as.integer(substr(GEOID, 1, nchar(GEOID) - 6))) %>%
  group_by(GEOID, year) %>%
  summarize(
    public_schools = sum(public, na.rm = T),
    private_schools = sum(private, na.rm = T),
    postsecondary_schools = sum(postsecondary, na.rm = T)) %>%
  mutate(
    public_schools = ifelse(public_schools == 0, NA, public_schools),
    private_schools = ifelse(private_schools == 0, NA, private_schools),
    postsecondary_schools = ifelse(postsecondary_schools == 0, NA, postsecondary_schools)
  ) %>%
  fill(private_schools,
       public_schools,
       postsecondary_schools)

df_housing <-
  read.csv("data/processed/building_permits_1990_2019.csv") %>%
  as_tibble() %>%
  select(-c(X)) %>%
  arrange(GEOID, year)

# Note these are hospitals present in 2017
df_hospital <-
  read.csv("data/processed/hospital_rating_location.csv") %>%
  as_tibble() %>%
  select(-c(X, county, state)) %>%
  filter(!is.na(GEOID))

df_countyfips_crosswalk <-
  read.csv("data/raw/crosswalks/county_fips.csv") %>%
  as_tibble() %>%
  select(GEOID = FIPS, name = Name, state = State)

## Pre-Merge Cleaning

## Merging

df <- df_housing %>%
  left_join(df_countyfips_crosswalk) %>%
  left_join(df_homelessness) %>%
  left_join(df_transit) %>%
  left_join(df_census) %>%
  left_join(df_hospital) %>%
  left_join(df_schools) %>%
  #left_join(df_sf) %>%
  arrange(GEOID, year)

## Saving

# Non-Spatial

df %>%
  write.csv("data/processed/master_dataset.csv")

df %>%
  filter(GEOID %in% df_census$GEOID) %>%
  write.csv("data/processed/DMV_dataset.csv")

# Spatial
df %>%
  left_join(df_sf) %>%
  st_as_sf() %>%
  # Transforms to NAD83 coordinates
  st_transform(crs = 5070) %>%
  st_write(., dsn = "data/processed/spatial/master_dataset", layer = "master_dataset", driver = "ESRI Shapefile")

df %>%
  filter(GEOID %in% df_census$GEOID) %>%
  left_join(df_sf) %>%
  st_as_sf() %>%
  # Transforms to NAD83 coordinates
  st_transform(crs = 5070) %>%
  st_write(., dsn = "data/processed/spatial/DMV_dataset", layer = "DMV_dataset", driver = "ESRI Shapefile")


