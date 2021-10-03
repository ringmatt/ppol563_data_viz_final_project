## Import data and packages

library(tidyverse)

# crosswalk

df_crosswalk <-
  read.csv("data/raw/crosswalks/county_fips.csv") %>%
  select(c(GEOID = FIPS, county = Name, state = State)) %>%
  mutate(GEOID = str_pad(GEOID, 5, pad = "0"),
         county = tolower(county)) 

df <- 
  read.csv("data/raw/hospital_ratings_and_locations.csv") %>%
  as_tibble() %>%
  select(c(county = County.Name, state = State, #provider_id = Provider.ID, 
         type = Hospital.Type, owner = Hospital.Ownership,
         ems = Emergency.Services, rating = Hospital.overall.rating)) %>%
  mutate(county = tolower(county),
         acute_care = ifelse(type == "Acute Care Hospitals", 1, 0),
         critical_access = ifelse(type == "Critical Access Hospitals", 1, 0),
         childrens = ifelse(type == "Childrens", 1, 0),
         public = ifelse(grepl("Government", owner), 1, 0),
         private = ifelse((owner == "Proprietary") | (owner == "Physician"), 1, 0),
         non_profit = ifelse(grepl("Government", owner), 1, 0),
         tribal = ifelse(grepl("Tribal", owner), 1, 0),
         rating = ifelse(rating == "Not Available", NA, as.integer(rating))) %>%
  select(-c(type, owner)) %>%
  group_by(county, state) %>%
  summarize(rating = mean(rating, na.rm = T),
            ems_hospitals = sum(ems),
            acute_care_hospitals = sum(acute_care),
            critical_access_hospitals = sum(critical_access),
            childrens_hospitals = sum(childrens),
            public_hospitals = sum(public),
            private_hospitals = sum(private),
            non_profit_hospitals = sum(non_profit),
            tribal_hospitals = sum(tribal)) %>%
  left_join(df_crosswalk) %>%
  ungroup() %>%
  select(c(GEOID, county, state, rating, ems_hospitals, acute_care_hospitals, 
           critical_access_hospitals, childrens_hospitals, public_hospitals,
           private_hospitals, non_profit_hospitals, tribal_hospitals))
  
## Save

df %>%
  write.csv("data/processed/hospital_rating_location.csv")

