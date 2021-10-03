## Loading packages

library(tidyverse)
library(haven)
library(stringi)

## Load and Clean Data

df <- read_dta("data/raw/building_permits_1990_2019.dta") %>%
  as_tibble() %>%
  mutate(GEOID = paste0(str_pad(as.character(state), 2, pad = "0"), 
                               str_pad(as.character(county), 3, pad = "0")),
         units = unit_sf + unit_2f + unit_3_4 + unit_mf,
         unit_2_4 = unit_sf + unit_3_4) %>%
  select(c(GEOID, year, units, unit_sf, unit_2_4, unit_mf)) %>%
  group_by(GEOID, year) %>%
  summarize(units = sum(units),
            units_sf = sum(unit_sf),
            units_2_4 = sum(unit_2_4),
            units_mf = sum(unit_mf)) %>%
  arrange(GEOID, year)

## Save Cleaned Data

df %>%
  write.csv("data/processed/building_permits_1990_2019.csv")
