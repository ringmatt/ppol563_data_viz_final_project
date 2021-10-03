## Import Data & Packages

library(tidyverse)

# Data

df <- 
  read.csv("data/raw/hud_homelessness_2007_2020_by_CoC.csv") %>%
  as_tibble() %>%
  select(c(coc_number = CoC.Number, coc_name = CoC.Name, year = Year,
           exp_homelessness = Overall.Homeless))

# Crosswalk

crosswalk <-
  read.csv("data/raw/crosswalks/county_CoC.csv") %>%
  as_tibble() %>%
  select(c(GEOID = county_fips, coc_number, coc_name, pct_cnty_pop_coc))

## Merge to county level

df <- df %>%
  left_join(crosswalk) %>%
  select(c(GEOID, year, coc_number, coc_exp_homelessness = exp_homelessness, pct_cnty_pop_coc))

## Save data

df %>%
  write.csv("data/processed/hud_homelessness.csv")
