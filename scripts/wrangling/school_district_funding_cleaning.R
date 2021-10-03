## Import data and packages

library(tidyverse)

# Data


df <-
  read.csv("data/raw/school_district_funding.csv") %>%
  as_tibble() %>%
  mutate(NAME = tolower(NAME)) %>%
  select(c(name = NAME, state = STATE,  enrollment = ENROLL, year = YRDATA, total_revenue = TOTALREV,
           fed_revenue = TFEDREV, state_revenue = TSTREV, local_revenue = TLOCREV)) %>%
  mutate(fed_revenue_per_pupil = fed_revenue/enrollment,
         state_revenue_per_pupil = state_revenue/enrollment,
         local_revenue_per_pupil = local_revenue/enrollment,
         total_revenue_per_pupil = total_revenue/enrollment) %>%
  select(-c(fed_revenue, state_revenue, local_revenue, total_revenue)) %>%
  mutate(name = gsub("school ", "sch ", name)) %>%
  mutate(name = gsub("unified ", "unif ", name)) %>%
  mutate(name = gsub("district ", "dist ", name)) %>%
  mutate(name = gsub("dst ", "dist", name)) %>%
  mutate(name = gsub("community unit school district ", "cusd ", name)) %>%
  mutate(name = gsub("elementary ", "elem ", name)) %>%
  mutate(name = gsub("city ", "cty ", name)) %>%
  mutate(name = gsub("high school ", "hs ", name)) %>%
  mutate(name = gsub("h s", "hs", name)) %>%
  mutate(name = gsub("town ", "twn ", name)) %>%
  mutate(name = gsub("technology ", "tech ", name)) %>%
  mutate(name = gsub("regional ", "reg ", name)) %>%
  mutate(name = gsub("river ", "rvr ", name)) %>%
  mutate(name = gsub("mount ", "mt ", name)) %>%
  mutate(name = gsub("mountain ", "mt ", name)) %>%
  mutate(name = gsub("independent ", "ind ", name)) %>%
  mutate(name = gsub("community ", "comm ", name)) %>%
  mutate(name = gsub("saint ", "st ", name)) %>%
  mutate(name = gsub("public school district ", "isd ", name)) %>%
  mutate(name = gsub(" school", " sch", name)) %>%
  mutate(name = gsub(" unified", " unif", name)) %>%
  mutate(name = gsub(" district", " dist", name)) %>%
  mutate(name = gsub(" dst", "dist", name)) %>%
  mutate(name = gsub(" community unit school district", " cusd", name)) %>%
  mutate(name = gsub(" elementary", " elem", name)) %>%
  mutate(name = gsub(" city", " cty", name)) %>%
  mutate(name = gsub(" high school", " hs", name)) %>%
  mutate(name = gsub(" town", " twn", name)) %>%
  mutate(name = gsub(" technology", " tech", name)) %>%
  mutate(name = gsub(" regional", " reg", name)) %>%
  mutate(name = gsub(" river", " rvr", name)) %>%
  mutate(name = gsub(" mount", " mt", name)) %>%
  mutate(name = gsub(" mountain", " mt", name)) %>%
  mutate(name = gsub(" independent", " ind", name)) %>%
  mutate(name = gsub(" community", " comm", name)) %>%
  mutate(name = gsub(" saint", " st", name)) %>%
  mutate(name = gsub(" public school district", " isd", name)) %>%
  mutate(name = gsub("[[:punct:]]", "", name))

# Crosswalk

crosswalk <-
  readxl::read_excel("data/raw/crosswalks/tract_school_district.xlsx") %>%
  as_tibble() %>%
  mutate(NAME_LEA20 = tolower(NAME_LEA20)) %>%
  select(c(GEOID = TRACT, name = NAME_LEA20)) %>%
  arrange(GEOID) %>%
  mutate(name = gsub("school ", "sch ", name)) %>%
  mutate(name = gsub("unified ", "unif ", name)) %>%
  mutate(name = gsub("district ", "dist ", name)) %>%
  mutate(name = gsub("dst ", "dist", name)) %>%
  mutate(name = gsub("community unit school district ", "cusd ", name)) %>%
  mutate(name = gsub("elementary ", "elem ", name)) %>%
  mutate(name = gsub("city ", "cty ", name)) %>%
  mutate(name = gsub("high school ", "hs ", name)) %>%
  mutate(name = gsub("h s", "hs", name)) %>%
  mutate(name = gsub("town ", "twn ", name)) %>%
  mutate(name = gsub("technology ", "tech ", name)) %>%
  mutate(name = gsub("regional ", "reg ", name)) %>%
  mutate(name = gsub("river ", "rvr ", name)) %>%
  mutate(name = gsub("mount ", "mt ", name)) %>%
  mutate(name = gsub("mountain ", "mt ", name)) %>%
  mutate(name = gsub("independent ", "ind ", name)) %>%
  mutate(name = gsub("community ", "comm ", name)) %>%
  mutate(name = gsub("saint ", "st ", name)) %>%
  mutate(name = gsub("public school district ", "isd ", name)) %>%
  mutate(name = gsub(" school", " sch", name)) %>%
  mutate(name = gsub(" unified", " unif", name)) %>%
  mutate(name = gsub(" district", " dist", name)) %>%
  mutate(name = gsub(" dst", "dist", name)) %>%
  mutate(name = gsub(" community unit school district", " cusd", name)) %>%
  mutate(name = gsub(" elementary", " elem", name)) %>%
  mutate(name = gsub(" city", " cty", name)) %>%
  mutate(name = gsub(" high school", " hs", name)) %>%
  mutate(name = gsub(" town", " twn", name)) %>%
  mutate(name = gsub(" technology", " tech", name)) %>%
  mutate(name = gsub(" regional", " reg", name)) %>%
  mutate(name = gsub(" river", " rvr", name)) %>%
  mutate(name = gsub(" mount", " mt", name)) %>%
  mutate(name = gsub(" mountain", " mt", name)) %>%
  mutate(name = gsub(" independent", " ind", name)) %>%
  mutate(name = gsub(" community", " comm", name)) %>%
  mutate(name = gsub(" saint", " st", name)) %>%
  mutate(name = gsub(" public school district", " isd", name)) %>%
  mutate(name = gsub("[[:punct:]]", "", name))

## Merge to tract level

test <- crosswalk %>%
  left_join(df) %>%
  arrange(GEOID, name, state, year) %>%
  #group_by(GEOID, name, year) %>%
  #filter(n()>1) %>%
  filter(GEOID == "01015000300")
  distinct(GEOID, name, year, .keep_all = T)
