## Loading API & Packages

# Add Census key
Sys.setenv(CENSUS_KEY="2f018b883dd35f1fd0f30cdf8b1a8663cff19bdb")

# packages
library(censusapi)
library(tidyverse)

# Used to extract tract data from GEOIDs
require(stringi)

# Imports the tidycensus package, which is a simpler way of working with census data
library(tidycensus)
census_api_key("2f018b883dd35f1fd0f30cdf8b1a8663cff19bdb")

library(sf)

# Import self-made functions
source("scripts/wrangling/census_extraction_functions/ACSFunctionsPackage.R")
acsFunctionsPackage()

## Longitudinal Tracts (consistent over decades)

# Longitudinal Tract Database - Convert 2000's tracts to 2010's tracts
ltdb <- read.csv("data/raw/temporal_census_crosswalk_2000_2010.csv") %>%
  # Selects only important variables
  select(c(tract = trtid00, tract_10 = trtid10, weight, changetype)) %>%
  as_tibble()

## Census Data Extractor

# Included so rbind.fill will work
require(plyr)

# includes all states
st <- str_pad(c(1:2, 4:6, 8:13, 15:42, 44:51, 53:56), 2, pad = "0")

# Sets the years 
years <- seq(2009, 2019)

# Inequality
df_tract_wineq <- temporalData5YearACS("B19083", geo = "tract", 
                                            years = 2010:2019, states = st, 
                                            geometry = F, specificity = 2) %>% 
  as_tibble() %>%
  select(c(GEOID, year, inequality_index = estimate))

# Median home value
df_tract_whome <- temporalData5YearACS("B25077", geo = "tract", years = years, 
                                       states = st, geometry = F, 
                                       specificity = 2) %>% 
  as_tibble() %>%
  select(c(GEOID, year, med_home_val = estimate))

# Aggregate interest/dividends/rental income
df_tract_wint <- temporalData5YearACS("B19064", geo = "tract", years = years, 
                                       states = st, geometry = F, 
                                       specificity = 2) %>% 
  as_tibble() %>%
  select(c(GEOID, year, agg_interest_div_rental = estimate))

# Median Household Income
df_tract_iinc <- temporalData5YearACS("B19013", geo = "tract", years = years, 
                                                 states = st, geometry = F, 
                                                 specificity = 2) %>% 
  as_tibble() %>%
  select(c(GEOID, year, med_home_income = estimate))

# Employment
df_tract_iemploy <- temporalData5YearACS("B23025", geo = "tract", 
                                         years = 2011:2019, 
                                       states = st, geometry = F, 
                                       specificity = 4)%>% 
  as_tibble() %>%
  filter(variable %in% c("Estimate_Total_In labor force_Civilian labor force_Unemployed",
                         "Estimate_Total_In labor force_Civilian labor force_Employed",
                         "Estimate_Total_In labor force_Armed Forces")) %>%
  pivot_wider(id_cols = c(GEOID, year), values_from = estimate,
              names_from = variable) %>%
  select(c(GEOID, year, 
           civ_unemp = `Estimate_Total_In labor force_Civilian labor force_Unemployed`,
           civ_emp = `Estimate_Total_In labor force_Civilian labor force_Employed`,
           armfor = `Estimate_Total_In labor force_Armed Forces`)) %>%
  mutate(unemp_rate = (civ_unemp)/(civ_emp + armfor)) %>%
  select(c(GEOID, year, unemp_rate))

# Poverty
df_tract_ipover <- temporalData5YearACS("B17001", geo = "tract", 
                                         years = years, 
                                         states = st, geometry = F, 
                                         specificity = 2) %>% 
  as_tibble() %>%
  filter(variable == "Estimate_Total_Income in the past 12 months below poverty level") %>%
  mutate(poverty_rate = estimate/summary_est) %>%
  select(c(GEOID, year, poverty_rate))

# Internet Access
df_tract_ointernet <- temporalData5YearACS("B28011", geo = "tract", 
                                        years = 2017:2019, 
                                        states = st, geometry = F, 
                                        specificity = 2) %>% 
  as_tibble() %>%
  filter(variable == "Estimate_Total_No Internet access") %>%
  mutate(internet_access = (summary_est - estimate)/summary_est)
  select(c(GEOID, year, internet_access))

# Health Insurance Coverage
df_tract_ohealthcov <- temporalData5YearACS("B27001", geo = "tract", 
                                           years = 2012:2019, 
                                           states = st, geometry = F, 
                                           specificity = 4) %>%
  as_tibble() %>%
  filter(grepl("With health insurance coverage", variable)) %>%
  group_by(GEOID, year) %>%
  dplyr::summarize(healthcare_coverage = sum(estimate)/mean(summary_est, na.rm = T))



# Means of Transportation
df_tract_otransport <- temporalData5YearACS("B08301", geo = "tract", 
                                            years = years, 
                                            states = st, geometry = F, 
                                            specificity = 2) %>%
  as_tibble() %>%
  filter(variable %in% c("Estimate_Total_Public transportation (excluding taxicab)",
                         "Estimate_Total_Worked at home")) %>%
  pivot_wider(id_cols = c(GEOID, year), values_from = estimate,
              names_from = variable) %>%
  select(c(GEOID, year, 
           pub_transit = `Estimate_Total_Public transportation (excluding taxicab)`,
           work_from_home = `Estimate_Total_Worked at home`))
  

# Public or SNAP Assistance
df_tract_apubsnap <- temporalData5YearACS("B19058", geo = "tract", 
                                            years = 2010:2019, 
                                            states = st, geometry = F, 
                                            specificity = 2) %>%
  as_tibble() %>%
  filter(variable == "Estimate_Total_With cash public assistance or Food Stamps/SNAP") %>%
  mutate(percent_with_cash_pub_assist_or_snap = estimate/summary_est) %>%
  select(c(GEOID, year, percent_with_cash_pub_assist_or_snap))
  

# Medicare Insurance Status
df_tract_amedins <- temporalData5YearACS("C27006", geo = "tract", 
                                         years = 2012:2019, 
                                         states = st, geometry = F, 
                                         specificity = 4) %>%
  as_tibble() %>%
  filter(grepl("With Medicare coverage", variable)) %>%
  group_by(GEOID, year) %>%
  dplyr::summarize(medicare_coverage = sum(estimate)/mean(summary_est, na.rm = T))

# Race
df_tract_drace <- temporalData5YearACS("B02001", geo = "tract", 
                                       years = years, 
                                       states = st, geometry = F, 
                                       specificity = 2) %>% 
  as_tibble() %>%
  filter(variable %in% c("Estimate_Total",
                         "Estimate_Total_White alone",
                         "Estimate_Total_Black or African American alone",
                         "Estimate_Total_American Indian and Alaska Native alone",
                         "Estimate_Total_Asian alone",
                         "Estimate_Total_Native Hawaiian and Other Pacific Islander alone",
                         "Estimate_Total_Two or more races",
                         "Estimate_Total_Some other race alone")) %>%
  pivot_wider(id_cols = c(GEOID, year), values_from = estimate,
              names_from = variable) %>%
  select(c(GEOID, year, pop = Estimate_Total, 
           white_pop = `Estimate_Total_White alone`,
           black_pop = `Estimate_Total_Black or African American alone`,
           nativeAm_pop = `Estimate_Total_American Indian and Alaska Native alone`,
           asian_pop = `Estimate_Total_Asian alone`,
           islander_pop = `Estimate_Total_Native Hawaiian and Other Pacific Islander alone`,
           mixedRace_pop = `Estimate_Total_Two or more races`,
           other_pop = `Estimate_Total_Some other race alone`)) %>%
  mutate(aapi_pop = asian_pop + islander_pop) %>%
  select(-c(asian_pop, islander_pop))

# Hispanic
df_tract_dlatinx <- temporalData5YearACS("B03001", geo = "tract", 
                                       years = years, 
                                       states = st, geometry = TRUE, 
                                       specificity = 2) %>% 
  as_tibble() %>%
  filter(variable == "Estimate_Total_Hispanic or Latino") %>%
  select(c(GEOID, year, latinx_pop = estimate, geometry))

# Rent
df_tract_rent <- temporalData5YearACS("B25064", geo = "tract", 
                                         years = years, 
                                         states = st, geometry = F, 
                                         specificity = 2) %>%
  as_tibble() %>%
  select(c(GEOID, year, med_rent = estimate))

# Monthly Owner Costs
df_tract_homecosts <- temporalData5YearACS("B25088", geo = "tract", 
                                      years = years, 
                                      states = st, geometry = F, 
                                      specificity = 2) %>%
  as_tibble() %>%
  filter(variable == "Estimate_Median selected monthly owner costs (dollars)_Total") %>%
  select(c(GEOID, year, med_monthly_home_cost = estimate))

# Remove plyr so dplyr will be used
detach(package:plyr)


## Merging the data

# Merges the data on tract and year
df_tract <- df_tract_race %>% left_join(df_tract_Latino, by = c("tract", "year")) %>%
  left_join(df_tract_HouseholdIncome, by = c("tract", "year")) %>%
  left_join(df_tract_Edu, by = c("tract", "year")) %>%
  left_join(df_tract_age, by = c("tract", "year")) %>%
  left_join(df_tract_house, by = c("tract", "year"))

# Adjusts pre-2010 tracts (ie only 2009 for this data) to 2010 tracts
df_tract <- df_tract %>%
  # Adjusting 2009 data by LTDB
  left_join(ltdb) %>%
  # Replaces the 2009 tracts with the 2010 tracts
  mutate(tract = ifelse(is.na(tract_10), tract, tract_10)) %>%
  select(-c(tract_10)) %>%
  # Adjusts population variables by the interpolation weights,
  # summing duplicate tracts (those combined from 2+ prior tracts)
  mutate(weight = ifelse(is.na(weight), 1, weight)) %>%
  group_by(tract, year) %>%
  summarize(Black_Pop = as.integer(sum(Black_Pop*weight)),
            Latino_Pop = as.integer(sum(Latino_Pop*weight)),
            Total_Pop = as.integer(sum(Total_Pop*weight)))


# Displays the data
df_tract %>% as_tibble()


df %>%
  write.csv("../../ProducedDatasets/TractDemogPerYear.csv")