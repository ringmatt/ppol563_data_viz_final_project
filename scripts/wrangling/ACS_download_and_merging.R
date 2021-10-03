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
census_api_key(Sys.getenv("CENSUS_API_KEY"))

library(sf)

# Import self-made functions
source("scripts/wrangling/census_extraction_functions/ACSFunctionsPackage.R")
acsFunctionsPackage()

## Census Data Extractor

# Included so rbind.fill will work
require(plyr)

# includes all states
#st <- str_pad(c(1:2, 4:6, 8:13, 15:42, 44:51, 53:56), 2, pad = "0")

# Only midwestern states based on Census definitions
#st <- str_pad(c(39, 26, 18, 17, 55, 27, 29, 19, 31, 20, 38, 46), 2, pad = "0")

# Only great lakes states (Based on NOAA definition,
# dropping PA and NY to save space and because they're split with the North Atlantic)
#st <- str_pad(c(39, 26, 27, 55, 17, 18))

# Only DMV Area
st <- c("11", "24", "51")

# Sets the years 
years <- seq(2009, 2019)

# Inequality
df <- temporalData5YearACS("B19083", geo = "county", 
                                            years = 2010:2019, states = st, 
                                            geometry = F, specificity = 2) %>% 
  as_tibble() %>%
  select(c(GEOID, year, inequality_index = estimate))

# Median home value
df_tract_whome <- temporalData5YearACS("B25077", geo = "county", years = years, 
                                       states = st, geometry = F, 
                                       specificity = 2) %>% 
  as_tibble() %>%
  select(c(GEOID, year, med_home_val = estimate))

# Appends to main dataset and removes old object
df <- df %>%
  left_join(df_tract_whome, by = c("GEOID", "year"))
rm(df_tract_whome)
  

# Aggregate interest/dividends/rental income
df_tract_wint <- temporalData5YearACS("B19064", geo = "county", years = years, 
                                       states = st, geometry = F, 
                                       specificity = 2) %>% 
  as_tibble() %>%
  select(c(GEOID, year, agg_interest_div_rental = estimate))

# Appends to main dataset and removes old object
df <- df %>%
  left_join(df_tract_wint, by = c("GEOID", "year"))
rm(df_tract_wint)

# Median Household Income
df_tract_iinc <- temporalData5YearACS("B19013", geo = "county", years = years, 
                                                 states = st, geometry = F, 
                                                 specificity = 2) %>% 
  as_tibble() %>%
  select(c(GEOID, year, med_home_income = estimate))

# Appends to main dataset and removes old object
df <- df %>%
  left_join(df_tract_iinc, by = c("GEOID", "year"))
rm(df_tract_iinc)

# Employment
df_tract_icivlabor <- temporalData5YearACS("B23025", geo = "county", 
                                         years = 2011:2019, 
                                       states = st, geometry = F, 
                                       specificity = 4) 

df_tract_iunemployed <- df_tract_icivlabor %>%
  as_tibble() %>%
  filter(variable == "Estimate_Total_In labor force_Civilian labor force_Unemployed") %>%
  select(c(GEOID, year, civ_unemp = estimate))

# Appends to main dataset and removes old object
df <- df %>%
  left_join(df_tract_iunemployed, by = c("GEOID", "year"))
rm(df_tract_iunemployed)
  
df_tract_iemployed <- df_tract_icivlabor %>%
  as_tibble() %>%
    filter(variable == "Estimate_Total_In labor force_Civilian labor force_Employed") %>%
    select(c(GEOID, year, civ_emp = estimate))
# Appends to main dataset and removes old object
df <- df %>%
  left_join(df_tract_iemployed, by = c("GEOID", "year"))
rm(df_tract_iemployed)
rm(df_tract_icivlabor)

# Poverty
df_tract_ipover <- temporalData5YearACS("B17001", geo = "county", 
                                         years = years, 
                                         states = st, geometry = F, 
                                         specificity = 2) %>% 
  as_tibble() %>%
  filter(variable == "Estimate_Total_Income in the past 12 months below poverty level") %>%
  mutate(poverty_rate = estimate/summary_est) %>%
  select(c(GEOID, year, poverty_rate))

# Appends to main dataset and removes old object
df <- df %>%
  left_join(df_tract_ipover, by = c("GEOID", "year"))
rm(df_tract_ipover)

# Internet Access
df_tract_ointernet <- temporalData5YearACS("B28011", geo = "county", 
                                        years = 2017:2019, 
                                        states = st, geometry = F, 
                                        specificity = 2) %>% 
  as_tibble() %>%
  filter(variable == "Estimate_Total_No Internet access") %>%
  mutate(internet_access = (summary_est - estimate)/summary_est) %>%
  select(c(GEOID, year, internet_access))

# Appends to main dataset and removes old object
df <- df %>%
  left_join(df_tract_ointernet, by = c("GEOID", "year"))
rm(df_tract_ointernet)

# Health Insurance Coverage
df_tract_ohealthcov <- temporalData5YearACS("B27001", geo = "county", 
                                           years = 2012,
                                           states = st, geometry = F, 
                                           specificity = 4) %>%
  as_tibble() %>%
  filter(grepl("With health insurance coverage", variable)) %>%
  group_by(GEOID, year) %>%
  dplyr::summarize(healthcare_coverage = sum(estimate)/mean(summary_est, na.rm = T))

# Appends to main dataset and removes old object
df <- df %>%
  left_join(df_tract_ohealthcov, by = c("GEOID", "year"))
rm(df_tract_ohealthcov)

# Means of Transportation
df_tract_otransport <- temporalData5YearACS("B08301", geo = "county", 
                                            years = years, 
                                            states = st, geometry = F, 
                                            specificity = 2)

df_tract_opublictransit <- df_tract_otransport %>%
    as_tibble() %>%
  filter(variable == "Estimate_Total_Public transportation (excluding taxicab)") %>%
  select(c(GEOID, year, pub_transit = estimate))

# Appends to main dataset and removes old object
df <- df %>%
  left_join(df_tract_opublictransit, by = c("GEOID", "year"))
rm(df_tract_opublictransit)

df_tract_oworkfromhome <- df_tract_otransport %>%
  as_tibble() %>%
  filter(variable == "Estimate_Total_Worked at home") %>%
  select(c(GEOID, year, work_from_home = estimate))

# Appends to main dataset and removes old object
df <- df %>%
  left_join(df_tract_oworkfromhome, by = c("GEOID", "year"))
rm(df_tract_oworkfromhome)
rm(df_tract_otransport)
  

# Public or SNAP Assistance
df_tract_apubsnap <- temporalData5YearACS("B19058", geo = "county", 
                                            years = 2010:2019, 
                                            states = st, geometry = F, 
                                            specificity = 2) %>%
  as_tibble() %>%
  filter(variable == "Estimate_Total_With cash public assistance or Food Stamps/SNAP") %>%
  mutate(percent_with_cash_pub_assist_or_snap = estimate/summary_est) %>%
  select(c(GEOID, year, percent_with_cash_pub_assist_or_snap))

# Appends to main dataset and removes old object
df <- df %>%
  left_join(df_tract_apubsnap, by = c("GEOID", "year"))
rm(df_tract_apubsnap)

# Medicare Insurance Status
df_tract_amedins <- temporalData5YearACS("C27006", geo = "county", 
                                         years = 2012:2019, 
                                         states = st, geometry = F, 
                                         specificity = 4) %>%
  as_tibble() %>%
  filter(grepl("With Medicare coverage", variable)) %>%
  group_by(GEOID, year) %>%
  dplyr::summarize(medicare_coverage = sum(estimate)/mean(summary_est, na.rm = T))

# Appends to main dataset and removes old object
df <- df %>%
  left_join(df_tract_amedins, by = c("GEOID", "year"))
rm(df_tract_amedins)

# Race

df_tract_drace <- temporalData5YearACS("B02001", geo = "county", 
                                       years = years, 
                                       states = st, geometry = F, 
                                       specificity = 2) #%>% 
df_tract_dpop <- df_tract_drace %>%
  as_tibble() %>%
  filter(variable == "Estimate_Total") %>%
  select(c(GEOID, year, pop = estimate))

# Appends to main dataset and removes old object
df <- df %>%
  left_join(df_tract_dpop, by = c("GEOID", "year"))
rm(df_tract_dpop)

df_tract_dwhite <- df_tract_drace %>%
  as_tibble() %>%
  filter(variable == "Estimate_Total_White alone") %>%
  select(c(GEOID, year, white_pop = estimate))

# Appends to main dataset and removes old object
df <- df %>%
  left_join(df_tract_dwhite, by = c("GEOID", "year"))
rm(df_tract_dwhite)

df_tract_dblack <- df_tract_drace %>%
  as_tibble() %>%
  filter(variable == "Estimate_Total_Black or African American alone") %>%
  select(c(GEOID, year, black_pop = estimate))

# Appends to main dataset and removes old object
df <- df %>%
  left_join(df_tract_dblack, by = c("GEOID", "year"))
rm(df_tract_dblack)

df_tract_dnative <- df_tract_drace %>%
  as_tibble() %>%
  filter(variable == "Estimate_Total_American Indian and Alaska Native alone") %>%
  select(c(GEOID, year, nativeAm_pop = estimate))

# Appends to main dataset and removes old object
df <- df %>%
  left_join(df_tract_dnative, by = c("GEOID", "year"))
rm(df_tract_dnative)

df_tract_dasian <- df_tract_drace %>%
  as_tibble() %>%
  filter(variable == "Estimate_Total_Asian alone") %>%
  select(c(GEOID, year, asian_pop = estimate))

# Appends to main dataset and removes old object
df <- df %>%
  left_join(df_tract_dasian, by = c("GEOID", "year"))
rm(df_tract_dasian)

df_tract_dislander <- df_tract_drace %>%
  as_tibble() %>%
  filter(variable == "Estimate_Total_Native Hawaiian and Other Pacific Islander alone") %>%
  select(c(GEOID, year, islander_pop = estimate))

# Appends to main dataset and removes old object
df <- df %>%
  left_join(df_tract_dislander, by = c("GEOID", "year"))
rm(df_tract_dislander)

df_tract_dmixed <- df_tract_drace %>%
  as_tibble() %>%
  filter(variable == "Estimate_Total_Two or more races") %>%
  select(c(GEOID, year, mixedRace_pop = estimate))

# Appends to main dataset and removes old object
df <- df %>%
  left_join(df_tract_dmixed, by = c("GEOID", "year"))
rm(df_tract_dmixed)

df_tract_dother <- df_tract_drace %>%
  as_tibble() %>%
  filter(variable == "Estimate_Total_Some other race alone") %>%
  select(c(GEOID, year, other_pop = estimate))

# Appends to main dataset and removes old object
df <- df %>%
  left_join(df_tract_dother, by = c("GEOID", "year"))
rm(df_tract_dother)
rm(df_tract_drace)


# Hispanic
df_tract_dlatinx <- temporalData5YearACS("B03001", geo = "county", 
                                       years = years, 
                                       states = st, geometry = TRUE, 
                                       specificity = 2) %>% 
  as_tibble() %>%
  filter(variable == "Estimate_Total_Hispanic or Latino") %>%
  select(c(GEOID, year, latinx_pop = estimate, geometry))

# Geometry

df_tract_geometry <- df_tract_dlatinx %>%
  select(c(GEOID, geometry)) %>%
  distinct(GEOID, .keep_all = T)

# Removes geometry from latinx data

df_tract_dlatinx <- df_tract_dlatinx %>%
  select(-c(geometry))

# Appends to main dataset and removes old object
df <- df %>%
  left_join(df_tract_dlatinx, by = c("GEOID", "year"))
rm(df_tract_dlatinx)


# Rent
df_tract_rent <- temporalData5YearACS("B25064", geo = "county", 
                                         years = years, 
                                         states = st, geometry = F, 
                                         specificity = 2) %>%
  as_tibble() %>%
  select(c(GEOID, year, med_rent = estimate))

# Appends to main dataset and removes old object
df <- df %>%
  left_join(df_tract_rent, by = c("GEOID", "year"))
rm(df_tract_rent)

# Monthly Owner Costs
df_tract_homecosts <- temporalData5YearACS("B25088", geo = "county", 
                                      years = years, 
                                      states = st, geometry = F, 
                                      specificity = 2) %>%
  as_tibble() %>%
  filter(variable == "Estimate_Median selected monthly owner costs (dollars)_Total") %>%
  select(c(GEOID, year, med_monthly_home_cost = estimate))

# Appends to main dataset and removes old object
df <- df %>%
  left_join(df_tract_homecosts, by = c("GEOID", "year"))
rm(df_tract_homecosts)

# Removes non-distinct values
df <- df %>%
  as_tibble() %>%
  distinct(.keep_all = T)

# Remove plyr so dplyr will be used
detach(package:plyr)

# Save
df %>%
  arrange(year, GEOID) %>%
  write.csv("data/processed/census.csv")

# Shapefile
df_spatial <- df %>%
  left_join(df_tract_geometry) %>%
  distinct(.keep_all = T) %>%
  st_as_sf() %>%
  # Transforms to NAD83 coordinates
  st_transform(crs = 5070)

st_write(df_spatial, dsn = "data/processed/spatial/census", layer = "census", driver = "ESRI Shapefile")
