popAdjust <- function(df, stPop, years, states){
  '
  This function uses ACS 1-Year data to create a dataframe of different census variables/capita by state and year
  Packages Required - dplyr and tidyverse
  Data Required     - Requires the `IDAndYearlyPop.csv` file if popAdjust is set to TRUE
  Inputs:
  df        - Dataframe where observations are by state and year, and there are columns of counts per category
  stPop     - Data file for when adjusting by population. Years must be columns and rows as state IDs.
  years     - Year, as an integer, between 2005 and 2019 to extract data from
  states    - States, as a vector of strings, from 01-56 Excluding 3, 7, 14, 43, 52
  
  Outputs: The outputs are returned in a list
  df        - A dataframe where each row is a given state and year, and each column is a census variable adjusted for by population
  '
  
  # Creates a 3 columns dataframe of year, state, and population as needed
  stPop <- stPop %>%
    subset( StateID %in% all_of(states)) %>%
    gather(year, value, all_of(years)) %>%
    spread(StateID, value) %>%
    subset(select = c("year", all_of(states))) %>%
    gather(StateID, value, all_of(states)) %>%
    drop_na() %>%
    rename(state = StateID, state_population = value)
  
  # Merges these dataframes to add a new column of each state's population by year
  df <- merge(df, stPop)
  
  # Normalizes all non-year, state, or population column by state population
  df[, -c(1,2,length(colnames(df)))] <- sweep(df[, -c(1,2,length(colnames(df)))], MARGIN = 1, df$state_population, FUN = "/")
  
  # Returns the dataframe
  return(df)
  
}