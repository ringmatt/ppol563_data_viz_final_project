temporalData5YearACS <- function(group, geo = "county subdivision", years = c(2012), states = c(NULL), counties = 1, 
                                 geometry = FALSE, specificity = 1, cb = F){
  '
  This function pulls uses ACS 1-Year data to create a dataframe of different census variables by state and year
  Packages Required   - censusapi, plyr and dplyr
  Functions Required  - extract1YearACSData & changeColNames
  Inputs:
  group     - A 1 letter, 5 digit code representing a unique census data table. 
                See the link in the markdown above for more information.
  geo - A choice of the geography to hone in on
  years      - Years, as a list of integers, between 2009 and 2018 to extract data from
  states    - States, as a vector of strings, from 01-56 Excluding 3, 7, 14, 43, 52
  counties  - List of vectors, where each vector is for a given state\'s counties
  specificity - The higher this value, the more specific the variables allowed in the final dataset
  cb  - Stands for "cartographic boundaries". When false, this will use the TIGER/Line files. When true, will use the tigris package defaults. 
        MUST BE SET TO FALSE FOR TRACT DATA TO WORK.
  
  Outputs: The outputs are returned in a list
  df        - A dataframe where each row is a given state and year
  '
  
  # Creates a new dataframe
  df <- data.frame()
  
  # Loops through the years
  for (yr in years){
    
    # Runs the function to extract ACS data, then saves the data and labels
    dfTemp <- extract5YearACSData(group, geo, year = yr, states = states, counties = counties, 
                                  geometry = geometry, specificity = specificity, cb = cb)
    
    # Binds to the existing dataframe
    df <- rbind.fill(df, dfTemp)
  }
  
  # Returns the dataframe
  return(df)
  
}