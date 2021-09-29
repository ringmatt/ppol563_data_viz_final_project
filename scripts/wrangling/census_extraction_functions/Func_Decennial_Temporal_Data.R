temporalDataDecennial <- function(groups, geo = "county subdivision", years = c(2010), states = c(NULL), counties = c(NULL), 
                                 geometry = FALSE, specificity = 1, cb = F){
  '
  This function pulls uses the decennial Census data to create a dataframe of different census variables by state and year
  Packages Required   - censusapi, plyr and dplyr
  Functions Required  - extract1YearACSData & changeColNames
  Inputs:
  group     - A list of group codes for each decennial census (as group codes for the same data change between groups)
              Cannot currently have multiple groups per year. May implement this using a dictionary in the future.
  geo - A choice of the geography to hone in on
  years      - Years, as a list of integers, of 2000 and/or 2010 to extract data from
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
  
  # Initializes an iterator to simultaneous loop through groups as years is looped
  # Currently cannot have multiple groups per year
  i = 1
  
  # Loops through the years
  for (yr in years){
    
    # Runs the function to extract ACS data, then saves the data and labels
    dfTemp <- extractDecennialData(groups[i], geo, year = yr, states = states, counties = counties, 
                                  geometry = geometry, specificity = specificity, cb = cb)
    
    i = i + 1
    
    # Binds to the existing dataframe
    df <- rbind.fill(df, dfTemp)
  }
  
  # Returns the dataframe
  return(df)
  
}