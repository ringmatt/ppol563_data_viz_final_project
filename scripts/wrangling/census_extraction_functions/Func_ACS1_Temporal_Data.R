temporalData1YearACS <- function(group, years, states, counties = NA, countyLevel = FALSE, specificity = 1){
  '
  This function pulls uses ACS 1-Year data to create a dataframe of different census variables by state and year
  Packages Required   - censusapi, plyr and dplyr
  Functions Required  - extract1YearACSData & changeColNames
  Inputs:
  group     - A 1 letter, 5 digit code representing a unique census data table. 
           See the link in the markdown above for more information.
  year      - Year, as an integer, between 2005 and 2019 to extract data from
  states    - States, as a vector of strings, from 01-56 Excluding 3, 7, 14, 43, 52
  counties  - List of vectors, where each vector is named for a given state
  countyLevel - Whether or not to look at the county level
  specificity - The higher this value, the more specific the variables allowed in the final dataset
  
  Outputs: The outputs are returned in a list
  df        - A dataframe where each row is a given state and year
  '
  
  # Creates a new dataframe
  df <- data.frame()
  
  # Loops through the years
  for (yr in years){
    
    # Runs the function to extract ACS data, then saves the data and labels
    dataAndLabels <- extract1YearACSData(group, year = yr, states = states, counties, countyLevel, specificity)
    dfTemp <- dataAndLabels[[1]]
    labelsTemp <- dataAndLabels[[2]]
    
    # Changes the column names to readable labels
    dfTemp <- changeColNames(dfTemp, labelsTemp)
    
    # Binds to the existing dataframe
    df <- rbind.fill(df, dfTemp)
  }
  
  # Returns the dataframe
  return(df)
  
}