normalizeVars <- function(df){
  '
  This function normalizes ACS 1-Year data to create a dataframe of different census variables/capita by state and year
  Packages Required - dplyr
  Inputs:
  df        - Dataframe where observations are by state and year, and there are columns of counts per category
  
  IMPORTANT!  Columns must include "state", "year", and "Estimate"
  
  Outputs: The outputs are returned in a list
  df        - A dataframe where each row is a given state and year, and each column is a normalized census variable
              Margin of Error and Estimates are normalized separately, as each has a separate "total" variable
  '

  # Extracts state, county, and year columns 
  # We do not need to worry about whether the dataframe has counties or not
  # Removing them when they aren't there won't throw errors
  df_st_yr <- subset(df, select = c(state, county, year))
  df_vars <- subset(df, select = -c(state, county, year))
  
  # Divides each dataframe by the total in each row
  df_vars <- sweep(df_vars, MARGIN = 1, df_vars$Estimate, FUN = "/")
  
  # Removes the total
  df_vars <- subset(df_vars, select = -c(Estimate))
  
  # Binds into a single dataframe
  df <- cbind(df_st_yr, df_vars)
  
  # Returns the dataframe
  return(df)
}