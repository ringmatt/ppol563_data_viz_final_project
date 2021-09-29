extract1YearACSData <- function(group, year = 2012, states = c("13"), counties = NA, countyLevel = FALSE, specificity = 1){
    '
    This function pulls ACS 1-Year data based on a particular group, year, and as many states as desired
    Packages Required - plyr and dplyr
    Inputs:
    group     - A 1 letter, 5 digit code representing a unique census data table. 
             See the link in the markdown above for more information.
    year      - Year, as an integer, between 2005 and 2019 to extract data from
    states    - States, as a vector of strings, from 01-56 Excluding 3, 7, 14, 43, 52
    counties  - List of vectors, where each vector is named for a given state
    countyLevel - Whether or not to look at the county level
    specificity - The higher this value, the more specific the variables allowed in the final dataset
    
    Outputs: The outputs are returned in a list
    acsData   - A dataframe where each row is a state and each column is a variable
    labels    - The full description of each variable
    '
  
  # Makes counties into a labeled dictionary for use in a loop later
  names(counties) <- states
  
  # Pulls a list of variables for the chosen group and year
  data <- listCensusMetadata(
    name = "acs/acs1",
    vintage = year,
    group = group)
  
  # Removes all annotations, which cuts the size in half
  data <- data[which(data$predicateType != 'string'),]
  
  # Removes `Total!!` from all labels and changes all `!!` to `_`
  data$label <- gsub("!!", "_", data$label)
  data$label <- gsub("_Total", "", data$label)
  # These are here to deal with new labeling in 2019 data
  data$label <- gsub(":", "", data$label)
  #data$label <- gsub("", "_", data$label)
  
  # More layers, designated by `_`, means more levels of abstraction
  # This code counts those layers and removes any entry with a label showing
  # more than 1 layer of abstraction
  data$LayersOfAbstraction <- lengths(regmatches(data$label, gregexpr("_", data$label)))
  data <- subset(data, LayersOfAbstraction <= specificity)
  data <- subset(data, select = -c(LayersOfAbstraction))
  
  # Sorts the data by label
  data <- data[order(data$label),]
  
  # Saves both the names and labels in the data
  variables <- data$name
  labels <- data$label
  
  # Initializes the final dataset
  acsData <- data.frame()
  
  # Iterates over each state, extracting all necessary variables from the group
  if(countyLevel == FALSE){
    for (st in states){
      # Extracts ACS 1 year data at state level
      acsDataTemp <- getCensus(
        name = "acs/acs1", 
        vintage = year, 
        vars = variables,
        region = paste("state:", st, sep = ""))
      
      # Appends the year in a new column
      acsDataTemp$year <- year
      
      # Merges this new data with the current data frame
      # rbind.fill will fill any missing columns with NAs
      acsData <- rbind.fill(acsData, acsDataTemp)
    }
  }
  else if (class(counties) != "list"){
    for (st in states){
      # Extracts ACS 1 year data at county level
      acsDataTemp <- getCensus(
        name = "acs/acs1", 
        vintage = year, 
        vars = variables,
        region = "county",
        regionin = paste("state:", st, sep = ""))
      
      # Appends the year in a new column
      acsDataTemp$year <- year
      
      # Merges this new data with the current data frame
      # rbind.fill will fill any missing columns with NAs
      acsData <- rbind.fill(acsData, acsDataTemp)
    }
  }
  else{
    for (st in states){
      for (county in counties[st][[1]]){
        # Extracts ACS 1 year data for specific counties
        acsDataTemp <- getCensus(
          name = "acs/acs1", 
          vintage = year, 
          vars = variables,
          region = paste("county:", county, sep = ""),
          regionin = paste("state:", st, sep = ""))
        
        # Appends the year in a new column
        acsDataTemp$year <- year
        
        # Merges this new data with the current data frame
        # rbind.fill will fill any missing columns with NAs
        acsData <- rbind.fill(acsData, acsDataTemp)
      }
    }
  }
  
  # Remove all columns (variables) with NAs
  #acsData <- acsData[, colSums(!is.na(acsData)) != 0]
  
  # Returns both the dataframe and corresponding labels
  return(list(acsData, labels))
}