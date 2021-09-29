extract5YearACSData <- function(group, geo = "county subdivision", year = 2012, states = c(NULL), 
                                counties = c(NULL), geometry = FALSE, specificity = 1, cb = F){
  '
    This function pulls ACS 1-Year data based on a particular group, year, and as many states as desired
    Packages Required - 
    Inputs:
    group     - A 1 letter, 5 digit code representing a unique census data table. 
                See the link in the markdown above for more information.
    geo - A choice of the geography to hone in on
    year      - Year, as an integer, between 2005 and 2019 to extract data from
    states    - States, as a vector of strings, from 01-56 Excluding 3, 7, 14, 43, 52
    counties  - List of vectors, where each vector is named for a given state
    geometry  - Useful for plotting on choropleths. Returns an sf tibble instead of a regular tibble
    specificity - The higher this value, the more specific the variables allowed in the final dataset
    cb  - Stands for "cartographic boundaries". When false, this will use the TIGER/Line files. When true, will use the tigris package defaults. 
          MUST BE SET TO FALSE FOR TRACT DATA TO WORK.
    
    Outputs: The outputs are returned in a list
    acsData   - A dataframe where each row is a state and each column is a variable
    labels    - The full description of each variable
    '
  
  # Pulls a list of variables for the chosen group and year
  data <- listCensusMetadata(
    name = "acs/acs5",
    vintage = year,
    group = group)
  
  # Removes all annotations, which cuts the size in half
  data <- data[which(data$predicateType != 'string'),]
  
  # Removes `Total!!` from all labels and changes all `!!` to `_`
  data$label <- gsub("!!", "_", data$label)
  #data$label <- gsub("_Total", "", data$label)
  # These are here to deal with new labeling in 2019 data
  data$label <- gsub(":", "", data$label)
  
  
  # Sorts the data by name
  data <- data[order(data$name),]
  
  # Saves both the names and labels in the data
  labels <- unique(data$label)
  
  # Initializes the final dataset
  acsData <- data.frame()
  
  for (i in length(states)){
    for (j in length(counties)){
      acsDataTemp <- get_acs(geography = geo, table = group, year = year,
                        state = states[i], county = counties[j], 
                        geometry = geometry, summary_var = paste0(group, "_001"), cb = cb)
      
      # Adds identifying columns
      acsDataTemp$year <- year
      acsDataTemp$state <- states[i]
      acsDataTemp$county <- counties[j]
      
      # Extracts the county name
      acsDataTemp$county_name <- str_match(acsDataTemp$NAME, ",\\s*(.*?)\\s*,")[,2]
      
      # Extracts the state name
      acsDataTemp$state_name <- trimws(gsub(".*,", "\\1", acsDataTemp$NAME))
      
      # Extracts the sub-county name
      acsDataTemp$NAME <- gsub(",.*", "\\1", acsDataTemp$NAME)
      
      # Merges this new data with the current data frame
      # rbind.fill will fill any missing columns with NAs
      acsData <- rbind.fill(acsData, acsDataTemp)
    }
  }
  
  # Removes state and other names from sub-county names
  acsData$NAME <- gsub(",.*", "\\1", acsData$NAME)
  
  # Removes the "Margin of Error" variable names
  labels <- labels[c(TRUE, FALSE)]
  
  # Changes the column names to readable labels
  map = setNames(labels, unique(acsData$variable))
  acsData$variable <- map[unlist(acsData$variable)]
  
  # Removes variables to the requested level of specificity
  acsData$LayersOfAbstraction <- lengths(regmatches(acsData$variable, gregexpr("_", acsData$variable)))
  acsData <- subset(acsData, LayersOfAbstraction <= specificity)
  acsData <- subset(acsData, select = -c(LayersOfAbstraction))
  
  # Removes labels to the requested level of specificity
  #labels <- labels[lengths(regmatches(labels, gregexpr("_", labels))) <= specificity]
  
  # Returns both the dataframe and corresponding labels
  return(acsData)
}