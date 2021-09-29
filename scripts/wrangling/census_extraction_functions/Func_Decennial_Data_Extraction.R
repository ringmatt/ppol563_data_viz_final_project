extractDecennialData <- function(group, geo = "county subdivision", year = 2012, states = c(NULL), 
                                counties = c(NULL), geometry = FALSE, specificity = 1, cb = F){
  '
    This function pulls ACS 1-Year data based on a particular group, year, and as many states as desired
    Packages Required - censusapi, tidycensus
    Inputs:
    group     - A 1 letter, 5 digit code representing a unique census data table. 
                See the link in the markdown above for more information.
    geo - A choice of the geography to hone in on
    year      - Year, as an integer, of 2000 or 2010 to extract data from
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
  
  data <- listCensusMetadata(
    name = "dec/sf1",
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
  censusData <- data.frame()
  
  for (i in length(states)){
    for (j in length(counties)){
      
      # Ensures that the group name used for listCensusMetadata works here
      # The issue is with "P8", which is needed for listCensusMetadata, but
      # below we need "P008"
      
      first <- substring(group, 1,1)
      last <- substring(group, 2)
      last <- str_pad(last, 3, side = "left", pad="0")
      
      group_full <- str_c(first, last, "001")
      
      censusDataTemp <- get_decennial(geography = geo, table = group_full, year = year,
                                   state = states[i], county = counties[j], 
                                   geometry = geometry, cb = cb) #, summary_var = paste0(group, "001"
      
      # Adds identifying columns
      censusDataTemp$year <- year
      censusDataTemp$state <- states[i]
      censusDataTemp$county <- counties[j]
      
      # Extracts the county name
      censusDataTemp$county_name <- str_match(censusDataTemp$NAME, ",\\s*(.*?)\\s*,")[,2]
      
      # Extracts the state name
      censusDataTemp$state_name <- trimws(gsub(".*,", "\\1", censusDataTemp$NAME))
      
      # Extracts the sub-county name
      censusDataTemp$NAME <- gsub(",.*", "\\1", censusDataTemp$NAME)
      
      # Merges this new data with the current data frame
      # rbind.fill will fill any missing columns with NAs
      censusData <- rbind.fill(censusData, censusDataTemp)
    }
  }
  
  # Removes state and other names from sub-county names
  censusData$NAME <- gsub(",.*", "\\1", censusData$NAME)
  
  # Removes the "Margin of Error" variable names
  labels <- labels[c(TRUE, FALSE)]
  
  # Changes the column names to readable labels
  map = setNames(labels, unique(censusData$variable))
  censusData$variable <- map[unlist(censusData$variable)]
  
  # Removes variables to the requested level of specificity
  censusData$LayersOfAbstraction <- lengths(regmatches(censusData$variable, gregexpr("_", censusData$variable)))
  censusData <- subset(censusData, LayersOfAbstraction <= specificity)
  censusData <- subset(censusData, select = -c(LayersOfAbstraction))
  
  # Removes labels to the requested level of specificity
  #labels <- labels[lengths(regmatches(labels, gregexpr("_", labels))) <= specificity]
  
  # Returns both the dataframe and corresponding labels
  return(censusData)
}