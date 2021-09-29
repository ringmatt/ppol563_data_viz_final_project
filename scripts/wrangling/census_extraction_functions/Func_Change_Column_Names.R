changeColNames <- function(data, labels){
  '
    Alters the column names of an ACS data table as outputted by the function `extract1YearACSData`
    data   - The data frame of state, year, and identifier variables
    labels - Strings which will replace the codes currently labeling the variables
    '
  # Sets the list of column names
  variables <- colnames(data)
  
  # Removes the state, county, and year columna so they will not be changed
  variables <- variables[!variables %in% c("state", "county", "year")]
  
  # Loops through each label via indices
  for (i in 1:length(labels)){
    
    # Changes the column name, replacing it with the corresponding label
    names(data)[names(data)==variables[i]] <- labels[i]
    
    # Fills all spaces with an underscore, so that they are simpler and callable
    colnames(data) <- gsub(" ", "_", colnames(data))
    
    # Removes all commas, so that columns are callable
    colnames(data) <- gsub(",", "", colnames(data))
  }
  
  # Returns the new data
  return(data)
}