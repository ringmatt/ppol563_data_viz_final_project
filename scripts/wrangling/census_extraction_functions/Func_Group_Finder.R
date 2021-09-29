
# Determines all relevant groups to your search.
# Once the list is returned, manually go to https://data.census.gov/cedsci/advanced and select the group you want table

groupFinder <- function(metadata, type = "B", subject = "24"){
  '
    This function takes in metadata along with the type and subject of the table you are interested in 
    and returns possible groups.
    metadata - dataframe extracted from `listCensusMetadata`
    type     - Listed as a string B, C, K20, S, R, GCT, DP, NP, CP, or SO201
               Tells one the format of the table
    subject  - A string "01" to "29", plus "98" and "99 which determines what information 
               the table will contain. 
               Only for B, C, K20, S, R, and GCT table types
    '
  
  # Removing the entries with a letter in the 7th position of the names
  # This indicates a subset of the data by race
  metadata <- subset(metadata, grepl("_", substr(name, 7, 7)))
  
  # Taking a specific tables based on type and subject
  # Find type and subject information here: https://www.census.gov/programs-surveys/acs/guidance/which-data-tool/table-ids-explained.html 
  metadata <- subset(metadata, grepl(paste0(type, subject), name))
  
  # Extracts the groups remaining
  groups <- unique(metadata$group)
  
  # Returns the list of groups
  return(groups)
}