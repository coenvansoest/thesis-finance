library(dplyr)
library(tidyr)
library(jsonlite)

# Get a list of JSON files
json_files <- list.files(path = 'data', pattern = "\\.json$", full.names = TRUE)

# Read all JSON files into a list
json_list <- lapply(json_files, fromJSON)

# Convert to a single data frame (if structured correctly)
filter_json_list <- function(json_list, team_filter) {
  lapply(seq_along(json_list), function(i) {  # Use seq_along to track period ID
    sublist <- json_list[[i]]
    
    # Find indices where team_name matches the filter
    team_indices <- which(sublist$team_name == team_filter)
    
    if (length(team_indices) == 0) {
      return(NULL)  # Skip if no matching teams
    }
    
    # Extract only the matching teams and add period_id
    filtered_sublist <- list(
      period_id = i,  # Assign the period ID
      team_name = sublist$team_name[team_indices],
      transfers_in = sublist$`in`[team_indices],  # Use backticks for `in`
      transfers_out = sublist$left[team_indices]  # Rename left as well for clarity
    )
    
    return(filtered_sublist)
  })
}

filtered_json_list <- filter_json_list(json_list, "Ajax Amsterdam")

# Extract all incoming transfers with period_id
incoming_transfers_df <- do.call(rbind, lapply(seq_along(filtered_json_list), function(i) {
  if (!is.null(filtered_json_list[[i]]$transfers_in[[1]])) {
    df <- filtered_json_list[[i]]$transfers_in[[1]]
    df$period_id <- i  # Add the period ID
    return(df)
  }
  return(NULL)
}))



# Extract all outgoing transfers with period_id
outgoing_transfers_df <- do.call(rbind, lapply(seq_along(filtered_json_list), function(i) {
  df <- filtered_json_list[[i]]$transfers_out[[1]]  # Extract the data frame
  
  if (!is.null(df) && nrow(df) > 0) {  # Ensure it's not NULL and has rows
    df$period_id <- i  # Add period ID
    return(df)
  }
  
  return(NULL)  # Return NULL if empty
}))

# Print summaries with period_id
print(head(incoming_transfers_df))
print(head(outgoing_transfers_df))
incoming_transfers_df$transfer_direction<-'Incoming'
outgoing_transfers_df$transfer_direction<-'Outgoing'
ajaxtotal<-rbind(incoming_transfers_df, outgoing_transfers_df)
