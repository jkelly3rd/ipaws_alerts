library(tidyverse)
library(lubridate)
library(jsonlite)
library(httr)
library(plyr)

# Download this json file fresh from FEMA
# This is only capturing the alerts since Sept. 23 (because of 1,000 record limit)
download.file("https://www.fema.gov/api/open/v1/IpawsArchivedAlerts?$filter=sent%20gt%20'2024-09-23T00:00:00.000z'", "alerts.json")

# Read in the json file alerts and convert to a dataframe
alerts <- fromJSON("alerts.json")

# Flatten the JSON structure
alerts_flat <- flatten(alerts$IpawsArchivedAlerts)

# Initialize an empty list to store the dataframes
info_list <- list()

# Loop through each record in the 'info' column and append to the list
for (i in seq_along(alerts_flat$info)) {
  if (!is.null(alerts_flat$info[[i]])) {
    # Convert to dataframe and add an 'id' column
    info_df <- as.data.frame(alerts_flat$info[[i]])
    info_df$id <- i
    info_list[[i]] <- info_df
  }
}

# Combine all the dataframes in the list into a single dataframe
alerts_archive_df <- bind_rows(info_list)