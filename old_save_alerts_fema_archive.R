library(tidyverse)
library(lubridate)
library(jsonlite)
library(httr)
library(plyr)
library(xml2)
library(dplyr)

# Import latest counties with deaths list from cbs desk
counties_with_deaths <- read_csv("counties_with_deaths.csv")

# Download this json file fresh from FEMA; archived ipaws alerts; capturing all since Sept. 23
download.file("https://www.fema.gov/api/open/v1/IpawsArchivedAlerts?$filter=sent%20gt%20'2024-09-23T12:00:00.000z'", "alerts.json")

# Read in the json file alerts
alerts <- fromJSON("alerts.json")

# Flatten the JSON structure to a dataframe; remaining nested data issues
alerts_flat <- flatten(alerts$IpawsArchivedAlerts)

# Query all fields in alerts_flat to see if any county in counties list is contained within originalMessage
alerts_flat$county <- sapply(alerts_flat$originalMessage, function(x) {
  if (is.null(x)) {
    return(NA)
  }
  for (county in counties_with_deaths$county) {
    if (grepl(county, x, ignore.case = TRUE)) {
      return(county)
    }
  }
  return(NA)
})
# Add the state from the counties file if county matches
alerts_flat$state <- sapply(alerts_flat$county, function(x) {
  if (is.na(x)) {
    return(NA)
  }
  return(counties_with_deaths$state[counties_with_deaths$county == x])
})
# leave blank if NA
alerts_flat$county[is.na(alerts_flat$county)] <- ""
alerts_flat$state[is.na(alerts_flat$state)] <- ""




# Add a new field called rowid to the dataframe to use as an id
alerts_flat$rowid <- seq.int(nrow(alerts_flat))

# Initialize an empty list to store the dataframes
info_list <- list()

# Loop through each record in the 'info' column and append to the list
for (i in seq_along(alerts_flat$info)) {
  if (!is.null(alerts_flat$info[[i]])) {
    # Convert to dataframe and add an 'id' column
    info_df <- as.data.frame(alerts_flat$info[[i]])
    info_df$rowid <- alerts_flat$rowid[i]
    info_list[[i]] <- info_df
  }
}

# Combine all the dataframes in the list into a single dataframe
alerts_archive_df <- bind_rows(info_list)

# Using rowid, keep only the first message in each set of duplicate rowids
alerts_archive_df <- alerts_archive_df %>% distinct(rowid, .keep_all = TRUE)

# Merge the alerts_archive_df with the alerts_flat dataframe using rowid to join
alerts_archive <- left_join(alerts_flat, alerts_archive_df, by = "rowid")

# Initialize an empty list to store the dataframes
area_list <- list()
# Loop through each record in the 'info' column and append to the list
for (i in seq_along(alerts_archive_df$area)) {
  if (!is.null(alerts_archive_df$area[[i]])) {
    # Convert to dataframe and add an 'id' column
    area_df <- as.data.frame(alerts_archive_df$area[[i]])
    area_df$rowid <- alerts_archive$rowid[i]
    area_list[[i]] <- area_df
  }
}
# Combine all the dataframes in the list into a single dataframe
alerts_area_df <- bind_rows(area_list)
# Using rowid, keep only the first message in each set of duplicate rowids
alerts_area_df <- alerts_area_df %>% distinct(rowid, .keep_all = TRUE)

# Merge the alerts_archive_df with the alerts_flat dataframe using rowid to join
alerts_archive <- left_join(alerts_archive, alerts_area_df %>% select(2,3), by = "rowid")

# filter out all records where the event includes the word "Test" and ignore case
alerts_archive <- alerts_archive %>% filter(!grepl("Test", event, ignore.case = TRUE))


# Create a slimmed down dataset for sharing and reviewing by selecting sender, sent, headline, event, description, instruction, and areaDesc
alerts_archive_slim <- alerts_archive %>% select(senderName, areaDesc, sent, headline, event, category, description, 
                                                 instruction,msgType,urgency,severity,
                                                 scope,sender,cogId,id)

# use lubridate to convert the time field to a date time object
alerts_archive_slim$sent <- ymd_hms(alerts_archive_slim$sent)
# convert the time from utc to edt
alerts_archive_slim$sent <- with_tz(alerts_archive_slim$sent, tzone = "America/New_York")
# create a new date field that only contains the date without the time at all
alerts_archive_slim$date <- as.Date(alerts_archive_slim$sent)
# create a new time field that just captures everything after the space in the sent field
alerts_archive_slim$time <- format(alerts_archive_slim$sent, format = "%H:%M:%S")
# drop the send field
alerts_archive_slim$sent <- NULL
# move date and time to the first two columns
alerts_archive_slim <- alerts_archive_slim %>% select(date, time, everything())

# export the slimmed down dataset to a csv file
write_csv(alerts_archive_slim, "alerts_archive.csv")




library(dplyr)
library(xml2)

# Extract the originalMessage field
messages <- alerts_flat %>% pull(originalMessage)

# Function to convert XML tags to headings
convert_xml_to_headings <- function(xml_content) {
  # Parse the XML content
  xml <- read_xml(xml_content)
  
  # Convert XML tags to headings
  headings <- xml_find_all(xml, "//*")
  for (heading in headings) {
    tag <- xml_name(heading)
    content <- xml_text(heading)
    xml_text(heading) <- paste0("\n#", tag, "\n", content)
  }
  
  # Return the modified content as text
  as.character(xml)
}

# Apply the function to each message
modified_messages <- sapply(messages, convert_xml_to_headings)

# Write the modified content to a text document
writeLines(modified_messages, "output.txt")







