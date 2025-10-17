library(jsonlite)
# List all the JSON files in the folder
# json_files_1st_batch <- list.files(path = "data/raw_data/1st batch", pattern = "*.JSON", full.names = TRUE)
# json_files_2nd_batch <- list.files(path = "data/raw_data/2nd batch", pattern = "*.JSON", full.names = TRUE)
# json_files_3rd_batch <- list.files(path = "data/raw_data/3rd batch", pattern = "*.JSON", full.names = TRUE)
# json_files_4th_batch <- list.files(path = "data/raw_data/4th batch", pattern = "*.JSON", full.names = TRUE)
# json_files <- c(json_files_1st_batch, json_files_2nd_batch,json_files_3rd_batch, json_files_4th_batch)


json_files <- list.files(path = "data/raw_data/all", pattern = "(?i)\\.json$", full.names = F )
# length(json_files)

# Create an empty list to hold the content of all JSON files
all_json_content <- list()

for (file in json_files) {
  json_content <- fromJSON(file.path("data/raw_data/all", file))
  all_json_content <- c(all_json_content, list(json_content))
}

# Write the combined content to a single JSON file as an array
write_json(all_json_content, "data/all_data_gridsearch_parkinson.json", pretty = TRUE)


# extract number of participants
# all_ids <- sapply(all_json_content, function(x) x$participantId)
# length(unique(all_ids))


