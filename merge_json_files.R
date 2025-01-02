library(jsonlite)
# List all the JSON files in the folder
json_files <- list.files(path = "raw_data/json", pattern = "*.JSON", full.names = TRUE)

# Create an empty list to hold the content of all JSON files
all_json_content <- list()

# Loop over each file, read the content, and append it to the list
for (file in json_files) {
  json_content <- fromJSON(file)
  all_json_content <- append(all_json_content, list(json_content))
}

# Write the combined content to a single JSON file as an array
write_json(all_json_content, "data/all_data_gridsearch_parkinson.json", pretty = TRUE)

