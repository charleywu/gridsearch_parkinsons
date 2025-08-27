
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Get the list of all .txt files in the directory
txt_files <- list.files(path = "JSON", pattern = "\\.txt$", full.names = TRUE)

# Loop through each file and rename it
for (file in txt_files) {
  # Create the new file name by replacing .txt with .JSON
  new_file <- sub("\\.txt$", ".JSON", file)
  
  # Rename the file
  file.rename(file, new_file)
}
