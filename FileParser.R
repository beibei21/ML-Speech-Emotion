#ToDo File Parsing Benjamin
library(readtext)

# Load the dataset to append Columns to
complete_speech_dataset <- read.csv(file = "")

#Use "r" for reading in text mode
DATA_DIR <- system.file("extdata/", package = "readtext")

allFolders <- list.dirs(path = "raw data")

# For folder in folders and file in folder
for (folder in allFolders) {
  for (file in list.files(path = folder, pattern = "*.wav")) {
    # Do stuff to that file character vector!
    # Open as you please...
  }
}

