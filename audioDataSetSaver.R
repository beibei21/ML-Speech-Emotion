### A set of scripts to run the pre-processing data preparation
### and then save it to a file so we only need to run the
### preparation once

# Run the two files needed for saving the dataset
source("waveToNum.R")
source("FileParser.R")

# Prepare the data in this one line
# 1 person for now
prepareData <- function(fileName = "emotion.csv", person = 24, call1 = TRUE, slice = 10) {
  ## Define a lambda to pass to the getWave function
  if (!call1) {
    waveFunToCall = waveToNumWithSlicesAdded
  } else {
    waveFunToCall = waveToNum
  }
  myData <- getWaveData(person, waveFunToCall, slice = slice) # ta da
  # Now save the whole data frame to a file: emotion.csv
  # col.names = TRUE is set by default. We want the colnames but not the row names
  # The row names will just be 1,2,3,4,... useless.
  write.csv(myData, fileName, na = "NA", row.names = FALSE) # give it the dataframe
  myData
}

getData <- function(fileName = "emotion.csv") {
  read.csv(fileName)
}

