### A set of scripts to run the pre-processing data preparation
### and then save it to a file so we only need to run the
### preparation once

# Run the two files needed for saving the dataset
source("waveToNum.R")
source("FileParser.R")

# Prepare the data in this one line
# 1 person for now
myData <- getWaveData(person = 1) # ta da

# Now save the whole data frame to a file: emotion.csv
# col.names = TRUE is set by default. We want the colnames but not the row names
# The row names will just be 1,2,3,4,... useless.
write.csv(myData, "emotion.csv", na = "NA", row.names = FALSE) # give it the dataframe

data <- read.csv("emotion.csv")

