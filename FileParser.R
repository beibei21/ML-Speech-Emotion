library(optimbase)
library(stringr)

# Load the dataset to append Columns to
#complete_speech_dataset <- read.csv(file = "")

allFolders <- list.dirs(path = "raw data")

# initialize values matches the length when adding rows
fileData <- data.frame(2, 3, 4, 5, 6) # Our Template, for file name data
waveData <- data.frame(2,3,3,4,5) # Our Template, for wave data


# works better if column names match when adding rows
colnames(fileData) <- c("X3", "X4", "X5", "X6", "X7") # more templates

# For folder in folders and file in folder
# Loop through all folders except the first one (which is "raw data")
# Subtract it out
# Create two data frames: 1 for the wave data, 1 for the file name data
for (folder in allFolders[-1]) {
  print(basename(folder))
  for (file in list.files(path = folder, pattern = "*.wav")) {
    ### Process and Add data to file name dataframe
    data <- as.character(basename(file))
    # splits on every - or .
    dataVector <- (strsplit(data, regex("\\-|\\.")))
    # drops the first two columns and last column and adds the row to the data frame
    # chop off what we don't want
    # row bind to the dataFrame
    fileData <- rbind(fileData, data.frame(transpose(matrix(unlist(dataVector))))[-1][-1][-6])
    
    ### Process and Add data to wave dataframe
    }
}
# put meaningfull names on the columns
colnames(fileData) <- c("Emotion", "intensity", "statement", "repetition" , "actor")
# drops the first row of garbage values (our template integers)
fileData <- fileData[-1,]

# Delete this line


# replae second frame with waveData
fileData <- data.frame(c(fileData, waveData))

View(fileData)




