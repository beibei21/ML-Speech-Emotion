library(optimbase)
library(stringr)

# Load the dataset to append Columns to
#complete_speech_dataset <- read.csv(file = "")

getWaveData <- function(){
allFolders <- list.dirs(path = "raw data")

# initialize values matches the length when adding rows
fileData <- data.frame(2, 3, 4, 5, 6) # Our Template, for file name data
#waveData <- data.frame(1,2,3,3,4,5,6,7,8,9,10) # Our Template, for wave data
waveData <- waveToNum("raw data/Actor_01/03-01-01-01-01-01-01.wav")

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
    # \\ to escape the backslash
    # We need to get either the stuff sep by - or .
    # | means or
    dataVector <- sapply(strsplit(data, regex("\\-|\\.")), FUN = function(str) strtoi(str, base=10))
    #print(dataVector1)
    #dataVector <- strsplit(data, regex("\\-|\\."))
    
    # drops the first two columns and last column and adds the row to the data frame
    # chop off what we don't want
    # row bind to the dataFrame
    fileData <- rbind(fileData, data.frame(transpose(matrix(unlist(dataVector))))[-1][-1][-6])
    
    ### Process and Add data to wave dataframe
    waveData <- rbind(waveData, waveToNum(file))
    #View(waveData)
  }
  break
}
# put meaningfull names on the columns
colnames(fileData) <- c("Emotion", "intensity", "statement", "repetition" , "actor")
# drops the first row of garbage values (our template integers)
fileData <- fileData[-1,]
waveData <- waveData[-1,]
#fileData <- as.data.frame.integer(fileData)
fileData <- data.frame(c(fileData, waveData))
# replae second frame with waveData
#fileData <- data.frame(scale(as.matrix(c(fileData, waveData))))
fileData <- gscale(fileData, scale.only = TRUE, vars = colnames(fileData)[-1][-4])
View(fileData)
return(fileData)
}


getWaveData()


