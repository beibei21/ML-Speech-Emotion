library(optimbase)
library(stringr)
library(jtools)

# Load the dataset to append Columns to
#complete_speech_dataset <- read.csv(file = "")

getWaveData <- function(){
allFolders <- list.dirs(path = "raw data")

# initialize values matches the length when adding rows
fileData <- data.frame(2, 3, 4, 5, 6) # Our Template, for file name data
#waveData <- data.frame(1,2,3,3,4,5,6,7,8,9,10) # Our Template, for wave data
# We need to originally create a default dataframe
# But this 
waveData <- waveToNum("Actor_01", "03-01-01-01-01-01-01.wav")

# works better if column names match when adding rows
colnames(fileData) <- c("X3", "X4", "X5", "X6", "X7") # more templates
person <- 8
# For folder in folders and file in folder
# Loop through all folders except the first one (which is "raw data")
# Subtract it out
# Create two data frames: 1 for the wave data, 1 for the file name data
stopNow <- FALSE
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
    
    # Turn the actor into a gender (male/female)
    # Odd Number  - Male
    # Even Number - Female
    # The actor is second to last in the vector
    # Actually, instead of returning a the result of a 
    # case_when(dataVector[7] %% 2 == 1 ~ 1, TRUE ~ 0)
    # we can just mod by 2. The result, whether 1 or 0, will differentiate the gender
    # for us, based on the rules above
    dataVector[7] <- dataVector[7] %% 2 # 0 or 1 differentiation
    
    # drops the first two columns and last column and adds the row to the data frame
    # chop off what we don't want
    # row bind to the dataFrame
    fileData <- rbind(fileData, data.frame(transpose(matrix(unlist(dataVector))))[-1][-1][-6])
    
    ### Process and Add data to wave dataframe
    ## This binds all of the waveToNum processing for a file to the old waveData data frame
    ## Which we will merge with the fileData after looping
    waveData <- rbind(waveData, waveToNum(basename(folder), file))
    #View(waveData)
  }
  # A control structure to make sure we only loop through 8 people (8 loops)
  if (person == 1) {
    break # we're done with 8 actors
  }
  else {
    person <- person - 1 # Decrement the person down to 1
  }
}
# put meaningfull names on the columns
colnames(fileData) <- c("Emotion", "intensity", "statement", "repetition" , "gender")
# drops the first row of garbage values (our template integers)
fileData <- fileData[-1,]
waveData <- waveData[-1,]
#fileData <- as.data.frame.integer(fileData)
combinedDataSet <- data.frame(c(fileData, waveData))
# replae second frame with waveData
#fileData <- data.frame(scale(as.matrix(c(fileData, waveData))))
# Don't scale the emotion (column 1)
# Scale.only = true means scale between -1 to 1
combinedDataSet <- gscale(combinedDataSet, scale.only = TRUE, vars = colnames(combinedDataSet)[-1])
View(combinedDataSet)
return(combinedDataSet)
}


