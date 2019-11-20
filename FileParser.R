library(optimbase)
library(stringr)

# Load the dataset to append Columns to
#complete_speech_dataset <- read.csv(file = "")

allFolders <- list.dirs(path = "raw data")

# initialize values matches the length when adding rows
dataFrame <- data.frame(2, 3, 4, 5, 6) # Our Template

# works better if column names match when adding rows
colnames(dataFrame) <- c("X3", "X4", "X5", "X6", "X7")
# For folder in folders and file in folder
for (folder in allFolders[-1]) {
  print(basename(folder))
  for (file in list.files(path = folder, pattern = "*.wav")) {
    # Do stuff to that file character vector!
    # Open as you please...
    data <- as.character(basename(file))
    # splits on every - or .
    dataVector <- (strsplit(data, regex("\\-|\\."))) 
    # drops the first two columns and last column and adds the row to the data frame
    dataFrame <- rbind(dataFrame, data.frame(transpose(matrix(unlist(dataVector))))[-1][-1][-6]) 
  }
}
# put meaningfull names on the columns
colnames(dataFrame) <- c("Emotion", "intensity", "statement", "repetition" , "actor")
# drops the first row of garbage values (our template integers)
dataFrame <- dataFrame[-1,]
View(dataFrame)