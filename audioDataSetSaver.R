### A set of scripts to run the pre-processing data preparation
### and then save it to a file so we only need to run the
### preparation once

# Run the two files needed for saving the dataset
source("waveToNum.R")
source("FileParser.R")

# Prepare the data in this one line
# 1 person for now
prepareData <- function(people = NULL, actor = 1, actorEnd = 24, filename = "emotion.csv", call1 = TRUE, saveIncrementally = FALSE, slice = 50) {
  ## Define a lambda to pass to the getWave function
  if (!call1) {
    waveFunToCall = waveToNumWithSlicesAdded
  } else {
    waveFunToCall = waveToNum
  }
  
  if (!saveIncrementally) {
    myData <- getWaveData(people = people, actor = actor, actorEnd = actorEnd, saveIncrementally = saveIncrementally, waveFunToCall = waveFunToCall, slice = slice) # ta da
    
    # Now save the whole data frame to a file: emotion.csv
    # col.names = TRUE is set by default. We want the colnames but not the row names
    # The row names will just be 1,2,3,4,... useless.
    write.csv(x = myData, file = filename, na = "NA", row.names = FALSE) # give it the dataframe
    return(myData)  
  }
  else {
    if(getWaveData(people = people, actor = actor, actorEnd = actorEnd, saveIncrementally = saveIncrementally, waveFunToCall = waveFunToCall, slice = slice)) { # ta da
      print("Successfully saved separately")  
      # Now save the whole data frame to a file: emotion.csv
      # col.names = TRUE is set by default. We want the colnames but not the row names
      # The row names will just be 1,2,3,4,... useless.
      write.csv(x = myData, file = filename, na = "NA", row.names = FALSE) # give it the dataframe
      return(TRUE)
    }
    else {
      print("Failed to save incrementally")
      return(FALSE)
    }
  }
}

getData <- function(filename = "emotion_extended.csv") {
  read.csv(filename)
}

# IF people is NULL, combine all possible actor datasets
combineSaveDatasets <- function(filename = "emotion_extended.csv", people = NULL) {
  # Check if we should combine all
  if (is.null(people)) {
    # Give the pattern regex any characters leading up to
    # the last two digits (0-9 and 1-9) with the .csv extension
    files <- list.files(pattern = "*_[0-9][1-9].csv") # underscore so we avoid any other file other than _(actorID).csv
    # We need rows to bind to. Process the first file
    fullDatasetBuild <- getData(files[1]) # for our template
    print(paste("Started processing", files[1], "dataset to fullDataset"))
    
    for(file in files[-1]) # we already processed the first file
    {
      # Declare a toBind dataset to get the intersection of
      toBind <- getData(file)
      # Get the columns that they agree on
      common_cols <- intersect(colnames(fullDatasetBuild), colnames(toBind))
      print("Determined Col Intersection")
      # Now bind to the fullDatasetBuild the toBind data only those columns that are common
      fullDatasetBuild <- rbind(fullDatasetBuild[,common_cols], toBind[,common_cols])
      print(paste("Bounded", file, "dataset to fullDataset"))
    }
  }
  else { # process each actor from the people vector
    # We need rows to bind to. Process the first file
    # Concat the "emotion_Actor_*" string with the actor ID to get the correct
    # folder. Get the first file in that folder matching the regular expression:
    # Can't just paste "emotion_Actor_0" with the actor ID. The ID could be greater than 10.
    # So if its less than 10, insert a "0", else don't
    if (people[1] < 10) {
      # Set the control to "0", we need this before the actor id
      pasteControl <- "0" 
    }
    else {
      # Set the control to empty, we don't need any string-number prefix before the actor id
      pasteControl <- ""
    }
    # We are banking on are assumption that every first file every actors raw folder directory
    # starts with 03-01-01-01-01-01-*.wav
    # where the star is the actor identification
    file <- paste("emotion_Actor_",
                      pasteControl, # Insert control of "0" or empty ""
                      people[1],
                      ".csv", sep="")
    fullDatasetBuild <- getData(file) # for our template
    print(paste("Started processing", file, "dataset to fullDataset"))
    for (actor in people[-1]) { # subtract the first actor out. We already processed its data
      
      # Concat the "emotion_Actor_*" string with the actor ID to get the correct
      # folder. Get the first file in that folder matching the regular expression:
      # Can't just paste "emotion_Actor_0" with the actor ID. The ID could be greater than 10.
      # So if its less than 10, insert a "0", else don't
      if (actor < 10) {
        # Set the control to "0", we need this before the actor id
        pasteControl <- "0" 
      }
      else {
        # Set the control to empty, we don't need any string-number prefix before the actor id
        pasteControl <- ""
      }
      # We are banking on are assumption that every first file every actors raw folder directory
      # starts with 03-01-01-01-01-01-*.wav
      # where the star is the actor identification
      file <- paste("emotion_Actor_",
                        pasteControl, # Insert control of "0" or empty ""
                        actor,
                        ".csv", sep="")
      # Declare a toBind dataset to get the intersection of
      toBind <- getData(file)
      # Get the columns that they agree on
      common_cols <- intersect(colnames(fullDatasetBuild), colnames(toBind))
      print("Determined Col Intersection")
      # Now bind to the fullDatasetBuild the toBind data only those columns that are common
      fullDatasetBuild <- rbind(fullDatasetBuild[,common_cols], toBind[,common_cols])
      print(paste("Bounded", file, "dataset to fullDataset"))
    }
  }
  write.csv(x=fullDatasetBuild, file=filename, na="NA", row.names=FALSE)
  fullDatasetBuild
}

# Receive a actor to save as the extension
saveFile <- function(data, actor) {
  # Concat the "Actor_*" string with the actor ID to get the correct
  # folder. Get the first file in that folder matching the regular expression:
  # Can't just paste "Actor_0" with the actor ID. The ID could be greater than 10.
  # So if its less than 10, insert a "0", else don't
  if (actor < 10) {
    # Set the control to "0", we need this before the actor id
    pasteControl <- "0" 
  }
  else {
    # Set the control to empty, we don't need any string-number prefix before the actor id
    pasteControl <- ""
  }
  filename <- paste("emotion_Actor_",
                    pasteControl, # Insert control of "0" or empty ""
                    actor,
                    ".csv", sep="")
  # Now save the a portion of the whole dataset to a file follwing the format:
  # emotion_Actor_*.csv where the * is the actor id
  # col.names = TRUE is set by default. We want the colnames but not the row names
  # The row names will just be 1,2,3,4,... useless.
  write.csv(x = data, file = filename, na = "NA", row.names = FALSE)# give it the dataframe
  TRUE # it worked
}

joinFileDatasets <- function(files = NULL, filetowrite, pattern = NULL) {
  # Favor the pattern default variable
  if (!is.null(pattern)) {
    print("Given pattern")
    files <- list.files(pattern = pattern)
    # We need rows to bind to. Process the first file
    # We need rows to bind to. Process the first file
    fullDatasetBuild <- getData(files[1]) # for our template
    print(paste("Started processing", files[1], "dataset to fullDataset"))
    for(file in files[-1]) # we already processed the first file
    {
      # Declare a toBind dataset to get the intersection of
      toBind <- getData(file)
      # Get the columns that they agree on
      common_cols <- intersect(colnames(fullDatasetBuild), colnames(toBind))
      print("Determined Col Intersection")
      # Now bind to the fullDatasetBuild the toBind data only those columns that are common
      fullDatasetBuild <- rbind(fullDatasetBuild[,common_cols], toBind[,common_cols])
      print(paste("Bounded", file, "dataset to fullDataset"))
    }
  }
  else if (!is.null(files)) {
    print("Given files")
    # We need rows to bind to. Process the first file
    # We need rows to bind to. Process the first file
    fullDatasetBuild <- getData(files[1]) # for our template
    print(paste("Started processing", files[1], "dataset to fullDataset"))
    for(file in files[-1]) # we already processed the first file
    {
      # Declare a toBind dataset to get the intersection of
      toBind <- getData(file)
      # Get the columns that they agree on
      common_cols <- intersect(colnames(fullDatasetBuild), colnames(toBind))
      print("Determined Col Intersection")
      # Now bind to the fullDatasetBuild the toBind data only those columns that are common
      fullDatasetBuild <- rbind(fullDatasetBuild[,common_cols], toBind[,common_cols])
      print(paste("Bounded", file, "dataset to fullDataset"))
    }
  }
  else {
    files <- list.files(pattern = "emotion_Actor_(.*).csv")
    print("Given no files or pattern")
    # We need rows to bind to. Process the first file
    # We need rows to bind to. Process the first file
    fullDatasetBuild <- getData(files[1]) # for our template
    print(paste("Started processing", files[1], "dataset to fullDataset"))
    for(file in files[-1]) # we already processed the first file
    {
      # Declare a toBind dataset to get the intersection of
      toBind <- getData(file)
      # Get the columns that they agree on
      common_cols <- intersect(colnames(fullDatasetBuild), colnames(toBind))
      print("Determined Col Intersection")
      # Now bind to the fullDatasetBuild the toBind data only those columns that are common
      fullDatasetBuild <- rbind(fullDatasetBuild[,common_cols], toBind[,common_cols])
      print(paste("Bounded", file, "dataset to fullDataset"))
    }
  }
  write.csv(x=fullDatasetBuild, file=filetowrite, na="NA", row.names=FALSE)
  fullDatasetBuild
}

numericallyEncode <- function(data) {
  # get a column list from 1 to the number of cols
  cols <- 1:ncol(data)
  # For every column, see if it has numeric data and scale it
  for (col in cols) {
    # See if the data in this column is numeric
    if (!is.numeric(data[,col])) {# if not, scale
      print(paste("Scaling column: ", col))
      # vars = colnames() is the columns to scale. We only should scale this column.
      data[,col] <- gscale(data = data, scale.only = TRUE, vars = colnames(data[,col])) # only scale this one column
    }
  }
  data # return the data
}
  
  
  
  
  

