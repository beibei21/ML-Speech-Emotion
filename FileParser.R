library(optimbase)
library(stringr)
library(jtools)

# Load the dataset to append Columns to
# optionally receive the number of persons to run, else run all
# all 24 actors
# useful for debugging
getWaveData <- function(person = 24, waveFunToCall = waveToNum, slice = 10){
  if (person <= 0 || person > 24) {
    print("Error, person amount must be in range: 1-24")
    return(NULL) # nothing!
  }
  
  allFolders <- list.dirs(path = "raw data")
  
  # initialize values matches the length when adding rows
  fileData <- data.frame(1,2,3,4,5) # Our Template, for file name data
  #waveData <- data.frame(1,2,3,3,4,5,6,7,8,9,10) # Our Template, for wave data
  # We need to originally create a default dataframe
  # But this is garbage, for now we don't know an easier way
  #waveData <- data.frame(c(1,2,3,4,5,6,7,8,9,10))
  waveData <- waveFunToCall("Actor_01", "03-01-01-01-01-01-01.wav", slice = slice)
  # works better if column names match when adding rows
  colnames(fileData) <- c("X3", "X4", "X5", "X6", "X7") # more templates
  # For debugging.... I think. UPDATE BENJAMIN
  toBind <- NULL
  # For folder in folders and file in folder
  # Loop through all folders except the first one (which is "raw data")
  # Subtract it out
  # Create two data frames: 1 for the wave data, 1 for the file name data
  for (folder in allFolders[-1]) {
    print(basename(folder))
    breakSoon <- FALSE
    for (file in list.files(path = folder, pattern = "*.wav")) {
      
      
      print(file)
      ### Process and Add data to file name dataframe
      data <- as.character(basename(file))
      # splits on every - or .
      # \\ to escape the backslash
      # We need to get either the stuff sep by - or .
      # | means or
      # We have integers in base 10. Let's specify.
      # Convert the strings to integers
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
      # Actually, if the data is just 0 or 1, write.csv() might
      # Save it as Inf.... so dumb. So save it as a string. gscale() will
      # Still convert it from -1 to 1.
      dataVector[7] <- 
        case_when(dataVector[7] %% 2 == 1 ~ "male",
                  TRUE ~ "female"
                  )# 0 or 1 differentiation
      
      # drops the first two columns and last column and adds the row to the data frame
      # chop off what we don't want
      # row bind to the dataFrame
      fileData <- rbind(fileData, data.frame(transpose(matrix(unlist(dataVector))))[-1][-1][-6])
      
      ### Process and Add data to wave dataframe
      ## This binds all of the waveFunToCall processing for a file to the old waveData data frame
      ## Which we will merge with the fileData after looping
      ## A slow function that works for now. 
      ## Probably more effective to brute force find out what the least number of rows is
      ## But this will help speed up debugging
      toBind <- waveFunToCall(basename(folder), file, slice = slice)
      common_cols <- intersect(colnames(waveData), colnames(toBind))
      # Now bind to the waveData the toBind data only those columns that are common
      waveData <- rbind(waveData[,common_cols], toBind[,common_cols])
      
      # Append to the file data what we just row binded to the file data
      # nrows of toBind times
      # We need duplicates!
      print(fileData[nrow(fileData), ])
      # print(data.frame(sapply(1:nrow(toBind), FUN = function(i) {
      #   fileData[nrow(fileData), ]})))
      sapply(1:nrow(toBind), FUN = function(dummyVar) {
          rbind(fileData, fileData[nrow(fileData), ]) # duplicate the last recorded file data nrow(toBind) times 
        }
      )
      #Debugging
      #if (breakSoon) break else breakSoon <- TRUE
      #View(waveData)
    }
    #View(waveData)
    # A control structure to make sure we only loop through 8 people (8 loops)
    if (person == 1) {
      break # we're done with 8 actors
    }
    else {
      person <- person - 1 # Decrement the person down to 1
    }
    break # REMOVE WHEN WORKS
  }
  # put meaningfull names on the columns
  colnames(fileData) <- c("emotion", "intensity", "statement", "repetition" , "gender")
  
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
  #View(combinedDataSet)
  return(combinedDataSet)
}


