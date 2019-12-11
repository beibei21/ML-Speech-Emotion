library(optimbase)
library(stringr)
library(jtools)
# We only include one or the other, not both! 
# If we source() a file that source() - d us, then that recursively
# sources both files and the stack will overflow!

# Load the dataset to append Columns to
# optionally receive the number of persons to run, else run all
# all 24 actors
# useful for debugging
# Actually! We will have a default variable called actorEnd which
# will mark which actor the user wants us to end on.
# actor is the actor to begin preparing the data on.
# saveIncrementally handles whether or not we should save the data for an actor to 
# a unique file for that actor each time we loop through an actor's folder or 
# only save in the audioDataSetSaver
getWaveData <- function(people = NULL, actor = 1, actorEnd = 24, saveIncrementally = FALSE, waveFunToCall = waveToNum, slice = 50){
  # Make sure numActors to run was not greater than our limit
  if (actorEnd <= 0 || actorEnd > 24) {
    print("Error, number of actors must be in range: 1-24")
    return(NULL) # nothing!
  }
  
  ### First, determine which folders to loop through
  # This is the parent directory of all the actors
  pathToFolder = "raw data" # use this for pasting the folder/actor_folder path together
  # See if we have certain people the user wants to process
  # people will be a list if not NULL, a list of actors to process
  # To make this easier, create a folders list for the for loop
  # to loop through based on the actors the people list wants
  if (!is.null(people)) { # Then process each actor in the list
    # Override whatever silly number the user gave for actor, they want
    # the actor id to process from th e people vector they gave us!
    actor <- people[1] # This is so we can generate our template
    # We also don't care about the number of actors to loop through
    # If they gave us a list of actors to loop through!
    # For each actor in people, get the dir name associated with that actor
    folders <- sapply(people, FUN = function(actorID) {
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
                # Concat "Actor_" with the control and actor ID for the folder string element going into the sapply() list
                # Not only do we need the "Actor_*" but we also need a "/" because 
                # the folder we are passing to the waveToNum functions is a string like
                # "raw data/Actor_*" not just "Actor_*"
                paste(pathToFolder, "/Actor_", pasteControl, actorID, sep="")
                })
  } # end if !is.null(people)
  else { # Just loop thorugh however many actors the user wants
    # Else the user wants to loop through actor to actorEnd
    # Generate the list of folders that the user wants
    folders <- sapply(actor:actorEnd, FUN = function(actorID) {
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
      # Concat "Actor_" with the control and actor ID for the folder string element going into the sapply() list
      # Not only do we need the "Actor_*" but we also need a "/" because 
      # the folder we are passing to the waveToNum functions is a string like
      # "raw data/Actor_*" not just "Actor_*"
      paste(pathToFolder, "/Actor_", pasteControl, actorID, sep="")
    })
    # the rest of the list will look like "raw data/Actor_*"
  } # end else
  
  # Debugging 
  numFiles <- 3
  
  
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
  # Get the folder to use below
  folder <- paste("Actor_", 
                  pasteControl,
                  actor, sep="") # Don't seperate each argument by anything
  # We are banking on are assumption that every first file every actors raw folder directory
  # starts with 03-01-01-01-01-01-*.wav
  # where the star is the actor identification
  filename <- paste("03-01-01-01-01-01-",
                pasteControl, # Insert control of "0" or empty ""
                actor,
                ".wav", sep="")
  
  
  
  # initialize values matches the length when adding rows
  # We need to originally create a default dataframe so we can rbind() to it
  # But this is garbage, for now we don't know an easier way
  waveData <- waveFunToCall(folder, 
                            filename, 
                            slice = slice)
  
  fileData <- sapply(strsplit(as.character(basename(filename)), regex("\\-|\\.")), FUN = function(str) strtoi(str, base=10))
  fileData[7] <- case_when(fileData[7] %% 2 == 1 ~ "male",
                                  TRUE ~ "female")
  # Also don't forget to duplicate the file data nrow(waveData) times
  fileData <- data.frame(
    transpose(matrix(unlist(fileData)))
    )[-1][-1][-6] %>% slice(rep(1, nrow(waveData)))
  
  # # Actually, we already have one instance of the file data, so we only need
  # # nrow(waveData) - 1 more because we already have it once
  # for (dummyVar in 1:(nrow(waveData) - 1)) {
  #   # Row bind to the fileData the previous instance processed into the file data
  #   fileData <- rbind(fileData, fileData[nrow(fileData), ]) # duplicate the last recorded file data nrow(toBind) times 
  # }
  # works better if column names match when adding rows
  colnames(fileData) <- c("X3", "X4", "X5", "X6", "X7") # more templates
  # A temporary value to make code more readable and get the number of wave data rows for a single file name
  toBind <- NULL
  # A boolean for controlling iteration
  first <- TRUE # This is the first file
  # For folder in folders and file in folder
  # Loop through all folders
  # Create two data frames: 1 for the wave data, 1 for the file name data
  for (folder in folders) {
    print(basename(folder))
    #breakSoon <- FALSE
    for (file in list.files(path = folder, pattern = "*.wav")) {
      # Don't process the first file, we already did for our template
      if (!first) { # then this is not the first one
        ### First: Process and Add data to wave dataframe
        ## Do this first so we can get the number of times we need
        ## To duplicate the file data
        ## This binds all of the waveFunToCall processing for a file to the old waveData data frame
        ## Which we will merge with the fileData after looping
        ## A slow function that works for now. 
        ## Probably more effective to brute force find out what the least number of rows is
        ## But this will help speed up debugging
        waveDataToBind <- waveFunToCall(basename(folder), file, slice = slice)
        print("Got Wave Data calling waveFunToCall")
        common_cols <- intersect(colnames(waveData), colnames(waveDataToBind))
        print("Determined Col Intersection")
        # Now bind to the waveData the toBind data only those columns that are common
        waveData <- rbind(waveData[,common_cols], waveDataToBind[,common_cols])
        print("Bounded waveData")
        
        ### Second: Process and Add data to file dataframe
        data <- as.character(basename(file))
        print(file)
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
        
        # Now we need nrow() number of this file data because we are duplicating the
        # file instance, slicing different segments of amplitudes in the wave data
        
        # drops the first two columns and last column and adds the row to the data frame
        # chop off what we don't want
        # Append to the file data what we just row binded to the file data
        # nrows of toBind times
        # We need duplicates!
        # From 1 to nrow of toBind, duplicate the last recorded instance
        # Important, we may have lots of rows possible in the toBind, but the number of rows for the 
        # common_cols will be equal to or less than toBind. So generate only as many rows as the common_cols agree on
        # Actually, we already have one instance of the file data, so we only need
        # nrow(waveData) - 1 more because we already have it once
        # for (dummyVar in 1:(nrow(toBind[,common_cols]) - 1)) {
        #   # Row bind to the fileData the previous instance processed into the file data
        #   fileData <- rbind(fileData, fileData[nrow(fileData), ]) # duplicate the last recorded file data nrow(toBind) times 
        # }
        # Try to speed it up
        # row bind to the dataFrame
        # Do the replication here, then row bind that result
        # Pipe the data frame creation out slicing and repeating the 1st row (the 1 in rep)
        # nrow(waveDataToBind) number of times
        # Then row bind that to the fileData
        fileData <- rbind(fileData, data.frame(
          transpose(matrix(unlist(dataVector))) # convert to data frame
          )[-1][-1][-6] %>% slice(rep(1, nrow(waveDataToBind))) # duplicate n times
        ) # rbind()
        print("Duplicated fileData")
        print("Bounded fileData")
        
        print(paste("Length of rows for fileData: ", nrow(fileData)))
        print(paste("Length of rows for waveData: ", nrow(waveData)))
        #Debugging
        #if (breakSoon) break else breakSoon <- TRUE
        #View(waveData)
      }
      else { # this is the first one, set it to false because we're done processing the first one 
        first <- FALSE
      }
      # if (numFiles == -2) {
      #   break
      # }
      # else {
      #   numFiles <- numFiles - 1
      # }
    } # Done processing all files of an actor
    # Now we are done with an actor. Save the dataset to a file if desired
    if (saveIncrementally) { # is true
      # First, combine the data
      # put meaningfull names on the columns
      colnames(fileData) <- c("emotion", "intensity", "statement", "repetition" , "gender")
      print("Add Col Names")
      combinedDataSet <- data.frame(c(fileData, waveData))
      print("Combine!")
      # Don't scale the emotion (column 1)
      # Scale.only = true means scale between -1 to 1
      combinedDataSet <- gscale(combinedDataSet, scale.only = TRUE, vars = colnames(combinedDataSet)[-1])
      print("Scale")
      # Then save it to a csv
      # We want to save scaled data
      # We have access to this function because audioDataSetSaver.R sourced us
      if (!saveFile(combinedDataSet, actor)) {
        print("saveFile failed")
        break # stop looping
      }
      # add one to the actor because we are going to save the next actor's data using its ID as the filename
      actor <- actor + 1
      
      ## And now start the first file of the next actor for our data frame initializations
      # Before we forget, set first to true again, this will be the first file we process
      first <- TRUE
      ## Don't get another template if we are out of actors
      if (actor > actorEnd && !is.null(people) && actor > people[length(people)]) {
        break # stop this loop and processing right now
      }
      
      # First get the correct filename
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
      # Get the folder to use below
      folder <- paste("Actor_", 
                      pasteControl,
                      actor, sep="") # Don't seperate each argument by anything
      # We are banking on are assumption that every first file every actors raw folder directory
      # starts with 03-01-01-01-01-01-*.wav
      # where the star is the actor identification
      filename <- paste("03-01-01-01-01-01-",
                        pasteControl, # Insert control of "0" or empty ""
                        actor,
                        ".wav", sep="")
      # initialize values matches the length when adding rows
      # We need to originally create a default dataframe so we can rbind() to it
      # But this is garbage, for now we don't know an easier way
      waveData <- waveFunToCall(folder, 
                                filename, # the first file, different from the file var in the nested loop
                                slice = slice)
      
      fileData <- sapply(strsplit(as.character(basename(filename)), regex("\\-|\\.")), FUN = function(str) strtoi(str, base=10))
      fileData[7] <- case_when(fileData[7] %% 2 == 1 ~ "male",
                               TRUE ~ "female")
      # Also don't forget to duplicate the file data nrow(waveData) times
      fileData <- data.frame(
        transpose(matrix(unlist(fileData)))
      )[-1][-1][-6] %>% slice(rep(1, nrow(waveData)))
    }
    # Else keep appending onto the old data
    # No need to increment the actor ID. That is just used for saving to actor dataset files
  }
  # If we're not saving incrementally, combine it all at once and return
  if (!saveIncrementally) {
    # put meaningfull names on the columns
    colnames(fileData) <- c("emotion", "intensity", "statement", "repetition" , "gender")
    print("Add Col Names")
    combinedDataSet <- data.frame(c(fileData, waveData))
    print("Combine!")
    # Don't scale the emotion (column 1)
    # Scale.only = true means scale between -1 to 1
    combinedDataSet <- gscale(combinedDataSet, scale.only = TRUE, vars = colnames(combinedDataSet)[-1])
    print("Scale")
    return(combinedDataSet)
  }
  # else return TRUE, individual actor datasets saved successfully
  return(TRUE)
}


