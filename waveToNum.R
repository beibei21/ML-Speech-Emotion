#ToDO Shania

# Data Set Info: we have 24 actors with 60 wave files per actor. 
# A wave file may have around 40,000 measured amplitudes 

# So say we want 1,000,000 rows of data to operate on, then
# we can divide each wave file's amplitude count by 50 and that will get us around
# 1,000,000 rows for us to work with 50 columns each

# Needed libraries
library(tuneR) 
library(seewave) # for spectro
library(reshape2)
library(dplyr)
library(purrr) # for map function

waveToNum <- function(folder, fileName, slice = 10){
# Read in a wave dataset
#wav <- readWave("~/Documents/BYU-Idaho Classes/ML/ML-Speech-Emotion/raw data/Actor_01/03-01-01-01-01-01-01.wav")
wav <- readWave(paste("raw data", folder, fileName, sep = "/"))

# gives the duration of the .wav file in time of seconds
dur <- duration(wav)

# Used this resource for help
# https://stackoverflow.com/questions/49411154/how-can-i-get-a-dataframe-of-frequency-and-time-from-a-wav-file-in-r
# Gives a plot of the spectro graph
# Gives three measurements: Time, Frequency, Amplitude
ss = spectro(wav)

# Pull all three measurements from spectro graph
# melt converts wide format into long format

# For amplitude, replace Var1 and Var 2 with new variables
amp = melt(ss$amp, value.name = "Amplitude") %>% 
  select(FrequencyIndex = Var1, TimeIndex = Var2, Amplitude)

# For frequency, add matching row num and changing value of frequency
# Frequency value from kHz to Hz
frequent = melt(ss$freq, value.name = "Frequency") %>% 
  mutate(FrequencyIndex = row_number(), Frequency = Frequency * 1000)

# For time, add a TimeIndex for left join later on
tm = melt(ss$time, value.name = "Time") %>% 
  mutate(TimeIndex = row_number())

# left join all datasets together 
# Only need Time, Frequency, and Amplitude
numeric_data <- amp %>% 
  left_join(frequent, by = "FrequencyIndex") %>% 
  left_join(tm, by = "TimeIndex") %>% 
  select(FrequencyIndex,Time, Frequency, Amplitude) %>% 
  filter(Time >= 1) %>% # shed first second of data
  filter(Time <= (dur - 1)) # shed last second of data

#View(numeric_data, title="Numeric Data")

# Get the sample of data by dividing the number of rows by 
# 10 We don't need tons of columns, just 10
segment <- trunc(nrow(numeric_data) / slice)
print(segment)

# Randomly sample from 10 equi sections  
rows <- rows <- sapply(1:segment, FUN = function(multiple) {
  numeric_data[round(sample(((multiple - 1) * segment + 1):(segment*multiple), size=1)),]
})

# Quote from Joe Armstrong about seven deadly sins (coding sins):
# No comments in the code. You can't understand it. No specification. Very obscure. Etc...
# That's how I feel ;) haha! Please explain what the 4 means.
# We only want the amplitude, which is in row 4 of all columns
# This is the comment
row <- data.frame(rows[4,]) # Only take amplitude, row 4
#row <- colnames(row,prefix = "Amp")
# Give this row meaningful column names so they will match
# the dataframe we will bind this row to in FileParser.R
colnames(row) <- colnames(row, do.NULL = FALSE, prefix = "Amp")

return(row)

}

# Recieve a slice amount to slice all the amount of amplitudes
# into even segments and randomly sample them
waveToNumWithSlicesAdded <- function(folder, fileName, slice = 50){
  # Read in a wave dataset
  #wav <- readWave("~/Documents/BYU-Idaho Classes/ML/ML-Speech-Emotion/raw data/Actor_01/03-01-01-01-01-01-01.wav")
  wav <- readWave(paste("raw data", folder, fileName, sep = "/"))
  
  # gives the duration of the .wav file in time of seconds
  dur <- duration(wav)
  
  # Used this resource for help
  # https://stackoverflow.com/questions/49411154/how-can-i-get-a-dataframe-of-frequency-and-time-from-a-wav-file-in-r
  # Gives a plot of the spectro graph
  # Gives three measurements: Time, Frequency, Amplitude
  ss = spectro(wav)
  
  # Pull all three measurements from spectro graph
  # melt converts wide format into long format
  
  # For amplitude, replace Var1 and Var 2 with new variables
  amp = melt(ss$amp, value.name = "Amplitude") %>% 
    select(FrequencyIndex = Var1, TimeIndex = Var2, Amplitude)
  
  # For frequency, add matching row num and changing value of frequency
  # Frequency value from kHz to Hz
  frequent = melt(ss$freq, value.name = "Frequency") %>% 
    mutate(FrequencyIndex = row_number(), Frequency = Frequency * 1000)
  
  # For time, add a TimeIndex for left join later on
  tm = melt(ss$time, value.name = "Time") %>% 
    mutate(TimeIndex = row_number())
  
  # left join all datasets together 
  # Only need Time, Frequency, and Amplitude
  numeric_data <- amp %>% 
    left_join(frequent, by = "FrequencyIndex") %>% 
    left_join(tm, by = "TimeIndex") %>% 
    select(FrequencyIndex,Time, Frequency, Amplitude) %>% 
    filter(Time >= 1) %>% # shed first second of data
    filter(Time <= (dur - 1)) # shed last second of data
  #View(numeric_data, title="Numeric Data")
  # Get the sample of data by dividing the number of rows by 
  # 50.
  # Make sure we have an integer segment so we can index the
  # rows
  segment <- trunc(nrow(numeric_data) / slice)
  print(segment)
  
  
  # Take slices of equalength 
  # This relies on the formula function using the truncated
  # number of rows of the data set divided by the slice
  # as segments. Will this ever cause us to index off the end of
  # the vector? Well, if there are 100 rows, and the slice is 10
  # then each segment will be of length 10. The indices for the sections 
  # will be: 1:10, 11:20, 21:30, 31:40, 41:50, 51:60, 61:70, 71:80,
  #          81:90, 91:100 and because sapply() goes from 1:slice with 
  # with slice being 10 and since we see above their are 10 slices above 
  # which is the math that sapply() does for the index slicing, we will not 
  # overflow the vector bounds. Trunc keeps us negative so we will always be less than or equal
  # to, never greater than the number of rows possible.
  # "We got a weird error and that was our work-around." - Jon Stutz
  dataToReturn <- dataToReturn <- sapply(1:slice, FUN = function(multiple) {
    # Index in R starts at 1, so add one
    # If segment is 10 then the first on upward sections will be 
    # ((1 - 1) * 10 + 1):(10*1) ->  1:10
    # ((2 - 1) * 10 + 1):(10*2) -> 11:20
    # ((3 - 1) * 10 + 1):(10*3) -> 21:30
    # And so on....
    # The last line ran is what is returned. Put this slice into the 
    # vector sapply() is building.
    numeric_data[((multiple - 1) * segment + 1):(segment*multiple),]
  })
  
  # Quote from Joe Armstrong about seven deadly sins (coding sins):
  # No comments in the code. You can't understand it. No specification. Very obscure. Etc...
  # That's how I feel ;) haha! Please explain what the 4 means.
  # We only want the amplitude, which is in row 4 of all columns
  # This is the comment
  dataToReturn <- data.frame(dataToReturn[4,]) # Only take amplitude, row 4
  #row <- colnames(row,prefix = "Amp")
  # Give this row meaningful column names so they will match
  # the dataframe we will bind this row to in FileParser.R
  # Prefix the rows 
  #colnames(row) <- c("Amp1", "Amp2", "Amp3", "Amp4", "Amp5", "Amp6", "Amp7", "Amp8", "Amp9", "Amp10")
  #View(dataToReturn, title = "WaveReturnBefore")
  column_names <- colnames_generator("Amp", 1:ncol(dataToReturn))
  colnames(dataToReturn) <- column_names
  #View(dataToReturn, title = "WaveReturnAfter")
  return(dataToReturn)
  
}

# colnames() is not intuitive and we can't get it to work. 
# Create a prefix colnames function generator ourselves
# Generate a vector of column names given a prefix and range
# The prefix is like "Amp" or "Col" and the range is the numbers from 
# beginning to end to append to the prefix
colnames_generator <- function(prefix, range) {
  sapply(range, function(index) {
    paste(prefix, index, sep="") # concat the two values together with nothing separating
  })
}