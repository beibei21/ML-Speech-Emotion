#ToDO Shania
# Needed libraries
library(tuneR) 
library(seewave) # for spectro
library(reshape2)
library(dplyr)
library(purrr) # for map function

waveToNum <- function(fileName){
# Read in a wave dataset
#wav <- readWave("~/Documents/BYU-Idaho Classes/ML/ML-Speech-Emotion/raw data/Actor_01/03-01-01-01-01-01-01.wav")
wav <- readWave("raw data/Actor_01/03-01-01-01-01-01-01.wav")

#datapath <- ("~/Documents/BYU-Idaho Classes/ML/ML-Speech-Emotion/raw data/Actor_01")
#files <- dir(datapath, pattern = "*.wav")

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
  select(Time, Frequency, Amplitude)

return( numeric_data)
}

View(waveToNum("raw data/Actor_01/03-01-01-01-01-01-01.wav"))
