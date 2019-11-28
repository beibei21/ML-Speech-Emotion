#TODO building the network

# Needed libraries
#library(tidyverse)
library(neuralnet)
source("waveToNum.R")  # Run this file
source("FileParser.R") # Run this file
#library(GGally)


build_network <- function(myData){
  # data needs to be normalized first and targets label encoded
  myData <- emotionData ### this line is for debugging perposes only ###
  # Nerual Network
  # network is a list
  # look up nerualnet:neuralnet lots of options to change
  # - formula(myData) gives a blue print of what the data looks like, returns type formula
  # - col1 + col2 = the futures  ## I think this is mainly used for the plotting the network model
  # - hidden = # of nodes in the layer
  # - act.fct = "logistic" used for smoothing the result
  # - linear.output = True -> run act.fct False -> not run act.fct
  # - Classification, not regression
  # - learningrate = value of learning rate used on;y for back propagation
  network <- neuralnet(formula(myData), data = myData, learningrate = .1, 
                       hidden = c(10,10), act.fct = "logistic", linear.output = FALSE)
  
  # see what the network looks like
  plot(network)
  
  # get test data and train data
  numRows = 1:nrow(myData) 
  testRows <- sample(numRows, trunc(length(numRows)* 0.3))
  testData <- myData[testRows,]
  trainData <- myData[-testRows]
  
  #get predictions
  predictions = predict(network, testData)
  #table(testData$Emotion, apply(predictions, 1, which.max))
  
  confusionMatrix <- table(pred = predictions, true = testData$Emotion)
  
  agreement <- predictions == testData$Emotion
  accuracy <- prop.table(table(agreement))
  
  
  
  # should print out the % for each emotion
  probablity <- predictions$net.result
  print(probablity)
}


emotionData <- getWaveData()
build_network(emotionData)
