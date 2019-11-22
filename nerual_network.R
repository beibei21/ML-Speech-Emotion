#TODO building the network

# Needed libraries
#library(tidyverse)
library(neuralnet)
#library(GGally)

build_network <- function(myData){
  # data needs to be normalized first and targets label encoded
  
  # Nerual Network
  # network is a list
  # look up nerualnet:neuralnet lots of options to change
  # - Emotion~ = col of targets
  # - col1 + col2 = the futures  ## I think this is mainly used for the plotting the network model
  # - hidden = # of nodes in the layer
  # - act.fct = "logistic" used for smoothing the result
  # - linear.output = True -> run act.fct False -> not run act.fct
  # - learningrate = value of learning rate used on;y for back propagation
  network <- neuralnet(Emotion~col1+col2, data = myData, learningrate = .1, 
                       hidden = 10, act.fct = "logistic", linear.output = FALSE )
  
  # see what the network looks like
  plot(network)
  
  # get test data and train data
  numRows = 1:nrow(myData) 
  testRows <- sample(numRows, trunc(length(numRows)* 0.3))
  testData <- myData[testRows,]
  trainData <- myData[-testRows]
  
  #get predictions
  predictions = compute(network, testData)
  # should print out the % for each emotion
  probablity <- prediction$net.result
  print(probablity)
  
}