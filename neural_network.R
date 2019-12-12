#TODO building the network

# Needed libraries
#library(tidyverse)
library(neuralnet)
source("waveToNum.R")  # Run this file
source("FileParser.R") # Run this file
source("audioDataSetSaver.R") # Run this file
#library(GGally)

# data needs to be normalized first and targets label encoded
myData = getData() ### this line is for debugging perposes only ###
myData <- numericallyEncode(myData)
# get test data and train data
numRows = 1:nrow(myData) 
print(paste("Number of rows: ", nrow(myData)))
testRows <- sample(numRows, trunc(length(numRows) * 0.3))
testData <- myData[testRows,]
trainData <- myData[-testRows,]

# Nerual Network
# network is a list
# look up nerualnet:neuralnet lots of options to change
# - formula(trainData) gives a blue print of what the data looks like, returns type formula
#    - more specifically it will denote for the nueralnet() that the emotion (our first column)
#    - is the target column and the rest of the columns are the training columns
# - col1 + col2 = the futures  ## I think this is mainly used for the plotting the network model
# - hidden = # of nodes in the layer
# - act.fct = "logistic" used for smoothing the result
# - linear.output = True -> run act.fct False -> not run act.fct
# - Classification, not regression
# - learningrate = value of learning rate used on;y for back propagation
network <- neuralnet(formula(trainData), data = trainData, learningrate = .1, 
                     hidden = c(15,10,8), linear.output = TRUE)

# see what the network looks like
plot(network)


#get predictions
network.results <- compute(network, testData[,-1]) # remove the emotion

# Or try this
results <- data.frame(actual = testData$emotion, prediction = nn.results$net.result) 

# View rounded results
roundedResults <- sapply(results, round, digits = 0)
roundedResultsdf <- data.frame(roundedResults)
attach(roundedResultsdf)
table(actual, prediction) # what is prediction
  
# See the net result
print(accuracy <- predictions$net.result)
#table(testData$, apply(predictions, 1, which.max))
confusionMatrix <- table(pred = predictions, true = testData$emotion)

agreement <- predictions == testData$emotion
accuracy <- prop.table(table(agreement))

print(confusionMatrix)
View(confusionMatrix)
View(accuracy)
print(accuracy)
# should print out the % for each emotion
#probablity <- predictions$net.result
#print(probablity)



# emotionData <- getWaveData()
# View(emotionData)
# build_network(emotionData)
