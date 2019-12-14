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
myData$emotion[myData$emotion == as.factor("1")] <- "Sad"
# get test data and train data
myData$emotion[myData$emotion == as.factor("0")] <- "Happy"

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
# - linear.output should be TRUE if doing regression. FALSE if doing classification
# - Classification, not regression
# - learningrate = value of learning rate used on;y for back propagation
# - lifesign = "full" means have the neuralnet print everything its doing
# - lifesign.step = integer. Given the learning steps, how often should we print the minimal threshold
# - algorithm recieves a string of how the nn will calculate. 
#    - by default -> "rprop+" which is resilient back propagation with or without weight bactracking (the plus or minus sign denotes with or without)
# - threshold: the limit for the partial derivates computing the error function. Once surpassed, learning will stop
# - stepmax is the maximum number of steps the learning can take before stopping
# - rep is the number of repitions for the learning. Model will then return the best fitted model out of all the repitions
network <- neuralnet(formula(trainData),
                     data = trainData,
                     hidden = c(25,20,13,8), 
                     linear.output = FALSE, # KEEP FALSE. WE ARE DOING CLASSIFICATION
                     threshold = 0.1,
                     lifesign = "full", # for printing calculation results
                     lifesign.step = 1, # given the learning steps, how often should we print the minimal threshold
                     #algorithm = "backprop", # "rprop+" by default
                     act.fct = "tanh",
                     rep = 1,
                     stepmax = 2)

sillynet <- neuralnet(formula(trainData), data = trainData, learningrate = .1, 
                     hidden = c(10,8), linear.output = FALSE, # KEEP FALSE. WE ARE DOING CLASSIFICATION
                     #threshold = 0.1,
                     lifesign = "full", # for printing calculation results
                     lifesign.step = 1, # given the learning steps, how often should we print the minimal threshold
                     #algorithm = "backprop", # "rprop+" by default
                     act.fct = "tanh") 

one_node_goodness <- neuralnet(formula(trainData), data = trainData, learningrate = .1, 
                      hidden = c(1), linear.output = FALSE, # KEEP FALSE. WE ARE DOING CLASSIFICATION
                      #threshold = 0.1,
                      lifesign = "full", # for printing calculation results
                      lifesign.step = 1, # given the learning steps, how often should we print the minimal threshold
                      #algorithm = "backprop", # "rprop+" by default
                      act.fct = "tanh") 

plot(one_node_goodness)

plot(sillynet)
# show the error, reached.threshold value, and the number of steps taken 
print(network$result.matrix[1:3,])

# see what the network looks like
plot(network) # if network was run through repetition of models, plot only the best one with the least error

testData$emotion = as.character(testData$emotion)

#get predictions
results <- predict(network, testData[,-1]) # remove the emotion

# The actual is our emotion data in the test set
# The prediction name is what the network.results were
results <- data.frame(actual = testData$emotion, prediction = results) 
  
# See the net result
#print(accuracy <- network.results$net.result)
#table(testData$, apply(predictions, 1, which.max))
confusionMatrix <- table(pred = network.results, true = testData$emotion)

agreement <- network.results == testData$emotion
accuracy <- prop.table(table(agreement))

print(confusionMatrix)v
View(confusionMatrix)
View(accuracy)
print(accuracy)
# should print out the % for each emotion
#probablity <- predictions$net.result
#print(probablity)



# emotionData <- getWaveData()
# View(emotionData)
# build_network(emotionData)
