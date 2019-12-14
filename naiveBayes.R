# using Naive Bayes on our dataset
library(e1071)
library(superml)
#source("waveToNum.R")  # Run this file
#source("FileParser.R") # Run this file
source("audioDataSetSaver.R") # Run this file

### poor tutorial I used for this code ###
### https://www.r-bloggers.com/understanding-naive-bayes-classifier-using-r/ ###

# get the data
myData <- getData()
myData = numericallyEncode(myData)

# label encode the data

# lbl <- LabelEncoder$new()
# testing <- lbl$fit_transform(myData$Emotion)
# 
# View(testing)
myData$emotion <- as.factor(myData$emotion)
# get test data and train data
numRows = 1:nrow(myData) 
testRows <- sample(numRows, trunc(length(numRows) * 0.3))
testData <- myData[testRows,]
trainData <- myData[-testRows,]

# building model
### type = "raw" -> gives the % for each emotions; by default it will preidict just one emotion
NBModel = naiveBayes(formula(trainData), data = trainData, 
                     laplace = 3)
#print(NBModel)

# making predictions
predictions = predict(NBModel, testData, threshold = 0.01)
print(predictions)

### This is bad that I have to do this ###
# since the mode can't make predictions that match or classes 
# we will convert the predictions to the appropriate class type
# predictionsTrue <- sapply(1:length(testData$emotion), FUN = function(index) {
#   which.max(as.matrix(predictions[index,]))
# })

# look at the predictions and the class of the test data
# print(predictionsTrue)
# print(testData$emotion)

# confusion matrix to check accuracy
confusionMatrix <- table(predictionsTrue, testData$emotion)
agreement <- predictionsTrue == testData$emotion
accuracy <- prop.table(table(agreement))

print(confusionMatrix)
print(accuracy)
# confusionMatrix <- table(pred = predictions, true = testData$Emotion)
# 
# agreement <- predictions == testData$Emotion
# accuracy <- prop.table(table(agreement))



