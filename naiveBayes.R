# using Naive Bayes on our dataset
library(e1071)
library(superml)
source("waveToNum.R")  # Run this file
source("FileParser.R") # Run this file

### poor tutorial I used for this code ###
### https://www.r-bloggers.com/understanding-naive-bayes-classifier-using-r/ ###

# get the data
myData = getWaveData()

# label encode the data

# lbl <- LabelEncoder$new()
# testing <- lbl$fit_transform(myData$Emotion)
# 
# View(testing)

# get test data and train data
numRows = 1:nrow(myData) 
testRows <- sample(numRows, trunc(length(numRows)* 0.3))
testData <- myData[testRows,]
trainData <- myData[-testRows]

# building model
NBModel = naiveBayes(formula(myData), data = myData, type = "raw")
print(NBModel)

# making predictions
predictions = predict(NBModel, testData, type= "raw ")
print(predictions)

### This is bad that I have to do this ###
# since the mode can't make predictions that match or classes 
# we will convert the predictions to the appropriate class type
predictionsTrue <- sapply(1:length(testData$Emotion), FUN = function(index) {
  which.max(as.matrix(predictions[index,]))
})

print(predictionsTrue)
print(testData$Emotion)

# confusion matrix to check accuracy
confusionMatrix <- table(predictionsTrue, testData$Emotion)
agreement <- predictionsTrue == testData$Emotion
accuracy <- prop.table(table(agreement))

print(confusionMatrix)
print(accuracy)
# confusionMatrix <- table(pred = predictions, true = testData$Emotion)
# 
# agreement <- predictions == testData$Emotion
# accuracy <- prop.table(table(agreement))
