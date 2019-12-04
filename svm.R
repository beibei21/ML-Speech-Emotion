# svm
# Our libraries needed
library(e1071)
library(caret)
source("waveToNum.R")  # Run this file
source("FileParser.R") # Run this file

# Helpful tutorials 

# https://www.geeksforgeeks.org/classifying-data-using-support-vector-machinessvms-in-r/

# Get the data 
myData = getWaveData()

# Get test data and train data
numRows = 1:nrow(myData) 
testRows <- sample(numRows, trunc(length(numRows) * 0.3))
testData <- myData[testRows,]
trainData <- myData[-testRows]

# Scale the DAta 
trainData[-1] = scale(trainData[-1]) 
testData[-1] = scale(testData[-1]) 

classifier = svm(formula = Emotion ~ ., 
                 data = trainData, 
                 type = 'C-classification', 
                 kernel = 'linear') 

y_pred = predict(classifier, newdata = testData[-1])


# Confusion Matrix 
cm = table(testData[, 1], y_pred) 

# 
table(predict(classifier), myData$Emotion, dnn=c("Prediction", "Actual"))  

# this didn't work 
#confusionMatrix(myData$Emotion, predict(classifier))
