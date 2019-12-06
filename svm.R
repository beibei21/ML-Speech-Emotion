# svm
# Our libraries needed
library(e1071)
library(caret)
#source("waveToNum.R")  # Run this file
#source("FileParser.R") # Run this file
source("audioDataSetSaver.R") # Run this file

# Helpful tutorials 

# https://www.geeksforgeeks.org/classifying-data-using-support-vector-machinessvms-in-r/

# Get the data 
myData = getData()

# Get test data and train data
numRows = 1:nrow(myData) 
testRows <- sample(numRows, trunc(length(numRows) * 0.3))
testData <- myData[testRows,]
trainData <- myData[-testRows,]

# Scale the DAta 
trainData[-1] = scale(trainData[-1])
testData[-1] = scale(testData[-1])
print(formula(trainData))



### use hyper parameters


# type 'nu-classification' or ' C-classification'
# kernel 'linear', 'polynomial', 'sigmoid', 'radial' 
#
classifier = svm(formula = formula(trainData), 
                 data = trainData, 
                 type = 'nu-classification', 
                 kernel = 'radial') 

y_pred = predict(classifier, newdata = testData[-1])


# Confusion Matrix 
cm = table(testData[, 1], y_pred) 

agreement <- y_pred == testData$emotion
accuracy <- prop.table(table(agreement))

print(cm)
print(accuracy)

# 
#table(predict(classifier), myData$Emotion, dnn=c("Prediction", "Actual"))  

# this didn't work 
#confusionMatrix(myData$Emotion, predict(classifier))
