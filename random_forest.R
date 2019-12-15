# random forest 

library(randomForest)

source("audioDataSetSaver.R") # Run this file

# Helpful tutorials 

# https://towardsdatascience.com/learn-classification-with-random-forests-in-r-998d3a734098

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

# random forest 
# ntree = 1000
forests <- randomForest(formula = as.factor(emotion) ~ ., ntree = 1000, data = trainData)
# Calm = 2 has the least amount of error at 65% error 
# Sad =  4 has 83% error 
# Happy = 3 82% error
# Angry = 5 71% error 
# Fearful = 6 92# error
# Disgust = 7 90% error
# Surprised = 8 80% error
# Neutral = 1 89% error 

# Out of Box estimate of error rate 80.75% 
#Calm least amount of error 65%
#Fearful greatest amount of error 90%
# Angry second least amount of error 71%

# trying to do a confusion matrix and show accuracy as well 
testData$pred <- predict(forests, testData)
testData$pred <- as.factor(testData$pred)
confusionMatrix(testData$pred, as.factor(testData$emotion))
# 22.45% accuracy 

#############################################################################
# New Data
#############################################################################

# Get the data 
library(readr)
emotion_extended <- read_csv("emotion_extended.csv") 

emotion_extended$gender[emotion_extended$gender == "male"] <- 1
emotion_extended$gender[emotion_extended$gender == "female"] <- 0

emotion_extended$gender <- as.numeric(emotion_extended$gender)

# Get test data and train data
numRows2 = 1:nrow(emotion_extended) 
testRows2 <- sample(numRows2, trunc(length(numRows2) * 0.3))
testData2 <- emotion_extended[testRows2,]
trainData2 <- emotion_extended[-testRows2,]

# Scale the DAta 
#trainData[-1] <- scale(trainData2[-1])
#testData[-1] <- scale(testData2[-1])
#print(formula(trainData2))

# random forest 
# ntree = 1000
forests2 <- randomForest(formula = as.factor(emotion) ~ ., ntree = 15, data = trainData2)
# Calm = 2 has the least amount of error at 65% error 
# Sad =  4 has 83% error 
# Happy = 3 82% error
# Angry = 5 71% error 
# Fearful = 6 92# error
# Disgust = 7 90% error
# Surprised = 8 80% error
# Neutral = 1 89% error 

# Out of Box estimate of error rate 80.75% 
#Calm least amount of error 65%
#Fearful greatest amount of error 90%
# Angry second least amount of error 71%

library(caret) # for confusionMatrix
# trying to do a confusion matrix and show accuracy as well 
testData2$pred <- predict(forests20, testData2)
testData2$pred <- as.factor(testData2$pred)
confusionMatrix(testData2$pred, as.factor(testData2$emotion))
# 87.87 % with 10 trees 
# 95.31 % with 20 trees
# 

forests3 <- randomForest(formula = as.factor(emotion) ~ ., ntree = 10, data = trainData2)

forests20 <- randomForest(formula = as.factor(emotion) ~ ., ntree = 20, data = trainData2)
# COOB 16.63%

forests30 <- randomForest(formula = as.factor(emotion) ~ ., ntree = 30, data = trainData2)

testData2$pred <- predict(forests30, testData2)
testData2$pred <- as.factor(testData2$pred)
confusionMatrix(testData2$pred, as.factor(testData2$emotion))

forests40 <- randomForest(formula = as.factor(emotion) ~ ., ntree = 40, data = trainData2)

testData2$pred <- predict(forests40, testData2)
testData2$pred <- as.factor(testData2$pred)
confusionMatrix(testData2$pred, as.factor(testData2$emotion))


forests50 <- randomForest(formula = as.factor(emotion) ~ ., ntree = 50, data = trainData2)

testData2$pred <- predict(forests50, testData2)
testData2$pred <- as.factor(testData2$pred)
confusionMatrix(testData2$pred, as.factor(testData2$emotion))
