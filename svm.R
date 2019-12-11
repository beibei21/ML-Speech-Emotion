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
myData = getData("emotion_all.csv")


# # the following are to encode the emotions to 2 emotions
# # Emotion 1 will be happy comprised of neutral, calm, happy, surprised
# myData[myData == 2] <- 1
# myData[myData == 3] <- 1
# myData[myData == 8] <- 1
# 
# #Emotion 2 will be sad comrised of sad, angry, fearful, disgust
# myData[myData == 4] <- 2
# myData[myData == 5] <- 2
# myData[myData == 6] <- 2
# myData[myData == 7] <- 2

# the following are to encode the emotions to 4 emotions
# Emotion 1 will be neutral comprised of neutral, calm
myData[myData == 2] <- 1
# Emotion 3 will be happy comprised of happy, sad
#myData[myData == 4] <- 3
# Emotion 5 will be angry comprised of angry, fearful
myData[myData == 6] <- 5
# Emotion 7 will be angry comprised of disgust, surprised
myData[myData == 8] <- 7




# Get test data and train data
numRows = 1:nrow(myData) 
testRows <- sample(numRows, trunc(length(numRows) * 0.3))
testData <- myData[testRows,]
trainData <- myData[-testRows,]

# Scale the DAta 
#already scaled It does not do any better to scale again
# trainData[-1] = scale(trainData[-1])
# testData[-1] = scale(testData[-1])
# print(formula(trainData))



### use hyper parameters


# type 'nu-classification' or ' C-classification'
# kernel 'linear', 'polynomial', 'sigmoid', 'radial' 
#
### best option on 2 class happy vs sad -> linear nu-Class..
## best option 2 class happy vs sad -> radial, C-Class
### best option on 5 radial, C-Class
classifier = svm(formula = formula(trainData), 
                 data = trainData, 
                 type = 'C-classification', 
                 kernel = 'radial', 
                 gamma = .5, 
                 cost = 4) 

obj <- tune(svm, emotion~., data = trainData, 
            ranges = list(gamma = 2^(-1:1), cost = 2^(2:4), nu = seq(0.01,0.5, .1),
            tunecontrol = tune.control(sampling = "fix")))



summary(obj)
plot(obj)

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
