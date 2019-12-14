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
myData <- getData("emotion_all.csv")
myData = numericallyEncode(myData)
#myData <- getData("emotion_all.csv")
SVMTypes <- c("C-classification")
kernels <- c('linear', 'polynomial', 'sigmoid', 'radial')
results <- vector()
myData$emotion <- myData$emtion
numbers <- c(1:8)
combinations <- combn(numbers, 2, simplify = FALSE)

for (number in combinations) {
pair_of_emotions <- substring(number, 1, 1) # break two numbers into separate strings
myData[myData == as.integer(pair_of_emotions[2])] <- as.integer(pair_of_emotions[1]) # Takes pair_of_emotions number and combines with second number
# Get test data and train data
numRows = 1:nrow(myData) 
testRows <- sample(numRows, trunc(length(numRows) * 0.3))
testData <- myData[testRows,]
trainData <- myData[-testRows,]

print(pair_of_emotions)
# another for loop for the SVM Types

  for (type in SVMTypes) {
    for (kernel in kernels) {
      print(type)
      print(kernel)
      # type 'nu-classification' or ' C-classification'
      # kernel 'linear', 'polynomial', 'sigmoid', 'radial' 
      #
      ### best option on 2 class happy vs sad -> linear nu-Class..
      ## best option 2 class happy vs sad -> radial, C-Class
      ### best option on 5 radial, C-Class
      classifier = svm(formula = formula(trainData), 
                       data = trainData, 
                       type = type, 
                       kernel = kernel,
                       gamma = .5, 
                       cost = 4) 
    
    print("stop")

    y_pred = predict(classifier, newdata = testData[-1])
    
    
    # Confusion Matrix 
    cm <- table(testData[, 1], y_pred) 
    
    agreement <- y_pred == testData$emotion
    accuracy <- prop.table(table(agreement))
    #pair_of_emotions[1], pair_of_emotions[2], type, kernel is key
    # accuracy is value (sorted on accuracy)
    # map
    results <- append(results, c(pair_of_emotions[1], pair_of_emotions[2], type, kernel, accuracy[2]))
    # print("Look here")
    # print(pair_of_emotions[1])
    # print(pair_of_emotions[2])
    # print(type)
    # print(kernel)
    # print(accuracy[2])
    }
  }
}


# obj <- tune(svm, emotion~., data = trainData, 
#             ranges = list(gamma = 2^(-1:1), cost = 2^(2:4), nu = seq(0.01,0.5, .1),
#             tunecontrol = tune.control(sampling = "fix")))



# summary(obj)
# plot(obj)

