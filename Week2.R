#### Set Working Directory ####
setwd("~/DataScience/PracticalMachineLearning")
####Load Necessary Libraries####
LoadLibraries <- function(){
  library(ISLR)
  library(ggplot2)
  library(caret)
  library(AppliedPredictiveModeling)
  library(kernlab)
}

LoadLibraries()

data(spam)
set.seed(333)
smallSpam <- spam[sample(nrow(spam),size=10),]
spamLabel <- (smallSpam$type=="spam")+1
plot(smallSpam$capitalAve,col=spamLabel)

#### Quiz 2 ####
# Question 1 #
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)
# Create training and test sets each containing 50% of the data #
adData <- data.frame(diagnosis,predictors)
inTrain <- createDataPartition(diagnosis,p= 0.50, list= FALSE)
training <- adData[inTrain,]
testing <- adData[-inTrain,]

# Question 2 #
data(concrete)
set.seed(1000)
inTrain <- createDataPartition(mixtures$CompressiveStrength,p=3/4)[[1]]
training <- mixtures[inTrain,]
testing <- mixtures[-inTrain,]
# Make a histogram to confirm skewedness of SuperPlasticizer variable #
hist(concrete$Superplasticizer)
# Show why taking the log transformation of this variable would be a poor decision #
summary(concrete$Superplasticizer)
summary(log(concrete$Superplasticizer))
# There are values of zero so when you take the log() transform those values will be -Inf. #

# Question 3 #
set.seed(3433)
data(AlzheimerDisease)
adData <- data.frame(diagnosis,predictors)
inTrain <- createDataPartition(adData$diagnosis,p=3/4)[[1]]
training <- adData[inTrain,]
testing <- adData[-inTrain,]
# Subset all the variables in the training set that begin with the characters "IL" #
IL <- adData[inTrain,grep("^IL",names(adData))]
# Perform principal components on these variables with the preProcess() function from caret #
preProcess(IL,method='pca')
#' Determine from the last command how many principal      #
#' components are necessary to capture 90% of the variance #
preProcess(IL,method='pca',thresh=0.80)

# Question 4 #
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
# Subset all the variables in the training set that begin with the characters "IL" #
IL.train <- adData[inTrain,grep("^IL",names(adData))]
IL.train <- data.frame(diagnosis[inTrain],IL.train)
names(IL.train)[1] <- "diagnosis"
# Make sure IL.test has identical variables to IL.train #
IL.test <- adData[-inTrain,grep("^IL",names(adData))]
IL.test <- data.frame(diagnosis[-inTrain],IL.test)
names(IL.test)[1] <- "diagnosis"
# Predict with normal IL variables #
modelFit <- train(diagnosis~.,data=IL.train,method="glm")
confusionMatrix(IL.test$diagnosis,predict(modelFit,IL.test))
# Predict with PCA #
preProc <- preProcess(IL.train[,-1],method='pca',thresh=0.80)
trainPC <- predict(preProc,IL.train[,-1])
modelFitPC <- train(IL.train$diagnosis~.,method='glm',data=trainPC)
testPC <- predict(preProc,IL.test[,-1])
confusionMatrix(IL.test$diagnosis,predict(modelFitPC,testPC))

