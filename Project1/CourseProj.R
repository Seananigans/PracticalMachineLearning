####Project 1 Directory Setup####
CourseDir<- getwd()
if (CourseDir!="~/DataScience/PracticalMachineLearning"){
  print("Changing Directory")
  setwd("~/DataScience/PracticalMachineLearning")
  CourseDir<- getwd()
}

####Create Directory for data and figures####
if (!file.exists("Project1")) {
  print("Creating Project1 Directory")
  dir.create("Project1")
  dir.create("Project1/data")
  dir.create("Project1/figures")
} 
ProjDir <- getwd()
if (ProjDir!="~/DataScience/PracticalMachineLearning/Project1"){
  print("Changing Directory")
  setwd(paste(CourseDir,"/Project1",sep=""))
}
ProjDir <- getwd()

####Download file####
if(!"pml-training.csv" %in% dir("data")){
  print("Downloading Activity Data")
  setwd(paste(ProjDir,"/data",sep=""))
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
                destfile = "pml-training.csv")
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
                destfile = "pml-testing.csv")
  setwd(ProjDir)
}

####Importing Data####
if(!"pml.training" %in% ls()){
  print("Importing Training Data into Environment")
  setwd(paste(ProjDir,"/data",sep=""))
  pml.training <- read.csv("pml-training.csv")
  setwd(ProjDir)
}
if(!"pml.testing" %in% ls()){
  print("Importing Testing Data into Environment")
  setwd(paste(ProjDir,"/data",sep=""))
  pml.testing <- read.csv("pml-testing.csv")
  setwd(ProjDir)
}

####Cleaning Data####
library(caret)

#### PreProcessing: Setting aside classe data ####
classe <- pml.training$classe

#### PreProcessing: Identify Near Zero Variability Predictors#### 
nsv <- nearZeroVar(pml.training,saveMetrics=TRUE)
pml.train <- pml.training[,!nsv$nzv]
dim(pml.train)

#### PreProcessing: Remove Precalculated Statistics columns ####
pml.train <- pml.train[,colSums(is.na(pml.train))<19216]
dim(pml.train)

#### PreProcessing: Remove Perceived Unecessary Predictors ####
pml.train <- pml.train[,sapply(pml.train,is.numeric)]
if(ncol(pml.train)>33){
  pml.train <- pml.train[,-grep("^X|stamp|window",names(pml.train))]
}
dim(pml.train)

#### PreProcessing: Identify Correlated Predictors####
desCor <- cor(pml.train)
summary(desCor[upper.tri(desCor)])
hiCor <- sum(abs(desCor[upper.tri(desCor)])>0.8)
hiCorDes <- findCorrelation(desCor,cutoff=0.75)
pml.train <- pml.train[, -hiCorDes]
dim(pml.train)

descrCor2 <- cor(pml.train[,7:(ncol(pml.train)-1)])
summary(descrCor2[upper.tri(descrCor2)])

#### PreProcessing: Making an equivalent test set####
name <- names(pml.train)
pml.train$classe <- classe
problem_id <- pml.testing$problem_id
pml.test <- pml.testing[,name]
pml.test$problem_id <- problem_id

#### Data Splitting: Create a training set and test set ####
set.seed(4190)
inTrain <- createDataPartition(pml.train$classe,p= 0.25, list= FALSE)
training <- pml.train[inTrain,]
testing <- pml.train[-inTrain,]

#### PreProcessing: Create 5-fold CV####
fitControl <- trainControl(method="repeatedcv",
                           number=5)

#### Model Building: Random Forests####
library(randomForest)
modelFit <- train(training$classe ~ .,
                  method="rf",
                  trControl=fitControl,
                  data=training)
confusionMatrix(testing$classe,predict(modelFit,testing))

#### Model Testing: Prediction accuracy ####
predict(modelFit,pml.testing)

postResample(predict(modelFit,testing),testing$classe)

confusionMatrix(predict(modelFit,testing),testing$classe)

####Answers####
answers = predict(modelFit,pml.testing)
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)