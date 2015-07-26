data(iris);
library(ggplot2)
library(caret)
names(iris)

table(iris$Species)

#split data into training and test set
inTrain <- createDataPartition(y=iris$Species,p=0.7,list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training);dim(testing)

# Exploratory analysis
qplot(Petal.Width,Sepal.Width,data=training, col=Species)

# Tree learning
modFit <- train(Species~.,method="rpart",data=training)
print(modFit$finalModel)

# Classification Tree or Dendogram
plot(modFit$finalModel,uniform=FALSE,main="Classification Tree")
text(modFit$finalModel,use.n=FALSE,all=FALSE,cex=0.8)

library(rattle)
?rattle
fancyRpartPlot(modFit$finalModel)

#' Quiz 3
#' Question 1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
training <- segmentationOriginal[segmentationOriginal$Case=="Train",]
testing <- segmentationOriginal[segmentationOriginal$Case=="Test",]
set.seed(125)
model <- rpart(Class~.,data=training)
modFit <- train(Class~.,data=training,method="rpart")

nodata <- as.data.frame(setNames(sapply(training,mean), names(training)))
nodata <-t(nodata)
newData <- data.frame(TotalIntench2=c(23000,50000,57000,NA),
                      FiberWidthCh1=c(10,10,8,8),
                      PerimStatusCh1=c(2,NA,NA,2),
                      VarIntenCh4=c(NA,100,100,NA))
newData <- merge(x=nodata,y=newData,by= intersect(names(nodata),names(newData)),all=TRUE)

newdata1 <- data.frame(TotalIntench2 = 23000, FiberWidthCh1 = 10, PerimStatusCh1=2)
predict(modFit,newdata=newdata1,type="raw")
#' Question 2
#' The bias is smaller and the variance is smaller. Under leave 
#' one out cross validation K is equal to the sample size.

#' Question 3
library(pgmm)
data(olive)
olive = olive[,-1]

newdata = as.data.frame(t(colMeans(olive)))

head(olive)
oliveMod <- train(Area~.,data=olive,method="rpart")
predict(oliveMod,newdata)

#' Question 4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
glm.SA <- train(chd~age+alcohol+obesity+tobacco+typea+ldl,data=trainSA,method="glm",family="binomial")

probsTrain <- predict(glm.SA,trainSA)
probsTest <- predict(glm.SA,testSA)

contrasts(factor(trainSA$chd))
pred <- rep(0,nrow(testSA))
pred[probsTest>0.5] <- 1
1-mean(pred==testSA$chd)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(trainSA$chd,probsTrain)
missClass(testSA$chd,probsTest)

#' Question 5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
View(vowel.train)

set.seed(33833)
vowel.rf <- train(factor(y)~.,data=vowel.train,method="rf")
varImp(vowel.rf)
