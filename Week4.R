library(ElemStatLearn)
library("caret")
data(vowel.train)
data(vowel.test)
View(vowel.train)
vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
set.seed(33833)
rf.fit <- train(y~., data=vowel.train,method="rf")
pred.rf <- predict(rf.fit,newdata=vowel.test)
confusionMatrix(pred.rf,vowel.test$y)
gbm.fit <- train(y~.,data=vowel.train,method="gbm",verbose=FALSE)
pred.gbm <- predict(gbm.fit,newdata=vowel.test)
confusionMatrix(pred.gbm,vowel.test$y)$overall[1]
confusionMatrix(pred.rf,pred.gbm)$overall[1]

library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData=data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis,p=3/4)[[1]]
training <- adData[inTrain,]
testing <- adData[-inTrain,]
set.seed(62433)
rf.fit <- train(diagnosis~.,data=training,method="rf")
pred.rf <- predict(rf.fit,newdata=testing)
conf.rf <- confusionMatrix(pred.rf,testing$diagnosis)
gbm.fit <- train(diagnosis~.,data=training,method="gbm",verbose=FALSE)
pred.gbm <- predict(gbm.fit,newdata=testing)
conf.gbm <- confusionMatrix(pred.gbm,testing$diagnosis)
lda.fit <- train(diagnosis~.,data=training,method="lda")
pred.lda <- predict(lda.fit,newdata=testing)
conf.lda <- confusionMatrix(pred.lda,testing$diagnosis)
stacked <- data.frame(diagnosis=testing$diagnosis,pred.rf,pred.gbm,pred.lda)
rfEns.fit <- train(diagnosis~.,data=stacked,method="rf")
pred.rfEns <- predict(rfEns.fit,testing$diagnosis)
conf.rfEns <- confusionMatrix(pred.rfEns,testing$diagnosis)
data.frame(Ensemble=conf.rfEns$overall[1],
           RF = conf.rf$overall[1],
           GBM = conf.gbm$overall[1],
           LDA = conf.lda$overall[1])

set.seed(3523)
data(concrete)
inTrain <- createDataPartition(concrete$CompressiveStrength, p=3/4)[[1]]
training <- concrete[inTrain,]
testing <- concrete[-inTrain,]
set.seed(233)
lasso.fit <- train(CompressiveStrength~.,data=training,method="lasso")

library(lubridate)
dat = read.csv("~/Desktop/gaData.csv")
training <- dat[year(dat$date) < 2012,]
testing <- dat[year(dat$date) > 2011,]
tstrain <- ts(training$visitsTumblr)
library(forecast)
fit <- bats(tstrain)
fit 
pred <- forecast(fit,level=c(80,95),h=dim(testing)[1])
names(data.frame(pred))
mean(testing$visitsTumblr > data.frame(pred)$Lo.95 & testing$visitsTumblr < data.frame(pred)$Hi.95)

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain <- createDataPartition(concrete$CompressiveStrength,p=3/4)[[1]]
training <- concrete[inTrain,]
testing <- concrete[-inTrain,]
set.seed(325)
svm.fit <- train(CompressiveStrength~.,data=training, method="svm")
library(e1071)
svm.fit <- svm(CompressiveStrength~.,data=training)
summary(svm.fit)
sqrt(mean((predict(svm.fit,newdata=testing)-testing$CompressiveStrength)^2))
