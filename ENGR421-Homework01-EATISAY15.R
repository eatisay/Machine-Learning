#I have imported the data here
imgs <- read.csv("hw01_images.csv" , header = FALSE)
lbls <- read.csv("hw01_labels.csv" , header = FALSE)
#Combined the images and labels
dataSet<-cbind(imgs,lbls)
#Partitioned the data as train and test
trainingSet <- dataSet[c(1:(nrow(dataSet)/2)),]
testSet <- dataSet[c(((nrow(dataSet)/2)+1):nrow(dataSet)),]
#To hold labels as columns
y<-trainingSet[,4097]
y1 <- lbls[c(1:(nrow(dataSet)/2)),]
#It is part where I seperate the train set based on labels
trainingMen<-subset(trainingSet,y==2,-c(4097))
trainingWomen<-subset(trainingSet,y==1,-c(4097))
#Means of the training set found separately
meansTrainingMen<-colMeans(trainingMen)
meansTrainingWomen<-colMeans(trainingWomen)
#Deviations are calculated column by column
deviationsTrainingMen<-sapply(X= 1:ncol(trainingMen), FUN=function(c){sqrt(colSums((trainingMen[c] - meansTrainingMen[c])^2)/nrow(trainingMen))})
deviationsTrainingWomen<-sapply(X= 1:ncol(trainingWomen), FUN=function(c){sqrt(colSums((trainingWomen[c] - meansTrainingWomen[c])^2)/nrow(trainingWomen))})
# Those seperate means and squared devaitions are binded
means<-cbind(meansTrainingWomen,meansTrainingMen)
deviations<-cbind(deviationsTrainingWomen,deviationsTrainingMen)
#Prior probabilities are observed by calculating the portions.
classPriors <- cbind(length(trainingSet[y==1])/length(trainingSet) , length(trainingSet[y==2])/length(trainingSet))
#TrainingSet is identified again as it excludes the label data
trainingSet <- imgs[c(1:(nrow(imgs)/2)),]
#Helper function for log operation is written to exclude the minus infinity
logHelper <- function(c) {return (log(1e-200 + c))}
#Score function implemented as the naive Bayes' classifier. Then, transpose of the result has taken to have matched lengths
#in creating the confusion matrix.
classScores <- sapply(X = 1:nrow(trainingSet) ,FUN = function(c){(as.matrix(trainingSet[c, ]) %*% as.matrix(logHelper(means))) + (as.matrix(1-trainingSet[c,]) %*% as.matrix(logHelper(1 - means))) + log(classPriors)})
classScores<-t(classScores)
yTraining <- matrix(NA,nrow(trainingSet),1)
#The greater score among the columns gives the predicted value for the label corresponds
yTraining <- sapply(X = 1: nrow(classScores) , FUN = function(c){if(classScores[c,1]>classScores[c,2]){yTraining[c] = 1} else{yTraining[c] = 2}})
#confusion matrix for the training set
trainingConfusion <- table(y1,yTraining)
print(trainingConfusion)
#For the confusion matrix, test set's labels are contained in y2
y2<-testSet[,4097]
#Test set is re-arrenged so that the labels are excluded
testSet <- imgs[c((nrow(imgs)/2)+1):nrow(imgs),]
#Scores are observed just as the way in the training set. Than, again, transpose of it is taken
testScores <- sapply(X = 1:nrow(testSet) , FUN = function(c){(as.matrix(testSet[c, ]) %*% as.matrix(logHelper(means))) + (as.matrix(1-testSet[c,]) %*% as.matrix(logHelper(1 - means))) + log(classPriors)})
testScores<-t(testScores)
#Same idea holds, greater score is specified from a function
yTest <- matrix(NA,nrow(testSet),1)
yTest <- sapply(X = 1: nrow(testScores) , FUN = function(c){if(testScores[c,1]>testScores[c,2]){yTest[c] = 1}else{yTest[c] = 2}})
trainingConfusionTest <- table(y2,yTest)
print(trainingConfusionTest)



