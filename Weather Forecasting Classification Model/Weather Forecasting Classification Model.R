# “Gain Familiarity with Classification Models Using R
# Loi Chai Lam  28136179

# Setting up the workspace
setwd("C:/Users/caila/Desktop/FIT3152 Assignment")

# Importing package
library(neuralnet)
#install.packages("dplyr")
library(dplyr)
#install.packages("tree")
library(tree)
#install.packages("e1071")
library(e1071)
#install.packages(("ROCR"))
library(ROCR)
#install.packages("randomForest")
library(randomForest)
#install.packages("adabag")
library(adabag)
#install.packages("rpart")
library(rpart)
#install.packages("neuralnet")


# Clean Workspace and read file
rm(list = ls())
WAUS <- read.csv("WAUS2019.csv")
L <- as.data.frame(c(1:49))
set.seed(28136179) # Your Student ID is the random seed
L <- L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS <- WAUS[(WAUS$Location %in% L),]
WAUS <- WAUS[sample(nrow(WAUS), 10000, replace = FALSE),] # sample 10000 rows



#----------------------------------------------------------------------------------------------------------------------------------#
# Q1. Explore the data
attach(WAUS)
#a. What is the proportion of rainy days to fine days in RainToday (rainy days : fine days)? 
RainTodaySummary = as.data.frame(as.table(summary(RainToday)))
rainDay = RainTodaySummary$Freq[2]
fineDay = RainTodaySummary$Freq[1]
# proportion is 2085 : 7771 (rainy days : fine days) 
cat("propotion : ",rainDay," : ",fineDay)


#b. Description of the predictor(independent variables) - means, standard deviations
WAUS$Location = as.factor(WAUS.Location)
WAUS$Year = as.factor(WAUS.Year)
WAUS$Month = as.factor(WAUS.Month)
WAUS$Day = as.factor(WAUS.Day)

predictorFrame = rbind(summary(na.omit(Day)),summary(na.omit(Month)),summary(na.omit(Year)),summary(na.omit(Location)),summary(na.omit(MinTemp)),summary(na.omit(MaxTemp)),summary(na.omit(Rainfall)),summary(na.omit(Evaporation)),summary(na.omit(Sunshine)),summary(na.omit(WindGustSpeed)),summary(na.omit(WindSpeed9am)),summary(na.omit(WindSpeed3pm)),summary(na.omit(Humidity9am)),summary(na.omit(Humidity3pm)),summary(na.omit(Pressure9am)),summary(na.omit(Pressure3pm)),summary(na.omit(Cloud9am)),summary(na.omit(Cloud3pm)),summary(na.omit(Temp9am)),summary(na.omit(Temp3pm)))

tmp = rbind(sd(na.omit(Day)),sd(na.omit(Month)),sd(na.omit(Year)),sd(na.omit(Location)),sd(na.omit(MinTemp)),sd(na.omit(MaxTemp)),sd(na.omit(Rainfall)),sd(na.omit(Evaporation)),sd(na.omit(Sunshine)),sd(na.omit(WindGustSpeed)),sd(na.omit(WindSpeed9am)),sd(na.omit(WindSpeed3pm)),sd(na.omit(Humidity9am)),sd(na.omit(Humidity3pm)),sd(na.omit(Pressure9am)),sd(na.omit(Pressure3pm)),sd(na.omit(Cloud9am)),sd(na.omit(Cloud3pm)), sd(na.omit(Temp9am)),sd(na.omit(Temp3pm)))

predictorFrame = cbind(predictorFrame,tmp)

row.names(predictorFrame) = c("Day","Month","Year","Location","MinTemp","MaxTemp","Rainfall","Evaporation","Sunshine","WindGustSpeed","WindSpeed9am","WindSpeed3pm","Humidity9am","Humidity3pm","Pressure9am","Pressure3pm","Cloud9am","Cloud3pm","Temp9am","Temp3pm")

colnames(predictorFrame)[7] = "Stand.Dev."

View(predictorFrame)

summary(WindGustDir)  
summary(WindDir9am)   
summary(WindDir3pm)  
summary(RainToday)


#e. Document any pre-pocessing of the data required based on your exploration
#Omit Day, Month, Year
WAUS$Day=NULL
WAUS$Month=NULL
WAUS$Year=NULL

# Remove NA values
WAUS.complete = WAUS[complete.cases(WAUS), ]

# Change Location to factor
WAUS.complete$Location = as.factor(WAUS.complete$Location)

# Print the structure of class
str(WAUS.complete)



#----------------------------------------------------------------------------------------------------------------------------------#
#Q2. Divide data into 70% training and 30% test set
set.seed(28136179) #Student ID as random seed
train.row = sample(1:nrow(WAUS.complete), 0.7*nrow(WAUS.complete))
WAUS.train = WAUS.complete[train.row,]
WAUS.test = WAUS.complete[-train.row,]


#----------------------------------------------------------------------------------------------------------------------------------#

#Q3. Create classification model using each of the following technique.

#a. Decision Tree
# Fit decision tree model on training set
WAUS.dtree = tree(RainTomorrow ~ ., data = WAUS.train)
print(summary(WAUS.dtree)) 
#Plot the tree
plot(WAUS.dtree)
text(WAUS.dtree, pretty = 0)


#b. Naive Bayes
# Fit Naive Bayes model on training set
WAUS.nbayes = naiveBayes(RainTomorrow ~ ., data = WAUS.train)


#c. Bagging
# Fit bagging model on training set
WAUS.bagging = bagging(RainTomorrow ~ ., data = WAUS.train, mfinal = 5)


#d. Boosting
# Fit boosting model on training set
WAUS.boosting = boosting(RainTomorrow ~ ., data = WAUS.train, mfinal = 10)


#e. Random Forest
# Fit random forests model on training set
WAUS.rforests = randomForest(RainTomorrow ~ ., data = WAUS.train)


#----------------------------------------------------------------------------------------------------------------------------------#


#Q4. Using test data, classify each of the test cases as "will rain tomorrow" or "will not rain tomorrow". 
#Create a confusion matrix and report the accuracy for each model.

#a. Decision Tree
# Predict test set using fitted decision tree model
WAUS.dtree.predict <- predict(WAUS.dtree, WAUS.test, type = "class")
# Tabulate confusion matrix and print
WAUS.dtree.confusion <- confusionMatrix(table(actual_RainTomorrow = WAUS.test$RainTomorrow, predicted_RainTomorrrow = WAUS.dtree.predict))
print(WAUS.dtree.confusion$table)
cat ("Accuracy is :",WAUS.dtree.confusion$overall["Accuracy"])


#b. Naive Bayes
# Predict test set using fitted Naive Bayes model
WAUS.nbayes.predict <- predict(WAUS.nbayes, WAUS.test, type = "class")
# Tabulate confusion matrix and print
WAUS.nbayes.confusion <- confusionMatrix(table(actual_RainTomorrow = WAUS.test$RainTomorrow, predicted_RainTomorrrow = WAUS.nbayes.predict))
print(WAUS.nbayes.confusion$table)
cat ("Accuracy is :",WAUS.nbayes.confusion$overall["Accuracy"])


#c. Bagging
# Predict test set using fitted bagging model
WAUS.bagging.predict <- predict(WAUS.bagging, WAUS.test)
# Tabulate confusion matrix and print
WAUS.bagging.confusion <- confusionMatrix(table(actual_RainTomorrow = WAUS.test$RainTomorrow, predicted_RainTomorrrow = WAUS.bagging.predict$class))
print(WAUS.bagging.confusion$table)
cat ("Accuracy is :",WAUS.bagging.confusion$overall["Accuracy"])
#accuracy is 0.8659    , 5 = 0.8579   15=0.8698  19 = 0.8698  20 = 0.8675 


#d. Boosting
# Predict test set using fitted boosting model
WAUS.boosting.predict <- predict(WAUS.boosting, WAUS.test)
# Tabulate confusion matrix and print
WAUS.boosting.confusion <- confusionMatrix(table(actual_RainTomorrow = WAUS.test$RainTomorrow, predicted_RainTomorrrow = WAUS.boosting.predict$class))
print(WAUS.boosting.confusion$table)
cat ("Accuracy is :",WAUS.boosting.confusion$overall["Accuracy"])


#e. Random Forest
# Predict test set using fitted random forests model
WAUS.rforests.predict <- predict(WAUS.rforests, WAUS.test, type = "class")
# Tabulate confusion matrix and print
WAUS.rforests.confusion <- confusionMatrix(table(actual = WAUS.test$RainTomorrow, predicted_RainTomorrrow = WAUS.rforests.predict))
print(WAUS.rforests.confusion$table)
cat ("Accuracy is :",WAUS.rforests.confusion$overall["Accuracy"])

#----------------------------------------------------------------------------------------------------------------------------------#


#Q5.Using the test data, calculate the confidence of predicting ‘will rain tomorrow’ for each case and construct an ROC curve for each classifier. You should be able to plot all the curves on the same axis. 
#Use a different colour for each classifier. Calculate the AUC for each classifier.

graphNames <- c("Decision Tree","Naive Bayes","Bagging","Boosting","Random Forest")
graphCol <- c("black","blueviolet","red","darkgreen","blue")

#a. Decision Tree
# The Confidence Level
WAUS.dtree.predict.confidence = predict(WAUS.dtree, WAUS.test, type = "vector")
# ROC curve plotting
WAUS.dtree.prediction = prediction(WAUS.dtree.predict.confidence[,2], WAUS.test$RainTomorrow)
ROC.dtree.plot = performance(WAUS.dtree.prediction,"tpr","fpr")
plot(ROC.dtree.plot,col = graphCol[1])
abline(0,1)
# AUC calculating
AUC.dtree = performance(WAUS.dtree.prediction,"auc")
cat("AUC is ",as.numeric(AUC.dtree@y.values))

#b. Naive Bayes
# The Confidence Level
WAUS.nbayes.predict.confidence <- predict(WAUS.nbayes, WAUS.test, type = "raw")
# ROC curve plotting
WAUS.nbayes.prediction <- prediction( WAUS.nbayes.predict.confidence[,2], WAUS.test$RainTomorrow)
ROC.nbayes.plot <- performance(WAUS.nbayes.prediction,"tpr","fpr")
plot(ROC.nbayes.plot, add=TRUE, col = graphCol[2])
# AUC calculating
AUC.nbayes = performance(WAUS.nbayes.prediction,"auc")
cat("AUC is ",as.numeric(AUC.nbayes@y.values))


#c. Bagging
# ROC curve plotting
WAUS.bagging.prediction <- prediction(WAUS.bagging.predict$prob[,2], WAUS.test$RainTomorrow)
ROC.bagging.plot <- performance(WAUS.bagging.prediction,"tpr","fpr")
plot(ROC.bagging.plot, add=TRUE, col = graphCol[3])
# AUC calculating
AUC.bagging = performance(WAUS.bagging.prediction,"auc")
cat("AUC is ",as.numeric(AUC.bagging@y.values))


#d. Boosting
# ROC curve plotting
WAUS.boosting.prediction <- prediction( WAUS.boosting.predict$prob[,2], WAUS.test$RainTomorrow)
ROC.boosting.plot <- performance(WAUS.boosting.prediction,"tpr","fpr")
plot(ROC.boosting.plot, add=TRUE, col = graphCol[4])
# AUC calculating
AUC.boosting = performance(WAUS.boosting.prediction,"auc")
cat("AUC is ",as.numeric(AUC.boosting@y.values))


#e. Random Forest
# The Confidence Level
WAUS.rforests.predict.confidence <- predict(WAUS.rforests, WAUS.test, type="prob")
# ROC curve plotting
WAUS.rforests.prediction <- prediction(WAUS.rforests.predict.confidence[,2], WAUS.test$RainTomorrow)
ROC.rforests.plot <- performance(WAUS.rforests.prediction,"tpr","fpr")
plot(ROC.rforests.plot, add=TRUE, col = graphCol[5])
# AUC calculating
AUC.rforests = performance(WAUS.rforests.prediction,"auc")
cat("AUC is ",as.numeric(AUC.rforests@y.values))

# Add the legend
legend(x=0.5,y=0.5,graphNames,fill=graphCol)


#----------------------------------------------------------------------------------------------------------------------------------#


#Q6 Create a table comparing the results in 4 and 5 for all classifier.
# Is there a single "best" classifer?

# Contruct the table and show
name = c("Model","Accuracy","AUC")
dtree.table = c("Decision Tree",unname(WAUS.dtree.confusion$overall["Accuracy"]), as.numeric(AUC.dtree@y.values))
nbayes.table = c("Naive Bayes",unname(WAUS.nbayes.confusion$overall["Accuracy"]), as.numeric(AUC.nbayes@y.values))
bagging.table = c("Bagging",unname(WAUS.bagging.confusion$overall["Accuracy"]), as.numeric(AUC.bagging@y.values))
boosting.table = c("Boosting",unname(WAUS.boosting.confusion$overall["Accuracy"]), as.numeric(AUC.boosting@y.values))
rforests.table = c("Random Forest",unname(WAUS.rforests.confusion$overall["Accuracy"]), as.numeric(AUC.rforests@y.values))

showTable = as.table(rbind(name,dtree.table,nbayes.table,bagging.table,boosting.table,rforests.table))
showTable = unname(showTable)
show(showTable)

#----------------------------------------------------------------------------------------------------------------------------------#


#Q7. Examining each of the models, determine the most important variables in predicting whether or not it will rain tomorrow. 
#Which variables could be omitted from the data with very little effect on performance? Give reasons
cat("\n#Decision Tree Attribute Importance\n")
summary(WAUS.dtree)


cat("\n#Naive Bayes attribute importance\n", 
    "\nNaive Bayes does not provide attribute importance information.\n")

cat("\n#Bagging Attribute Importance\n")
print(sort(WAUS.bagging$importance,decreasing=TRUE))

cat("\n#Boosting Attribute Importance\n")
print(sort(WAUS.boosting$importance,decreasing=TRUE))


cat("\n#Random Forest Attribute Importance\n")
print(WAUS.rforests$importance)

# important: humidity3pm, sunshine, pressure3pm, windgustspeed, (windgustdir)
#omit : raintoday
#----------------------------------------------------------------------------------------------------------------------------------#



#Q8. By experimenting with parameter settings for at least one of the classifiers, create the best classifier you can – 
#that is, one with an accuracy greater than the models you originally created in Part 3.
# Demonstrate this improved accuracy using ROC, AUC, or other accuracy measures. 
#Report the parameter settings and assumptions made in designing this classifier

# Decision tree: Change parameter, not include rain today
# For Decision tree, we try to prune and use k-fold cross validaiton. However, the performance was worst than default.
# Try to use cross validation and prune the tree
WAUS.dtree2= cv.tree(WAUS.dtree,FUN=prune.misclass)
WAUS.dtree.prune = prune.misclass(WAUS.dtree,best=4)
WAUS.dtree.prune.predict = predict(WAUS.dtree.prune,WAUS.test,type="class")
WAUS.dtree.prune.confusion <- confusionMatrix(table(actual = WAUS.test$RainTomorrow, predicted = WAUS.dtree.prune.predict))

WAUS.dtree.prune.confidence = predict(WAUS.dtree.prune,WAUS.test,type="vector")
WAUS.dtree.prune.prediction = prediction(WAUS.dtree.prune.confidence[,2],WAUS.test$RainTomorrow)
WAUS.dtree.prune.auc = performance(WAUS.dtree.prune.prediction,"auc")
# Show th accuracy and AUC values for pruned tree
print(WAUS.dtree.prune.confusion)
print(as.numeric(WAUS.dtree.prune.auc@y.values))

#Default tree value
print(as.numeric(AUC.dtree@y.values))
print(WAUS.dtree.confusion)
print(summary(WAUS.dtree)) 



# For Naive Bayes, no improvement for the model


# For Bagging
# After tested with several values, change the value of mfinal to 15
# Fit the bagging model and construct confusion matrix
WAUS.bagging2 = bagging(RainTomorrow ~ ., data = WAUS.train, mfinal = 15)
WAUS.bagging.predict2 <- predict(WAUS.bagging2, WAUS.test)
# Tabulate confusion matrix and print
WAUS.bagging.confusion2 <- confusionMatrix(table(actual = WAUS.test$RainTomorrow, predicted = WAUS.bagging.predict2$class))
print(WAUS.bagging.confusion2)


WAUS.bagging.prediction2 <- prediction(WAUS.bagging.predict2$prob[,2], WAUS.test$RainTomorrow)
ROC.bagging.plot2 <- performance(WAUS.bagging.prediction2,"tpr","fpr")
plot(ROC.bagging.plot2, add=TRUE, col = "red")
# AUC calculating
AUC.bagging2 = performance(WAUS.bagging.prediction2,"auc")

print(as.numeric(AUC.bagging2@y.values))


# For Random Forest, change the ntree and mtry
WAUS.rforests2 = randomForest(RainTomorrow~.,data = WAUS.train,ntree = 2000,mtry = 5)
# Predict test set using fitted random forests model
WAUS.rforests.predict2<- predict(WAUS.rforests2, WAUS.test, type = "class")
# Tabulate confusion matrix and print
WAUS.rforests.confusion2 <- confusionMatrix(table(actual = WAUS.test$RainTomorrow, predicted = WAUS.rforests.predict2))
print(WAUS.rforests.confusion2)


WAUS.rforests.predict.confidence2 <- predict(WAUS.rforests2, WAUS.test, type="prob")
# ROC curve plotting
WAUS.rforests.prediction2 <- prediction(WAUS.rforests.predict.confidence2[,2], WAUS.test$RainTomorrow)
ROC.rforests.plot2 <- performance(WAUS.rforests.prediction2,"tpr","fpr")
plot(ROC.rforests.plot2, add=TRUE, col = "blue")
# AUC calculating
AUC.rforests2 = performance(WAUS.rforests.prediction2,"auc")
print(as.numeric(AUC.rforests2@y.values))

#----------------------------------------------------------------------------------------------------------------------------------#


#Q9. Implement an Artificial Neural Network classifier and report its performance. 
#Comment on attributes used and your data pre-processing required. 
#How does this classifier compare with the others? Can you give any reasons?

#Pre-processing the data
# change category data to binary
WAUS.train$RainToday <- as.numeric(WAUS.train$RainToday == "Yes")
WAUS.train$RainTomorrow <- as.numeric(WAUS.train$RainTomorrow == "Yes")
WAUS.test$RainToday <- as.numeric(WAUS.test$RainToday == "Yes")
WAUS.test$RainTomorrow <- as.numeric(WAUS.test$RainTomorrow == "Yes")

train.tmpMatrix <- model.matrix( ~ WindGustDir + WindDir9am + WindDir3pm, data=WAUS.train)
summary(train.tmpMatrix)
train.tmpMatrix = train.tmpMatrix[,c(2:45)]
test.tmpMatrix <- model.matrix( ~ WindGustDir + WindDir9am + WindDir3pm, data=WAUS.test)
test.tmpMatrix = test.tmpMatrix[,c(2:45)]

# Bind the data
WAUS.train$WindGustDir= NULL
WAUS.train$WindDir9am= NULL
WAUS.train$WindDir3pm= NULL
WAUS.test$WindGustDir= NULL
WAUS.test$WindDir9am= NULL
WAUS.test$WindDir3pm= NULL

WAUS.train = cbind(WAUS.train,train.tmpMatrix)
WAUS.test = cbind(WAUS.test,test.tmpMatrix)

# Change int class to numeric class
WAUS.train$Location = as.numeric(WAUS.train$Location)
WAUS.test$Location = as.numeric(WAUS.test$Location)
WAUS.train$WindGustSpeed = as.numeric(WAUS.train$WindGustSpeed)
WAUS.test$WindGustSpeed = as.numeric(WAUS.test$WindGustSpeed)
WAUS.train$WindSpeed9am = as.numeric(WAUS.train$WindSpeed9am)
WAUS.test$WindSpeed9am = as.numeric(WAUS.test$WindSpeed9am)
WAUS.train$WindSpeed3pm = as.numeric(WAUS.train$WindSpeed3pm)
WAUS.test$WindSpeed3pm = as.numeric(WAUS.test$WindSpeed3pm)
WAUS.train$Humidity9am = as.numeric(WAUS.train$Humidity9am)
WAUS.test$Humidity9am = as.numeric(WAUS.test$Humidity9am)
WAUS.train$Humidity3pm = as.numeric(WAUS.train$Humidity3pm)
WAUS.test$Humidity3pm = as.numeric(WAUS.test$Humidity3pm)
WAUS.train$Cloud9am = as.numeric(WAUS.train$Cloud9am)
WAUS.test$Cloud9am = as.numeric(WAUS.test$Cloud9am)
WAUS.train$Cloud3pm = as.numeric(WAUS.train$Cloud3pm)
WAUS.test$Cloud3pm = as.numeric(WAUS.test$Cloud3pm)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

WAUS.train <- as.data.frame(lapply(WAUS.train, normalize))
WAUS.test <- as.data.frame(lapply(WAUS.test, normalize))

# Fit ANN model
WAUS.ann = neuralnet(RainTomorrow ~ Humidity3pm+Sunshine+WindGustDirENE+WindGustDirESE+WindGustDirN+WindGustDirNE+WindGustDirNNE+WindGustDirNNW+WindGustDirNW+WindGustDirS+WindGustDirSE+WindGustDirSSE+WindGustDirSSW+WindGustDirSW+WindGustDirW+WindGustDirWNW +WindGustDirWSW , WAUS.train, hidden=1,stepmax=1e6) #0.8563

summary(WAUS.ann)
plot(WAUS.ann,rep="best")


# Predict the test data

WAUS.ann.predict = predict(WAUS.ann,WAUS.test[c(11,6,20:34)])


WAUS.ann.predict <- as.numeric(WAUS.ann.predict[ ,1] > 0.5)


# Construct the confusion matrix
WAUS.ann.confusion <- confusionMatrix(table(actual = WAUS.test$RainTomorrow, predicted = WAUS.ann.predict))
print(WAUS.ann.confusion)


#----------------------------------------------------------------------------------------------------------------------------------#


#Q10. Write a brief report (4 pages max) summarizing your results in parts 1 – 9. Use commenting (# ----) in your R script.
#Alternatively combine working, comments and reporting in R Markdown.






