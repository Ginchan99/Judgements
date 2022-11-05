rm(list = ls())
library("dplyr")
library("magrittr")
library("ggplot2")

library("caTools")
library("rpart")
library("rpart.plot")
library("ROCR")
library("randomForest")
library("caret")
library("e1071")

#install.packages("dplyr")
#install.packages("magrittr")
#install.packages("ggplot2")
#install.packages("caTools")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("ROCR")
#install.packages("randomForest")
#install.packages("caret")
#install.packages("e1071")

update.packages(checkBuilt = TRUE)
judge = read.csv("judgements.csv", stringsAsFactors = TRUE)
summary(judge)

judge_main = judge[-c(1,2)]#removing 1st & 2nd cols

#logistic regression
model_logreg = glm(Reverse ~ . , data = judge_main,family = binomial)
summary(model_logreg)

#create a tree model
#create a split with 80% of data
set.seed(7)
split1 = sample.split(judge_main$Reverse,SplitRatio = 0.8)
train = subset(judge_main,split1 == TRUE)
test = subset(judge_main,split1 == FALSE)

#check logistic model for train dataset
modelTrain_logreg = glm(Reverse ~ . , data = train,family = binomial)
summary(modelTrain_logreg)

preds = predict(modelTrain_logreg,type = "response",newdata = test)
truthTable = table(test$Reverse,preds>0.5)
#a = set of accurate data and b is set of all data

#Very Very important
LM_AutomaticAccuracy = (truthTable[1,1]+truthTable[2,2])/(truthTable[1,1]+truthTable[1,2]+truthTable[2,1]+truthTable[2,2])

#false positive is the proportion of all negatives that still yield positive test outcomes
LM_FalsePositive = truthTable[1,2]/(truthTable[1,2]+truthTable[1,1])
#false positive is the proportion of all positives that still yield negative test outcomes
LM_FalseNegative = truthTable[2,1]/(truthTable[2,1]+truthTable[2,2])

#ALWAYS ADD BY ROWS
#senstivity -> Sensitivity(true positive rate)
#refers to the probability of a +ve test, conditioned as truly being +ve
LM_Sensitivity = truthTable[2,2]/(truthTable[1,2]+truthTable[2,2])
LM_TRUEPOSITIVE = truthTable[2,2]/(truthTable[1,2]+truthTable[2,2])


#specificity -> Specificity(true -ve rates)
#refers to the probability of a -ve test, conditioned as truly being -ve
LM_Specificity = truthTable[1,1]/(truthTable[1,1]+truthTable[1,2])
LM_TRUENEGATIVE = truthTable[1,1]/(truthTable[1,1]+truthTable[1,2])

#all the models are conditioned on real world

#FINALLY TREE MODEL
#CART
treeModel = rpart(Reverse ~ ., data = train,method = 'class',minbucket=5)
#method - class --- this gives us a classification tree, minbucket = min size of buckets
#visualize the tree
prp(treeModel)

# predict
predsoftree = predict(treeModel,newdata = test, type = "class")
treeTruthtable = table(test$Reverse,predsoftree==2)


treeAccuracy = (treeTruthTable[1,1]+treeTruthTable[2,2])/(treeTruthTable[1,1]+treeTruthTable[1,2]+treeTruthTable[2,1]+treeTruthTable[2,2])
treeTP = (treeTruthTable[2,2])/(treeTruthTable[2,2]+treeTruthTable[2,1])
treeTN = (treeTruthTable[1,1])/(treeTruthTable[1,1]+treeTruthTable[1,2])
treeFP = (treeTruthTable[1,2])/(treeTruthTable[1,1]+treeTruthTable[1,2])
treeFN = (treeTruthTable[2,1])/(treeTruthTable[2,2]+treeTruthTable[2,1])

# Get the prediction curve
# ROC curve = characteristic curve (operator characteristic)
predsOfTreeROC = predict(treeModel, newdata = test)


curve = prediction(predsOfTreeROC[,2], test$Reverse)
# performanceCurve = performance(curve,"tru_positive_rate","false_positive_rate")
performanceCurve = performance(curve,"tpr","fpr")
plot(performanceCurve)

#area under the curve
auc <- as.numeric(performance(curve, "auc")@y.values)

performanceCurve2 = performance(curve,"fnr","fpr")
plot(performanceCurve2)

# Random Forest
#prelimnary
test$Reverse = as.factor(test$Reverse)
train$Reverse = as.factor(train$Reverse)
set.seed(5)

modelRandomForest = randomForest(Reverse ~.,data=train, ntree=100, nodesize=5)
randomForestPreds = predict(modelRandomForest, newdata = test)

rfTable = table(test$Reverse, randomForestPreds)

rfAccuracy = (rfTable[1,1]+rfTable[2,2])/(rfTable[1,1]+rfTable[1,2]+rfTable[2,1]+rfTable[2,2])
rfTP = (rfTable[2,2])/(rfTable[2,2]+rfTable[2,1])
rfTN = (rfTable[1,1])/(rfTable[1,1]+rfTable[1,2])
rfFP = (rfTable[1,2])/(rfTable[1,1]+rfTable[1,2])
rfFN = (rfTable[2,1])/(rfTable[2,2]+rfTable[2,1])












