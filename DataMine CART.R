install.packages("pacman")
pacman::p_load(ISLR)
pacman::p_load(rpart)
pacman::p_load(caTools)
pacman::p_load(rpart.plot)
pacman::p_load(tidyverse)
pacman::p_load(FSelector) #Need java
df <- ISLR::College
#Q1
#seed and split
set.seed(5)
sample.df <- sample.split(df$Private, SplitRatio = 0.7)
train.df <- subset(df, sample.df == TRUE)
test.df <- subset(df, sample.df == FALSE)
## Q2
##Max depth model
treeMax <- rpart(Private ~ ., train.df, method = "class", cp = -1)
prp(treeMax)
printcp(treeMax)
####Testing something
cp_df <- as.data.frame(printcp(treeMax))
cp_df$ErrorRule <- ifelse(cp_df$xerror > cp_df$xstd + cp_df$rel error, 1, 0)
cp_df
#####
###Prune back to best tree to optimal complexity parameter where min(xerror)
treeOpti <- rpart(Private ~ ., train.df, method = "class", cp = 0)  ###it doesnt look like it matters if we go with cp = 0 or -1 for this data
prp(treeOpti)
printcp(treeOpti)
predictOpti <- predict(treeOpti, test.df, type = "class")
caret::confusionMatrix(predictOpti, test.df$Private, positive = "Yes")
##Q2
##Automated method
treeAuto <- rpart(Private ~ ., train.df, method = "class")
prp(treeAuto, type = 1, extra = 101, under = TRUE, split.font = 2, varlen = -15, main= "Automated")
printcp(treeAuto)
predict.Auto <- predict(treeAuto, test.df, type = 'class')
caret::confusionMatrix(predict.Auto, test.df$Private, positive = "Yes")
##Q3
## Entropy Full Model
treefull.Entropy <- rpart(Private ~ ., train.df, method = "class", cp = -1, parms = list(split = "information"))
prp(treefull.Entropy)
printcp(treefull.Entropy)
### After prune
treefull.Entropy.Prune <- rpart(Private ~ ., train.df, method = "class", cp = 0.020270, parms = list(split = "information"))
prp(treefull.Entropy.Prune)
printcp(treefull.Entropy.Prune)
predict.Entopy <- predict(treefull.Entropy.Prune, test.df, type = "class")
caret::confusionMatrix(predict.Entopy, test.df$Private, positive = "Yes")
##Auto Entropy
tree.EntropyAuto <- rpart(Private ~ ., train.df, method = "class", parms = list(split = "information"))
prp(tree.EntropyAuto, type = 1, extra = 101, under = TRUE, split.font = 2, varlen = -15, main= "Automated Entropy")
printcp(tree.EntropyAuto)
predict.tree.EntropyAuto <- predict(tree.EntropyAuto, test.df, type = 'class')
caret::confusionMatrix(predict.tree.EntropyAuto, test.df$Private, positive = "Yes")
##Q4
## Rando ForrForr
pacman::p_load("randomForest")
rf.df <- randomForest(Private ~ ., train.df, importance = TRUE)
#look at confusion matrix on training data just to get an idea of what is going on
#we REALLY look at confusion matrix on the test data for analysis purposes
rf.df$confusion
#importance based on gini index
rf.df$importance
#or can pull individual columns (1= last column, 2 = 2nd to last column)
importance(rf.df, type = 1)
#predictions
rf.pred <- predict(rf.df, test.df)
table(rf.pred, test.df$Private)
caret::confusionMatrix(rf.pred, test.df$Private)
#To rankorder the importance
rankorder_rf <- as.data.frame(rf.df$importance)
rankorder <- data.frame(Names = rownames(rankorder_rf), GiniImp = rankorder_rf$MeanDecreaseGini)
rankorder[order(rankorder$GiniImp, decreasing = T), ]
###For the other models
rankorder_other <- as.data.frame(rf.df$importance)
rankorder <- data.frame(Names = rownames(rankorder_rf), GiniImp = rankorder_rf$MeanDecreaseGini)
rankorder[order(rankorder$GiniImp, decreasing = T), ]
