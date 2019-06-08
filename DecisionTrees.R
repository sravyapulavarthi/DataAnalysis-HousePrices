rm(list = ls())
cat('\014')
setwd('C:\\Users\\sravy\\Desktop\\Data Mining and Business Analytics in Knowledge Services\\Major Project')

library(tree)
library(randomForest)
library(gbm)

# Decision Tree ####

FinalTrain <- readRDS(file = 'FinalTrain.rds')
names(FinalTrain)

# Train the tree on the entire dataset

tree.NewTrain <- tree(SalePrice ~ ., data = FinalTrain)
summary(tree.NewTrain)

plot(tree.NewTrain)
text(tree.NewTrain, pretty = 0)
title('Decision Tree with Significant Predictors')

# To check if pruning the tree will improve the performance
cv.price <- cv.tree(tree.NewTrain)
plot(cv.price$size, cv.price$dev, type = 'b', xlab = 'Tree Size', ylab = 'CV Deviance')
title('CV error for train on all rows')

# We choose the tree with 4 terminal nodes because the error is within 1 standard deviation

prune.tree <- prune.tree(tree.NewTrain, best = 8)
plot(prune.tree)
text(prune.tree, pretty = 0) 
title('Pruned Decision Tree on Significant Predictors')


yhat.pru <- predict(prune.tree, newdata = FinalTrain)
plot(yhat.pru, FinalTrain$SalePrice)
abline(0,1)
mean((yhat.pru - FinalTrain$SalePrice)^2) 

# 1,793,892,669

set.seed(1)
folddistri <- sample(1:10, nrow(FinalTrain), replace = T)
FinalTrain$folddistri <- folddistri

CVTestError <- rep(NA, 10)
for(i in 1:10){
  tree.train <- tree(SalePrice ~ . - folddistri, data = FinalTrain[which(FinalTrain$folddistri != i),])
  prunetree <- prune.tree(tree.train, best = 8)
  cvtestpred <- predict(prunetree, newdata = FinalTrain[which(FinalTrain$folddistri == i),])
  CVTestError[i] <- mean((FinalTrain$SalePrice[which(FinalTrain$folddistri == i)] - cvtestpred)^2)  
}
mean(CVTestError)
# 2,414,375,872

set.seed(1)
train <- sample(nrow(FinalTrain), 0.7 * nrow(FinalTrain))
val.train <- FinalTrain[train, ]
val.test <- FinalTrain[-train, ]

tree.val <- tree(SalePrice ~ ., data = val.train)
summary(tree.val)
plot(tree.val)
text(tree.val, pretty = 0)
title("Decision Tree on Training Set")

cv.price <- cv.tree(tree.NewTrain)
plot(cv.price$size, cv.price$dev, type = 'b', xlab = 'Tree Size', ylab = 'CV Deviance')
title('CV error for train on all rows')

prune.tree <- prune.tree(tree.val, best = 4)
plot(prune.tree)
text(prune.tree, pretty = 0) 
title('Pruned Decision Tree on Significant Predictors')


yhat.pru <- predict(prune.tree, newdata = val.test)
plot(yhat.pru, val.test$SalePrice)
abline(0,1)
mean((yhat.pru - val.test$SalePrice)^2) 

# 2,165,425,664

# Bagging ####

rm(list = ls())
cat('\014')
FinalTrain <- readRDS(file = 'FinalTrain.rds')
names(FinalTrain)

set.seed(1)
bag.FinalTrain <- randomForest(SalePrice ~ ., data = FinalTrain, mtry = 21, importance = TRUE)
bag.FinalTrain

# Number of trees grown = 500

yhat.bag <- predict(bag.FinalTrain, newdata = FinalTrain)
plot(yhat.bag, FinalTrain$SalePrice)
abline(0, 1)
mean((yhat.bag - FinalTrain$SalePrice)^2)

# 171,024,871

# Now we try growing only 25 trees
bag.FinalTrain <- randomForest(SalePrice ~ ., data = FinalTrain, mtry = 21, ntree = 25)
yhat.bag <- predict(bag.FinalTrain, newdata = FinalTrain)
mean((yhat.bag - FinalTrain$SalePrice)^2) 

# 198,155,504


# Validation

set.seed(1)
train <- sample(nrow(FinalTrain), 0.7 * nrow(FinalTrain))
val.train <- FinalTrain[train, ]
val.test <- FinalTrain[-train, ]

bag.FinalTrain <- randomForest(SalePrice ~ ., data = val.train, mtry = 21, importance = TRUE)
bag.FinalTrain

# Number of trees grown = 500

yhat.bag <- predict(bag.FinalTrain, newdata = val.test)
plot(yhat.bag, val.test$SalePrice)
abline(0, 1)
mean((yhat.bag - val.test$SalePrice)^2)

# 787,851,033

# Random Forests ####
set.seed(1)
rf.FinalTrain <- randomForest(SalePrice ~ ., data = FinalTrain, importance = T)
yhat.bag <- predict(rf.FinalTrain, newdata = FinalTrain)
mean((yhat.bag - FinalTrain$SalePrice)^2) 

# 186,230,644

importance(rf.FinalTrain)

# Two measures of variable importance are reported. The former is based upon the mean decrease of accuracy in predictions on the out of bag samples when a given variable is excluded from the model. The latter is a measure of the total decrease in node impurity that results from splits over that variable, averaged over all trees. 

# In the case of regression trees, the node impurity is measured by the training RSS, and for classiﬁcation trees by the deviance.

varImpPlot(rf.FinalTrain)

# The results indicate that across all of the trees considered in the random forest, the over all quality (OverallQual), the total square feet (1stFlrSF & 2ndFlrSF), and the type of dwelling involved in the sale (MSSubClass) are by far the two most important variables.

# Validation

set.seed(1)
train <- sample(nrow(FinalTrain), 0.7 * nrow(FinalTrain))
val.train <- FinalTrain[train, ]
val.test <- FinalTrain[-train, ]

set.seed(1)
rf.FinalTrain <- randomForest(SalePrice ~ ., data = val.train, importance = T)
yhat.bag <- predict(rf.FinalTrain, newdata = val.test)
mean((yhat.bag - val.test$SalePrice)^2) 

# 680,921,160

importance(rf.FinalTrain)

varImpPlot(rf.FinalTrain)



# Boosting ####
rm(list = ls())
cat('\014')
FinalTrain <- readRDS(file = 'FinalTrain.rds')
names(FinalTrain)

set.seed(1) 
boost.FinalTrain <- gbm(SalePrice ~ ., data = FinalTrain, distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
summary(boost.FinalTrain)

# We see that OverallQual, X1stFlrSF, LotArea, MSSubClass & 2ndFlrSF are by far the most important variables. We can also produce partial dependence plots for these variables. These plots illustrate the marginal eﬀect of the selected variables on the response after integrating out the other variables. 

plot(boost.FinalTrain, i = "OverallQual")
plot(boost.FinalTrain, i = "MSSubClass")
plot(boost.FinalTrain, i = "LotArea")
plot(boost.FinalTrain, i = "X1stFlrSF")
plot(boost.FinalTrain, i = "X2ndFlrSF")

# In this case, the median house prices are increasing with an increase in OverallQual, X1stFlrSF, X2ndFlrSF and LotArea.

yhat.boost <- predict(boost.FinalTrain, newdata = FinalTrain, n.trees = 5000)
mean((yhat.boost - FinalTrain$SalePrice)^2) 
# 6,889,754

# The MSE obtained is 6,889,754 and is similar to the error for random forests and superior to that for bagging.


# Validation 

set.seed(1)
train <- sample(nrow(FinalTrain), 0.7 * nrow(FinalTrain))
val.train <- FinalTrain[train, ]
val.test <- FinalTrain[-train, ]

set.seed(1) 
boost.FinalTrain <- gbm(SalePrice ~ ., data = val.train, distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
yhat.boost <- predict(boost.FinalTrain, newdata = val.test, n.trees = 5000)
mean((yhat.boost - val.test$SalePrice)^2) 
# 1,449,635,414