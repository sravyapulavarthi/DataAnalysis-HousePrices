rm(list = ls())
cat('\014')
setwd('C:\\Users\\sravy\\Desktop\\Data Mining and Business Analytics in Knowledge Services\\Major Project')

library(corrplot)
library(leaps)
library(glmnet)
library(Metrics)

FinalTrain <- readRDS(file = 'FinalTrain.rds')
names(FinalTrain)

# Index vector numeric variables
numericVars <- which(sapply(FinalTrain, is.numeric)) 
# Index vector factor variables
factorVars <- which(sapply(FinalTrain, is.factor)) 
cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 'categoric variables')

# Correlations of all numeric variables are plotted below
all_numVar <- FinalTrain[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") 
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)

# SalePrice ~ MSSubClass + LotArea +LandSlope + OverallQual + YearBuilt + RoofMatl + MasVnrType + MasVnrArea + ExterQual + BsmtQual + X1stFlrSF + X2ndFlrSF + FullBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + Fireplaces + GarageType + GarageArea + GarageCars + OverallCond, data = FinalTrain

# BSS cannot be applied our data since there are many levels for each of the qualitative predictors

# Forward Stepwise Selection
regfit.fwd <- regsubsets(SalePrice ~ ., data = FinalTrain, method = 'forward')
summary.fwd <- summary(regfit.fwd)

regfit.bkw <- regsubsets(SalePrice ~ ., data = FinalTrain, method = 'forward')
summary.bkw <- summary(regfit.bkw)

# Plotting RSS, adjusted R2, Cp, and BIC for all of the models at once will help us decide which model to select

par(mfrow=c(2,2))
plot(summary.fwd$rss , xlab = "Number of Variables", ylab = "RSS", type = 'l')
plot(summary.fwd$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
points (which.max(summary.fwd$adjr2),summary.fwd$adjr2[which.max(summary.fwd$adjr2)], col="red",cex=2,pch=20)


plot(summary.fwd$cp , xlab = "Number of Variables", ylab = "Cp", type = 'l')
points(which.min(summary.fwd$cp), summary.fwd$cp[which.min(summary.fwd$cp)], col="red",cex=2,pch=20)
plot(summary.fwd$bic , xlab = "Number of Variables", ylab = "BIC", type = 'l')
points(which.min(summary.fwd$bic),summary.fwd$bic[which.min(summary.fwd$bic)], col="red",cex=2,pch =20)

# Backward Stepwise Selection
regfit.bkw <- regsubsets(SalePrice ~ ., data = FinalTrain, method = 'forward')
summary.bkw <- summary(regfit.bkw)

# Plotting RSS, adjusted R2, Cp, and BIC for all of the models at once will help us decide which model to select
par(mfrow=c(2,2))
plot(summary.bkw$rss , xlab = "Number of Variables", ylab = "RSS", type = 'l')
plot(summary.bkw$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
points (which.max(summary.bkw$adjr2),summary.bkw$adjr2[which.max(summary.bkw$adjr2)], col="red",cex=2,pch=20)


plot(summary.bkw$cp , xlab = "Number of Variables", ylab = "Cp", type = 'l')
points(which.min(summary.bkw$cp), summary.bkw$cp[which.min(summary.bkw$cp)], col="red",cex=2,pch=20)
plot(summary.bkw$bic , xlab = "Number of Variables", ylab = "BIC", type = 'l')
points(which.min(summary.bkw$bic),summary.bkw$bic[which.min(summary.bkw$bic)], col="red",cex=2,pch =20)


# Ridge Regression and the Lasso ####
rm(list = ls())
cat('\014')
FinalTrain <- readRDS(file = 'FinalTrain.rds')
names(FinalTrain)
par(mfrow = c(1,1))

# We generate a matrix corresponding to the 22 predictors and also transform qualitative variables into dummy variables, since glmnet() can only take numerical, quantitative inputs
x <- model.matrix(SalePrice ~ ., FinalTrain)[, -1]
y <- FinalTrain$SalePrice

grid <- 10 ^ seq(10, -2, length = 100)

# Ridge Regression ####
ridge.mod <- glmnet(x, y, alpha=0, lambda = grid)
dim(coef(ridge.mod))

# 84 100
# In this case, it is a 84x100 matrix, with 84 rows and 100 columns (one for each value of lambda)

# To choose the best value of the tuning parameter lambda, we use cross-validation and choose the model with the lowest cv error

set.seed(1)
cv.out <- cv.glmnet(x, y, alpha = 0)
plot(cv.out)
title('CV Results for Ridge Regression', line = 3)
bestlam <- cv.out$lambda.min
bestlam 
# The best value of the tuning parameter lambda according to CV is 8388.824

# The FinalTraining error associated with this value of lambda is computed as:
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x)
mean((ridge.pred - y)^2)
# 887416149

# The coefficients estimates associated with this value of lambda is computed as:
predict(ridge.mod, type = "coefficients", s = bestlam)[1:20,]


# Test Error Ridge ####
set.seed(1)
train <- sample(1:nrow(x), 0.7*nrow(x)) 
test=(-train) 
y.test=y[test]

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
# 46185.8

ridge.mod <- glmnet(x[train,], y[train], alpha=0, lambda=grid)
ridge.pred <- predict(ridge.mod, s=bestlam, newx=x[test ,])
mean((ridge.pred -y.test)^2)
# 810823946


# CV Error Ridge ####
rm(list = ls())
cat('\014')
FinalTrain <- readRDS(file = 'FinalTrain.rds')
names(FinalTrain)
par(mfrow = c(1,1))


grid <- 10 ^ seq(10, -2, length = 100)

set.seed(1)
folddistri <- sample(1:10, nrow(FinalTrain), replace = T)
FinalTrain$folddistri <- folddistri
cv.tester <- vector()

CVTestError <- rep(NA, 10)
for(i in 1:10){
  x <- model.matrix(SalePrice ~ ., FinalTrain)[which(FinalTrain$folddistri != i), -1]
  y <- FinalTrain$SalePrice[which(FinalTrain$folddistri != i)]
  tst <- model.matrix(SalePrice ~ ., FinalTrain)[which(FinalTrain$folddistri == i), -1]
  y.test <- FinalTrain$SalePrice[which(FinalTrain$folddistri == i)]
  set.seed(1)
  cv.out=cv.glmnet(x, y, alpha=0)
  bestlam = cv.out$lambda.min
  
  ridge.mod <- glmnet(x, y, alpha=0, lambda=grid)
  ridge.pred <- predict(ridge.mod, s=bestlam, newx=tst)
  cv.tester[i] <- mean((ridge.pred -y.test)^2)
  print(i)
  print(cv.tester[i])
  }

cv.tester <- mean(cv.tester)
cv.tester
# 1185049832


# The Lasso ####
lasso.mod <- glmnet(x, y, alpha=1, lambda = grid)
dim(coef(lasso.mod))
# 84 100
# In this case, it is a 84x100 matrix, with 84 rows and 100 columns (one for each value of lambda)


set.seed(1)
cv.out <- cv.glmnet(x, y, alpha = 1)
plot(cv.out)
title('CV Results for Lasso', line = 3)
bestlam <- cv.out$lambda.min
bestlam 
# The best value of the tuning parameter lambda according to CV is 664.8

# The FinalTraining error associated with this value of lambda is computed as:
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x)
mean((lasso.pred - y)^2)
# 901835183

# The coefficients estimates associated with this value of lambda is computed as:
predict(lasso.mod, type = "coefficients", s = bestlam)[1:20,]


# Test Error Lasso ####
set.seed(1)
train <- sample(1:nrow(x), 0.7*nrow(x)) 
test=(-train) 
y.test=y[test]

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
# 5827.987

ridge.mod <- glmnet(x[train,], y[train], alpha=1, lambda=grid)
ridge.pred <- predict(ridge.mod, s=bestlam, newx=x[test ,])
mean((ridge.pred -y.test)^2)
# 957045433



# CV Error Lasso ####
rm(list = ls())
cat('\014')
FinalTrain <- readRDS(file = 'FinalTrain.rds')
names(FinalTrain)
par(mfrow = c(1,1))


grid <- 10 ^ seq(10, -2, length = 100)

set.seed(1)
folddistri <- sample(1:10, nrow(FinalTrain), replace = T)
FinalTrain$folddistri <- folddistri
cv.tester <- vector()

CVTestError <- rep(NA, 10)
for(i in 1:10){
  x <- model.matrix(SalePrice ~ ., FinalTrain)[which(FinalTrain$folddistri != i), -1]
  y <- FinalTrain$SalePrice[which(FinalTrain$folddistri != i)]
  tst <- model.matrix(SalePrice ~ ., FinalTrain)[which(FinalTrain$folddistri == i), -1]
  y.test <- FinalTrain$SalePrice[which(FinalTrain$folddistri == i)]
  set.seed(1)
  cv.out=cv.glmnet(x, y, alpha=1)
  bestlam = cv.out$lambda.min
  
  ridge.mod <- glmnet(x, y, alpha=1, lambda=grid)
  ridge.pred <- predict(ridge.mod, s=bestlam, newx=tst)
  cv.tester[i] <- mean((ridge.pred -y.test)^2)
  print(i)
  print(cv.tester[i])
}

cv.tester <- mean(cv.tester)
cv.tester
# 1172463095