rm(list = ls())
cat('\014')
setwd('C:\\Users\\sravy\\Desktop\\Data Mining and Business Analytics in Knowledge Services\\Major Project')

library(ggplot2)
library(corrplot)
library(ggrepel)
library(scales)
library(dplyr)

train <- read.csv('train.csv', header = T, stringsAsFactors = F)
test <- read.csv('test.csv', header = T, stringsAsFactors = F)
# We use stringsAsFactors = F because all variables will need string manipulation and feature engineering first

dim(train)
names(train)
str(train)

# We will remove the column ID because no particular purpose
test_labels <- test$Id
test$Id <- NULL
train$Id <- NULL
#colnames(train)

test$SalePrice <- NA
CompleteData <- rbind(train, test)

dim(CompleteData)

# The first column is MSSubClass, which is a factor so converting it into a factor variable
train$MSSubClass <- as.factor(train$MSSubClass)
test$MSSubClass <- as.factor(test$MSSubClass)

# Checking if there is atleast 1 row with all the values present
comp <- complete.cases(train)

# First task will be to remove the columns with majority of the NA values
# Apply the function that checks to see if there is NA in each column of the train data frame
op <- apply(train, 2, function(x) sum(is.na(x)))
plot(op)

# Looking at the data, we will remove all the columns with more than 100 NA values
names(which(op > 100))

col_NA <- colnames(train)[colSums(is.na(train)) > 0]


# Overall missing value treatment
NewTrain <- train[, names(which(op < 100))]

# Next we want to try running multiple linear regression on this data, so we will remove the rows with even one NA value 
# We can use the complete.cases function to this effect

asdf <- complete.cases(NewTrain)
table(asdf)

NewTrain <- NewTrain[complete.cases(NewTrain),]

# Now we will run a linear regression model with SalePrice as the predictor to just get a sense of which predictors are statistically significant
baremodel <- lm(SalePrice ~ ., data = NewTrain)
summary(baremodel)

# Here we can see the variables who are correlated with the SalePrice
# These variables are:
# MSZoninG, LotArea, StreetPave, LandSlope, OverallQual, OverallCond, YearBuilt, RoofMatl, MasVnrArea, ExterQual, BsmtQual, X1stFlrSF, X2ndFlrSF, KitchenQual

# Cannot see all variables
# dividing this into 3 different parts
BareModel1 <- lm(as.formula(paste('SalePrice ~ ', paste(colnames(NewTrain)[1:25], collapse = '+'), sep = '')), data = NewTrain)

BareModel2 <- lm(as.formula(paste('SalePrice ~ ', paste(colnames(NewTrain)[26:50], collapse = '+'), sep = '')), data = NewTrain)

BareModel3 <- lm(as.formula(paste('SalePrice ~ ', paste(colnames(NewTrain)[51:73], collapse = '+'), sep = '')), data = NewTrain)

summary(BareModel1)
summary(BareModel2)
summary(BareModel3)

# The significant variables are:
#LotArea + LandSlope + Neighborhood + OverallQual + YearBuilt + RoofMatl + MasVnrType + MasVnrArea + ExterQual + BsmtQual + X1stFlrSF + X2ndFlrSF + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + Fireplaces + GarageType + GarageCars + GarageArea

# Now we will create a model with only these variables and look at he value of RSE and 

SigModel <- lm(SalePrice ~ LotArea + LandSlope + Neighborhood + OverallQual + YearBuilt + RoofMatl + MasVnrType + MasVnrArea + ExterQual + BsmtQual + X1stFlrSF + X2ndFlrSF + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + Fireplaces + GarageType + GarageCars + GarageArea, data = NewTrain)
summary(SigModel)

# We can further remove GarageCars, neighborhood and halfbath

SigModel1 <- lm(SalePrice ~ LotArea + LandSlope + OverallQual + YearBuilt + RoofMatl + MasVnrType + MasVnrArea + ExterQual + BsmtQual + X1stFlrSF + X2ndFlrSF + FullBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + Fireplaces + GarageType + GarageArea, data = NewTrain)
summary(SigModel1)

# In our current dataset, multi collinearity might be a problem as well

