rm(list = ls())
cat('\014')
setwd('C:\\Users\\sravy\\Desktop\\Data Mining and Business Analytics in Knowledge Services\\Major Project')

library(ggplot2)
library(corrplot)
library(ggrepel)
library(scales)

train <- read.csv('train.csv', header = T, stringsAsFactors = F)
test <- read.csv('test.csv', header = T, stringsAsFactors = F)
# We use stringsAsFactors = F because all variables will need string manipulation and feature engineering first

dim(train)
str(train[,c(1:10, 81)])

test_label <- test$Id
train$Id <- NULL
test$Id <- NULL

test$SalePrice <- NA
all <- rbind(test, train)
dim(all)

ggplot(data = all[!is.na(all$SalePrice),], aes(x = SalePrice)) + geom_histogram(fill = "blue", binwidth = 10000) + scale_x_continuous(breaks = seq(0, 800000, by = 100000))

summary(all$SalePrice)

numeric_Vars <- which(sapply(all, is.numeric))
numeric_Names <- names(numeric_Vars)
cat("There are", length(numeric_Vars), "numeric variables")

all_numVar <- all[,numeric_Vars]
corval <- cor(all_numVar, use =  "pairwise.complete.obs")
cor_matrix <- as.matrix(sort(corval[,"SalePrice"], decreasing = T))

cor_High <- names(which(apply(cor_matrix, 1, function(x) abs(x) > 0.5)))                        

cor_numVar <- corval[cor_High, cor_High]
corrplot.mixed(cor_numVar, tl.col = 'black', tl.pos = 'lt')

# Overall quality has the highest correlation with SalePrice

ggplot(data = all[!is.na(all$SalePrice),], aes(x = SalePrice)) + 
  geom_histogram(fill = "blue", binwidth = 10000) + 
  scale_x_continuous(breaks = seq(0, 800000, by = 100000))

ggplot(data = all[!is.na(all$SalePrice),], aes(x = factor(OverallQual), y = SalePrice)) +
         geom_boxplot(col = 'blue') +
         labs(x = "Overall Quality", y = "Sale Price")

# Second highest correlation value with Sale Price is GrLivArea       

ggplot(data = train, aes(x = GrLivArea, y = SalePrice)) +
  geom_point(col = 'blue') +
  geom_smooth(method = 'lm', se = F, color = 'black', aes(group = 1)) +
  geom_text_repel(aes(label = ifelse(train$GrLivArea > 4000, rownames(train), ''))) + 
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) + 
  ggtitle('asasasa')

all[c(524, 1299), c('SalePrice', 'GrLivArea', 'OverallQual')]

# Since the Overall Quality is high, these two points could be outliers

