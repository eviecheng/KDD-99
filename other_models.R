# 10 percent data
setwd("~/Desktop/")
data = readRDS("data_10_percent_processed.rds")

### train / tune / test
set.seed(1)
train = sample(1:nrow(data), 0.8*nrow(data), replace = FALSE)
tune = sample(1:length(train),0.2*length(train), replace = FALSE)
data.train = data[train, ] 
data.test = data[-train, ]
data.tune = data.train[tune, ]
data.train = data.train[-tune,]

result.train = data$result[train]
result.test = data$result[-train]
result.tune = result.train[tune]
result.train = result.train[-tune]


# multinom
library(nnet)
multinom.fit = multinom(result~., data = data.train)
# summary(multinom.fit)
multinom.pred = predict(multinom.fit, type = "class", data.tune)
table(multinom.pred, result.tune)
sum(multinom.pred == result.tune)/length(multinom.pred)

# knn - numeric only
library(class)
numeric_f = c(1, 5, 6, 9:11, 13, 16:19, 22:39) # numeric features
knn.pred = knn(data.train[,numeric_f], data.test[,numeric_f], result.train, k = 3, l = 0, prob = FALSE, use.all = TRUE)
summary(knn.pred)
table(knn.pred, result.test)
sum(knn.pred == result.test)/length(knn.pred)

# knncat - numeric + categorical
library(knncat)
cat.pred = knncat(data.train, data.tune, classcol = 40)
sum(cat.pred == result.tune)/length(cat.pred)
cat.pred

# naive bayes
library(e1071)
naive.fit = naiveBayes(result~., data = data.train)
naive.pred = predict(naive.fit, data.tune, type = "class")
table(naive.pred, result.tune)
sum(naive.pred == result.tune)/length(naive.pred)