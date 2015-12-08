library(tree)
library(randomForest)
library(rpart)


connections = readRDS("10_percent_service_processed.rds")

### splitting training, tuning and testing data
set.seed(1)
num = nrow(connections)
train = sample(1:num, 0.8*num, replace = FALSE)
tune = sample(1:length(train),0.2*length(train) ,replace=FALSE)
training = connections[train, ] 
testing =  connections[-train,]
tuning = training[tune,]
training = training[-tune,]

### single tree
# package tree
connections.tree = tree(result~., data=training)
summary(connections.tree)
plot(connections.tree)
text(connections.tree, pretty=0)
pred.tree = predict(connections.tree,tuning[,-40],type="class")
table(pred.tree, tuning$result)
sum(pred.tree==tuning$result)/length(pred.tree)
# prune
connections.cv.tree = cv.tree(connections.tree, FUN=prune.misclass)
par(mfrow=c(1,2))
plot(connections.cv.tree$size, connections.cv.tree$dev, type="b")
plot(connections.cv.tree$k, connections.cv.tree$dev, type="b")
connections.prune = prune.misclass(connections.tree, best=6) # six-node tree in this case
plot(connections.prune)
text(connections.prune, pretty=0)
pred.prune = predict(connections.prune,tuning[,-40],type="class")
table(pred.prune, tuning$result)
sum(pred.tree==tuning$result)/length(pred.prune)

# package rpart
connections.rp = rpart(result~., method="class", data=training)
printcp(connections.rp) 
plotcp(connections.rp) 
plot(connections.rp)
pred.rp = predict(connections.rp,tuning[,-40],type="class")
table(pred.rp, tuning$result)
sum(pred.rp==tuning$result)/length(pred.rp)
connections.rp.prune =  prune(connections.rp, cp=connections.rp$cptable[which.min(connections.rp$cptable[,"xerror"]),"CP"])


### random forest

connections.rf = randomForest(x =training[,-40], y = training$result, ntree = 250, importance = TRUE)
save(connections.rf, file="rf_all_feature.rda")
print(connections.rf)
varImpPlot(connections.rf)
#mtry is also a parameter
pred.rf = predict(connections.rf, type="class", tuning[,-40])
table(pred.rf, tuning$result)
sum(pred.rf==tuning$result)/length(pred.rf)

##boosting 
library(adabag) #rpart required
connections.boost = boosting(result~., data=training, boos = TRUE, mfinal = 20, coeflearn = 'Breiman')
pred.boost = predict.boosting(connections.boost, newdata=tuning)
pred.boost$confusion
pred.boost$error


### final testing
training = connections[train,] # combining training and tuning data
connections.rf = randomForest(x =training[,-40], y = training$result, ntree = 250, importance = TRUE)
pred.rf = predict(connections.rf, type="class", testing[,-40])
table(pred.rf, testing$result)
sum(pred.rf==testing$result)/length(pred.rf)

connections.boost = boosting(result~., data=training, boos = TRUE, mfinal = 20, coeflearn = 'Breiman')
pred.boost = predict.boosting(connections.boost, newdata=testing)
pred.boost$confusion
pred.boost$error