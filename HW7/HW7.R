
set.seed(1)
train=sample(1:nrow(Carseats),200)
Carseats.train=Carseats[train,]
Carseats.test=Carseats[-train,]

tree.carseats=tree(Sales~.,Carseats,subset=train)
summary(tree.carseats)

par(mfrow=c(1,1))
plot(tree.carseats)
text(tree.carseats, pretty=0)

tree.pred=predict(tree.carseats,Carseats.test)
mean((tree.pred-Carseats.test$Sales)^2)

cv.carseats=cv.tree(tree.carseats)
plot(cv.carseats$size,cv.carseats$dev, type='b')

prune.carseats=prune.tree(tree.carseats,best=8)
plot(prune.carseats)
text(prune.carseats, pretty=0)

yhat=predict(prune.carseats, newdata=Carseats.test)
plot(yhat, Carseats.test$Sales)
abline(0,1)
mean((yhat-Carseats.test$Sales)^2)


library(randomForest)
set.seed(1)
bag.carseats=randomForest(Sales~.,data=Carseats,subset=train,mtry=10,importance=T)

yhat.bag=predict(bag.carseats,newdata=Carseats.test)
plot(yhat.bag, Carseats.test$Sales)
abline(0,1)
mean((yhat.bag-Carseats.test$Sales)^2)
varImpPlot(bag.carseats)


set.seed(1)
rf.carseats=randomForest(Sales~.,data=Carseats,subset=train,importance=T)
yhat.rf=predict(rf.carseats,newdata=Carseats.test)
plot(yhat.rf, Carseats.test$Sales)
abline(0,1)
mean((yhat.rf-Carseats.test$Sales)^2)
varImpPlot(rf.carseats)


#Boosting
library(ISLR)
fix(Hitters)
Hitters=na.omit(Hitters)
Hitter=na.omit(Hitters)
Hitter$Salary=log(Hitters$Salary)

train=1:200
Hitter.train=Hitter[train,]
Hiter.test=Hitter[-train,]



library(gbm)
set.seed(1)

train.mse=matrix(data=NA, nrow=100, ncol=2)

for(i in 1:100){
  lambda=i*0.01
  train.mse[i,1]=lambda
  boost.hitter=gbm(Salary~.,data=Hitter.train, distribution="gaussian", n.trees=1000,interaction.depth=4,shrinkage = lambda)
  train.mse[i,2]=mean(boost.hitter$train.error)
}

plot(train.mse[,1],train.mse[,2], xlab="Shrinkage factor", ylab="Training MSE")

