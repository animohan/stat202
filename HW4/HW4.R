attach(Default)
set.seed(1)
glm.fit=glm(default~income+balance, family=binomial)

#cross validation
train=sample(10000,6000)
train_default=(Default[train,])
test_default=(Default[-train,])
glm.fit=glm(default~income+balance, train_default,family=binomial)
glm.probs=predict(glm.fit,test_default,type="response")
glm.pred=rep("No",4000)
glm.pred[glm.probs>0.5]="Yes"
table(glm.pred,test_default$default)
mean(glm.pred!=test_default$default)


train=sample(10000,2000)
train_default=(Default[train,])
test_default=(Default[-train,])
glm.fit=glm(default~income+balance, train_default,family=binomial)
glm.probs=predict(glm.fit,test_default,type="response")
glm.pred=rep("No",8000)
glm.pred[glm.probs>0.5]="Yes"
table(glm.pred,test_default$default)
mean(glm.pred!=test_default$default)

train=sample(10000,5000)
train_default=(Default[train,])
test_default=(Default[-train,])
glm.fit=glm(default~income+balance, train_default,family=binomial)
glm.probs=predict(glm.fit,test_default,type="response")
glm.pred=rep("No",5000)
glm.pred[glm.probs>0.5]="Yes"
table(glm.pred,test_default$default)
mean(glm.pred!=test_default$default)

train=sample(10000,8000)
train_default=(Default[train,])
test_default=(Default[-train,])
glm.fit=glm(default~income+balance, train_default,family=binomial)
glm.probs=predict(glm.fit,test_default,type="response")
glm.pred=rep("No",2000)
glm.pred[glm.probs>0.5]="Yes"
table(glm.pred,test_default$default)
mean(glm.pred!=test_default$default)


train=sample(10000,5000)
train_default=(Default[train,])
test_default=(Default[-train,])
glm.fit=glm(default~income+balance+student, train_default,family=binomial)
glm.probs=predict(glm.fit,test_default,type="response")
glm.pred=rep("No",5000)
glm.pred[glm.probs>0.5]="Yes"
table(glm.pred,test_default$default)
mean(glm.pred!=test_default$default)


#Q2 (6 in book)
set.seed(1)
glm.fit=glm(default~income+balance,Default,family=binomial)
summary(glm.fit)

boot.fn =function(data, index){
  train_data=data[index,]
  glm.fit=glm(default~income+balance,train_data,family=binomial)
  return(glm.fit$coefficients)
}

boot(data=Default, statistic = boot.fn, R=1000)

#Q3 (8 in book)

set.seed(1)
y=rnorm(100)
x=rnorm(100)
y=x-2*x^2+rnorm(100)
plot(x,y)

set.seed(2)
glm.fit=glm(y~x, data=xydat)
cv.err=cv.glm(xydat,glm.fit)
cv.err$delta[1]

glm.fit=glm(y~poly(x,2), data=xydat)
cv.err=cv.glm(xydat,glm.fit)
cv.err$delta[1]

glm.fit=glm(y~poly(x,3), data=xydat)
cv.err=cv.glm(xydat,glm.fit)
cv.err$delta[1]

glm.fit=glm(y~poly(x,4), data=xydat)
cv.err=cv.glm(xydat,glm.fit)
cv.err$delta[1]


#Q4 ( 9 in book)
library(MASS)

boot.fn2 = function(inputdata,index){
  Boston_temp=inputdata[index,]
  return (mean(Boston_temp$medv))
}

boot(data=Boston, statistic = boot.fn2, R=500)


set.seed(1)
attach(USArrests)

library(boot)
boot_pca.fn = function(input, index){
  temp_input=input[index,]
  pca_set=prcomp(temp_input,scale=TRUE)
  pca_var=pca_set$sdev^2
  prop_Var=(pca_var[1]+pca_var[2])/sum(pr.var)
  return(prop_Var)
}

boot(data=USArrests, statistic = boot_pca.fn, R=1000)

boot_pca2.fn = function(input, index){
  temp_input=input[index,]
  pca_set=prcomp(temp_input,scale=TRUE)
  pca_pc1=pca_set$rotation[,1]
  return(pca_pc1)
}

boot(data=USArrests, statistic = boot_pca2.fn, R=1000)


