library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
dim(Hitters)

library(glmnet)
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)

dim(coef(ridge.mod))
ridge.mod$lambda[50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
sqrt(sum(coef(ridge.mod)[-1,60]^2))
predict(ridge.mod, s=50, type="coefficients")[1:20]

set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train], alpha=0, lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4, newx=x[test,])
mean((ridge.pred-y.test)^2)


#Test
X=rnorm(100)
eps=rnorm(100)
X2=X^2
X3=X^3
Y=5+1*X+2*X2+4*X3+eps
