library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
dim(Hitters)

library(leaps)
regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)

regfit.full=regsubsets(Salary~.,data=Hitters, nvmax=19)
reg.summary=summary(regfit.full)

par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of variables", ylab="RSS",type = "l")
plot(reg.summary$adjr2, xlab="Number of variables", ylab="Adjusted RSq",type = "l")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11],col="red",cex=2,pch=20)

plot(reg.summary$cp, xlab="Number of variables", ylab="Cp",type="l")
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)

#Test
X=rnorm(100)
eps=rnorm(100)
X2=X^2
X3=X^3
Y=5+1*X+2*X2+4*X3+eps

df=data.frame(Y)
for(i in 1:10){
 df[paste0('X',i)]=X^i
}

regfit.X=regsubsets(df$Y~., data=df,nvmax=10)

