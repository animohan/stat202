

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

plot(reg.summary$bic, xlab=" Number of Variables ", ylab ="BIC",type = "l")
which.min(reg.summary$bic)
points (6,reg.summary$bic[6],col=" red ",cex =2,pch =20)

par(mfrow=c(1,1))
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

#Test

set.seed(1)
X=rnorm(100)
eps=rnorm(100)

X2=X^2
X3=X^3
beta0=3
beta1=2
beta2=-3
beta3=0.3
Y=beta0+beta1*X+beta2*X2+beta3*X3+eps

library(leaps)
df=data.frame(y=Y,x=X)
regfit.X=regsubsets(y~poly(x,10,raw=T), data=df, nvmax=10)
regfitx.summary=summary(regfit.X)

par(mfrow=c(2,2))

plot(regfitx.summary$bic, xlab="Number of variables", ylab="bic",type = "l")
k=which.min(regfitx.summary$bic)
points(k,regfitx.summary$bic[k],col="red",cex=2,pch=20)

plot(regfitx.summary$adjr2, xlab="Number of variables", ylab="Adjusted RSq",type = "l")
k=which.max(regfitx.summary$adjr2)
points(k,regfitx.summary$adjr2[k],col="red",cex=2,pch=20)

plot(regfitx.summary$cp, xlab="Number of variables", ylab="Cp",type="l")
k=which.min(regfitx.summary$cp)
points(k,regfitx.summary$cp[k],col="red",cex=2,pch=20)