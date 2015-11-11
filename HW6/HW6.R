library(ISLR)
library(boot)

#Find at least one non-linear estimate which 
#does better than linear regression, and 
#justify this using a t-test or by showing an 
#improvement in the cross-validation error 
#with respect to a linear model. 
#You must also produce a plot of the predictor X vs. 
#the non-linear estimate f^(X).

set.seed(1)
#10-Fold validation comparing a linear and cubic model

fit.lm=glm(mpg~horsepower, data=Auto)
cv.error=cv.glm(Auto,fit.lm,K=10)
cv.error$delta


fit.poly=glm(mpg~poly(horsepower,3),data=Auto)
cv.error=cv.glm(Auto,fit.poly,K=10)
cv.error$delta

#Using Polynomial regression
hplims=range(Auto$horsepower)
hp.grid=seq(from=hplims[1],to=hplims[2])
preds=predict(fit.poly,newdata=list(horsepower=hp.grid), se=T)
se.bands=cbind(preds$fit+2*preds$se,preds$fit-2*preds$se.fit)

plot(horsepower, mpg, xlim=hplims, cex=0.5, col="darkgrey")
title("Applying polynomial regression")
lines(hp.grid, preds$fit, lwd=2, col="blue")
matlines(hp.grid, se.bands, lwd=1, col="blue", lty=3)

#using  ANOVA to analyze variance.
fit.1=lm(mpg~horsepower, data=Auto)
fit.2=lm(mpg~poly(horsepower,2), data=Auto)
fit.3=lm(mpg~poly(horsepower,3), data=Auto)
fit.4=lm(mpg~poly(horsepower,4), data=Auto)

anova(fit.1,fit.2,fit.3,fit.4)
print(paste0("Quadratic model seems to be a good fit"))

library(splines)
fit=lm(mpg~bs(horsepower, df=6),data=Auto)
pred=predict(fit,newdata=list(horsepower=hp.grid),se=T)
plot(horsepower,mpg, col="gray")
title('Applying splines')
lines(hp.grid, pred$fit,lwd=2)
lines(hp.grid, pred$fit+2*pred$se,lty="dashed")
lines(hp.grid, pred$fit-2*pred$se,lty="dashed")


#Applying local regression
fit=loess(mpg~horsepower, span=0.2, data=Auto)
pred=predict(fit, newdata=data.frame(horsepower=hp.grid))
plot(horsepower,mpg, col="gray")
title('Applying Local regression')
lines(hp.grid, pred,col="blue", lwd=2)


#Problem 5
library(MASS)
attach(Boston)

fit.poly=glm(nox~poly(dis,5))
print(paste0("Coefficient of the polynomial eqn"))
coef(fit.poly)
summary(fit.poly)
dislims=range(Boston$dis)
dis.grid=seq(from=dislims[1],to=dislims[2])
preds=predict(fit.poly,newdata=list(dis=dis.grid), se=T)
se.bands=cbind(preds$fit+2*preds$se,preds$fit-2*preds$se.fit)

plot(dis,nox, xlim=dislims,xlab="Distance from Employment Ctr", ylab="Notrogen Oxides Conc", cex=0.5, col="darkgrey")
title("Polynomial Regression: Boston Dataset")
lines(dis.grid, preds$fit, lwd=2, col="blue")
matlines(dis.grid, se.bands, lwd=1, col="blue", lty=3)

# (b)

fit=matrix(0,nrow=10,ncol=1)
plot(dis,nox, xlim=dislims,xlab="Distance from Employment Ctr", ylab="Notrogen Oxides Conc", cex=0.5, col="darkgrey")
for (i in 1:10){
  lm.fit=glm(nox~poly(dis,i),data=Boston)
  fit[i]=sum(lm.fit$residuals^2)
  preds=predict(lm.fit,newdata=list(dis=dis.grid), se=T)
  lines(dis.grid, preds$fit, lwd=2, col=i)
  
}

fit

#c
set.seed(1)
cv.error.delta=rep(NA,10)
for(i in 1:10){
  fit.poly=glm(nox~poly(dis,i),data=Boston)
  cv.error=cv.glm(Boston,fit.poly,K=10)
  cv.error.delta[i]=cv.error$delta[1]
}
plot(cv.error.delta)

#degree 4 polymial is the simplest model with lowest polynomial


#d
library(splines)
fit=lm(nox~bs(dis,df=4),data=Boston)
dislims=range(Boston$dis)
dis.grid=seq(from=dislims[1],to=dislims[2])
summary(fit)
pred=predict(fit,newdata=list(dis=dis.grid), se=T)
plot(dis,nox, col="gray")
lines(dis.grid, pred$fit, lwd=2)
lines(dis.grid, pred$fit+2*pred$se, lty="dashed")
lines(dis.grid, pred$fit-2*pred$se, lty="dashed")

#e
plot(dis,nox, col="gray")
rss=rep(NA,10)
for(i in 3:10){
  lm.fit=lm(nox~bs(dis,df=i),data=Boston)
  pred=predict(lm.fit,newdata=list(dis=dis.grid), se=T)
  lines(dis.grid, pred$fit, lwd=2, col=i*10)
  rss[i]=sum(lm.fit$residuals^2)
}

plot(rss)
print(paste0("RSS decreases as we increase degrees of freedom. "))
#lowers RSS is with df=10

#f
set.seed(1)
cv.error.delta=rep(NA,10)
for(i in 1:10){
  fit.spline=glm(nox~bs(dis,df=i),data=Boston)
  cv.error=cv.glm(Boston,fit.spline,K=10)
  cv.error.delta[i]=cv.error$delta[1]
}
plot(cv.error.delta)

#lowest error obtained through cross validation is for df=10
#error decreases till degree 5, then inc and then deec


x1=rnorm(100)
x2=rnorm(100)
eps=rnorm(100)
y=5+2*x1+7*x2+eps

beta0=rep(NA,1000)
beta1=rep(NA,1000)
beta2=rep(NA,1000)

beta1[1]=5

for(i in 1:1000){
  a=y-beta1[i]*x1
  beta2[i]=lm(a~x2)$coef[2]
  beta0[i]=lm(a~x2)$coef[1]
  
  a=y-beta2[i]*x2
  beta1[i+1]=lm(a~x1)$coef[2]

}

par(mfrow=c(1,1))

x=-2:2
beta0=1
beta1=1
beta2=-2

for(i in 1:5 ) {
  if(x[i]<1){
    y[i]=beta0+x[i]*beta1
  } else {
    y[i]=beta0+beta1*x[i]+beta2*(x[i]-1)^2
  }
}
plot(x,y)
