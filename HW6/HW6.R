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
plot(dis,nox, xlim=dislims,xlab="Distance from Employment Ctr", ylab="Notrogen Oxides Conc", cex=0.5, col="darkgrey")
fit.1=lm(nox~dis,data=Boston)
preds=predict(fit.1,newdata=list(dis=dis.grid), se=T)
lines(dis.grid, preds$fit, lwd=2, col="blue")

fit.2=lm(nox~poly(dis,2),data=Boston)
preds=predict(fit.2,newdata=list(dis=dis.grid), se=T)
lines(dis.grid, preds$fit, lwd=2, col="green")

fit.3=lm(nox~poly(dis,3),data=Boston)


fit.4=lm(nox~poly(dis,4),data=Boston)
fit.5=lm(nox~poly(dis,5),data=Boston)
fit.6=lm(nox~poly(dis,6),data=Boston)
fit.7=lm(nox~poly(dis,7),data=Boston)
fit.8=lm(nox~poly(dis,8),data=Boston)
fit.9=lm(nox~poly(dis,9),data=Boston)
fit.10=lm(nox~poly(dis,10),data=Boston)
anova(fit.1,fit.2,fit.3,fit.4,fit.5,fit.6,fit.7,fit.8,fit.9,fit.10)
