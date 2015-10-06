states = row.names(USArrests)
Arrests_Data=USArrests
HC_Arrests_Complete=hclust(dist(Arrests_Data),method="complete")
plot(HC_Arrests_Complete, main ="US Arrests-Complete Linkage",xlab="States", sub=" ",cex=0.9)
cutree(HC_Arrests_Complete,3)

Arrests_Data_Scaled=scale(Arrests_Data,scale=TRUE)
HC_ScaledArrests_Complete=hclust(dist(Arrests_Data_Scaled),method="complete")
plot(HC_ScaledArrests_Complete, main ="US Scaled Arrests-Complete Linkage",xlab="States", sub="",cex=0.9)

library(MASS)
library(ISLR)
autodat=Auto
pairs(autodat)
cor(autodat[,1:8])

attach(autodat)
autolm=lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year+origin)
summary(autolm)
