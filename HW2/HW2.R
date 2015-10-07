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


MA=Boston
MAName=names(MA)
for (i in c(9,10,13)){
  y=MA$crim
  x=MA[,i]
  print(summary(lm(y~x)))
  plot(x,y,xlab=MAName[i],ylab="Crim")
}

#2 crim-zn -4.594
#3 crim-indus: 9.991
#4 c-chas -1.257
#5 c- nox-10.419
#6 rm -5.045
#7 age 8.463
#8 dis -9.213
#9 rad 17.998
#10 tax 16
#11 ptratio 6.801
#12 black -9.367
#13 lstat 11.491
#14 medv 9.46



