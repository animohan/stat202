train=(Year<2009)
weekly.2009=weekly[!train,]
Direction.2009=Direction[!train]

limlog_reg=glm(Direction~Lag2+Lag1,data=weekly, family=binomial,subset=train)
limlog_reg_probs=predict(limlog_reg,weekly.2009,type="response")
limlog_reg_pred=rep("Down",104)
limlog_reg_pred[limlog_reg_probs>0.50]="Up"
table(limlog_reg_pred,Direction.2009)

limlda_fit=lda(Direction~Lag2,data=weekly,subset=train)
limlda_fit
limlda_pred=predict(limlda_fit,weekly.2009)
limlda_class=limlda_pred$class
table(limlda_class,Direction.2009)

limqda_fit=qda(Direction~Lag2,data=weekly,subset=train)
limqda_fit
limqda_class=predict(limqda_fit,weekly.2009)$class
table(limqda_class,Direction.2009)


library(class)
train.X=cbind(Lag1)[train,]
test.X=cbind(Lag1)[!train,]
train.Direction=Direction[train]
set.seed(1)
knn.pred=knn(data.frame(train.X), data.frame(test.X), train.Direction, k=5)
table(knn.pred,Direction.2009)