#Getting the training features and target

#Get the input target or Y i.e the response variable
train_targ<-read.csv("training_target.csv")
print(paste0("Number of patients: ", nrow(train_targ)))

#get the input features or X
train_feat<-read.csv("training_features.csv")
print(paste0("Number of features: ", ncol(train_feat)))

#summary of training target
summary(train_targ["ALSFRS_slope"])


#function to count the number of empty entries in an inputx
numnas<-function(x){
  sum_na<-0
  for (n in x){
    if(is.na(n)==T)
      sum_na=sum_na+1
  }
  return(sum_na)
}

#Running the function on training features gives us the number of entries in a column that are empty.
num_nas_col=apply(train_feat,2,numnas)

#Function to get columns with a greater number than 'a' empty entries
for( i in names(num_nas_col)){
  if(num_nas_col[i]<500){
    print(i)
  }
}

#Missing data points are problem, creat an alternative data set with all missing values
# filled with median

feature.names<-names(train_feat)
temp=train_feat

for(feature.name in feature.names[-1]){
  dummy_name<-paste0("is.na.",feature.name)
  is.na.feature <-is.na(temp[,feature.name])
  temp[,dummy_name]<-as.integer(is.na.feature)
  temp[is.na.feature,feature.name]<-median(temp[,feature.name], na.rm=TRUE)
}


#Create a dataframe containing both the ALFRS Slope and input colument
df=data.frame(ALFRS_slope=train_targ$ALSFRS_slope,train_feat)

#Module to use only those columns that have more than 2300 entries as non empty
df.reduced=data.frame(subject.id=df$subject.id, ALFRS_slope=df$ALFRS_slope)

# Starting from df.reduced only having subject id and ALFRS, we append only those columns that have less than 'x' empty rows
for( i in names(num_nas_col)){
  if(num_nas_col[i]<500 && i!="subject.id"){
    df.reduced=data.frame(df.reduced,train_feat[i])
  }
}


#Now fill the empty entries with median values
feature.names=names(df.reduced)
temp=df.reduced

for(feature.name in feature.names[-1]){
  dummy_name<-paste0("is.na.",feature.name)
  is.na.feature <-is.na(temp[,feature.name])
  temp[,dummy_name]<-as.integer(is.na.feature)
  temp[is.na.feature,feature.name]<-median(temp[,feature.name], na.rm=TRUE)
}

df.reduced=temp[1:2424,1:295]


library(glmnet)
x=model.matrix(df.reduced$ALFRS_slope~.,df.reduced)[,-1]
y=df.reduced$ALFRS_slope

grid=10^seq(10,-2,length=100)
lasso.mod=glmnet(x,y, alpha=1, lambda=grid)

plot(lasso.mod)

#using cross validation
set.seed(1)
cv.out=cv.glmnet(x,y,alpha=1,lambda=grid)
plot(cv.out)

bestlam=cv.out$lambda.min


#Testing with validation feature set
valid_feat=read.csv("validation_features.csv")
valid_targ=read.csv("validation_target.csv")


num_nas_col=apply(valid_feat,2,numnas)

feature.names<-names(valid_feat)
temp=valid_feat

for(feature.name in feature.names[-1]){
  dummy_name<-paste0("is.na.",feature.name)
  is.na.feature <-is.na(temp[,feature.name])
  temp[,dummy_name]<-as.integer(is.na.feature)
  ifelse (is.na(median(temp[,feature.name]))==F, temp[is.na.feature,feature.name]<-median(temp[,feature.name], na.rm=TRUE),temp[is.na.feature,feature.name]<-0)
}

valid_feat_median=temp[1:101,1:858]

valid.median=data.frame(ALFRS_slope=valid_targ$ALSFRS_slope,valid_feat_median)

valid.x=model.matrix(valid.median$ALFRS_slope~.,valid.median)[,-1]
valid.y=valid.median$ALFRS_slope

lasso.pred=predict(lasso.mod,s=bestlam,newx=valid.x)
mean((lasso.pred-valid.y)^2)




#Preparing for leaderboard predictions:
lb_feat<-read.csv("leaderboard_features.csv")
leaderboard.predictions <- read.csv("leaderboard_predictions-example.csv")

num_nas_col=apply(lb_feat,2,numnas)

feature.names<-names(lb_feat)
temp=lb_feat

for(feature.name in feature.names[-1]){
  dummy_name<-paste0("is.na.",feature.name)
  is.na.feature <-is.na(temp[,feature.name])
  temp[,dummy_name]<-as.integer(is.na.feature)
  ifelse (is.na(median(temp[,feature.name]))==F, temp[is.na.feature,feature.name]<-median(temp[,feature.name], na.rm=TRUE),temp[is.na.feature,feature.name]<-0)
  
  
}

lb_feat_median=temp[1:187,1:858]

lb.median=data.frame(ALFRS_slope=leaderboard.predictions$ALSFRS_slope, lb_feat_median)

lb.x=model.matrix(lb.median$ALFRS_slope~.,lb.median)[,-1]
lb.y=lb.median$ALFRS_slope

lasso.pred=predict(lasso.mod,s=bestlam,newx=lb.x)
leaderboard.predictions$ALSFRS_slope=lasso.pred
write.csv(leaderboard.predictions, file = "leaderboard_predictions.csv",row.names=FALSE)
