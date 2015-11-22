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

train_feat_median=temp[1:2424,1:858]

#Create a dataframe containing both the ALFRS Slope and input colument
df=data.frame(ALFRS=train_targ$ALSFRS_slope, train_feat)
set.seed(1)
train=sample(1:nrow(df),2000)
test=-train

df.test=df[test,]
df.train=df[train,]

#Module to use only those columns that have more than 2300 entries as non empty
  df.reduced=data.frame(subject.id=df$subject.id, ALFRS=df$ALFRS)
  for( i in names(num_nas_col)){
    if(num_nas_col[i]<500 && i!="subject.id"){
      df.reduced=data.frame(df.reduced,train_feat[i])
    }
  }
  df.reduced.train=df.reduced[train,]
  df.reduced.test=df.reduced[test,]


  # First we define a new data.frame df.10 that takes subject ids and ALRFRs from original data frame df.
  df.10=data.frame(subject.id=df$subject.id, ALFRS=df$ALFRS)
  
  # Starting from df.10 only having subject id and ALFRS, we append only those columns that have less than 10%
  # missing data.
  for( i in names(num_nas_col)){
    if(num_nas_col[i]<242 && i!="subject.id"){
      df.10=data.frame(df.10,train_feat[i])
    }
  }
  
  # We need the complete df with all the variables.
  
  
  #Now we remove any row entries that have any missing data.
  df.10=na.omit(df.10)
  
  # only 1781 rows meet this criteria. 
  # After these procedures, we have already removed 2424-1781 data points. We can use the deleted datapoints as test
  # entries later.

  
  
  #Trying ridge and lasso regression:
  library(glmnet)
  x=model.matrix(df.10$ALFRS~.,df.10)[,-1]
  y=df.10$ALFRS
  
  grid=10^seq(10,-2,length=100)
  lasso.mod=glmnet(x,y, alpha=1, lambda=grid)
  
  plot(lasso.mod)

  #using cross validation
  set.seed(1)
  cv.out=cv.glmnet(x,y,alpha=1,lambda=grid)
  plot(cv.out)
  
  bestlam=cv.out$lambda.min
  lasso.pred=predict(lasso.mod,s=bestlam,newx=df.x)
  # the reason, predict is failing is becuase, df has columns for which we did not take
  #values before
  
  
  
  #Testing on full data.set
  df.x=model.matrix(df$ALFRS~.,df)[,-1]
  df.y=df$ALFRS
  
  
  
  df.0=data.frame(subject.id=df$subject.id, ALFRS=df$ALFRS)
  for( i in names(num_nas_col)){
    if(num_nas_col[i]<1 && i!="subject.id"){
      df.0=data.frame(df.0,train_feat[i])
    }
  }


feature.names<-names(train_feat)

for(feature.name in feature.names[-1]){
  dummy_name<-paste0("is.na.",feature.name)
  is.na.feature <-is.na(train_feat[,feature.name])
  train_feat[,dummy_name]<-as.integer(is.na.feature)
  train_feat[is.na.feature,feature.name]<-median(train_feat[,feature.name], na.rm=TRUE)
  
}

#
valid_features<-read.csv("validation_features.csv")
valid_target<-read.csv("validation_target.csv")
print(paste0("Number of validation patients: ", nrow(valid_target)))
print(paste0("Number of validation features: ", ncol(valid_features)))


summary(in_training_target["ALSFRS_slope"])
summary(valid_target["ALSFRS_slope"])
summary(valid_features["weight.slope"])




a=train_feat[1:10,c("q1_speech.slope", "q2_salivation.slope")]

