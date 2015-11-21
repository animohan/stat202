#Getting the training features and target

#Get the input target or Y i.e the response variable
train_targ<-read.csv("training_target.csv")
print(paste0("Number of patients: ", nrow(train_targ)))

#get the input features or X
train_feat<-read.csv("training_features.csv")
print(paste0("Number of features: ", ncol(train_feat)))

#summary of training target
summary(train_target["ALSFRS_slope"])


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

#Create a dataframe containing both the ALFRS Slope and input colument
df=data.frame(ALFRS=train_targ$ALSFRS_slope, train_feat)
set.seed(1)
train=sample(1:nrow(df),2000)
test=-train

df.test=df[test,]
df.train=df[train,]

#Module to use only those columns that have more than 2300 entries as non empty
  df.reduced=data.frame(subject.id=df$subject.id)
  for( i in names(num_nas_col)){
    if(num_nas_col[i]<500 && i!="subject.id"){
      df.reduced=data.frame(df.reduced,train_feat[i])
    }
  }
  df.reduced.train=df.reduced[train,]
  df.reduced.test=df.reduced[test,]



feature.names<-names(train_feat)

for(feature.name in feature.names[-1]){
  dummy_name<-paste0("is.na.",feature.name)
  is.na.feature <-is.na(train_feat[,feature.name])
  train_feat[,dummy_name]<-as.integer(is.na.feature)
  train_feat[is.na.feature,feature.name]<-median(train_feat[,feature.name], na.rm=TRUE)
  
}

valid_features<-read.csv("validation_features.csv")
valid_target<-read.csv("validation_target.csv")
print(paste0("Number of validation patients: ", nrow(valid_target)))
print(paste0("Number of validation features: ", ncol(valid_features)))


summary(in_training_target["ALSFRS_slope"])
summary(valid_target["ALSFRS_slope"])
summary(valid_features["weight.slope"])




a=train_feat[1:10,c("q1_speech.slope", "q2_salivation.slope")]

