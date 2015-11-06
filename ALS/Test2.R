#Getting the training features and target

#Get the input target or Y
train_targ<-read.csv("training_target.csv")
print(paste0("Number of patients: ", nrow(train_targ)))

#get the input features or X
train_feat<-read.csv("training_features.csv")
print(paste0("Number of features: ", ncol(train_feat)))

#summary of training target
summary(train_target["ALSFRS_slope"])


#function to read the training features and replace empty entries by na.
numnas<-function(x){
  sum_na<-0
  for (n in x){
    if(is.na(n)==T)
      sum_na=sum_na+1
  }
  return(sum_na)
}

#replace empty numbers with NA.
num_nas_col=apply(train_feat,2,numnas)

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

