#Q6 Part a
in_training_target<-read.csv("training_target.csv")
print(paste0("Number of patients: ", nrow(in_training_target)))


summary(in_training_target["ALSFRS_slope"])


# Q6. Part b

in_training_features<-read.csv("training_features.csv")
print(paste0("Number of features: ", ncol(in_training_features)))

  numnas<-function(x){
    sum_na<-0
    for (n in x){
      if(is.na(n)==T)
        sum_na=sum_na+1
    }
    return(sum_na)
  }
  

  num_nas_col=apply(in_training_features,2,numnas)
  

  
  hist(num_nas_col)

#Q6 Part c
  feature.names<-names(in_training_features)
  
  for(feature.name in feature.names[-1]){
    dummy_name<-paste0("is.na.",feature.name)
    is.na.feature <-is.na(in_training_features[,feature.name])
    in_training_features[,dummy_name]<-as.integer(is.na.feature)
    in_training_features[is.na.feature,feature.name]<-median(in_training_features[,feature.name], na.rm=TRUE)
    
  }
  

#Q6 Part d
valid_features<-read.csv("validation_features.csv")
valid_target<-read.csv("validation_target.csv")
print(paste0("Number of validation patients: ", nrow(valid_target)))
print(paste0("Number of validation features: ", ncol(valid_features)))


summary(in_training_target["ALSFRS_slope"])
summary(valid_target["ALSFRS_slope"])

summary(valid_features["weight.slope"])



  