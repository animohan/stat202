

in_training_target<-read.csv("training_target.csv")
print(paste0("Number of patients: ", nrow(in_training_target)))


summary(in_training_target["ALSFRS_slope"])

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


feature.names<-names(in_training_features)

for(feature.name in feature.names[-1]){
  dummy_name<-paste0("is.na.",feature.name)
  is.na.feature <-is.na(in_training_features[,feature.name])
  in_training_features[,dummy_name]<-as.integer(is.na.feature)
  in_training_features[is.na.feature,feature.name]<-median(in_training_features[,feature.name], na.rm=TRUE)
  
}


valid_features<-read.csv("validation_features.csv")
valid_target<-read.csv("validation_target.csv")
print(paste0("Number of validation patients: ", nrow(valid_target)))
print(paste0("Number of validation features: ", ncol(valid_features)))


summary(in_training_target["ALSFRS_slope"])
summary(valid_target["ALSFRS_slope"])

summary(valid_features["weight.slope"])




newset3=in_training_features[,c("q1_speech.slope", "q2_salivation.slope", "q3_swallowing.slope", "q4_handwriting.slope", "q5a_cutting_without_gastrostomy.slope", "q6_dressing_and_hygiene.slope", "q7_turning_in_bed.slope", "q8_walking.slope", "q9_climbing_stairs.slope")]

pca_set3=prcomp(newset3,scale=TRUE)
var_set3=pca_set3$sdev^2
prop_var_set3=var_set3/sum(var_set3)
plot(cumsum(prop_var_set3),xlab="Principal Component (with subject ID: 525450 included)",ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')

biplot(pca_set3, scale=0,cex=0.6) 