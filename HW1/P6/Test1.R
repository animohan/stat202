
# Q6. Part b
  numnas<-function(x){
    sum_na<-0
    for (n in x){
      if(is.na(n)==T)
        sum_na=sum_na+1
    }
    return(sum_na)
  }
  
  #in_training_features<-read.csv("training_features.csv")
  in_training_features<-matrix(data=c("one",1,NA,3,"two",2,NA,4,"three",5,NA,7),nrow=4,ncol=3)
  num_nas_col=apply(in_training_features,2,numnas)
  
  # Iterarive Code
    # iter<-seq(1:ncol(B))
    # k=length(iter)
    # C = matrix(0,nrow=k, ncol=1)
    # 
    # for(count in iter){
    #   C[count,1]=numnas(B[,count])
    # }
  
  hist(num_nas_col)

#Q6 Part c
  feature.names<-names(in_training_features)
  for(feature.name in feature.names[-1]){
    dummy_name<-paste0("is.na.",feature.name)
    is.na.feature <-is.na(in_training_features[,feature.name])
    in_training_features[,dummy_name]<-as.integer(is.na.feature)
    in_training_features[is.na.feature,feature.name]<-median(in_training_features[,feature.name], na.rm=TRUE)
    
  }