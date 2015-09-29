numnas<-function(x){
  sum_na<-0
  for (n in x){
    if(is.na(n)==T)
      sum_na=sum_na+1
  }
  return(sum_na)
}

B<-read.csv("training_features.csv")


iter<-seq(1:ncol(in_training_features))
k=length(iter)
C = matrix(0,nrow=1, ncol=k)

for(count in iter){
  C[count]=numnas(B[iter,])
}
