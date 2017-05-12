#******Functions
#Get rid of NA/missing values 

deletemissing = function(data){
  #get rid of NA values
  data = data[complete.cases(data),]
  #loop through each feature and delete its missing values
  for(feature in 1:ncol(data)){
    data = data[!(is.na(data[feature]) | data[feature] ==""), ]
  }
  return(data)
}