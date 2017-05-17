#KNN algorithm for predicting adjusted IMDB score
library(class)
library(FNN)
set.seed(1);
#divide data
train = sample(1:nrow(new_sub), nrow(new_sub)* (2/3))

train.score= new_sub[train,"imdb_score"]
test.score= new_sub[-train,"imdb_score"]

#scale variables
new_sub <- scale(new_sub[,!names(new_sub) %in% 'imdb_score'])

error<-rep(0,6)
K<-c(1,3,5,10,20,50)
for(i in (1:6)){
  set.seed(1);
  knn.pred<-knn.reg(new_sub[train,], new_sub[-train,] , train.score, k=K[i]);
  #hold on to squared error
  error[i]<-mean((knn.pred$pred-test.score)^2)
}
#plot cv errors
plot(K,error, xlab = "K",  ylab = "MSE", width = 12, height = 6)
#find optimal mtry
which.min(error)
#=4 -> 10
min(error)
#0.809826
#find test error

#KNN algorithm for predicting adjusted gross
library(class)
library(FNN)
set.seed(1);
#divide data
train = sample(1:nrow(new_sub), nrow(new_sub)* (2/3))

train.score= new_sub[train,"gross2016"]
test.score= new_sub[-train,"gross2016"]

#scale variables
new_sub <- scale(new_sub[,!names(new_sub) %in% 'gross2016'])

error<-rep(0,6)
K<-c(1,3,5,10,20,50)
for(i in (1:6)){
  set.seed(1);
  knn.pred<-knn.reg(new_sub[train,], new_sub[-train,] , train.score, k=K[i]);
  #hold on to squared error
  error[i]<-mean((knn.pred$pred-test.score)^2)
}
#plot cv errors
plot(K,error, xlab = "K",  ylab = "MSE", width = 12, height = 6)
#find optimal mtry
which.min(error)
#=2 -> 3
#find test error
min(error)
#2.714498e+14


#KNN algorithm for predicting top 10th percentile
library(class)
library(FNN)
set.seed(1);
#divide data
train = sample(1:nrow(new_sub), nrow(new_sub)* (2/3))

#create new 90th percentile predictor
percentile_90 = quantile(new_sub$gross2016,.90)
over_90 =ifelse(new_sub$gross2016<percentile_90,0,1)
new_sub = data.frame(new_sub,over_90)
train.score= new_sub[train,"over_90"]
test.score= new_sub[-train,"over_90"]

#scale variables
new_sub <- scale(new_sub[,!names(new_sub) %in% c('over_90','gross2016' )])

error<-rep(0,6)
K<-c(1,3,5,10,20,50)
for(i in (1:6)){
  set.seed(1);
  knn.pred<-knn(new_sub[train,], new_sub[-train,] , train.score, k=K[i]);
  #hold on to squared error
  error[i]<-1-mean(knn.pred==test.score)
}
#plot cv errors
plot(K,error, xlab = "K",  ylab = "MSE", width = 12, height = 6)
#find optimal mtry
which.min(error)
#=2 -> 5
#find test error
min(error)
#0.06830601


