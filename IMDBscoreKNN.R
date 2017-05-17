#KNN algorithm for predicting adjusted gross amount
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
K<-c(5,10,20,50,100,200)
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
#=2 -> 10
min(error)
#find test error

