set.seed(1)
library(tree)
library(ISLR)
library(MASS)
library(randomForest)
#divide data
train = sample(1:nrow(new_sub), nrow(new_sub)/2)
#get test gross values
movies.test=new_sub[-train,"gross2016"]
movies.cv = new_sub[train,"gross2016"]
#Cross Validation on mtry
cv.error=rep(0,ncol(new_sub))
for (i in 1:ncol(new_sub))
{
  #fit model
  movies=randomForest(gross2016 ~.-budget,data=new_sub,subset=train, mtry = i)   
  #calculate error
  yhat.bag = predict(movies, newdata=new_sub[train,])
  cv.error[i] =  mean((yhat.bag - movies.cv)^2)
}
#plot cv errors
plot(cv.error, xlab = "mtry", ylab = "MSE", width = 12, height = 6)
#find optimal mtry
which.min(cv.error)
#=35
#Calculate test error
#Fit Random Forest. Set number of trees (ntree) and variables (mtry) at each iteration to default
movies=randomForest(gross2016 ~.-budget,data=new_sub,subset=train, importance=TRUE, mtry = which.min(cv.error))
#Calculate Error
yhat = predict(movies, newdata=new_sub[-train,])
mean((yhat- movies.test)^2)
#3.081916e+14
#Find Important Variables
importance(movies)
varImpPlot(movies)
