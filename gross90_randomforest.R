set.seed(1)
library(tree)
library(ISLR)
library(MASS)
library(randomForest)
#create new 90th percentile predictor
percentile_90 = quantile(new_sub$gross2016,.90)
new_sub$over_90<-new_sub$gross2016>=percentile_90

#divide data
train = sample(1:nrow(new_sub), nrow(new_sub)/2)
#get test gross values
movies.test=new_sub[-train,"over_90"]
movies.cv = new_sub[train,"over_90"]
#Cross Validation on mtry
cv.error=rep(0,(ncol(new_sub)-2))
for (i in 1:(ncol(new_sub)-2))
{
  #fit model
  movies=randomForest(over_90 ~.-budget-gross2016,data=new_sub,subset=train, mtry = i)   
  #calculate error
  yhat.bag = predict(movies, newdata=new_sub[train,])
  cv.error[i] =  mean((yhat.bag - movies.cv)^2)
}
#plot cv errors
plot(cv.error, xlab = "mtry", ylab = "MSE")
#find optimal mtry
which.min(cv.error)
#=27
#Calculate test error
#Fit Random Forest. Set number of trees (ntree) and variables (mtry) at each iteration to default
movies=randomForest(over_90 ~.-budget-gross2016,data=new_sub,subset=train, importance=TRUE, mtry = which.min(cv.error))
#Calculate Error
yhat.bag = predict(movies, newdata=new_sub[-train,])
mean((yhat.bag - movies.test)^2)
#2.881236e+14
#Find Important Variables
importance(movies)
varImpPlot(movies)
