set.seed(1)
library(tree)
library(ISLR)
library(MASS)
library(randomForest)
#divide data
train = sample(1:nrow(new_sub), nrow(new_sub)*(2/3))
#get test gross values
movies.test=new_sub[-train,"imdb_score"]
movies.cv = new_sub[train,"imdb_score"]
#Cross Validation on mtry
cv.error=rep(0,(ncol(new_sub) -1))
for (i in 1:(ncol(new_sub) -1))
{
  #fit model
  movies=randomForest(imdb_score ~.,data=new_sub,subset=train, mtry = i)   
  #calculate error
  yhat.bag = predict(movies, newdata=new_sub[train,])
  cv.error[i] =  mean((yhat.bag - movies.cv)^2)
}
#plot cv errors
plot(cv.error, xlab = "mtry", ylab = "MSE", width = 12, height = 6)
#find optimal mtry
which.min(cv.error)
#=30
min(cv.error)
#=0.08140791
#Calculate test error
#Fit Random Forest. Set number of trees (ntree) and variables (mtry) at each iteration to default
movies=randomForest(imdb_score ~.,data=new_sub,subset=train, importance=TRUE, mtry = which.min(cv.error))
#Calculate Error
yhat.bag = predict(movies, newdata=new_sub[-train,])
mean((yhat.bag - movies.test)^2)
#0.5220254
#Find Important Variables
importance(movies)
varImpPlot(movies)



#small tree*********************
library(tree)
library(ISLR)

set.seed(1)
#divide data
train = sample(1:nrow(new_sub), nrow(new_sub)*(2/3))
#get test gross values
movies.test=new_sub[-train,"imdb_score"]
movies.cv = new_sub[train,"imdb_score"]
#cross validation
tree_model =tree(imdb_score~.,new_sub)

cv.movie=cv.tree(tree_model)
#=12
tree_model =prune.tree(tree_model,best=12)
plot(tree_model)
text(tree_model,pretty=0)
tree.pred=predict(tree_model,movies.test)
table(tree.pred,movies.test)
summary(tree_model)


plot(tree_model)
text(tree_model,pretty=0)
