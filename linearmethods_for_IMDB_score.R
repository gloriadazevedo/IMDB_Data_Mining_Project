#Just linear regression
model_1<-lm(imdb_score~.,data=new_sub)
summary(model_1)
#LOOCV for linear model
glm.fit = glm(imdb_score~., data=new_sub)
summary(glm.fit)
cv.error = cv.glm(new_sub, glm.fit, K = 10)$delta[1]
cv.error
#=0.5992433

#LASSO for IMDB score
library(glmnet)
y = new_sub$imdb_score
x = as.matrix(new_sub[,!names(new_sub) %in% 'imdb_score'])
cv_error = cv.glmnet(x, y, alpha = 1, nfolds = 10)
cv_error$lambda.min
coef(cv_error, cv_error$lambda.min)
#get test error
mse.min <- cv_error $cvm[cv_error $lambda == cv_error $lambda.min]

#LASSO for gross
library(glmnet)
y = new_sub$gross2016
x = as.matrix(new_sub[,!names(new_sub) %in% 'gross2016'])
cv_error = cv.glmnet(x, y, alpha = 1, nfolds = 10)
cv_error$lambda.min
coef(cv_error, cv_error$lambda.min)
#get test error
mse.min <- cv_error $cvm[cv_error $lambda == cv_error $lambda.min]