library(randomForest)

learn.model <- function(train.data){
  ids <- train.data$ids
  train.x <- train.data$data
  train.y <- train.data$target
  train.x <- rfImpute(train.x, train.y, iter=2, ntree=10)
  
  yhat1 <- crossvalidate(train.x, train.y, num.folds = 5, train.fun=randomForest, ntree=50, mtry=5)
  yhat2 <- exp(crossvalidate(train.x, log(train.y+1), num.folds = 5, train.fun=randomForest, ntree=50, mtry=5))-1
  
  print(paste("rmse = ", rmse(train.y, yhat1), ", MAPE = ", mape(train.y+1, yhat1)))
  print(paste("rmse = ", rmse(train.y, yhat2), ", MAPE = ", mape(train.y+1, yhat1)))
  
  plot(train.y, yhat1, log="xy", main="no transformation")
  abline(0, 1, col="red")
  
  plot(train.y, yhat2, log="xy", main="target log transformed")
  abline(0, 1, col="blue")
  
  browser()
}


crossvalidate <- function(X, Y, num.folds, train.fun, predict.fun=predict, ...){
  n <- nrow(X)
  folds <- sample(1:num.folds, nrow(X), replace=TRUE)
  yhat <- numeric(n)
  for(i in 1:num.folds){
    fold <- folds == i
    fit <- train.fun(X[!fold,], Y[!fold], ...)
    yhat[fold] <- predict.fun(fit, X[fold,])    
  }
  yhat
}


rmse <- function(y, yhat){
  sqrt(mean((y - yhat)^2, na.rm=T))
}

mape <- function(y, yhat){
  mean(abs(y - yhat)/y, na.rm=T)
}

