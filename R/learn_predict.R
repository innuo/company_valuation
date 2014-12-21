library(randomForest)

learn.model <- function(train.data, test.data){
  train.ids <- train.data$ids
  test.ids <- test.data$ids

  train.x.na <- train.data$data
  test.x.na <- test.data$data
  
  n.train <- nrow(train.x.na)
  n.test <-  nrow(test.x.na)
  
  train.y <- train.data$target
  test.y <- test.data$target
  
  x.filled <- rfImpute(rbind(train.x.na, test.x.na), c(train.y, test.y), iter=2, ntree=10) #impute missing
  
  train.x <- x.filled[1:n.train,]
  test.x <- x.filled[(n.train+1):nrow(x.filled),]
  
  yhat1 <- crossvalidate(train.x, train.y, num.folds = 5, train.fun=randomForest, ntree=50, mtry=5)
  yhat2 <- exp(crossvalidate(train.x, log(train.y+1), num.folds = 5, train.fun=randomForest, ntree=100, mtry=5))-1
  
  print(paste("rmse = ", rmse(train.y, yhat1), ", MAPE = ", mape(train.y+1, yhat1)))
  print(paste("rmse = ", rmse(train.y, yhat2), ", MAPE = ", mape(train.y+1, yhat2)))
  
  plot(train.y, yhat1, log="xy", main="no transformation")
  abline(0, 1, col="red")
  
  plot(train.y, yhat2, log="xy", main="target log transformed")
  abline(0, 1, col="blue")
  
  valuation.model <- list(train.ids=train.ids, train.x.na = train.x.na,
                          train.x=train.x, train.y=train.y, train.y.cv = yhat2,
                          test.ids=test.ids, test.x.na = test.x.na, test.x=test.x, test.y=test.y)
  valuation.model$rf.model <- randomForest(train.x, log(train.y+1), ntree=100, mtry=5)
  valuation.model
 
}

predict.valuation <- function(model, test.index){
  test.id <- model$test.ids[test.index]
  test.feature.vector <- model$test.x[test.index,]
  pp <- predict(model$rf.model, rbind(test.feature.vector, model$train.x), nodes=TRUE)
  #pp <- predict(model$rf.model, rbind(test.feature.vector, model$train.x), nodes=TRUE, predict.all=T)
  nodes <- attr(pp, "nodes")
  test.nodes <-  nodes[1,]
  train.nodes <- nodes[-1,]
  neighbor.inds <-  c()
  individual.preds <- c()
  for(i in 1:ncol(train.nodes)){
    tree.neighbors <- which(train.nodes[,i] == test.nodes[i])
    tree.preds <- mean(log(model$train.y[tree.neighbors] + 1))
    individual.preds <- c(individual.preds, tree.preds)
    neighbor.inds <- c(neighbor.inds, tree.neighbors)
  }
  
  
  mean.prediction <- exp(mean(individual.preds))-1
  median.prediction <- exp(median(individual.preds))-1
  range.5.95 <- exp(quantile(individual.preds, probs = c(0.05, 0.95)))-1
    
  neighbor.df <-  my.table(neighbor.inds)
  neighbor.df <- subset(neighbor.df, freq > min(max(freq)/4, 3))
  
  neighbor.ids <- model$train.ids[neighbor.df$ids]
  neighbor.similarities <-  neighbor.df$freq/ncol(nodes)
  
  return(list(value = model$test.y[test.index], mean.prediction=mean.prediction,
              median.prediction=median.prediction, range.5.95=range.5.95,
              neighbor.ids = neighbor.ids, neighbor.similarities=neighbor.similarities,
              neighbor.features = model$train.x[neighbor.df$ids,],
              neighbor.features.na = model$train.x[neighbor.df$ids,],
              neighbor.values=model$train.y[neighbor.df$ids], 
              neighbor.predictions=model$train.y.cv[neighbor.df$ids])) 
          
  
  
}





my.table <- function(vec){
  tt <- tabulate(vec)
  res <- data.frame(ids=which(tt >0), freq=tt[tt >0])
  
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

