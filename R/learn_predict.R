library(randomForest)

learn.model <- function(train.data, test.data, train.fun, model.name){
  model <- train.fun(train.data, test.data)
  saveRDS(model, paste0("data/", model.name, ".Rdata"))

}

train.industry.rev.multiple <- function(train.data, test.data){
  model <- train.industry.multiple(train.data, train.targets, "Revenues")
}

train.industry.ebitda.multiple <- function(train.data, test.data){
  yhat1 <- crossvalidate(train.data$data, train.data$target, num.folds = 5, 
                         train.fun=train.industry.multiple, predict.fun=predict.industry.multiple.df,
                         base="EBITDA")
  print(paste("rmse = ", rmse(train.data$target, yhat1), ", MAPE = ", mape(train.data$target+1, yhat1)))
  plot(train.data$target, yhat1, log="xy", main="Multiple of EBITDA")
  abline(0, 1, col="red") 
  
  valuation.model <- list(train.ids=train.ids, train.x.na = train.x.na,
                          train.x=train.x, train.y=train.y, train.y.cv = yhat1,
                          test.ids=test.ids, test.x.na = test.x.na, test.x=test.x, test.y=test.y)
  valuation.model$rf.model <- randomForest(train.x, log(train.y+1), ntree=100, mtry=5)
  
  
  model <- train.industry.multiple(train.data, train.data$target, "EBITDA")
}


train.industry.multiple <- function(train.data, train.target, base){
  
  model <- list(train.data=train.data, train.target =train.target, base=base)
  class(model) <- "industry.multiple" 
  print(paste(base, "Industry multiple model trained"))
  model
}

predict.industry.multiple.df <- function(model, newdata){
  yhat <- rep(NA, nrow(newdata))
  base <- model$base
  newdata.base <- newdata[[base]]
  train.data.base <- model$train.data[[base]]
  train.industry.group <- model$train.data$Industry.Group
  newdata.industry.group <- newdata$Industry.Group
  
  multiple <- model$train.target / model$train.data[[base]]
  
  for(i in 1:nrow(newdata)){
    inds <- train.industry.group == newdata.industry.group[i]
    inds <- inds &  - (abs(train.data.base - newdata.base[i])/(newdata.base[i]+0.01) < 0.5)
    inds <- inds & train.data.base > 0
    if(any(inds)){
      yhat[i] <- mean(multiple[inds]) * newdata.base[i]
    }
    else
      yhat[i] <- NA
    
  }
  yhat
}

predict.industry.multiple <- function(model, test.index){
  
}


train.using.random.forest <- function(train.data, test.data){
  train.ids <- train.data$ids
  test.ids <- test.data$ids
  
  train.x.na <- train.data$data
  test.x.na <- test.data$data
  
  n.train <- nrow(train.x.na)
  n.test <-  nrow(test.x.na)
  
  c.train <- ncol(train.x.na)
  
  train.y <- train.data$target
  test.y <- test.data$target
  
  x.filled <- rfImpute(rbind(train.x.na, test.x.na), c(train.y, test.y), iter=2, ntree=10) #impute missing
  
  train.x <- x.filled[1:n.train,-1]
  test.x <- x.filled[(n.train+1):nrow(x.filled),-1]
  
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
  class(valuation.model) <- "rf.no.industry.group"
  valuation.model
  
}


predict.rf.no.industry.group <- function(model, test.index){
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

