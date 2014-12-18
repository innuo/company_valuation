library(randomForest)

learn.model <- function(train.data){
  ids <- train.data$ids
  train.x <- train.data$data
  train.y <- train.data$target
  #train.x <- rfImpute(train.x, train.y, iter=2, ntree=10)
  
  ind <- complete.cases(train.x) & !is.na(train.y)
  rf <- randomForest(train.x[ind,], train.y[ind], na.action=na.omit, ntree=50, mtry=5)
  browser()
}


