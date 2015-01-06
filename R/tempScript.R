train.data <-  readRDS("data/company_train_data.Rdata")
test.data <-  readRDS("data/company_test_data.Rdata")
source("R/learn_predict.R")

model <- learn.model(train.data, test.data)
saveRDS(model,"data/company_valuation_model.Rdata") 
#if you don't want to run the model learning over and over again
a <- predict.valuation(model, 1) 
# 1 means predict on the first test example (from the test data set) a$mean.prediction, a$median.prediction and a$range.5.95 are self explanatory

a$neighbor.ids 
#the training set companies that the model thought were neighbors
a$neighbor.similarities 
#the similarity scores in (0, 1] for each of those neighbors

mean(abs(a$neighbor.values - a$neighbor.predictions)/a$neighbor.values) 
#mean error on the training set neighbors if the same procedure were used for those