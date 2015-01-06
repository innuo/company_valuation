train.data <-  readRDS("data/company_train_data.Rdata")
test.data <-  readRDS("data/company_test_data.Rdata")
source("R/learn_predict.R")

train.all.models()
runApp("R/", launch.browser = TRUE)