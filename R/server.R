library(shiny)
source("learn_predict.R")

models <- list(readRDS("../data/model_ebidta.Rdata"), readRDS("../data/model_revenues.Rdata"),
            readRDS("../data/model_rf.Rdata"))
test.data <- readRDS("../data/company_test_data.Rdata")
model.choices <- c("Multiple of EBIDTA", "Multiple of Revenue", "Statistical (no industry group)")


shinyServer(
  function(input, output) {
     output$text1 <- renderText({ 
      paste("You have selected", input$id)
    })
    
    output$table <- renderTable({
      id <- input$id
      test.index <- which(test.data$ids == id)
      
      model.id <- input$model.id
      model <- models[[which(model.choices == model.id)]]
      
      result <- predict(model, test.index)
      
    #  if(result$)
      
      dat <- cbind.data.frame(Name=result$neighbor.ids, 
                              Similarity = result$neighbor.similarities,
                              result$neighbor.features.na)
      dat <- dat[order(dat$Similarity, decreasing=TRUE),]
      dat
    })
    
    output$table_test <- renderTable({
      id <- input$id
      test.index <- which(test.data$ids == id)
      
      dat <- cbind.data.frame(Name=test.data$ids[test.index], test.data$data[test.index,])
      dat
    })
    
    
  }
)