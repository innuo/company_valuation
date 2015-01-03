library(shiny)
source("learn_predict.R")

shinyServer(
  function(input, output) {
    
     
    output$text1 <- renderText({ 
      paste("You have selected", input$id)
    })
    
    output$table <- renderTable({
      id <- input$id
      test.index <- which(test.data$ids == id)
      
      result <- predict(model, test.index)
      
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