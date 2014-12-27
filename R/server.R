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
      
      result <- predict.valuation(model, test.index)
      
      dat <- cbind.data.frame(Name=result$neighbor.ids, 
                              Similarity = result$neighbor.similarities,
                              result$neighbor.features.na)
      dat <- dat[order(dat$Similarity, decreasing=TRUE),]
      dat
    })
    
  }
)