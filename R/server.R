require(shiny)
require(rCharts)
require(ggplot2)
require(d3Network)
source("learn_predict.R")
source("modelLoader.R")

test.data <- readRDS("../data/company_test_data.Rdata")



shinyServer(
  function(input, output) {
      output$text1 <- renderText({ 
        id <- input$id
        test.index <- which(test.data$ids == id)
        
        model.id <- input$model.id
        model <- models[[which(model.choices == model.id)]]
        
        result <- predict(model, test.index)
        paste(
              "Predicted value $", round(result$mean.prediction,0), 
              " versus actual value of $", round(result$value,0)
        )
      })
    
      #output$table <- renderTable({
      output$table <- renderChart2({
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
        #dat
        dTable(dat, sPaginationType = "two_button")
      })
    
      output$table_test <- renderTable({
        id <- input$id
        test.index <- which(test.data$ids == id)
      
        dat <- cbind.data.frame(Name=test.data$ids[test.index], test.data$data[test.index,])
        dat
      })
      
      output$errorPlot <- renderPlot({
        df <- make.model.error.df()
        ggplot(na.omit(df), aes(factor(Model), PercentageError)) +
          geom_boxplot(aes(fill=factor(Model))) + 
          coord_cartesian(ylim = c(0, 800))
        })
      
      output$networkPlot <- renderPrint({
        id <- input$id
        test.index <- which(test.data$ids == id)
        
        model.id <- input$model.id
        model <- models[[which(model.choices == model.id)]]
        
        result <- predict(model, test.index)
        
        neighbor.inds <- match(result$neighbor.ids, train.data$ids)
        nodes <- data.frame(name=result$neighbor.ids, 
                            group=train.data$data$Industry.Group[neighbor.inds],stringsAsFactors = FALSE)
        nodes$ID <- 1:nrow(nodes)
        
        nodes <- rbind(nodes, data.frame(name=id, group=test.data$data$Industry.Group[test.index],ID = nrow(nodes)+1))
        
        links <- data.frame(source=nrow(nodes), target=1:(nrow(nodes)-1), value=result$neighbor.similarities)
        
        d3ForceNetwork(Nodes = nodes, 
                       Links = links,  
                       Source = "source", Target = "target", 
                       Value = "value", NodeID = "name", 
                       Group = "group", width = 400, height = 500, 
                       opacity = 0.9, standAlone = FALSE,
                       parentElement = '#networkPlot')
      })
      
  }
)