library(shiny)
test.data <- readRDS("../data/company_test_data.Rdata")
model.choices <- c("Multiple of EBIDTA", "Multiple of Revenue", "Statistical (no industry group)")


shinyUI(fluidPage(
  titlePanel("Beyond Compare: Company Valuation Demo"),
  sidebarLayout(sidebarPanel(
    selectizeInput(
      'id', 'Test Company', choices = test.data$ids, multiple = FALSE
    ),
    selectizeInput(
      'model.id', 'Model',
      choices = model.choices
    )),
    mainPanel(
      textOutput("text1"),
      tableOutput("table_test"),
      tableOutput("table")
    )
  )
))