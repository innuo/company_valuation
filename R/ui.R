library(shiny)

test.data <- readRDS("../data/company_test_data.Rdata")
model <- readRDS("../data//company_valuation_model.Rdata")


shinyUI(fluidPage(
  titlePanel("Company Valuation Demo"),
  sidebarLayout(sidebarPanel(
    selectizeInput(
      'id', 'Test Company', choices = test.data$ids, multiple = FALSE
    )),
    mainPanel(
      textOutput("text1"),
      tableOutput("table_test"),
      tableOutput("table")
    )
  )
))