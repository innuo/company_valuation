require(shiny)
require(rCharts)
source("modelLoader.R")

test.data <- readRDS("../data/company_test_data.Rdata")


shinyUI(
  fluidPage(
    titlePanel("Beyond Compare: Company Valuation Demo"),
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          'id', 'Test Company', choices = test.data$ids, multiple = FALSE
        ),
        selectizeInput(
          'model.id', 'Model',
          choices = model.choices
        )
      ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Input", tableOutput("table_test")), 
        tabPanel("Valuation", textOutput("text1")), 
        tabPanel("Comps", showOutput("table", "datatables"))
      )
    )
  )
))

# Previous non-tabbed version
# shinyUI(
#   fluidPage(
#     titlePanel("Beyond Compare: Company Valuation Demo"),
#     sidebarLayout(sidebarPanel(
#       selectizeInput(
#         'id', 'Test Company', choices = test.data$ids, multiple = FALSE
#       ),
#       selectizeInput(
#         'model.id', 'Model',
#         choices = model.choices
#       )
#     ),
#     mainPanel(
#       textOutput("text1"),
#       tableOutput("table_test"),
#       tableOutput("table")
#     )
#   )
# ))