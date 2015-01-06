require(shiny)
require(rCharts)
test.data <- readRDS("../data/company_test_data.Rdata")
model.choices <- c("Multiple of EBIDTA", "Multiple of Revenue", "Statistical (no industry group)")


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