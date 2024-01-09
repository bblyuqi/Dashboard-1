library(shiny)
library(shinydashboard)
library(lmtest)
library(car)
library(ggplot2)
library(DT)
library(rsconnect)

# Data example
example_data <- data.frame(
  Month = month.abb,
  x1 = c(150000, 160000, 170000, 180000, 190000, 200000, 210000, 220000, 230000, 240000, 250000, 260000),
  x2 = c(8000, 9500, 10000, 10500, 11000, 9000, 11500, 12000, 12500, 13000, 14000, 15000),
  x3 = c(5, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5.0, 5.2, 5.3, 5.4, 5.5),
  x4 = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9.0),
  x5 = c(20000, 22000, 25000, 23000, 30000, 28000, 27000, 35000, 40000, 45000, 50000, 60000),
  y = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350)
)

ui <- dashboardPage(
  dashboardHeader(title = "Sales Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Select Data", tabName = "select_data", icon = icon("database")),
      menuItem("Regression & Assumption", tabName = "regression", icon = icon("line-chart"), 
               menuSubItem("Regression Model", tabName = "regression_model"),
               menuSubItem("Assumption Tests", tabName = "assumption_tests")
      ),
      menuItem("Sales Prediction", tabName = "prediction", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "select_data",
              fluidRow(
                column(6, radioButtons("data_choice", "Choose Data Source:", choices = c("Example", "Upload"), selected = "Example")),
                column(6, fileInput("upload_data", "Upload Data (CSV only)", accept = ".csv"))
              ),
              actionButton("load_data_button", "Load Data"),
              verbatimTextOutput("data_summary")
      ),
      tabItem(tabName = "regression_model", 
              fluidRow(
                DTOutput("data_table_regression"),
                column(6, verbatimTextOutput("summary")),
                column(6, plotOutput("residualPlot"))
              )
      ),
      tabItem(tabName = "assumption_tests", 
              fluidRow(
                plotOutput("normalityPlot"),
                verbatimTextOutput("normalityTest"),
                plotOutput("multicollinearityPlot"),
                verbatimTextOutput("multicollinearityTest"),
                plotOutput("heteroskedasticityPlot"),
                verbatimTextOutput("heteroskedasticityTest")
              )
      ),
      tabItem(tabName = "prediction", 
              sidebarLayout(
                sidebarPanel(
                  numericInput("x1", "Number of Visitors", value = 200000),
                  numericInput("x2", "Number of Transactions", value = 10000),
                  numericInput("x3", "Average Items per Transaction", value = 5),
                  sliderInput("x4", "Customer Satisfaction Rating", min = 1, max = 10, value = 8.5),
                  numericInput("x5", "Number of Ads", value = 30000),
                  actionButton("predictButton", "Predict Sales")
                ),
                mainPanel(
                  plotOutput("scatter_plot"),
                  DTOutput("data_table"),
                  verbatimTextOutput("prediction")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # Data placeholder
  data <- reactiveVal()
  
  # Load data based on user choice
  observeEvent(input$load_data_button, {
    if (input$data_choice == "Example") {
      data(example_data)
    } else {
      req(input$upload_data)
      uploaded_data <- read.csv(input$upload_data$datapath, sep = ";")
      data(uploaded_data)
    }
  })
  
  # Render data summary
  output$data_summary <- renderPrint({
    summary(data())
  })
  
  # Render data table for regression model
  output$data_table_regression <- renderDT({
    datatable(data(), options = list(lengthMenu = c(5, 10, 15), pageLength = 5),
              colnames = c("Month", "Number of Visitors", "Number of Transactions", 
                           "Average Items per Transaction", "Customer Satisfaction Rating",
                           "Number of Ads", "Sales (in thousands of USD)"),
              rownames = FALSE)
  })
  
  # Regression Model
  model <- reactive({
    lm(y ~ x1 + x2 + x3 + x4 + x5, data = data())
  })
  
  output$summary <- renderPrint({
    summary(model())
  })
  
  output$residualPlot <- renderPlot({
    plot(model(), which = 1)
  })
  
  output$data_table <- renderDT({
    datatable(data(), options = list(lengthMenu = c(5, 10, 15), pageLength = 5))
  })
  
  # Assumption Tests
  output$normalityTest <- renderPrint({
    shapiro.test(model()$residuals)
  })
  
  output$normalityPlot <- renderPlot({
    ggplot(data(), aes(sample = model()$residuals)) +
      geom_qq() +
      geom_qq_line() +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "lightblue", color = "darkblue")) +
      ggtitle("Normality Assumption")
  })
  
  output$multicollinearityTest <- renderPrint({
    vif(model())
  })
  
  output$multicollinearityPlot <- renderPlot({
    plot(model())
  })
  
  output$heteroskedasticityTest <- renderPrint({
    bptest(model())
  })
  
  output$heteroskedasticityPlot <- renderPlot({
    ggplot(data(), aes(x = fitted(model()), y = sqrt(abs(model()$residuals)))) +
      geom_point() +
      geom_smooth() +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "lightgreen", color = "darkgreen")) +
      ggtitle("Homoskedasticity Assumption")
  })
  
  # Sales Prediction
  output$scatter_plot <- renderPlot({
    ggplot(data(), aes(x = x1, y = y)) +
      geom_point() +
      labs(title = "Scatter Plot - Visitors vs Sales",
           x = "Number of Visitors",
           y = "Sales (in thousands of USD)") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "lightcoral", color = "darkred"))
  })
  
  output$data_table <- renderDT({
    input_data <- data.frame(
      x1 = input$x1,
      x2 = input$x2,
      x3 = input$x3,
      x4 = input$x4,
      x5 = input$x5
    )
    datatable(input_data, options = list(lengthMenu = c(5, 10, 15), pageLength = 5))
  })
  
  observeEvent(input$predictButton, {
    prediction <- predict(model(), newdata = data.frame(x1 = input$x1, x2 = input$x2, x3 = input$x3, x4 = input$x4, x5 = input$x5))
    output$prediction <- renderText({
      paste("Predicted Sales: $", round(prediction, 2))
    })
  })
}


# Run the application
shinyApp(ui, server)