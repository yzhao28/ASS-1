library(shiny)
library(lpSolveAPI)
ui <- navbarPage("Production Plan with Fixed Costs",
             tabPanel("Instructions",
                      mainPanel(
                        h3('Instructions'),
                        h4('This app asks the user to input the values of demand for each month'),
                        h4('There are several other constraints that cannot be changed by user input:'),
                        h4('1. the difference between the storage for the end of month and the quantity of products produce in a month should be equal to the demand for each month. 2. the quantity of products produce in each month should be smaller or equal to two times of the working hours for each month'),
                        h3('the program:'),
                        h4('A manufacturing manager is in charge of minimizing the total costs (raw materials, labor and storage costs) of the following four months. In the following table can be found the cost of raw materials of one unit of final product, the demand of final product and the working hours available for each month. Labor costs are of $12 per hour, and only worked hours are payed. Each unit of final product needs 30 minutes of labor. Storage costs are equal to $2 for each unit stored at the end of the month. Any unit produced at a given month can be used to cover the demand of the same month, or be stored to cover the demand of months to come. At the beginning of month 1 there is no stock, and there are no minimum stock requirements for any month.'),
                        h4('the app can be found at here:'),
                        h3(a('Shiny App For LP', 
                             href="",
                             target="_blank"))
                        )),           
             tabPanel("LP Model",
                      mainPanel(
                        h3('This is the constraints of this question which is created in R'),
                        verbatimTextOutput("model"))),
             
             tabPanel("Optimization",
                      sidebarPanel(
                        h3('Please select values for every variables'),
                        numericInput('Demand1','Demand for month 1 ', 100, min = 0, max = 20000, step = 1),
                        numericInput('Demand2','Demand for month 2', 200, min = 0, max = 20000, step = 1),
                        numericInput('Demand3','Demand for month 3', 150, min = 0, max = 20000, step = 1),
                        numericInput('Demand4','Demand for month 4', 400, min = 0, max = 20000, step = 1),
                        submitButton('Submit')),
                      mainPanel(
                        h3('Results'),
                        h4('Demand 1'),
                        verbatimTextOutput("outDemand1"),
                        h4('Demand 2'),
                        verbatimTextOutput("outDemand2"),
                        h4('Demand 3'),
                        verbatimTextOutput("outDemand3"),
                        h4('Demand 4'),
                        verbatimTextOutput("outDemand4"),
                        h4('output for the variables: q1,q2,q3,q4,s1,s2,s3,s4'),
                        verbatimTextOutput("variables"),
                        h4("the minimum total cost is" ),
                        verbatimTextOutput("objective")))
             )

server <- function(input, output) {
  lprec <- make.lp(8, 8)
  invisible(lp.control(lprec, sense = "min"))
  set.objfn(lprec, c(12,14,16,18,2,2,2,2))
  set.constr.value(lprec, rhs = c(100,200,150,400,400,400,300,300), constraints=seq(1:8))
  set.constr.type(lprec, c(rep("=", 4), rep("<=", 4)))
  set.row(lprec, 1, c(1,0,0,0,-1,0,0,0))
  set.row(lprec, 2, c(0,1,0,0,1,-1,0,0))
  set.row(lprec, 3, c(0,0,1,0,0,1,-1,0))
  set.row(lprec, 4, c(0,0,0,1,0,0,1,-1))
  set.row(lprec, 5, c(1,0,0,0,0,0,0,0))
  set.row(lprec, 6, c(0,1,0,0,0,0,0,0))
  set.row(lprec, 7, c(0,0,1,0,0,0,0,0))
  set.row(lprec, 8, c(0,0,0,1,0,0,0,0))
  set.type(lprec,1,"integer")
  set.type(lprec,2,"integer")
  set.type(lprec,3,"integer")
  set.type(lprec,4,"integer")
  set.type(lprec,5,"integer")
  set.type(lprec,6,"integer")
  set.type(lprec,7,"integer")
  set.type(lprec,8,"integer")
  name.lp(lprec, "Minimum the given Function")
  dimnames(lprec) <- list(c("Constraint1","Constraint 2","Constraint 3","Constraint 4","Constraint 5","Constraint 6","Constraint 7","Constraint 8"), c("q1","q2","q3","q4","s1","s2","s3","s4"))
  output$outDemand1 <- renderPrint({input$Demand1})
  output$outDemand2 <- renderPrint({input$Demand2})
  output$outDemand3 <- renderPrint({input$Demand3})
  output$outDemand4 <- renderPrint({input$Demand4})
  
  output$objective <- renderText({
    set.constr.value(lprec, rhs = c(input$Demand1,input$Demand2,input$Demand3,input$Demand4), constraints=seq(1:4))
    solve(lprec)
    get.objective(lprec)
  })
  
  output$variables <- renderText({
    set.constr.value(lprec, rhs = c(input$Demand1,input$Demand2,input$Demand3,input$Demand4), constraints=seq(1:4))
    solve(lprec)
    get.variables(lprec)
  })  
  
  output$model <- renderPrint({
    set.constr.value(lprec, rhs = c(input$Demand1,input$Demand2,input$Demand3,input$Demand4), constraints=seq(1:4))
    solve(lprec)
    print(lprec)
  })
}

shinyApp(ui = ui, server = server)