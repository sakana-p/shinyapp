library(shiny)

ui <- navbarPage("Optimization",
                 tabPanel("LP Model",
                          mainPanel(
                            h3('The LP formulation below is created in R based on the user inputs and is solved using the 
                               lpSolveAPI package'),
                            verbatimTextOutput("model"))),
                 
                 tabPanel("plant hour Optimization",
                          sidebarPanel(
                            h3('Please select hrs for 3 plants'),
                            numericInput('plant1',' PLANT-1 AVAILABALE HRS', 4, min = 0, max = 200, step = 1),
                            numericInput('plant2',' PLANT-2 AVAILABALE HRS', 12, min = 0, max = 200, step = 1),
                            numericInput('plant3','PLANT-3 TO AVAILABLE HRS', 18, min = 0, max = 200, step = 1),
                            numericInput('product1','PRODUCT-1 cost', 1, min = 0, max = 200, step = 1),
                            numericInput('product2','PRODUCT-2 cost', 1, min = 0, max = 200, step = 1),
                            submitButton('Submit')),
                          mainPanel(
                            h3('Results of optimization'),
                            h4('hours for plant 1'),
                            verbatimTextOutput("outplant1"),
                            h4('hours for plant 2'),
                            verbatimTextOutput("outplant2"),
                            h4('hours for plant 3'),
                            verbatimTextOutput("outplant3"),
                            h4('product-1, product-2 '),
                            verbatimTextOutput("variables"),
                            h4('profit is'),
                            verbatimTextOutput("objective"))))


library(shiny)
library(lpSolveAPI)
server <- function(input, output) {
  
  lprec <- make.lp(3, 2)
  invisible(lp.control(lprec, sense = "max"))
  set.objfn(lprec, c(3000, 5000))
  set.constr.value(lprec, rhs = c(4,12,18), constraints=seq(1:3))
  set.constr.type(lprec, c(rep("<=", 3)))
  set.row(lprec, 1, c(1, 0), indices = c(1, 2))
  set.row(lprec, 2, c(0,2), indices = c(1,2))
  set.row(lprec, 3, c(3,2), indices = c(1,2))
  
  name.lp(lprec, " Optimization")
  dimnames(lprec) <- list(c("plant1","plant2", "plant3"), c("product1","product2"))
  output$outplant1 <- renderPrint({input$plant1}) 
  output$outplant2 <- renderPrint({input$plant2})
  output$outplant3 <- renderPrint({input$plant3})
  output$outproduct1 <- renderPrint({input$product1})
  output$outproduct2 <- renderPrint({input$product2})
  
  output$objective <- renderText({
    set.objfn(lprec,c(input$product1,input$product2))
    set.constr.value(lprec, rhs = c(input$plant1,input$plant2,input$plant3), constraints=seq(1:3))
    solve(lprec)
    get.objective(lprec)
  })
  
  output$variables <- renderText({
    set.objfn(lprec,c(input$product1,input$product2))
    set.constr.value(lprec, rhs = c(input$plant1,input$plant2,input$plant3), constraints=seq(1:3))
    solve(lprec)
    get.variables(lprec)
  })
  
  output$model <- renderPrint({
    set.objfn(lprec,c(input$product1,input$product2))
    set.constr.value(lprec, rhs = c(input$plant1,input$plant2,input$plant3), constraints=seq(1:3))
    solve(lprec)
    print(lprec)
  })
}




shinyApp(ui=ui, server=server)
