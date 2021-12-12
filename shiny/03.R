library(shiny)

ui = fluidPage(
  titlePanel("Cencus Vis"),
  
  #----------------------------------------
  sidebarLayout(
    
    sidebarPanel(
      helpText("Create demographic maps with information from the 2010 US"),
      
      selectInput("var",
                  label = "Choice a Variable to Display",
                  choices = c("Percent White",
                              "Percent Black",
                              "Percent Hispanic",
                              "Percent Asian"),
                  selected = "Percent White"),
      
      sliderInput("range",
                  label = "Range of interest :",
                  min = 0, max = 100, value = c(0,100)
                  )
    ),
    #--------------------------------------
    mainPanel(
      textOutput("Selected_Var"),
      textOutput("min_max")
    )
    #--------------------------------------
  )
)

server = function(input, output){
  output$Selected_Var = renderText({
    paste("You have selected this", input$var)
  })
  
  output$min_max = renderText({
    paste("Range", input$range[1], "to", input$range[2])
  })
}

shinyApp(ui = ui, server = server)

runApp("C:/Users/Ridson Alfarizal/Documents/Main R/shiny/cencus-app",
       display.mode = "showcase")
