library(shiny)

ui <- fluidPage(
  titlePanel("Basic Widgets"),
  #--------------------------
  fluidRow(
    
    column(3, h3("Button"),
           actionButton("action", "Action"),
           br(),
           br(), #jarak
           submitButton("Submit")),
    
    column(3, h3("Single Checkbox"),
           checkboxInput("checkbox", "Choice A", value = T)),
    
    column(3,
           checkboxGroupInput("checkGroup", 
                              h3("Checkbox Group"), #harus ada label
           choices = list("choice 1" = 1,
                          "choice 2" = 2,
                          "choice 3" = 3),
           selected = 2)),
    
    column(2, #column width
           dateInput("date",
                     h3("Date Input"),
                     value = "2020-10-28"))
    
    
  ),
  #--------------------------
  
  fluidRow(
    column(3,
           dateRangeInput("dates",
                          h3("Date Range"))),
    
    column(3,
           fileInput("file", 
                     h3("File Input"))),
    
    column(3,
           h3("Help Text"),
           helpText("Ini namanya help text")),
    
    column(3,
           numericInput("num",
                        h3("Numeric Input"),
                        value = 1))
  ),
  #-------------------------
  
  fluidRow(
    column(3,
           radioButtons("radio",
                        h3("Radio Buttons"),
                        choices = list("choice 1" = 1,
                                       "choice 2" = 2,
                                       "choice 3" = 3),
                        selected = 3)),
    
    column(3,
           selectInput('select', 
                       h3("Select Input"),
                       choices = list("Pilihan 1" = 1,
                                      "Pilihan 2" = 2,
                                      "Pilihan 3" = 3),
                       selected = 1)),
    
    column(3,
           sliderInput("slider1",
                       h3("Sliders 2 Values"),
                       min = 0, max = 100,
                       value = c(25,75)
                       ),
           
           sliderInput("slider2",
                       h3("Sliders 1 value"),
                       min = 0, max = 100,
                       value = 9
                        )
           ),
    
    column(3,
           textInput("text",
                     h3("Text Input"),
                     value = "Enter text...."))
    
   
  )
  #-------------------------
)

server <- function(input, output){
  
}

shinyApp(ui = ui, server = server)
