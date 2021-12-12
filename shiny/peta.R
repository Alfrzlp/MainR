library(shiny)
library(rgdal)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinythemes)

indo = readOGR(dsn = "D:/_Datasets/gadm36_IDN_shp", layer = "gadm36_IDN_2", stringsAsFactors = F)

indonesia = fortify(indo, region = "NAME_2")
dataindo = indo@data
colnames(dataindo)[7] = "id"

indonesia = left_join(indonesia, dataindo, by="id")

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Membuat Peta Indonesia per Kabupaten"),
  #--------------
  sidebarLayout(
    
    sidebarPanel(
      h4("Provinsi"),
      strong("Silahkan Pilih Provinsi yang anda inginkan :"),
      selectInput('select', "",
                  choices = sort(unique(indonesia$NAME_1)),
                  selected = 1),
      
      actionButton("update", "Lihat Peta"),
      
      tags$hr()
    ),
  
    #-------------
    mainPanel(
      fluidRow(
        column(8,plotlyOutput("peta", height = 500)),
        column(3, tableOutput("view"))
      ),
  
      #tabsetPanel(type = "tabs",
      #            tabPanel("Plot", plotOutput("plot")),
      #            tabPanel("Summary", verbatimTextOutput("summary")),
      #            tabPanel("Table", tableOutput("table"))
      #)
    )
    #-------------
  )
  #-------------
)

server = function(input, output){
  
  datasetInput <- eventReactive(input$update, {
    indonesia %>% filter(NAME_1 == input$select) %>% rename(kabupaten = id)
  }, ignoreNULL = FALSE)
  
  
  output$peta = renderPlotly({
      p =   ggplot(datasetInput()) + 
            geom_polygon(aes(x=long, y=lat, group=group,fill=kabupaten),
                         col="white") +
            scale_fill_viridis_d("B") + coord_map() +
            theme(legend.position = "none") + 
            labs(title = paste("Provinsi", datasetInput()$NAME_1))+
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank())
      peta = ggplotly(p)
  })
  
  
  output$view = renderTable({
    datasetInput() %>% 
      group_by(kabupaten) %>% count() %>% 
      select(kabupaten) %>% head(., nrow(.))
  })
}

shinyApp(ui, server)

runExample("06_tabsets")
runExample("08_tml")

  
