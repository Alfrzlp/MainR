library(shiny)
library(shinydashboard)
library(plotly)
library(leaflet)
library(rgdal)
library(dplyr)
library(coronavirus)
library(rjson)
update_dataset()

#filter negara indonesia dan ambil tanggal mulai dari tgl 1 maret
#karena kasus positif pertama di indonesia mulai tanggal 2 maret

#select -> kita ambil hanya 3 kolom ini, karena kolom lain tidak terlalu penting
#pivor_wider -> mengubah kolom type dan cases menjadi kolom confirmed, death, recovered (dengan library tidyr)

covid_indo = coronavirus %>%
  filter(country == "Indonesia", date >= "2020-03-01") %>% 
  dplyr::select(date, type, cases) %>%
  tidyr::pivot_wider(names_from = type, values_from = cases)

meninggal_terbanyak = covid_indo[which.max(covid_indo$death),]

#-------------------------------------------------------------------------------------
indo = readOGR(dsn = "D:/_Datasets/SHP Indonesia", layer = "prov")

data = fromJSON(file="https://api.kawalcorona.com/indonesia/provinsi/")

df = matrix(data, nrow=34, ncol=6, byrow=T) %>% data.frame()

colnames(df) = c("FID", "KODE", "NAME_1", "Kasus_pos", "Kasus_sem", "Kasus_meni")
glimpse(df)

df = df %>% mutate_at(-3, as.numeric) %>% mutate_at(3, as.character)

indo@data = left_join(indo@data, df, by="KODE")
indo@data
#-------------------------------------------------------------------------------------
bins = c(0,10,20,30,40,50,60,70,80,90,100)
pal2 = colorBin("viridis", domain = indo$KODE, bins = bins)

pal1 = colorNumeric("OrRd", domain = indo$Kasus_sem)

mytext = paste(
  "Provinsi :",indo$NAME_1.x,"<br/>",
  "Positif :", indo$Kasus_pos,"<br/>",
  "Sembuh :", indo$Kasus_sem,"<br/>",
  "Meninggal :", indo$Kasus_meni,"<br/>"
) %>% lapply(htmltools::HTML)

#------------------------------------------------------------------------------------
ui <- fluidPage(
  dashboardPage(skin = "blue",
                dashboardHeader(title = "Informasi Virus Corona", titleWidth = 600),
                #-------------------------------
                dashboardSidebar(
                  sidebarMenu(
                    menuItem("Apa itu Covid-19", tabName = "penjelasan1", icon = icon("question-circle")),
                    menuItem("chart", icon = icon("bar-chart-o"),
                             menuSubItem("Sebaran di Indonesia",
                                         tabName = "chart1",
                                         icon = icon("line-chart")),
                             
                             menuSubItem("Sebaran di Dunia",
                                         tabName = "chart2",
                                         icon = icon("line-chart"))
                    ), #menu item 2
                    menuItem("Database", tabName = "db", icon = icon("database"))
                  )#sidebar menu
                ), 
                #-------------------------------
                dashboardBody(
                  tabItems(
                    tabItem("penjelasan1", h4("Merupakan suatu jenis virus")),
                    tabItem(tabName = "chart1",
                            # First Row
                            fluidRow(box(title = "Peta Indonesia", width = 12,
                                         leafletOutput("peta"),
                                         selectInput('select', label="Pilih kasus", width = "200px",
                                                     choices = list("Meninggal" = 1,
                                                                    "Sembuh" = 2,
                                                                    "Dalam Perawatan" = 3),
                                                     selected = 1)),
                                     
                                     box(width = 3, background = "green",
                                         h4("Sembuh"),
                                         tags$hr(),
                                         h3(sum(covid_indo$recovered))),
                                     box(width = 3, background = "yellow",
                                         h4("Dalam Perawatan"),
                                         tags$hr(),
                                         h3(sum(covid_indo$confirmed)-sum(covid_indo$death)-sum(covid_indo$recovered))),
                                     box(width = 3, background = "navy",
                                         h4("Meninggal"),
                                         tags$hr(),
                                         h3(sum(covid_indo$death))),
                                     box(width = 3, background = "red",
                                         h4("Total Kasus"),
                                         tags$hr(),
                                         h3(sum(covid_indo$confirmed))),
                                     box(title = "Perbandingan Kasus", plotlyOutput("plot1", height = 320), width = 12),
                                     box(title = "Kasus Sembuh Terbanyak", width = 4, background = "green"),
                                     box(title = "Kasus Positif Terbanyak", width = 4, background = "orange"),
                                     box(title = "Kasus Meninggal Terbanyak", width = 4, background = "maroon"),
                                     box(title = "Perbandingan Kasus Korona Beberapa Negara", plotlyOutput("plot2", height = 250),
                                         width=6, solidHeader = F),
                                     box(title = "Hubungan Kasus Positif dan Meninggal", plotlyOutput("plot3", height = 250), width = 6
                                     ))),
                    tabItem(tabName = "chart2",
                            # First Row
                            fluidRow(box(title = "Box with a plot", plotlyOutput("plot4", height = 450)), width = 12)),
                    tabItem(tabName = "db",
                            # First Row
                            fluidRow(tabBox(id="tabchart1",
                                            tabPanel("World",DT::dataTableOutput("Tab1", height = "450px"), width = 9),
                                            tabPanel("Indonesia",DT::dataTableOutput("Tab2", height = "450px"), width = 9), width = 12)))
                  )
                )
                #-------------------------------
  )#dasboardpage
  
)#fluid


server <- function(input, output){
  
  output$peta <- renderLeaflet({
      leaflet(indo) %>% addTiles() %>% 
      setView(lng = 118, lat=-2.5, 4.5) %>% 
      addPolygons(weight = 2,
                  color = "white", #warna garis
                  fillColor = ~pal1(indo$Kasus_sem),
                  fillOpacity = 0.9, #ketebalan warna
                  label = mytext,
                  labelOptions = labelOptions(
                    style = list("font-weight"="normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto",
                  ),
                  highlightOptions = highlightOptions(color = "black",
                                                      weight = 2,
                                                      bringToFront = T)) %>% 
      addLegend(pal = pal1, values = indo$Kasus_sem,
                title = "Total Kasus",
                position = "bottomleft")
    })
  #--------------------------------------------------------------------------
  
  output$plot1 <- renderPlotly({
    plot_ly(
      data = covid_indo,
      x=~date, 
      y=~confirmed, 
      type = "bar",
      name = "Kasus Terkonfirmasi" ,
      marker = list(color = "skyblue")
    ) %>%  add_trace(
      x=~date, 
      y=~recovered, 
      type = "scatter", 
      mode = "markers+lines",
      name = "Kasus Sembuh",
      line = list(color = "forestgreen"),
      marker = list(color = "forestgreen")
    ) %>%  add_trace(
      x=~date, 
      y=~death, 
      type = "scatter", 
      mode = "markers+lines",
      name = "Kasus Meninggal",
      line = list(color = "black"),
      marker = list(color = "black")
    ) %>% layout(
      title = "Covid-19 in Indonesia",
      yaxis = list(title = "Kasus Positif"),
      xaxis = list(title = "Tanggal"),
      hovermode = "compare",
      legend = list(x = 0.1, y = 0.9)
    )%>% add_annotations(
      x = as.Date("2020-03-02"),
      y = 2,
      text = paste("Kasus Pertama"),
      arrowhead = 5, #besar kepala
      arrowsize = 1,
      showarrow = TRUE,
      ax = -10,
      ay = -60
    ) %>% add_annotations(
      x = as.Date("2020-03-11"),
      y = 1,
      text = paste("Kematian Pertama"),
      arrowhead = 5,
      showarrow = TRUE,
      ax = 50,
      ay = -90
    )
  })
  #--------------------------------------------------------------------------  
  
}


shinyApp(ui, server)

