library(shiny)

# mendefinisikan ui
ui <- fluidPage(
  titlePanel("App pertama"),

  sidebarLayout( # selalu terdiri siderbarpanel dan mainpanel
    # position = "right",
    sidebarPanel(
      "Side bar",
      h2("sidebar pertama"),
      p("paragraf baru"),
      strong("membuat tebal kalimat"),
      em("italic tulisan"),
      br(),
      code("kelihatan error"),
      div("ya seperti ini lah beda kan", style = "color:green"),
      br(),

      p("ini tulisan biasa", span("khusus ini", style = "color:blue"), "normal")
    ),

    mainPanel(
      "Main panel",
      h1("pertama"),
      h2("kedua", align = "center"),
      img(src = "20200429_214118_0000.png", height = 200, width = 400)
    )
  )
)

# mendefinisikan logika server
server <- function(input, output) {

}

shinyApp(ui = ui, server = server)
