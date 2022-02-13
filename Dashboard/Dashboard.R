library(shiny)
library(shinydashboard)


# Header ------------------------------------------------------------------
header <- dashboardHeader(title = "Dashboard")
# Sidebar -----------------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th"))
  )
)
# Body --------------------------------------------------------------------
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "dashboard",
      h2("Data Pertanian Provinsi Jawa Barat"),
      fluidRow(
        infoBox("Total Panen", 10 * 2, icon = icon("seedling"), fill = TRUE, color = "light-blue"),
        infoBoxOutput("progressBox2"),
        infoBoxOutput("approvalBox2"),
        infoBox("Luas Lahan", 10 * 2, icon = icon("map"), fill = TRUE, color = "olive"),
        infoBox("Produktivitas", 10 * 2, icon = icon("percent"), fill = TRUE, color = "orange"),
      ),
      fluidRow(
        leafletOutput("mymap"),
        tableOutput("tabel")
      ),
    ),

    tabItem(
      tabName = "widgets",
      h2("Widgets tab content")
    )
  )
)



# UI and Server -----------------------------------------------------------
ui <- dashboardPage(
  skin = "black",
  header, sidebar, body
)

server <- function(input, output) {
  output$mymap <- renderLeaflet({
    m
  })
}


# run ---------------------------------------------------------------------
shinyApp(ui, server)
