library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(coronavirus)
update_dataset()


ui <- fluidPage(
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Informasi Virus Corona", titleWidth = 600),
    #-------------------------------
    dashboardSidebar(
      sidebarMenu(
        menuItem("Apa itu Covid-19", tabName = "penjelasan1", icon = icon("question-circle")),
        menuItem("chart",
          icon = icon("bar-chart-o"),
          menuSubItem("Sebaran di Indonesia",
            tabName = "chart1",
            icon = icon("line-chart")
          ),

          menuSubItem("Sebaran di Dunia",
            tabName = "chart2",
            icon = icon("line-chart")
          )
        ), # menu item 2
        menuItem("Database", tabName = "db", icon = icon("database"))
      ) # sidebar menu
    ),
    #-------------------------------
    dashboardBody(
      tabItems(
        tabItem("penjelasan1", h4("Merupakan suatu jenis virus")),
        tabItem(
          tabName = "chart1",
          # First Row
          fluidRow(
            box(title = "Perbandingan Kasus positif dan Meninggal", plotlyOutput("plot1", height = 250), width = 12),
            box(
              title = "Perbandingan Kasus Korona Beberapa Negara", plotlyOutput("plot2", height = 250),
              width = 6, solidHeader = F
            ),
            box(title = "Hubungan Kasus Positif dan Meninggal", plotlyOutput("plot3", height = 250))
          )
        ),
        tabItem(
          tabName = "chart2",
          # First Row
          fluidRow(box(title = "Box with a plot", plotlyOutput("plot4", height = 450)), width = 12)
        ),
        tabItem(
          tabName = "db",
          # First Row
          fluidRow(tabBox(
            id = "tabchart1",
            tabPanel("World", DT::dataTableOutput("Tab1", height = "450px"), width = 9),
            tabPanel("Indonesia", DT::dataTableOutput("Tab2", height = "450px"), width = 9), width = 12
          ))
        )
      )
    )
    #-------------------------------
  ) # dasboardpage
) # fluid

server <- shinyServer(function(input, output, session) {

  # membandingkan kasus beberapa negara
  output$plot2 <- renderPlotly({
    df <- coronavirus %>%
      dplyr::group_by(country, type) %>%
      dplyr::summarise(total = sum(cases)) %>%
      tidyr::pivot_wider(
        names_from = type,
        values_from = total
      )

    # membandingkan kasus
    konfirmasi_harian <- coronavirus %>%
      dplyr::filter(type == "confirmed") %>%
      dplyr::filter(date >= "2020-06-25") %>%
      dplyr::mutate(country = country) %>%
      dplyr::group_by(date, country) %>%
      dplyr::summarise(total = sum(cases)) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = country, values_from = total)
    konfirmasi_harian %>%
      plotly::plot_ly() %>%
      plotly::add_trace(
        x = ~date,
        y = ~Austria,
        type = "scatter",
        mode = "lines+markers",
        name = "Austria"
      ) %>%
      plotly::add_trace(
        x = ~date,
        y = ~Philippines,
        type = "scatter",
        mode = "lines+markers",
        name = "Philippines"
      ) %>%
      plotly::add_trace(
        x = ~date,
        y = ~Singapore,
        type = "scatter",
        mode = "lines+markers",
        name = "Singapore"
      ) %>%
      plotly::add_trace(
        x = ~date,
        y = ~Indonesia,
        type = "scatter",
        mode = "lines+markers",
        name = "Indonesia"
      ) %>%
      plotly::layout(
        title = "",
        legend = list(x = 0.1, y = 0.9),
        yaxis = list(title = "Kasus Positif"),
        xaxis = list(title = "Tanggal"),
        hovermode = "compare",
        margin = list(
          b = 10,
          t = 10,
          pad = 2
        )
      )
  })

  ## Plot terkonfirmasi positif dan meninggal
  output$plot1 <- renderPlotly({ ## tampilkan data
    df <- coronavirus %>%
      dplyr::filter(country == "Indonesia") %>%
      dplyr::group_by(country, type) %>%
      dplyr::summarise(total = sum(cases))
    ## Panggil nama tabel
    # untuk mempercantik tabel
    df <- coronavirus %>%
      # dplyr::filter(date == max(date)) %>%
      dplyr::filter(country == "Indonesia") %>%
      dplyr::group_by(country, type) %>%
      dplyr::summarise(total = sum(cases)) %>%
      tidyr::pivot_wider(
        names_from = type,
        values_from = total
      )

    # melihat kasus corona perhari
    ## Data Harian
    df_harian <- coronavirus %>%
      dplyr::filter(country == "Indonesia") %>%
      dplyr::filter(date >= "2020-03-01") %>%
      dplyr::group_by(date, type) %>%
      dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
      tidyr::pivot_wider(
        names_from = type,
        values_from = total
      ) %>%
      dplyr::arrange(date) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(active = confirmed - death) %>%
      dplyr::mutate(
        confirmed_cum = cumsum(confirmed),
        death_cum = cumsum(death),
        active_cum = cumsum(active)
      )

    ## Plot Data Harian (mati dan terkonfirmasi)
    confirmed_color <- "purple"
    active_color <- "#7FFFD4"
    recovered_color <- "forestgreen"
    death_color <- "##B22222"
    plotly::plot_ly(data = df_harian) %>%
      plotly::add_trace(
        x = ~date,
        # y = ~active_cum,
        y = ~confirmed_cum,
        type = "scatter",
        mode = "lines+markers",
        name = "Terkonfirmasi Positif",
        line = list(color = active_color),
        marker = list(color = active_color)
      ) %>%
      plotly::add_trace(
        x = ~date,
        y = ~death_cum,
        type = "scatter",
        mode = "lines+markers",
        name = "Meninggal",
        line = list(color = death_color),
        marker = list(color = death_color)
      ) %>%
      plotly::add_annotations(
        x = as.Date("2020-03-02"),
        y = 1,
        text = paste("Kasus Pertama"),
        xref = "x",
        yref = "y",
        arrowhead = 5,
        arrowhead = 3,
        arrowsize = 1,
        showarrow = TRUE,
        ax = -10,
        ay = -80
      ) %>%
      plotly::add_annotations(
        x = as.Date("2020-03-11"),
        y = 3,
        text = paste("Kematian Pertama"),
        xref = "x",
        yref = "y",
        arrowhead = 5,
        arrowhead = 3,
        arrowsize = 1,
        showarrow = TRUE,
        ax = -90,
        ay = -90
      ) %>%
      plotly::layout(
        title = "",
        yaxis = list(title = "Kasus Positif"),
        xaxis = list(title = "Tanggal"),
        legend = list(x = 0.1, y = 0.9),
        hovermode = "compare"
      )
  })

  ## tabel selanjutnya jika ingin dimasukkan
  output$plot4 <- renderPlotly({
  })

  ## tabel 1
  output$Tab1 <- DT::renderDataTable(DT::datatable({
    data <- coronavirus
  }))
  ## tabel 2
  output$Tab2 <- DT::renderDataTable(DT::datatable({
    # filter indo
    df_indo <- coronavirus %>%
      dplyr::filter(country == "Indonesia") %>%
      dplyr::filter(date >= "2020-03-01") %>%
      dplyr::group_by(date, type) %>%
      dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
      tidyr::pivot_wider(
        names_from = type,
        values_from = total
      ) %>%
      dplyr::arrange(date) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(active = confirmed - death) %>%
      dplyr::mutate(
        confirmed_cum = cumsum(confirmed),
        death_cum = cumsum(death),
        active_cum = cumsum(active)
      )
    data <- df_indo
  }))

  ## Plotly Scatter Plot
  output$plot3 <- renderPlotly({
    df_indo <- coronavirus %>%
      dplyr::filter(country == "Indonesia", date >= "2020-03-01") %>%
      dplyr::group_by(date, type) %>%
      dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
      tidyr::pivot_wider(
        names_from = type,
        values_from = total
      ) %>%
      dplyr::arrange(date) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(active = confirmed - death) %>%
      dplyr::mutate(
        confirmed_cum = cumsum(confirmed),
        death_cum = cumsum(death),
        active_cum = cumsum(active)
      )

    plot_ly(
      data = df_indo,
      x = ~confirmed,
      y = ~death, color = "#8FBC8F",
      type = "scatter",
      mode = "markers"
    )
  })
})
shinyApp(ui, server)

runApp("C:\\Users\\Ridson Alfarizal\\Documents\\Main R\\shiny\\covid-app")
