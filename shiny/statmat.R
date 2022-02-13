library(shiny)
library(rSymPy)
library(shinythemes)
library(MASS)
sympyStart()
x <- Var("x")
y <- Var("y")

ubah <- function(str) {
  return(eval(parse(text = str)))
}

randvar <- function(expr, xmin, xmax, ymin, ymax, new_exp = "1", thd = "x", ds = "kontinu") {
  new <- paste("(", new_exp, ")")
  hasil <- c()
  p <- c("x", "y", "x**2", "y**2", "x*y", new)

  if (ds == "kontinu") {
    op <- "integrate"
  } else {
    op <- "sum"
  }

  for (i in 1:length(p)) {
    if (tolower(thd) == "x") {
      hasil[i] <- sympy(paste(op, "((", p[i], ")*(", expr, "),(x,", xmin, ",", xmax, "),(y,", ymin, ",", ymax, "))"))
    } else if (tolower(thd) == "y") {
      hasil[i] <- sympy(paste(op, "((", p[i], ")*(", expr, "),(y,", ymin, ",", ymax, "),(x,", xmin, ",", xmax, "))"))
    }
  }
  varx <- ubah(hasil[3]) - ubah(hasil[1])^2
  vary <- ubah(hasil[4]) - ubah(hasil[2])^2
  covxy <- ubah(hasil[5]) - ubah(hasil[1]) * ubah(hasil[2])
  p <- covxy / (sqrt(varx) * sqrt(vary))

  vary <- as.character(fractions(vary))
  varx <- as.character(fractions(varx))
  covxy <- as.character(fractions(covxy))
  p <- as.character(fractions(p))

  fx <- sympy(paste(op, "((", expr, "),(y,", ymin, ",", ymax, "))"))
  fy <- sympy(paste(op, "((", expr, "),(x,", xmin, ",", xmax, "))"))

  fx1y <- sympy(paste0("simplify(", expr, "/(", op, "((", expr, "),(x,", xmin, ",", xmax, "))))"))
  fy1x <- sympy(paste0("simplify(", expr, "/(", op, "((", expr, "),(y,", ymin, ",", ymax, "))))"))

  Ex1y <- sympy(op, "(x*(", fx1y, "), (x, ", xmin, ",", xmax, "))")
  Ex21y <- sympy(op, "((x**2)*(", fx1y, "), (x, ", xmin, ",", xmax, "))")
  Ey1x <- sympy(op, "(y*(", fy1x, "), (y, ", ymin, ",", ymax, "))")
  Ey21x <- sympy(op, "((y**2)*(", fy1x, "), (y, ", ymin, ",", ymax, "))")

  n <- gsub(" ", "", paste("E(", toupper(new_exp), ")"))
  Nama <- c(
    "E(X)", "E(Y)", "E(X^2)", "E(Y^2)", "E(XY)", n, "Var(X)", "Var(Y)", "Cov(X,Y)",
    "Pxy", "f(x)", "f(y)", "f(x|y)", "f(y|x)", "E(X|y)", "E(X^2|y)", "E(Y|x)", "E(Y^2|x)"
  )
  hasil <- c(hasil, c(varx, vary, covxy, p, fx, fy, fx1y, fy1x, Ex1y, Ex21y, Ey1x, Ey21x))

  output <- data.frame(Nama, Nilai = hasil)
  return(output)
}

ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Statistika Matematika"),

  sidebarLayout(
    sidebarPanel(
      h4("Masukkan fungsi"),
      p("Silahkan Masukkan fungsi anda disini"),
      code("Gunakan ** untuk pangkat"),
      textInput("fungsi",
        "",
        value = "x**2"
      ),
      textInput("x",
        strong("interval x"),
        value = "0,1"
      ),
      textInput("y",
        strong("Interval y"),
        value = "0,1"
      ),
      selectInput("select", "",
        choices = list(
          "integral dx dy" = 1,
          "integral dy dx" = 2,
          "differensial thd x" = 3,
          "differensial thd y" = 4,
          "differensial thd x y" = 5,
          "sum x sum y" = 6,
          "sum y sum x" = 7
        ),
        selected = 1
      ),

      actionButton("tombol", "Hitung", icon = icon("paper-plane"))
    ),
    #----------------------------
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Operasi",
          tags$h5("Hasil"),
          verbatimTextOutput("hasil")
        ),
        tabPanel(
          "Expetasi dan Varians",
          tableOutput("table")
        ),
        tabPanel(
          "Transformasi",
          tableOutput("tabl")
        )
      )
    )
    #----------------------------
  )
)

server <- function(input, output) {
  # jika tombol diklik

  observeEvent(input$tombol, {
    p <- tolower(isolate(input$fungsi))
    tryCatch(
      {
        xint <- as.vector(strsplit(isolate(input$x), ",")[[1]])
        yint <- as.vector(strsplit(isolate(input$y), ",")[[1]])
      },
      error = function(a) {
        xint <- 0
        yint <- 0
      }
    )

    output$hasil <- renderText({
      if (input$select == 3) {
        hasil <- sympy(paste("diff(", p, ", x)"))
      } else if (input$select == 4) {
        hasil <- sympy(paste("diff(", p, ", y)"))
      } else if (input$select == 5) {
        hasil <- sympy(paste("diff(", p, ", x, y)"))
      } else if (input$select == 1) {
        hasil <- sympy(paste("integrate((", p, "),(x,", xint[1], ",", xint[2], "),(y,", yint[1], ",", yint[2], "))"))
      } else if (input$select == 2) {
        hasil <- sympy(paste("integrate((", p, "),(y,", yint[1], ",", yint[2], "),(x,", xint[1], ",", xint[2], "))"))
      } else if (input$select == 7) {
        hasil <- sympy(paste("sum((", p, "),(y,", yint[1], ",", yint[2], "),(x,", xint[1], ",", xint[2], "))"))
      } else if (input$select == 6) {
        hasil <- sympy(paste("sum((", p, "),(x,", xint[1], ",", xint[2], "),(y,", yint[1], ",", yint[2], "))"))
      }
    })
  })

  observeEvent(input$tombol2, {
    p <- tolower(isolate(input$fungsi2))

    output$table <- renderTable({
      xint <- as.vector(strsplit(isolate(input$x2), ",")[[1]])
      yint <- as.vector(strsplit(isolate(input$y2), ",")[[1]])
      table <- randvar(p, xint[1], xint[2], yint[1], yint[2], 1, "x")
    })
  })
}

shinyApp(ui, server)

runExample("07_widgets")
devtools::install_github("rasyidstat/indonesia")
library(indonesia)
id_map("indonesia", "kota")
hai()
