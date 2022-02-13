library(shiny)
library(rSymPy)
library(shinythemes)
library(MASS)
library(dplyr)

sympyStart()

# membuat semua huruf sebagai variable
for (i in letters) Var(i)
for (i in LETTERS) Var(i)

# fungsi merubah "1/2" menjadi 0.5
ubah <- function(str) {
  return(eval(parse(text = str)))
}

randvar <- function(expr, xmin, xmax, ymin, ymax, new_exp = "1", thd = "x", ds = "kontinu") {
  new <- paste("(", new_exp, ")")
  hasil <- c()
  p <- c("x", "y", "x**2", "y**2", "x*y", new)

  if (ds == "kontinu") {
    op <- "integrate"
    for (i in 1:length(p)) {
      if (tolower(thd) == "x") {
        hasil[i] <- sympy(paste(op, "((", p[i], ")*(", expr, "),(x,", xmin, ",", xmax, "),(y,", ymin, ",", ymax, "))"))
      } else if (tolower(thd) == "y") {
        hasil[i] <- sympy(paste(op, "((", p[i], ")*(", expr, "),(y,", ymin, ",", ymax, "),(x,", xmin, ",", xmax, "))"))
      }
    }
  } else {
    for (i in 1:length(p)) {
      op <- "sum"
      if (tolower(thd) == "x") {
        hasil[i] <- sympy(paste(op, "((", p[i], ")*(", expr, "),(x,", xmin, ",", xmax, "),(y,", ymin, ",", ymax, "))"))
      } else if (tolower(thd) == "y") {
        hasil[i] <- sympy(paste(op, "((", p[i], ")*(", expr, "),(y,", ymin, ",", ymax, "),(x,", xmin, ",", xmax, "))"))
      }
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
#-----------------------------------------------------------------------------------------------------------------



ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Statistika Matematika"),

  tabsetPanel(
    #--------------------------------------------------------------------------------------
    tabPanel(
      "Operasi",
      sidebarLayout(
        sidebarPanel(
          h4("Masukkan Fungsi"),

          textInput("fungsi", "", value = "exp(-x-y)"),
          textInput("x", strong("interval x"), value = "0,oo"),
          textInput("y", strong("Interval y"), value = "0,oo"),
          fluidRow(
            column(width = 8, selectInput("metode", "",
              width = "120px",
              choices = list(
                "Integral" = 1,
                "Differensial" = 2,
                "Sum" = 3,
                "Limit" = 4
              ),
              selected = 1
            )),
            column(width = 4, selectInput("thd", "",
              width = "120px",
              choices = list(
                "x" = 1,
                "y" = 2,
                "x y" = 3,
                "y x" = 4
              ),
              selected = 1
            )),
          ),

          actionButton("tombol", "Hitung", icon = icon("refresh", lib = "glyphicon")),
          helpText("Untuk interval tak hingga silahkan gunakan oo/-oo (2 huruf o) dan untuk pangkat gunakan **"),
          helpText("Sedangkan untuk Limit cukup masukkan 1 angka pada interval x atau y")
        ),
        #----------------------------
        mainPanel(
          tags$h5("Hasil"),
          verbatimTextOutput("hasil")
        )
        #----------------------------
      )
    ), # akhir tabpanel 1---------------------------------------------------------------------------

    #---------------------------------------------------------------------------------------------
    tabPanel(
      "Expetasi dan Varians",
      sidebarLayout(
        sidebarPanel(
          h4("Masukkan Fungsi"),

          textInput("fungsi2", "", value = "24*x*y"),
          textInput("x2", strong("interval x"), value = "0,1-y"),
          textInput("y2", strong("Interval y"), value = "0,1"),
          textInput("new", strong("Expetasi baru"), value = "x/y"),
          selectInput("select2", "",
            choices = list(
              "dx dy" = 1,
              "dy dx" = 2,
              "Diskret x y" = 3,
              "Diskret y x" = 4
            ),
            selected = 1
          ),

          actionButton("tombol2", "Hitung", icon = icon("refresh", lib = "glyphicon")),
          helpText("Untuk interval tak hingga silahkan gunakan oo/-oo (2 huruf o) dan untuk pangkat gunakan **")
        ),
        #----------------------------
        mainPanel(
          tableOutput("table")
        )
        #----------------------------
      )
    ), # akhir tabspanel 2-------------------------------------------------------------------------

    #--------------------------------------------------------------------------------------------
    tabPanel(
      "Transformasi",
      sidebarLayout(
        sidebarPanel(
          h3("Fungsi"), tags$hr(),

          textInput("fungsi3", "", value = "x**3"),
          textInput("x3", strong("interval x"), value = "0,1"),
          textInput("y3", strong("Interval y"), value = "0,1"),

          actionButton("tombol3", "Hitung", icon = icon("refresh", lib = "glyphicon")),
          helpText("Untuk interval tak hingga silahkan gunakan oo/-oo (2 huruf o) dan untuk pangkat gunakan **")
        ),
        #----------------------------
        mainPanel(
          tags$h5("Hasil Transformasi"),
          verbatimTextOutput("hasiltrans")
        )
        #----------------------------
      )
    )
    # akhir tabpanel 3-----------------------------------------------------------------------------
  )
)

server <- function(input, output) {
  # jika tombol diklik

  observeEvent(input$tombol, {
    p <- tolower(isolate(input$fungsi))
    tryCatch(
      {
        yint <- as.vector(strsplit(isolate(input$y), ",")[[1]])
      },
      error = function(a) {
        yint <- 0
      }
    )

    tryCatch(
      {
        xint <- as.vector(strsplit(isolate(input$x), ",")[[1]])
      },
      error = function(a) {
        xint <- 0
      }
    )

    metode <- isolate(input$metode)
    thd <- isolate(input$thd)

    output$hasil <- renderText({
      if (metode == 2 & thd == 1) {
        hasil <- sympy(paste("diff(", p, ", x)"))
      } else if (metode == 2 & thd == 2) {
        hasil <- sympy(paste("diff(", p, ", y)"))
      } else if (metode == 2 & thd >= 3) {
        hasil <- sympy(paste("diff(", p, ", x, y)"))
      } else if (metode == 1 & thd == 1) {
        hasil <- sympy(paste("integrate((", p, "),(x,", xint[1], ",", xint[2], "))"))
      } else if (metode == 1 & thd == 2) {
        hasil <- sympy(paste("integrate((", p, "),(y,", yint[1], ",", yint[2], "))"))
      } else if (metode == 1 & thd == 3) {
        hasil <- sympy(paste("integrate((", p, "),(x,", xint[1], ",", xint[2], "),(y,", yint[1], ",", yint[2], "))"))
      } else if (metode == 1 & thd == 4) {
        hasil <- sympy(paste("integrate((", p, "),(y,", yint[1], ",", yint[2], "),(x,", xint[1], ",", xint[2], "))"))
      } else if (metode == 3 & thd == 1) {
        hasil <- sympy(paste("sum((", p, "),(x,", xint[1], ",", xint[2], "))"))
      } else if (metode == 3 & thd == 2) {
        hasil <- sympy(paste("sum((", p, "),(y,", yint[1], ",", yint[2], "))"))
      } else if (metode == 3 & thd == 4) {
        hasil <- sympy(paste("sum((", p, "),(y,", yint[1], ",", yint[2], "),(x,", xint[1], ",", xint[2], "))"))
      } else if (metode == 3 & thd == 3) {
        hasil <- sympy(paste("sum((", p, "),(x,", xint[1], ",", xint[2], "),(y,", yint[1], ",", yint[2], "))"))
      } else if (metode == 4 & thd == 1) {
        hasil <- sympy(paste("limit(", p, ", x, ", xint[1], ")"))
      } else if (metode == 4 & thd == 2) {
        hasil <- sympy(paste("limit(", p, ", y, ", yint[1], ")"))
      } else if (metode == 4 & thd == 3) {
        hasil <- sympy(paste("limit(limit(", p, ", x, ", xint[1], "), y, ", yint[1], ")"))
      } else if (metode == 4 & thd == 4) {
        hasil <- sympy(paste("limit(limit(", p, ", y, ", yint[1], "), x, ", xint[1], ")"))
      }
    })
  })
  #-------------------------------------------------------------------------
  observeEvent(input$tombol2, {
    p <- tolower(isolate(input$fungsi2))
    new <- tolower(isolate(input$new))
    if (isolate(input$select2) == 1 | isolate(input$select2) == 3) {
      thd <- "x"
    } else {
      thd <- "y"
    }

    if (isolate(input$select2) == 4 | isolate(input$select2) == 3) {
      d <- "diskret"
    } else {
      d <- "kontinu"
    }

    output$table <- renderTable({
      xint <- as.vector(strsplit(isolate(input$x2), ",")[[1]])
      yint <- as.vector(strsplit(isolate(input$y2), ",")[[1]])

      tryCatch(
        {
          table <- randvar(p, xint[1], xint[2], yint[1], yint[2], new, thd, d)
        },
        error = function(a) {
          table <- data.frame(Error = "Terdapat Kesalahan Input")
        }
      )
    })
  })
  #-------------------------------------------------------------------------
}

# run app
shinyApp(ui, server)

ubah("0.71623")

sympy("limit(limit(x*y, x, 8), y, 8)")
sympy("sum(x**2, (x, 1, 3), (y, 1,2))")
sympy("x/2/x - y*2")

rSymPy::Integrate("x**2*y")
sympy("pi.evalf(99990)")
sympy("Eq(2*x**2, y)")
sympy("solve(3*x - 3, x)") # 3x-3 = 0
sympy("solve(Eq(3*x - 7, 3), x)")
