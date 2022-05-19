library(devtools)
create_package("C:/Users/Ridson Alfarizal/Documents/coba")

use_git()

# membuat atau edit fungsi nya
use_r("separator_convert")

# simulates the process of building, installing,
# attaching the package
load_all()


check()

use_mit_license("Ridson Alfarizal")

# menulis dokumntasi fungsi
# taruh cursor di fungsi -> code -> insert roxygen skeleton
document()

?str2vec
?read_img
?dm
?bpjs
?separator_convert

# check lagi setelah tambah dokumentasi
check()

devtools::install_github("")
# final
# jika ada error error retriviewing help
# maka restart r session
install()


# jika pakai package
use_package("stringr")
use_package("reticulate")
use_package("tesseract")
use_package("tidyr")
use_package("dplyr")
use_package("zeallot")
use_package("crayon")

# Data --------------------------------------------------------------------
bpjs <- read.csv("D:/_Datasets/case_cost_prediction_train.csv")
use_data(bpjs)

awal <- pryr::mem_used()
load_all()
pryr::mem_used() - awal

invisible(dplyr::storms)
pryr::mem_used()
pryr::object_size(dplyr::storms)

# membuat fungsi baru
use_r("utils")

# buat readme
use_readme_rmd()
build_readme()
check()


# cara menghubungkan
# buat package dulu
# buat repository github
usethis::use_git()
# In the shell, in Project’s directory, do
# git init
# git remote add origin https://github.com/Alfrzlp/package-dm.git
# git push -u origin (main atau master)


use_devtools()




# nama --------------------------------------------------------------------
available::available("vgwr")
getwd()
usethis::create_package("")

load_all()
# Ctrl+Shift+L

# cari fungsi ctrl + .

# style -------------------------------------------------------------------
# memperbaiki penulisan yang buruk
styler::style_pkg() # restyles an entire R package.
styler::style_dir() # restyles all files in a directory.
usethis::use_tidy_style() # is wrapper that applies one of the above functions depending on whether the current project is an R package or not.
styler::style_file(path = "../all/__Advance R/membuat package.R") # restyles a single file.
styler::style_text() # restyles a character vector.


# edit state --------------------------------------------------------------

# Set an R option	with_options(), local_options()
# Set an environment variable	with_envvar(), local_envvar()
# Change working directory	with_dir(), local_dir()
# Set a graphics parameter	with_par(), local_par()

withr::with_options(
  list(digits = 22),
  print(10.11111)
)

# local_*() functions are best for modifying state
# “from now until the function exits”
g <- function(x, sig_digits) {
  withr::local_options(list(digits = sig_digits))
  print(x)
  # imagine lots of code here
}

# Restore state with
base::on.exit(add = TRUE)

packageStartupMessage("Welcome to my package")




dplyr::tribble(
  ~where, ~english,
  "beach", "US",
  "coast", "US",
  "seashore", "UK",
  "seaside", "UK"
)

withr::local_locale(c("LC_TIME" = "C"))
withr::local_timezone("UTC")
format(Sys.time(), "%Y-%B-%d_%H-%M-%S")



#' Foo bar generic
#'
#' @param x Object to foo.
foobar <- function(x) UseMethod("foobar")

#' @describeIn foobar Difference between the mean and the median
foobar.numeric <- function(x) abs(mean(x) - median(x))

#' @describeIn foobar First and last values pasted together in a string.
foobar.character <- function(x) paste0(x[1], "-", x[length(x)])


#' Basic arithmetic
#'
#' @param x,y numeric vectors.
add <- function(x, y) x + y

#' @rdname add
times <- function(x, y) x * y



# dput --------------------------------------------------------------------
dput_small <- function(x,
                       name = as.character(substitute(x)),
                       multiline = TRUE,
                       n = if ("list" %in% class(x)) length(x) else nrow(x),
                       random = FALSE,
                       seed = 1) {
  name
  if ("tbl_df" %in% class(x)) {
    create_fun <- "tibble::tibble"
  } else
  if ("list" %in% class(x)) {
    create_fun <- "list"
  } else
  if ("data.table" %in% class(x)) {
    create_fun <- "data.table::data.table"
  } else {
    create_fun <- "data.frame"
  }

  if (random) {
    set.seed(seed)
    if (create_fun == "list") {
      x <- x[sample(1:length(x), n)]
    } else {
      x <- x[sample(1:nrow(x), n), ]
    }
  } else {
    x <- head(x, n)
  }

  line_sep <- if (multiline) "\n    " else ""
  cat(
    sep = "", name, " <- ", create_fun, "(\n  ",
    paste0(unlist(
      Map(
        function(item, nm) paste0(nm, if (nm == "") "" else " = ", paste(capture.output(dput(item)), collapse = line_sep)),
        x, if (is.null(names(x))) rep("", length(x)) else names(x)
      )
    ),
    collapse = ",\n  "
    ),
    if (create_fun == "data.frame") ",\n  stringsAsFactors = FALSE)" else "\n)"
  )
}
