install.packages('pak')


# -------------------------------------------------------------------------
all_pkg <- list.files('F:/4.2')

sapply(
  all_pkg,
  FUN = function(x) {
    tryCatch(
      pak::pak(x),
      error = function(x) print(x)
    )
  }
)



# github ------------------------------------------------------------------
devtools::install_github('alfrzlp/package-dm')
dm::str2vec('1 2 3')
