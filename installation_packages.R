install.packages('pak')

# Mencadangkan ------------------------------------------------------------
all_pkg <- list.files(.libPaths()[1])
all_pkg <- all_pkg[all_pkg != '_cache']
write.table(all_pkg, 'all_pkg.txt', sep = '', row.names = F, quote = F)


# Install -----------------------------------------------------------------
ll_pkg <- read.table('all_pkg.txt', header = T)$x

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
