## Start with a contingency table.
ftable(Titanic, row.vars = 1:3)
ftable(Titanic, row.vars = 1:2, col.vars = "Survived")
ftable(Titanic, row.vars = 2:1, col.vars = "Survived")

## Start with a data frame.
x <- ftable(mtcars[c("cyl", "vs", "am", "gear")])
x
ftable(x, row.vars = c(2, 4))

## Start with expressions, use table()'s "dnn" to change labels
ftable(mtcars$cyl, mtcars$vs, mtcars$am, mtcars$gear,
  row.vars = c(2, 4),
  dnn = c("Cylinders", "V/S", "Transmission", "Gears")
)



# NOT RUN {
## Agresti (1990), page 157, Table 5.8.
## Not in ftable standard format, but o.k.
file <- tempfile()
cat("             Intercourse\n",
  "Race  Gender     Yes  No\n",
  "White Male        43 134\n",
  "      Female      26 149\n",
  "Black Male        29  23\n",
  "      Female      22  36\n",
  file = file
)
# }
# NOT RUN {
file.show(file)
# }
# NOT RUN {
ft1 <- read.ftable(file)
ft1
unlink(file)

## Agresti (1990), page 297, Table 8.16.
## Almost o.k., but misses the name of the row variable.
file <- tempfile()
cat("                      \"Tonsil Size\"\n",
  "            \"Not Enl.\" \"Enl.\" \"Greatly Enl.\"\n",
  "Noncarriers       497     560           269\n",
  "Carriers           19      29            24\n",
  file = file
)
# }
# NOT RUN {
file.show(file)
# }
# NOT RUN {
ft <- read.ftable(file,
  skip = 2,
  row.var.names = "Status",
  col.vars = list(
    "Tonsil Size" =
      c("Not Enl.", "Enl.", "Greatly Enl.")
  )
)
ft
unlink(file)

ft22 <- ftable(Titanic, row.vars = 2:1, col.vars = 4:3)
write.ftable(ft22, quote = FALSE)
write.ftable(ft22, quote = FALSE, method = "row.compact")
write.ftable(ft22, quote = FALSE, method = "col.compact")
write.ftable(ft22, quote = FALSE, method = "compact")
# }
