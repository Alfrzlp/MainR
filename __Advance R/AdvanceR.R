
# awal --------------------------------------------------------------------
library(lobstr)

x <- 1:10
# membinding nilai 1:10 ke x dan y, bukan copy
y <- x
obj_addr(x)
obj_addr(y)
# jika y ada modifikasi baru y di binding dengan yang lain

mean
base::mean
get("mean")
evalq(mean)
match.fun("mean")

x <- c("a", "a", "abc", "d")
ref(x, character = TRUE)


# size --------------------------------------------------------------------
obj_size(letters)
obj_size(ggplot2::diamonds)

# list malah lebih kecil
x <- runif(1e6)
obj_size(x)

y <- list(x, x, x)
obj_size(y)

# R menggunakan global string pool sehingga minim memory
# meskipun diulang2
banana <- "bananas bananas bananas"
obj_size(banana)
# tidak berukuran 100*obj_size(banana)
obj_size(rep(banana, 100))

# ALTREP, short for alternative representation
# R menyimpan angka pertama dan terakhir saja
obj_size(1:3)
obj_size(1:1e3)
obj_size(1:1e6)
obj_size(1:1e9)


# vector ------------------------------------------------------------------
# ada atomic vector dan list
# c selalu atomic
c(c(1, 2), c(3, 4))

NA ^ 0
NA | TRUE
NA & FALSE

x <- c(NA, 5, NA, 10)
x == NA
# yang benar
is.na(x)

x <- c(FALSE, FALSE, TRUE)
as.numeric(x)

e1 <- rlang::env(a = 1, b = 2, c = 3)
e2 <- e1
e1


# date times --------------------------------------------------------------
# POSIX” is short for Portable Operating System Interface,
# which is a family of cross-platform standards.
# “ct” stands for calendar time (the time_t type in C),
# and “lt” for local time (the struct tm type in C)

now_ct <- as.POSIXct("2018-08-01 22:00", tz = "UTC")
now_ct

typeof(now_ct)
attributes(now_ct)


# Durations ---------------------------------------------------------------
one_week_1 <- as.difftime(1, units = "weeks")
one_week_1

typeof(one_week_1)
attributes(one_week_1)

one_week_2 <- as.difftime(7, units = "days")
one_week_2

typeof(one_week_2)
attributes(one_week_2)


l5 <- c(list(1, 2), c(3, 4))
str(l5)


# list --------------------------------------------------------------------
x <- list(
  a = list(1, 2, 3),
  b = list(3, 4, 5)
)

purrr::pluck(x, "a", 1)
purrr::pluck(x, "c", 1)
purrr::pluck(x, "c", 1, .default = NA)


# logika ------------------------------------------------------------------

x <- 1:10
y <- 11:20

(x1 <- 1:10 %% 2 == 0)
(x2 <- which(x1))
(y1 <- 1:10 %% 5 == 0)
(y2 <- which(y1))

x1 & y1
intersect(x2, y2)

x1 | y1
union(x2, y2)

x1 & !y1
setdiff(x2, y2)

xor(x1, y1)
setdiff(union(x2, y2), intersect(x2, y2))


# switch ------------------------------------------------------------------
x_option <- function(x) {
  if (x == "a") {
    "option 1"
  } else if (x == "b") {
    "option 2"
  } else if (x == "c") {
    "option 3"
  } else {
    stop("Invalid `x` value")
  }
}

x_option <- function(x) {
  switch(x,
         a = "option 1",
         b = "option 2",
         c = "option 3",
         stop("Invalid `x` value")
  )
}
# The last component of a switch() should always throw an error,
# otherwise unmatched inputs will invisibly return NULL


legs <- function(x) {
  switch(x,
         cow = ,
         horse = ,
         dog = 4,
         human = ,
         chicken = 2,
         plant = 0,
         stop("Unknown input")
  )
}
legs("cow")
#> [1] 4
legs("dog")
#> [1] 4


# loop --------------------------------------------------------------------
xs <- as.Date(c("2020-01-01", "2010-01-01"))
for (i in seq_along(xs)) {
  print(xs[[i]])
}


# function ----------------------------------------------------------------
f02 <- function(x, y) {
  # A comment
  x + y
}
# The formals(), the list of arguments that control how you
# call the function.

# The body(), the code inside the function.

# The environment(), the data structure that determines
# how the function finds the values associated with the
# names.

formals(f02)
body(f02)
environment(f02)

# soirce refference
attr(f02, "srcref")

funs <- list(
  half = function(x) x / 2,
  double = function(x) x * 2
)

funs$double(10)

# jika sudah punya parameter dlm struktur data, mis dlm list
args <- list(1:10, na.rm = TRUE)
# maka pakai do call
do.call(mean, args)

objs <- mget(ls("package:base", all = TRUE), inherits = TRUE)
funs <- Filter(is.function, objs)

formals(funs$abbreviate) %>% length()

sapply(funs, function(x){
  formals(x) %>% length()
}) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  as_tibble() %>%
  `colnames<-`(c('fun', 'nargs')) %>%
  arrange(-nargs)


g11 <- function() {
  if (!exists("a")) {
    a <- 1
  } else {
    a <- a + 1
  }
  a
}

g11()
a <- 2
g11()


codetools::findGlobals(g11)


rlang::list2()
rlang::enquos()

# agar 1 tidak di print
j04 <- function() invisible(1)
j04()

# stop() langsung keluar
# dieksekusi diakhir selalu
on.exit(cat("Goodbye!\n"), add = TRUE)

# sangat berguna
cleanup <- function(dir, code) {
  old_dir <- setwd(dir)
  on.exit(setwd(old_dir), add = TRUE)

  old_opt <- options(stringsAsFactors = FALSE)
  on.exit(options(old_opt), add = TRUE)
}



# fungsi jahat
`(` <- function(e1) {
  if (is.numeric(e1)){
    e1 + 1
  } else {
    e1
  }
}

(122+1)
rm('(')
(122+1)


# S3 ----------------------------------------------------------------------
x <- 1
attr(x, "class") <- "foo"
x

# Or in one line
x <- structure(1, class = "foo")
x

class(x) <- c("A", "B")
class(x) <- LETTERS



# contoh ------------------------------------------------------------------
rata2 <- function (x, ...) {
  UseMethod("rata2", x)
}

rata2.a <- function(x, ...) sum(x) / length(x)
rata2.b <- function(x, ...) sapply(x, mean, ...)
rata2.c <- function(x, ...) apply(x, 2, mean)

x <- iris[-5]
class(x) <- 'b'
x

rata2(x)
methods('rata2')



# contoh lain -------------------------------------------------------------
baz <- function(x) UseMethod("baz", x)
baz.A <- function(x) "A"
baz.B <- function(x) "B"

ab <- structure(1, class = c("A", "B"))
ba <- structure(1, class = c("B", "A"))
baz(ab)
baz(ba)

# jika tidak ada class kedua lalu NextMethod maka
# akan error
baz.C <- function(x) c("C", NextMethod())
ca <- structure(1, class = c("C", "A"))
cb <- structure(1, class = c("C", "B"))
baz(ca)
baz(cb)


baz.D <- function(x) {
  class(x) <<- "A"
  NextMethod()
}
da <- structure(1, class = c("D", "A"))
db <- structure(1, class = c("D", "B"))
baz(da)
baz(db)



XXX <- function(...) {}
is.XXX <- function(x) inherits(x, "XXX")

class(x) <- 'X'
is.XXX(x)
class(x) <- 'XXX'
is.XXX(x)


# penerapan
print.XXX <- function(x, ...) cat(format('ini kelas XXX', ...), "\n")
class(x) <- 'XXX'
print(x)
