library(rlang)

e1 <- env(
  a = 1,
  b = 2
)
e1$d <- 3

env_print(e1)
env_names(e1)
env_parent(e1)

e1$a
identical(global_env(), current_env())

search()
detach(.env)

ge <- global_env()
ge$a <- 1:10


.env <- new.env()
.env$a <- 1:10
rm(list = ls(envir = .env), envir = .env)
ls(envir = .env)


attach(a <- 1:10, pos = '.GlobalEnv')
attach(.env)
detach(.env)



.env$ht = function(d, n = 5) rbind(head(d, n), tail(d, n))