tm <- matrix(c(0.5, 0.25, 0.25,
               0.5, 0, 0.5,
               0.25, 0.25, 0.5),
             nrow = 3, byrow = T)
tm

state0 <- c(0, 1, 0)
# state 1
state0 %*% tm 
# state 2
state0 %*% tm %*% tm



# Metropolis Hasting ------------------------------------------------------
