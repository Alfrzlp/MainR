# Utils -------------------------------------------------------------------
tabkon3 <- function(x, y, z, nilai, print_tab = F, return_df = T){
  tab <- 
    xtabs(freq ~ Var3 + Var2 + Var1,
        cbind(
          expand.grid(z, y, x),
          freq = mat
         )
        )
  v <- match.call(expand.dots = T)
  
  names(dimnames(tab)) <- as.character(v[2:4])
  if(print_tab) print(ftable(tab))
  
  if(return_df){
    tab %>% 
      as.data.frame() %>% 
      mutate_at(
        -ncol(.), ~ factor(.x, levels = rev(unique(.x)))
      ) %>% 
      return()
  }else{
    return(tab)
  }
}
softmax <- function(z) exp(z)/sum(exp(z))

df <- tab %>% 
  as.data.frame() %>% 
  mutate_at(
    -ncol(.), ~ factor(.x, levels = rev(unique(.x)))
  )

library(MASS)


# Soal --------------------------------------------------------------------
mat <- 
  c(28, 12, 5,
  12, 44, 20,
  15, 10, 10,
  8, 18, 25,
  24, 29, 25,
  5, 18, 40)

x <- jenis_kapal <- c('< 2 GT', '2-5 GT', '> 5 GT')
y <- modal <- c('<= 2 juta', '> 2 juta')
z <- lama_trip <- c('< 3 hari', '3-7 hari', '> 7 hari')

tab <- tabkon3(jenis_kapal, modal, lama_trip, mat, print_tab = T)
tab %>% str()


# R tidak perlu pakai - lagi
m <- polr(lama_trip ~ modal + jenis_kapal,
          data = tab, weights = Freq, Hess = T) 
m
summary(m)


# koefisien ---------------------------------------------------------------
(ctable <- coef(summary(m)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = round(p, 3)))



# odds ratio and ci
ci <- confint(m)
exp(cbind(odds_ratio = coef(m), ci))
