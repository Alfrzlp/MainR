df = OrchardSprays

library(dplyr)
# No 1A
df1a = df %>% filter(treatment %in% c("A", "B"))

t.test(decrease~treatment, data = df1a, var.equal = F,
       alternative = "less", mu = 2.8)

# No 1B
df1b = df %>% filter(treatment %in% c("C", "D", "F", "H"))

anova = aov(decrease~treatment, data = df1a)
summary(anova)



# No 2-------------------------------------
mtcars

sample_n(mtcars, nrow(mtcars), replace = T)

cor(mpq, hp)

bootstrap <- function(data, B, fun){
boot = c()
  for(i in 1:B){
    sample = sample_n(mtcars, nrow(mtcars), replace = T)
    boot = c(boot, with(sample, cor(mpg, hp)))
  }
return(boot)
}

bootstrap(mtcars, 1000, cor)
