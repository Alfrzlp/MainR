# Distribusi peluang

round(1, 9599, digits = 4)
# r = membangkitkan bilangan acak
# p = cdf
# d = pdf
# q = fungsi quantile

# normal kumulatif
qnorm(0.975) # 0.975 kekiri
pnorm(1.96) # p(z < 1,96)


# ___________________________________________________________________________
# chi-square

# karena biasanya luas alpa kekanan makan lower.tail=F
qchisq(0.025, 9, lower.tail = F)

# ___________________________________________________________________________
# t
# lower.tail = True(deafult) ----> -t
#              False ---> t

#-t 0.025
qt(0.01, 10) # nilai kritis (alfa, df)
qt(c(0.01, 0.995), 7, lower.tail = F)


# lower.tail = True(deafult) ----> P(T<=t)
#              False ---> P(T>t)
pt(-2.567, 17, lower.tail = F)

# ___________________________________________________________________________
# f
qf(0.05, 15, 7)

x <- rnorm(100, 30, 8)
frex <- dnorm(x, mean(x), sd(x))
plot(x, frex)


qchisq(0.01, 39, lower.tail = F)

qt(0, lower.tail = F)
qnorm(0.05)

pnorm(-4.08)
