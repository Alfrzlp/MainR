library(survival)
library(survminer)

leuk <- read.csv('D:/__SEMESTER 6/AKH/leuk.csv')


# Memisahkan data ---------------------------------------------------------
leuk_kurang12 <- leuk[which(leuk$waktu < 12), ]
leuk_lebih12 <- leuk[which(leuk$waktu >= 12), ]



# formula
form <- Surv(waktu, status) ~ sex


# Waktu Kurang dari 12 Minggu ---------------------------------------------
km_kurang12 <- survfit(Surv(waktu, status) ~ sex, data = leuk_kurang12)

# PLot
ggsurvplot(km_kurang12)

# Uji beda rata2
survdiff(form, data = leuk_kurang12)








# Waktu Lebih dari 12 Minggu ----------------------------------------------
km_lebih12 <- survfit(Surv(waktu, status) ~ sex, data = leuk_lebih12)

# PLot
ggsurvplot(km_lebih12) 


# Uji beda rata2
survdiff(form, data = leuk_lebih12)



