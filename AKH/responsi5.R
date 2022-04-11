library(survival)
library(dplyr)

leuk <- read.csv('D:/__SEMESTER 6/AKH/leuk.csv', stringsAsFactors = T)
head(leuk)

leuk_new <- 
  leuk %>% 
  mutate(
    sex = ifelse(sex == 'male', 1, 0),
    treat = as.numeric(substr(treat, 2, 2)),
    sext = sex * (waktu),
    logWBCt = logWBC * (waktu),
    treatt = treat * (waktu)
  )


cox1 <- coxph(Surv(waktu, status) ~ I(sex * waktu), data = leuk_new)
summary(cox1)
# tolak Ho maka asumsi PH tidak terpenuhi

cox2 <- coxph(Surv(waktu, status) ~ I(treat * waktu), data = leuk_new)
summary(cox2)
# gagal tolak Ho asumsi terpenuhi

cox3 <- coxph(Surv(waktu, status) ~ I(logWBC * log(waktu)), data = leuk_new)
summary(cox3)
# gagal tolak Ho asumsi terpenuhi

