data()
data("chickwts")
chick <- chickwts
dim(chick)
head(chick)
summary(chick)

# one sample t test
t.test(chick$weight, alternative = "two.sided", mu = 150) # t test 1 populasi

# unpaired Two Sample t-Test
chick3 <- chick[chick$feed %in% c("casein", "sunflower"), ]

# test for normality
# for variable weight in chick3 data set

# ekstrasi weight dari group casein
dtcasein <- chick3$weight[chick3$feed == "casein"]
shapiro.test(dtcasein)

# ekstrasi weight dari group sunflower
dtsun <- chick3$weight[chick3$feed == "sunflower"]
shapiro.test(dtsun)

# test for equal varians
var.test(weight ~ feed, data = chick3)
bartlett.test(weight ~ feed, data = chick3)

# Assume Equal Variance
t.test(weight ~ feed, data = chick3, var.equal = T)

# Unpaired Two sample t test
Ban <- data.frame(
  kelBan = rep(c("radial", "Biasa"), each = 12),
  KPL = c(
    4.2, 4.7, 6.6, 7, 6.7, 4.5, 5.7, 6, 7.4, 4.9, 6.1, 5.2, 4.1, 4.9, 6.2,
    6.9, 6.8, 4.4, 5.7, 5.8, 6.9, 4.7, 6, 4.9
  )
)

# uji kenormalan simultan
shapiro.test(Ban$KPL) # jika          p > alpa terima ho

# uji kenormalan parsial , kelompok pertama
shapiro.test(Ban[1:12, ]$KPL)

# uji kenormalan parsial , kelompok kedua
shapiro.test(Ban[13:24, ]$KPL)

# Test for Equal variance
var.test(KPL ~ kelBan, data = Ban)
bartlett.test(KPL ~ kelBan, data = Ban)

t.test(KPL ~ kelBan, data = Ban, alternative = "greater", var.equal = T)

boxplot(KPL ~ kelBan, data = Ban)

# paired two sampel t test
before <- c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3)
after <- c(12.7, 13.6, 12, 15.2, 16.8, 20, 12, 15.9, 16, 11.1)

# uji kenormalan simultan
shapiro.test(before - after)

t.test(before, after, paired = T)

# Paired Two sample Test
baseline <- c(115, 112, 107, 119, 115, 138, 126, 105, 104, 115)
theraphy <- c(128, 115, 106, 128, 122, 145, 132, 109, 102, 117)

# uji kenormalan
shapiro.test(theraphy - baseline)
t.test(theraphy, baseline, paired = T, alternative = "greater")


data("InsectSprays")
insect <- InsectSprays

# explore data
dim(insect)
head(insect)
attach(insect)
summary(insect)
table(spray, count)

# Unpaired two sample t test
subset.insectSpray <- InsectSprays[InsectSprays$spray %in% c("A", "C"), ]

attach(subset.insectSpray)
# test for normality
# for variable count
shapiro.test(count)

# Test for equal Variance
var.test(count ~ spray)

subset.insectSpray$spray <- factor(subset.insectSpray$spray)
boxplot(count ~ spray,
  data = subset.insectSpray,
  xlab = "Type of spray", ylab = "Insect count",
  main = "InsectSprays data", varwidth = T, col = "lightgray"
)

# Assume unequal variance
t.test(count ~ spray, var.equal = F)
