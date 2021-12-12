#one way Analysis of Variance
chick <- chickwts
by(data = chick$weight, INDICES = chick$feed, FUN = shapiro.test)

#tes homogenitas varians
bartlett.test(weight~feed, data = chick)  #y=weight     x=feed

anova1 <- aov(weight~feed, data = chick)
summary(anova1)

plot(weight~feed, data = chick)

pairwise.t.test(chick$weight, chick$feed, p.adj = "bonferroni", paired = F)

TukeyHSD(anova1)

data("InsectSprays")
insect <- InsectSprays
#explore data
dim(insect)
head(insect)
attach(insect)
summary(insect)

boxplot(count~spray, data=insect, xlab="Type of Spray",
        ylab = "Insect Count", main="InsectSprays data", varwidth=T,
        col = "lightgray")

#one way Analysis of Variance
by(data = count, INDICES = spray, FUN = shapiro.test)
aov.insect <- aov(count~spray, data = insect)
summary(aov.insect)

TukeyHSD(aov.insect)


#Two way Anova
hsb <- data.frame(read.csv("D:/Datasets/hsb2-2.csv"))
#uji homogen varian
bartlett.test(science~c(female+schtyp), data = hsb)

anova2 <- aov(science~female+schtyp, data = hsb)
summary(anova2)

data("ToothGrowth")
tooth <- ToothGrowth
str(tooth)
tooth$dose <- as.factor(tooth$dose)
str(tooth)

bartlett.test(len~supp, data = tooth)
bartlett.test(len~dose, data = tooth)

#Anova two ways
aov.vitc <- aov(len~supp+dose, data = tooth)
summary(aov.vitc)
TukeyHSD(aov.vitc)

boxplot(len~supp+dose, data = tooth, xlab = "Dosis dan Metode pemberian",
        ylab = "Pertumbuhan Gigi", main = "Boxplot Percobaan pemberian Vitamin C pada Marmut")

coplot(len~dose|supp, data=tooth, panel = panel.smooth,
       xlab = "ToothGrowth data: Length vs dose, given type of supplement")
