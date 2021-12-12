library(dplyr)

#No 1
#Historgram distribusi sampel rata-rata
sampel = c()
for(i in 1:10000){
  sampel = c(sampel, mean(runif(10, 1, 10)))
}
hist(sampel, main = "Distribusi sampel rata-rata")

#mungkin ini merupakan contoh dari bukti CLT, 



#No 2
cancer = read.csv("D:/Datasets/cancer-survival.csv")
head(cancer)

#uji normalitas 
by(data = cancer$Survival, INDICES = cancer$Organ, FUN = shapiro.test)

#uji Homogenitas varians
bartlett.test(Survival~Organ, data = cancer)

#Uji Anova
anova1 = aov(Survival~Organ, data = cancer)
summary(anova1)
TukeyHSD(anova1)




# No 3A
library(car)
data("Vocab")
head(Vocab)


# Uji Normalitas

# pisahkan data terlebih dahulu
datasplit = split(Vocab$vocabulary, Vocab$sex)

male = datasplit$Male
female = datasplit$Female

library(nortest)
lillie.test(male)
lillie.test(female)


# Uji Homogenitas varians
bartlett.test(vocabulary~sex, data = Vocab)
# didapat varians tidak sama

# Uji t
t.test(vocabulary~sex, data = Vocab, var.equal = F)

# NO 3B 
t.test(male, mu=6)





#no 4
str = 
"64   72   74
55   57   47
59   66   58
58   57   53"

panen = read.table(textConnection(str))
panen

# tambah variable jenis pupuk dan letak kan sebelum kolom V1
panen = panen %>% 
  mutate(pupuk = c("p1", "p2", "p3", "p4"), .before = "V1")
panen

#Ubah struktur data agar dapat dilakukan uji2
library(tidyr)
panen = panen %>% pivot_longer(2:4)
panen

#Uji Normalitas
shapiro.test(panen$value) # gatau bener apa enggak


#Uji Homogenitas varians
bartlett.test(value~pupuk, data = panen) # pupuk
bartlett.test(value~name, data = panen) # varietas

# anova 2 arah
anova_panen = aov(value~name+pupuk, data = panen)
summary(anova_panen)
TukeyHSD(anova_panen)


