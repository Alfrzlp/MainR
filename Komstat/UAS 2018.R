# No 1------------------------------------------------------------
# copy dari soal
input = "No Hair Eye Sex Count N_UTS N_UAS N_TUGAS
1 Black Brown Male 32 61.19 67.06 82.6
2 Brown Brown Male 53 61.51 58.08 79.94
3 Red Brown Male 10 67.58 64.44 89.91
4 Blond Brown Male 3 64.51 72.12 87.39
5 Black Blue Male 11 68.62 63.61 83.89
6 Brown Blue Male 50 68.22 62.05 86.71
7 Red Blue Male 10 62.47 57.89 77.51
8 Blond Blue Male 30 60.76 65.91 88.09
9 Black Hazel Male 10 62.35 65.39 85.54
10 Brown Hazel Male 25 66.25 65.97 83.95
11 Red Hazel Male 7 63.05 58.76 81.75
12 Blond Hazel Male 5 63.04 59.88 82.37
13 Black Green Male 3 60.56 66.52 85.23
14 Brown Green Male 15 66.25 67.41 81.93
15 Red Green Male 7 70.04 64.07 84.7
16 Blond Green Female 8 67.45 71.68 82.71
17 Black Brown Female 36 65.95 64.66 88.14
18 Brown Brown Female 66 64.84 77.45 87.23
19 Red Brown Female 16 71.48 62.22 84.74
20 Blond Brown Female 4 64.61 60.18 87.73
21 Black Blue Female 9 67.65 67.71 80.85
22 Brown Blue Female 34 60.4 50.68 81.69
23 Red Blue Female 7 61.90 68.36 80.71
24 Blond Blue Female 64 62.71 75.09 82.65
25 Black Hazel Female 5 64.55 59.42 81.24
26 Brown Hazel Female 29 71.80 71.53 83.01
27 Red Hazel Female 7 63.23 64.03 88.61
28 Blond Hazel Female 5 63.23 65.71 85.96
29 Black Green Female 2 67.42 73.36 84.77
30 Brown Green Female 14 65.48 66.61 87.38
31 Red Green Female 7 64.27 66.53 86.76
32 Blond Green Female 8 64.62 56.01 86.95
"
df = read.table(textConnection(input), header = T)
df
# membuat kolom nilai total
# Nilai Total = 0.3*N_UTS + 0.2*N_TUGAS + 0.5*N_UAS
library(dplyr)
df = df %>% mutate(n_total = 0.3*N_UTS + 0.2*N_TUGAS + 0.5*N_UAS)
df
anova1 = aov(n_total~Sex+Hair+Eye, data = df)
summary(anova1)

TukeyHSD(anova1)




# No 2------------------------------------------------------------------
df %>% 
  group_by(Sex) %>% 
  count()
  
df %>% 
  group_by(Sex) %>% 
  filter(Hair == "Blond", Eye == "Blue") %>% 
  count()

prop.test(x = c(1,1), n = c(17, 15), correct = TRUE)






# No 3-----------------------------------------------------------------
# bootsrap----------
bootstrap = function(x, fun, B){
  boot = c()
  for (i in 1:B) boot = c(boot, fun(sample(x, replace = T)))
  print(boot)
  return(fun(boot))
}
# Male
x = df %>% filter(Sex == "Male") %>% select(n_total) %>% pull()
bootstrap(x, mean, 1000)
bootstrap(x, var, 1000)


# Female
y = df %>% filter(Sex == "Female") %>% select(n_total) %>% pull()
bootstrap(y, mean, 1000)
bootstrap(y, var, 1000)




# jackknife---------- mean, var, sd, cor, koef regresi
jackknife = function(x, fun){
  jack = c()
  for(i in 1:length(x)) jack = c(jack, fun(x[-i]))
  print(jack)
  return(sum(jack)/length(x))
}

# Male
jackknife(x, mean)
jackknife(x, var)

# Female
jackknife(y, mean)
jackknife(y, var)



# No 4-------------------------------------------------------------------
titanic = data.frame(Titanic)
titanic

# Ho : P tdk survive penumpang = P tdk survive awak

titanic %>% 
  filter(Survived == "No") %>% 
  group_by(Class) %>% 
  summarise(n = sum(Freq))


titanic %>% 
  group_by(Class) %>% 
  summarise(n = sum(Freq))

tab = as.table(rbind(c(122+167+528, 673),
                     c(325+285+706, 885)))

dimnames(tab) = list(kategori = c("Tdk Survive", "Total"),
                     id = c("penumpang", "awak"))
tab
prop.test(tab)


# No 5--------------------------------------------------------------------
library(vcd)
titanic = data.frame(Titanic)

titanic = titanic %>% 
  mutate(bb = rnorm(32, 35, sqrt(25)),
         bb = round(bb, 2))

anova2 = aov(bb~Class*Age*Sex, data = titanic)
summary(anova2)

data.frame(Titanic)


BB<-rnorm(32, mean=34.00, sd=5.00)
BB
dataa<-data.frame(BB)
dataa
dataa1<-data.frame(dat2$Class,dat2$Sex,dat2$Age)
dataa1
x<-cbind(dataa, dataa1, col=2)
x
anova<-aov(BB~dat2.Class+dat2.Age+dat2.Sex, data = x)
summary(anova)
#Gagal Tolak karena p-value > alfa
