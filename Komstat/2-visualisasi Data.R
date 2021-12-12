#visualisasi data
x = 1:15 ; y = x^2

par(mfrow=c(2,4)) #membagi jendela grafik 2 baris 4 kolom

tipe = c("p", "l", "b", "o", "h", "s", "n")
for (i in tipe){
  plot(x,y, type=i, main=paste("bentuk :", i))
}

#________________________________________________________________________
set.seed(123)
x <- seq(0,10, 0.1)
y <- x^2*exp(-x/2)*(1+rnorm(n=length(x), mean=0, sd=0.05))
par(mfrow=c(1,2),
    #margin grafik bawah,kiri,atas,kanan
    mar=c(3,3,1.5,1.5),
    #margin sumbu, ukuran kotak
    mex=1,
    #panjang dan arah garis" di sumbu koordinat(- kekiri)
    tcl=0.3
   )
plot(x,y, type="l")
plot(x,y, type="o")

#dengan curve____________________________________________________________
curve(expr=x^2*exp(-x/2), from=0, to=10)

plot(x,y, pch=19, cex=0.7, xlab="waktu(detik)",
     ylab="Sinyal Intensitas")
curve(expr=x^2*exp(-x/2), from=0, to=10, add=TRUE) 
#add ke plot sebelumnya

#visualisasi Lainnya_____________________________________________________

#bar plot
z = c(12,3,4,8,9)
names(z) = c("a", "b", "c", "d", "e")
par(mfrow=c(1,2))
barplot(VADeaths[, "Rural Male"], main="barplot 1")
barplot(z, main="barplot 1",
        col = c("cornsilk", "purple", "lavender", "blue", "cyan"),
        horiz=TRUE)
#barplot stacked
barplot(VADeaths, main="VADeaths", 
        legend = rownames(VADeaths))
barplot(VADeaths, main="VADeaths", 
        legend = rownames(VADeaths), beside = TRUE)

#Histogram_______________________________________________________________
dens = density(trees$Volume)
hist(trees$Volume, col=("grey"), main = "Volume",
     freq=FALSE, #Mengubah kernel hist dari frekuensi ke density
     xlab="Tress Volume", font.lab=4,
     sub="contoh Histogram", font.sub=3)
polygon(dens, border = "red")

#Boxplot_________________________________________________________________
par(mfrow=c(2,2),
    tcl=-0.5,
    mex=0.5)
boxplot(iris$Sepal.Length~iris$Species, main="Sepal Lenght", col=c("cyan","lightblue", "green"), xlab=NULL, ylab=NULL)
boxplot(iris$Sepal.Width~iris$Species, main="Sepal Width", col=c("cyan","lightblue", "green"), xlab=NULL, ylab=NULL)
boxplot(iris$Petal.Length~iris$Species, main="Petal length", col=c("cyan","lightblue", "green"), xlab=NULL, ylab=NULL)
boxplot(iris$Petal.Width~iris$Species, main="Petal Width", col=c("cyan","lightblue", "green"), xlab=NULL, ylab=NULL)

#legend_________________________________________________________________
s = seq(0,360,30)
y = cos(s)
z = sin(s)

plot(s,z, type="o", col="blue", lty=2)
lines(s,y, type="o", col="red", lty=1)
#cEX = Ukuran legend
legend(0, 1, legend=c("sin", "cos"),lty=1:2, cex=1,
       col=c("red", "blue"),title="Garis", bg="grey")

#tandai mpg <15 dan wt >5
par(mfrow=c(1,2))
d <- mtcars[mtcars$wt >=5 & mtcars$mpg <=15, ]

plot(mtcars$wt, mtcars$mpg)
text(d$wt, d$mpg, row.names(d), cex=0.65, col="red")

plot(1:10, 1:10, )
text(7,4, expression(bar(x)==sum(frac(x[i], n), i==1, n)))

#mtext
mtext("ini mtext", 4)

#membuat garis, 
#lwd = ketebalan, lty = tipe garis
plot(mtcars$wt, mtcars$mpg, main="Milage vs Car weight", xlab="weight", ylab="Miles / gallon"
     , pch=19)
abline(v=mean(mtcars$wt), col="red", lwd=3, lty=3)
abline(h=mean(mtcars$mpg), col="green", lwd=3, lty=3)
#garis regresi
abline(lm(mpg~wt, data = mtcars), lwd=4, lty=4, col="gray")

#axis
par(mfrow=c(2,4))
plot(x, log(x), 
     #ukuran titik plot
     cex=1,
     #orientasi 0,1,2,3
     las=1,
     frame=FALSE)
axis(3, 1:9, col="red", col.axis="blue")

#plot 2d dan 3d__________________________________________________________ 
n = 1:20
x = sin(n)
y = cos(n)*exp(-n/3)
z = outer(x,y)

par(mar=c(3,3,1.5,1.5), mex=1, mgp=c(2,0.5,0), tcl=0.3)
par(mfrow=c(1,2))

image(z, col=gray(1:10/10))
contour(z)

persp(n,n,z, 
      #sudut penglihatan 
      theta=50,
      #rotasi (bawah atas)
      phi=40)

persp(n,n,z, theta = 120, phi=40, shade=0.6)

#ggplot_________________________________________________________________
library(ggplot2)
loc = "D:/R/gapminder.tsv"
data = read.csv(loc, header = TRUE, sep = "\t")
View(data)

write.csv(data, file="gap_minder.csv")

Negara = data$country[1:36]
grafik = qplot(data$year[1:36], data$gdpPercap[1:36], main = "GDP Percapita", 
      xlab="Year", ylab = "GDP Percapita", color=Negara, shape=Negara)
grafik
#menentukan bentuk
grafik + scale_shape_manual(values = c(17,16,14))
#grafik
#grafik + facet_grid(.~ )

grafik = grafik + geom_vline(xintercept = c(1980,1985), colour = "purple") + geom_vline(xintercept=2000)
grafik = grafik + geom_hline(yintercept = 1000, linetype='longdash')
grafik

bar = ggplot(data=data, aes(x=data$pop[1:12], y=data$gdpPercap[1:12])) + geom_bar(stat = "indentity")
