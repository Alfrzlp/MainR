plot(AirPassengers, main = "Monthly totals of international airline passengers 
     from 1949 to 1960")

data <- matrix(AirPassengers, 12, 12, byrow = TRUE)
data
rownames(data) <- 1949:1960
colnames(data) <- month.name
data

summary(data) # summary tiap bulan
tabel2arah <- cbind(rowSums(data[, 1:4]), rowSums(data[, 5:8]), rowSums(data[, 9:12]))
colnames(tabel2arah) <- c("Jan-Apr", "Mei-Agu", "Sep-Des")
tabel2arah
