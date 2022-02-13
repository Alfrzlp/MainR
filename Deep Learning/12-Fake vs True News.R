

fake <- read.csv("D:/Datasets/NLP/Berita/Fake.csv")
true <- read.csv("D:/Datasets/NLP/Berita/True.csv")

dim(fake)
dim(true)

head(fake, 1)

unique(fake$subject)
unique(true$subject)
