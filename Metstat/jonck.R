# ===== jonckhere ============
a <- '44.8
47.2
54.0
62.7
67.0
67.4
71.1
80.2'

b <- '79.6
79.8
81.7
82.0
85.7
88.5
88.8
'
c <- '91.1
93.3
94.5
95.8
98.6
98.9
99.5'

l = list(str2vec(a), str2vec(b), str2vec(c))
jonckheere(l)



str <- '1	4	1
1	2	4
3	2	4
1	1	4
4	0	2
4	1	1
0	1	4'

df <- read.table(textConnection(str), header = F) %>% 
  pivot_longer(everything()) %>% 
  drop_na() %>% 
  mutate(name = as.numeric(str_remove_all(name, '\\D'))) %>% 
  arrange(name)

clinfun::jonckheere.test(df$value, df$name,
                         alternative = 'inc')

df %>%   
  {split((.), (.)$name)} %>% 
  lapply(pull, value) %>% 
  jonckheere() 


jonckheere <- function(l){
  value <- function(a, b){
    x = c()
    for (i in a) {
      hasil = c()
      for (j in b) {
        if(i < j) hasil = c(hasil, 1)
        else if(i == j) hasil = c(hasil, 1/2)
        else if(i > j) hasil = c(hasil, 0)
      }
      x = c(x, sum(hasil))
    }
    return(x)
  }
  
  hasil = list()
  for (i in 1:(length(l)-1)) {
    for(j in (i+1):length(l)){
      x = value(l[[i]], l[[j]])
      hasil <- c(hasil, list(x))
    }
  }
  hasil
  
  mx = max(sapply(hasil, length))
  hasil <- lapply(hasil, function(x){
    return(c(x, rep(NA, mx-length(x))))
  }) %>% 
    bind_cols() %>% 
    `colnames<-`(NULL)
  
  U = colSums(hasil, na.rm = T)
  return(list(uij = hasil, 
              'U'=U, 'J-hitung' = sum(U)))
}





library(clinfun)  
a = str2vec(a)
b = str2vec(b)
c = str2vec(c)
jonckheere.test(c(a, b, c),
                rep(1:3, c(length(a), length(b), length(c))),
                alternative = 'inc')

# uji page ==============================================
str <- '9	6	20
5	12	19
11	8	21
18	21	30
8	12	20
9	7	10
21	16	20
18	27	9
22	16	30
19	22	27
4	8	10
6	12	7
10	12	21
11	14	23
8	5	10'

str = '70	70	80
75	70	75
75	80	80
80	80	75
80	83	85
80	80	70
85	80	70
75	75	80
70	80	80
65	80	75
83	90	90
83	80	80'

df <- read.table(textConnection(str), header = F)

rank = NULL
# dari kecil ke besar
for (i in 1:nrow(df)) rank = rbind(rank, rank(df[i,]))
rank
rank %>% colSums()
(1:length(colSums(rank))*colSums(rank)) %>% sum


library(DescTools)
PageTest(df %>% as.matrix)
