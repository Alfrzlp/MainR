library(car)
library(lmtest)
# No 2 ============
df <- readxl::read_xlsx("D:/anareg/Data 2 UAS Regresi 2021.xlsx") %>% 
  mutate_all(~str_replace_all(.x, ',', '.')) %>% 
  type_convert() %>% 
  janitor::clean_names()

df

model <- lm(y~x1+x2, df)
bptest(model)


df <- df %>% 
  mutate_all(~log(.x))

model <- lm(y~x1+x2, df)
model
bptest(model)



# no 3
data <- readxl::read_xlsx("D:/anareg/Data 3 UAS Regresi 2021.xlsx") %>% 
  select(-Tahun, y = `Y (unit)`, x =`X (%)`)
data

model3 <- lm(y~x, data)
model3 %>% 
  summary

durbinWatsonTest(model3)




# No 4
s = "4962.269 
5906.235
1421.926
3911.216
413.131
1421.738
408.320"

read.table(textConnection(s), header = F) %>% 
  select(sse = V1) %>% 
  mutate(p = c(2, 2, 2, 3, 3, 3, 4),
         mse = sse/(20-p),
         r2 = 1-sse/7628.950,
         cp = sse/25.20 - (20-2*p),
         aic = 20*log(sse) - 20*log(20) + 2*p) %>% 
  arrange(r2, -mse, -aic, -cp)
