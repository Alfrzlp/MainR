train <- jb_ka[-c(183:187),]
test <- jb_ka[183:187,]

train <- train %>% 
  mutate(t = c(-rev(seq.int(2, 182, 2)), seq.int(2, 182, 2)))

y = train$jb
t = train$t
n = nrow(train)

a1 = mean(y)
b = sum(y*t)/sum(t^2)

a2 = (sum(y)*sum(t^4) - sum(t^2*y)*sum(t^2))/(n*sum(t^4) - sum(t^2)^2)
b = sum(y*t)/sum(t^2)
c = (n*sum(t^2*y) - sum(y)*sum(t^2) )/(n*sum(t^4) - sum(t^2)^2)
  
train %>% 
  mutate(trend = a1 + b*t,
         trend2 = a2 + b*t + c*t^2) %>% 
  mutate(res = jb-trend,
         detrend = jb/trend) %>%
  group_by(month = format(date, "%m")) %>% 
  summarise(mean(detrend)) %>% 
  copy2c()
  
  # summarise(
  #   bias = mean(res, na.rm = T),
  #   mad = mean(abs(res), na.rm = T),
  #   mse = mean(res^2, na.rm = T),
  #   mape = mean(abs(res)/jb, na.rm = T)*100
  # ) 
  ggplot(aes(x = date, y = jb)) +
  geom_line() +
  geom_line(aes(y = trend2))


