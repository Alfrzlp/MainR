str = "x y
0 508,1
0 498,4
1 568,2
1 577,3
2 651,7
2 657,0
3 713,4
3 697,5
4 755,3
4 758,9
5 787,6
5 792,1
6 841,4
6 831,8"

df = 
  read.table(textConnection(str), header = T) %>% 
  type_convert() %>% 
  mutate(y = y/10)

# tanpa X - Xbar
lm(Y ~ X + I(X^2), df) %>% summary
lm(y ~ poly(x, 3, raw = T), df) %>% summary

# dengan X - Xbar untuk mengurangi korelasi yg tinggi antara X dgn X^2
df <- df %>% mutate_at(vars(x), ~.x - mean(.x))
    
model <- lm(Y ~ X + I(X^2), df)


# lack of fit
model <- lm(y ~ x + I(x^2), df)
model2 <- lm(y ~ factor(x) + factor(I(x^2)), df)
anova(model, model2)
# error
# pure error     lack of fit
qf(0.05, 3, 6, lower.tail = F)

# mencari SSPE 
df %>% 
  group_by(x) %>% 
  mutate(ybarj = mean(y),
         nilai = (y-ybarj)^2) %>% 
  as.data.frame() %>% 
  summarise(sum(nilai))


# parsial test
model %>% summary




# Matriks
df <- mutate(df, x = x-mean(x))
x = as.matrix(data.frame(1, df$x, (df$x)^2
                         )) %>% `colnames<-`(NULL)
x
y = as.matrix(df$y)
y

# =============================================
t(x)%*%x
# (X'X)^-1
solve(t(x)%*%x)
t(x)%*%y
b = solve(t(x)%*%x)%*%t(x)%*%y
b

n = nrow(y)
J = matrix(1, nrow=n, ncol=n)
# Total sum of Square 
SSTO = t(y)%*%y - (1/n)*t(y)%*%J%*%y
SSTO
# regression sum of square 
SSR = t(b)%*%t(x)%*%y - (1/n)%*%t(y)%*%J%*%y
SSR
SSE = SSTO-SSR
SSE

# MSR dan MSE
SSR/(ncol(x)-1)
SSE/(n-ncol(x))

# mean ===========================
hasil <- model %>% predict(tibble(x = 0.5),
                  interval = "confidence", se.fit = T)
hasil
# manual
xh = matrix(c(1, 0.5, 0.25))
t(xh)%*%b
# var
2.98*t(xh)%*%solve(t(x)%*%x)%*%xh

qt(1-0.05/2, 9)

38.78357-qt(1-0.05/2, 9)*0.7659271


# predict =======================
model %>% predict(tibble(x = 0.5),
                  interval = "prediction", se.fit = T)
hasil$fit
# var ycap new
(2.98 + hasil$se.fit^2)

2.98*(1 + t(xh)%*%solve(t(x)%*%x)%*%xh)







qf(0.05, 1, 9, lower.tail = F)







str = "35	35	40	40	45	45	50	50	55	55	60	60
22	20	28	31	37	38	41	39	34	37	27	30"

read.table(textConnection(str)) %>% 
  t() %>% as.data.frame() %>% 
  `colnames<-`(c('x', 'y')) %>% 
  remove_rownames() %>% 
  mutate(x = x-mean(x)) %>% 
  lm(y~x+I(x^2)+I(x^3), .) %>% 
  anova

model <- 
  lm(y~x+I(x^3)+I(x^3), df)


ggplot(df)+
  geom_point(aes(x, y), col = 'red')+
  geom_line(aes(x, y = model2$fitted.values), lwd = 1, col = 'blue')+
  labs(title = 'Mileage Study', x = 'speed (miles/hours)',
       y = 'gasoline consumption (miles / galon)')+
  theme_bw()

ggplot(df)+
  geom_point(aes(model$fitted.values, model$residuals), col = 'red')+
  geom_hline(yintercept = 0)+
  labs(title = 'Mileage Study', x = 'Fitted Value',
       y = 'residual')+
  theme_bw()

qqnorm(model$residuals, pch = 'o')
qqline(model$residuals, col = "blue", lwd = 2)

