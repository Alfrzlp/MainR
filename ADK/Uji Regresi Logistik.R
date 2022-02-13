(dat <- read.csv("D:/_Datasets/diabetes.csv") %>%
  janitor::clean_names())

summary(dat)

dat <- dat %>%
  dplyr::filter(glucose != 0)

frml <- diabetes ~ glucose
# Model ---------------------------------------------------------------
mb <- glm(frml, dat, family = binomial("logit"))
mp <- glm(frml, dat, family = binomial("probit"))

summary(mb)
summary(mp)



logLik(mb)[1]
1 - (logLik(mb)[1] / -493.35)
1 - (-493.35 / -393.28)^2 / nrow(df)


# Pseudo R2 ---------------------------------------------------------------
# -2 log lik
-2 * logLik(mb)[1]

DescTools::PseudoR2(mb, which = "all")
DescTools::PseudoR2(mb, c("CoxSnell", "Nagelkerke"))


# Hosmer and Lemeshow test ------------------------------------------------
# vcdExtra::HosmerLemeshow(mb)
# Ho : Model fit dengan data
ResourceSelection::hoslem.test(mb$y, mb$fitted.values)


hl <- vcdExtra::HLtest(mb)
plot(hl)
summary(hl)

# Uji Serentak ------------------------------------------------------------
lmtest::lrtest(fit)
# Tolak H0 minimal terdapat 1 parameter != 0


# median effective level --------------------------------------------------
-coef(fit)[1] / coef(fit)[2]


# interpretasi ------------------------------------------------------------
# B0
# Perkiraan nilai odds (kecenderungan) bahwa Yi = 1,
# jika Xi = 0 adalah exp(-15.705) = 1.511489594e-07
# Dg kata lain, kecenderungan orang dg skor TPA = 0 untuk LULUS
# sangat kecil

# B1
# Perkiraan perubahan nilai odds (kecenderungan) bahwa Yi = 1,
# jika Xi bertambah satu unit adalah exp(0.0251) = 1.03
# Seseorang dg skor TPA 1 point lebih
# tinggi memiliki kecenderungan 1,03 kali untuk LULUS.

# Kurva ROC ---------------------------------------------------------------
probb <- mb %>% predict(select(dat, glucose), type = "response")
probp <- mp %>% predict(select(dat, glucose), type = "response")
ytest <- dat$diabetes


ypred <- ifelse(probb > 0.5, 1, 0)
cmtest <- table(ytest, ypred)
(cm <- confusionMatrix(cmtest))


library(pROC)
rocb <- roc(ytest ~ probb,
  plot = TRUE, print.auc = TRUE,
  col = "black", lwd = 4, legacy.axes = TRUE, main = "ROC Curves"
)
rocp <- roc(ytest ~ probp,
  plot = TRUE, print.auc = TRUE,
  col = "black", lwd = 4, legacy.axes = TRUE, main = "ROC Curves"
)

rocb$auc
rocp$auc
data.frame(
  sen = c(rocb$sensitivities, rocp$sensitivities),
  spe = c(1 - rocb$specificities, 1 - rocp$specificities),
  model = rep(c("Binomial", "Probit"), each = 136)
) %>%
  ggplot(aes(x = spe, y = sen, color = model)) +
  geom_line(color = "red", lwd = 1) +
  geom_text(aes(x = 0.75, y = 0.25, label = paste("AUC :", round(rocb$auc, 3))),
    inherit.aes = F
  ) +
  geom_segment(x = 0, y = 0, xend = 1, yend = 1, inherit.aes = F, lwd = 0.1) +
  facet_grid(~model) +
  labs(
    title = "Kurva ROC",
    x = "1 - Specificity", y = "Sensitivity"
  ) +
  coord_equal() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()

# Plot --------------------------------------------------------------------
data.frame(
  glukosa = dat$glucose,
  p = mp$fitted.values,
  b = mb$fitted.values
) %>%
  pivot_longer(-1, names_to = "Model", values_to = "Probabilitas") %>%
  ggplot(aes(x = glukosa, color = Model)) +
  geom_line(aes(y = Probabilitas), lwd = 1.5) +
  # geom_point(aes(y = Probabilitas)) +
  labs(
    title = "Perbandingan Model Binomial dengan Probit",
    x = "Glukosa",
    caption = expression(italic("Sumber Data :National Institute of Diabetes and Digestive and Kidney Diseases"))
  ) +
  scale_color_discrete("Model", labels = c("Binomial", "Probit")) +
  theme_bw()
