library(philsfmisc)
library(data.table)

# dados simulados ---------------------------------------------------------
dados.rls <- fread("Aulas/Cap18-19/dados-rls.csv")
dados.rls.het <- fread("Aulas/Cap18-19/dados-rls-het.csv")
dados.rlm <- fread("Aulas/Cap31-32/dados-rlm.csv")

rls.modelo1 <- lm(BMD ~ BMI, data = dados.rls)
summary(rls.modelo1)
round(coef(rls.modelo1)[1])
format.interval(confint(rls.modelo1)[1, ], 1) # IC intercept
round(coef(rls.modelo1)[2])
format.interval(confint(rls.modelo1)[2, ], 1) # IC slope

# predicao ----------------------------------------------------------------

pred39 <- predict(rls.modelo1, newdata = data.table(BMI=39), interval = "conf")
format.float(pred39[1], 1)
format.interval(pred39[2:3], 1)

# graficos ----------------------------------------------------------------

library(ggplot2)
b <- ggplot(dados.rls, aes(BMI, BMD)) +
  geom_point() +
  xlim(range(dados.rls$BMI)) + ylim(range(dados.rls$BMD)) +
  xlab("BMI (kg/m2)") + ylab("BMD (escala ficitícia)") +
  theme_bw() +
  ggtitle("BMI x BMD")
ggsave("Aulas/Cap18-19/pratica-rls1.png", h = 7, w = 7)

bsmooth <- b + geom_smooth(method = "lm")
ggsave("Aulas/Cap18-19/pratica-rls2.png", h = 7, w = 7)

bsmooth.only <- ggplot(dados.rls, aes(BMI, BMD)) +
  geom_smooth(method = "lm") +
  xlim(range(dados.rls$BMI)) + ylim(range(dados.rls$BMD)) +
  xlab("BMI (kg/m2)") + ylab("BMD (escala ficitícia)") +
  theme_bw() +
  ggtitle("BMI x BMD")
ggsave("Aulas/Cap18-19/pratica-rls3.png", h = 7, w = 7)

ggplot(data.frame(Fitted = fitted(rls.modelo1), Residuals = residuals(rls.modelo1)), aes(Fitted, Residuals)) +
  geom_point() +
  ylim(c(-100, 100)) +
  theme_bw() +
  ggtitle("Valores ajustados x Resíduos")
ggsave("Aulas/Cap18-19/pratica-rls-resid.png", h = 7, w = 7)

png("Aulas/Cap18-19/pratica-rls-resid-hist.png")
hist(residuals(rls.modelo1), col = "gray", main = "Distribuição dos resíduos", xlab = "")
dev.off()

bsmooth + geom_vline(xintercept = 39, lty = 2, lwd =1, col = "red")
ggsave("Aulas/Cap18-19/pratica-rls4.png", h = 7, w = 7)

# heterocedasticidade -----------------------------------------------------

b2 <- ggplot(dados.rls.het, aes(BMI, BMD2)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlim(range(dados.rls.het$BMI)) + ylim(range(dados.rls.het$BMD2)) +
  xlab("BMI (kg/m2)") + ylab("BMD (escala ficitícia)") +
  theme_bw() +
  ggtitle("BMI x BMD")
ggsave("Aulas/Cap18-19/pratica-rls-het.png", h = 7, w = 7)

rls.modelo.het <- lm(BMD2 ~ BMI, dados.rls.het)
summary(rls.modelo.het)

b2.res <- ggplot(data.frame(Fitted = fitted(rls.modelo.het), Residuals = residuals(rls.modelo.het)), aes(Fitted, Residuals)) +
  geom_point() +
  ylim(c(-150, 150)) +
  theme_bw() +
  ggtitle("Valores ajustados x Resíduos")
ggsave("Aulas/Cap18-19/pratica-rls-het-resid.png", h = 7, w = 7)

# obsoleto ----------------------------------------------------------------

# png("Aulas/modelos/pratica-rls1.png")
# with(dados.rls, plot(BMI,BMD, main = "BMI x BMD", xlab = "BMI (kg/m2)", ylab = "BMD (escala ficitícia)"))
# dev.off()
# 
# png("Aulas/modelos/pratica-rls2.png")
# with(dados.rls, plot(BMI,BMD, main = "BMI x BMD", xlab = "BMI (kg/m2)", ylab = "BMD (escala ficitícia)"))
# abline(modelo, col = "blue", lwd = 2)
# dev.off()
# 
# png("Aulas/modelos/pratica-rls3.png")
# with(dados.rls, plot(BMI,BMD, main = "BMI x BMD", xlab = "BMI (kg/m2)", ylab = "BMD (escala ficitícia)"))
# abline(modelo, col = "blue", lwd = 2)
# abline(v = 39, lty = 2, lwd = 2, col = "red")
# dev.off()
