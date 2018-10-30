library(philsfmisc)
library(data.table)

# dados simulados ---------------------------------------------------------
dados.rls <- fread("Aulas/Cap18-19/dados-rls.csv")
dados.rls.het <- fread("Aulas/Cap18-19/dados-rls-het.csv")
dados.rlm <- fread("Aulas/Cap31-32/dados-rlm.csv")

modelo <- lm(BMD ~ BMI, data = dados.rls)
summary(modelo)
round(coef(modelo)[1])
format.interval(confint(modelo)[1, ], 1) # IC intercept
round(coef(modelo)[2])
format.interval(confint(modelo)[2, ], 1) # IC slope

# predicao ----------------------------------------------------------------

pred39 <- predict(modelo, newdata = data.table(BMI=39), interval = "conf")
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
ggsave("Aulas/Cap18-19/pratica-plot1.png", h = 7, w = 7)

bsmooth <- b + geom_smooth(method = "lm")
ggsave("Aulas/Cap18-19/pratica-plot2.png", h = 7, w = 7)

bsmooth.only <- ggplot(dados.rls, aes(BMI, BMD)) +
  geom_smooth(method = "lm") +
  xlim(range(dados.rls$BMI)) + ylim(range(dados.rls$BMD)) +
  xlab("BMI (kg/m2)") + ylab("BMD (escala ficitícia)") +
  theme_bw() +
  ggtitle("BMI x BMD")
ggsave("Aulas/Cap18-19/pratica-plot3.png", h = 7, w = 7)

ggplot(data.frame(Fitted = fitted(modelo), Residuals = residuals(modelo)), aes(Fitted, Residuals)) +
  geom_point() +
  ylim(c(-100, 100)) +
  ggtitle("Valores ajustados x Resíduos")
ggsave("Aulas/Cap18-19/pratica-plot-resid.png", h = 7, w = 7)

png("Aulas/Cap18-19/pratica-hist-resid.png")
hist(residuals(modelo), col = "gray", main = "Distribuição dos resíduos", xlab = "")
dev.off()

bsmooth + geom_vline(xintercept = 39, lty = 2, lwd =1, col = "red")
ggsave("Aulas/Cap18-19/pratica-plot4.png", h = 7, w = 7)

b2 <- ggplot(dados.rls.het, aes(BMI, BMD2)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlim(range(dados.rls.het$BMI)) + ylim(range(dados.rls.het$BMD2)) +
  xlab("BMI (kg/m2)") + ylab("BMD (escala ficitícia)") +
  ggtitle("BMI x BMD")
ggsave("Aulas/Cap18-19/pratica-plot-heterocedasticidade.png", h = 7, w = 7)

# heterocedasticidade -----------------------------------------------------

modelo2 <- lm(BMD2 ~ BMI, dados.rls.het)
summary(modelo2)
b2.res <- ggplot(data.frame(Fitted = fitted(modelo2), Residuals = residuals(modelo2)), aes(Fitted, Residuals)) +
  geom_point() +
  ylim(c(-150, 150)) +
  ggtitle("Valores ajustados x Resíduos")
ggsave("Aulas/Cap18-19/pratica-plot-heterocedasticidade-resid.png", h = 7, w = 7)

b3 <- ggplot(dados.rlm, aes(BMI, BMD)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("BMI (kg/m2)") + ylab("BMD (escala ficitícia)") +
  xlim(range(dados.rlm$BMI)) + ylim(range(dados.rlm$BMD)) +
  ggtitle("BMI x BMD")
ggsave("Aulas/Cap18-19/pratica-plot-heterocedasticidade-sin.png", h = 7, w = 7)

modelo3 <- lm(BMD ~ BMI, dados.rlm)
summary(modelo3)
b3.res <- ggplot(data.frame(Fitted = fitted(modelo3), Residuals = residuals(modelo3)), aes(Fitted, Residuals)) +
  geom_point() +
  ylim(c(-300, 300)) +
  ggtitle("Valores ajustados x Resíduos")
ggsave("Aulas/Cap18-19/pratica-plot-heterocedasticidade-sin-resid.png", h = 7, w = 7)

# obsoleto ----------------------------------------------------------------

# png("Aulas/modelos/pratica-plot1.png")
# with(dados.rls, plot(BMI,BMD, main = "BMI x BMD", xlab = "BMI (kg/m2)", ylab = "BMD (escala ficitícia)"))
# dev.off()
# 
# png("Aulas/modelos/pratica-plot2.png")
# with(dados.rls, plot(BMI,BMD, main = "BMI x BMD", xlab = "BMI (kg/m2)", ylab = "BMD (escala ficitícia)"))
# abline(modelo, col = "blue", lwd = 2)
# dev.off()
# 
# png("Aulas/modelos/pratica-plot3.png")
# with(dados.rls, plot(BMI,BMD, main = "BMI x BMD", xlab = "BMI (kg/m2)", ylab = "BMD (escala ficitícia)"))
# abline(modelo, col = "blue", lwd = 2)
# abline(v = 39, lty = 2, lwd = 2, col = "red")
# dev.off()
