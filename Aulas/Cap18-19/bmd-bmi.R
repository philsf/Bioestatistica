library(philsfmisc)
library(data.table)

# dados simulados ---------------------------------------------------------

set.seed(1)
BMI <- c(rnorm(100, 30, 5), rnorm(100, 50, 5))
idade <- rnorm(200, 60, 5)
horm <- round(sin(BMI))
scatter <- rnorm(200, 0, 20)
BMD <- -3*BMI - idade + 400 + scatter
dados <- data.table(BMI, BMD, idade, horm)

BMD2 <- -3*BMI -
  c(
    rnorm(50, 0, 10), # introduzir heterocedasticidade
    rnorm(50, 0, 20),
    rnorm(50, 0, 40),
    rnorm(50, 0, 45)
  ) + 300
heterocedasticidade <-  data.table(BMI, BMD2)

BMD3 <- -7*BMI - 10*idade - 100*horm + scatter + 1200
heterocedasticidade2 <-  data.table(BMI, BMD3)

modelo <- lm(BMD ~ BMI, data = dados)
print(summary(modelo))
format.interval(confint(modelo)[1, ]) # IC intercept
format.interval(confint(modelo)[2, ]) # IC slope

pred39 <- predict(modelo, newdata = data.table(BMI=39), interval = "conf")
format.float(pred39[1], 1)
format.interval(pred39[2:3], 1)

# png("Aulas/modelos/pratica-plot1.png")
# with(dados, plot(BMI,BMD, main = "BMI x BMD", xlab = "BMI (kg/m2)", ylab = "BMD (escala ficitícia)"))
# dev.off()
# 
# png("Aulas/modelos/pratica-plot2.png")
# with(dados, plot(BMI,BMD, main = "BMI x BMD", xlab = "BMI (kg/m2)", ylab = "BMD (escala ficitícia)"))
# abline(modelo, col = "blue", lwd = 2)
# dev.off()
# 
# png("Aulas/modelos/pratica-plot3.png")
# with(dados, plot(BMI,BMD, main = "BMI x BMD", xlab = "BMI (kg/m2)", ylab = "BMD (escala ficitícia)"))
# abline(modelo, col = "blue", lwd = 2)
# abline(v = 39, lty = 2, lwd = 2, col = "red")
# dev.off()

library(ggplot2)
b <- ggplot(dados, aes(BMI,BMD)) +
  geom_point() +
  xlab("BMI (kg/m2)") + ylab("BMD (escala ficitícia)") +
  ggtitle("BMI x BMD")
ggsave("Aulas/Cap18-19/pratica-plot1.png", h = 7, w = 7)

bsmooth <- b + geom_smooth(method = "lm")
ggsave("Aulas/Cap18-19/pratica-plot2.png", h = 7, w = 7)

bsmooth + geom_vline(xintercept = 39, lty = 2, lwd =1, col = "red")
ggsave("Aulas/Cap18-19/pratica-plot3.png", h = 7, w = 7)

bsmooth.only <- ggplot(dados, aes(BMI, BMD)) +
  geom_smooth(method = "lm") +
  xlim(range(BMI)) + ylim(range(BMD)) +
  xlab("BMI (kg/m2)") + ylab("BMD (escala ficitícia)") +
  ggtitle("BMI x BMD")
ggsave("Aulas/Cap18-19/pratica-plot4.png", h = 7, w = 7)

ggplot(data.frame(Fitted = fitted(modelo), Residuals = residuals(modelo)), aes(Fitted, Residuals)) +
  geom_point() +
  ggtitle("Valores ajustados x Resíduos")
ggsave("Aulas/Cap18-19/pratica-plot-resid.png", h = 7, w = 7)

png("Aulas/Cap18-19/pratica-hist-resid.png")
hist(residuals(modelo), col = "gray", main = "Distribuição dos resíduos", xlab = "")
dev.off()

b2 <- ggplot(heterocedasticidade, aes(BMI, BMD2)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("BMI (kg/m2)") + ylab("BMD (escala ficitícia)") +
  ggtitle("BMI x BMD")
ggsave("Aulas/Cap18-19/pratica-plot-heterocedasticidade.png", h = 7, w = 7)

modelo2 <- lm(BMD2 ~ BMI, heterocedasticidade)
b2.res <- ggplot(data.frame(Fitted = fitted(modelo2), Residuals = residuals(modelo2)), aes(Fitted, Residuals)) +
  geom_point() +
  ggtitle("Valores ajustados x Resíduos")
ggsave("Aulas/Cap18-19/pratica-plot-heterocedasticidade-resid.png", h = 7, w = 7)

b3 <- ggplot(heterocedasticidade2, aes(BMI, BMD3)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("BMI (kg/m2)") + ylab("BMD (escala ficitícia)") +
  ggtitle("BMI x BMD")
ggsave("Aulas/Cap18-19/pratica-plot-heterocedasticidade-sin.png", h = 7, w = 7)

modelo3 <- lm(BMD3 ~ BMI, heterocedasticidade2)
b3.res <- ggplot(data.frame(Fitted = fitted(modelo3), Residuals = residuals(modelo3)), aes(Fitted, Residuals)) +
  geom_point() +
  ggtitle("Valores ajustados x Resíduos")
ggsave("Aulas/Cap18-19/pratica-plot-heterocedasticidade-sin-resid.png", h = 7, w = 7)
