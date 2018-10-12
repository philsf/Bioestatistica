
# dados simulados ---------------------------------------------------------

set.seed(1)
BMI <- c(rnorm(50, 15, 1), rnorm(50, 20, 1), rnorm(50, 25, 1), rnorm(50, 35, 4))
scatter <- rnorm(200, 0, 20)
BMD <- -3*BMI - scatter + 100
dados <- data.frame(BMI,BMD)

modelo <- lm(BMD ~ BMI, data = dados)
pred <- predict(modelo, interval = "conf")
pred28 <- predict(modelo, newdata = data.frame(BMI=28))

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
# abline(v = 28, lty = 2, lwd = 2, col = "red")
# dev.off()

library(ggplot2)
b <- ggplot(dados, aes(BMI,BMD)) +
  geom_point() +
  xlab("BMI (kg/m2)") + ylab("BMD (escala ficitícia)") +
  ggtitle("BMI x BMD")
ggsave("Aulas/Cap18-19/pratica-plot1.png", h = 7, w = 7)

bsmooth <- b + geom_smooth(method = "lm")
ggsave("Aulas/Cap18-19/pratica-plot2.png", h = 7, w = 7)

bsmooth + geom_vline(xintercept = 28, lty = 2, lwd =1, col = "red")
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

BMD2 <- -3*BMI -
  c(
    rnorm(50, 0, 5), # introduzir heterocedasticidade
    rnorm(50, 0, 10),
    rnorm(50, 0, 30),
    rnorm(50, 0, 35)
    ) + 100
heterocedasticidade <-  data.frame(BMI, BMD2)

b2 <- ggplot(heterocedasticidade, aes(BMI, BMD2)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("BMI (kg/m2)") + ylab("BMD (escala ficitícia)") +
  ggtitle("BMI x BMD")
ggsave("Aulas/Cap18-19/pratica-plot-heterocedasticidade.png", h = 7, w = 7)

BMD3 <- -3.3*BMI - scatter + 111 + mean(BMI)*sin(BMI/1.5)*2
heterocedasticidade2 <-  data.frame(BMI, BMD3)
b3 <- ggplot(heterocedasticidade2, aes(BMI, BMD3)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("BMI (kg/m2)") + ylab("BMD (escala ficitícia)") +
  ggtitle("BMI x BMD")
ggsave("Aulas/Cap18-19/pratica-plot-heterocedasticidade-sin.png", h = 7, w = 7)
