library(philsfmisc)
library(data.table)

# dados simulados ---------------------------------------------------------

dados.rlm <- fread("Aulas/Cap31-32/dados-rlm.csv")
summary(dados.rlm)

# modelos -----------------------------------------------------------------

modelo1 <- lm(BMD ~ BMI, data = dados.rlm)
modelo2 <- lm(BMD ~ BMI + etnia, dados.rlm)
modelo2.1 <- lm(BMD ~ BMI + idade, data = dados.rlm)
modelo2.2 <- lm(BMD ~ BMI + horm, data = dados.rlm)
modelo3 <- lm(BMD ~ BMI + idade + horm, data = dados.rlm)

print(summary(modelo1))
print(summary(modelo2))
print(summary(modelo2.1))
print(summary(modelo2.2))
print(summary(modelo3))

# graficos ----------------------------------------------------------------

library(ggplot2)
baseplot <- ggplot(dados.rlm, aes(BMI, BMD)) +
  xlab("BMI (kg/m2)") + ylab("BMD (escala ficitícia)") +
  xlim(range(dados.rlm$BMI)) + ylim(range(dados.rlm$BMD)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  ggtitle("BMI x BMD")

baseplot + geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Modelo 1 - BMI x BMD")
ggsave("Aulas/Cap31-32/pratica-rlm1.png", h = 7, w = 7)

ggplot(data.frame(Fitted = fitted(modelo1), Residuals = residuals(modelo1)), aes(Fitted, Residuals)) +
  geom_point() +
  ylim(c(-300, 300)) +
  theme_bw() +
  ggtitle("Modelo 1 - Valores ajustados x Resíduos", subtitle = "Apenas BMI")
ggsave("Aulas/Cap31-32/pratica-rlm1-resid.png", h = 7, w = 7)

baseplot +
  geom_point(aes(col = etnia)) +
  geom_smooth(method = "lm") +
  ggtitle("BMI x BMD")
ggsave("Aulas/Cap31-32/pratica-rlm2_0.png", h = 7, w = 7)

ggplot(data.frame(Fitted = fitted(modelo2), Residuals = residuals(modelo2)), aes(Fitted, Residuals)) +
  geom_point() +
  ylim(c(-300, 300)) +
  theme_bw() +
  ggtitle("Modelo 2 - Valores ajustados x Resíduos", subtitle = "BMI ajustado por etnia")
ggsave("Aulas/Cap31-32/pratica-rlm2_0-resid.png", h = 7, w = 7)

baseplot +
  geom_point(aes(col = idade)) +
  geom_smooth(method = "lm")
ggsave("Aulas/Cap31-32/pratica-rlm2_1.png", h = 7, w = 7)

ggplot(data.frame(Fitted = fitted(modelo2.1), Residuals = residuals(modelo2.1)), aes(Fitted, Residuals)) +
  geom_point() +
  ylim(c(-300, 300)) +
  theme_bw() +
  ggtitle("Modelo 2.1 - Valores ajustados x Resíduos", subtitle = "BMI ajustado por idade")
ggsave("Aulas/Cap31-32/pratica-rlm2_1-resid.png", h = 7, w = 7)

ggplot(data.frame(Fitted = fitted(modelo2.2), Residuals = residuals(modelo2.2)), aes(Fitted, Residuals)) +
  geom_point() +
  ylim(c(-300, 300)) +
  theme_bw() +
  ggtitle("Modelo 2.2 - Valores ajustados x Resíduos", subtitle = "BMI ajustado por hormônio")
ggsave("Aulas/Cap31-32/pratica-rlm2_2-resid.png", h = 7, w = 7)

baseplot +
  geom_point(aes(col = horm)) +
  geom_smooth(method = "lm")
ggsave("Aulas/Cap31-32/pratica-rlm3.png", h = 7, w = 7)

ggplot(data.frame(Fitted = fitted(modelo3), Residuals = residuals(modelo3)), aes(Fitted, Residuals)) +
  geom_point() +
  ylim(c(-300, 300)) +
  theme_bw() +
  ggtitle("Modelo 3 - Valores ajustados x Resíduos", subtitle = "BMI ajustado por idade e hormônio")
ggsave("Aulas/Cap31-32/pratica-rlm3-resid.png", h = 7, w = 7)

ggplot(data.frame(Fitted = fitted(modelo3), Residuals = residuals(modelo3)), aes(Fitted, Residuals)) +
  geom_point() +
  ylim(c(-80, 80)) +
  theme_bw() +
  ggtitle("Modelo 3 - Valores ajustados x Resíduos", subtitle = "BMI ajustado por idade e hormônio")
ggsave("Aulas/Cap31-32/pratica-rlm3-resid-zoom.png", h = 7, w = 7)
