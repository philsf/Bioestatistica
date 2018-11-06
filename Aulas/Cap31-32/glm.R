library(philsfmisc)

# dados simulados ---------------------------------------------------------

dados.rlm <- fread("Aulas/Cap31-32/dados-rlm.csv", stringsAsFactors = TRUE)
dados.rlm$horm <- factor(dados.rlm$horm, levels = c("baixo", "medio", "alto"))
dados.rlm$osteo <- relevel(dados.rlm$osteo, "Sadio")
dados.rlm$obeso <- cut(dados.rlm$BMI, c(-Inf, 30, Inf), c("Nao Obeso", "Obeso"))
dados.rlm$idoso <- cut(dados.rlm$BMI, c(-Inf, 60, Inf), c("Nao Idoso", "Idoso"))
summary(dados.rlm)

# modelos -----------------------------------------------------------------

modelo4 <- glm(osteo ~ idoso, binomial, dados.rlm)
summary(modelo4)

c(format.float(exp(coef(modelo4)[2])), format.interval(exp(confint.default(modelo4)[2, ])))

tc.idoso.osteo <- with(dados.rlm, table(idoso, osteo))
fisher.test(tc.idoso.osteo)

modelo5 <- glm(osteo ~ BMI + idade + horm, binomial, dados.rlm)
summary(modelo5)

c(format.float(exp(coef(modelo5)[2])), format.interval(exp(confint.default(modelo5)[2, ])))
c(format.float(exp(coef(modelo5)[3])), format.interval(exp(confint.default(modelo5)[3, ])))
c(format.float(exp(coef(modelo5)[4])), format.interval(exp(confint.default(modelo5)[4, ])))
c(format.float(exp(coef(modelo5)[5])), format.interval(exp(confint.default(modelo5)[5, ])))

# graficos ----------------------------------------------------------------

ggplot(dados.rlm, aes(idade, BMD)) +
  xlab("idade (anos)") + ylab("BMD (escala ficitícia)") +
  xlim(range(dados.rlm$idade)) + ylim(range(dados.rlm$BMD)) +
  geom_point(aes()) +
  geom_hline(yintercept = 500, lwd = 1, col = "red", lty = 2) +
  annotate("text", x = min(dados.rlm$idade)*1.2, y = 492.5, label = "Osteoporose", col = "red") +
  geom_vline(xintercept = 60, lwd = 1, col = "blue", lty = 2) +
  annotate("text", x = 63, y = 450,  label = "Idoso", col = "blue") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(subtitle = "Desfecho binário: [BMD < 500] -> Osteoporose ") +
  ggtitle("Idoso x Osteoporose")
ggsave("Aulas/Cap31-32/pratica-glm4.png", h = 7, w = 7)

baseplot +
  geom_point(aes(col = horm)) +
  labs(subtitle = "Desfecho binário: [BMD < 500] -> Osteoporose ") +
  geom_hline(yintercept = 500, lwd = 1, col = "red", lty = 2) +
  annotate("text", x = min(dados.rlm$BMI)*1.2, y = 492.5, label = "Osteoporose", col = "red") +
ggsave("Aulas/Cap31-32/pratica-glm5.png", h = 7, w = 7)
