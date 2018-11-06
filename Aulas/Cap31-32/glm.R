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

tc.idoso.osteo <- with(dados.rlm, table(idoso, osteo))
fisher.test(tc.idoso.osteo)

modelo5 <- glm(osteo ~ BMI + idade + horm, binomial, dados.rlm)
summary(modelo5)

# graficos ----------------------------------------------------------------

baseplot +
  geom_point(aes()) +
  geom_hline(yintercept = 500, lwd = 1, col = "red", lty = 2) +
  labs(subtitle = "Desfecho binário: [BMD < 500] -> Osteoporose ") +
  annotate("text", x = min(dados.rlm$BMI)*1.2, y = 492.5, label = "Osteoporose", col = "red")
ggsave("Aulas/Cap31-32/pratica-glm4.png", h = 7, w = 7)

baseplot +
  geom_point(aes(col = horm)) +
  geom_hline(yintercept = 500, lwd = 1, col = "red", lty = 2) +
  labs(subtitle = "Desfecho binário: [BMD < 500] -> Osteoporose ") +
  annotate("text", x = min(dados.rlm$BMI)*1.2, y = 492.5, label = "Osteoporose", col = "red")
ggsave("Aulas/Cap31-32/pratica-glm5.png", h = 7, w = 7)
