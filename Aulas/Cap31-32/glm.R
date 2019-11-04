library(philsfmisc)

# dados simulados ---------------------------------------------------------

source('~/Documents/Docencia/Bioestatistica/Aulas/Cap31-32/rlm.R')
summary(dados.rlm)

# modelos -----------------------------------------------------------------

glm.modelo4 <- glm(osteo ~ idoso, binomial, dados.rlm)
summary(glm.modelo4)

paste0("OR: ", format.float(exp(coef(glm.modelo4)[2])), ", IC: ", format.interval(exp(confint.default(glm.modelo4)[2, ])))

tc.idoso.osteo <- with(dados.rlm, table(idoso, osteo))
fisher.test(tc.idoso.osteo)
paste0("OR: ", format.float(fisher.test(tc.idoso.osteo)$estimate), ", IC: ", format.interval(fisher.test(tc.idoso.osteo)$conf.int))

glm.modelo5 <- glm(osteo ~ BMI + idade + vitD, binomial, dados.rlm)
summary(glm.modelo5)

paste0("OR: ", format.float(exp(coef(glm.modelo5)[2])), ", IC: ", format.interval(exp(confint.default(glm.modelo5)[2, ])))
paste0("OR: ", format.float(exp(coef(glm.modelo5)[3])), ", IC: ", format.interval(exp(confint.default(glm.modelo5)[3, ])))
paste0("OR: ", format.float(exp(coef(glm.modelo5)[4]), 7), ", IC: ", format.interval(exp(confint.default(glm.modelo5)[4, ]), 7))
paste0("OR: ", format.float(exp(coef(glm.modelo5)[5]), 7), ", IC: ", format.interval(exp(confint.default(glm.modelo5)[5, ]), 7))

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
  geom_point(aes(col = vitD)) +
  labs(subtitle = "Desfecho binário: [BMD < 500] -> Osteoporose ") +
  geom_hline(yintercept = 500, lwd = 1, col = "red", lty = 2) +
  annotate("text", x = min(dados.rlm$BMI)*1.2, y = 492.5, label = "Osteoporose", col = "red") +
ggsave("Aulas/Cap31-32/pratica-glm5.png", h = 7, w = 7)
