# setup -------------------------------------------------------------------

set.seed(2)
library(ggplot2)
library(tidyr)

# dados -------------------------------------------------------------------

cenario1 <- data.frame(Placebo = rnorm(8, 4, 1), Trat.A = rnorm(8, 4, 1), Trat.B = rnorm(8, 4, 1))
cenario2 <- data.frame(Placebo = rnorm(8, 4, 1), Trat.A = rnorm(8, 6, 1), Trat.B = rnorm(8, 6, 1))
cenario1 <- cbind(cenario1, Genero = sample(c("M", "F"), 8, replace = T))
cenario2 <- cbind(cenario2, Genero = sample(c("M", "F"), 8, replace = T))
cenario1.long <- gather(cenario1, Grupo, y, -Genero)
cenario2.long <- gather(cenario2, Grupo, y, -Genero)

# DataViz -----------------------------------------------------------------

baseplot11 <- ggplot(cenario1.long, aes(Grupo, y, col = Grupo)) +
  geom_point() +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0, 10)) +
  ggtitle("Cenário 1") +
  theme(legend.position = "bottom")
ggsave("Aulas/Topicos_adv/cenario1.png", height = 7, width = 7)

baseplot11 +
  geom_hline(yintercept = apply(cenario1[,1:3], 2, mean), lty = 2, lwd = .3)
ggsave("Aulas/Topicos_adv/cenario1_medias.png", height = 7, width = 7)

baseplot21 <- ggplot(cenario2.long, aes(Grupo, y, col = Grupo)) +
  geom_point() +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0, 10)) +
  ggtitle("Cenário 2") +
  theme(legend.position = "bottom")
ggsave("Aulas/Topicos_adv/cenario2.png", height = 7, width = 7)

baseplot21 +
  geom_hline(yintercept = apply(cenario2[,1:3], 2, mean), lty = 2, lwd = .3)
ggsave("Aulas/Topicos_adv/cenario2_medias.png", height = 7, width = 7)

baseplot12 <- ggplot(cenario1.long, aes(Grupo, y, col = Genero)) +
  geom_point() +
  # scale_x_discrete(labels = NULL) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0, 10)) +
  ggtitle("Cenário 3") +
  theme(legend.position = "bottom")
ggsave("Aulas/Topicos_adv/cenario12.png", height = 7, width = 7)

baseplot12 +
  geom_hline(yintercept = apply(cenario1[,1:3], 2, mean), lty = 2, lwd = .3)
ggsave("Aulas/Topicos_adv/cenario12_medias.png", height = 7, width = 7)

baseplot22 <- ggplot(cenario2.long, aes(Grupo, y, col = Genero)) +
  geom_point() +
  # scale_x_discrete(labels = NULL) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0, 10)) +
  ggtitle("Cenário 4") +
  theme(legend.position = "bottom")
ggsave("Aulas/Topicos_adv/cenario22.png", height = 7, width = 7)

baseplot22 +
  geom_hline(yintercept = apply(cenario2[,1:3], 2, mean), lty = 2, lwd = .3)
ggsave("Aulas/Topicos_adv/cenario22_medias.png", height = 7, width = 7)

# testes t sem correcao ---------------------------------------------------

# cenario 1
format.pval(with(cenario1, t.test(Placebo, Trat.A, var.equal = T)$p.value), scientific = F, digits = 4, eps = 1e-4)
format.pval(with(cenario1, t.test(Placebo, Trat.B, var.equal = T)$p.value), scientific = F, digits = 4, eps = 1e-4)
format.pval(with(cenario1, t.test(Trat.A, Trat.B, var.equal = T)$p.value), scientific = F, digits = 4, eps = 1e-4)

# cenario 2
format.pval(with(cenario2, t.test(Placebo, Trat.A, var.equal = T)$p.value), scientific = F, digits = 4, eps = 1e-4)
format.pval(with(cenario2, t.test(Placebo, Trat.B, var.equal = T)$p.value), scientific = F, digits = 4, eps = 1e-4)
format.pval(with(cenario2, t.test(Trat.A, Trat.B, var.equal = T)$p.value), scientific = F, digits = 4, eps = 1e-4)

# 1-way -------------------------------------------------------------------

anova11 <- aov(y ~ Grupo, cenario1.long)
anova21 <- aov(y ~ Grupo, cenario2.long)

# Bonferroni
anova11.p.bonf <- with(cenario1.long, pairwise.t.test(y, Grupo, p.adjust.method = "bonf"))
anova21.p.bonf <- with(cenario2.long, pairwise.t.test(y, Grupo, p.adjust.method = "bonf"))

# Tukey
anova11.p.tukey <- TukeyHSD(anova11)
anova21.p.tukey <- TukeyHSD(anova21)

# 2-way -------------------------------------------------------------------

# sem interacao
anova12 <- update(anova11, . ~ . + Genero)
anova22 <- update(anova21, . ~ . + Genero)

# Bonferroni
# anova12.p.bonf <- with(cenario1.long, pairwise.t.test(y, Grupo, p.adjust.method = "bonf"))
# anova22.p.bonf <- with(cenario2.long, pairwise.t.test(y, Grupo, p.adjust.method = "bonf"))

# Tukey
anova12.p.tukey <- TukeyHSD(anova12)
anova22.p.tukey <- TukeyHSD(anova22)

# com interacao
summary(aov(y ~ Grupo + Genero, cenario2.long))
summary(aov(y ~ Grupo * Genero, cenario2.long))
TukeyHSD(aov(y ~ Grupo * Genero, cenario2.long))