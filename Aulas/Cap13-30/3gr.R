# setup -------------------------------------------------------------------

# seed  8 =  A x B  - M = 1
# seed 55 =  ANOVA sig
# seed 72 = Plac x A **
# seed 76 = A x B .024
# seed 101 = Plac X B .047
# seed 116 = Plac X B .039
# seed 145 = Plac X A .025

set.seed(145)

library(philsfmisc)
library(ggplot2)
library(tidyr)
library(data.table)

# dados -------------------------------------------------------------------

# cenario1 <- data.table(Placebo = rnorm(8, 5, 1), Trat.A = rnorm(8, 5, 1), Trat.B = rnorm(8, 5, 1))
# cenario2 <- data.table(Placebo = rnorm(8, 4, 1), Trat.A = rnorm(8, 6, 1), Trat.B = rnorm(8, 6, 1))
# cenario1 <- cbind(cenario1, Genero = sample(c("M", "F"), 8, replace = T))
# cenario2 <- cbind(cenario2, Genero = sample(c("M", "F"), 8, replace = T))
# fwrite(cenario1, "Aulas/Cap13-30/cenario1.csv")
# fwrite(cenario2, "Aulas/Cap13-30/cenario2.csv")

cenario1 <- fread("Aulas/Cap13-30/cenario1.csv")
cenario2 <- fread("Aulas/Cap13-30/cenario2.csv")
cenario1$Genero <- factor(cenario1$Genero)
cenario2$Genero <- factor(cenario2$Genero)
cenario1.long <- data.table(gather(cenario1, Grupo, y, -Genero))
cenario2.long <- data.table(gather(cenario2, Grupo, y, -Genero))
cenario1.long$Grupo <- factor(cenario1.long$Grupo)
cenario2.long$Grupo <- factor(cenario2.long$Grupo)

# médias ------------------------------------------------------------------

medias11 <- cenario1.long[, .(M= mean(y)), by = Grupo]
medias21 <- cenario2.long[, .(M= mean(y)), by = Grupo]

# testes t sem correcao ---------------------------------------------------

# cenario 1
# format.pval(with(cenario1, t.test(Placebo, Trat.A, var.equal = T)$p.value), scientific = F, digits = 4, eps = 1e-4)
# format.pval(with(cenario1, t.test(Placebo, Trat.B, var.equal = T)$p.value), scientific = F, digits = 4, eps = 1e-4)
# format.pval(with(cenario1, t.test(Trat.A, Trat.B, var.equal = T)$p.value), scientific = F, digits = 4, eps = 1e-4)
with(cenario1.long, pairwise.t.test(y, Grupo, pool.sd = FALSE, p.adjust.method = "none"))

# cenario 2
# format.pval(with(cenario2, t.test(Placebo, Trat.A, var.equal = T)$p.value), scientific = F, digits = 4, eps = 1e-4)
# format.pval(with(cenario2, t.test(Placebo, Trat.B, var.equal = T)$p.value), scientific = F, digits = 4, eps = 1e-4)
# format.pval(with(cenario2, t.test(Trat.A, Trat.B, var.equal = T)$p.value), scientific = F, digits = 4, eps = 1e-4)
with(cenario2.long, pairwise.t.test(y, Grupo, pool.sd = FALSE, p.adjust.method = "none"))

# 1-way -------------------------------------------------------------------

anova11 <- aov(y ~ Grupo, cenario1.long)
anova21 <- aov(y ~ Grupo, cenario2.long)

# Bonferroni
anova11.p.bonf <- with(cenario1.long, pairwise.t.test(y, Grupo, pool.sd = FALSE, p.adjust.method = "bonf"))
anova21.p.bonf <- with(cenario2.long, pairwise.t.test(y, Grupo, pool.sd = FALSE, p.adjust.method = "bonf"))

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

# DataViz -----------------------------------------------------------------

# The colorblind palette with black:
cbbPalette <- c("#984ea3", "#a65628", "#ff7f00", "#377eb8", "#e41a1c", "#4daf4a")

grupos.color <- cbbPalette[1:3] # c("#1b9e77", "#d95f02", "#7570b3")
generos.color <- cbbPalette[4:5]
grupos.axis.label.color <- element_text(color = grupos.color, face = "bold")

baseplot11 <- ggplot(cenario1.long, aes(Grupo, y, col = Grupo)) +
  geom_point() +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0, 10)) +
  ggtitle("Cenário 1") +
  scale_colour_manual(values=grupos.color) +
  theme(legend.position = "bottom")
ggsave("Aulas/Cap13-30/cenario11.png", height = 7, width = 7)

# baseplot11 +
#   geom_point(aes(y = M, fill = Grupo), data = medias11, size = 4, shape = 23)
baseplot11 +
  geom_hline(yintercept = apply(cenario1[,1:3], 2, mean), lty = 2, lwd = .6, col = grupos.color)
ggsave("Aulas/Cap13-30/cenario11_medias.png", height = 7, width = 7)

baseplot21 <- ggplot(cenario2.long, aes(Grupo, y, col = Grupo)) +
  geom_point() +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0, 10)) +
  ggtitle("Cenário 2") +
  scale_colour_manual(values=grupos.color) +
  theme(legend.position = "bottom")
ggsave("Aulas/Cap13-30/cenario21.png", height = 7, width = 7)

# baseplot21 +
#   geom_point(aes(y = M, col = Grupo, fill = Grupo), data = medias21, size = 4, shape = 23)
baseplot21 +
  geom_hline(yintercept = apply(cenario2[,1:3], 2, mean), lty = 2, lwd = .6, col = grupos.color)
ggsave("Aulas/Cap13-30/cenario21_medias.png", height = 7, width = 7)

baseplot12 <- ggplot(cenario1.long, aes(Grupo, y, col = Genero)) +
  geom_point() +
  # scale_x_discrete(labels = NULL) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0, 10)) +
  ggtitle("Cenário 3") +
  scale_colour_manual(values=generos.color) +
  theme(legend.position = "bottom")
ggsave("Aulas/Cap13-30/cenario12.png", height = 7, width = 7)

# baseplot12 +
#   geom_point(aes(y = M, col = Grupo, fill = Grupo), data = medias11, size = 4, shape = 23)
baseplot12 +
  geom_hline(yintercept = apply(cenario1[,1:3], 2, mean), lty = 2, lwd = .6, col = grupos.color)
ggsave("Aulas/Cap13-30/cenario12_medias.png", height = 7, width = 7)

baseplot22 <- ggplot(cenario2.long, aes(Grupo, y, col = Genero)) +
  geom_point() +
  # scale_x_discrete(labels = NULL) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0, 10)) +
  ggtitle("Cenário 4") +
  scale_colour_manual(values=generos.color) +
  theme(legend.position = "bottom")
ggsave("Aulas/Cap13-30/cenario22.png", height = 7, width = 7)

# baseplot22 +
#   geom_point(aes(y = M, col = Grupo), data = medias21, size = 4, shape = 23)
baseplot22 +
  geom_hline(yintercept = apply(cenario2[,1:3], 2, mean), lty = 2, lwd = .6, col = grupos.color)
ggsave("Aulas/Cap13-30/cenario22_medias.png", height = 7, width = 7)
