set.seed(2)
library(ggplot2)
library(tidyr)
cenario1 <- data.frame(Placebo = rnorm(8, 4, 1), Trat.A = rnorm(8, 4, 1), Trat.B = rnorm(8, 4, 1))
cenario1.long <- gather(cenario1, Grupo, y)

format.pval(with(cenario1, t.test(Placebo, Trat.A, var.equal = T)$p.value), scientific = F, digits = 4, eps = 1e-4)
format.pval(with(cenario1, t.test(Placebo, Trat.B, var.equal = T)$p.value), scientific = F, digits = 4, eps = 1e-4)
format.pval(with(cenario1, t.test(Trat.A, Trat.B, var.equal = T)$p.value), scientific = F, digits = 4, eps = 1e-4)

baseplot <- ggplot(cenario1.long, aes(Grupo, y, col = Grupo)) +
  geom_point() +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(limits = c(0,10)) +
  ggtitle("Cenário 1") +
  theme(legend.position = "bottom")
ggsave("Aulas/Topicos_adv/cenario1.png")

baseplot +
  geom_hline(yintercept = apply(cenario1, 2, mean), lty = 2, lwd = .3)
ggsave("Aulas/Topicos_adv/cenario1_medias.png")

cenario2 <- data.frame(Placebo = rnorm(8, 4, 1), Trat.A = rnorm(8, 6, 1), Trat.B = rnorm(8, 6, 1))
cenario2.long <- gather(cenario2, Grupo, y)

format.pval(with(cenario2, t.test(Placebo, Trat.A, var.equal = T)$p.value), scientific = F, digits = 4, eps = 1e-4)
format.pval(with(cenario2, t.test(Placebo, Trat.B, var.equal = T)$p.value), scientific = F, digits = 4, eps = 1e-4)
format.pval(with(cenario2, t.test(Trat.A, Trat.B, var.equal = T)$p.value), scientific = F, digits = 4, eps = 1e-4)

baseplot2 <- ggplot(cenario2.long, aes(Grupo, y, col = Grupo)) +
  geom_point() +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(limits = c(0,10)) +
  ggtitle("Cenário 2") +
  theme(legend.position = "bottom")
ggsave("Aulas/Topicos_adv/cenario2.png")

baseplot2 +
  geom_hline(yintercept = apply(cenario2, 2, mean), lty = 2, lwd = .3)
ggsave("Aulas/Topicos_adv/cenario2_medias.png")

anova1 <- aov(y ~ Grupo, cenario1.long)
anova2 <- aov(y ~ Grupo, cenario2.long)

anova1.p.bonf <- with(dados2.long, pairwise.t.test(y, Grupo, p.adjust.method = "bonf"))
anova2.p.bonf <- with(dados2.long, pairwise.t.test(y, Grupo, p.adjust.method = "bonf"))
anova1.p.tukey <- TukeyHSD(anova1)
anova2.p.tukey <- TukeyHSD(anova2)

# format.pval(la$p.value, scientific =F, eps = 1e-4, digits = 4)
# format.pval(la$p.value, scientific =F, eps = 1e-4, digits = 4)