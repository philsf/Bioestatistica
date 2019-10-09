prog <- c(76, 129)
nprog <- c(399,332)
exemplo8.1 <- cbind(prog, nprog)
rownames(exemplo8.1) <- c("AZT", "Placebo")
colnames(exemplo8.1) <- c("Progrediu", "Não progrediu")

png("Aulas/Teste_qui2/barplot.png")
barplot(exemplo8.1, beside = T, ylim = c(0,500), legend.text = T)
dev.off()

exemplo3 <- matrix(c(157, 18, 54, 268, 44, 34), byrow = T, nrow = 2)
colnames(exemplo3) <- c("confirmado", "incompleto", "incorreto")
rownames(exemplo3) <- c("A", "B")
