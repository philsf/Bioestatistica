prog <- c(76, 129)
nprog <- c(399,332)
exemplo8.1 <- cbind(prog, nprog)
rownames(exemplo8.1) <- c("AZT", "Placebo")
colnames(exemplo8.1) <- c("Progrediu", "Não progrediu")

png("Aulas/Teste_qui2/barplot.png")
barplot(exemplo8.1, beside = T, ylim = c(0,500), legend.text = T)
dev.off()
