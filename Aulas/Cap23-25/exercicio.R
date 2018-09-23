ob <- read.csv("Aulas/Cap23-25/obesidade-teste-t.csv")

ob2 <- tidyr::gather(ob, Grupo, Perda)

png("Aulas/Cap23-25/obesidade-independentes.png")
boxplot(Perda ~ Grupo, data = ob2, ylab = "Perda de peso (lbs)", col = "gray")
abline(h = 0, lty = 2)
dev.off()

png("Aulas/Cap23-25/obesidade-pareadas.png")
with(ob,
     plot(Grupo1, Grupo2,
          xlab = "Perda de peso (lbs) Grupo 1",
          ylab = "Perda de peso (lbs) Grupo 2")
     )
abline(h = 0, v=0, lty = 2)
dev.off()

png("Aulas/Cap23-25/obesidade-hist1.png")
hist(ob2$Perda, main = "Grupo 1 + 2", xlab = "Perda (lbs)", col = "gray")
dev.off()

png("Aulas/Cap23-25/obesidade-hist2.png", 600, 400)
par(mfrow = c(1, 2))
hist(ob$Grupo1, main = "Grupo 1", xlab = "Perda (lbs)", col = "gray", ylim = c(0, 50))
hist(ob$Grupo2, main = "Grupo 2", xlab = "Perda (lbs)", col = "gray", ylim = c(0, 50))
dev.off()

t.test(Perda ~ Grupo, data = ob2, var.equal = TRUE)
t.test(Perda ~ Grupo, data = ob2, var.equal = TRUE, paired = TRUE)
t.test(Perda ~ Grupo, data = ob2)
