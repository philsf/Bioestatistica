ob <- read.csv("Aulas/Cap23-25/obesidade-teste-t.csv")

ob2 <- tidyr::gather(ob, Grupo, Perda)

png("Aulas/Cap23-25/obesidade-hist1.png")
hist(ob2$Perda, main = "Grupo 1 + 2", xlab = "Perda (lbs)", col = "gray")
dev.off()

png("Aulas/Cap23-25/obesidade-hist2.png", 600, 400)
par(mfrow = c(1, 2))
hist(ob$Grupo1, main = "Grupo 1", xlab = "Perda (lbs)", col = "gray")
hist(ob$Grupo2, main = "Grupo 2", xlab = "Perda (lbs)", col = "gray")
dev.off()

t.test(Perda ~ Grupo, data = ob2, var.equal = TRUE)
t.test(Perda ~ Grupo, data = ob2, var.equal = TRUE, paired = TRUE)
t.test(Perda ~ Grupo, data = ob2)
