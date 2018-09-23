ob <- read.csv("Aulas/Cap23-25/obesidade-teste-t.csv")

ob2 <- tidyr::gather(ob, Grupo, Perda)

png("Aulas/Cap23-25/obesidade-hist.png")
par(mfrow = c(1, 2))
hist(ob$Grupo1, main = "Grupo 1", xlab = "Perda (lbs)")
hist(ob$Grupo2, main = "Grupo 2", xlab = "Perda (lbs)")
dev.off()

t.test(Perda ~ Grupo, data = ob2, var.equal = TRUE)
t.test(Perda ~ Grupo, data = ob2, var.equal = TRUE, paired = TRUE)
t.test(Perda ~ Grupo, data = ob2)
