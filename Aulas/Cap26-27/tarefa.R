N.vitamina <- 407
N.placebo <- 411
d.vitamina <- 105
d.placebo <- 76
grupo.placebo <- c(d.placebo, N.placebo - d.placebo)
grupo.vitamina <- c(d.vitamina, N.vitamina - d.vitamina)

tc <- matrix(c(grupo.placebo, grupo.vitamina), nrow = 2, byrow = TRUE)
rownames(tc) <- c("Placebo", "Vitamina")
colnames(tc) <- c("Livre de resfriado", "Com resfriado")
tc <- as.table(tc)
print(addmargins(tc))

# X2 do paper =  5.92
chisq.test(tc) # X2 = 5.9196, p = 0.01497
chisq.test(tc, correct = FALSE) # X2 = 6.3366, p = 0.01183
fisher.test(tc) # OR = 0.65, IC = [0.46, 0.92], p = 0.01444
