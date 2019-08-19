library(philsfmisc)
RSD <- function(x) sd(x)/mean(x)

## dados
colesterol <- c(144, 146, 139, 155, 144, 148)
mean(colesterol); median(colesterol)
colesterol2 <- colesterol; colesterol2[3] <- 13
mean(colesterol2) ; median(colesterol2)

D <- colesterol - round(mean(colesterol)) # desvios
sum(D) # soma dos desvios
MAD <- format.float(mean(abs(colesterol - mean(colesterol))), 1) # MAD
format.float(var(colesterol), 1)
format.float(sd(colesterol), 1)
format.pct(RSD(colesterol), 0)

# plot(colesterol, ylim = (c(0, 200)), ylab = "colesterol", xlab = "")
# plot(colesterol2, ylim = (c(0, 200)), ylab = "colesterol*", xlab = "")
png("Aulas/Cap3/histograma-colesterol.png")
hist(colesterol, main = "Colesterol")
abline(v=c(round(mean(colesterol)), median(colesterol)), lty = 2, lwd =2, col = c("blue", "red"))
dev.off()
png("Aulas/Cap3/histograma-colesterol2.png")
hist(colesterol2, main = "Colesterol*")
abline(v=c(mean(colesterol2), median(colesterol2)), lty = 2, lwd =2, col = c("blue", "red"))
dev.off()
