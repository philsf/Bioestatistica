library(philsfmisc)
RSD <- function(x) sd(x)/mean(x)

## dados
bp <- c(144, 146, 139, 155, 144, 148)
round(mean(bp)); median(bp)
bp2 <- bp; bp2[3] <- 13
round(mean(bp2)) ; median(bp2)

D <- bp - round(mean(bp)) # desvios
sum(D) # soma dos desvios
MAD <- format.float(mean(abs(bp - mean(bp))), 0) # MAD
format.float(var(bp), 1)
format.float(sd(bp), 1)
format.pct(RSD(bp), 0)

# plot(bp, ylim = (c(0, 200)), ylab = "BP", xlab = "")
# plot(bp2, ylim = (c(0, 200)), ylab = "BP*", xlab = "")
png("Aulas/Cap3/histograma-bp.png")
hist(bp, main = "BP")
abline(v=c(round(mean(bp)), median(bp)), lty = 2, lwd =2, col = c("blue", "red"))
dev.off()
png("Aulas/Cap3/histograma-bp2.png")
hist(bp2, main = "BP*")
abline(v=c(mean(bp2), median(bp2)), lty = 2, lwd =2, col = c("blue", "red"))
dev.off()
