library(philsfmisc)
RSD <- function(x) sd(x)/mean(x)

## dados
# musculo <- rnorm(32, 90, 5)
# dput(musculo)
musculo <- c(91.84, 89.33, 88.99, 83.59, 91.71, 90.3, 81.25, 93.8, 93.33, 
             90.65, 90.3, 87.1, 90.83, 99.4, 91.84, 83.74, 85.85, 85.87, 89.03, 
             89.4, 89.35, 96.48, 97.2, 91, 90.04, 86.98, 93.12, 93.72, 85.37, 
             96.26, 91.33, 83.32)

format.float(musculo)

ggplot2::ggplot(data.frame(musculo), aes(musculo)) + geom_histogram(binwidth = 5)

format.float(mean(musculo))
format.float(median(musculo))

format.float(musculo-mean(musculo)) # desvios

mad <- mean(abs(musculo - mean(musculo))) # MAD
format.float(mad)

format.float(var(musculo))
format.float(sd(musculo))
format.pct(RSD(musculo))

png("Aulas/Cap3/histograma-musculo.png")
hist(musculo, xlab = "", ylab = "", main = "")
dev.off()

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
