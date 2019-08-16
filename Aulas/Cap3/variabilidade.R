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

bp <- c(142, 144, 176, 203, 134, 191)
sort(bp)
bp2 <- bp
bp2[1] <- 14

plot(bp, ylim = (c(0, 200)), ylab = "BP", xlab = "")
hist(bp, main = "BP")
plot(bp2, ylim = (c(0, 200)), ylab = "BP*", xlab = "")
hist(bp2, main = "BP*")
