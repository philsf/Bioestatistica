library(philsfmisc)
library(data.table)

# exemplo -----------------------------------------------------------------

exemplo17 <- data.table(
  Gordura = c(17.9, 18.3, 18.3, 18.4, 18.4, 20.2, 20.3, 21.8, 21.9, 22.1, 23.1, 24.2, 24.4),
  Insulina = c(250, 220, 145, 115, 230, 200, 330, 400, 370, 260, 270, 530, 375)
)
with(exemplo17, cor.test(Gordura, Insulina))

# dados -------------------------------------------------------------------

set.seed(1)
P <- rnorm(30, 10, sd = 0.5)
M <- rnorm(30, 10, sd = 1.5)
G <- rnorm(30, 10, sd = 5.0)
GP <- G + rnorm(30,sd = 1)
GM <- G + rnorm(30,sd = 5)
GG <- G + rnorm(30,sd = 12)

DF <- data.frame(P,M,G)
summary(DF)

library(tidyr)

pch = 1

png("Aulas/Cap17/dot-P.png")
stripchart(P, xlim = c(0,25), pch = pch, sub = paste("var = ", format.float(var(P),1)), xlab = "P")
dev.off()
png("Aulas/Cap17/dot-M.png")
stripchart(M, xlim = c(0,25), pch = pch, sub = paste("var = ", format.float(var(M),1)), xlab = "M")
dev.off()
png("Aulas/Cap17/dot-G.png")
stripchart(G, xlim = c(0,25), pch = pch, sub = paste("var = ", format.float(var(G),1)), xlab = "G")
dev.off()

png("Aulas/Cap17/dot-box-P.png")
par(mfrow = c(1,2))
stripchart(P, ylim = c(0,25), pch = pch, vertical = T)
boxplot(P, ylim = c(0,25))
var(P)
dev.off()
png("Aulas/Cap17/dot-box-M.png")
par(mfrow = c(1,2))
stripchart(M, ylim = c(0,25), pch = pch, vertical = T)
boxplot(M, ylim = c(0,25))
var(M)
dev.off()
png("Aulas/Cap17/dot-box-G.png")
par(mfrow = c(1,2))
stripchart(G, ylim = c(0,25), pch = pch, vertical = T)
boxplot(G, ylim = c(0,25))
var(G)
dev.off()

png("Aulas/Cap17/anim-0.png")
plot(G,G, xlim = c(0,25), ylim = c(-50,50), sub = paste("cov =", format.float(cov(G,G), 1)))
dev.off()
png("Aulas/Cap17/anim-1.png")
plot(G,GP, xlim = c(0,25), ylim = c(-50,50), sub = paste("cov =", format.float(cov(G,GP), 1)))
dev.off()
png("Aulas/Cap17/anim-2.png")
plot(G,GM, xlim = c(0,25), ylim = c(-50,50), sub = paste("cov =", format.float(cov(G,GM), 1)))
dev.off()
png("Aulas/Cap17/anim-3.png")
plot(G,GG, xlim = c(0,25), ylim = c(-50,50), sub = paste("cov =", format.float(cov(G,GG), 1)))
dev.off()

png("Aulas/Cap17/anim-n.png")
plot(G,seq(1,30), xlim = c(0,25), ylim = c(-50,50), sub = paste("cov =", format.float(cov(G,rep(1,30)), 1)), ylab = "1, 2, 3, ...")
dev.off()

cor.test(G,G)
cor.test(G,GP)
cor.test(G,GM)
cor.test(G,GG)
cor.test(G,seq(1,30))

# Problema 6
serotonin <- c(2,5, 6, 10, 15, 60, 65, 165)
cross.sec <- c(4, 7, 28, 26, 30, 34, 35, 42)
cor.test(log(serotonin), cross.sec)
