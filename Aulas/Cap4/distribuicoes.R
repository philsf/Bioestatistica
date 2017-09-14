# setup -------------------------------------------------------------------
set.seed(20) # exemplo reprodutível
par(mfrow = c(2,2))

# Normal ------------------------------------------------------------------

# testar até achar uma vistualização didaticamente adequada
h <- hist(la <- rnorm(100, 123.4, 14), main = "", xlab = "")

# h <- hist(la, main = "")
xfit <- seq(min(la), max(la),length.out = 100)
yfit <- dnorm(xfit, mean = mean(la), sd = sd(la))
yfit <- yfit * diff(h$mids[1:2]) * length(la)
lines(xfit, yfit, col = "black", lwd = 2)
abline(v = 123.4 + c(-1,1)*14, lty = 2, lwd = 1.5, col = "blue")

png("Aulas/Cap4/normal1.png")
plot(h, xlim = range(la)*c(.9,1.1), main = "", xlab = "", ylim = c(0, round(max(h$counts)*1.2)))
dev.off()

png("Aulas/Cap4/normal2.png")
plot(h, xlim = range(la)*c(.9,1.1), main = "", xlab = "", ylim = c(0, round(max(h$counts)*1.2)))
lines(xfit, yfit, col = "black", lwd = 2)
dev.off()

png("Aulas/Cap4/normal3.png")
plot(h, xlim = range(la)*c(.9,1.1), main = "", xlab = "", ylim = c(0, round(max(h$counts)*1.2)))
abline(v = 123.4 + c(-1,1)*14, lty = 2, lwd = 1.5, col = "blue")
dev.off()

png("Aulas/Cap4/normal4.png")
plot(h, xlim = range(la)*c(.9,1.1), main = "", xlab = "", ylim = c(0, round(max(h$counts)*1.2)))
lines(xfit, yfit, col = "black", lwd = 2)
abline(v = 123.4 + c(-1,1)*14, lty = 2, lwd = 1.5, col = "blue")
dev.off()


# log-normal --------------------------------------------------------------

h <- hist(la <- rlnorm(100, 5), main = "", xlab = "")

png("Aulas/Cap4/lognormal.png")
plot(h, xlim = range(la)*c(.9,1.2), main = "", xlab = "", ylim = c(0, round(max(h$counts)*1.2)))
dev.off()


# poisson -----------------------------------------------------------------

h <- hist(la <- rpois(100, 3), main = "", xlab = "")

png("Aulas/Cap4/poisson.png")
plot(h, xlim = range(la)*c(.9,1.1), main = "", xlab = "", ylim = c(0, round(max(h$counts)*1.2)))
dev.off()


# uniforme ----------------------------------------------------------------

h <- hist(la <- runif(100, 80, 120), main = "", xlab = "")

png("Aulas/Cap4/uniforme.png")
plot(h, xlim = range(la)*c(.9,1.1), main = "", xlab = "", ylim = c(0, round(max(h$counts)*1.2)))
dev.off()
