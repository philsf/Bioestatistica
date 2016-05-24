## Exemplo aula Correlação & Regressão

## Dados ####
x <- c(17.9, 18.3, 18.3, 18.4, 18.4, 20.2, 20.3, 21.8, 21.9, 22.1, 23.1, 24.2, 24.4)
y <- c(250, 220, 145, 115, 230, 200, 330, 400, 370, 260, 270, 530, 375)
plot(y~x, pch=20, xlim = c(16,27), ylim = c(0,600), xlab="", ylab="")
leg.txt <- paste("r = ", round(cor(x,y), digits = 2))
legend("topleft",legend = leg.txt, border = "")

cor(x,y)

## Regressão ####
reta <- lm(y ~ x)
summary(reta)
abline(reta, lwd = 2, col = "blue")
