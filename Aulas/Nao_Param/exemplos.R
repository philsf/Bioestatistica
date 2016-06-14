## normal x lognormal ####
nl <- read.table("Aulas/Nao_Param/normal-lognormal.dat")
attach(nl)

par(mfrow = c(1,2))
hist(x1, main = "")
hist(x2, main = "")
par(mfrow = c(1,1))
boxplot(x1)
boxplot(x2)
par(mfrow = c(1,2))
qqnorm(x1); qqline(x1); qqnorm(x2); qqline(x2)
shapiro.test(x1)
shapiro.test(x2)

## Transformação ####
par(mfrow = c(1,2))
hist(x2); hist(log(x2))
hist(x2); qqnorm(log(x2)); qqline(log(x2))
hist(log(x2)); qqnorm(log(x2)); qqline(log(x2))
qqnorm(x2, main = "Dados originais"); qqline(x2); qqnorm(log(x2), main = "Dados (log)"); qqline(log(x2))
shapiro.test(log(x2))

detach(nl)
rm(nl)

## 2 amostras ####
twosamples <- read.table("Aulas/Nao_Param/2samples.dat")
attach(twosamples)

par(mfrow = c(1,1))
boxplot(x,y)
par(mfrow = c(1,2))
hist(x, main = "Amostra 1")
hist(y, main = "Amostra 2")
qqnorm(x, main = "Amostra 1"); qqline(x); qqnorm(y, main = "Amostra 2"); qqline(y)

detach(twosamples)
rm(twosamples)

## 3 amostras ####
m.p <- aov(Ozone ~ Month, data = airquality)
m.np <- kruskal.test(Ozone ~ Month, data = airquality)
m.ols <- lm(Ozone ~ Month, data = airquality)

summary(m.p) # não significativo
m.np # significativo
summary(m.ols)
