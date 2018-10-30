library(data.table)

set.seed(1)
BMI <- c(rnorm(100, 30, 5), rnorm(100, 50, 5))
idade <- round(rnorm(200, 60, 5))
scatter <- rnorm(200, 0, 20)
etnia <- sample(gl(2, 100, labels = c("branca", "parda")))
horm <- round(sin(BMI))
BMD <- -7*BMI - 10*idade - 100*horm + scatter + 1200
dados.rlm <- data.table(BMI, BMD, etnia, idade, horm=factor(horm))
levels(dados.rlm$horm) <- c("baixo", "medio", "alto")

fwrite(dados.rlm, "Aulas/Cap31-32/dados-rlm.csv")
