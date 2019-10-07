library(data.table)

set.seed(1)
BMI <- c(rnorm(75, 30, 6), rnorm(50, 40, 7), rnorm(75, 50, 7))
idade <- round(rnorm(200, 60, 5))
scatter <- rnorm(200, 0, 20)
etnia <- sample(gl(2, 100, labels = c("branca", "parda")))
horm <- round(sin(BMI*2*pi/15))
BMD <- - 2*BMI - 3*idade - 15*horm + scatter/3 + 790
dados.rlm <- data.table(BMI, BMD, etnia, idade, horm=factor(horm))
levels(dados.rlm$horm) <- c("baixo", "medio", "alto")
dados.rlm$osteo <- cut(dados.rlm$BMD, c(-Inf, 500, Inf), c("Osteoporose", "Sadio"))
dados.rlm$osteo <- relevel(dados.rlm$osteo, "Sadio")

fwrite(dados.rlm, "Aulas/Cap31-32/dados-rlm.csv")
