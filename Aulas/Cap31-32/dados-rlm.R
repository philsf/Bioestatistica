library(data.table)

set.seed(1)
BMI <- c(rnorm(75, 30, 6), rnorm(50, 40, 7), rnorm(75, 50, 7))
idade <- round(rnorm(200, 60, 5))
scatter <- rnorm(200, 0, 20)
etnia <- sample(gl(2, 100, labels = c("branca", "parda")))
vitD <- round(sin(BMI*2*pi/20))
BMD <- - 2*BMI - 3*idade - 30*vitD + scatter/3 + 770
# intercepto 770 selecionado para ter casos positivos/negativos de osteoporose com BMD<500 (GLM)
dados.rlm <- data.table(BMI, BMD, etnia, idade, vitD=factor(vitD))
levels(dados.rlm$vitD) <- c("alta", "media", "baixa")
dados.rlm$osteo <- cut(dados.rlm$BMD, c(-Inf, 500, Inf), c("Osteoporose", "Sadio"))
dados.rlm$osteo <- relevel(dados.rlm$osteo, "Sadio")

# dados discretizados para GLM
dados.rlm$obeso <- cut(dados.rlm$BMI, c(-Inf, 30, Inf), right = FALSE, c("Nao Obeso", "Obeso"))
dados.rlm$idoso <- cut(dados.rlm$idade, c(-Inf, 60, Inf), right = FALSE, c("Nao Idoso", "Idoso"))

fwrite(dados.rlm, "Aulas/Cap31-32/dados-rlm.csv")
rm(BMD, BMI, etnia, idade, scatter, vitD)
