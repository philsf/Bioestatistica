library(data.table)

set.seed(1)
BMI <- c(rnorm(100, 30, 5), rnorm(100, 50, 5))
idade <- rnorm(200, 60, 5)
horm <- round(sin(BMI))
scatter <- rnorm(200, 0, 20)
BMD <- -3*BMI - idade + 400 + scatter
dados <- data.table(BMI, BMD, idade, horm)

BMD2 <- -3*BMI -
  c(
    rnorm(50, 0, 10), # introduzir heterocedasticidade
    rnorm(50, 0, 20),
    rnorm(50, 0, 40),
    rnorm(50, 0, 45)
  ) + 300
dados.het <- data.table(BMI, BMD2)

fwrite(dados, "Aulas/Cap18-19/dados-rls.csv")
fwrite(dados.het, "Aulas/Cap18-19/dados-rls-het.csv")
