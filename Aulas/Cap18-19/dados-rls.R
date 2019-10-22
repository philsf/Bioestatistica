library(data.table)

set.seed(1)
BMI <- c(rnorm(100, 30, 5), rnorm(100, 50, 5))
idade <- rnorm(200, 60, 5)
vitD <- round(sin(BMI))
scatter <- rnorm(200, 0, 20)
BMD <- -3*BMI - idade + 400 + scatter
dados.rls <- data.table(BMI, BMD, idade, vitD)

BMD2 <- -3*BMI -
  c(
    rnorm(50, 0, 05), # introduzir heterocedasticidade
    rnorm(50, 0, 15),
    rnorm(50, 0, 20),
    rnorm(50, 0, 35)
  ) + 300
dados.rls.het <- data.table(BMI, BMD2)

fwrite(dados.rls, "Aulas/Cap18-19/dados-rls.csv")
fwrite(dados.rls.het, "Aulas/Cap18-19/dados-rls-het.csv")
rm(BMD, BMD2, BMI, idade, scatter, vitD)
