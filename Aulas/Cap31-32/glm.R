library(philsfmisc)

# dados simulados ---------------------------------------------------------

dados.rlm <- fread("Aulas/Cap31-32/dados-rlm.csv", stringsAsFactors = TRUE)
dados.rlm$horm <- factor(dados.rlm$horm, levels = c("baixo", "medio", "alto"))
dados.rlm$osteo <- relevel(dados.rlm$osteo, "Sadio")
summary(glm(osteo ~ BMI + idade + horm, binomial, dados.rlm))

# modelos -----------------------------------------------------------------

# graficos ----------------------------------------------------------------
