dados <- data.frame(
  Control = c(1162, 1095, 1327, 1261, 1103, 1235),
  Drug    = c(892, 903, 1164, 1002, 961, 875)
)
dados <- tidyr::gather(dados, Group, Receptors)
t.test(Receptors ~ Group, paired = TRUE, dados)
