
# criar um novo projeto para nao utilizar o caminho completo
dados <- read.csv("CompanhiaMB.csv")

View(dados)

summary(dados)

table(dados$estado_civil)
