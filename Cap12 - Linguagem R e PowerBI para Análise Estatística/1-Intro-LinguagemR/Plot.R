# Definindo a pasta de trabalho
# Substitua o caminho abaixo pela pasta no seu computador
setwd("C:/Users/dmpm/Documents/PowerBI/Cap12")
getwd()

# Carregando o dataset
vendas <- read.csv("Vendas.csv", fileEncoding = "windows-1252")

# Carrega o ggplot2
library(ggplot2)

# Cria o gráfico
?qplot
qplot(Valor, Custo, data = vendas)
