# Estatística Básica

# Parte 1 - Medidas de Posição

#Defina a pasta de trabalho
setwd("D:/Pablo/DSA/Microsoft Power BI - DataScienceV2/Cap12 - Linguagem R e PowerBI para Análise Estatística")
getwd()

#Carregando o DataSet
vendas <- read.csv("Vendas.csv", stringsAsFactors=FALSE, fileEncoding="latin1")


#Resumo DataSset
View(vendas)
str(vendas)
summary(vendas$Valor)
summary(vendas$Custo)

#Carrega o GGPLOT2
library(ggplot2)
?qplot
qplot(Valor, Custo, data = vendas)
