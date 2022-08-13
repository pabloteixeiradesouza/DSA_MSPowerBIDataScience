# Estatística Básica

# Parte 2 - Medidas de Dispersão

#Defina a pasta de trabalho
setwd("D:/Pablo/DSA/Microsoft Power BI - DataScienceV2/Cap12 - Linguagem R e PowerBI para Análise Estatística")
getwd()

#Carregando o DataSet
vendas <- read.csv("Vendas.csv", stringsAsFactors=FALSE, fileEncoding="latin1")

#Resumo DataSset
View(vendas)
str(vendas)
summary(vendas$Valor)

#Variância
var(vendas$Valor)

#Desvio Padrão
sd(vendas$Valor)
