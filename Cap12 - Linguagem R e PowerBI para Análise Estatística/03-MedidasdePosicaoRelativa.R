# Estatística Básica

# Parte 3 - Medidas de Posição Relativa

#Defina a pasta de trabalho
setwd("D:/Pablo/DSA/Microsoft Power BI - DataScienceV2/Cap12 - Linguagem R e PowerBI para Análise Estatística")
getwd()

#Carregando o DataSet
vendas <- read.csv("Vendas.csv", stringsAsFactors=FALSE, fileEncoding="latin1")

#Resumo DataSset
View(vendas)
str(vendas)
summary(vendas$Valor)

#Resumo dos dados
head(vendas)
tail(vendas)
View(vendas)

#Medidas de Tendência Central
summary(vendas$Valor)
summary(vendas[c('Valor','Custo')])

#Explorando variáveis numéricas
mean(vendas$Valor)
median(vendas$Valor)
quantile(vendas$Valor)
quantile(vendas$Valor, probs = c(0.01, 0.099))  #Percentis
quantile(vendas$Valor, seq(from = 0, to = 1, by = 0.20)) #Percentis agrupados
IQR(vendas$Valor) #Diferença entre Q3 e Q1
range(vendas$Valor)
summary(vendas$Valor)
diff(range(vendas$Valor))
