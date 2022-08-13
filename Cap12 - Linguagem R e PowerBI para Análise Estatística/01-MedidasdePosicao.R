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


#Média
?mean
mean(vendas$Valor)

#Média Ponderada
?weighted.mean
weighted.mean(vendas$Valor, w = vendas$Custo)

#Mediana
?median
median(vendas$Valor)
median(vendas$Custo)

#Moda
#Cirando uma função

moda <- function(v){
  valor_unico <- unique(v)
  valor_unico[which.max(tabulate(match(v,valor_unico)))]
  
}

#Obtendo a moda
resultado <- moda(vendas$Valor)
print(resultado)

resultado_custo <- moda(vendas$Custo)
print(resultado_custo)

#Criando gráfico de média de valor por estado com GGPLOT2
install.packages("ggplot2")
library(ggplot2)

#Cria o gráfico
ggplot(vendas) +
  stat_summary(aes(x = Estado,
                   y = Valor),
                fun = mean,
               geom = "bar",
               fill = "lightgreen",
               col = "grey50") +
  labs(title = "Média de Valor por Estado")


