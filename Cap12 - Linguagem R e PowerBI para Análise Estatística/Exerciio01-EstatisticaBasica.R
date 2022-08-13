# Estatística Básica

# Exercício 1


# Em anexo você encontra um dataset contendo notas de diversos alunos em duas turmas diferentes. 
  # Usando a Linguagem R, resolva os itens abaixo:
  #   
  #   Exercício 1: Apresente um resumo de tipos de dados e estatísticasdo dataset.
  #   Exercício 2: Qual a média de cada turma?
  #   Exercício 3: Qual turma apresentou maior variabilidade de notas? Justifique sua resposta.
  #   Exercício 4 -Calcule o coeficiente de variação das 2 turmas.
  #   Exercício 5 -Qual nota apareceu mais vezes em cada turma?

#Defina a pasta de trabalho
setwd("D:/Pablo/DSA/Microsoft Power BI - DataScienceV2/Cap12 - Linguagem R e PowerBI para Análise Estatística")
getwd()

#Carregando o DataSet
notas <- read.csv("Notas.csv", stringsAsFactors=FALSE, fileEncoding="latin1")


#   Exercício 1: Apresente um resumo de tipos de dados e estatísticasdo dataset.
View(notas)
str(notas)
summary(notas$TurmaA)
summary(notas$TurmaB)


#   Exercício 2: Qual a média de cada turma?
mean(notas$TurmaA)
mean(notas$TurmaB)

#Exercício 3: Qual turma apresentou maior variabilidade de notas? Justifique sua resposta.
sd(notas$TurmaA)
sd(notas$TurmaB)
# Turma A possui Desvio Padrão de 14 enquanto a turma B possui variância de 6


#   Exercício 4 -Calcule o coeficiente de variação das 2 turmas.

MediaTurmaA <- mean(notas$TurmaA)
MediaTurmaB <- mean(notas$TurmaB)

SDTurmaA <- sd(notas$TurmaA)
SDTurmaB <- sd(notas$TurmaB)

CVA <- SDTurmaA / MediaTurmaA *100
CVB <- SDTurmaB / MediaTurmaB * 100

CVA
CVB


#   Exercício 5 -Qual nota apareceu mais vezes em cada turma?
moda <- function(v){
  valor_unico <- unique(v)
  valor_unico[which.max(tabulate(match(v,valor_unico)))]
  
}

#Obtendo a moda
resultadoTurmaA <- moda(notas$TurmaA)
print(resultadoTurmaA)

resultadoTurmaB <- moda(notas$TurmaB)
print(resultadoTurmaB)