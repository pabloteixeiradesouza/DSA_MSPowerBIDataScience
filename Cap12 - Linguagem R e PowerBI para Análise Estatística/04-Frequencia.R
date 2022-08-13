# Estatística Básica

# Parte 4 - Frequência

#Defina a pasta de trabalho
setwd("D:/Pablo/DSA/Microsoft Power BI - DataScienceV2/Cap12 - Linguagem R e PowerBI para Análise Estatística")
getwd()

#Carregando o DataSet
dados <- read.table("Usuarios.csv",
                    dec = ".",
                    sep = ",",
                    h = T,
                    fileEncoding = "windows-1252")


#Resumo DataSset
View(dados)
names(dados)
str(dados)
summary(dados$salario)  
summary(dados$grau_instrucao)  
mean(dados$salario)
mean(dados$grau_instrucao)


#Tabela de Frequencias Absolutas  
freq <- table(dados$grau_instrucao)
View(freq)


#Tabela de Frequencias Relativas
freq_rel <- prop.table(freq)
View(freq_rel)

#Porcentagem (100 * freq_rl)
p_freq_rel <- 100 * prop.table(freq)
View(p_freq_rel)

#Adiciona Linhas de Total
freq <- c(freq,sum(freq))
View(freq)
names(freq)[4] <- "Total"
View(freq)


#Tabela Final com todos os valores

#Calculamos frequencia relativa e frequencia proporcional
freq_rel <- c(freq_rel,sum(freq_rel))
View(freq_rel)
p_freq_rel <- c(p_freq_rel,sum(p_freq_rel))
View(p_freq_rel)

#Tabela fianl cm todos os vetores
tabela_final <- cbind(freq,
                      freq_rel = round(freq_rel ,digits = 2),
                      p_freq_rel = round(p_freq_rel, digits = 2))

View(tabela_final)