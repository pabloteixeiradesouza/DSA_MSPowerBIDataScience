# Carregando pasta de Trabalho

setwd("D:/Pablo/DSA/Microsoft Power BI - DataScienceV2/Cap15 - Análise de Dados e Machine Leraning")
getwd()

#Instalando os pacotes
install.packages("Amelia")
install.packages("lattice", dependencies = TRUE)
install.packages("recipes", dependencies = TRUE)
install.packages("ipred", dependencies = TRUE)
install.packages("caret", dependencies = TRUE)
install.packages("recipes")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("reshape")
install.packages("randomForest")
install.packages("e1071")

#Carregando os pacotes
library(Amelia)
library(ggplot2)
library(lattice)
library(recipes)
library(ipred)
library(dplyr)
library(e1071)
library(randomForest)
library(reshape)
library(caret)



#Carregando o Dataset
dados_clientes <- read.csv("dataset.csv")

#Visualizando os dados e sua estrutura
View(dados_clientes)
dim(dados_clientes)
str(dados_clientes)
summary(dados_clientes)

#Análise Exploratória, Limpeza e Transformação

#Removendo a primeira coluna ID

dados_clientes$ID <- NULL
dim(dados_clientes)
View(dados_clientes)

#Renomeadno a coluna de classe
colnames(dados_clientes)
colnames(dados_clientes)[24] <- 'inadiplente'
colnames(dados_clientes)
View(dados_clientes)

#Verificando valores ausentes e retirando do Dataset
sapply(dados_clientes, function(x) sum(is.na(x)))
?missmap
missmap(dados_clientes, main = "Valores Missing Observados")
dados_clientes <- na.omit(dados_clientes)

#Convertendo os atributos Genero, Escolaridade, Estado Civil e Idade
#Para fatores (categorias)

colnames(dados_clientes)
colnames(dados_clientes)[2] <- 'Genero'
colnames(dados_clientes)[3] <- 'Escolaridade'
colnames(dados_clientes)[4] <- 'Estado_Civil'
colnames(dados_clientes)[5] <- 'Idade'
colnames(dados_clientes)
View(dados_clientes)


#Genero
View(dados_clientes$Genero)
str(dados_clientes$Genero)
summary(dados_clientes$Genero)
?cut
dados_clientes$Genero <- cut(dados_clientes$Genero,
                      c(0,1, 2),
                      labels = c("Masculino", "Feminino"))
View(dados_clientes$Genero)
str(dados_clientes$Genero)
View(dados_clientes)
summary(dados_clientes$Genero)

#Escolaridade
View(dados_clientes$Escolaridade)
str(dados_clientes$Escolaridade)
summary(dados_clientes$Escolaridade)
?cut
dados_clientes$Escolaridade <- cut(dados_clientes$Escolaridade,
                             c(0, 1, 2, 3, 4),
                             labels = c("Pos Graduado", "Graduado", "Ensino Medio", "Outros"))
View(dados_clientes$Escolaridade)
str(dados_clientes$Escolaridade)
View(dados_clientes)
summary(dados_clientes$Escolaridade)

#Estado Civil
View(dados_clientes$Estado_Civil)
str(dados_clientes$Estado_Civil)
summary(dados_clientes$Estado_Civil)
?cut
dados_clientes$Estado_Civil <- cut(dados_clientes$Estado_Civil,
                                   c(-1, 0, 1, 2, 3),
                                   labels = c("Desconhecido", "Casado", "Solteiro", "Outro"))
View(dados_clientes$Estado_Civil)
str(dados_clientes$Estado_Civil)
View(dados_clientes)
summary(dados_clientes$Estado_Civil)

#Convertento a variável Idade em Faixa Etária

str(dados_clientes$Idade)
summary(dados_clientes$Idade)
hist(dados_clientes$Idade)
?cut
dados_clientes$Idade <- cut(dados_clientes$Idade,
                                   c(0, 30, 50, 100),
                                   labels = c("Jovem", "Adulto", "Idoso"))


View(dados_clientes)
str(dados_clientes$Idade)
summary(dados_clientes$Idade)


#Convertendo as variáveis que indicam pagamento para o tipo FATOR
dados_clientes$PAY_0 <- as.factor(dados_clientes$PAY_0)
dados_clientes$PAY_2 <- as.factor(dados_clientes$PAY_2)
dados_clientes$PAY_3 <- as.factor(dados_clientes$PAY_3)
dados_clientes$PAY_4 <- as.factor(dados_clientes$PAY_4)
dados_clientes$PAY_5 <- as.factor(dados_clientes$PAY_5)
dados_clientes$PAY_6 <- as.factor(dados_clientes$PAY_6)

#DataSet pós as Conversões
str(dados_clientes)
sapply(dados_clientes, function(x) sum(is.na(x)))
missmap(dados_clientes, main = "Valores Missing Observados")
dados_clientes <- na.omit(dados_clientes)
missmap(dados_clientes, main = "Valores Missing Observados")
dim(dados_clientes)
View(dados_clientes)


#Alterando a variável independente para o tipo FATOR
str(dados_clientes$inadiplente)
colnames(dados_clientes)
dados_clientes$inadiplente <- as.factor(dados_clientes$inadiplente)
str(dados_clientes$inadiplente)
View(dados_clientes)

#Total Indimplentes X Total Não Inadimplentes
table(dados_clientes$inadiplente)

#Vejamos os percentuais entre as classes
prop.table(table(dados_clientes$inadiplente))

#Plot da Distribuição usando Ggplot2
qplot(inadiplente, data = dados_clientes, geom = "bar") +
  theme(axis.text = element_text(angle = 90, hjust = 1))

set.seed(12345)

#Amostragem estratificada 
#Seleciona as linhas de acordo com a variavel inadimplente como Strata

?createDataPartition
indice <- createDataPartition(dados_clientes$inadiplente, p = 0.75, list = FALSE)
dim(indice)

#Definimos os dados de treinamento como subconjunto do conjunto de dados original 
#com número de índice de linha (conforme indetificado acima) e todas as colunas

dados_treino <- dados_clientes[indice,]
dim(dados_treino)
dim(dados_clientes)
table(dados_treino$inadiplente)

#Vejamos os percentuais entre as classes
prop.table(table(dados_treino$inadiplente))

#Numero de registros do DataSet de Treinamento
dim(dados_treino)

#Comparamos as porcentagens entre as classes de treinamento e dados originais
compara_dados <- cbind(prop.table(table(dados_treino$inadiplente)),
                       prop.table(table(dados_clientes$inadiplente)))
colnames(compara_dados) <- c("Treinamento", "Original")
compara_dados

#Melt Data - Converte colunas em linhas
?reshape::melt
melt_compara_dados <- melt(compara_dados)
melt_compara_dados

#Plot para ver a distribuição do Treinamento X Original
ggplot(melt_compara_dados, aes(x = X1, y = value)) +
  geom_bar(aes(fill = X2), stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Tudo que não está no Dataset de Treinamento está no Dataset de Teste. Observe o sinal - (menos)
dados_teste <- dados_clientes[-indice,]
dim(dados_teste)
dim(dados_treino)


########### MODELO MACHINE LEARNING ###################
?randomForest
modelo_v1 <- randomForest(inadiplente ~ ., data = dados_treino)
modelo_v1

#Avaliando o modelo
plot(modelo_v1)


#Previsões com dados de teste
previsoes_v1 <- predict(modelo_v1, dados_teste)


#Confusion Matrix
?caret::confusionMatrix
cm_v1 <- caret::confusionMatrix(previsoes_v1, dados_teste$inadiplente, positive = "1")
cm_v1


#Calculando Precision, Recall e F1-Score, métricas de avaliação do modelo preditivo
y <-dados_teste$inadiplente
y_pred_v1 <- previsoes_v1

precision <- posPredValue(y_pred_v1, y)
precision

recall <- sensitivity(y_pred_v1, y)
recall

F1 <- (2 * precision * recall) / (precision + recall)
F1


#Balanceamento de classe

?ROSE

#Aplicando o ROSE (Generation of synthetic data by Randomly Over Sampling Examples)
table(dados_treino$inadiplente)
prop.table(table(dados_treino$inadiplente))
set.seed(9560)
dados_treino_bal <- ovun.sample(inadiplente ~ . , data = data_treino)

table(dados_treino_bal$inadiplente)
prop.table(table(dados_treino_bal$inadiplente))

#Constuindo a segunda versão do modelo
modelo_v2 <- randomForest(inadiplente ~ ., data = dados_treino_bal)

plot(modelo_v1)
plot(modelo_v2)

modelo_v1
modelo_v2

#Previsões com dados de teste
previsoes_v2 <- predict(modelo_v2, dados_teste)

#Confusion Matrix
?caret::confusionMatrix
cm_v2 <- caret::confusionMatrix(previsoes_v2, dados_teste$inadiplente, positive = "1")
cm_v2
cm_v1

#Calculando Precision, Recall e F1-Score, métricas de avaliação do modelo preditivo
y <-dados_teste$inadiplente
y_pred_v2 <- previsoes_v2

precision <- posPredValue(y_pred_v2, y)
precision

recall <- sensitivity(y_pred_v2, y)
recall

F1 <- (2 * precision * recall) / (precision + recall)
F1

#Importância das variáveis preditoras para as previsões
View(dados_treino_bal)
varImpPlot(modelo_v2)

#Obtendo as variáveis mais importantes
imp_var <- importance(modelo_v2)
varImportance <- data.frame(Variables = row.names(imp_var),
                            Importance = round(imp_var[ ,'MeanDecreaseGini'],2))

#Criando um Rank de variáveis baseado na importância
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#', dense_rank(desc(Importance))))

#Usando GGplot2 para visualizar a importância relativa das variáveis
ggplot(rankImportance,
       aes(x = reorder(Variables, Importance),
           y = Importance,
           fill = Importance)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust = 0,
            vjust = 0.55,
            size = 4,
            colour = 'red') +
  labs(x = 'Variables') + 
  coord_flip()


#Construindo a terceira versão do modelo apenas com as variáveis mais importantes

colnames(dados_treino_bal)
modelo_v3 <- randomForest(inadiplente ~ PAY_0 + PAY_2 + PAY_3 + PAY_AMT1 + PAY_AMT2 + PAY_5 + BILL_AMT1,
                          data = dados_treino_bal)
modelo_v3

plot(modelo_v1)
plot(modelo_v2)
plot(modelo_v3)

modelo_v1
modelo_v2
modelo_v3

#Previsões com dados de teste
previsoes_v3 <- predict(modelo_v3, dados_teste)

#Confusion Matrix
?caret::confusionMatrix
cm_v3 <- caret::confusionMatrix(previsoes_v3, dados_teste$inadiplente, positive = "1")
cm_v3
cm_v2
cm_v1

#Calculando Precision, Recall e F1-Score, métricas de avaliação do modelo preditivo
y <-dados_teste$inadiplente
y_pred_v3 <- previsoes_v3

precision <- posPredValue(y_pred_v3, y)
precision

recall <- sensitivity(y_pred_v3, y)
recall

F1 <- (2 * precision * recall) / (precision + recall)
F1

# Salvando o modelo em disco
saveRDS(modelo_v3,file = "modelo_v3.rds")

#carregando o modelo
modelo_final <- readRDS("modelo_v3.rds")

#Previsões com novos dados de 3 clientes
PAY_0 <- c(0,0,0)
PAY_2 <- c(0,0,0)
PAY_3 <- c(1,0,0)
PAY_AMT1 <- c(1100,1000,1200)
PAY_AMT2 <- c(1500,1300,1150)
PAY_5 <- c(0,0,0)
BILL_AMT1 <- c(350,420,280)

#Concatena em um dataframe
novos_clientes <- data.frame(PAY_0, PAY_2, PAY_3, PAY_AMT1, PAY_AMT2, PAY_5, BILL_AMT1)
View(novos_clientes)

#Convertendo as variáveis que indicam pagamento para o tipo FATOR
str(novos_clientes)
str(dados_treino_bal)
novos_clientes$PAY_0 <- factor(novos_clientes$PAY_0, levels = levels(dados_treino_bal$PAY_0))
novos_clientes$PAY_2 <- factor(novos_clientes$PAY_2, levels = levels(dados_treino_bal$PAY_2))
novos_clientes$PAY_3 <- factor(novos_clientes$PAY_3, levels = levels(dados_treino_bal$PAY_3))
novos_clientes$PAY_5 <- factor(novos_clientes$PAY_5, levels = levels(dados_treino_bal$PAY_5))
novos_clientes$PAY_AMT1 <- as.integer(novos_clientes$PAY_AMT1)
novos_clientes$PAY_AMT2 <- as.integer(novos_clientes$PAY_AMT2)
novos_clientes$BILL_AMT1 <- as.integer(novos_clientes$BILL_AMT1)


#Previsões
previsoes_novos_clientes <- predict(modelo_final, novos_clientes)
View(previsoes_novos_clientes)


