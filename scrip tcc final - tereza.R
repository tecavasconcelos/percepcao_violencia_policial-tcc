#---------------------------------------#
#  tcc - como a violência policial pode #
#  afetar a na participação política?   #
#                                       #
#  Pesquisadora: Tereza Vasconcelos     #
#---------------------------------------#


# importacao dos dados #       
# banco de dados latino barometro 2015 #

load('C:/Users/HP/Desktop/Aulas R/Latinobarometro_2015_Eng.rdata')

#retorna a estrutura do banco
str(Latinobarometro_2015_Eng)

#retorna os nomes das colunas no banco
colnames(Latinobarometro_2015_Eng)

#selecionar segunda coluna do banco
Latinobarometro_2015_Eng[2,] 
head(Latinobarometro_2015_Eng)


#selecionar colunas no banco de dados por nome
database<-Latinobarometro_2015_Eng[,c('idenpa','P21TGB.A','P21ST.B','P21ST.C','P21N.D','P32N','P20TGB.A','P63NJ.G','S23','S12','S6','S18','S13')]


#renomear colunas
colnames(database)<-c('paises','fazer_peticao','assistir_protestos','participacao_protestos','meioscomunicacao','engajamento_politico','conversar_politica','percepcao_violencia_policial','raca','genero','classe_social','escolaridade','idade')

View(database)


#contabilizar a quantidade de casos faltantes
is.na(database)
sum(is.na(database))
dim(database)


library(Amelia)
library('dplyr')
library (ggplot2)
library(stargazer)
library("nnet")
library(ordinal)

missmap(database)

# porcentagem de casos faltantes do banco de dados #
sum(is.na(database))/dim(database)[1]  

# comando para tirar os casos faltantes do banco de dados #
# selecao de todos os casos que contem informacao #
databaseClean <- database[complete.cases(database), ]


#1construir uma data.frame da variavel paises

paisesData <-data.frame(table(databaseClean$paises))
paisesData
mutate(paisesData, porcentagem = Freq/sum(Freq))* 100

paisesData$Nomes <-''
paisesData$Nomes <- factor(paisesData$Var1,levels = c('32','68','76','152','170','188','214','218','222','320','340','484','558','591','600','604','858','862'),
                           labels = c('argentina','bolivia','brazil','chile', 'colombia','costa_rica','rep_dominicana','equador','el_salvador','guatemala','honduras','mexico','nicaragua','panama','paraguay','peru','uruguay','venezuela'))


#seleção do caso Brasileiro


databaseClean$paises <- as.factor(databaseClean$paises)

# Selecionar casos com base no valor de uma variavel
databaseClean <- databaseClean[databaseClean$paises == 76, ]


#1construir uma data.frame da variavel percepcao_da_violencia_policial

violencia_policialData <-data.frame(table(databaseClean$percepcao_violencia_policial))
violencia_policialData
mutate(violencia_policialData, porcentagem = (Freq/sum(Freq)) * 100)


violencia_policialData$Nomes <- ''
violencia_policialData$Nomes <- factor(violencia_policialData$Var1,levels = c('1','2','3','4','5'),
                                       labels = c('quase todos os dias','uma/duas vezes por semana','poucas vezes por mes','poucas vezes por ano', 'nao ocorre'))


# 2construir data.frame com a variavel participacao_protestos

participacaoData <- data.frame(table(databaseClean$participacao_protestos))
participacaoData
mutate(participacaoData, porcentagem = (Freq/sum(Freq)) * 100)

summary(databaseClean$participacao_protestos)

participacaoData$Nomes <- ''
participacaoData$Nomes <- factor(participacaoData$Var1,levels = c('1','2','3'),
                                 labels = c('talvez participaria','participaria','nunca participaria'))

# 3construir data.frame com a variavel formas de engajamento politico

table(databaseClean$engajamento_politico)

engajamento_politicoData <- data.frame(table(databaseClean$engajamento_politico))

engajamenmto_politicoData
mutate(engajamento_politicoData, porcentagem = (Freq/sum(Freq)) * 100)

summary(databaseClean$engajamento_politico)

engajamento_politicoData$Nomes <-''
engajamento_politicoData$Nomes <- factor(engajamento_politicoData$Var1,levels = c('1','2','3','4','5'),
                                         labels = c('votar sempre','votar e também protestar','so protestar e nao serve votar','nao votar e nem protestar','nao sei'))


Database_Recod <- databaseClean[databaseClean$engajamento_politico != 5,]
# 4construir data.frame com a variavel de raca

table(databaseClean$raca)

racaData <-data.frame(table(databaseClean$raca))
racaData
mutate(racaData, porcentagem = (Freq/sum(Freq)) *100)

summary(databaseClean$raca)

racaData$Names <- ''
racaData$Names <- factor(racaData$Var1,levels = c('1','2','3','4','5','6','7'),
                         labels = c('asiatico','negro','indegena','mestico','mulato','branco','outra'))

# 5construir data.frame com a variavel de genero

table(databaseClean$genero)

generoData <-data.frame(table(databaseClean$genero))
generoData
mutate(generoData, porcentagem = (Freq/sum(Freq)) *100)

summary(databaseClean$genero)

generoData$Names <- ''
generoData$Names <- factor(generoData$Var1, levels = c('1','2'),
                           labels = c('masculino','feminino'))


#6 construir data.frame com a variavel de classe social

table(databaseClean$classe_social)

classesocialData <-data.frame(table(databaseClean$classe_social))
classesocialData
mutate(classesocialData, porcentagem = (Freq/sum(Freq)) *100)

summary(databaseClean$classe_social)

classesocialData$Names <- ''
classesocialData$Names <- factor(classesocialData$Var1, levels = c('1','2','3','4','5'),
                                 labels = c('alta','media alta','media','media baixa','baixa'))

#8 construir data.frame com a variavel escolaridade

table(databaseClean$escolaridade)

escolaridadeData <-data.frame(table(databaseClean$escolaridade))
escolaridadeData
mutate(escolaridadeData, porcentagem = (Freq/sum(Freq)) *100)

summary(databaseClean$escolaridade)

#9 construir data.frame com a variavel fazer_peticoes

table(databaseClean$fazer_peticao)

fazer_peticaoData <-data.frame(table(databaseClean$fazer_peticao))
fazer_peticaoData
mutate(fazer_peticaoData, porcentagem = (Freq/sum(Freq)) *100)

summary(databaseClean$fazer_peticao)

fazer_peticaoData$Names <- ''
fazer_peticaoData$Names <- factor(fazer_peticaoData$Var1, levels = c('1','2','3'),
                                  labels = c('ja fez','poderia fazer','nunca faria'))

#10 construir data.frame com a variavel assistir_protestos

table(databaseClean$assistir_protestos)

assistir_protestosData <-data.frame(table(databaseClean$assistir_protestos))
assistir_protestosData
mutate(assistir_protestosData, porcentagem = (Freq/sum(Freq)) *100)

summary(databaseClean$assistir_protestos)

assistir_protestosData$Names <- ''
assistir_protestosData$Names <- factor(assistir_protestosData$Var1, levels = c('1','2','3'),
                                       labels = c('ja fez','poderia fazer','nunca faria'))

#11 construir data.frame com a variavel usar os meios comunicacao para fazer uma denuncia

table(databaseClean$meioscomunicacao)

meioscomunicacaoData <-data.frame(table(databaseClean$meioscomunicacao))
meioscomunicacaoData
mutate(meioscomunicacaoData, porcentagem = (Freq/sum(Freq)) *100)

summary(databaseClean$meioscomunicacao)

meioscomunicacaoData$Names <- ''
meioscomunicacaoData$Names <- factor(meioscomunicacaoData$Var1, levels = c('1','2','3'),
                                     labels = c('ja fez','poderia fazer','nunca faria'))


#12 construir data.frame com a variavel conversar politica

table(databaseClean$conversar_politica)


conversar_politicaData <-data.frame(table(databaseClean$conversar_politica))
conversar_politicaData
mutate(conversar_politicaData, porcentagem = (Freq/sum(Freq)) *100)

summary(databaseClean$conversar_politica)

conversar_politicaData$Names <- ''
conversar_politicaData$Names <- factor(conversar_politicaData$Var1, levels = c('1','2','3','4'),
                                       labels = c('muita frequencia','frequentemente','quase nunca','nunca'))


#12 construir data.frame com a variavel idade

table(databaseClean$idade)

idadeData <-data.frame(table(databaseClean$idade))
idadeData
mutate(idadeData, porcentagem = (Freq/sum(Freq))* 100)

summary(databaseClean$idade)


## Recodificar VD
databaseClean <- mutate(databaseClean, participacao_protestosParticipou = ifelse(databaseClean$participacao_protestos == 
                                                                                   1, 1, 0))

# Recodificar Violencia policial VI
databaseClean <- mutate(databaseClean, violenciaPolicialOrd = 0)
databaseClean$violenciaPolicialOrd[databaseClean$percepcao_violencia_policial == 5] <- 0
databaseClean$violenciaPolicialOrd[databaseClean$percepcao_violencia_policial == 4] <- 1
databaseClean$violenciaPolicialOrd[databaseClean$percepcao_violencia_policial == 3] <- 2
databaseClean$violenciaPolicialOrd[databaseClean$percepcao_violencia_policial == 2] <- 3
databaseClean$violenciaPolicialOrd[databaseClean$percepcao_violencia_policial == 1] <- 4

table(databaseClean$percepcao_violencia_policial)

violencia_policialData <- data.frame(table(databaseClean$percepcao_violencia_policial))
violencia_policialData <- mutate(violencia_policialData, porcentagem = (Freq/sum(Freq)) * 100)
violencia_policialData$porcentagem <- round(violencia_policialData$porcentagem, 2)

databaseClean <- mutate(databaseClean, percepcao_violencia_policialTodosDias = ifelse(databaseClean$percepcao_violencia_policial == 
                                                                                        1, 1, 0))





# criar fator com rotulos
violencia_policialData$Nomes <- ''
violencia_policialData$Nomes <- factor(violencia_policialData$Var1, levels = c('1', '2', '3', '4', '5'),
                                       labels = c('quase todos os dias','uma/duas vezes por semana','poucas vezes por mes','poucas vezes por ano', 'nao ocorre'))



# Recodificar VIs categoricas - controle
databaseClean <- mutate(databaseClean, classeBaixa = ifelse(classe_social == 
                                                              5, 1, 0))
databaseClean <- mutate(databaseClean, generoMulher = ifelse(genero == 2, 1, 
                                                             0))
databaseClean <- mutate(databaseClean, racaNegroMulato = ifelse(raca == 2 | 
                                                                  raca == 5, 1, 0))

# Recodificar formas de engajamento político
Database_Recod <- mutate (Database_Recod, engajamento_politico_Recod = 0)
Database_Recod$engajamento_politico_Recod[Database_Recod$engajamento_politico == 4] <- 1
Database_Recod$engajamento_politico_Recod[Database_Recod$engajamento_politico == 3] <- 2
Database_Recod$engajamento_politico_Recod[Database_Recod$engajamento_politico == 2] <- 3
Database_Recod$engajamento_politico_Recod[Database_Recod$engajamento_politico == 1] <- 2



#----------------------------# 
# INDICE PART. POLITICA

Database_Recod <- mutate(Database_Recod, indice_participacao_politica =
                           Database_Recod$participacao_protestos + Database_Recod$assistir_protestos +
                           Database_Recod$fazer_peticao + Database_Recod$meioscomunicacao + Database_Recod$conversar_politica)

summary(Database_Recod)


#executar modelo de regressao com efeito dos controles

# executar modelo 1 - apenas variaveis de controle


modelo1 <- lm(indice_participacao_politica ~
                 escolaridade +
                 classeBaixa +
                 generoMulher +
                 idade +
                 racaNegroMulato,
               data = Database_Recod)


summary(modelo1)

stargazer(modelo1, type = "text", title = "Resultados do Modelo 1", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)

# executar modelo 2 - só variável independente


modelo2 <- lm( indice_participacao_politica ~
                 percepcao_violencia_policialTodosDias,
               data = Database_Recod)


summary(modelo2)

stargazer(modelo2, type = "text", title = "Resultados do Modelo 2", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)

#executar modelo 3 - variavel independente + variaveis de controle


modelo3 <- lm(indice_participacao_politica ~
                 percepcao_violencia_policialTodosDias +
                 escolaridade +
                 classeBaixa +
                 generoMulher +
                 idade +
                 racaNegroMulato,
               data = Database_Recod)


summary(modelo3)

stargazer(modelo3, type = "text", title = "Resultados do Modelo 3", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)


#executar modelo 4 - variavel independente + variaveis de controle + moderadora

Database_Recod <- mutate(Database_Recod, percepcao_violenciaTodosDias_Esc = percepcao_violencia_policialTodosDias*escolaridade)

modelo4 <- lm(indice_participacao_politica ~
                percepcao_violenciaTodosDias_Esc +
                escolaridade +
                classeBaixa +
                generoMulher +
                idade +
                racaNegroMulato,
              data = Database_Recod)




summary(modelo4)

stargazer(modelo4, type = "text", title = "Resultados do Modelo 4", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)



#-----------------------------------#
# modelo de engajamento politico
# recodificado              





# executar modelo 1 - apenas variaveis de controle


modelo5 <- multinom(engajamento_politico_Recod ~
                escolaridade +
                classeBaixa +
                generoMulher +
                idade +
                racaNegroMulato,
              data = Database_Recod)


summary(modelo5)
stargazer(modelo5, type = "text", title = "Resultados do Modelo 5", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)

# executar modelo 6 - só variável independente


modelo6 <- multinom(engajamento_politico_Recod ~
                 percepcao_violencia_policialTodosDias,
               data = Database_Recod)


summary(modelo6)

stargazer(modelo6, type = "text", title = "Resultados do Modelo 6", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)


#executar modelo 3 - variavel independente + variaveis de controle


modelo7 <- multinom (engajamento_politico_Recod ~
                percepcao_violencia_policialTodosDias +
                escolaridade +
                classeBaixa +
                generoMulher +
                idade +
                racaNegroMulato,
              data = Database_Recod)


summary(modelo7)

stargazer(modelo7, type = "text", title = "Resultados do Modelo 7", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)


#executar modelo 8 - variavel independente + variaveis de controle + moderadora


modelo8 <- multinom(engajamento_politico_Recod ~
                percepcao_violenciaTodosDias_Esc +
                  percepcao_violencia_policialTodosDias +
                escolaridade +
                  classeBaixa +
                  generoMulher +
                  idade +
                  racaNegroMulato,
              data = Database_Recod)




summary(modelo8)

stargazer(modelo8, type = "text", title = "Resultados do Modelo 8", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)




