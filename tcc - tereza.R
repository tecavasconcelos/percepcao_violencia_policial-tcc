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

# selecionar o caso do brasil
# segunda coluna IDENPA=76 BRAZIL
#retorna os nomes das colunas no banco
colnames(Latinobarometro_2015_Eng)


Latinobarometro_2015_Eng[2,]  #selecionar segunda coluna do banco
head(Latinobarometro_2015_Eng)

#selecionar colunas no banco de dados por nome
database<-Latinobarometro_2015_Eng[,c('idenpa','P21TGB.A','P21ST.B','P21ST.C','P21N.D','P32N','P20TGB.A','P63NJ.G','S23','S12','S6','S24.K','S18','S13')]


#renomear colunas
colnames(database)<-c('paises','fazer_peticao','assistir_protestos','participacao_protestos','meioscomunicacao','engajamento_politico','conversar_politica','percepcao_violencia_policial','raca','genero','classe_social','esgotamento','escolaridade','idade')

View(database)

#contabilizar a quantidade de casos faltantes
is.na(database)
sum(is.na(database))
dim(database)

# porcentagem de casos faltantes do banco de dados #
sum(is.na(database))/dim(database)[1]  

# comando para tirar os casos faltantes do banco de dados #
# selecao de todos os casos que contem informacao #
databaseClean <- database[complete.cases(database), ]



library(Amelia)
library('dplyr')
library (ggplot2)
library(stargazer)
library("nnet")
library(ordinal)

missmap(database)



#-----------------------------------------------#
# frequencia de contagem com todas as variaveis #
# selecionadas do latinobarometro               #
#-----------------------------------------------#


table(databaseClean$paises)
table(databaseClean$fazer_peticao) 
table(databaseClean$assistir_protestos)
table(databaseClean$participacao_protestos)
table(databaseClean$meioscomunicacao)
table(databaseClean$engajamento_politico)
table(databaseClean$conversar_politica)
table(databaseClean$engajamento_politico)
table(databaseClean$percepcao_violencia_policial)
table(databaseClean$raca)
table(databaseClean$genero)
table(databaseClean$classe_social)
table(databaseClean$esgotamento)
table(databaseClean$escolaridade)
table(databaseClean$idade)

library('dplyr')
library (ggplot2)

#1construir uma data.frame da variavel paises

paisesData <-data.frame(table(databaseClean$paises))
paisesData
mutate(paisesData, porcentagem = Freq/sum(Freq))* 100

paisesData$Nomes <-''
paisesData$Nomes <- factor(paisesData$Var1,levels = c('32','68','76','152','170','188','214','218','222','320','340','484','558','591','600','604','858','862'),
                                       labels = c('argentina','bolivia','brazil','chile', 'colombia','costa_rica','rep_dominicana','equador','el_salvador','guatemala','honduras','mexico','nicaragua','panama','paraguay','peru','uruguay','venezuela'))



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



# 3construir data.frame com a variavel de interesse politico

table(databaseClean$engajamento_politico)

engajamento_politicoData <- data.frame(table(databaseClean$engajamento_politico))

engajamenmto_politicoData
mutate(engajamento_politicoData, porcentagem = (Freq/sum(Freq)) * 100)

summary(databaseClean$engajamento_politico)

engajamento_politicoData$Nomes <-''
engajamento_politicoData$Nomes <- factor(engajamento_politicoData$Var1,levels = c('1','2','3','4','5'),
                                      labels = c('votar sempre','votar e também protestar','so protestar e nao serve votar','nao votar e nem protestar','nao sei'))


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

# 7construir data.frame com a variavel de esgotamento

table(databaseClean$esgotamento)

saneamentoData <-data.frame(table(databaseClean$esgotamento))
saneamentoData
mutate(saneamentoData, porcentagem = (Freq/sum(Freq)) *100)

summary(databaseClean$esgotamento)

saneamentoData$Names <- ''
saneamentoData$Names <- factor(saneamentoData$Var1, levels = c('1','2'),
                               labels = c('sim','nao'))


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


#11 construir data.frame com a variavel meioscomunicacao

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




#seleção do caso Brasileiro


databaseClean$paises <- as.factor(databaseClean$paises)

# Selecionar casos com base no valor de uma variavel
databaseClean <- databaseClean[databaseClean$paises == 76, ]




## Recodificar VD
databaseClean <- mutate(databaseClean, participacao_protestosParticipou = ifelse(databaseClean$participacao_protestos == 
                                                                                   1, 1, 0))

# Recodificar Violencia policial
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



# Recodificar VIs categoricas
databaseClean <- mutate(databaseClean, esgotamentoSim = ifelse(esgotamento == 
                                                                 1, 1, 0))
databaseClean <- mutate(databaseClean, classeBaixa = ifelse(classe_social == 
                                                              5, 1, 0))
databaseClean <- mutate(databaseClean, generoMulher = ifelse(genero == 2, 1, 
                                                             0))
databaseClean <- mutate(databaseClean, racaNegroMulato = ifelse(raca == 2 | 
                                                                         raca == 5, 1, 0))

table(databaseClean$esgotamentoSim)
# executar modelo 1 PARTICIPACAO DE PROTESTOS

modelo1 <- glm(participacao_protestosParticipou ~
                 percepcao_violencia_policialTodosDias +
                 esgotamentoSim + 
                 escolaridade +
                 classeBaixa +
                 generoMulher +
                 idade +
                 racaNegroMulato,
               data = databaseClean, 
               family = "binomial")


# visualizar resultados
summary(modelo1)

stargazer(modelo1, type = "text", title = "Resultados do Modelo", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)


# executar modelo participação de protestos sem var. de controle
modelo1.1 <- glm(participacao_protestosParticipou ~
                 percepcao_violencia_policialTodosDias, 
               data = databaseClean, 
               family = "binomial")

#vizualização do modelo 1.1
summary(modelo1.1)
stargazer(modelo1.1, type = "text", title = "Resultados do Modelo", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)



## Recodificar VD FAZER_PETICAO
databaseClean <- mutate(databaseClean,fazer_peticaoParticipou = ifelse(databaseClean$fazer_peticao ==
                                                                         1,1,0))

# executar modelo
modelo2 <-glm(fazer_peticaoParticipou ~
                percepcao_violencia_policialTodosDias +
                esgotamentoSim +
                escolaridade +
                classeBaixa +
                generoMulher +
                racaNegroMulato +
                idade,
              data = databaseClean,
              family = 'binomial')

# visualizar resultados
summary(modelo2)


# visualizar em log odds (razao de chance)
stargazer(modelo2,
          type = "text", title = "Resultados do Modelo", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)

# executar modelo 2.2 fazer peticao sem var. de controle 
modelo2.2 <- glm(fazer_peticaoParticipou ~
                   percepcao_violencia_policialTodosDias, 
                 data = databaseClean, 
                 family = "binomial")

#vizualização do modelo 2.2
summary(modelo2.2)
stargazer(modelo2.2, type = "text", title = "Resultados do Modelo", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)



## Recodificar VD ASSISTIR_PROTESTOS

databaseClean <- mutate(databaseClean,assistir_protestosParticipou
                        = ifelse(databaseClean$assistir_protestos ==
                                   1,1,0))
# executar modelo
modelo3 <-glm(assistir_protestosParticipou ~
                percepcao_violencia_policialTodosDias +
                esgotamentoSim +
                escolaridade +
                classeBaixa +
                generoMulher +
                racaNegroMulato +
                idade,
              data = databaseClean,
              family = 'binomial')

# visualizar resultados
summary(modelo3)

# visualizar em log odds (razao de chance)
stargazer(modelo3,
          type = "text", title = "Resultados do Modelo", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)


# executar modelo 3.3 assistir protestos sem var. de controle 
modelo3.3 <- glm(assistir_protestosParticipou ~
                   percepcao_violencia_policialTodosDias, 
                 data = databaseClean, 
                 family = "binomial")

#vizualização do modelo 3.3
summary(modelo3.3)
stargazer(modelo3.3, type = "text", title = "Resultados do Modelo", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)


## Recodificar VD MEIOSCOMUNICACAO

databaseClean <- mutate(databaseClean,meioscomunicacaoParticipou
                        = ifelse(databaseClean$meioscomunicacao==
                                   1,1,0))



# executar modelo MEIOS DE COMUNICACAO

modelo4 <- glm(meioscomunicacaoParticipou ~ percepcao_violencia_policialTodosDias +
                 esgotamentoSim +
                 escolaridade +
                 classeBaixa +
                 generoMulher +
                 racaNegroMulato +
                 idade,
               data = databaseClean,
               family = 'binomial')

# visualizar resultados
summary(modelo4)

# visualizar em log odds (razao de chance)
stargazer(modelo4,
          type = "text", title = "Resultados do Modelo", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)

# executar modelo 4.4 meios de comunicação sem var. de controle 
modelo4.4 <- glm(meioscomunicacaoParticipou ~
                   percepcao_violencia_policialTodosDias, 
                 data = databaseClean, 
                 family = "binomial")

#vizualização do modelo 4.4
summary(modelo4.4)
stargazer(modelo4.4, type = "text", title = "Resultados do Modelo", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)


# Modelo de Regressão Logística Multinomial (Multinomial Logit Regression) #
library("nnet")

# recodificar VD

databaseClean$engajemento_politico <- factor(databaseClean$engajamento_politico, 
                                             levels = c(1, 2, 3, 4, 5), 
                                             labels = c("votar sempre", "votar e também protestar", 
                                                                                   "só protestar e não serve votar", "não votar e nem protestar", "nao sei"))



# Definir a categoria de parametro para comparacao
databaseClean$engajamento_politico <- relevel(databaseClean$engajamento_politico, 
                                              ref = "votar e também protestar")




modelo5 <- multinom(engajamento_politico ~ percepcao_violencia_policialTodosDias + esgotamentoSim + 
                      escolaridade + classeBaixa + generoMulher + racaNegroMulato + idade, data = databaseClean)



summary(modelo5)

# executar modelo ENGAJAMENTO POLITICO sem var. de controle

modelo5.5 <- multinom(engajamento_politico ~ percepcao_violencia_policialTodosDias, data = databaseClean)


summary(modelo5.5)

stargazer(modelo5, type = "text", title = "Tipos de Engajamento", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)

?stargazer

# MODELO 6 conversar sobre politica

# Recodificar VD
databaseClean <- mutate(databaseClean, conversar_politicaOrd = 0)
databaseClean$conversar_politicaOrd[databaseClean$conversar_politica == 
                                      1] <- 4
databaseClean$conversar_politicaOrd[databaseClean$conversar_politica == 
                                      2] <- 3
databaseClean$conversar_politicaOrd[databaseClean$conversar_politica == 
                                      3] <- 2
databaseClean$conversar_politicaOrd[databaseClean$conversar_politica == 
                                      4] <- 1

databaseClean$conversar_politicaOrd <- factor(databaseClean$conversar_politicaOrd, 
                                              levels = c("1", "2", "3", "4"), labels = c("nunca", "quase nunca", "frequentemente", 
                                                                                         "muita frequência"), ordered = TRUE)


modelo6 <- clm(conversar_politicaOrd ~
                 percepcao_violencia_policialTodosDias +
                 esgotamentoSim + 
                 escolaridade +
                 classeBaixa +
                 generoMulher +
                 racaNegroMulato +
                 idade,
               data = databaseClean)


summary(modelo6)

# executar modelo 6 conversar politica sem var. de controle 
modelo6.6 <- glm(conversar_politicaOrd ~
                   percepcao_violencia_policialTodosDias, 
                 data = databaseClean, 
                 family = "binomial")

summary(modelo6.6)


#vizualização do modelo 6
summary(modelo6)
stargazer(modelo6, type = "text", title = "Resultados do Modelo", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)

stargazer(modelo6.6, type = "text", title = "Resultados do Modelo", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)


#grafico de disperção efeito da violencia policial em participar de protestos 

head(databaseClean)
ggplot(data = databaseClean, aes(x = escolaridade , y = participacao_protestosParticipou)) + 
  geom_point()



#correlação de pearsen 
plot (databaseClean$percepcao_violencia_policial, databaseClean$idade)

cor (databaseClean$percepcao_violencia_policial, databaseClean$idade)

cor.test (databaseClean$percepcao_violencia_policial, databaseClean$idade)


plot (databaseClean$percepcao_violencia_policial, databaseClean$escolaridade)

cor (databaseClean$percepcao_violencia_policial, databaseClean$escolaridade)

cor.test (databaseClean$percepcao_violencia_policial, databaseClean$escolaridade)

cor(databaseClean$meioscomunicacao, databaseClean$idade)

cor (databaseClean$meioscomunicacao, databaseClean$idade)

plot (databaseClean$fazer_peticao,databaseClean$idade)

cor (databaseClean$percepcao_violencia_policial, databaseClean$genero)

cor (databaseClean$percepcao_violencia_policialTodosDias, databaseClean$racaNegroMulato)

cor (databaseClean$percepcao_violencia_policial, databaseClean$genero)
cor (databaseClean$percepcao_violencia_policialTodosDias, databaseClean$generoMulher)
plot(databaseClean$percepcao_violencia_policial, databaseClean$generoMulher)

cor (databaseClean$percepcao_violencia_policialTodosDias, databaseClean$esgotamento)
cor (databaseClean$percepcao_violencia_policial, databaseClean$classe_social)
cor (databaseClean$percepcao_violencia_policialTodosDias, databaseClean$classeBaixa)
cor (databaseClean$percepcao_violencia_policialTodosDias, databaseClean$engajamento_politico)
cor (databaseClean$percepcao_violencia_policialTodosDias, databaseClean$conversar_politica)
cor (databaseClean$percepcao_violencia_policialTodosDias, databaseClean$fazer_peticaoParticipou)
cor (databaseClean$percepcao_violencia_policialTodosDias, databaseClean$meioscomunicacaoParticipou)
cor (databaseClean$percepcao_violencia_policialTodosDias, databaseClean$engajamento_politico)


      
install.packages('corrplot')

library('corrplot')

table(databaseClean$engajamento_politico)

?ggplot

ggplot(databaseClean, aes(x = percepcao_violencia_policialTodosDias , y = engajamento_politico)) + 
  geom_point() + stat_smooth(method = "multinom", method.args = list(family = "multinomial"))

ggplot(databaseClean, aes(y=engajamento_politico, x=percepcao_violencia_policial)) +
  geom_point() + 
  geom_smooth(method="lm")

ggplot(databaseClean, aes(x = percepcao_violencia_policialTodosDias, y = engajamento_politico )) + 
  theme_bw() +
  geom_boxplot() +
  labs(title="Efeito da Violência Policial sobre os tipos de engajamento ", x="", y="Engajamentos") + scale_y_continuous(breaks=seq(0,1,5))
