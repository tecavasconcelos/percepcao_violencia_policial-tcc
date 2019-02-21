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


#selecionar colunas no banco de dados por nome
database<-Latinobarometro_2015_Eng[,c('idenpa','P21TGB.A','P21ST.B','P21ST.C','P21N.D','P32N','P20TGB.A','P63NJ.G','S23','S12','S6','S18','S13')]

str(database)

#renomear colunas
colnames(database)<-c('paises','fazer_peticao','assistir_protestos','participacao_protestos','meioscomunicacao','engajamento_politico','conversar_politica','percepcao_violencia_policial','raca','genero','classe_social','escolaridade','idade')


library(Amelia)
library('dplyr')
library (ggplot2)
library(stargazer)
library("nnet")
library(ordinal)


#1construir uma data.frame da variavel paises

paisesData <-data.frame(table(database$paises))


paisesData
mutate(paisesData, porcentagem = Freq/sum(Freq))* 100

paisesData$Nomes <-''
paisesData$Nomes <- factor(paisesData$Var1,levels = c('32','68','76','152','170','188','214','218','222','320','340','484','558','591','600','604','858','862'),
                           labels = c('argentina','bolivia','brazil','chile', 'colombia','costa_rica','rep_dominicana','equador','el_salvador','guatemala','honduras','mexico','nicaragua','panama','paraguay','peru','uruguay','venezuela'))


#------------------------------#
#  SELECAO DO CASO BRASILEIRO  #
#------------------------------#


database$paises <- as.factor(database$paises)

# Selecionar casos com base no valor de uma variavel
databaseBr <- database[database$paises == 76, ]






#-------------------------#
#  VARIAVEL INDEPENDENTE  #
#-------------------------#


# construir uma data.frame da variavel percepcao_da_violencia_policial

violencia_policialData <-data.frame(table(databaseBr$percepcao_violencia_policial))

violencia_policialData
mutate(violencia_policialData, porcentagem = (Freq/sum(Freq)) * 100)


violencia_policialData$Nomes <- ''
violencia_policialData$Nomes <- factor(violencia_policialData$Var1,levels = c('1','2','3','4','5'),
                                       labels = c('quase todos os dias','uma/duas vezes por semana','poucas vezes por mes','poucas vezes por ano', 'nao ocorre'))

ggplot(data = violencia_policialData,                
       aes(x = Nomes, y = Freq) ) +   
  geom_col()  

#-------------------------#
#  VARIAVEIS DEPENDENTES  #
#-------------------------#

#1
# construir data.frame com a variavel participacao_protestos

participacaoData <- data.frame(table(databaseBr$participacao_protestos))
participacaoData
mutate(participacaoData, porcentagem = (Freq/sum(Freq)) * 100)

summary(databaseBr$participacao_protestos)

participacaoData$Nomes <- ''
participacaoData$Nomes <- factor(participacaoData$Var1,levels = c('1','2','3'),
                                 labels = c('já fez','talvez participaria','nunca participaria'))


#2
# construir data.frame com a variavel fazer_peticoes

table(databaseBr$fazer_peticao)


fazer_peticaoData <-data.frame(table(databaseBr$fazer_peticao))
fazer_peticaoData
mutate(fazer_peticaoData, porcentagem = (Freq/sum(Freq)) *100)

summary(databaseBr$fazer_peticao)

fazer_peticaoData$Names <- ''
fazer_peticaoData$Names <- factor(fazer_peticaoData$Var1, levels = c('1','2','3'),
                                  labels = c('ja fez','poderia fazer','nunca faria'))



#3
# construir data.frame com a variavel assistir_protestos

table(databaseBr$assistir_protestos)

assistir_protestosData <-data.frame(table(databaseBr$assistir_protestos))
assistir_protestosData
mutate(assistir_protestosData, porcentagem = (Freq/sum(Freq)) *100)


summary(databaseBr$assistir_protestos)

assistir_protestosData$Names <- ''
assistir_protestosData$Names <- factor(assistir_protestosData$Var1, levels = c('1','2','3'),
                                       labels = c('ja fez','poderia fazer','nunca faria'))



#4
# construir data.frame com a variavel usar os meios comunicacao para fazer uma denuncia

table(databaseBr$meioscomunicacao)

meioscomunicacaoData <-data.frame(table(databaseBr$meioscomunicacao))
meioscomunicacaoData
mutate(meioscomunicacaoData, porcentagem = (Freq/sum(Freq)) *100)

summary(databaseBr$meioscomunicacao)

meioscomunicacaoData$Names <- ''
meioscomunicacaoData$Names <- factor(meioscomunicacaoData$Var1, levels = c('1','2','3'),
                                     labels = c('ja fez','poderia fazer','nunca faria'))

#5
# construir data.frame com a variavel conversar politica

table(databaseBr$conversar_politica)


conversar_politicaData <-data.frame(table(databaseBr$conversar_politica))
conversar_politicaData
mutate(conversar_politicaData, porcentagem = (Freq/sum(Freq)) *100)

summary(databaseBr$conversar_politica)


conversar_politicaData$Names <- ''
conversar_politicaData$Names <- factor(conversar_politicaData$Var1, levels = c('1','2','3','4'),
                                       labels = c('muita frequencia','frequentemente','quase nunca','nunca'))





#6
# construir data.frame com a variavel formas de engajamento politico

table(databaseBr$engajamento_politico)

engajamento_politicoData <- data.frame(table(databaseBr$engajamento_politico))

engajamento_politicoData
mutate(engajamento_politicoData, porcentagem = (Freq/sum(Freq)) * 100)

summary(databaseBr$engajamento_politico)

engajamento_politicoData$Nomes <-''
engajamento_politicoData$Nomes <- factor(engajamento_politicoData$Var1,levels = c('1','2','3','4','5'),
                                         labels = c('votar sempre','votar e também protestar','so protestar e nao serve votar','nao votar e nem protestar','nao sei'))



#-------------------------#
#       CONTROLES         #
#-------------------------#



#1
#construir data.frame com a variavel de raca

table(databaseBr$raca)

racaData <-data.frame(table(databaseBr$raca))
racaData

utate(racaData, porcentagem = (Freq/sum(Freq)) *100)

summary(databaseBr$raca)

racaData$Names <- ''
racaData$Names <- factor(racaData$Var1,levels = c('1','2','3','4','5','6','7'),
                         labels = c('asiatico','negro','indegena','mestico','mulato','branco','outra'))

#2
# construir data.frame com a variavel de genero

table(databaseBr$genero)

generoData <-data.frame(table(databaseBr$genero))
generoData
mutate(generoData, porcentagem = (Freq/sum(Freq)) *100)

summary(databaseBr$genero)

generoData$Names <- ''
generoData$Names <- factor(generoData$Var1, levels = c('1','2'),
                           labels = c('masculino','feminino'))

#3
# construir data.frame com a variavel de classe social

table(databaseBr$classe_social)

classesocialData <-data.frame(table(databaseBr$classe_social))
classesocialData
mutate(classesocialData, porcentagem = (Freq/sum(Freq)) *100)

summary(databaseBr$classe_social)

classesocialData$Names <- ''
classesocialData$Names <- factor(classesocialData$Var1, levels = c('1','2','3','4','5'),
                                 labels = c('alta','media alta','media','media baixa','baixa'))

ggplot(data = classesocialData,               
       aes(x = Names, y = Freq) ) +  
  geom_col()


#4
# construir data.frame com a variavel idade

table(databaseBr$idade)

idadeData <-data.frame(table(databaseBr$idade))
idadeData
mutate(idadeData, porcentagem = (Freq/sum(Freq))* 100)

summary(databaseBr$idade)

ggplot(data = databaseBr, aes(x = databaseBr$idade)) + geom_histogram()

#-------------------------#
#  VARIAVEL MODERADORA    #
#-------------------------#

#5
# construir data.frame com a variavel escolaridade

table(databaseBr$escolaridade)

escolaridadeData <-data.frame(table(databaseBr$escolaridade))
escolaridadeData
mutate(escolaridadeData, porcentagem = (Freq/sum(Freq)) *100)

summary(databaseBr$escolaridade)


ggplot(data = databaseBr, aes(x = databaseBr$escolaridade)) + geom_histogram()




#------------------------#
#    RECODIFICACA0       #
# variavel independente  #
#------------------------#


databaseBr <- mutate(databaseBr, violenciaPolicialOrd = 0)
databaseBr$violenciaPolicialOrd[databaseBr$percepcao_violencia_policial == 5] <- 0
databaseBr$violenciaPolicialOrd[databaseBr$percepcao_violencia_policial == 4] <- 1
databaseBr$violenciaPolicialOrd[databaseBr$percepcao_violencia_policial == 3] <- 2
databaseBr$violenciaPolicialOrd[databaseBr$percepcao_violencia_policial == 2] <- 3

databaseBr$violenciaPolicialOrd[databaseBr$percepcao_violencia_policial == 1] <- 4


table(databaseBr$violenciaPolicialOrd)

violencia_policialData <- data.frame(table(databaseBr$violenciaPolicialOrd))
violencia_policialData <- mutate(violencia_policialData, porcentagem = (Freq/sum(Freq)) * 100)

violencia_policialData$porcentagem <- round(violencia_policialData$porcentagem, 2)

databaseBr <- mutate(databaseBr, percepcao_violencia_policialTodosDias = ifelse(databaseBr$violenciaPolicialOrd == 
                                                                                        1,1,0))





#------------------------#
#    RECODIFICACA0       #
# variaveis dependentes  #
#------------------------#

#1
databaseBr <- mutate(databaseBr, participacao_protestosParticipou = ifelse(databaseBr$participacao_protestos == 
                                                                                   1, 1, 0))


#2
databaseBr <- mutate(databaseBr, fazer_peticicaoParticipou = ifelse(databaseBr$fazer_peticao == 
                                                                                   1, 1, 0))

#3
databaseBr <- mutate(databaseBr, assistir_protestosParticipou = ifelse(databaseBr$assistir_protestos == 
                                                                                   1, 1, 0))


#4
databaseBr <- mutate(databaseBr, meioscomunicacaoParticipou = ifelse(databaseBr$meioscomunicacao ==
                                                                       1, 1, 0))


#5

databaseBr<- mutate(databaseBr, conversar_politicaParticipou = ifelse(databaseBr$conversar_politica == 1 | 
                                                                      databaseBr$conversar_politica== 2, 1, 0))

#6
# Recodificar formas de engajamento político
databaseBr<- mutate (databaseBr, engajamento_politico_Recod = 0)
databaseBr$engajamento_politico_Recod[databaseBr$engajamento_politico == 4] <- 1
databaseBr$engajamento_politico_Recod[databaseBr$engajamento_politico == 3] <- 2
databaseBr$engajamento_politico_Recod[databaseBr$engajamento_politico == 2] <- 3
databaseBr$engajamento_politico_Recod[databaseBr$engajamento_politico == 1] <- 2

databaseBr$engajamento_politico_Recod <- factor(databaseBr$engajamento_politico_Recod, 
                                                   levels = c("1", "2", "3"),
                                                labels = c("nao votar e protestar", "votar ou protestar", "votar e protestar"), ordered = TRUE)




#------------------------#
#    RECODIFICACA0       #
#     controles          #
#------------------------#


databaseBr<- mutate(databaseBr, classeBaixa = ifelse(classe_social == 
                                                              5, 1, 0))



databaseBr<- mutate(databaseBr, generoMulher = ifelse(genero == 2, 1, 
                                                             0))
databaseBr<- mutate(databaseBr, racaNegroMulato = ifelse(raca == 2 | 
                                                                  raca == 5, 1, 0))




#------------------------#
#    indice              #
#     part. politica     #
#------------------------#

databaseBr <-mutate(databaseBr, indice_participacao_politica =
                      databaseBr$participacao_protestosParticipou + databaseBr$fazer_peticicaoParticipou +
                      databaseBr$assistir_protestosParticipou + databaseBr$meioscomunicacaoParticipou + databaseBr$conversar_politicaParticipou)





#-----------------------#
#  modelos de regressão #
# indice part. politica #
#-----------------------#


#apenas variaveis de controle

modelo1 <- lm(indice_participacao_politica ~
                escolaridade +
                classeBaixa +
                generoMulher +
                idade +
                racaNegroMulato,
              data = databaseBr)


summary(modelo1)

stargazer(modelo1, type = "text", title = "Resultados do Modelo 1", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)

#só variável independente


modelo2 <- lm( indice_participacao_politica ~
                 percepcao_violencia_policialTodosDias,
               data = databaseBr
                 )


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
              data = databaseBr)

summary(modelo3)

stargazer(modelo3, type = "text", title = "Resultados do Modelo 3", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)


#executar modelo 4 - variavel independente + variaveis de controle + moderadora

databaseBr<- mutate(databaseBr, percepcao_violenciaTodosDias_Esc = percepcao_violencia_policialTodosDias*escolaridade)

modelo4 <- lm(indice_participacao_politica ~
                percepcao_violenciaTodosDias_Esc +
                percepcao_violencia_policialTodosDias +
                escolaridade +
                classeBaixa +
                generoMulher +
                idade +
                racaNegroMulato,
              data = databaseBr)




summary(modelo4)

stargazer(modelo4, type = "text", title = "Resultados do Modelo 4", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)



#-----------------------#
#  modelos de regressão #
# engajamento politico  #
#-----------------------#



# executar modelo 1 - apenas variaveis de controle


modelo5 <- clm(engajamento_politico_Recod ~
                      escolaridade +
                      classeBaixa +
                      generoMulher +
                      idade +
                      racaNegroMulato,
                    data = databaseBr)




summary(modelo5)
stargazer(modelo5, type = "text", title = "Resultados do Modelo 5", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)

# executar modelo 6 - só variável independente


modelo6 <- clm(engajamento_politico_Recod ~
                      percepcao_violencia_policialTodosDias,
                    data = databaseBr)


summary(modelo6)

stargazer(modelo6, type = "text", title = "Resultados do Modelo 6", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)


#executar modelo 3 - variavel independente + variaveis de controle


modelo7 <- clm (engajamento_politico_Recod ~
                       percepcao_violencia_policialTodosDias +
                       escolaridade +
                       classeBaixa +
                       generoMulher +
                       idade +
                       racaNegroMulato,
                     data = databaseBr)


summary(modelo7)

stargazer(modelo7, type = "text", title = "Resultados do Modelo 7", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)


#executar modelo 8 - variavel independente + variaveis de controle + moderadora


modelo8 <- clm(engajamento_politico_Recod ~
                      percepcao_violenciaTodosDias_Esc +
                      percepcao_violencia_policialTodosDias +
                      escolaridade +
                      classeBaixa +
                      generoMulher +
                      idade +
                      racaNegroMulato,
                    data = databaseBr)




summary(modelo8)

stargazer(modelo8, type = "text", title = "Resultados do Modelo 8", style = "ajps", 
          apply.coef = exp, p.auto = FALSE)


ggplot(data = violencia_policialDataonar o banco de dados
       aes(x = Nomes, y = porcentagem) ) +   # selecionar variaveis do eixo x e y
  geom_col()  


