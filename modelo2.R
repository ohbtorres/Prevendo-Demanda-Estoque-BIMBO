### PROJETO 2 - CALCULO DE DEMANDA DO GRUPO BIMBO

################################# INFORMAÇÕES RELEVANTES

#. Existem produtos no dataset de teste que não existem no dataset de treino e o modelo deve prever mesmo assim
#. A demanda é o saldo entre as vendas e o valor dos produtos que retornaram sem venda
#. É necessário prever a demanda de um produto de uma determinada loja
#. Cliente ID está duplicado, ou seja, um mesma Cliente ID para vários nomes. Necessidade de tratar os dados
#. Os dados de teste não possuem as informações de semana, vendas, devoluções e demanda
#. A demanda ajustada (Demanda_uni_equil) é sempre> = 0, pois a demanda deve ser 0 ou um valor positivo. 
#       A razão pela qual Venta_uni_hoy - Dev_uni_proxima às vezes tem valores negativos 
#       é que os registros de retorno às vezes duram algumas semanas.
#. Os dados de teste só possuem as informações de Semana, Agencia_ID, Canal_ID, Ruta_SAK, Cliente_ID, Producto_ID

################################# SELECIONANDO O DIRETORIO DE TRABALHO

setwd("D:/ohbto/Documents/FormacaoCientistaDados/BigDataRAzure/Projeto/Projeto2")

################################# CARREGANDO AS BIBLIOTECAS DE INTERESSE

library(data.table)
library(explore)
library(ggplot2)
library(caret)
library(corrplot)
library(openxlsx)
library(Metrics)
library(doParallel)

################################# CARREGANDO DATASET 


treino <- fread("train.csv",stringsAsFactors = F)
# ao todo, são 74.180.464 de registros no arquivo de treino
cliente_tabla <- fread("cliente_tabla.csv",stringsAsFactors = F)
producto_tabla <- fread("producto_tabla.csv",stringsAsFactors = F)
town_state <- fread("town_state.csv",stringsAsFactors = F)
#dados de teste, ao todo, são 6.999.251 de registros
teste <- fread("test.csv",stringsAsFactors = F)

################################# EXPLORANDO CLIENTE_TABLA 

describe(cliente_tabla)

cliente_tabla[,.N,by=Cliente_ID,
              ][N>=2] #podemos ver que existem 4862 registros de clientes duplicados.
#Dessa forma, precisamos remover os valores duplicados, para que Cliente_ID seja uma chave primária

#Vamos dar uma olhada em como estão os valores duplicados
View(cliente_tabla[Cliente_ID %in% cliente_tabla[duplicated(Cliente_ID),Cliente_ID], with = T])

#Os valores duplicados são apenas uma forma diferente de escrever o mesmo nome. Por isso, para os valores
#duplicados deste dataset, vamos remover a primeira referencia
cliente_tabla <- cliente_tabla[!duplicated(Cliente_ID)]


################################# EXPLORANDO PRODUCTO_TABLA 

describe(producto_tabla) # a quantidade de valores únicos para o ID este dataset é a mesma quantidade
#de linhas, portanto, os valores estão corretos

################################# EXPLORANDO TOWN_STATE 

describe(town_state) # a quantidade de valores únicos para o ID este dataset é a mesma quantidade
#de linhas, portanto, os valores estão corretos

################################# JOIN DATASET

#Unindo as informações dos dataset de treino e teste com os dataset complementares
treino[cliente_tabla, on = c("Cliente_ID"), NombreCliente := NombreCliente,
                ][producto_tabla, on = c("Producto_ID"), NombreProducto := NombreProducto
                ][town_state, on = c("Agencia_ID"), c("Town","State") := list(Town,State)]

teste[cliente_tabla, on = c("Cliente_ID"), NombreCliente := NombreCliente,
      ][producto_tabla, on = c("Producto_ID"), NombreProducto := NombreProducto
        ][town_state, on = c("Agencia_ID"), c("Town","State") := list(Town,State)]



#Remove os dataset auxiliares que nao são mais uteis
rm(cliente_tabla)
rm(producto_tabla)
rm(town_state)

################################# CRIANDO NOVAS VARIAVEIS 

str(treino)
describe(treino)

#Vamos juntar os dados de treino e de teste num conjunto de dados só
treino <- rbind(treino,teste,fill = T)
rm(teste)

#Para a variável ID da agência, como existem 552 na base de treino, vamos juntar essa informação com
#os dados de teste e descobrir a quantidade de aparições delas nos 2 dataset
#Para a variável Ruta_SAK, faremos os mesmo procedimento, assim como para Cliente_ID, Producto_ID, Town e State

#vamos agora fazer algumas combinacoes desses dados para contagem para verificar, posteriormente, a relevancia
treino[,Agencia_ID_N := .N,by = Agencia_ID,
                          ][,Ruta_SAK_N := .N, by = Ruta_SAK,
                          ][,Cliente_ID_N := .N, by = Cliente_ID,
                          ][,Producto_ID_N := .N, by = Producto_ID,
                          ][,Town_N := .N, by = Town,
                          ][,State_N := .N, by = State,
                          ][,Agencia_ID_Ruta_SAK_N := .N, by = .(Agencia_ID,Ruta_SAK),
                          ][,Agencia_ID_Producto_ID_N := .N, by = .(Agencia_ID,Producto_ID),
                          ][,Agencia_ID_Town_N := .N, by = .(Agencia_ID,Town),
                          ][,Agencia_ID_State_N := .N, by = .(Agencia_ID,State),
                          ][,Ruta_SAK_Cliente_ID_N := .N, by = .(Ruta_SAK,Cliente_ID),
                          ][,Ruta_SAK_Producto_ID_N := .N, by = .(Ruta_SAK, Producto_ID),
                          ][,Ruta_SAK_Town_N := .N, by = .(Ruta_SAK,Town),
                          ][,Ruta_SAK_State_N := .N, by = .(Ruta_SAK, State),
                          ][,Cliente_ID_Producto_ID_N := .N, by = .(Cliente_ID,Producto_ID),
                          ][,Cliente_ID_Town_N := .N, by = .(Cliente_ID, Town),
                          ][,Cliente_ID_State_N := .N, by = .(Cliente_ID,State),
                          ][,Producto_ID_Town_N := .N, by= .(Producto_ID,Town),
                          ][,Producto_ID_State_N := .N, by = .(Producto_ID,State),
                          ][,Agencia_ID_Producto_ID_Cliente_ID_N := .N, by = .(Agencia_ID,Producto_ID,Cliente_ID),
                          ][,Agencia_ID_Ruta_SAK_Producto_ID_N := .N, by=.(Agencia_ID,Ruta_SAK,Producto_ID),
                          ][,Agencia_ID_Cliente_ID_Producto_ID_Town_N := .N, by = .(Agencia_ID,Cliente_ID,
                                                                                   Producto_ID,Town),
                          ][,Cliente_ID_Producto_ID_Town_N := .N, by = .(Cliente_ID, Producto_ID, Town),
                          ][,Agencia_ID_Cliente_ID_Town := .N, by = .(Agencia_ID,Cliente_ID,Town),
                          ][,ClasseCliente := ifelse(NombreCliente == "NO IDENTIFICADO", 
                                                     "Nao_Cadastrado","Cadastrado"),
                          ][,State := ifelse(State == "Queretaro de Arteaga", "QUERETARO",State), #estado mudou o nome
                          ][,NombreProducto := NULL,
                          ][,NombreCliente := NULL]

#Realizando uma pesquisa, foi possivel encontrar a população dos estados do Mexico, que estão no arquivo anexo
Pop2015 <- read.xlsx("PopMexico2015.xlsx")
treino[Pop2015, on = c("State"), Pop := Pop2015]
rm(Pop2015)

################################# CRIAÇÃO DE BACKUP PARA AMOSTRAGEM

#Criando os backups depois das manipulações e removendo os dados de teste para posterior utilização
fwrite(treino[!is.na(id)],"backup_teste.csv")

#Para o arquivo de treino, que é maior, teremos que dividi-lo em 4 arquivos menores
sequencia <- seq(1,18545116*4,by = 18545116)
sequencia2 <- seq(18545116,18545116*4,by = 18545116)
sequencia3 <- 1:4
mapply(function(x,y,z){fwrite(treino[x:y],paste("backup_treino",z,".csv",sep=""))},sequencia,sequencia2,sequencia3)


#Necessário agora fazer uma amostragem dos dados de teste dos 4 arquivos, pois, pelo tamanho, não será possível
#carregá-lo inteiro
source("Tools.r")
#Para essa amostra, vamos considerar um arquivo de treino de 100.000 registros, para que possamos
#trabalhar com os dados
treino <- do.call(rbind,
                        lapply(sequencia3,
                               function(x){sample_file(paste("backup_treino",x,".csv",sep = ""),0.001348072,18545116)}))
fwrite(treino,"amostra_menor_treino_backup.csv")


################################# EXPLORAÇÃO E MANIPULAÇÃO DOS DADOS

#Carregando o backup manipulado
treino <- fread("amostra_menor_treino_backup.csv",stringsAsFactors = F)
#a partir dessa amostra, vamos fazer a analise
describe(treino)

#Vamos fazer algumas alterações nos tipos de dados
#A variável semana pode ser transformar numa variável do tipo fator
#A variável Canal_ID pode ser tratada como fator
treino[,c("Semana","Canal_ID","ClasseCliente") := 
         list(factor(Semana, levels = c(3,4,5,6,7,8,9,10,11), ordered = T), #os niveis foram considerados o dataset de treino e de teste
              factor(Canal_ID),
              factor(ClasseCliente))]

#Vamos agora, carregar os tipos de dados de cada variavel do dataset
classes <- lapply(treino[0],class)

classes_numericas <- grep(pattern = "integer",classes)
classes_numericas <- append(classes_numericas,grep(pattern = "numeric",classes))
classes_fator <- grep(pattern = "factor",classes)
variaveis_valor <- c("Venta_uni_hoy","Venta_hoy","Dev_uni_proxima","Dev_proxima","Demanda_uni_equil")

#Vamos verificar a correlação entre as variáveis
corrplot(cor(treino[,classes_numericas,with=F]),tl.cex = 0.7,method="color",type="lower")
#Demanda_uni_equil tem uma alta correlaçao com  Venta_uni_hoy e Dev_proxima

#Verificando a demanda por estado
ggplot(treino,aes(x = State, y = Demanda_uni_equil)) + geom_col() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Verificando a margem de cada variavel numerica em um box-plot por semana
Map(function(x){
  ggplot(treino,mapping = aes_string(x = "Semana", y = x)) + geom_boxplot() 
}, variaveis_valor)


#Verificando como algumas variaveis se comportam em relação à demanda
Map(function(x){
  ggplot(treino,mapping = aes_string(x = x, y = variaveis_valor[5])) + 
    geom_point(aes_string(colour = variaveis_valor[5])) + 
    geom_smooth(method = "glm") +
    scale_colour_gradient(low = "green", high = "blue")
},names(treino)[classes_numericas[c(8,9,10,6)]])

#Verificando o comportamento da demanda ao longo das semanas
ggplot(treino,aes(x = Semana, y = Demanda_uni_equil)) + geom_col()

#Vamos normalizar os dados numericos
variaveis_normalizar <- classes_numericas[c(-1,-2,-3,-4)]

sdTarget <- treino[,sd(Demanda_uni_equil)]
meanTarget <- treino[,mean(Demanda_uni_equil)]

treino[,(variaveis_normalizar) := lapply(.SD, function(x){(x-mean(x))/sd(x)}),
           .SDcols = variaveis_normalizar]

################################# FEATURE SELECTION VENTA E DEV
#Vamos usar a função train do pacote caret para definir quais as variaveis são mais relevantes.
#Contudo, iremos fazer a modelagem e 3 etapas: a primeira irá predizer o valor de Venta_uni_hoy,
#em seguida, o valor de Dev_uni_proxima e, por último, o valor de Demanda_uni_equil, utilizando
# os valores anteriores
featureSelectionVenta_uni_hoy <- train(form = Venta_uni_hoy ~ . ,
                          data = treino[,-unlist(list("State","id","Town",variaveis_valor[c(2,3,4,5)])),with = F],
                          method = "xgbTree",
                          trControl = trainControl(method = "cv",
                                                   number = 10),
                          tuneGrid = expand.grid(nrounds = 300,
                                                 max_depth = 3,
                                                 eta = 0.1,
                                                 gamma = 1/3,
                                                 colsample_bytree = 1,
                                                 min_child_weight = 1, 
                                                 subsample = 1),
                          metric = "RMSE")

#Visualizando as variáveis de maior relevancia para esta feature selection
varImp(featureSelectionVenta_uni_hoy)
ggplot(data.frame(Variavel = row.names(varImp(featureSelectionVenta_uni_hoy)$importance),
                  varImp(featureSelectionVenta_uni_hoy)$importance),aes(x=reorder(Variavel,Overall),y=Overall))+
  geom_bar(stat="identity")+coord_flip()
variaveis_selecionadas_Venta_uni_hoy <- c("Ruta_SAK_Cliente_ID_N","Ruta_SAK_Town_N","Cliente_ID_Producto_ID_N",
                                          "Pop","Producto_ID_N", "Agencia_ID_Producto_ID_Cliente_ID_N",
                                          "Agencia_ID_Producto_ID_N","Cliente_ID_N","Semana",
                                          "Agencia_ID_Ruta_SAK_N",
                                          "Agencia_ID_Cliente_ID_Producto_ID_Town_N",
                                          "Agencia_ID_Ruta_SAK_Producto_ID_N","Producto_ID_State_N")

  
featureSelectionDev_uni_proxima <- train(form = Dev_uni_proxima~ . ,
                                           data = treino[,-unlist(list("State","id","Town",
                                                                       variaveis_valor[c(1,2,4,5)])),with = F],
                                           method = "xgbTree",
                                           trControl = trainControl(method = "cv",
                                                                    number = 10),
                                           tuneGrid = expand.grid(nrounds = 300,
                                                                  max_depth = 3,
                                                                  eta = 0.1,
                                                                  gamma = 1/3,
                                                                  colsample_bytree = 1,
                                                                  min_child_weight = 1, 
                                                                  subsample = 1),
                                           metric = "RMSE")

#Visualizando as variáveis de maior relevancia para esta feature selection
varImp(featureSelectionDev_uni_proxima)
ggplot(data.frame(Variavel = row.names(varImp(featureSelectionDev_uni_proxima)$importance),
                  varImp(featureSelectionDev_uni_proxima)$importance),aes(x=reorder(Variavel,Overall),y=Overall))+
  geom_bar(stat="identity")+coord_flip()
variaveis_selecionadas_Dev_uni_proxima <- c("Agencia_ID_Ruta_SAK_Producto_ID_N","Semana","Producto_ID_State_N",
                                            "Producto_ID_N","Pop","Ruta_SAK_Cliente_ID_N",
                                            "Agencia_ID_Ruta_SAK_N","Agencia_ID_N","Town_N",
                                            "Cliente_ID_Producto_ID_N","Agencia_ID_Producto_ID_N",
                                            "Agencia_ID_Cliente_ID_Producto_ID_Town_N",
                                            "Cliente_ID_Producto_ID_N","Agencia_ID_Producto_ID_N",
                                            "Producto_ID_Town_N","Agencia_ID_Producto_ID_Cliente_ID_N",
                                            "Cliente_ID_Town_N","Ruta_SAK_Town_N","Ruta_SAK_Producto_ID_N")
  
  
################################# DIVIDINDO AS VARIAVEIS DE TREINO E DE VALIDAÇÃO
index_treino <- sample(1:nrow(treino),round(0.7*nrow(treino)))

validacao <- treino[!index_treino]
treino <- treino[index_treino]  

################################ CRIANDO O MODELO PARA DEV E VENTA

modelo_Venta_uni_hoy <- train(form = as.formula(paste("Venta_uni_hoy  ~ ",
                                         paste(variaveis_selecionadas_Venta_uni_hoy,collapse = " + "))) ,
                 data = treino,
                 method = "xgbTree",
                 trControl = trainControl(method = "repeatedcv",
                                          number = 10,
                                          p = 0.7,
                                          search = "grid",
                                          allowParallel  =T),
                 tuneGrid = expand.grid(nrounds = 300,
                                        max_depth = 3,
                                        eta = 0.1,
                                        gamma = 1/3,
                                        colsample_bytree = 1,
                                        min_child_weight = 1, 
                                        subsample = 1),
                 metric = "RMSE")

#Calculando a taxa de erro do modelo para o conjunto de treino e validacao
rmse(treino$Venta_uni_hoy,predict(modelo_Venta_uni_hoy,treino))
rmse(validacao$Venta_uni_hoy,predict(modelo_Venta_uni_hoy,validacao))

modelo_Dev_uni_proxima <- train(form = as.formula(paste("Dev_uni_proxima  ~ ",
                                         paste(variaveis_selecionadas_Dev_uni_proxima,collapse = " + "))) ,
                 data = treino,
                 method = "xgbTree",
                 trControl = trainControl(method = "repeatedcv",
                                          number = 10,
                                          p = 0.7,
                                          search = "grid",
                                          allowParallel  =T),
                 tuneGrid = expand.grid(nrounds = 300,
                                        max_depth = 3,
                                        eta = 0.1,
                                        gamma = 1/3,
                                        colsample_bytree = 1,
                                        min_child_weight = 1, 
                                        subsample = 1),
                 metric = "RMSE")

#Calculando a taxa de erro do modelo para o conjunto de treino e validacao
rmse(treino$Venta_uni_hoy,predict(modelo_Dev_uni_proxima,treino))
rmse(validacao$Venta_uni_hoy,predict(modelo_Dev_uni_proxima,validacao))

#Adicionando nos dataset de treino e validação as novas predicoes
treino[,Venta_uni_hoy_previsto := predict(modelo_Venta_uni_hoy,treino),
       ][,Dev_uni_proxima_previsto := predict(modelo_Dev_uni_proxima,treino)]
validacao[,Venta_uni_hoy_previsto := predict(modelo_Venta_uni_hoy,validacao),
          ][,Dev_uni_proxima_previsto := predict(modelo_Dev_uni_proxima,validacao)]

################################# FEATURE SELECTION OBJETIVO
featureSelection <- train(form = Demanda_uni_equil ~ . ,
                          data = treino[,-unlist(list("State","id","Town",variaveis_valor[1:4])),with = F],
                          method = "xgbTree",
                          trControl = trainControl(method = "cv",
                                                   number = 10),
                          tuneGrid = expand.grid(nrounds = 300,
                                                 max_depth = 3,
                                                 eta = 0.1,
                                                 gamma = 1/3,
                                                 colsample_bytree = 1,
                                                 min_child_weight = 1, 
                                                 subsample = 1),
                          metric = "RMSE")
varImp(featureSelection)
ggplot(data.frame(Variavel = row.names(varImp(featureSelection)$importance),
                  varImp(featureSelection)$importance),aes(x=reorder(Variavel,Overall),y=Overall))+
  geom_bar(stat="identity")+coord_flip()

variaveis_selecionadas <- c("Agencia_ID_Ruta_SAK_N",
                            "Pop","Semana","Cliente_ID_N",
                            "Agencia_ID_Producto_ID_Cliente_ID_N",
                            "Cliente_ID_Town_N","Cliente_ID_State_N",
                            "Agencia_ID_Ruta_SAK_N","Cliente_ID_N",
                            "Agencia_ID_Ruta_SAK_Producto_ID_N",
                            "Producto_ID_N","Ruta_SAK_State_N",
                            "Agencia_ID_Producto_ID_N","Cliente_ID_Producto_ID_N",
                            "Venta_uni_hoy_previsto","Dev_uni_proxima_previsto")

################################# CRIACAO DO MODELO PARA O OBJETIVO


modelo <- train(form = as.formula(paste("Demanda_uni_equil ~ ",paste(variaveis_selecionadas,collapse = " + "))) ,
                data = treino,
                method = "xgbTree",
                trControl = trainControl(method = "repeatedcv",
                                         number = 10,
                                         p = 0.7,
                                         search = "grid",
                                         allowParallel  =T),
                tuneGrid = expand.grid(nrounds = 300,
                                       max_depth = 3,
                                       eta = 0.1,
                                       gamma = 1/3,
                                       colsample_bytree = 1,
                                       min_child_weight = 1, 
                                       subsample = 1),
                metric = "RMSE")



#Calculando a taxa de erro do modelo para o conjunto de treino e validacao
rmse(treino$Demanda_uni_equil,predict(modelo,treino))
rmse(validacao$Demanda_uni_equil,predict(modelo,validacao))

#Inserindo as predicoes no dataset
treino[,predicao := predict(modelo,treino)]
validacao[,predicao := predict(modelo,validacao)]

################################# PREVENDO OS DADOS DE TESTE
teste <- fread("backup_teste.csv",stringsAsFactors = F)

#Alterando para fator
teste[,c("Semana","Canal_ID","ClasseCliente") := 
         list(factor(Semana, levels = c(3,4,5,6,7,8,9,10,11), ordered = T), #os niveis foram considerados o dataset de treino e de teste
              factor(Canal_ID),
              factor(ClasseCliente))]

#Normalizando as variaveis
teste[,(variaveis_normalizar) := lapply(.SD, function(x){(x-mean(x))/sd(x)}),
       .SDcols = variaveis_normalizar]

#Inserindo as predições de Venta_uni_hoy_previsto e Dev_uni_proxima_previsto
teste[,Venta_uni_hoy_previsto := predict(modelo_Venta_uni_hoy,teste),
       ][,Dev_uni_proxima_previsto := predict(modelo_Dev_uni_proxima,teste)]

#Por fim, calculando a previsão da demanda, já removendo a normalização
teste[,predicao := predict(modelo,teste)*sdTarget+meanTarget]

teste[,.(id,predicao)]
