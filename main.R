library(caret)
library(mlbench)
#library(doMC)
library(PerfMeas) #F.measure
library(DMwR) # para balancear a classe.
library(FSelector) # IG
library(doParallel)
library(EnvStats) # rosnerTest


##### PROCESSAMENTO DAS ESTATÃSTICAS #####
processaResultados <-
  function(predicao,
           teste,
           nome_dataset,
           index_classe) {
    matriz_de_confuncao <-
      confusionMatrix(predicao, teste[, index_classe])# fonte: https://rpubs.com/ryankelly/svm
    
    # PreparaÃ§Ã£o da tabela que sera utilizada como referÃªncia gerada
    # a partir dos dados de teste
    Targets <-
      matrix(0, ncol = length(unique(teste[, index_classe])), nrow = nrow(teste))
    
    for (i in 1:nrow(Targets)) {
      Targets[i, teste[i, index_classe]] = 1
    }
    
    # ConstruÃ§Ã£o da tabela que serÃ¡ usada para avaliar o modelo
    

    tabela_predicao <- as.numeric(predicao)
    
    # a linha abaixo cria uma matriz chamada Pred de valores 0
    # com a quantidade de colunas igual ao nÃºmero de classes e linhas
    # igual o nÃºmero de exemplos.
    Pred <-
      matrix(0, ncol = length(unique(teste[, index_classe])), nrow = nrow(teste))
    
    
    # para cada valor instÃ¢ncia na tabela de prediÃ§Ã£o, atribui-se 1 Ã  cÃ©lula
    # que na sua coluna eh a classe atribuida.
    for (i in 1:nrow(Pred)) {
      Pred[i, tabela_predicao[i]] = 1
    }
    
    # F.score and other metrics.
    colnames(Pred) <-
      colnames(Targets) <- paste("Classe ", unique(teste[, index_classe]))
    
    medidas <- F.measure.single.over.classes(Pred, Targets)
    
    # AUC
    auc <- AUC.single.over.classes(Targets, Pred)
    cat(
      nome_dataset,
      auc$average,
      matriz_de_confuncao$overall['Accuracy'],
      matriz_de_confuncao$overall['Kappa'],
      medidas$average['P'],
      medidas$average['R'],
      medidas$average['S'],
      medidas$average['F'],
      "\n",
      sep = ";"
    )
  }

#registerDoMC(cores = 8)

# definindo a notaÃ§Ã£o numerica
options("scipen" = 100, "digits" = 4)

# local RESULTADOS
file_resultados_c50 <-
  paste(
    "c:/Users/Angelo/Documents/Escrita Mortalidade Infantil/dados_obitos_nascimentos/Resultados/resultados_C50_SMOTED_IG.csv",
    sep = ""
  )

#file_resultados_c50 <- paste("/home/angelo/Dropbox/Doutorado/Aulas/2019 - Mineração de Dados/Trabalho/Experimento com Mortalidade Infantil/Experimento com Mortalidade Infantil/dados_obitos_nascimentos/processados/final_resultados_c50.csv",sep="")

sink(file_resultados_c50, append = FALSE)
cat(
  "Dataset",
  "Ãrea AUC",
  "AcurÃ¡cia",
  "Kaapa",
  "Precision",
  "Recall",
  "Specificity",
  "F-measure",
  "\n",
  sep = ";"
)
sink()


dataset_name <- "ds_criancas"

# WINDOWS
# REPARA NO FACTOR!
#ds_new <- read.csv("C:/Users/Angelo/Documents/Escrita Mortalidade Infantil/dados_obitos_nascimentos/processados/final.csv", header=T,stringsAsFactors=FALSE)
ds_new <-
  read.csv(
    "C:/Users/Angelo/Documents/Escrita Mortalidade Infantil/dados_obitos_nascimentos/processados/finalV6.csv",
    header = T
  )

# LINUX
#ds_new <- read.csv("/home/angelo/Dropbox/Doutorado/Aulas/2019 - Mineração de Dados/Trabalho/Experimento com Mortalidade Infantil/Experimento com Mortalidade Infantil/dados_obitos_nascimentos/processados/finalV6", header=T)
# Removendo atributos desnecessários

# Colocando a classe como factor para atender a necessidadedo classificador.
ds_new$SITUACAO <- factor(ds_new$SITUACAO)

# Removendo Features desnecessarias
ds_new <- ds_new[, -grep("NUMERODN", colnames(ds_new))] # NumeroDN
ds_new <- ds_new[, -grep("CODMUNRES", colnames(ds_new))] # CODMUNRES
ds_new <-ds_new[, -grep("CODMUNNATU", colnames(ds_new))] # CODMUNnatu
ds_new <- ds_new[, -grep("DTNASC", colnames(ds_new))] # DTNASC
ds_new <- ds_new[, -grep("LOCNASC", colnames(ds_new))] # LOCNAS
# ds_new <- ds_new[,-grep("IDADEMAE", colnames(ds_new))] # IDADEMAE

########################## LAÇO PRINCIPAL ##########################
dataset <- ds_new
k <- 1
repeticoes <- 1
peso_minimo <- 350


# Taxa mínima para seleção das melhores features.
min_best <- 0.03

while (k <= repeticoes) {
  index_classe <- ncol(dataset)
  ########################## Separando Treino e Teste ##########################
  cat("Repeticao ", k, "/", repeticoes, "\n")
  cat("Separando..")
  intrain <-
    createDataPartition(dataset[, index_classe], p = 7 / 10, list = FALSE)
  train_data = dataset[intrain, ]
  teste = dataset[-intrain, ]
  cat("OK!\n")
  
  
  ########################## KDD ON TRAIN ##########################
  
  # cat("detectando outliers")
  # for(y in 1:ncol(ds_new)){
  #   rosnerTest(ds_new[,y], k = 4, warn = T)
  # }
  # cat("OK...")
  
  ########################## PESO ##########################
  # O trecho de código acima identificou que algumas criancas com PESO
  # menor que 350 viveram. Isto é uma falha do RIGOR!
  cat("Corrigindo PESO...")
  y <- 1
  contador_peso <- 0;
  col_peso <- grep("PESO", colnames(train_data))
  while (y <= nrow(train_data))
  {
    if (!is.na(train_data[y, col_peso]) &&
        train_data[y, col_peso] <= peso_minimo)
    {
      cat(y, ", ")
      train_data[y, ncol(train_data)] <- "morreu"
      contador_peso <- contador_peso + 1
    }
    y <- y + 1
  }
  cat("OK! Identificadas", contador_peso,"criancas com peso abaixo de ",peso_minimo,"\n")
  
  ########################## APGAR1 ##########################
  # A detecção de outlier ainda verificou um apagar1 sendo 99
  cat("Corrigindo APGAR1.. ")
  y <- 1
  col_APGAR <- grep("APGAR1", colnames(train_data))
  while (y <= nrow(train_data))
  {
    if (!is.na(train_data[y, col_APGAR]) &&
        train_data[y, col_APGAR] == "99")
    {
      train_data[y, col_APGAR] <- 9
      cat(y, ", ")
      #Sys.sleep(1000)
    }
    y <- y + 1
  }
  cat("OK!\n")
  
  ########################## CONSPRENAT ##########################
  # Foi verificado que 39 mães tiveram 99 consultas pré-natais, impossível.
  cat("Corrigindo CONSPRENAT... ")
  col_CONSPRENAT <- grep("CONSPRENAT", colnames(train_data))
  y <- 1
  cat("Encontrado em y: ")
  while (y <= nrow(train_data))
  {
    if (!is.na(train_data[y, col_CONSPRENAT]) &&
        train_data[y, col_CONSPRENAT] == "99")
    {
      train_data[y, col_CONSPRENAT] <-
        mean(train_data[, col_CONSPRENAT], na.rm = TRUE)
      cat(y, ", ")
    }
    y <- y + 1
  }
  cat("OK!\n")
  
  ########################## QTDGESTANT ##########################
  # We also looked at the QTDGESTANT feature of a 34-year-old mother who had 95
  #previous pregnancies, an easy-to-remove outlier.
  cat("Corrigindo QTDGESTANT. ")
  col_QTDGESTANT <- grep("QTDGESTANT", colnames(train_data))
  y <- 1
  cat("Encontrado em y: ")
  while (y <= nrow(train_data))
  {
    if (!is.na(train_data[y, col_QTDGESTANT]) &&
        train_data[y, col_QTDGESTANT] == "99")
    {
      train_data[y, col_QTDGESTANT] <-
        mean(train_data[, col_QTDGESTANT], na.rm = TRUE)
      cat(y, ", ")
    }
    y <- y + 1
  }
  cat("OK!\n")
  
  #Sys.sleep(1000)
  
  ########################## NAs por Média da feature ##########################
  cat("Substituindo NAs pela média...")
  for (y in 1:ncol(train_data)) {
    if (is.numeric(train_data[1, y])) {
      train_data[is.na(train_data[, y]), y] <-
        mean(train_data[, y], na.rm = TRUE)
    }
  }
  cat("OK!\n")
  
  
  ########################## FIM KDD ##########################
  
  
  
  ################## IDENTIFICANDO AS MELHORES FEATURES ##################
  # O cOdigo abaixo foi utilizado apenas uma vez para descobrir quais  #
  # features s?o importantes.
  
  cat("Identificando melhores Features com IG...")
  IG_weights <- gain.ratio(SITUACAO ~ ., data = train_data)
  subset_of_features_from_IG <- cutoff.k.percent(IG_weights, 0.42)
  IG <- matrix(data = -1,
               ncol = 2,
               nrow = nrow(IG_weights))
  colnames(IG) <- c("atributo", "ganho")
  IG[, 1] <- rownames(IG_weights)
  IG[, 2] <- IG_weights$attr_importance[]
  ord <- order(IG[, 2], decreasing = TRUE)
  IG_ord <- IG[ord, ]
  #View(IG_ord)
  cat("OK!\n")
  #Sys.sleep(111)
  
  ################## SELECIONANDO NO DATASET DE TREINO SOMENTE AS BEST FEATURES ##################
  cat("Selecionando as melhores features no conjunto de treino...")
  qtd_features <- nrow(IG_ord)
  best_features <- c()
  cont <- 1
  while (cont <= qtd_features) {
    if (IG_ord[cont, 2] > min_best) {
      best_features <- rbind(best_features, IG_ord[cont, 1])
    }
    cont <- cont + 1
  }
  
  cat("Removendo feature: ")
  contador_treino <- 1
  feature_to_remove <- 0
  while (contador_treino <= ncol(train_data) - 1) {
    contador_best_features <- 1
    achou <- FALSE
    while (contador_best_features <= length(best_features)) {
      if (colnames(train_data)[contador_treino] == best_features[contador_best_features]) {
        achou <- TRUE
        #Sys.sleep(10)
      }
      contador_best_features <- contador_best_features + 1
    }
    if (achou == FALSE)
    {
      cat(colnames(train_data)[contador_treino], ", ")
      train_data <- train_data[, -contador_treino]
      contador_treino <- 1
    }
    #Sys.sleep(10)
    contador_treino <-  contador_treino + 1
  }
  cat("OK!\n")
  
  
  
  ################## SELECIONANDO NO DATASET DE TESTE SOMENTE AS BEST FEATURES ##################
  cat("Selecionando as melhores features no conjunto de teste...")
  cat("Removendo feature: ")
  contador_treino <- 1
  feature_to_remove <- 0
  while (contador_treino <= ncol(teste) - 1) {
    contador_best_features <- 1
    achou <- FALSE
    while (contador_best_features <= length(best_features)) {
      if (colnames(teste)[contador_treino] == best_features[contador_best_features]) {
        achou <- TRUE
        #Sys.sleep(10)
      }
      contador_best_features <- contador_best_features + 1
    }
    if (achou == FALSE)
    {
      cat(colnames(teste)[contador_treino], ", ")
      teste <- teste[, -contador_treino]
      contador_treino <- 1
    }
    #Sys.sleep(10)
    contador_treino <-  contador_treino + 1
  }
  cat("OK!\n")
  
  #Sys.sleep(10)
  ########################## NORMALIZAÇÃO ##########################
  # Abaixo o código que normaliza os dados quantitativos.
  # Eu testei e verifiquei que ficou pior quando normalizado.
  
  # cat("Normalizando...")
  # temp<-1
  # while(temp<=ncol(train_data))
  # {
  #   if(is.numeric(train_data[1,temp]))
  #   {
  #     train_data[,temp] <- scale(train_data[,temp])
  #   }
  #
  #   temp<-temp+1
  # }
  # cat("OK!\n")
  #Sys.sleep(1000)
  
  
  
  ########################## SMOTING ##########################
  # Aumente a acurácia e o kappa depois que mechi na proporção.
  # ove under kap
  #5000 60 .28
  #4000 70 .35
  cat("Somting...")
  train_smoted <-
    SMOTE(
      SITUACAO ~ .,
      data = train_data,
      perc.over = 4000,
      perc.under = 70,
      k = 10
    )
  # Abaixo o comando para ver a distribuição de probabilides
  #prop.table(table(smoted_data$targetclass))
  # returns 0.5  0.5
  cat("OK!\n")
  
  
  cat("dormii")
  Sys.sleep(100000)
  
  # Rremova o comentário para serem usados os DS em outro programa.
  #write.csv(x = teste, row.names=FALSE, file = "/home/angelo/Dropbox/Doutorado/Aulas/2019 - Mineração de Dados/Trabalho/Experimento com Mortalidade Infantil/Experimento com Mortalidade Infantil/dados_obitos_nascimentos/processados/teste_ds_smothed.csv")
  #write.csv(x = train_smoted, row.names=FALSE, file = "/home/angelo/Dropbox/Doutorado/Aulas/2019 - Mineração de Dados/Trabalho/Experimento com Mortalidade Infantil/Experimento com Mortalidade Infantil/dados_obitos_nascimentos/processados/train_ds_smothed.csv")
  
  # abaixo sem a classe
  
  
  ######################## MODELO CLASSIFICADOR C50 ###################
  # Caso tente usar o SVM,
  # - utilize apenas features núméricas!!!!
  # - a classe deve ser do tipo inteira, nada de por vivo ou morto!!
  # - tente colocar a classe na primeira coluna!
  
  x <- train_smoted[, -ncol(train_smoted)]
  fitControl <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 3,
    returnResamp = "all",
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )
  y <- train_smoted$SITUACAO
  grid <-
    expand.grid(
      .winnow = c(TRUE, FALSE),
      .trials = c(1, 5, 10, 15, 20),
      .model = "tree"
    )
  cat("Gerando o modelo C50...")
  set.seed(sample(1:999, 1))
  
  # PARALELISMO
  # Nao use todos os kernels!
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  mdl <-
    train(
      x = x,
      y = y,
      tuneGrid = grid,
      trControl = fitControl,
      method = "C5.0",
      verbose = FALSE,
      metric = "ROC"
    )
  
  ######## OUTRO ALGORITMO DE CLASSIFIA??O BASEADO EM ?RVORE DE DECIS??O ##############
  # cat("Gerando o modelo RPART...")
  # trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
  # mdl<- train(x=x,y=y,method = "rpart",
  #             parms = list(split = "information"),
  #             trControl=trctrl,
  #             tuneLength = 10)
  stopCluster(cl)
  
  ##### plot tree ####
  #cat(mdl$finalModel$output) ##### plot tree ####
  cat("OK!\n")
  
  index_classe <- ncol(teste)
  predicao <- predict(mdl, teste[, -index_classe])
  
  # visualize the resample distributions
  #xyplot(mdl,type = c("g", "p", "smooth"))
  
  cat("Escrevendo resultados...")
  sink(file_resultados_c50, append = TRUE)
  processaResultados(predicao = predicao,
                     test = teste,
                     dataset_name,
                     index_classe)
  sink()
  cat("OK!\n")
  k <- k + 1
}
