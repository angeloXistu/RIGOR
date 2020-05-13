library(stringr)
#library(doParallel)
#require(doMC)
#registerDoMC(cores=8)

remove_falecido <- function(ds, NUMERODO){
  indice <- which(ds[,12] == NUMERODO)
  cat("Encontrado morto na linha ", indice, "\n")
  return (indice)
}



# LINUX
#file_nascimentos <- "/home/angelo/Dropbox/Doutorado/Aulas/2019 - MineraA§A£o de Dados/Trabalho/Experimento com Mortalidade Infantil/Dados_obitos_nascimentos/processados/SINASC-unidos/dados_nasc_unidos"
#file_mortos <- "/home/angelo/Dropbox/Doutorado/Aulas/2019 - MineraA§A£o de Dados/Trabalho/Experimento com Mortalidade Infantil/Dados_obitos_nascimentos/processados/SIM-unidos/dados_mortalidade_unidos"

# Windows
file_nascimentos <- "c:/Users/Angelo/Documents/Escrita Mortalidade Infantil/dados_obitos_nascimentos/processados/SINASC-unidos/dados_nasc_unidos.csv"
file_mortos <- "c:/Users/Angelo/Documents/Escrita Mortalidade Infantil/dados_obitos_nascimentos/processados/SIM-unidos/dados_mortalidade_unidos.csv"
ds_mortos <- read.csv(file_mortos)
ds_nascimentos <- read.csv(file_nascimentos)


ds_nascimentos$SEXO <- str_replace_all(ds_nascimentos$SEXO, 'F', '2')
ds_nascimentos$SEXO <- str_replace_all(ds_nascimentos$SEXO, 'M', '1')

contador_encontrados_por_features <- 0
contador_encontrados_por_dn <- 0
contador_encontrados_rigor_90 <- 0
contador_encontrados_rigor_80 <- 0
contador_encontrados_rigor_70 <- 0
contador_encontrados_rigor_60 <- 0
contador_encontrados_rigor_50 <- 0
contador_encontrados_rigor_40 <- 0
contador_encontrados_rigor_30 <- 0
contador_encontrados_rigor_30_V2 <- 0
contador_encontrados_rigor_30_V3 <- 0
contador_encontrados_rigor_10 <- 0

# colocando o head nas colunas.
ds <- as.data.frame(x = NULL)
ds <- rbind(ds, ds_nascimentos[1,])
ds <- cbind(ds, NA)
colnames(ds)[ncol(ds)] <- "SITUACAO"
ds <- ds[-1, ]

# # LOOP PRINCIPAL para procurar por DN
i <- 1
k <- 1 # contador do "ds"
while (i <= nrow(ds_nascimentos))
{
  #cat("nrow(ds_nascimentos) i: ", i,"/", nrow(ds_nascimentos), "nrow(ds_mortos): ", nrow(ds_mortos), "\n")
  # if(nrow(ds_mortos) == 0)
  #   return()
  j <- 1
  while(j <= nrow(ds_mortos)) # procurando O FALECIDO por NUMERO DE NASCIMENTO
  {

    if(!is.na(ds_mortos[j,1])) {
      numerodn <- ds_mortos[j,1]
      if (numerodn == ds_nascimentos[i,1])
      {
        ds[k,1:31] <- ds_nascimentos[i,1:31]
        ds[k,32] <- "morreu"
        cat("contador_encontrados_por_dn:",contador_encontrados_por_dn,"\n")
        contador_encontrados_por_dn <- contador_encontrados_por_dn + 1
        ds_mortos <- ds_mortos[-j, ]  # removendo a linha já encontrada.
        ds_nascimentos <- ds_nascimentos[-i, ]  # removendo a linha já encontrada.
        k<-k+1
      }
    }
    j <- j + 1
  }
  i<-i+1
  
}

# LOOP PRINCIPAL para procurar com RIGOR!

i <- 1
while (i <= nrow(ds_nascimentos))
{
  cat("\n")
  cat("nrow(ds_nascimentos) i: ", i,"/", nrow(ds_nascimentos), "\n")
  cat("nrow(ds_mortos): ", nrow(ds_mortos), " ")
  # if(nrow(ds_mortos) == 0)
  #   return()
  achou <- FALSE
  
  # verificando se já o registro tem NUMERODN
  if(!is.na(ds_mortos[i,1])) {
    numerodn <- ds_mortos[i,1]
    if (numerodn == ds_nascimentos[i,1])
    {
      
    }
  }
  
  IDADEMAE_nasc <- ds_nascimentos[i,grep("IDADEMAE", colnames(ds_nascimentos))]#
  SEXO_nasc <- ds_nascimentos[i,grep("SEXO", colnames(ds_nascimentos))]#
  PESO_nasc <- ds_nascimentos[i,grep("PESO", colnames(ds_nascimentos))]#
  DTNASC_nasc <- ds_nascimentos[i,grep("DTNASC", colnames(ds_nascimentos))]#
  CODMUNNATU_nasc <- ds_nascimentos[i,grep("CODMUNNATU", colnames(ds_nascimentos))]#
  CODMUNRES_nasc <- ds_nascimentos[i,grep("CODMUNRES", colnames(ds_nascimentos))]#
  RACACOR_nasc <- ds_nascimentos[i, 21]#
  GESTACAO_nasc <- ds_nascimentos[i,grep("GESTACAO", colnames(ds_nascimentos))]#
  GRAVIDEZ_nasc <- ds_nascimentos[i,grep("GRAVIDEZ", colnames(ds_nascimentos))]#
  PARTO_nasc <- ds_nascimentos[i,6]#

  # UTILIZANDO RIGOR 90%, OU SEJA, AS OUTRAS CARACTERÍSTICAS TEM Q SER 90% COMPATÍVEIS
  
  if(achou == FALSE){ # procurando O FALECIDO ARAVAS DAS OUTRAS CARACTERASTICAS
    rigor <- 90
    if(is.na(CODMUNNATU_nasc) #
       || is.na(PESO_nasc) #
       || is.na(SEXO_nasc) #
       || is.na(DTNASC_nasc) #
       || is.na(CODMUNRES_nasc)#
       || is.na(IDADEMAE_nasc)
       || is.na(RACACOR_nasc)
       || is.na(GESTACAO_nasc)
       || is.na(GRAVIDEZ_nasc)
       || is.na(PARTO_nasc)){
      #cat("Dados inválidos... pulando\n")
    }
    else{
      ds_temp <- ds_mortos[,2]
      ds_temp <- cbind(ds_temp, ds_mortos[,3])
      ds_temp <- cbind(ds_temp, ds_mortos[,4])
      ds_temp <- cbind(ds_temp, ds_mortos[,5])
      ds_temp <- cbind(ds_temp, ds_mortos[,6])
      ds_temp <- cbind(ds_temp, ds_mortos[,7])
      ds_temp <- cbind(ds_temp, ds_mortos[,8])
      ds_temp <- cbind(ds_temp, ds_mortos[,9])
      ds_temp <- cbind(ds_temp, ds_mortos[,10])
      ds_temp <- cbind(ds_temp, ds_mortos[,11])
      ds_temp <- cbind(ds_temp, ds_mortos[,12])
      ds_temp <- ds_temp[complete.cases(ds_temp), ]
      
      # DEBUG
      #View(ds_temp)
      #View(ds_mortos)
      #Sys.sleep(10000)
      
      j <- 1    
      while(j <= nrow(ds_temp) && achou == FALSE){
        DTNASC_m <- ds_temp[j,1]#
        SEXO_m <- ds_temp[j,2]#
        CODMUNNATU_m <- ds_temp[j,3]#
        CODMUNRES_m <- ds_temp[j,4]#
        IDADEMAE_m <- ds_temp[j, 5]#
        RACACOR_m <- ds_temp[j,6]
        GESTACAO_m <- ds_temp[j,7]
        GRAVIDEZ_m <- ds_temp[j,8]
        PARTO_m <- ds_temp[j, 9]
        PESO_m <- ds_temp[j,10]#
        if ((CODMUNRES_m == CODMUNRES_nasc) 
            && (DTNASC_m == DTNASC_nasc) 
            && (SEXO_m == SEXO_nasc)
            && (CODMUNNATU_m == CODMUNNATU_nasc) 
            && (PESO_m == PESO_nasc)
            && (IDADEMAE_m == IDADEMAE_nasc)
            && (RACACOR_m == RACACOR_nasc)
            && (GESTACAO_m == GESTACAO_nasc)
            && (GRAVIDEZ_m == GRAVIDEZ_nasc)
            && (PARTO_m == PARTO_nasc)
        ){
          ds[k,1:31] <- ds_nascimentos[i,1:31]
          ds[k,32] <- "morreu"
          achou <- TRUE
          cat("achou!\n")
          index <- remove_falecido(ds_mortos, ds_temp[j,11])
          ds_mortos <- ds_mortos[-index,]
          contador_encontrados_por_features <- contador_encontrados_por_features + 1
          contador_encontrados_rigor_90 <- contador_encontrados_rigor_90 + 1
        }
        j <- j + 1
      }
    }
  }
  
  # UTILIZANDO RIGOR 80%, OU SEJA, AS OUTRAS CARACTERÍSTICAS TEM Q SER 90% COMPATÍVEIS
  
  if(achou == FALSE){ # procurando O FALECIDO ARAVAS DAS OUTRAS CARACTERASTICAS
    rigor <- 80
    if(is.na(CODMUNNATU_nasc) #
       || is.na(PESO_nasc) #
       || is.na(SEXO_nasc) #
       || is.na(DTNASC_nasc) #
       || is.na(CODMUNRES_nasc)#
       || is.na(IDADEMAE_nasc)
       #|| is.na(RACACOR_nasc)
       || is.na(GESTACAO_nasc)
       || is.na(GRAVIDEZ_nasc)
       || is.na(PARTO_nasc)){
      #cat("Dados inválidos... pulando\n")
    }
    else{
      
      ds_temp <- ds_mortos[,2]
      ds_temp <- cbind(ds_temp, ds_mortos[,3])
      ds_temp <- cbind(ds_temp, ds_mortos[,4])
      ds_temp <- cbind(ds_temp, ds_mortos[,5])
      ds_temp <- cbind(ds_temp, ds_mortos[,6])
      #ds_temp <- cbind(ds_temp, ds_mortos[,7])
      ds_temp <- cbind(ds_temp, ds_mortos[,8])
      ds_temp <- cbind(ds_temp, ds_mortos[,9])
      ds_temp <- cbind(ds_temp, ds_mortos[,10])
      ds_temp <- cbind(ds_temp, ds_mortos[,11])
      ds_temp <- cbind(ds_temp, ds_mortos[,12])
      ds_temp <- ds_temp[complete.cases(ds_temp), ]
      
      
      j <- 1    
      while(j <= nrow(ds_temp) && achou == FALSE){
        DTNASC_m <- ds_temp[j,1]#
        SEXO_m <- ds_temp[j,2]#
        CODMUNNATU_m <- ds_temp[j,3]#
        CODMUNRES_m <- ds_temp[j,4]#
        IDADEMAE_m <- ds_temp[j, 5]#
        #RACACOR_m <- ds_temp[j,6]
        GESTACAO_m <- ds_temp[j,6]
        GRAVIDEZ_m <- ds_temp[j,7]
        PARTO_m <- ds_temp[j, 8]
        PESO_m <- ds_temp[j,9]#
        if ((CODMUNRES_m == CODMUNRES_nasc) 
            && (DTNASC_m == DTNASC_nasc) 
            && (SEXO_m == SEXO_nasc)
            && (CODMUNNATU_m == CODMUNNATU_nasc) 
            && (PESO_m == PESO_nasc)
            && (IDADEMAE_m == IDADEMAE_nasc)
            && (GESTACAO_m == GESTACAO_nasc)
            && (GRAVIDEZ_m == GRAVIDEZ_nasc)
            && (PARTO_m == PARTO_nasc)
        ){
          ds[k,1:31] <- ds_nascimentos[i,1:31]
          ds[k,32] <- "morreu"
          achou <- TRUE
          cat("achou!\n")
          index <- remove_falecido(ds_mortos, ds_temp[j,10])
          ds_mortos <- ds_mortos[-index,]
          contador_encontrados_por_features <- contador_encontrados_por_features + 1
          contador_encontrados_rigor_80 <- contador_encontrados_rigor_80 + 1
        }
        j <- j + 1
      }
    }
  }
  
  # UTILIZANDO RIGOR 70%, OU SEJA, AS OUTRAS CARACTERÍSTICAS TEM Q SER 90% COMPATÍVEIS
  
  if(achou == FALSE){ # procurando O FALECIDO ARAVAS DAS OUTRAS CARACTERASTICAS
    rigor <- 70
    if(is.na(CODMUNNATU_nasc) #
       || is.na(PESO_nasc) #
       || is.na(SEXO_nasc) #
       || is.na(DTNASC_nasc) #
       || is.na(CODMUNRES_nasc)#
       #|| is.na(IDADEMAE_nasc)
       #|| is.na(RACACOR_nasc)
       || is.na(GESTACAO_nasc)
       || is.na(GRAVIDEZ_nasc)
       || is.na(PARTO_nasc)){
      #cat("Dados inválidos... pulando\n")
    }
    else{
      ds_temp <- ds_mortos[,2]
      ds_temp <- cbind(ds_temp, ds_mortos[,3])
      ds_temp <- cbind(ds_temp, ds_mortos[,4])
      ds_temp <- cbind(ds_temp, ds_mortos[,5])
      #ds_temp <- cbind(ds_temp, ds_mortos[,6])
      #ds_temp <- cbind(ds_temp, ds_mortos[,7])
      ds_temp <- cbind(ds_temp, ds_mortos[,8])
      ds_temp <- cbind(ds_temp, ds_mortos[,9])
      ds_temp <- cbind(ds_temp, ds_mortos[,10])
      ds_temp <- cbind(ds_temp, ds_mortos[,11])
      ds_temp <- cbind(ds_temp, ds_mortos[,12])
      ds_temp <- ds_temp[complete.cases(ds_temp), ]
      
      
      j <- 1    
      while(j <= nrow(ds_temp) && achou == FALSE){
        DTNASC_m <- ds_temp[j,1]#
        SEXO_m <- ds_temp[j,2]#
        CODMUNNATU_m <- ds_temp[j,3]#
        CODMUNRES_m <- ds_temp[j,4]#
        #IDADEMAE_m <- ds_temp[j, 5]#
        #RACACOR_m <- ds_temp[j,6]
        GESTACAO_m <- ds_temp[j,5]
        GRAVIDEZ_m <- ds_temp[j,6]
        PARTO_m <- ds_temp[j, 7]
        PESO_m <- ds_temp[j,8]#
        if ((CODMUNRES_m == CODMUNRES_nasc) 
            && (DTNASC_m == DTNASC_nasc) 
            && (SEXO_m == SEXO_nasc)
            && (CODMUNNATU_m == CODMUNNATU_nasc) 
            && (PESO_m == PESO_nasc)
            && (GESTACAO_m == GESTACAO_nasc)
            && (GRAVIDEZ_m == GRAVIDEZ_nasc)
            && (PARTO_m == PARTO_nasc)
        ){
          ds[k,1:31] <- ds_nascimentos[i,1:31]
          ds[k,32] <- "morreu"
          achou <- TRUE
          cat("achou!\n")
          index <- remove_falecido(ds_mortos, ds_temp[j,9])
          ds_mortos <- ds_mortos[-index,]
          contador_encontrados_por_features <- contador_encontrados_por_features + 1
          contador_encontrados_rigor_70 <- contador_encontrados_rigor_70 + 1
        }
        j <- j + 1
      }
    }
  }
  
  
  # UTILIZANDO RIGOR 60%, OU SEJA, AS OUTRAS CARACTERÍSTICAS TEM Q SER 90% COMPATÍVEIS
  
  if(achou == FALSE){ # procurando O FALECIDO ARAVAS DAS OUTRAS CARACTERASTICAS
    rigor <- 60
    if(is.na(CODMUNNATU_nasc) #
       || is.na(PESO_nasc) #
       || is.na(SEXO_nasc) #
       || is.na(DTNASC_nasc) #
       || is.na(CODMUNRES_nasc)#
       #|| is.na(IDADEMAE_nasc)
       #|| is.na(RACACOR_nasc)
       #|| is.na(GESTACAO_nasc)
       || is.na(GRAVIDEZ_nasc)
       || is.na(PARTO_nasc)){
      #cat("Dados inválidos... pulando\n")
    }
    else{
      ds_temp <- ds_mortos[,2]
      ds_temp <- cbind(ds_temp, ds_mortos[,3]) 
      ds_temp <- cbind(ds_temp, ds_mortos[,4])
      ds_temp <- cbind(ds_temp, ds_mortos[,5])
      #ds_temp <- cbind(ds_temp, ds_mortos[,6])
      #ds_temp <- cbind(ds_temp, ds_mortos[,7])
      #ds_temp <- cbind(ds_temp, ds_mortos[,8])
      ds_temp <- cbind(ds_temp, ds_mortos[,9])
      ds_temp <- cbind(ds_temp, ds_mortos[,10])
      ds_temp <- cbind(ds_temp, ds_mortos[,11])
      ds_temp <- cbind(ds_temp, ds_mortos[,12])
      ds_temp <- ds_temp[complete.cases(ds_temp), ]
      
      j <- 1    
      while(j <= nrow(ds_temp) && achou == FALSE){
        DTNASC_m <- ds_temp[j,1]#
        SEXO_m <- ds_temp[j,2]#
        CODMUNNATU_m <- ds_temp[j,3]#
        CODMUNRES_m <- ds_temp[j,4]#
        #IDADEMAE_m <- ds_temp[j, 5]#
        #RACACOR_m <- ds_temp[j,6]
        #GESTACAO_m <- ds_temp[j,5]
        GRAVIDEZ_m <- ds_temp[j,5]
        PARTO_m <- ds_temp[j, 6]
        PESO_m <- ds_temp[j,7]#
        
        
        
        #cat(CODMUNRES_nasc,DTNASC_nasc,SEXO_nasc,CODMUNNATU_nasc,PESO_nasc,GRAVIDEZ_nasc,PARTO_nasc, "\n")
        #cat(CODMUNRES_m,DTNASC_m,SEXO_m,CODMUNNATU_m,PESO_m,GRAVIDEZ_m,PARTO_m, "\n")
        
        if ((CODMUNRES_m == CODMUNRES_nasc) 
            && (DTNASC_m == DTNASC_nasc) 
            && (SEXO_m == SEXO_nasc)
            && (CODMUNNATU_m == CODMUNNATU_nasc) 
            && (PESO_m == PESO_nasc)
            && (GRAVIDEZ_m == GRAVIDEZ_nasc)
            && (PARTO_m == PARTO_nasc)
        ){
          ds[k,1:31] <- ds_nascimentos[i,1:31]
          ds[k,32] <- "morreu"
          achou <- TRUE
          index <- remove_falecido(ds_mortos, ds_temp[j,8])
          ds_mortos <- ds_mortos[-index,]
          contador_encontrados_por_features <- contador_encontrados_por_features + 1
          contador_encontrados_rigor_60 <- contador_encontrados_rigor_60 + 1
        }
        j <- j + 1
      }
    }
  }
  
  # UTILIZANDO RIGOR 50%, OU SEJA, AS OUTRAS CARACTERÍSTICAS TEM Q SER 50% COMPATÍVEIS
  
  if(achou == FALSE){ # procurando O FALECIDO ARAVAS DAS OUTRAS CARACTERASTICAS
    rigor <- 50
    if(is.na(CODMUNNATU_nasc) #
       #|| is.na(PESO_nasc) #
       || is.na(SEXO_nasc) #
       || is.na(DTNASC_nasc) #
       || is.na(CODMUNRES_nasc)#
       #|| is.na(IDADEMAE_nasc)
       #|| is.na(RACACOR_nasc)
       #|| is.na(GESTACAO_nasc)
       || is.na(GRAVIDEZ_nasc)
       || is.na(PARTO_nasc)){
      #cat("Dados inválidos... pulando\n")
    }
    else{
      ds_temp <- ds_mortos[,2]
      ds_temp <- cbind(ds_temp, ds_mortos[,3])
      ds_temp <- cbind(ds_temp, ds_mortos[,4])
      ds_temp <- cbind(ds_temp, ds_mortos[,5])
      #ds_temp <- cbind(ds_temp, ds_mortos[,6])
      #ds_temp <- cbind(ds_temp, ds_mortos[,7])
      #ds_temp <- cbind(ds_temp, ds_mortos[,8])
      ds_temp <- cbind(ds_temp, ds_mortos[,9])
      ds_temp <- cbind(ds_temp, ds_mortos[,10])
      #ds_temp <- cbind(ds_temp, ds_mortos[,11])
      ds_temp <- cbind(ds_temp, ds_mortos[,12])
      ds_temp <- ds_temp[complete.cases(ds_temp), ]
      
      
      
      
      j <- 1    
      while(j <= nrow(ds_temp) && achou == FALSE){
        DTNASC_m <- ds_temp[j,1]#
        SEXO_m <- ds_temp[j,2]#
        CODMUNNATU_m <- ds_temp[j,3]#
        CODMUNRES_m <- ds_temp[j,4]#
        #IDADEMAE_m <- ds_temp[j, 5]#
        #RACACOR_m <- ds_temp[j,6]
        #GESTACAO_m <- ds_temp[j,5]
        GRAVIDEZ_m <- ds_temp[j,5]
        PARTO_m <- ds_temp[j, 6]
        #PESO_m <- ds_temp[j,7]#
        if ((CODMUNRES_m == CODMUNRES_nasc) 
            && (DTNASC_m == DTNASC_nasc) 
            && (SEXO_m == SEXO_nasc)
            && (CODMUNNATU_m == CODMUNNATU_nasc) 
            && (GRAVIDEZ_m == GRAVIDEZ_nasc)
            && (PARTO_m == PARTO_nasc)
        ){
          
          ds[k,1:31] <- ds_nascimentos[i,1:31]
          ds[k,32] <- "morreu"
          achou <- TRUE
          cat("achou!\n")
          index <- remove_falecido(ds_mortos, ds_temp[j,7])
          ds_mortos <- ds_mortos[-index,]
          contador_encontrados_por_features <- contador_encontrados_por_features + 1
          contador_encontrados_rigor_50 <- contador_encontrados_rigor_50 + 1
        }
        j <- j + 1
      }
    }
  }
  
  # A diferença para o 3.0 para o 3.1 é que estou afinando mais o 40%.
  # o Objetivo é aumentar o número de encontrados mortos.
  
  # UTILIZANDO RIGOR 40%, OU SEJA, AS OUTRAS CARACTERÍSTICAS TEM Q SER 40% COMPATÍVEIS
  
  if(achou == FALSE){ # procurando O FALECIDO ARAVAS DAS OUTRAS CARACTERASTICAS
    rigor <- 40
    if(is.na(CODMUNNATU_nasc) #
       #|| is.na(PESO_nasc) #
       || is.na(SEXO_nasc) #
       || is.na(DTNASC_nasc) #
       || is.na(CODMUNRES_nasc)#
       || is.na(IDADEMAE_nasc)
       #|| is.na(RACACOR_nasc)
       #|| is.na(GESTACAO_nasc)
       #|| is.na(GRAVIDEZ_nasc)
       #|| is.na(PARTO_nasc)
    ){
      #cat("Dados inválidos... pulando\n")
    }
    else{
      ds_temp <- ds_mortos[,2]
      ds_temp <- cbind(ds_temp, ds_mortos[,3])
      ds_temp <- cbind(ds_temp, ds_mortos[,4])
      ds_temp <- cbind(ds_temp, ds_mortos[,5])
      ds_temp <- cbind(ds_temp, ds_mortos[,6])
      #ds_temp <- cbind(ds_temp, ds_mortos[,7])
      #ds_temp <- cbind(ds_temp, ds_mortos[,8])
      #ds_temp <- cbind(ds_temp, ds_mortos[,9])
      #ds_temp <- cbind(ds_temp, ds_mortos[,10])
      #ds_temp <- cbind(ds_temp, ds_mortos[,11])
      ds_temp <- cbind(ds_temp, ds_mortos[,12])
      ds_temp <- ds_temp[complete.cases(ds_temp), ]
      
      
      j <- 1    
      while(j <= nrow(ds_temp) && achou == FALSE){
        DTNASC_m <- ds_temp[j,1]#
        SEXO_m <- ds_temp[j,2]#
        CODMUNNATU_m <- ds_temp[j,3]#
        CODMUNRES_m <- ds_temp[j,4]#
        IDADEMAE_m <- ds_temp[j, 5]#
        #RACACOR_m <- ds_temp[j,6]
        #GESTACAO_m <- ds_temp[j,5]
        #GRAVIDEZ_m <- ds_temp[j,5]
        #PARTO_m <- ds_temp[j, 6]
        #PESO_m <- ds_temp[j,7]#
        if ((CODMUNRES_m == CODMUNRES_nasc) 
            && (DTNASC_m == DTNASC_nasc) 
            && (SEXO_m == SEXO_nasc)
            && (CODMUNNATU_m == CODMUNNATU_nasc) 
            && (IDADEMAE_m == IDADEMAE_nasc || IDADEMAE_m == IDADEMAE_nasc + 1 )
        ){
          ds[k,1:31] <- ds_nascimentos[i,1:31]
          ds[k,32] <- "morreu"
          achou <- TRUE
          cat("achou!\n")
          index <- remove_falecido(ds_mortos, ds_temp[j,6])
          ds_mortos <- ds_mortos[-index,]
          contador_encontrados_por_features <- contador_encontrados_por_features + 1
          contador_encontrados_rigor_40 <- contador_encontrados_rigor_40 + 1
        }
        j <- j + 1
      }
    }
  }
  
  # UTILIZANDO RIGOR 30%, OU SEJA, AS OUTRAS CARACTERÍSTICAS TEM Q SER 30% COMPATÍVEIS
  
  if(achou == FALSE){ # procurando O FALECIDO ARAVAS DAS OUTRAS CARACTERASTICAS
    rigor <- 30
    if(#is.na(CODMUNNATU_nasc) #
      #|| is.na(PESO_nasc) #
      is.na(SEXO_nasc) #
      || is.na(DTNASC_nasc) #
      || is.na(CODMUNRES_nasc)#
      || is.na(IDADEMAE_nasc)
      #|| is.na(RACACOR_nasc)
      #|| is.na(GESTACAO_nasc)
      #|| is.na(GRAVIDEZ_nasc)
      #|| is.na(PARTO_nasc)
    ){
      #cat("Dados inválidos... pulando\n")
    }
    else{
      ds_temp <- ds_mortos[,2]
      ds_temp <- cbind(ds_temp, ds_mortos[,3])
      #ds_temp <- cbind(ds_temp, ds_mortos[,4])
      ds_temp <- cbind(ds_temp, ds_mortos[,5])
      ds_temp <- cbind(ds_temp, ds_mortos[,6])
      #ds_temp <- cbind(ds_temp, ds_mortos[,7])
      #ds_temp <- cbind(ds_temp, ds_mortos[,8])
      #ds_temp <- cbind(ds_temp, ds_mortos[,9])
      #ds_temp <- cbind(ds_temp, ds_mortos[,10])
      #ds_temp <- cbind(ds_temp, ds_mortos[,11])
      ds_temp <- cbind(ds_temp, ds_mortos[,12])
      ds_temp <- ds_temp[complete.cases(ds_temp), ]
      
      
      j <- 1    
      while(j <= nrow(ds_temp) && achou == FALSE){
        DTNASC_m <- ds_temp[j,1]#
        SEXO_m <- ds_temp[j,2]#
        #CODMUNNATU_m <- ds_temp[j,3]#
        CODMUNRES_m <- ds_temp[j,3]#
        IDADEMAE_m <- ds_temp[j, 4]#
        #RACACOR_m <- ds_temp[j,6]
        #GESTACAO_m <- ds_temp[j,5]
        #GRAVIDEZ_m <- ds_temp[j,5]
        #PARTO_m <- ds_temp[j, 6]
        #PESO_m <- ds_temp[j,7]#
        if ((CODMUNRES_m == CODMUNRES_nasc) 
            && (DTNASC_m == DTNASC_nasc) 
            && (SEXO_m == SEXO_nasc)
            && (IDADEMAE_m == IDADEMAE_nasc || IDADEMAE_m == IDADEMAE_nasc + 1 )
        ){
          ds[k,1:31] <- ds_nascimentos[i,1:31]
          ds[k,32] <- "morreu"
          achou <- TRUE
          cat("achou!\n")
          index <- remove_falecido(ds_mortos, ds_temp[j,5])
          ds_mortos <- ds_mortos[-index,]
          contador_encontrados_por_features <- contador_encontrados_por_features + 1
          contador_encontrados_rigor_30 <- contador_encontrados_rigor_30 + 1
        }
        j <- j + 1
      }
    }
  }
  
  if(achou == FALSE){ # procurando O FALECIDO ARAVAS DAS OUTRAS CARACTERASTICAS
    rigor <- 30.2
    if(is.na(CODMUNNATU_nasc) #
       #|| is.na(PESO_nasc) #
       || is.na(SEXO_nasc) #
       || is.na(DTNASC_nasc) #
       #|| is.na(CODMUNRES_nasc)#
       || is.na(IDADEMAE_nasc)
       #|| is.na(RACACOR_nasc)
       #|| is.na(GESTACAO_nasc)
       #|| is.na(GRAVIDEZ_nasc)
       #|| is.na(PARTO_nasc)
    ){
      #cat("Dados inválidos... pulando\n")
    }
    else{
      ds_temp <- ds_mortos[,2]
      ds_temp <- cbind(ds_temp, ds_mortos[,3])
      ds_temp <- cbind(ds_temp, ds_mortos[,4])
      #ds_temp <- cbind(ds_temp, ds_mortos[,5])
      ds_temp <- cbind(ds_temp, ds_mortos[,6])
      #ds_temp <- cbind(ds_temp, ds_mortos[,7])
      #ds_temp <- cbind(ds_temp, ds_mortos[,8])
      #ds_temp <- cbind(ds_temp, ds_mortos[,9])
      #ds_temp <- cbind(ds_temp, ds_mortos[,10])
      #ds_temp <- cbind(ds_temp, ds_mortos[,11])
      ds_temp <- cbind(ds_temp, ds_mortos[,12])
      ds_temp <- ds_temp[complete.cases(ds_temp), ]
      
      
      j <- 1    
      while(j <= nrow(ds_temp) && achou == FALSE){
        DTNASC_m <- ds_temp[j,1]#
        SEXO_m <- ds_temp[j,2]#
        CODMUNNATU_m <- ds_temp[j,3]#
        #CODMUNRES_m <- ds_temp[j,3]#
        IDADEMAE_m <- ds_temp[j, 4]#
        #RACACOR_m <- ds_temp[j,6]
        #GESTACAO_m <- ds_temp[j,5]
        #GRAVIDEZ_m <- ds_temp[j,5]
        #PARTO_m <- ds_temp[j, 6]
        #PESO_m <- ds_temp[j,7]#
        if ((DTNASC_m == DTNASC_nasc) 
            && (SEXO_m == SEXO_nasc)
            && (CODMUNNATU_m == CODMUNNATU_nasc)
            && (IDADEMAE_m == IDADEMAE_nasc || IDADEMAE_m == IDADEMAE_nasc + 1 )
        ){
          ds[k,1:31] <- ds_nascimentos[i,1:31]
          ds[k,32] <- "morreu"
          achou <- TRUE
          cat("achou!\n")
          index <- remove_falecido(ds_mortos, ds_temp[j,5])
          ds_mortos <- ds_mortos[-index,]
          contador_encontrados_por_features <- contador_encontrados_por_features + 1
          contador_encontrados_rigor_30_V2 <- contador_encontrados_rigor_30_V2 + 1
        }
        j <- j + 1
      }
    }
  }
  
  if(achou == FALSE){ # procurando O FALECIDO ARAVAS DAS OUTRAS CARACTERASTICAS
    rigor <- 30.3
    if(is.na(CODMUNNATU_nasc) #
       || is.na(SEXO_nasc) #
       || is.na(DTNASC_nasc) #
       #|| is.na(CODMUNRES_nasc)#
       #|| is.na(IDADEMAE_nasc)
       || is.na(RACACOR_nasc)
       #|| is.na(GESTACAO_nasc)
       #|| is.na(GRAVIDEZ_nasc)
       #|| is.na(PARTO_nasc)
    ){
      #cat("Dados inválidos... pulando\n")
    }
    else{
      ds_temp <- ds_mortos[,2]
      ds_temp <- cbind(ds_temp, ds_mortos[,3])
      ds_temp <- cbind(ds_temp, ds_mortos[,4])
      #ds_temp <- cbind(ds_temp, ds_mortos[,5])
      #ds_temp <- cbind(ds_temp, ds_mortos[,6])
      ds_temp <- cbind(ds_temp, ds_mortos[,7])
      #ds_temp <- cbind(ds_temp, ds_mortos[,8])
      #ds_temp <- cbind(ds_temp, ds_mortos[,9])
      #ds_temp <- cbind(ds_temp, ds_mortos[,10])
      #ds_temp <- cbind(ds_temp, ds_mortos[,11])
      ds_temp <- cbind(ds_temp, ds_mortos[,12])
      ds_temp <- ds_temp[complete.cases(ds_temp), ]
      
      
      j <- 1    
      while(j <= nrow(ds_temp) && achou == FALSE){
        DTNASC_m <- ds_temp[j,1]#
        SEXO_m <- ds_temp[j,2]#
        CODMUNNATU_m <- ds_temp[j,3]#
        #CODMUNRES_m <- ds_temp[j,3]#
        #IDADEMAE_m <- ds_temp[j, 4]#
        RACACOR_m <- ds_temp[j,4]
        #GESTACAO_m <- ds_temp[j,5]
        #GRAVIDEZ_m <- ds_temp[j,5]
        #PARTO_m <- ds_temp[j, 6]
        #PESO_m <- ds_temp[j,7]#
        if ((DTNASC_m == DTNASC_nasc) 
            && (SEXO_m == SEXO_nasc)
            && (CODMUNNATU_m == CODMUNNATU_nasc)
            && (RACACOR_m == RACACOR_nasc )
        ){
          ds[k,1:31] <- ds_nascimentos[i,1:31]
          ds[k,32] <- "morreu"
          achou <- TRUE
          cat("achou!\n")
          index <- remove_falecido(ds_mortos, ds_temp[j,5])
          ds_mortos <- ds_mortos[-index,]
          contador_encontrados_por_features <- contador_encontrados_por_features + 1
          contador_encontrados_rigor_30_V3 <- contador_encontrados_rigor_30_V3 + 1
        }
        j <- j + 1
      }
    }
  }
  
  
  if(achou == FALSE){
    cat("Registro de Morto nao encontrado!\n")
    ds[k,1:31] <- ds_nascimentos[i,1:31]
    ds[k,32] <- "vivo"
  }
  cat("contador_encontrados_por_features: ", contador_encontrados_por_features, "\n")
  i <- i + 1
  k <- k + 1 # incrementando contadosr do "ds"
}

# TRocando CODANOMAL por S/N

i <- 1
while(i<=nrow(ds)){
  if(!is.na(ds[i,12])){
    ds[i,12] <- "Q000" # SIGNIFICA ANOMALIA! é UM factor cadastrado.
  }
  else{
    ds[i,12] <- "Q012" # SIGNIFICA SEM ANOMALIA! é UM factor cadastrado.
  }
  i<-i+1
  
}

cat("contador_encontrados_por_features: ", contador_encontrados_por_features, "\n")
cat("contador_encontrados_por_dn: ", contador_encontrados_por_dn, "\n")
#cat("contador_encontrados_rigor_10: ", contador_encontrados_rigor_10, "\n")
cat("contador_encontrados_rigor_30: ", contador_encontrados_rigor_30, "\n")
cat("contador_encontrados_rigor_30_V2: ", contador_encontrados_rigor_30_V2, "\n")
cat("contador_encontrados_rigor_30_V3: ", contador_encontrados_rigor_30_V3, "\n")
cat("contador_encontrados_rigor_40: ", contador_encontrados_rigor_40, "\n")
cat("contador_encontrados_rigor_50: ", contador_encontrados_rigor_50, "\n")
cat("contador_encontrados_rigor_60: ", contador_encontrados_rigor_60, "\n")
cat("contador_encontrados_rigor_70: ", contador_encontrados_rigor_70, "\n")
cat("contador_encontrados_rigor_80: ", contador_encontrados_rigor_80, "\n")
cat("contador_encontrados_rigor_90: ", contador_encontrados_rigor_90, "\n")



#Gravando o arquivo em disco.
cat("Gravando arquivo em disco")
write.csv(x = ds, row.names=FALSE, file = "C:/Users/Angelo/Documents/Escrita Mortalidade Infantil/dados_obitos_nascimentos/processados/finalV6.csv")
