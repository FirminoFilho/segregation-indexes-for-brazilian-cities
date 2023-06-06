#########################################################
# SEGREGATION INDEX SETOR CENSITÁRIO URBANO RENDA DOMICILIAR PER CAPITA
# Gabrielle Rebouças
# 23/11/21         
# CIDACS
#########################################################
rm(list = ls())
if(!require(dplyr)){ install.packages("dplyr"); require(dplyr)} 
library(openxlsx)
library(OasisR)
library(sp) 

#definir diretório
setwd("C:/Users/pc/Desktop/CIDACS/Renda_Domiciliar/DI_RendaDomiciliar")


myFiles <- list.files(pattern=".*xlsx") 

res = data.frame(matrix(NA, nrow = 0, ncol = 2))
x <- c("Cod_setor", "Seg_Index")
colnames(res) <- x

for(k in myFiles) {
  df <- read.xlsx(k)
  df <- df[complete.cases(df),]
  
  # seleciona apenas os setores censitários urbanos
  df <- df %>% filter(Situacao_setor==1 | Situacao_setor==2 | Situacao_setor==3)
  
  d1 = dim(df)
  d1 = d1[1]
  
  df = replace(df, df == "X", 0.000001)
  
  for(j in 1:d1) {
    df[j,1] = substr(df[j,1], 1, 7)
  }
  
  df_cod <- df[,-(3:length(df)),drop=FALSE]
  df[] <- lapply(df, function(x) type.convert(as.character(x)))
  
  df_cod <- aggregate(. ~ Cod_setor, df, sum)
  df_set <- aggregate(. ~ Cod_setor, df, FUN=length) #Cod_setor~Situacao,FUN=length,data=df
  
  d2 = dim(df_cod)
  d2 = d2[1]
  
  cod <- vector(,d2)
  Seg_Index <- vector(,d2)
  
  scc = 0
  for(i in 1:d2) {
    city = data.frame(matrix(NA, nrow = 0, ncol = 16))
    city <- setNames(city, colnames(df))
    #print(df_cod$Cod_setor[i])
    
    for(j in 1:d1) {
      if (df_cod$Cod_setor[i] == df$Cod_setor[j]){
        city = data.frame(rbind(as.matrix(city), as.matrix(df[j,])))
      }
    }
    
    cod[i] = df_cod$Cod_setor[i]
    
    dd = dim(city)
    
    if (df_set$Situacao_setor[i] >= 4){      
      
      scc = scc + 1
      
      ##################################################################################    
      #                       1/8 de sm
      ################################################################################## 
      
      ### dom com até 1/8 de renda per capita na area i (linha i)
      renda18_i = as.numeric(as.character(city[,c("V005")])) + as.numeric(as.character(city[,c("V014")]))
      
      # todos dom com até 1/8 de renda per capita na cidade
      renda18 = sum(renda18_i)
      
      # proporção de dom com até 1/8 de renda per capita na area i (linha i)
      # dom com até 1/8 de renda per capita na área i sobre o total de dom com até 1/8 de renda per capita na cidade
      p_renda18 = (renda18_i/renda18)*100
      
      
      ### dom com mais de 1/8 de renda per capita na area i
      renda_maior_i = as.numeric(as.character(city[,c("V006")])) + as.numeric(as.character(city[,c("V007")])) + as.numeric(as.character(city[,c("V008")])) + as.numeric(as.character(city[,c("V009")]))+ as.numeric(as.character(city[,c("V010")]))+ as.numeric(as.character(city[,c("V011")]))+ as.numeric(as.character(city[,c("V012")]))+ as.numeric(as.character(city[,c("V013")]))
      
      # total de dom com mais de 1/8 de renda per capita na cidade
      renda_maior = sum(renda_maior_i)
      
      # proporção de dom com mais de 1/8 de renda per capita na area i (linha i)
      # dom com mais de 1/8 de renda per capita na área i sobre o total de dom com mais de 1/8 de renda per capita na cidade
      p_renda_maior = (renda_maior_i/renda_maior)*100
      
      
      x = cbind( p_renda18 , p_renda_maior )
      
      IS1 = ISDuncan(x)
      Seg_Index[scc] = IS1[1]
      
    }
  }
  res2 <- cbind(cod, Seg_Index)
  res = data.frame(rbind(as.matrix(res), as.matrix(res2)))
}
print(k)
View(res)
write.csv(res, file = "SI_urb_wage_1_8_percapita.csv")

############################################################################################################
rm(list = ls())
if(!require(dplyr)){ install.packages("dplyr"); require(dplyr)} 
library(openxlsx)
library(OasisR)
library(sp) 

#definir diretório
setwd("C:/Users/pc/Desktop/CIDACS/Renda_Domiciliar/DI_RendaDomiciliar")


myFiles <- list.files(pattern=".*xlsx") 

res = data.frame(matrix(NA, nrow = 0, ncol = 2))
x <- c("Cod_setor", "Seg_Index")
colnames(res) <- x

for(k in myFiles) {
  df <- read.xlsx(k)
  df <- df[complete.cases(df),]
  
  # seleciona apenas os setores censitários urbanos
  df <- df %>% filter(Situacao_setor==1 | Situacao_setor==2 | Situacao_setor==3)
  
  d1 = dim(df)
  d1 = d1[1]

  
  df = replace(df, df == "X", 0.000001)
  
  for(j in 1:d1) {
    df[j,1] = substr(df[j,1], 1, 7)
  }
  
  df_cod <- df[,-(3:length(df)),drop=FALSE]
  df[] <- lapply(df, function(x) type.convert(as.character(x)))
  
  df_cod <- aggregate(. ~ Cod_setor, df, sum)
  df_set <- aggregate(. ~ Cod_setor, df, FUN=length) #Cod_setor~Situacao,FUN=length,data=df
  
  d2 = dim(df_cod)
  d2 = d2[1]
  
  cod <- vector(,d2)
  Seg_Index <- vector(,d2)
  
  scc = 0
  for(i in 1:d2) {
    city = data.frame(matrix(NA, nrow = 0, ncol = 16))
    city <- setNames(city, colnames(df))
    #print(df_cod$Cod_setor[i])
    
    for(j in 1:d1) {
      if (df_cod$Cod_setor[i] == df$Cod_setor[j]){
        city = data.frame(rbind(as.matrix(city), as.matrix(df[j,])))
      }
    }
    
    cod[i] = df_cod$Cod_setor[i]
    
    dd = dim(city)
    
    if (df_set$Situacao_setor[i] >= 4){      
      
      scc = scc + 1
      
      ##################################################################################    
      #                       1/4 de sm
      ################################################################################## 
      
      ### dom com até 1/4 de renda per capita na area i (linha i)
      renda14_i = as.numeric(as.character(city[,c("V005")])) + as.numeric(as.character(city[,c("V006")])) + as.numeric(as.character(city[,c("V014")]))
      
      # todos dom com até 1/4 de renda per capita na cidade
      renda14 = sum(renda14_i)
      
      # proporção de dom com até 1/4 de renda per capita na area i (linha i)
      # dom com até 1/4 de renda per capita na área i sobre o total de dom com até 1/4 de renda per capita na cidade
      p_renda14 = (renda14_i/renda14)*100
      
      
      ### dom com mais de 1/4 de renda per capita na area i
      renda_maior_i =  as.numeric(as.character(city[,c("V007")])) + as.numeric(as.character(city[,c("V008")])) + as.numeric(as.character(city[,c("V009")]))+ as.numeric(as.character(city[,c("V010")]))+ as.numeric(as.character(city[,c("V011")]))+ as.numeric(as.character(city[,c("V012")]))+ as.numeric(as.character(city[,c("V013")]))
      
      # total de dom com mais de 1/4 de renda per capita na cidade
      renda_maior = sum(renda_maior_i)
      
      # proporção de dom com mais de 1/4 de renda per capita na area i (linha i)
      # dom com mais de 1/4 de renda per capita na área i sobre o total de dom com mais de 1/4 de renda per capita na cidade
      p_renda_maior = (renda_maior_i/renda_maior)*100
      
      
      x = cbind( p_renda14 , p_renda_maior )
      
      IS1 = ISDuncan(x)
      Seg_Index[scc] = IS1[1]
      
    }
  }
  res2 <- cbind(cod, Seg_Index)
  res = data.frame(rbind(as.matrix(res), as.matrix(res2)))
}
write.csv(res, file = "SI_urb_wage_1_4_percapita.csv")

############################################################################################################
rm(list = ls())
if(!require(dplyr)){ install.packages("dplyr"); require(dplyr)} 
library(openxlsx)
#library(readxl)
library(OasisR)
library(sp) 

#definir diretório
setwd("C:/Users/pc/Desktop/CIDACS/Renda_Domiciliar/DI_RendaDomiciliar")



myFiles <- list.files(pattern=".*xlsx") 

res = data.frame(matrix(NA, nrow = 0, ncol = 2))
x <- c("Cod_setor", "Seg_Index")
colnames(res) <- x

for(k in myFiles) {
  df <- read.xlsx(k)
  df <- df[complete.cases(df),]
  
  # seleciona apenas os setores censitários urbanos
  df <- df %>% filter(Situacao_setor==1 | Situacao_setor==2 | Situacao_setor==3)
  
  d1 = dim(df)
  d1 = d1[1]

  
  df = replace(df, df == "X", 0.000001)
  
  for(j in 1:d1) {
    df[j,1] = substr(df[j,1], 1, 7)
  }
  
  df_cod <- df[,-(3:length(df)),drop=FALSE]
  df[] <- lapply(df, function(x) type.convert(as.character(x)))
  
  df_cod <- aggregate(. ~ Cod_setor, df, sum)
  df_set <- aggregate(. ~ Cod_setor, df, FUN=length) #Cod_setor~Situacao,FUN=length,data=df
  
  d2 = dim(df_cod)
  d2 = d2[1]
  
  cod <- vector(,d2)
  Seg_Index <- vector(,d2)
  
  scc = 0
  for(i in 1:d2) {
    city = data.frame(matrix(NA, nrow = 0, ncol = 16))
    city <- setNames(city, colnames(df))
    #print(df_cod$Cod_setor[i])
    
    for(j in 1:d1) {
      if (df_cod$Cod_setor[i] == df$Cod_setor[j]){
        city = data.frame(rbind(as.matrix(city), as.matrix(df[j,])))
      }
    }
    
    cod[i] = df_cod$Cod_setor[i]
    
    dd = dim(city)
    
    if (df_set$Situacao_setor[i] >= 4){      
      
      scc = scc + 1
      
      ##################################################################################    
      #                       1/2 de sm
      ################################################################################## 
      
      ### dom com até 1/2 de renda per capita na area i (linha i)
      renda12_i = as.numeric(as.character(city[,c("V005")])) + as.numeric(as.character(city[,c("V006")])) + as.numeric(as.character(city[,c("V007")])) + as.numeric(as.character(city[,c("V014")]))
      
      # todos dom com até 1/2 de renda per capita na cidade
      renda12 = sum(renda12_i)
      
      # proporção de dom com até 1/2 de renda per capita na area i (linha i)
      # dom com até 1/2 de renda per capita na área i sobre o total de dom com até 1/2 de renda per capita na cidade
      p_renda12 = (renda12_i/renda12)*100
      
      
      ### dom com mais de 1/2 de renda per capita na area i
      renda_maior_i =  as.numeric(as.character(city[,c("V008")])) + as.numeric(as.character(city[,c("V009")]))+ as.numeric(as.character(city[,c("V010")]))+ as.numeric(as.character(city[,c("V011")]))+ as.numeric(as.character(city[,c("V012")]))+ as.numeric(as.character(city[,c("V013")]))
      
      # total de dom com mais de 1/2 de renda per capita na cidade
      renda_maior = sum(renda_maior_i)
      
      # proporção de dom com mais de 1/2 de renda per capita na area i (linha i)
      # dom com mais de 1/2 de renda per capita na área i sobre o total de dom com mais de 1/2 de renda per capita na cidade
      p_renda_maior = (renda_maior_i/renda_maior)*100
      
      
      x = cbind( p_renda12 , p_renda_maior )
      
      IS1 = ISDuncan(x)
      Seg_Index[scc] = IS1[1]
      
    }
  }
  res2 <- cbind(cod, Seg_Index)
  res = data.frame(rbind(as.matrix(res), as.matrix(res2)))
}
write.csv(res, file = "SI_urb_wage_1_2_percapita.csv")

############################################################################################################
rm(list = ls())
if(!require(dplyr)){ install.packages("dplyr"); require(dplyr)} 
library(openxlsx)
#library(readxl)
library(OasisR)
library(sp) 

#definir diretório
setwd("C:/Users/pc/Desktop/CIDACS/Renda_Domiciliar/DI_RendaDomiciliar")


myFiles <- list.files(pattern=".*xlsx") 

res = data.frame(matrix(NA, nrow = 0, ncol = 2))
x <- c("Cod_setor", "Seg_Index")
colnames(res) <- x

for(k in myFiles) {
  df <- read.xlsx(k)
  df <- df[complete.cases(df),]
  
  # seleciona apenas os setores censitários urbanos
  df <- df %>% filter(Situacao_setor==1 | Situacao_setor==2 | Situacao_setor==3)
  
  d1 = dim(df)
  d1 = d1[1]

  
  df = replace(df, df == "X", 0.000001)
  
  for(j in 1:d1) {
    df[j,1] = substr(df[j,1], 1, 7)
  }
  
  df_cod <- df[,-(3:length(df)),drop=FALSE]
  df[] <- lapply(df, function(x) type.convert(as.character(x)))
  
  df_cod <- aggregate(. ~ Cod_setor, df, sum)
  df_set <- aggregate(. ~ Cod_setor, df, FUN=length) #Cod_setor~Situacao,FUN=length,data=df
  
  d2 = dim(df_cod)
  d2 = d2[1]
  
  cod <- vector(,d2)
  Seg_Index <- vector(,d2)
  
  scc = 0
  for(i in 1:d2) {
    city = data.frame(matrix(NA, nrow = 0, ncol = 16))
    city <- setNames(city, colnames(df))
    #print(df_cod$Cod_setor[i])
    
    for(j in 1:d1) {
      if (df_cod$Cod_setor[i] == df$Cod_setor[j]){
        city = data.frame(rbind(as.matrix(city), as.matrix(df[j,])))
      }
    }
    
    cod[i] = df_cod$Cod_setor[i]
    
    dd = dim(city)
    
    if (df_set$Situacao_setor[i] >= 4){      
      
      scc = scc + 1
      
      ##################################################################################    
      #                       1 sm
      ################################################################################## 
      
      ### dom com até 1 sm de renda per capita na area i (linha i)
      renda1_i = as.numeric(as.character(city[,c("V005")])) + as.numeric(as.character(city[,c("V006")])) +  as.numeric(as.character(city[,c("V007")])) + as.numeric(as.character(city[,c("V008")])) +  as.numeric(as.character(city[,c("V014")]))
      
      # todos dom com até 1 sm de renda per capita na cidade
      renda1 = sum(renda1_i)
      
      # proporção de dom com até 1 sm de renda per capita na area i (linha i)
      # dom com até 1 sm de renda per capita na área i sobre o total de dom com até 1 sm de renda per capita na cidade
      p_renda1 = (renda1_i/renda1)*100
      
      
      ### dom com mais de 1 sm de renda per capita na area i
      renda_maior_i = as.numeric(as.character(city[,c("V009")]))+ as.numeric(as.character(city[,c("V010")]))+ as.numeric(as.character(city[,c("V011")]))+ as.numeric(as.character(city[,c("V012")]))+ as.numeric(as.character(city[,c("V013")]))
      
      # total de dom com mais de 1 sm de renda per capita na cidade
      renda_maior = sum(renda_maior_i)
      
      # proporção de dom com mais de 1 sm de renda per capita na area i (linha i)
      # dom com mais de 1 sm de renda per capita na área i sobre o total de dom com mais de 1 sm de renda per capita na cidade
      p_renda_maior = (renda_maior_i/renda_maior)*100
      
      
      x = cbind( p_renda1 , p_renda_maior )
      
      IS1 = ISDuncan(x)
      Seg_Index[scc] = IS1[1]
      
    }
  }
  res2 <- cbind(cod, Seg_Index)
  res = data.frame(rbind(as.matrix(res), as.matrix(res2)))
}
write.csv(res, file = "SI_urb_wage_1_percapita.csv")

############################################################################################################
rm(list = ls())
if(!require(dplyr)){ install.packages("dplyr"); require(dplyr)} 
library(openxlsx)
#library(readxl)
library(OasisR)
library(sp) 

#definir diretório
setwd("C:/Users/pc/Desktop/CIDACS/Renda_Domiciliar/DI_RendaDomiciliar")


myFiles <- list.files(pattern=".*xlsx") 

res = data.frame(matrix(NA, nrow = 0, ncol = 2))
x <- c("Cod_setor", "Seg_Index")
colnames(res) <- x

for(k in myFiles) {
  df <- read.xlsx(k)
  df <- df[complete.cases(df),]
  
  # seleciona apenas os setores censitários urbanos
  df <- df %>% filter(Situacao_setor==1 | Situacao_setor==2 | Situacao_setor==3)
  
  d1 = dim(df)
  d1 = d1[1]

  
  df = replace(df, df == "X", 0.000001)
  
  for(j in 1:d1) {
    df[j,1] = substr(df[j,1], 1, 7)
  }
  
  df_cod <- df[,-(3:length(df)),drop=FALSE]
  df[] <- lapply(df, function(x) type.convert(as.character(x)))
  
  df_cod <- aggregate(. ~ Cod_setor, df, sum)
  df_set <- aggregate(. ~ Cod_setor, df, FUN=length) #Cod_setor~Situacao,FUN=length,data=df
  
  d2 = dim(df_cod)
  d2 = d2[1]
  
  cod <- vector(,d2)
  Seg_Index <- vector(,d2)
  
  scc = 0
  for(i in 1:d2) {
    city = data.frame(matrix(NA, nrow = 0, ncol = 16))
    city <- setNames(city, colnames(df))
    #print(df_cod$Cod_setor[i])
    
    for(j in 1:d1) {
      if (df_cod$Cod_setor[i] == df$Cod_setor[j]){
        city = data.frame(rbind(as.matrix(city), as.matrix(df[j,])))
      }
    }
    
    cod[i] = df_cod$Cod_setor[i]
    
    dd = dim(city)
    
    if (df_set$Situacao_setor[i] >= 4){      
      
      scc = scc + 1
      
      ##################################################################################    
      #                       2 sm
      ################################################################################## 
      
      ### dom com até 2 sm de renda per capita na area i (linha i)
      renda2_i = as.numeric(as.character(city[,c("V005")])) + as.numeric(as.character(city[,c("V006")])) +  as.numeric(as.character(city[,c("V007")])) + as.numeric(as.character(city[,c("V008")])) + as.numeric(as.character(city[,c("V009")]))+ as.numeric(as.character(city[,c("V014")]))
      
      # todos dom com até 2 sm de renda per capita na cidade
      renda2 = sum(renda2_i)
      
      # proporção de dom com até 2 sm de renda per capita na area i (linha i)
      # dom com até 2 sm de renda per capita na área i sobre o total de dom com até 2 sm de renda per capita na cidade
      p_renda2 = (renda2_i/renda2)*100
      
      
      ### dom com mais de 2 sm de renda per capita na area i
      renda_maior_i =  as.numeric(as.character(city[,c("V010")]))+ as.numeric(as.character(city[,c("V011")]))+ as.numeric(as.character(city[,c("V012")]))+ as.numeric(as.character(city[,c("V013")]))
      
      # total de dom com mais de 2 sm de renda per capita na cidade
      renda_maior = sum(renda_maior_i)
      
      # proporção de dom com mais de 2 sm de renda per capita na area i (linha i)
      # dom com mais de 2 sm de renda per capita na área i sobre o total de dom com mais de 2 sm de renda per capita na cidade
      p_renda_maior = (renda_maior_i/renda_maior)*100
      
      
      x = cbind( p_renda2 , p_renda_maior )
      
      IS1 = ISDuncan(x)
      Seg_Index[scc] = IS1[1]
      
    }
  }
  res2 <- cbind(cod, Seg_Index)
  res = data.frame(rbind(as.matrix(res), as.matrix(res2)))
}
write.csv(res, file = "SI_urb_wage_2_percapita.csv")

############################################################################################################
rm(list = ls())
if(!require(dplyr)){ install.packages("dplyr"); require(dplyr)} 
library(openxlsx)
library(OasisR)
library(sp) 

#definir diretório
setwd("C:/Users/pc/Desktop/CIDACS/Renda_Domiciliar/DI_RendaDomiciliar")



myFiles <- list.files(pattern=".*xlsx") 

res = data.frame(matrix(NA, nrow = 0, ncol = 2))
x <- c("Cod_setor", "Seg_Index")
colnames(res) <- x

for(k in myFiles) {
  df <- read.xlsx(k)
  df <- df[complete.cases(df),]
  
  # seleciona apenas os setores censitários urbanos
  df <- df %>% filter(Situacao_setor==1 | Situacao_setor==2 | Situacao_setor==3)
  
  d1 = dim(df)
  d1 = d1[1]

  
  df = replace(df, df == "X", 0.000001)
  
  for(j in 1:d1) {
    df[j,1] = substr(df[j,1], 1, 7)
  }
  
  df_cod <- df[,-(3:length(df)),drop=FALSE]
  df[] <- lapply(df, function(x) type.convert(as.character(x)))
  
  df_cod <- aggregate(. ~ Cod_setor, df, sum)
  df_set <- aggregate(. ~ Cod_setor, df, FUN=length) #Cod_setor~Situacao,FUN=length,data=df
  
  d2 = dim(df_cod)
  d2 = d2[1]
  
  cod <- vector(,d2)
  Seg_Index <- vector(,d2)
  
  scc = 0
  for(i in 1:d2) {
    city = data.frame(matrix(NA, nrow = 0, ncol = 16))
    city <- setNames(city, colnames(df))
    #print(df_cod$Cod_setor[i])
    
    for(j in 1:d1) {
      if (df_cod$Cod_setor[i] == df$Cod_setor[j]){
        city = data.frame(rbind(as.matrix(city), as.matrix(df[j,])))
      }
    }
    
    cod[i] = df_cod$Cod_setor[i]
    
    dd = dim(city)
    
    if (df_set$Situacao_setor[i] >= 4){      
      
      scc = scc + 1
      
      ##################################################################################    
      #                       3 sm
      ################################################################################## 
      
      ### dom com até 3 sm de renda per capita na area i (linha i)
      renda3_i = as.numeric(as.character(city[,c("V005")])) + as.numeric(as.character(city[,c("V006")])) +  as.numeric(as.character(city[,c("V007")])) + as.numeric(as.character(city[,c("V008")])) + as.numeric(as.character(city[,c("V009")]))+ as.numeric(as.character(city[,c("V010")]))+ as.numeric(as.character(city[,c("V014")]))
      
      # todos dom com até 3 sm de renda per capita na cidade
      renda3 = sum(renda3_i)
      
      # proporção de dom com até 3 sm de renda per capita na area i (linha i)
      # dom com até 3 sm de renda per capita na área i sobre o total de dom com até 3 sm de renda per capita na cidade
      p_renda3 = (renda3_i/renda3)*100
      
      
      ### dom com mais de 3 sm de renda per capita na area i
      renda_maior_i = as.numeric(as.character(city[,c("V011")]))+ as.numeric(as.character(city[,c("V012")]))+ as.numeric(as.character(city[,c("V013")]))
      
      # total de dom com mais de 3 sm de renda per capita na cidade
      renda_maior = sum(renda_maior_i)
      
      # proporção de dom com mais de 3 sm de renda per capita na area i (linha i)
      # dom com mais de 3 sm de renda per capita na área i sobre o total de dom com mais de 3 sm de renda per capita na cidade
      p_renda_maior = (renda_maior_i/renda_maior)*100
      
      
      x = cbind( p_renda3 , p_renda_maior )
      
      IS1 = ISDuncan(x)
      Seg_Index[scc] = IS1[1]
      
    }
  }
  res2 <- cbind(cod, Seg_Index)
  res = data.frame(rbind(as.matrix(res), as.matrix(res2)))
}
write.csv(res, file = "SI_urb_wage_3_percapita.csv")

rm(list = ls())
