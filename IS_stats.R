# Estatísticas descritivas

# Pacotes necessários
library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(esquisse)

# limpar enviroment
rm(list = ls())

#definir diretório
setwd('C:/Users/pc/Desktop/CIDACS/Estatísticas_segregação/Indice')

# ind segregação entre brancos e negros
race_bxn <- read_csv('SI_All_Race_Cities_Brazil_brancosXnegros.csv')

# ind segregação de renda
renda <- read_csv('renda_domiciliar_per_capita.csv')

# dados do censo ibge 2010
ibge <- read_csv('C:/Users/pc/Documents/CURSO R/BRAZIL_CITIES.csv')

# selecionar variáveis do ibge
ibge <- ibge %>% 
  select(
    CITY,
    STATE,
    CAPITAL,
    IBGE_RES_POP,
    IBGE_DU,
    IBGE_DU_URBAN,
    IBGE_DU_RURAL,
    IBGE_POP,
    `IDHM Ranking 2010` ,
    IDHM,
    ALT,
    AREA,
    GDP,
    GDP_CAPITA,
    Cars
  )

# cod dos estados 
cd_estados <- read_excel("cd_estados.xlsx")

# selecionar apenas os 2 números do cod do estado
cd_estados$cd <- str_sub(cd_estados$cd, end = 2)
cd_estados$cd <- as.numeric(cd_estados$cd)

# cod dos municipios 
cd_city <- read_excel("cd_city.xlsx")
cd_city$cod_uf <- as.numeric(cd_city$cod_uf)

# Juntar cod estados e cod municipios
cd_city <- left_join(cd_city, cd_estados, by = c('cod_uf' = 'cd'))
# selecionar variaveis
cd_city <- cd_city %>% 
  mutate(
    uf = uf.x ,
    cod_mun = as.numeric(cod_mun)
    ) %>% 
  select(
    cod_mun, city, cod_uf, uf, sigla_uf, regiao
  )

# criar variavel cd do estado na base de segregação racial
race_bxn <- race_bxn %>% 
  mutate(
    cod_uf = as.numeric(str_sub(race_bxn$cod, end=2)),
    cod_mun = cod,
    seg_bxn = Seg_Index
  ) %>% 
  select(
    cod_uf, cod_mun, seg_bxn
  )

# juntar base de segregação racial e cod da cidade
race_bxn <- left_join(race_bxn, cd_city, by = c('cod_mun', 'cod_uf'))

# juntar base de segregação racial e segregação de renda
df <- left_join(race_bxn, renda, by= c('cod_mun' = 'cod')) %>% 
  select(
    cod_mun , city , cod_uf , uf , sigla_uf , regiao , seg_bxn , seg_index_3 : seg_index_1_8
  )

# juntar base de segregação e dados do ibge
df <- left_join(df, ibge, by = c('city' = 'CITY', 'sigla_uf' = 'STATE'))

# limpar enviroment
rm( ibge, cd_city, cd_estados, race_bxn, renda )

# exportar df

write_csv(df, 'df_indices.csv')

#############################################################################################
# 
# Estatísticas
#
#############################################################################################
#
# Segregação de renda por região
#
#############################################################################################

# segregação renda familiar até 1/8 de sm

#Brasil
mean(df$seg_index_1_8)
median(df$seg_index_1_8)
sd(df$seg_index_1_8)

#Estados
renda_estados <- df %>% 
  group_by(regiao) %>% 
  summarise(mean(seg_index_1_8), median(seg_index_1_8), sd(seg_index_1_8))

print(renda_estados[,])

# segregação renda familiar até 1/4 de sm

#Brasil
mean(df$seg_index_1_4)
median(df$seg_index_1_4)
sd(df$seg_index_1_4)

#Estados
renda_estados <- df %>% 
  group_by(regiao) %>% 
  summarise(mean(seg_index_1_4), median(seg_index_1_4), sd(seg_index_1_4))

print(renda_estados[,])


# segregação renda familiar até 1/2 de sm

#Brasil
mean(df$seg_index_1_2)
median(df$seg_index_1_2)
sd(df$seg_index_1_2)

#Estados
renda_estados <- df %>% 
  group_by(regiao) %>% 
  summarise(mean(seg_index_1_2), median(seg_index_1_2), sd(seg_index_1_2))

print(renda_estados[,])

# segregação renda familiar até 1 sm

#Brasil
mean(df$seg_index_1)
median(df$seg_index_1)
sd(df$seg_index_1)

#Estados
renda_estados <- df %>% 
  group_by(regiao) %>% 
  summarise(mean(seg_index_1), median(seg_index_1), sd(seg_index_1))

print(renda_estados[,])

# segregação renda familiar até 2 sm

#Brasil
mean(df$seg_index_2)
median(df$seg_index_2)
sd(df$seg_index_2)

#Estados
renda_estados <- df %>% 
  group_by(regiao) %>% 
  summarise(mean(seg_index_2), median(seg_index_2), sd(seg_index_2))

print(renda_estados[,])

#############################################################################################

df_norte <- filter(df, regiao== 'Norte')
df_nordeste <- filter(df, regiao== 'Nordeste')
df_sul <- filter(df, regiao== 'Sul')
df_sudeste <- filter(df, regiao== 'Sudeste')
df_co <- filter(df, regiao== 'Centro-Oeste')


library(ggplot2)

ggplot(df) +
 aes(x = "", y = seg_index_1_8, fill = regiao) +
 geom_boxplot(shape = "circle") +
 scale_fill_manual(values = list(
 `Centro-Oeste` = "#B21F21", Nordeste = "#0C972E", Norte = "#8838A2", Sudeste = "#0D7AF2", Sul = "#D96411")) +
 labs(x = "Renda familiar até 1/8 de salário mínimo per capita", y = "Índice de segregação", fill = "Região") +
 theme_minimal() +
 theme(axis.title.y = element_text(size = 12L, face = "bold"), axis.title.x = element_text(size = 12L, 
 face = "bold"))

 
#############################################################################################

sum(is.na(race_bxn$Seg_Index))

filter(race_bxn, is.na(race_bxn$Seg_Index) == T) # cidade de santa catarina sem ind. segregação
