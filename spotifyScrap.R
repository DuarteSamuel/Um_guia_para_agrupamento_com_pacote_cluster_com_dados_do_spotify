# Monografia - Graduação Estatística
# Universidade Federal do Rio Grande do Norte
# Dicente: Samuel Ricardo Nobre Duarte
# Orientador: Carla Vivacqua
# Data da Ultima Atualizacao: 02/06/2021
# Data de inicio: 15/07/2021

## Biblioteca
#install.packages(spotifyr)
#install.packages(corrplot)
#install.packages(ggplot2)
#install.packages(GGally)
#install.packages(readxl)

library(spotifyr)
library(corrplot)
library(cluster)
library(ggplot2)
library(GGally)
library(readxl)


## Configurações
          # Diretório correto
rm(list=ls())
setwd("~/Estudos/Monografia") # selecione o diretorio correto

# chaves do spotify - acesse Spotify for developers e crie as chaves
id<-"_" 
secret<-"_"

          # configuração das chaves
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token <- get_spotify_access_token()


# Dados das playlist que serão extraidas
dados_playlist <- read_excel("SpotifyScrap.xlsx")
dados_playlist<-dados_playlist[ ,1:5]
View(dados_playlist)

# extração de dados
play_name<-'spotify'
play_uris<-dados_playlist$cod

data<-get_playlist_audio_features(play_name, play_uris)
View(data)

# Seleção de variaveis
View(data[ ,c(2,6:16,19,31,33,36,37,43,49,59,60,61)])

# artista
artist<-c(rep("undefined",length(data$playlist_id)))

for(i in 1:length(data$playlist_id)){
  tratado<-data[[28]][[i]][[3]]
  tratado<-as.character(tratado)
  artist[i]<-tratado
}
art_list<-data.frame(artist)
artist<-art_list$artist

nomes<-names(data[ ,c(2,6:16,19,31,33,36,37,43,49,59,60,61)])
data_ws<-data.frame(artist,data[ ,c(2,6:16,19,31,33,36,37,43,49,59,60,61)])
names(data_ws)<-c("artista",nomes)
data_ws<-data_ws[ ,-22]
View(data_ws)

data_ws$mode<-as.numeric(data_ws$mode);class(data_ws$mode)
data_ws$key_name<-as.factor(data_ws$key_name);class(data_ws$key_name)
data_ws$mode_name<-as.factor(data_ws$mode_name);class(data_ws$mode_name)
data_ws$track.album.album_type<-as.factor(data_ws$track.album.album_type)


names(data_ws)<-c("artista","playlist","dancabilidade","energia","tom",
                  "volume","modo","cantado","acustica",
                  "instrumentabilidade","vivacidade","valencia"
                  ,"bpm","compasso","duracao_ms","Explicito","musica","popularidade",
                  "tipo","album","tom","modo_do_tom")

data_ws<-na.omit(data_ws)

# organizando variaveis númericas

features<-data_ws[ ,c(3,4,6,8:11,13,15,17)]
View(features)

# outlier 
for(i in 1:10){
  id.out<-matrix(c(rep(rep(0,55970),10)),ncol=10)
  q1<-summary(features[ ,i])[2]
  q3<-summary(features[ ,i])[5]
  iqr<-q3-q1
  io<-c(q1-1.5*iqr,q3+1.5*iqr)
  for(j in 1:length(features$danceability)){
    if(features[j,i]<io[1]||features[j,i]>io[2]){
      id.out[j,i]<-1
    }
  }
}
View(id.out) # filtrando pelo maior valor, há valores diferente de 0?

