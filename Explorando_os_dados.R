# Monografia - Graduacao Estatistica
# Universidade Federal do Rio Grande do Norte
# Dicente: Samuel Ricardo Nobre Duarte
# Data da Ultima Atualizacao: 05/08/2021
# Data de inicio: 15/07/2021

## Biblioteca
# install.packages(dplyr)
# install.packages(GGally)
# install.packages(readxl)
# install.packages(cluster)
# install.packages(corrplot)
# install.packages(gridExtra)

library(dplyr)
library(readxl)
library(GGally)
library(ggplot2)
library(cluster)
library(corrplot)
library(gridExtra)

# configuracao
rm(list = ls())
setwd("~/Estudos/Monografia") # selecione o diretorio correto

# dados
data <- read_excel("Dados_oficial.xlsx")

data_ws <- read_excel("Dados_oficial.xlsx", sheet = "Data_ws")

features <- read_excel("Dados_oficial.xlsx", sheet = "Features", 
                       col_types = c("numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric"))

View(data);View(features);View(data_ws)

# analise exploratoria
length(data_ws);length(data_ws$artista)

summary(features)

x<-NULL;for(i in 2:11){x[i]<-var(features[ ,i])}
y<-NULL;for(i in 2:11){y[i]<-sqrt(var(features[ ,i]))}
xx<-data.frame(names(features),x,y);xx;rm(xx);rm(x)

x<-NULL
for(i in 1:length(data_ws$nº)){x[i]<-sqrt(var(features[ ,i]))}
matrix(c(names(features),x),ncol=2)

grid.arrange(
  ggplot(features,aes(x=dancabilidade))+
    geom_histogram(fill="skyblue4",col="black")+ylab("Contagem")+xlab("Dançabilidade"),
  ggplot(features,aes(x=energia))+
    geom_histogram(fill="skyblue4",col="black")+ylab("Contagem"),
  ggplot(features,aes(x=volume))+
    geom_histogram(fill="skyblue4",col="black")+ylab("Contagem"),
  ggplot(features,aes(x=cantado))+
    geom_histogram(fill="skyblue4",col="black")+ylab("Contagem"),
  ggplot(features,aes(x=acustica))+
    geom_histogram(fill="skyblue4",col="black")+ylab("Contagem"),
  ggplot(features,aes(x=instrumentabilidade))+
    geom_histogram(fill="skyblue4",col="black")+ylab("Contagem"),
  ggplot(features,aes(x=vivacidade))+
    geom_histogram(fill="skyblue4",col="black")+ylab("Contagem"),
  ggplot(features,aes(x=bpm))+
    geom_histogram(fill="skyblue4",col="black")+ylab("Contagem"),
  ggplot(features,aes(x=duracao_ms))+
    geom_histogram(fill="skyblue4",col="black")+ylab("Contagem"),
  ggplot(features,aes(x=popularidade))+
    geom_histogram(fill="skyblue4",col="black")+ylab("Contagem"),ncol=2)

# correlacao
mt.cor.tracks <- cor(features[,-1])
corrplot(mt.cor.tracks,
         order = "hclust", tl.col = "black", 
         tl.cex = 0.6, tl.srt = 0,method = "color",
         addCoef.col = T,pch="red",
         col=colorRampPalette(c("skyblue4","grey96","brown3"))(100))

# 1
grid.arrange(
  ggplot(features,aes(x=dancabilidade,y=energia))+
    geom_point()+xlab("1")+ylab("2"),
  ggplot(features,aes(x=dancabilidade,y=volume))+
    geom_point()+xlab("1")+ylab("3"),
  ggplot(features,aes(x=dancabilidade,y=cantado))+
    geom_point()+xlab("1")+ylab("4"),
  ggplot(features,aes(x=dancabilidade,y=acustica))+
    geom_point()+xlab("1")+ylab("5"),
  ggplot(features,aes(x=dancabilidade,y=instrumentabilidade))+
    geom_point()+xlab("1")+ylab("6"),
  ggplot(features,aes(x=dancabilidade,y=vivacidade))+
    geom_point()+xlab("1")+ylab("7"),
  ggplot(features,aes(x=dancabilidade,y=bpm))+
    geom_point()+xlab("1")+ylab("8"),
  ggplot(features,aes(x=dancabilidade,y=duracao_ms))+
    geom_point()+xlab("1")+ylab("9"),
  ggplot(features,aes(x=dancabilidade,y=popularidade))+
    geom_point()+xlab("1")+ylab("10"),
  ggplot(features,aes(x=energia,y=volume))+
    geom_point()+xlab("2")+ylab("3"),nrow=2)
# 2
grid.arrange(
  ggplot(features,aes(x=energia,y=cantado))+
    geom_point()+xlab("2")+ylab("4"),
  ggplot(features,aes(x=energia,y=acustica))+
    geom_point()+xlab("2")+ylab("5"),
  ggplot(features,aes(x=energia,y=instrumentabilidade))+
    geom_point()+xlab("2")+ylab("6"),
  ggplot(features,aes(x=energia,y=vivacidade))+
    geom_point()+xlab("2")+ylab("7"),
  ggplot(features,aes(x=energia,y=bpm))+
    geom_point()+xlab("2")+ylab("8"),
  ggplot(features,aes(x=energia,y=duracao_ms))+
    geom_point()+xlab("2")+ylab("9"),
  ggplot(features,aes(x=energia,y=popularidade))+
    geom_point()+xlab("2")+ylab("10"),
  ggplot(features,aes(x=volume,y=cantado))+
    geom_point()+xlab("3")+ylab("4"),
  ggplot(features,aes(x=volume,y=acustica))+
    geom_point()+xlab("3")+ylab("5"),
  ggplot(features,aes(x=volume,y=instrumentabilidade))+
    geom_point()+xlab("3")+ylab("6"),nrow=2)

# 3
grid.arrange(
  ggplot(features,aes(x=volume,y=vivacidade))+
    geom_point()+xlab("3")+ylab("7"),
  ggplot(features,aes(x=volume,y=bpm))+
    geom_point()+xlab("3")+ylab("8"),
  ggplot(features,aes(x=volume,y=duracao_ms))+
    geom_point()+xlab("3")+ylab("9"),
  ggplot(features,aes(x=volume,y=popularidade))+
    geom_point()+xlab("3")+ylab("10"),
  ggplot(features,aes(x=cantado,y=acustica))+
    geom_point()+xlab("4")+ylab("5"),
  ggplot(features,aes(x=cantado,y=instrumentabilidade))+
    geom_point()+xlab("4")+ylab("6"),
  ggplot(features,aes(x=cantado,y=vivacidade))+
    geom_point()+xlab("4")+ylab("7"),
  ggplot(features,aes(x=cantado,y=bpm))+
    geom_point()+xlab("4")+ylab("8"),
  ggplot(features,aes(x=cantado,y=duracao_ms))+
    geom_point()+xlab("4")+ylab("9"),
  ggplot(features,aes(x=cantado,y=popularidade))+
    geom_point()+xlab("4")+ylab("10"),nrow=2)

# 4
grid.arrange(
  ggplot(features,aes(x=acustica,y=instrumentabilidade))+
    geom_point()+xlab("5")+ylab("6"),
  ggplot(features,aes(x=acustica,y=vivacidade))+
    geom_point()+xlab("5")+ylab("7"),
  ggplot(features,aes(x=acustica,y=bpm))+
    geom_point()+xlab("5")+ylab("8"),
  ggplot(features,aes(x=acustica,y=duracao_ms))+
    geom_point()+xlab("5")+ylab("9"),
  ggplot(features,aes(x=acustica,y=popularidade))+
    geom_point()+xlab("5")+ylab("10"),
  ggplot(features,aes(x=instrumentabilidade,y=vivacidade))+
    geom_point()+xlab("6")+ylab("7"),
  ggplot(features,aes(x=instrumentabilidade,y=bpm))+
    geom_point()+xlab("6")+ylab("8"),
  ggplot(features,aes(x=instrumentabilidade,y=duracao_ms))+
    geom_point()+xlab("6")+ylab("9"),
  ggplot(features,aes(x=instrumentabilidade,y=popularidade))+
    geom_point()+xlab("6")+ylab("10"),
  ggplot(features,aes(x=vivacidade,y=bpm))+
    geom_point()+xlab("7")+ylab("8"),nrow=2)

#4.5
grid.arrange(
  ggplot(features,aes(x=vivacidade,y=duracao_ms))+
    geom_point()+xlab("7")+ylab("9"),
  ggplot(features,aes(x=vivacidade,y=popularidade))+
    geom_point()+xlab("7")+ylab("10"),
  ggplot(features,aes(x=bpm,y=duracao_ms))+
    geom_point()+xlab("8")+ylab("9"),
  ggplot(features,aes(x=bpm,y=popularidade))+
    geom_point()+xlab("8")+ylab("10"),
  ggplot(features,aes(x=duracao_ms,y=popularidade))+
    geom_point()+xlab("9")+ylab("10"),nrow=1)

# 1 - Dancabilidade
# 2 - Energia
# 3 - Volume
# 4 - Cantado
# 5 - Acustica
# 6 - Instrumentabilidae
# 7 - Vivacidade
# 8 - Bpm
# 9 - duracao_ms
# 10- popularidade

# ggpairs
ggpairs(features[ ,2:11])+theme_minimal(base_size = 7)

names(features)

# variaveis categorias 
data_cat<-data.frame(data_ws$artista,data_ws$playlist,
                     data_ws$modo,data_ws$compasso,data_ws$musica,
                     data_ws$tipo,data_ws$album,data_ws$tom,
                     data_ws$modo_do_tom,data_ws$track.explicit)

names(data_cat)<-c("artista","playlist","modo","compasso","musica",
                   "tipo","album","tom_nome","modo_do_tom","explicito")

data_cat$tom_nome<-as.factor(data_cat$tom_nome)
data_cat$modo<-as.factor(data_cat$modo)
data_cat$compasso<-as.factor(data_cat$compasso)
data_cat$explicito<-as.factor(data_cat$explicito)

# Tom
fastdata<-as.data.frame(table(data_cat$tom_nome))
ggplot(fastdata,aes(x=reorder(Var1,Freq),y=Freq))+
  geom_col(fill="Skyblue4")+
  geom_text(aes(label=Freq), vjust=-0.4,color="black",size=3.1)+
  xlab("Tom")+ylab("Contagem")+ggtitle("Frequencia dos tons nas musicas")
rm(fastdata)

# Tipos de albuns
fastdata<-as.data.frame(table(data_cat$tipo))
ggplot(fastdata,aes(x=reorder(Var1,Freq),y=Freq))+
  geom_col(fill="Skyblue4")+coord_flip()+
  geom_text(aes(label=Freq), vjust=0.4,hjust=-.2,color="black",size=3.1)+
  xlab("Tipo")+ylab("Contagem")+
  ggtitle("Frequencia dos tipos de organizacao de musicas")
rm(fastdata)

# Artista
fastdata<-as.data.frame(table(data_cat$artista))
fastdata<-fastdata[fastdata$Freq>20, ]
fastdata<-fastdata[fastdata$Freq<100, ]
ggplot(fastdata,aes(x=reorder(Var1,Freq),y=Freq))+
  geom_col(fill="Skyblue4")+
  coord_flip()+
  geom_text(aes(label=Freq), vjust=0.4,hjust=-.2,color="black",size=3.1)+
  xlab("Artista")+ylab("Contagem")+ggtitle("Artistas mais frequentes")
rm(fastdata)
length(table(data_cat$artista))

# explicito
fastdata<-as.data.frame(table(data_cat$explicito))
fastdata<-data.frame(fastdata,pt=c("Falso", "Verdadeiro"))
ggplot(fastdata,aes(x=reorder(pt,Freq),y=Freq))+
  geom_col(fill="Skyblue4")+
  geom_text(aes(label=Freq), vjust=-0.4,hjust=0,color="black",size=3.1)+
  xlab("Explícito")+ylab("Contagem")+ggtitle("Conteúdo explícito")
rm(fastdata)
