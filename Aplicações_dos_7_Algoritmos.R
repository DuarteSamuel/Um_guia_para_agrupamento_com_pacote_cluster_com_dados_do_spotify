# Monografia - Graduacao Estatistica
# Universidade Federal do Rio Grande do Norte
# Dicente: Samuel Ricardo Nobre Duarte
# Data da Ultima Atualizacao: 16/08/2021
# Data de inicio: 10/08/2021

## Biblioteca
# install.packages("DT")
# install.packages("boot")
# install.packages("doBy")
# install.packages("dplyr")
# install.packages("readxl")
# install.packages("GGally")
# install.packages("ggplot2")
# install.packages("cluster")
# install.packages("corrplot")
# install.packages("factoextra")

library(DT)
library(boot)
library(doBy)
library(dplyr)
library(readxl)
library(GGally)
library(ggplot2)
library(cluster)
library(corrplot)
library(factoextra)

# configuracao
rm(list = ls())
setwd("~/Estudos/Monografia")

# dados
data <- read_excel("Dados_oficial.xlsx")

data_ws <- read_excel("Dados_oficial.xlsx", sheet = "Data_ws")

# organização para leitura - fazendo isso pela funcao nao resolveu
{data_ws$dancabilidade<-as.numeric(data_ws$dancabilidade)
  data_ws$energia<-as.numeric(data_ws$energia)
  data_ws$volume<-as.numeric(data_ws$volume)
  data_ws$cantado<-as.numeric(data_ws$cantado)
  data_ws$acustica<-as.numeric(data_ws$acustica)
  data_ws$instrumentabilidade<-as.numeric(data_ws$instrumentabilidade)
  data_ws$vivacidade<-as.numeric(data_ws$vivacidade)
  data_ws$bpm<-as.numeric(data_ws$bpm)
  data_ws$duracao_ms<-as.numeric(data_ws$duracao_ms)}

features <- read_excel("Dados_oficial.xlsx", sheet = "Features", 
                       col_types = c("numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric"))

# variaveis categorias 
data_cat<-data.frame(data_ws$artista,data_ws$playlist,
                     data_ws$modo,data_ws$compasso,data_ws$musica,
                     data_ws$tipo,data_ws$album,data_ws$tom,
                     data_ws$modo_do_tom,data_ws$track.explicit)


names(data_cat)<-c("artista","playlist","modo","compasso","musica",
                   "tipo","album","tom_nome","modo_do_tom","explicito")

View(data_ws);View(features);View(data)
data_ws$
  data_cat$tom_nome<-as.factor(data_cat$tom_nome)
data_cat$tom<-as.factor(data_cat$tom)
data_cat$modo<-as.factor(data_cat$modo)
data_cat$compasso<-as.factor(data_cat$compasso)


# Daisy
ds_pam<-daisy(features[ ,c(2,7)])
ds_agnes <- daisy(artistas_features,stand = T)
dn <- diana(ds_diana,diss=T)

summary(ds_pam)


# 1 - pam
help(pam)
help("fviz_nbclust")
fviz_nbclust(features[ ,c(2,7)], kmeans, method = "silhouette")

ds_pam<-daisy(features[ ,c(2,7)])

pm<-pam(ds_pam,2)

pmfeatures<-data.frame(features[ ,c(2,7)],as.factor(pm$clustering))
names(pmfeatures)<-c("dancabilidade","instrumentabilidade","cluster")
ggplot(pmfeatures,aes(x=instrumentabilidade,y=dancabilidade))+
  geom_point(aes(col=cluster))+ylab("Dançabilidade")+
  xlab("Instrumentabilidade")

# Avaliando os grupos
m1.mnv<-manova(cbind(instrumentabilidade,dancabilidade)~cluster,data=pmfeatures);m1.mnv
summary(m1.mnv)

pm$medoids
summary(pm)

# Clara

fviz_nbclust(features, kmeans, method = "silhouette")
clr<-clara(features,k=4)

cluster<-as.factor(clr$clustering)
clrfeatures<-data.frame(features,cluster)

# medir a mÃ©dia do cluster para cada atributo
v<-clrfeatures %>%
  group_by(cluster) %>%
  summarise(dancab. = round(mean(dancabilidade),4),
            energia = round(mean(energia),4),
            volume = round(mean(volume),4),
            cantado = round(mean(cantado),4),
            acustica = round(mean(acustica),4),
            instrum. = round(mean(instrumentabilidade),4),
            vivacidade = round(mean(vivacidade),4),
            bpm = round(mean(bpm),4),
            duracao_ms = round(mean(duracao_ms),4),
            popularidade = round(mean(popularidade),4))


datatable(v)
summary(clr)

m2.mnv<-manova(cbind(dancabilidade,energia,volume,cantado,acustica,
                     instrumentabilidade,vivacidade,bpm,duracao_ms,
                     popularidade)~cluster,data=clrfeatures) 

help(ggpairs)
m2.mnv

summary(m2.mnv)
tk1<-TukeyHSD(aov(dancabilidade~cluster,data=clrfeatures));tk1;plot(tk1)
tk2<-TukeyHSD(aov(energia~cluster,data=clrfeatures));tk2;plot(tk2)
tk3<-TukeyHSD(aov(volume~cluster,data=clrfeatures));tk3;plot(tk3)
tk4<-TukeyHSD(aov(cantado~cluster,data=clrfeatures));tk4;plot(tk4)
tk5<-TukeyHSD(aov(acustica~cluster,data=clrfeatures));tk5;plot(tk5)
tk6<-TukeyHSD(aov(instrumentabilidade~cluster,data=clrfeatures));tk6;plot(tk6)
tk7<-TukeyHSD(aov(vivacidade~cluster,data=clrfeatures));tk7;plot(tk7)
tk8<-TukeyHSD(aov(bpm~cluster,data=clrfeatures));tk8;plot(tk8)
tk9<-TukeyHSD(aov(duracao_ms~cluster,data=clrfeatures));tk9;plot(tk9)
tk10<-TukeyHSD(aov(popularidade~cluster,data=clrfeatures));tk10;plot(tk10)

# Fanny

x<-c(1,5,4,1,3,1,2,3,4,2,2,3,25,25,26,27,28,26,27,28,49,49,49,50,50,46,47,50)
y<-c(1,2,3,4,2,1,5,4,1,3,2,6,50,18,49,48,47,50,48,47,1,2,4,3,5,2,1,3)
test_data<-data.frame(x,y)
k<-kmeans(test_data,4)

plot(y~x,data=test_data,col=k$cluster,pch=20)

# Nome das playlist
playlist_nomes<-names(table(data_ws$tom))
features2<-data.frame(features,data_ws$tom)


# Calculando as medias de cada playlist para todos atributos desejados
medias_tons<-features2 %>%
  group_by(data_ws.tom) %>%
  summarise(n=n(),
            dancabilidade = mean(dancabilidade),
            energia = mean(energia),
            volume = mean(volume),
            cantado = mean(cantado),
            acustica = mean(acustica),
            instrumentabilidade = mean(instrumentabilidade),
            vivacidade = mean(vivacidade),
            bpm = mean(bpm),
            duracao_ms = mean(duracao_ms))

# organizacao dos dados
medias_tons<-medias_tons[ ,-c(1,2)]
medias_tons<-data.frame(medias_tons,row.names = playlist_nomes)

# Fanny
fnn<-fanny(medias_tons,3,diss=F,metric = "SqEuclidean")
summary(fnn)
plot(fnn,title(main=NULL,xlab="Componente 1",ylab="Componente 2"))

medias_tons


# Agnes
# Artista
fastdata<-as.data.frame(table(data_cat$artista))
fastdata<-fastdata[fastdata$Freq>10, ]
fastdata<-fastdata[fastdata$Freq<100, ]
artist<-fastdata$Var1

m<-NULL
for(i in 1:length(data_ws$nº)){
  for(j in 1:length(artist)){
    if(data_ws$artista[i]==artist[j]){
      m[i]<-data_ws$nº[i]
    }
  }
}
m<-na.omit(m);m

features2<-data_ws[m, ]

artistas_features<-features2 %>%
  group_by(artista) %>%
  summarise(dancabilidade = mean(dancabilidade),
            energia = mean(energia),
            volume = mean(volume),
            cantado = mean(cantado),
            acustica = mean(acustica),
            instrumentabilidade = mean(instrumentabilidade),
            vivacidade = mean(vivacidade),
            bpm = mean(bpm),
            duracao_ms = mean(duracao_ms))

artist2<-artistas_features$artista
artistas_features<-artistas_features[ ,-1]
artistas_features<-data.frame(artistas_features,row.names = artist2);rm(artist2)
View(artistas_features)

help(agnes)

# aplicacao da agnes
ds_agnes <- daisy(artistas_features,stand = T)
summary(ds_agnes)

#agnes
gns<-agnes(ds_agnes,diss=T)

plot(gns)

# variando criterio - single
gns<-agnes(artistas_features,diss=F,stand = T,metric = "euclidean",
           method = "single")
plot(gns)

# variando criterio - complete
gns<-agnes(artistas_features,diss=F,stand = T,metric = "euclidean",
           method = "complete")
plot(gns)

# variando criterio - flexible
gns<-agnes(artistas_features,diss=F,stand = T,metric = "euclidean",
           method = "flexible",par.method = c(0.9,0.9,0.9,0.9))
plot(gns)

# variando criterio - gaverage
gns<-agnes(artistas_features,diss=F,stand = T,metric = "euclidean",
           method = "gaverage")
plot(gns)


# Diana

# Nome das playlist
playlist_nomes<-names(table(data_ws$playlist))

# Calculando as medias de cada playlist para todos atributos desejados
medias_play<-data_ws %>%
  group_by(playlist) %>%
  summarise(n=n(),
            dancabilidade = mean(dancabilidade),
            energia = mean(energia),
            volume = mean(volume),
            cantado = mean(cantado),
            acustica = mean(acustica),
            instrumentabilidade = mean(instrumentabilidade),
            vivacidade = mean(vivacidade),
            bpm = mean(bpm),
            duracao_ms = mean(duracao_ms))

# organizacao dos dados
medias_play<-medias_play[ ,-c(1,2)]
medias_play<-data.frame(medias_play,row.names = playlist_nomes)


# Matriz de dissimilaridade com variaveis mistas
ds_diana <- daisy(medias_play,stand = T)

# aplicacao da Diana
dn <- diana(ds_diana,diss=T)

plot(dn)

summary(dn)

# Mona
# variaveis binarias que temos: modo e explicito
# formar novas variaveis binarias
# Popularidade: se a musica consegui algum tipo de popularidade > 0 = 1 


explicito<-data_cat%>%
  select(explicito)%>%
  mutate(explicito=ifelse(data_cat$explicito==FALSE,0,1))

table(explicito)

pop_bin<-data_ws%>%
  select(popularidade)%>%
  mutate(popularidade=ifelse(popularidade==0,0,1))

table(pop_bin)

# se o tom e sustenido ou nao
# 0 = Do |  1 = do# | 2 = re | 3 = re# | 4 = mi | 5 = fa | 6= fa#
# 7 = G | 8 = G# | 9 = A| 10 = A# | 11 = B
sus_bin<-data_ws%>%
  select(tom)%>%
  mutate(tom=ifelse(tom=="C"|
                      tom=="D"|
                      tom=="E"|
                      tom=="F"|
                      tom=="G"|
                      tom=="A"|
                      tom=="B",1,0))
table(data_ws$tom)
table(sus_bin)
# tipo: se a musica fizer parte de um album o valor e 1 se for single ou 
# compilado recebe 0

tipo_bin<-data_ws%>%
  select(tipo)%>%
  mutate(tipo=ifelse(tipo=="album",1,0))

table(tipo_bin)

# criando o data frame com 5  variaveis binarias
data_bin<-data.frame(explicito$explicito,sus_bin$tom,
                     pop_bin$popularidade,tipo_bin$tipo,data_ws$modo)
names(data_bin)<-c("explicito","Sustenidos?","popularidade","tipo","modo")
head(data_bin,nrow=2)

# aplicacao da mona
mn<-mona(data_bin)

plot(mn,col="black")
lines(c(1.2,2),c(2950,4000),lwd=1)
lines(c(1.2,2),c(2950,500),lwd=1)
lines(c(2.5,3),c(4100,4500),lwd=1)
lines(c(2.5,3),c(4100,2950),lwd=1)
lines(c(2.34,3),c(400,700),lwd=1)
lines(c(2.34,3),c(400,250),lwd=1)
lines(c(3.24,4),c(4600,4720),lwd=1)
lines(c(3.24,4),c(4600,4250),lwd=1)
lines(c(3.5,4),c(3000,2975),lwd=1)
lines(c(3.5,4),c(3000,3200),lwd=1)
lines(c(3.25,4),c(270,295),lwd=1)
lines(c(3.25,4),c(270,150),lwd=1)
lines(c(3.5,4),c(690,490),lwd=1)
lines(c(3.5,4),c(690,1320),lwd=1)
lines(c(4.35,5),c(4760,4800),lwd=1)
lines(c(4.35,5),c(4760,4630),lwd=1)
lines(c(4.35,5),c(4190,4230),lwd=1)
lines(c(4.35,5),c(3320,3650),lwd=1)
lines(c(4.35,5),c(3320,3100),lwd=1)
lines(c(4.35,5),c(2950,2980),lwd=1)
lines(c(4.47,5),c(1370,1840),lwd=1)
lines(c(4.47,5),c(1370,900),lwd=1)
lines(c(4.47,5),c(300,310),lwd=1)
lines(c(4.47,5),c(300,270),lwd=1)
lines(c(4.47,5),c(450,610),lwd=1)
lines(c(4.47,5),c(450,400),lwd=1)
lines(c(4.47,5),c(130,150),lwd=1)
lines(c(4.47,5),c(130,10),lwd=1)
