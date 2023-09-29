#### Limpiar memoria
    rm(list=ls())
    
#### Importar Librería    
    library(plotrix)
    
    
#### Espacio de trabajo
    setwd('C:/Users/abia/Desktop/13.-TESIS/06.-Resultados/Analisis de sequías')
    
#### Importar datos desde archivos
    dat <- read.table("Data_Indices_Sequias.txt", header=TRUE)
    summary(dat)
    
#### Crear una serie de tiempo
    Observado <- dat$SPEI_Sur
    Modelo1 <- dat$SPI_AG_Sur
    Modelo2 <- dat$SPI_MV_Sur
    
    model.names <-c('AG','MV')
    
    a<-taylor.diagram(Observado,Modelo1, main="Modelos de Indicadores de Sequía", pos.cor=T, show.gamma=T, sd.arcs=T, ref.sd=T, pcex=2,
                   normalize=F,pch = 16)
    b<-taylor.diagram(Observado,Modelo2, main="", pos.cor=T, show.gamma=T, sd.arcs=T, ref.sd=T, pcex=2, normalize=F, xlab="",ylab="", add=T,
                   col="blue",pch=15)
    legend("bottomleft",legend=model.names,pch=c(16,15),col=c("red","blue"))
    a$mar
    