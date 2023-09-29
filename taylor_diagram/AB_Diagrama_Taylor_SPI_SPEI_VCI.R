#
#--------------------------------------------------------------------------
# AUTOR         : Ing. Iván Arturo Ayala Bizarro
# FECHA         : 2016.05.17
# PROPÓSITO     : Plotea el diagrama de Taylor
#--------------------------------------------------------------------------
#
#### Limpiar memoria
    rm(list=ls())
    
#### Importar Librería    
    library(plotrix)
    
    
#### Espacio de trabajo
    setwd('C:/Users/abia/Desktop/13.-TESIS/Resultados/Analisis_Correlacion_Cruzada')
    
#### Importar datos desde archivos
    dat <- read.table("Data_SPI_SPEI_VCI.txt", header=TRUE)
    summary(dat)
    boxplot(dat[,3:ncol(dat)])
    
#### Crear una serie de tiempo
    Observado <- dat$VCI
    Modelo1 <- dat$SPI1_MV
    Modelo2 <- dat$SPI1_AG
    Modelo3 <- dat$SPEI1
    Modelo4 <- dat$SPI3_MV
    Modelo5 <- dat$SPI3_AG
    Modelo6 <- dat$SPEI3
    Modelo7 <- dat$SPI6_MV
    Modelo8 <- dat$SPI6_AG
    Modelo9 <- dat$SPEI6
    Modelo10 <- dat$SPI9_MV
    Modelo11 <- dat$SPI9_AG
    Modelo12 <- dat$SPEI9
    Modelo13 <- dat$SPI12_MV
    Modelo14 <- dat$SPI12_AG
    Modelo15 <- dat$SPEI12
    
    
    
    
    #model.names <-c('SPEI','SPI_AG','SPI_MV')
    model.names <-colnames(dat[,3:17])
    color<- c(111, 222, 16,1,15, 4,140,623,13,50,70,11,8,10,150)
    color<-rainbow(15)
    
    symb<-c(0,1,2,4,5,6,7,8,17,15,16,18,11,22,23)
    #symb<-seq(0:14)

    taylor.diagram(Observado,Modelo1, main="Modelos de Indicadores de Sequía", pos.cor=T, show.gamma=T, sd.arcs=T, ref.sd=T, pcex=2,
                   normalize=F,pch = symb[1])
    taylor.diagram(Observado,Modelo2, main="", pos.cor=T, show.gamma=T, sd.arcs=T, ref.sd=T, pcex=2, normalize=F, xlab="",ylab="", add=T,
                   col=color[2],pch=symb[2])
    taylor.diagram(Observado,Modelo3, main="", pos.cor=T, show.gamma=T, sd.arcs=T, ref.sd=T, pcex=2, normalize=F, xlab="",ylab="", add=T,
                   col=color[3],pch=symb[3])
    taylor.diagram(Observado,Modelo4, main="", pos.cor=T, show.gamma=T, sd.arcs=T, ref.sd=T, pcex=2, normalize=F, xlab="",ylab="", add=T,
                   col=color[4],pch=symb[4])
    taylor.diagram(Observado,Modelo5, main="", pos.cor=T, show.gamma=T, sd.arcs=T, ref.sd=T, pcex=2, normalize=F, xlab="",ylab="", add=T,
                   col=color[5],pch=symb[5])
    taylor.diagram(Observado,Modelo6, main="", pos.cor=T, show.gamma=T, sd.arcs=T, ref.sd=T, pcex=2, normalize=F, xlab="",ylab="", add=T,
                   col=color[6],pch=symb[6])
    taylor.diagram(Observado,Modelo7, main="", pos.cor=T, show.gamma=T, sd.arcs=T, ref.sd=T, pcex=2, normalize=F, xlab="",ylab="", add=T,
                   col=color[7],pch=symb[7])
    taylor.diagram(Observado,Modelo8, main="", pos.cor=T, show.gamma=T, sd.arcs=T, ref.sd=T, pcex=2, normalize=F, xlab="",ylab="", add=T,
                   col=color[8],pch=symb[8])
    taylor.diagram(Observado,Modelo9, main="", pos.cor=T, show.gamma=T, sd.arcs=T, ref.sd=T, pcex=2, normalize=F, xlab="",ylab="", add=T,
                   col=color[9],pch=symb[9])
    taylor.diagram(Observado,Modelo10, main="", pos.cor=T, show.gamma=T, sd.arcs=T, ref.sd=T, pcex=2, normalize=F, xlab="",ylab="", add=T,
                   col=color[10],pch=symb[10])
    taylor.diagram(Observado,Modelo11, main="", pos.cor=T, show.gamma=T, sd.arcs=T, ref.sd=T, pcex=2, normalize=F, xlab="",ylab="", add=T,
                   col=color[11],pch=symb[11])
    taylor.diagram(Observado,Modelo12, main="", pos.cor=T, show.gamma=T, sd.arcs=T, ref.sd=T, pcex=2, normalize=F, xlab="",ylab="", add=T,
                   col=color[12],pch=symb[12])
    taylor.diagram(Observado,Modelo13, main="", pos.cor=T, show.gamma=T, sd.arcs=T, ref.sd=T, pcex=2, normalize=F, xlab="",ylab="", add=T,
                   col=color[13],pch=symb[13])
    taylor.diagram(Observado,Modelo14, main="", pos.cor=T, show.gamma=T, sd.arcs=T, ref.sd=T, pcex=2, normalize=F, xlab="",ylab="", add=T,
                   col=color[14],pch=symb[14])
    taylor.diagram(Observado,Modelo15, main="", pos.cor=T, show.gamma=T, sd.arcs=T, ref.sd=T, pcex=2, normalize=F, xlab="",ylab="", add=T,
                   col=color[15],pch=symb[15])
    #legend("topright",legend=model.names,pch=symb,col=color ,cex=0.8)#,"magenta"))
    legend(1.3,1.3,legend=model.names,pch=symb,col=color ,cex=0.8)#,"magenta"))

#### Verificación en series de tiempo
    library(hydroGOF)
    require(zoo)
    
    # Crear una serie de tiempo
    td = seq(as.Date("2000/3/1"), as.Date("2015/12/1"), "months")
    
    # Precipitaciones Estaciones Senamhi
    #Modelo1.s = zoo(x=Modelo1, order.by=td) 
    Modelo1.s = zoo(x=Modelo1, order.by=td) 
    Modelo2.s = zoo(x=Modelo2, order.by=td) 
    
    ## Ploteo Observado y simulado
    #gofs=c("ME", "MAE", "RMSE", "NRMSE", "PBIAS", "RSR", "rSD", "NSE", "mNSE", 
    #       "rNSE", "d", "md", "rd", "r", "R2", "bR2", "KGE", "VE")
    
    
    # Ploteo Mensual y Anual observado y simulado
    gofs1=c("RMSE", "NSE" , "d", "r", "R2")
    
        

    
    # Modelo 01: VCI Versus SPEI
    #ggof(sim=Observado, obs=Modelo1.s, ftype="ma", gofs=gofs1, FUN=mean, ylab=c("Indices Normalizados"),main="VCI versus SPEI",legend=c("VCI","SPEI"))
    
    # Modelo 02: VCI Versus SPIAG
    ggof(sim=Observado, obs=Modelo1.s, ftype="ma", gofs=gofs1, FUN=mean, ylab=c("Indices Normalizados"),main="VCI versus SPI AG",legend=c("VCI","SPI AG"))
    
    # Modelo 03: VCI Versus SPIMV
    ggof(sim=Observado, obs=Modelo3.s, ftype="ma", gofs=gofs1, FUN=mean, ylab=c("Indices Normalizados"),main="VCI versus SPI MV",legend=c("VCI","SPI MV"))
    
    