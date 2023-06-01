# INDICADORES ------------
# Cargando las librerias 
suppressMessages(suppressWarnings(library(data.table)))
suppressMessages(suppressWarnings(library(RODBC)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(summarytools)))
suppressMessages(suppressWarnings(library(GGally)))
suppressMessages(suppressWarnings(library(DT)))
suppressMessages(suppressWarnings(library(lubridate)))
suppressMessages(suppressWarnings(library(venn)))
suppressMessages(suppressWarnings(library(readxl)))
suppressMessages(suppressWarnings(library(stringi)))
suppressMessages(suppressWarnings(library(stringr)))
suppressMessages(suppressWarnings(library(writexl)))
suppressMessages(suppressWarnings(library(matrixStats)))

library(robustbase)
library(rrcov)      # PCA robusto
library(factoextra) # Visualizacion de PCA
library(h2o)        # Entrenar autoencoders
library(tidyverse)  # Preparacion de datos y gráficos
library(ggplot2)
library(DDoutlier)
library(isotree) #Se utiliza para la realización del modelo de árboles de aislamiento

options(scipen = 999)

#setwd("G:/Unidades compartidas/Fiscalizacion Preventiva de Compras Publicas/Fiscalización Preventiva de Compras Públicas/04_Modelos_Anomalias_2022/13_Corrida_2022_Planificacion")

# Conexion SICOP y CASP

SICOP <-odbcConnect("Conexion_SICOP")
tablas <- sqlTables(SICOP, tableType = "TABLE")

tablas$TABLE_NAME

## Indicadores Plazo -------------
# Cargando las tablas necesarias 
Plazos <- data.table(sqlFetch(SICOP, "ds.plazos",as.is=TRUE))

# Cargando los datos
#dim(Plazos)
#names(Plazos)
#str(Plazos)


Plazos$difIniAdj<- as.numeric(Plazos$difIniAdj)
Plazos$difAdjContPri<- as.numeric(Plazos$difAdjContPri)
Plazos$difAdjContUlt<- as.numeric(Plazos$difAdjContUlt)
Plazos$difContPriRec<- as.numeric(Plazos$difContPriRec)
Plazos$difContUltRec<- as.numeric(Plazos$difContUltRec)
Plazos$cantModif<- as.numeric(Plazos$cantModif)


Plazos$codigoProductoAdjudicado_16<- substr(Plazos$codigoProductoAdjudicado, 1, 16)

#limpiando los datos para incluir solo datos desde el 2018, y solo bienes
Plazos<- Plazos[(anno < 2023 & anno > 2019),]
Plazos[, tipo_bien:= substr(codigoProductoSolicitado,1,1)]
Plazos<- Plazos[Plazos$tipo_bien%in%c("1","2","3","4","5","6"), ]

#uniqueN(x = Plazos, by = c("numeroActo","idProcedimiento","numeroOferta","idLinea"))

### Indicador 1

# 1. Desviaci?n de la cantidad de d?as entre la fecha de inicio y la fecha de adjudicaci?n, respecto al "promedio" del bien.
# Se hace en dos pasos (podria hacerse en uno solo pero se mantiene en dos para mostrar mas informacion) 
# Primero se calcula los d?as promedio del bien 

#Promedio_iniadj <- Plazos[, .(Prom.inicioadju = mean(difIniAdj,na.rm = TRUE)), keyby = .(codigoProductoAdjudicado)][]
Promedio_iniadj_16 <- Plazos[, .(Prom.inicioadju_16 = mean(difIniAdj,na.rm = TRUE)), keyby = .(codigoProductoAdjudicado_16)][]

#uniqueN(Promedio_iniadj,by = "codigoProductoAdjudicado")
#uniqueN(Promedio_iniadj_16,by = "codigoProductoAdjudicado_16")

# Pegando a Plazo el promedio del codigoProductoAdjudicado

#Plazo.1 <- merge(x = Plazos,y = Promedio_iniadj,by = "codigoProductoAdjudicado",all.x = TRUE)
Plazo.1 <- merge(x = Plazos, y = Promedio_iniadj_16,by = "codigoProductoAdjudicado_16",all.x = TRUE)

#names(Plazo.1)
Plazo.1[,.N, keyby = .(Prom.inicioadju_16)][]
summary(Plazo.1$Prom.inicioadju_16)

# Posteriormente se calcula la desviacion entre cada una de las adjudicaciones y el plazo promedio
Plazo.1[, Desv.inicioadju := difIniAdj/Prom.inicioadju_16][]
Plazo.1[,.N, keyby = .(codigoProductoAdjudicado, difIniAdj, Prom.inicioadju_16, Desv.inicioadju)][]
#View(Plazo.1[codigoProductoAdjudicado_16 == "1010151692156358",.(codigoProductoAdjudicado,difIniAdj,Prom.inicioadju_16,Desv.inicioadju)])
Plazo.1[,.N, keyby = Desv.inicioadju][order(-N)][,Porcentaje := (N/sum(N))*100][]
#View(Plazo.1[Desv.inicioadju == max(Desv.inicioadju),.(codigoProductoAdjudicado,difIniAdj,Prom.inicioadju_16,Desv.inicioadju)])
#View(Plazo.1)
summary(Plazo.1$Desv.inicioadju) # Tiene 7 NA's

### Indicador 2
# 2.1. Desviaci?n de la cantidad de d?as entre la fecha de Contrato (primera) y la fecha de adjudicaci?n, respecto al "promedio" del bien.
#Promedio_contprirec <- Plazos[, .(Prom.contprirec = mean(difContPriRec,na.rm = TRUE)), keyby = .(codigoProductoAdjudicado)][]
Promedio_contprirec_16 <- Plazos[, .(Prom.contprirec_16 = mean(difContPriRec,na.rm = TRUE)), keyby = .(codigoProductoAdjudicado_16)][]

#uniqueN(Promedio_contprirec,by = "codigoProductoAdjudicado")
uniqueN(Promedio_contprirec_16,by = "codigoProductoAdjudicado_16")


# Pegando a Plazo el promedio del codigoProductoAdjudicado

#Plazo.1 <- merge(x = Plazo.1,y = Promedio_contprirec,by = "codigoProductoAdjudicado",all.x = TRUE)
Plazo.1 <- merge(x = Plazo.1, y = Promedio_contprirec_16,by = "codigoProductoAdjudicado_16",all.x = TRUE)

names(Plazo.1)
Plazo.1[,.N, keyby = .(Prom.contprirec_16)][]
summary(Plazo.1$Prom.contprirec_16) #posee 98032 NA's

# Posteriormente se calcula la desviacion entre cada una de las adjudicaciones y el plazo promedio
Plazo.1[, Desv.contprirec := difContPriRec/Prom.contprirec_16][]
Plazo.1[,.N, keyby = .(codigoProductoAdjudicado,difContPriRec,Prom.contprirec_16,Desv.contprirec)][]
#View(Plazo.1[codigoProductoAdjudicado == "1010151692156358_16",.(codigoProductoAdjudicado,difContPriRec,Prom.contprirec_16,Desv.contprirec)])
Plazo.1[,.N, keyby = Desv.contprirec][order(-N)][,Porcentaje := (N/sum(N))*100][]
#View(Plazo.1[Desv.contprirec == max(Desv.contprirec),.(codigoProductoAdjudicado,difContPriRec,Prom.contprirec_16,Desv.contprirec)])
#View(Plazo.1)
summary(Plazo.1$Desv.contprirec) #Posee 169277 Na's

# 2.2. Grado de desviaci?n entre la fecha de recepci?n y la formalizaci?n del ultimo contrato
#Promedio_contultrec <- Plazo.1[, .(Prom.contultrec = mean(difContUltRec,na.rm = TRUE)), keyby = .(codigoProductoAdjudicado)][]
Promedio_contultrec_16 <- Plazo.1[, .(Prom.contultrec_16 = mean(difContUltRec,na.rm = TRUE)), keyby = .(codigoProductoAdjudicado_16)][]

#uniqueN(Promedio_contultrec,by = "codigoProductoAdjudicado")
uniqueN(Promedio_contultrec_16,by = "codigoProductoAdjudicado_16")

# Pegando a Plazo el promedio del codigoProductoAdjudicado

#Plazo.1 <- merge(x = Plazo.1,y = Promedio_contultrec,by = "codigoProductoAdjudicado",all.x = TRUE)
Plazo.1 <- merge(x = Plazo.1,y = Promedio_contultrec_16,by = "codigoProductoAdjudicado_16",all.x = TRUE)

names(Plazo.1)
Plazo.1[,.N, keyby = .(Prom.contultrec_16)][]
summary(Plazo.1$Prom.contultrec_16) #Posee un total de 200716 NA's

# Posteriormente se calcula la desviacion entre cada una de las adjudicaciones y el plazo promedio
Plazo.1[, Desv.contultrec := difContUltRec/Prom.contultrec_16][]
Plazo.1[,.N, keyby = .(codigoProductoAdjudicado,difContUltRec,Prom.contultrec_16,Desv.contultrec)][]
#View(Plazo.1[codigoProductoAdjudicado_16 == "1010151692156358",.(codigoProductoAdjudicado,difContUltRec,Prom.contultrec_16,Desv.contultrec)])
Plazo.1[,.N, keyby = Desv.contultrec][order(-N)][,Porcentaje := (N/sum(N))*100][]
#View(Plazo.1[Desv.contultrec == max(Desv.contultrec),.(codigoProductoAdjudicado,difContUltRec,Prom.contultrec_16,Desv.contultrec)])
#View(Plazo.1)
summary(Plazo.1$Desv.contultrec) #Posee un total de 220784 NA's

### Indicador 3
# 3.1. Grado de desviaci?n entre la fecha de adjudicaci?n y la formalizaci?n del ultimo contrato
#Promedio_contultadj <- Plazos[, .(Prom.contultadj = mean(difAdjContUlt,na.rm = TRUE)), keyby = .(codigoProductoAdjudicado)][]
Promedio_contultadj_16 <- Plazos[, .(Prom.contultadj_16 = mean(difAdjContUlt,na.rm = TRUE)), keyby = .(codigoProductoAdjudicado_16)][]
#uniqueN(Promedio_contultadj_16,by = "codigoProductoAdjudicado")
uniqueN(Promedio_contultadj_16,by = "codigoProductoAdjudicado_16")

# Pegando a Plazo el promedio del codigoProductoAdjudicado

#Plazo.1 <- merge(x = Plazo.1,y = Promedio_contultadj,by = "codigoProductoAdjudicado",all.x = TRUE)
Plazo.1 <- merge(x = Plazo.1,y = Promedio_contultadj_16,by = "codigoProductoAdjudicado_16",all.x = TRUE)

names(Plazo.1)
Plazo.1[,.N, keyby = .(Prom.contultadj_16)][]
summary(Plazo.1$Prom.contultadj_16)  #Posee un total de 147832 NA's

# Posteriormente se calcula la desviacion entre cada una de las adjudicaciones y el plazo promedio
Plazo.1[, Desv.contultadj := difAdjContUlt/Prom.contultadj_16][]
Plazo.1[,.N, keyby = .(codigoProductoAdjudicado,difAdjContUlt,Prom.contultadj_16,Desv.contultadj)][]
#View(Plazo.1[codigoProductoAdjudicado_16 == "1010151692156358",.(codigoProductoAdjudicado,difAdjContUlt,Prom.contultadj_16,Desv.contultadj)])
Plazo.1[,.N, keyby = Desv.contultadj][order(-N)][,Porcentaje := (N/sum(N))*100][]
#View(Plazo.1[Desv.contultadj == max(Desv.contultadj),.(codigoProductoAdjudicado,difAdjContUlt,Prom.contultadj_16,Desv.contultadj)])
#View(Plazo.1)
summary(Plazo.1$Desv.contultadj)  #Posee un total de 206752 NA's

# 3.2. Grado de desviaci?n entre la fecha de adjudicaci?n y la formalizaci?n del primer contrato

#Promedio_contpriadj <- Plazos[, .(Prom.contpriradj = mean(difAdjContPri,na.rm = TRUE)), keyby = .(codigoProductoAdjudicado)][]
Promedio_contpriadj_16 <- Plazos[, .(Prom.contpriradj_16 = mean(difAdjContPri,na.rm = TRUE)), keyby = .(codigoProductoAdjudicado_16)][]

#uniqueN(Promedio_contpriadj,by = "codigoProductoAdjudicado")
uniqueN(Promedio_contpriadj_16,by = "codigoProductoAdjudicado_16")

# Pegando a Plazo el promedio del codigoProductoAdjudicado

#Plazo.1 <- merge(x = Plazo.1,y = Promedio_contpriadj,by = "codigoProductoAdjudicado",all.x = TRUE)
Plazo.1 <- merge(x = Plazo.1,y = Promedio_contpriadj_16,by = "codigoProductoAdjudicado_16",all.x = TRUE)

names(Plazo.1)
Plazo.1[,.N, keyby = .(Prom.contpriradj_16)][]
summary(Plazo.1$Prom.contpriradj_16) #Existe un valor de -25, es una contrataci?n por Demanda, y en SICOP el procedimiento parece estar bien (Procedimiento, 2019CD-000156-0000100001)
#View(Plazo.1)

# Posteriormente se calcula la desviacion entre cada una de las adjudicaciones y el plazo promedio
Plazo.1[, Desv.contpriadj := difAdjContPri/Prom.contpriradj_16][]
#View(Plazo.1[,.N, keyby = .(codigoProductoAdjudicado,difAdjContPri,Prom.contpriradj_16,Desv.contpriadj)][])
#View(Plazo.1[codigoProductoAdjudicado_16 == "1010151692156358",.(codigoProductoAdjudicado,difAdjContPri,Prom.contpriradj_16,Desv.contpriadj)])
Plazo.1[,.N, keyby = Desv.contpriadj][order(-N)][,Porcentaje := (N/sum(N))*100][]
#View(Plazo.1[Desv.contpriadj == max(Desv.contpriadj),.(codigoProductoAdjudicado,difAdjContPri,Prom.contpriradj_16,Desv.contpriadj)])
#View(Plazo.1)
summary(Plazo.1$Desv.contpriadj) #Existen 2955 NA?s

names(Plazo.1)


summary(Plazo.1$Desv.inicioadju) #Hay 7 NA
summary(Plazo.1$Desv.contprirec) #Hay 169277 NA
summary(Plazo.1$Desv.contultrec) #Hay 220784 NA
summary(Plazo.1$Desv.contultadj) #Hay 206752 NA
summary(Plazo.1$Desv.contpriadj) #Hay 2955 NA

summary(Plazo.1$cantModif) #Hay 2955 NA
Plazo.1[,.N, keyby = cantModif][order(-N)][,Porcentaje := (N/sum(N))*100][]


### Indicador 4
####
#### Propuesta de indicador
####

#Diferencia entre adjudicaci?n y primer contrato
Plazo.1[, difIniContPri := difIniAdj + difAdjContPri][]

Promedio_Ini_ContPri_16 <- Plazo.1[, .(Prom.Ini_ContPri_16 = mean(difIniContPri,na.rm = TRUE)), keyby = .(codigoProductoAdjudicado_16)][]
#uniqueN(Promedio_Ini_ContPri_16,by = "codigoProductoAdjudicado_16")

#Uniendo los datos
Plazo.1 <- merge(x = Plazo.1,y = Promedio_Ini_ContPri_16,by = "codigoProductoAdjudicado_16",all.x = TRUE)

Plazo.1[, Desv.IniContPri := difIniContPri/Prom.Ini_ContPri_16][]

Plazo.1[,.N, keyby = .(codigoProductoAdjudicado,difAdjContUlt,Prom.Ini_ContPri_16,Desv.IniContPri)][]
#View(Plazo.1[codigoProductoAdjudicado_16 == "1010151692156358",.(codigoProductoAdjudicado,difAdjContUlt,Prom.Ini_ContPri_16,Desv.contultadj)])
Plazo.1[,.N, keyby = Desv.IniContPri][order(-N)][,Porcentaje := (N/sum(N))*100][]
#View(Plazo.1[Desv.IniContPri == max(Desv.IniContPri),.(codigoProductoAdjudicado,difAdjContUlt,Prom.Ini_ContPri_16,Desv.contultadj)])
#View(Plazo.1)
summary(Plazo.1$Desv.IniContPri) #Existe 1 NA

Plazo.1$CantModf_Plazo<-Plazo.1$cantModif


#View(Plazo.1[,.N, keyby = .(numeroActo,numeroOferta,idLinea,codigoProductoAdjudicado,Desv.inicioadju,Desv.contprirec,Desv.contultrec,Desv.contultadj,Desv.contpriadj,Desv.IniContPri,CantModf_Plazo)][])
Ind.Plazo <- Plazo.1[, .(numeroActo,numeroOferta,idLinea,Desv.inicioadju,Desv.contprirec,Desv.contultrec,Desv.contultadj,Desv.contpriadj,Desv.IniContPri,CantModf_Plazo)][]

# summary(Desv.contultadj)

# Manteniendo solamente las variables de identificacion y los indicadores

uniqueN(x = Plazo.1,by = c("numeroActo","numeroOferta","idLinea"))
Indicadores_Plazo <- Plazo.1[, .(numeroActo,numeroOferta,idLinea,Desv.inicioadju,Desv.contprirec,Desv.contultrec,Desv.contultadj,Desv.contpriadj,Desv.IniContPri,CantModf_Plazo)][]

#fwrite(x = Indicadores_Plazo,file = "Plazo_Indicadores_V2023_V2.csv",sep = ";",dec = ",")

    #### LIMPIEZA DE DATOS
    # Lista de archivos a conservar
    objetos_a_conservar <- c("Indicadores_Plazo", "SICOP", "tablas")
    objetos_totales <- ls()
    
    # Obtener la lista de objetos a eliminar
    objetos_a_eliminar <- setdiff(objetos_totales, objetos_a_conservar)
    rm(list = objetos_a_eliminar)


## Indicadores Alcance -------------
Alcance <- data.table(sqlFetch(SICOP, "ds.alcance",as.is=TRUE))

#dim(Alcance)
#names(Alcance)
#str(Alcance)
attach(Alcance)

#Definiendo los valores como num?ricos
Alcance$cantidadSolicitada<- as.numeric(Alcance$cantidadSolicitada)
Alcance$cantidadAdjudicada<- as.numeric(Alcance$cantidadAdjudicada)
Alcance$cantidadContratadaBase<- as.numeric(Alcance$cantidadContratadaBase)
Alcance$cantidadContratadaMas<- as.numeric(Alcance$cantidadContratadaMas)
Alcance$cantidadContratadaMenos<- as.numeric(Alcance$cantidadContratadaMenos)
Alcance$cantidadContratada<- as.numeric(Alcance$cantidadContratada)
Alcance$cantidadRecibida<- as.numeric(Alcance$cantidadRecibida)
Alcance$cantModif<- as.numeric(Alcance$cantModif)

#Convirtiendo los c?digos de producto de 24 a 16
Alcance$codigoProductoAdjudicado_16<- substr(Alcance$codigoProductoAdjudicado, 1, 16)
Alcance$codigoProductoContratado_16<- substr(Alcance$codigoProductoContratado, 1, 16)
Alcance$codigoProductoRecibido_16<- substr(Alcance$codigoProductoRecibido, 1, 16)


#limpiando los datos para incluir solo datos desde el 2018, y solo bienes
Alcance<- Alcance[(anno < 2023 & anno > 2019),]
Alcance[, tipo_bien:= substr(codigoProductoSolicitado,1,1)]
Alcance<- Alcance[Alcance$tipo_bien%in%c("1","2","3","4","5","6"), ]

# Calculo de indicadores 
### Indicador 1
# 1. Raz?n entre cantidad recibida y cantidad adjudicada 
# Se calcula la desviacion entre cada una de las adjudicaciones y su cantidad adjudicada y la cantidad recibida
Alcance[, Raz.cant.recibida.contratada := cantidadContratada/cantidadRecibida][]
#PROPUESTA PARA MANEJAR LOS VALORES INFINITOS EN LOS DATOS
Alcance[, Raz.cant.recibida.contratada := ifelse(is.infinite(Raz.cant.recibida.contratada), 
                                                 cantidadContratada, 
                                                 Raz.cant.recibida.contratada)]

Alcance[,.N, keyby = .(codigoProductoAdjudicado_16,cantidadContratada,cantidadRecibida,Raz.cant.recibida.contratada)][]
#View(Alcance[codigoProductoAdjudicado_16 == "1010150792256375",.(codigoProductoAdjudicado_16,cantidadContratada,cantidadRecibida,Raz.cant.recibida.contratada)])
#View(Alcance[codigoProductoAdjudicado_16 == "1010151192197092",.(codigoProductoAdjudicado_16,cantidadContratada,cantidadRecibida,Raz.cant.recibida.contratada)])
#Alcance[,.N, keyby = Raz.cant.recibida.contratada][order(-N)][,Porcentaje := (N/sum(N))*100][]
#View(Alcance[Raz.cant.recibida.contratada == max(Raz.cant.recibida.contratada),.(codigoProductoAdjudicado_16,cantidadContratada,cantidadRecibida,Raz.cant.recibida.contratada)])
#View(Alcance)
summary(Alcance$Raz.cant.recibida.contratada) # Hay en total 169243 NA's

### Indicador 2
# 2. Raz?n entre cantidad recibida y cantidad solicitada
Alcance[, Raz.recibida.solicitada := cantidadSolicitada/cantidadRecibida][]
#PROPUESTA PARA MANEJAR LOS VALORES INFINITOS EN LOS DATOS
Alcance[, Raz.recibida.solicitada := ifelse(is.infinite(Raz.recibida.solicitada), 
                                            cantidadSolicitada, 
                                            Raz.recibida.solicitada)]

Alcance[,.N, keyby = .(codigoProductoAdjudicado_16,cantidadSolicitada,cantidadRecibida,Raz.recibida.solicitada)][]
#View(Alcance[codigoProductoAdjudicado_16 == "1010150792256382",.(codigoProductoAdjudicado_16,cantidadSolicitada,cantidadRecibida,Raz.recibida.solicitada)])
#View(Alcance[codigoProductoAdjudicado_16 == "1010150792256375",.(codigoProductoAdjudicado_16,cantidadSolicitada,cantidadRecibida,Raz.recibida.solicitada)])
Alcance[,.N, keyby = Raz.recibida.solicitada][order(-N)][,Porcentaje := (N/sum(N))*100][]
#View(Alcance[Raz.recibida.solicitada == max(Raz.recibida.solicitada),.(codigoProductoAdjudicado_16,cantidadSolicitada,cantidadRecibida,Raz.recibida.solicitada)])
#View(Alcance)
summary(Alcance$Raz.recibida.solicitada) # 169 243, son NA

### Indicador 3
# 3. Codigo de producto recibido es igual a codigo de producto contratado.
Alcance[, Cod.Cont.Rec := ifelse(codigoProductoContratado_16 == codigoProductoRecibido_16,1,0)][]
#View(Alcance)
summary(Alcance$Cod.Cont.Rec)# todos son 1
#Alcance[Cod.Cont.Rec != 1,]

#4 Cantidad de procedimientos adjudicados del mismo bien en la misma instituci?n durante el a?o
names(Alcance)

Alcance.1 <- unique(x = Alcance[,c(4,5,6,22), with = FALSE], by = c("anno", "idInstitucion","codigoProductoAdjudicado_16","idProcedimiento"))

Alcance.1[, Compras.unico := .N, by = .(anno, idInstitucion,codigoProductoAdjudicado_16)][order(-Compras.unico)]
#View(Alcance.1)
#View(Alcance.1[anno == 2019 & idInstitucion == "3014042056" & codigoProductoAdjudicado_16 == "1111170192032281"])

# 5. Raz?n entre cantidad adjudicada y cantidad contratada
Alcance[, Raz.adjudicada.contratada := cantidadAdjudicada/cantidadContratada][]

#PROPUESTA PARA MANEJAR LOS VALORES INFINITOS EN LOS DATOS
Alcance[, Raz.adjudicada.contratada := ifelse(is.infinite(Raz.adjudicada.contratada), cantidadAdjudicada, 
                                              Raz.adjudicada.contratada)]

Alcance[,.N, keyby = .(codigoProductoAdjudicado_16,cantidadAdjudicada,cantidadContratada,Raz.adjudicada.contratada)][]
#View(Alcance[codigoProductoAdjudicado_16 == "1010150792256382",.(codigoProductoAdjudicado_16,cantidadSolicitada,cantidadRecibida,Raz.recibida.solicitada)])
#View(Alcance[codigoProductoAdjudicado_16 == "1010150792256375",.(codigoProductoAdjudicado_16,cantidadSolicitada,cantidadRecibida,Raz.recibida.solicitada)])
Alcance[,.N, keyby = Raz.adjudicada.contratada][order(-N)][,Porcentaje := (N/sum(N))*100][]
#View(Alcance[Raz.recibida.solicitada == max(Raz.recibida.solicitada),.(codigoProductoAdjudicado_16,cantidadSolicitada,cantidadRecibida,Raz.recibida.solicitada)])
#View(Alcance)
summary(Alcance$Raz.adjudicada.contratada) #


# 6. Raz?n entre cantidad solicitada y cantidad contratada
Alcance[, Raz.solicitada.contratada := cantidadSolicitada/cantidadContratada][]

#PROPUESTA PARA MANEJAR LOS VALORES INFINITOS EN LOS DATOS
Alcance[, Raz.solicitada.contratada := ifelse(is.infinite(Raz.solicitada.contratada), cantidadSolicitada, 
                                              Raz.solicitada.contratada)]


Alcance[,.N, keyby = .(codigoProductoAdjudicado_16,cantidadSolicitada,cantidadContratada,Raz.solicitada.contratada)][]
#View(Alcance[codigoProductoAdjudicado_16 == "1010150792256382",.(codigoProductoAdjudicado_16,cantidadSolicitada,cantidadRecibida,Raz.recibida.solicitada)])
#View(Alcance[codigoProductoAdjudicado_16 == "1010150792256375",.(codigoProductoAdjudicado_16,cantidadSolicitada,cantidadRecibida,Raz.recibida.solicitada)])
Alcance[,.N, keyby = Raz.solicitada.contratada][order(-N)][,Porcentaje := (N/sum(N))*100][]
#View(Alcance[Raz.recibida.solicitada == max(Raz.recibida.solicitada),.(codigoProductoAdjudicado_16,cantidadSolicitada,cantidadRecibida,Raz.recibida.solicitada)])
#View(Alcance)
summary(Alcance$Raz.solicitada.contratada)

names(Alcance)
#numeroActo == 420698
#Alcance[numeroOferta== "D20181108085627350215416889870660",][]
#idlinea 69

# 7. Codigo de producto adjudicado es igual a codigo de producto contratado.
Alcance[, Cod.Cont.Adj := ifelse(codigoProductoContratado_16 == codigoProductoAdjudicado_16,1,0)][]
#View(Alcance)
summary(Alcance$Cod.Cont.Adj)# todos son 1
#Alcance[Cod.Cont.Adj != 1,]

names(Alcance)

Alcance.2 <- unique(x = Alcance.1, by = c("anno", "idInstitucion","codigoProductoAdjudicado_16"))

Alcance.3 <- merge(x = Alcance, y = Alcance.2[,-3,with = FALSE], by = c("anno", "idInstitucion","codigoProductoAdjudicado_16"),all.x = TRUE)
names(Alcance.3)
Alcance.3$CantModf_Alcance<-Alcance.3$cantModif
#View(Alcance.3)

# Manteniendo solamente las variables de identificacion y los indicadores
uniqueN(x = Alcance.3,by = c("numeroActo","numeroOferta","idLinea"))

Indicadores_Alcance <- Alcance.3[,.(numeroActo,numeroOferta,idLinea,
                                    Raz.cant.recibida.contratada,
                                    Raz.recibida.solicitada,
                                    Cod.Cont.Rec, Compras.unico, 
                                    Raz.adjudicada.contratada, 
                                    Raz.solicitada.contratada,
                                    Cod.Cont.Adj,
                                    CantModf_Alcance)]

      #### LIMPIEZA DE DATOS
      # Lista de archivos a conservar
      objetos_a_conservar <- c("Indicadores_Plazo", "Indicadores_Alcance", "SICOP", "tablas")
      objetos_totales <- ls()
      
      # Obtener la lista de objetos a eliminar
      objetos_a_eliminar <- setdiff(objetos_totales, objetos_a_conservar)
      rm(list = objetos_a_eliminar)

## Indicadores Costo -----------
Costos <- data.table(sqlFetch(SICOP, "ds.costos",as.is=TRUE))
LineasOfertadas <- data.table(sqlFetch(SICOP, "dbo.LineasOfertadas",as.is=TRUE))
Ofertas <- data.table(sqlFetch(SICOP, "dbo.Ofertas",as.is=TRUE))
      
# Definir las variables que son caracteres 
glimpse(Costos)

uniqueN(x = Costos,by = c("numeroActo","numeroOferta","idLinea")) #321640

Costos[,.N, keyby = c("numeroActo","numeroOferta","idLinea")][order(-N)][]


#FILTRAR POR A?O DE 2018 - 2020, AGREGAR VARIABLE DE PRIMER DIGITO
#Y QUITAR 7,8,9 PORQUE EST?N EN SERVICIOS

Costos <- Costos[(anno < 2023 & anno > 2019),]
Costos[ ,tipo_bien := substr(codigoProductoSolicitado,1,1)]
Costos <- Costos[Costos$tipo_bien %in% c("1","2","3","4","5","6"),]

dim(Costos)
uniqueN(x = Costos,by = c("numeroActo","idLinea","numeroOferta")) #
#las primeras 2 91-99 menos el 95, o sea el 95 SI lo queremos

# 3.Distribucion y correlaciones
Costos$precioUnitarioEstimado<- as.numeric(Costos$precioUnitarioEstimado)
Costos$precioUnitarioOfertado<- as.numeric(Costos$precioUnitarioOfertado)
Costos$precioUnitarioAdjudicado<- as.numeric(Costos$precioUnitarioAdjudicado)
Costos$precioUnitarioMenorColones<- as.numeric(Costos$precioUnitarioMenorColones)
Costos$tipoCambioCrcOfertado<- as.numeric(Costos$tipoCambioCrcOfertado)
Costos$tipoCambioDolarOfertado<- as.numeric(Costos$tipoCambioDolarOfertado)
Costos$tipoCambioCrcAdjudicado<- as.numeric(Costos$tipoCambioCrcAdjudicado)
Costos$tipoCambioDolarAdjudicado<- as.numeric(Costos$tipoCambioDolarAdjudicado)


#Convirtiendo los c?digos de producto de 24 a 16
Costos$codigoProductoAdjudicado_16<- substr(Costos$codigoProductoAdjudicado, 1, 16)
Costos$codigoProductoOfertado_16<- substr(Costos$codigoProductoOfertado, 1, 16)

#cantidad de productos a 16 digitos
#length(unique(Costos$codigoProductoAdjudicado_16)) #85218

#summary(Costos)

# Pasando los datos a colones 
### dolares
Costos[,.N, keyby = .(tipoMonedaOfertada)][order(-N)][]
Costos[,.N, keyby = .(tipoMonedaAdjudicada)][order(-N)][]
Costos[tipoMonedaOfertada=="USD",.N, keyby = .(tipoCambioDolarOfertado)][]
Costos[tipoMonedaAdjudicada=="USD",.N, keyby = .(tipoCambioDolarAdjudicado)][]

#":=" para varias nuevas columnas
#cambiar precio OFERTADO a colones no importa el tipo de moneda
Costos[,":=" (Prec.Unit.Ofer.Col = ifelse(tipoMonedaOfertada == "CRC",
                                          yes = precioUnitarioOfertado,
                                          no = precioUnitarioOfertado*tipoCambioCrcOfertado*(1/tipoCambioDolarOfertado)))][]

nrow(Costos[Prec.Unit.Ofer.Col == 0]) #son 33 #puede ser que esten regalando estas lineas
#porque los descuentos son iguales al precio

#tabla de frecuencias por Precio Unitario Ofertado en colones
Costos[,.N, keyby = .(Prec.Unit.Ofer.Col)][]
#Cuando la oferta es 0 y la modalidad es Cantidad definida, son 4 casos
Prec.Ofer.0 <- Costos[Prec.Unit.Ofer.Col==0 & modalidadProcedimiento=="Cantidad definida"][]
#View(Prec.Ofer.0)

#las modalidades de los procedimientos en tabla
Costos[,.N, keyby = .(modalidadProcedimiento)][] #tipo de procedimiento es diferente


Verif.Prec.Unit.Ofer.Col <- Costos[,.(tipoMonedaOfertada,Prec.Unit.Ofer.Col,precioUnitarioOfertado,tipoCambioCrcOfertado,tipoCambioDolarOfertado)][]
Verif.Prec.Unit.Ofer.Col[tipoMonedaOfertada=="EUR"][]

#cambiar a colones el ADJUDICADO:
Costos[,Prec.Unit.Adj.Col := ifelse(tipoMonedaAdjudicada == "CRC",precioUnitarioAdjudicado,precioUnitarioAdjudicado*tipoCambioCrcAdjudicado*(1/tipoCambioDolarAdjudicado))][]
Costos[,.N, keyby = .(Prec.Unit.Adj.Col)][]

#Costos[, Prec.Unit.Adj.Col := Prec.Unit.Adj.Col - 
#         as.numeric(descuentoAdjudicado) + 
#         as.numeric(ivaAdjudicado) + 
#         as.numeric(otrosImpuestosAdjudicado) + 
#         as.numeric(acarreosAdjudicado)]

Prec.Adj.0 <- Costos[Prec.Unit.Adj.Col==0 & modalidadProcedimiento=="Cantidad definida"][]
nrow(Prec.Adj.0) #son 0

Costos[Prec.Unit.Adj.Col==0 & precioUnitarioEstimado==0][] #cero igual

#estos son 0
Verif.Prec.Unit.Adj.Col <- Costos[,.(tipoMonedaAdjudicada,Prec.Unit.Adj.Col,precioUnitarioAdjudicado,tipoCambioCrcAdjudicado,tipoCambioDolarAdjudicado)][]
Verif.Prec.Unit.Adj.Col[tipoMonedaAdjudicada=="EUR"][]


# Indicadores

# 1. Grado de desviaci?n del monto unitario adjudicado de la linea con respecto al resto de las compras u ofertas del mismo bien ##

# Se hace en dos pasos (podria hacerse en uno solo pero se mantiene en dos para mostrar mas informacion) 
# Primero se calcula el precio promedio del bien 
Promedio_Costos <- Costos[, .(Prom.precioUnitarioAdjudicado = mean(Prec.Unit.Adj.Col,na.rm = TRUE),
                              Conteo_Adjudicado = length(Prec.Unit.Adj.Col)), 
                          keyby = .(codigoProductoAdjudicado_16)][]

uniqueN(Promedio_Costos,by = "codigoProductoAdjudicado_16") #esto calza con los unicos de costosCostos[,Prec.Unit.Adj.Col := ifelse(tipoMonedaAdjudicada == "CRC",precioUnitarioAdjudicado,precioUnitarioAdjudicado*tipoCambioCrcAdjudicado*(1/tipoCambioDolarAdjudicado))][]


# *ver esta interpretacion*
Promedio_Costos[,.N, keyby = Conteo_Adjudicado][, Porcentaje := (N/sum(N))*100][] #61% de las veces se ha adjudicado con un precio unico

#cambiar a dolares con tipo de cambio
LineasOfertadas[, precio_unitario_ofertado_colones := ifelse(tipo_moneda == "CRC", as.numeric(precio_unitario_ofertado),as.numeric(precio_unitario_ofertado)*as.numeric(tipo_cambio_crc)*(1/as.numeric(tipo_cambio_dolar)))]


# Calculando la variable del Monto por linea en colones en lineas OFERTADAS
#es que los precios de referencia no tienen ni iva ni descuento y acarreos
#LineasOfertadas[, Monto_linea_colones := as.numeric(cantidad_ofertada) * precio_unitario_ofertado_colones - as.numeric(descuento) + as.numeric(iva) + as.numeric(otros_impuestos) + as.numeric(acarreos)]
#LineasOfertadas[, Monto_linea_colones := precio_unitario_ofertado_colones - as.numeric(descuento) + as.numeric(iva) + as.numeric(otros_impuestos) + as.numeric(acarreos)]
#summary(LineasOfertadas$Monto_linea_colones)

# 453 553 564 464 000 

#Se une lineas adjudicadas con procedimientos

Prel_0 <- merge(LineasOfertadas, Ofertas[,.(nro_sicop, nro_oferta, elegible)], 
                by = c("nro_sicop","nro_oferta"), 
                all.x = TRUE) #para agregar si es elegible o no

Prel_1 <- Prel_0[(elegible == "Sí"),] #filtrar filas elegibles

rm(Prel_0) #para que no se pegue tanto la memoria

Prel_1 <- Prel_1[, codigo_producto_16 := substr(codigo_producto,1,16)]

Precios_ofertados<- Prel_1[,.(Promedio_ofertados =mean(precio_unitario_ofertado_colones, na.rm = TRUE), 
                              Conteo_Oferta=length(precio_unitario_ofertado_colones)), 
                           keyby=.(codigo_producto_16)]

#ofertados unicos son el 40%
Precios_ofertados[,.N, keyby = Conteo_Oferta][, Porcentaje := (N/sum(N))*100][]

#base con promedios de adjudicados y ofertados
Precios_compilados <- merge(x = Promedio_Costos,
                            y = Precios_ofertados, 
                            by.x = "codigoProductoAdjudicado_16", 
                            by.y = "codigo_producto_16", 
                            all.x = TRUE )

#head(Precios_compilados)

#Precios_compilados[,.N, keyby = Conteo_Adjudicado][, Porcentaje := (N/sum(N))*100][] #para el 61% de los productos se ha hecho una sola adjudicacion
#Precios_compilados[,.N, keyby = Conteo_Oferta][, Porcentaje := (N/sum(N))*100][] #para el 31% de los productos se ha hecho una sola oferta

# Pegando a Costos.completo el promedio del codigoProductoAdjudicado
Costos.1 <- merge(x = Costos,y = Precios_compilados,by = "codigoProductoAdjudicado_16",all.x = TRUE)

#verificar que tienen la misma cantidad de lineas
#dim(Costos.1);dim(Costos)

#verificar que tengan la misma dimension
#uniqueN(x = Costos,by = c("numeroActo","numeroOferta","idLinea")) #224337
#uniqueN(x = Costos.1,by = c("numeroActo","numeroOferta","idLinea")) #224337

#verificar que son la misma cantidad de productos
#uniqueN(Costos.1, by = "codigoProductoAdjudicado_16") #85218
#uniqueN(Costos$codigoProductoAdjudicado_16) #85218

#ver cuantas veces se ha adjudicado cada producto
Costos.1[,.N, keyby = codigoProductoAdjudicado_16][, Porcentaje := (N/sum(N))*100][order(-N)]

#verificar con alguno que salio una sola vez en los compilados
#vs en Costos.1 que est? una sola vez en adjudicado y ofertado

Costos.1[,.N, keyby = Conteo_Adjudicado][, Porcentaje := (N/sum(N))*100][] #23% de las lineas tiene una sola adjudicacion
Costos.1[,.N, keyby = Conteo_Oferta][, Porcentaje := (N/sum(N))*100][] #11% de las lineas tiene un sola oferta
Costos.1[Conteo_Adjudicado != 1 & Conteo_Oferta == 1] #no puede haber mas adjudicaciones que ofertas
Costos.1[Conteo_Adjudicado > Conteo_Oferta] #cero, o sea todo bien

Costos.1[,.N, keyby = .(Prom.precioUnitarioAdjudicado)][] #frecuencia de promedios
summary(Costos.1$Prom.precioUnitarioAdjudicado)
summary(Costos.1$Promedio_ofertados)#hay 2 NA's, son subastas a baja

# AQUI ES DONDE SE CALCULA EL INDICADOR 1.
#Posteriormente se calcula la desviacion (que en realidad es una razon) entre cada una de las adjudicaciones y el precio promedio


#nuevo con ofertas condicional, ESTE ES EL INDICADOR QUE SI VAMOS A USAR:
Costos.1[, Desv.precioUnitarioAdjudicado := ifelse(Conteo_Adjudicado >= 3 ,
                                                   Prec.Unit.Adj.Col/Prom.precioUnitarioAdjudicado,
                                                   Prec.Unit.Adj.Col/((Prom.precioUnitarioAdjudicado+Promedio_ofertados)/2))]

#cuanto el conteo de ofertados y adjudicados es 1 el indicador tiene que ser 1!
nrow(Costos.1[Desv.precioUnitarioAdjudicado == 1]) #hay 1

#summary(Costos.1$Desv.precioUnitarioGrupo) #hay 2 NA's
summary(Costos.1$Desv.precioUnitarioAdjudicado) 


Costos.1[,.N, keyby = .(codigoProductoAdjudicado_16,Prec.Unit.Adj.Col,Prom.precioUnitarioAdjudicado,Desv.precioUnitarioAdjudicado)][]
#ejemplos
#View(Costos.1[codigoProductoAdjudicado_16 == "1010150792256375",.(Prec.Unit.Adj.Col,Prom.precioUnitarioAdjudicado,Desv.precioUnitarioAdjudicado,Desv.precioUnitarioGrupo)])
#View(Costos.1[codigoProductoAdjudicado_16 == "1010150792256378",.(Prec.Unit.Adj.Col,Prom.precioUnitarioAdjudicado,Desv.precioUnitarioAdjudicado,Desv.precioUnitarioGrupo)])

#Costos.1[,.N, keyby = Desv.precioUnitarioAdjudicado][order(-N)][,Porcentaje := (N/sum(N))*100][] #**MUY IMPORTANTE** ver las desviaciones de las compras que tienen el indicador en 1 pasa de un 24%
#Costos.1[,.N, keyby = Desv.precioUnitarioAdjudicado][order(-N)][,Porcentaje := (N/sum(N))*100][] #a un 6.24% aprox

#dim(Costos.1[Conteo_Adjudicado == 1 & Conteo_Oferta == 1])[1]/dim(Costos.1)[1]
#26551/224337 #donde la oferta y la adjudicacion son 1 es un 11%
#pero es diferente a los 1s del indicador
#puede ser el efecto de ponerle los promedios de los promedios

(Costos.1[Desv.precioUnitarioAdjudicado == max(Desv.precioUnitarioAdjudicado, na.rm = TRUE),.(Prec.Unit.Adj.Col,Prom.precioUnitarioAdjudicado,Desv.precioUnitarioAdjudicado)])

summary(Costos.1$Desv.precioUnitarioAdjudicado) # No tiene NA's
#summary(Costos.1$Desv.precioUnitarioGrupo) # Tiene 3

# 2. Grado de desviaci?n del monto unitario adjudicado de la linea con respecto a los precios de referencia (presupuestado en la solicitud)
# Tengo dudas sobre la variable precioUnitarioEstimado si es la q se debe usar o si debe ser precioUnitarioPresupuestado (pero esta no aparece)
# Este indicador responde a la gesti?n propia de las instituciones, no tanto a la dimensi?n de precio

#primero era as?, pero tiene infinitos
#Costos.1[,Desv.Monto.Unit.Adj := Prec.Unit.Adj.Col/precioUnitarioEstimado][]

Costos.1[, Desv.Monto.Unit.Adj := ifelse(precioUnitarioEstimado != 0 ,
                                         Prec.Unit.Adj.Col/precioUnitarioEstimado,
                                         Prec.Unit.Adj.Col)]

#Costos.1[,log.Desv.Monto.Unit.Adj := log(Desv.Monto.Unit.Adj)][]
summary(Costos.1$Desv.Monto.Unit.Adj)
#summary(Costos.1$log.Desv.Monto.Unit.Adj)
summary(Costos.1$Desv.Monto.Unit.Adj[is.infinite(Costos.1$Desv.Monto.Unit.Adj) == FALSE])
Costos.1[,.(Prec.Unit.Adj.Col,precioUnitarioEstimado,Desv.Monto.Unit.Adj)][]


Costos.1[,.N, keyby = .(Desv.Monto.Unit.Adj)][order(-N)][]

# 3. Monto adjudicado unitario de la linea se mantuvo igual al monto ofertado.

#Costos.1[,Monto.Adj.igual := Prec.Unit.Adj.Col/Prec.Unit.Ofer.Col]
Costos.1[, Monto.Adj.igual := ifelse(Prec.Unit.Ofer.Col == 0 ,
                                     Prec.Unit.Adj.Col,
                                     Prec.Unit.Adj.Col/Prec.Unit.Ofer.Col)]

summary(Costos.1$Monto.Adj.igual)
#View(Costos.1$Monto.Adj.igual)
#View(Costos.1[Monto.Adj.igual== 0,.(Prec.Unit.Adj.Col,Prec.Unit.Ofer.Col,Monto.Adj.igual)])# No hay
#View(Costos.1[Monto.Adj.igual== Inf,.(Prec.Unit.Adj.Col,Prec.Unit.Ofer.Col,Monto.Adj.igual)])# Problemas con esto, son 7
#si el denominador es cero entonces se pone el denominador

Costos.1[,.N, keyby = .(Monto.Adj.igual)][order(-N)][]

#verificacion de que las dimensiones sigan bien:
uniqueN(x = Costos,by = c("numeroActo","numeroOferta","idLinea")) #224337
uniqueN(x = Costos.1,by = c("numeroActo","numeroOferta","idLinea")) #224337

# 4. Grado de desviaci?n entre el monto de la linea adjudicada con respecto a la siguiente linea ofertada m?s baja
summary(Costos.1$precioUnitarioMenorColones)
#Costos.1[,Desv.Mas.Baja := Prec.Unit.Adj.Col/precioUnitarioMenorColones][]

Costos.1[, Desv.Mas.Baja := ifelse(precioUnitarioMenorColones == 0 ,
                                   Prec.Unit.Adj.Col,
                                   Prec.Unit.Adj.Col/precioUnitarioMenorColones)]

Costos.1[,.N, keyby = .(Desv.Mas.Baja)][]
summary(Costos.1$Desv.Mas.Baja)

nrow(Costos.1[Desv.Mas.Baja== 0,.(Prec.Unit.Adj.Col,precioUnitarioMenorColones,Desv.Mas.Baja)])# 0
nrow(Costos.1[Desv.Mas.Baja== Inf,.(Prec.Unit.Adj.Col,precioUnitarioMenorColones,Desv.Mas.Baja)])# 280
nrow(Costos.1[Desv.Mas.Baja== Inf,.(numeroActo,numeroOferta,idLinea,Prec.Unit.Adj.Col,precioUnitarioMenorColones,Desv.Mas.Baja)])# Problemas con esto

Menor.Desv.Baja <- Costos.1[Desv.Mas.Baja < 1][]# Es para ver los casos en los que el precioUnitarioMenorColones es mayor al adjudicado
Menor.Desv.Baja[,.N, keyby = .(tipoMonedaOfertada)][]

#verificar que tengan la misma dimension
uniqueN(x = Costos,by = c("numeroActo","numeroOferta","idLinea")) #224337
uniqueN(x = Costos.1,by = c("numeroActo","numeroOferta","idLinea")) #224337

# 5. Grado de desviaci?n del adjudicado vs el contratado. Como todos son adj/x lo voy a dejar
#SICOP <-odbcConnect("SICOP", uid="casp", pwd="Casp2021*")
#tablas <- sqlTables(SICOP, tableType = "TABLE")
contrataciones <- data.table(sqlFetch(SICOP, "dbo.LineasContratadas",as.is=TRUE))

contrataciones[, precio_unitario_contratado_colones := ifelse(tipo_moneda == "CRC", 
                                                              as.numeric(precio_unitario_contratado),
                                                              as.numeric(precio_unitario_contratado)*as.numeric(tipo_cambio_crc)*(1/as.numeric(tipo_cambio_dolar)))]


Costos.2 <- merge(x = Costos.1, y = contrataciones[,c("nro_acto","nro_sicop","nro_linea_cartel",
                                                      "precio_unitario_contratado_colones")], 
                  by.x = c("numeroActo","idProcedimiento","idLinea"),
                  by.y = c("nro_acto","nro_sicop","nro_linea_cartel"), 
                  all.x = TRUE )

#uniqueN(x = Costos,by = c("numeroActo","numeroOferta","idLinea")) #224337
#uniqueN(x = Costos.1,by = c("numeroActo","numeroOferta","idLinea")) #224337
#uniqueN(x = Costos.2,by = c("numeroActo","numeroOferta","idLinea")) #224337
#peeero
#dim(Costos.2) #247846, esto significa que no son unicas esas combinaciones que estamos haciendo

uniqueN(x = contrataciones, by = c("nro_acto","nro_sicop","nro_linea_contrato",
                                   "nro_linea_contrato","nro_linea_cartel")) #489964
#Es contratado/adjudicado
Costos.2[, nombre_unico:= do.call(paste, c(.SD, sep = "_")), .SDcols =  c("numeroActo","numeroOferta","idLinea")]
Costos.2[, Conteo_nombre := length(anno), keyby = .(nombre_unico)]
Costos.2[Conteo_nombre==1 & is.na(precio_unitario_contratado_colones), ]
#los precios ofertados unicos, no tienen NAs entonces los vamos a quitar
Costos.2 <- na.omit(Costos.2, cols=c("precio_unitario_contratado_colones"))
dim(Costos.2) #236444
#hay que volver a correr el conteo nombre porque quitamos los NA
Costos.2[, Conteo_nombre := length(anno), keyby = .(nombre_unico)]
Costos.2[,.N, keyby = Conteo_nombre][order(-N)][,Porcentaje := (N/sum(N))*100][]
#View(Costos.2[Conteo_nombre==8,])
#Comparar si hay contrataciones diferentes al promedio
Promedio_contratado<- Costos.2[, .(Prom.contr= mean(precio_unitario_contratado_colones,na.rm = TRUE)), 
                               keyby = c("numeroActo","numeroOferta","idLinea")][]

Costos.2 <- merge(x = Costos.2 , y = Promedio_contratado ,by = c("numeroActo","numeroOferta","idLinea") ,all.x = TRUE)

Costos.2[, Verif := ifelse(Prom.contr == precio_unitario_contratado_colones,
                           0,1)]

Costos.2[,.(Prom.contr, precio_unitario_contratado_colones, Verif)]

Costos.2[,.N, keyby = Verif][order(-N)][,Porcentaje := (N/sum(N))*100][]
#cuantas veces las contrataciones aparecen mas de una vez con precios distintos
#para un mismo procedimiento y linea
# ese 1.1% o 2612 se distribuyen asi:
Costos.2[Verif==1 ,.N, keyby = Conteo_nombre][order(-N)][,Porcentaje := (N/sum(N))*100][]
#verifiquemos que el promedio contratado es igual al precio contratado para los casos en que
#son unicos o 98.8% o 233 832
nrow(Costos.2[precio_unitario_contratado_colones == Prom.contr]) #aunque esto es igual al ifelse jeje

#le ponemos el precio contratado a COSTOS.3 para que no afecte la dimensionalidad
Costos.3 <- merge(x = Costos.1 ,
                  y = Promedio_contratado ,
                  by = c("numeroActo","numeroOferta","idLinea") ,
                  all.x = TRUE)

#Costos.1[,Desv.Adj.de.Cont := Prec.Unit.Adj.Col/][]


# Manteniendo solamente las variables de identificacion y los indicadores
uniqueN(x = Costos,by = c("numeroActo","numeroOferta","idLinea")) #224337
uniqueN(x = Costos.1,by = c("numeroActo","numeroOferta","idLinea")) #224337
uniqueN(x = Costos.2,by = c("numeroActo","numeroOferta","idLinea")) #224337
uniqueN(x = Costos.3,by = c("numeroActo","numeroOferta","idLinea")) #224337
uniqueN(x = Costos.2,by = c("Prom.contr")) #precios unicos

dim(Costos)  #224337   34
dim(Costos.1)#224337   43
dim(Costos.2)#236444   48
dim(Costos.3)#224337   44 :D

Costos.3[, Desv.Monto.Unit.Cont := (Prom.contr/Prec.Unit.Adj.Col)]

summary(Costos.3$Desv.Monto.Unit.Cont)

Indicadores_Costo <- Costos.3[,.(numeroActo,numeroOferta,
                                 idLinea,Desv.precioUnitarioAdjudicado,
                                 Desv.Monto.Unit.Adj,
                                 Monto.Adj.igual,
                                 Desv.Mas.Baja,
                                 Desv.Monto.Unit.Cont)]

names(Indicadores_Costo) <- c("numeroActo","numeroOferta","idLinea",
                              "desv.adj.vs.grupo",
                              "desv.adj.vs.estimado",
                              "monto.adj.vs.ofert",
                              "desv.adj.vs.oferta.mas.baja",
                              "desv.cont.vs.adj")      

          #### LIMPIEZA DE DATOS
          # Lista de archivos a conservar
          objetos_a_conservar <- c("Indicadores_Plazo", "Indicadores_Alcance", "Indicadores_Costo", 
                                   "LineasOfertadas", "Ofertas", "Costos", "SICOP", "tablas")
          objetos_totales <- ls()
          
          # Obtener la lista de objetos a eliminar
          objetos_a_eliminar <- setdiff(objetos_totales, objetos_a_conservar)
          rm(list = objetos_a_eliminar)

### Consolidación de Indicadores -----------
# Primero guardamos todos los archivos en una lista
lista <- list(Indicadores_Alcance, Indicadores_Costo, Indicadores_Plazo)

# Los unimos en uno solo mediante una funcion dentro del comando Reduce
Set_final <- Reduce(function(...) merge(..., by = c("numeroActo","numeroOferta","idLinea"), all = TRUE), lista)
#names(Set_final)
uniqueN(Set_final, by = c("numeroActo","numeroOferta","idLinea")) #Dim 224337

colSums(is.na(Set_final))

# Sin NA
Set_final_2 <- Set_final[,c(1,2,3,7,9,15:17,21,23)]
Set_Modelo_sin_NA <- Set_final_2[na.omit(Set_final_2)]
Set_Modelo_sin_NA <- Set_Modelo_sin_NA[,c(1:10)]

#dim(Set_Modelo_sin_NA)
#names(Set_Modelo_sin_NA)


            #### LIMPIEZA DE DATOS
            # Lista de archivos a conservar
            objetos_a_conservar <- c("Set_Modelo_sin_NA", "Indicadores_Plazo", "Indicadores_Alcance", "Indicadores_Costo", 
                                     "LineasOfertadas", "Ofertas", "Costos", "SICOP", "tablas")
            objetos_totales <- ls()
            
            # Obtener la lista de objetos a eliminar
            objetos_a_eliminar <- setdiff(objetos_totales, objetos_a_conservar)
            rm(list = objetos_a_eliminar)

# LIMPIEZA DE DATOS ----------------

## Acotación ----------------
datos<-Set_Modelo_sin_NA

#Estableciendo los percentiles para la validaci?n de resultados
Deciles_Compras.Unico <-quantile(datos$Compras.unico, probs = seq(0, 1, 0.01), na.rm = FALSE)
#Deciles_Raz.Adj.Contra <-quantile(datos$Raz.adjudicada.contratada, probs = seq(0, 1, 0.01), na.rm = FALSE)
Deciles_Raz.Solic.Contra <-quantile(datos$Raz.solicitada.contratada, probs = seq(0, 1, 0.01), na.rm = FALSE)
#Deciles_modif.Alcance <-quantile(datos$CantModf_Alcance, probs = seq(0, 1, 0.01), na.rm = FALSE)
#Deciles_Desv.Adj.Grup <-quantile(datos$desv.adj.vs.grupo, probs = seq(0, 1, 0.01), na.rm = FALSE)
#Deciles_Mont.Adj.Ofer <-quantile(datos$monto.adj.vs.ofert, probs = seq(0, 1, 0.01), na.rm = FALSE)
Deciles_Desv.Adj.Ofer.Baj <-quantile(datos$desv.adj.vs.oferta.mas.baja, probs = seq(0, 1, 0.01), na.rm = FALSE)
Deciles_Desv.Cont.Adju <-quantile(datos$desv.cont.vs.adj, probs = seq(0, 1, 0.01), na.rm = FALSE)
Deciles_Desv.Ini.Adj <-quantile(datos$Desv.inicioadju, probs = seq(0, 1, 0.01), na.rm = FALSE)
Deciles_Desv.Cont.Pri.Adj <-quantile(datos$Desv.contpriadj, probs = seq(0, 1, 0.01), na.rm = FALSE)
#Deciles_Desv.Ini.Cont.Pri <-quantile(datos$Desv.IniContPri, probs = seq(0, 1, 0.01), na.rm = FALSE)
Deciles_Cant.Mod.Plazo <-quantile(datos$CantModf_Plazo, probs = seq(0, 1, 0.01), na.rm = FALSE)

#En caso de querer observar los percentiles
#Deciles_Compras.Unico  
#Deciles_Raz.Adj.Contra
#Deciles_Raz.Solic.Contra
#Deciles_modif.Alcance
#Deciles_Desv.Adj.Grup
#Deciles_Mont.Adj.Ofer
#Deciles_Desv.Adj.Ofer.Baj 
#Deciles_Desv.Cont.Adju
#Deciles_Desv.Ini.Adj
#Deciles_Desv.Cont.Pri.Adj 
#Deciles_Desv.Ini.Cont.Pri 
#Deciles_Cant.Mod.Plazo


#names(datos)
datos[, Compras.unico.Acot:=ifelse(datos$Compras.unico > Deciles_Compras.Unico[100],Deciles_Compras.Unico[100]*2,Compras.unico)]
#datos[, Raz.adjudicada.contratada.Acot:=ifelse(datos$Raz.adjudicada.contratada > Deciles_Raz.Adj.Contra[100],Deciles_Raz.Adj.Contra[100]*2,Raz.adjudicada.contratada)]
datos[, Raz.solicitada.contratada.Acot:=ifelse(datos$Raz.solicitada.contratada > Deciles_Raz.Solic.Contra[100],Deciles_Raz.Solic.Contra[100]*2,Raz.solicitada.contratada)]
#datos[, CantModf_Alcance.Acot:=ifelse(datos$CantModf_Alcance > Deciles_modif.Alcance[100],Deciles_modif.Alcance[100]*2,CantModf_Alcance)]
#datos[, desv.adj.vs.grupo.Acot:=ifelse(datos$desv.adj.vs.grupo > Deciles_Desv.Adj.Grup[100],Deciles_Desv.Adj.Grup[100]*2,desv.adj.vs.grupo)]
#datos[, monto.adj.vs.ofert.Acot:=ifelse(datos$monto.adj.vs.ofert > Deciles_Mont.Adj.Ofer[100],Deciles_Mont.Adj.Ofer[100]*2,monto.adj.vs.ofert)]
datos[, desv.adj.vs.oferta.mas.baja.Acot:=ifelse(datos$desv.adj.vs.oferta.mas.baja > Deciles_Desv.Adj.Ofer.Baj[100],Deciles_Desv.Adj.Ofer.Baj[100]*2,desv.adj.vs.oferta.mas.baja)]
datos[, desv.cont.vs.adj.Acot:=ifelse(datos$desv.cont.vs.adj > Deciles_Desv.Cont.Adju[100],Deciles_Desv.Cont.Adju[100]*2,desv.cont.vs.adj)]
datos[, Desv.inicioadju.Acot:=ifelse(datos$Desv.inicioadju > Deciles_Desv.Ini.Adj[100],Deciles_Desv.Ini.Adj[100]*2,Desv.inicioadju)]
datos[, Desv.contpriadj.Acot:=ifelse(datos$Desv.contpriadj > Deciles_Desv.Cont.Pri.Adj[100],Deciles_Desv.Cont.Pri.Adj[100]*2,Desv.contpriadj)]
#datos[, Desv.IniContPri.Acot:=ifelse(datos$Desv.IniContPri > Deciles_Desv.Ini.Cont.Pri[100],Deciles_Desv.Ini.Cont.Pri[100]*2,Desv.IniContPri)]
datos[, CantModf_Plazo.Acot:=ifelse(datos$CantModf_Plazo > Deciles_Cant.Mod.Plazo[100],Deciles_Cant.Mod.Plazo[100]*2,CantModf_Plazo)]

datos_acotados<- datos[,-c(4:10)][]

## Normalización ----------------
datos_acotados[, Log_Compras.unico.Acot:=log(datos$Compras.unico +1)]
# datos_acotados[, Log_Raz.adjudicada.contratada.Acot:=log(datos$Raz.adjudicada.contratada.Acot+1)]
datos_acotados[, Log_Raz.solicitada.contratada.Acot:=log(datos$Raz.solicitada.contratada.Acot+1)]
# datos_acotados[, Log_CantModf_Alcance.Acot:=log(datos$CantModf_Alcance.Acot+1)]
# datos_acotados[, Log_desv.adj.vs.grupo.Acot:=log(datos$desv.adj.vs.grupo.Acot+1)]
# datos_acotados[, Log_monto.adj.vs.ofert.Acot:=log(datos$monto.adj.vs.ofert.Acot+1)]
datos_acotados[, Log_desv.adj.vs.oferta.mas.baja.Acot:=log(datos$desv.adj.vs.oferta.mas.baja.Acot+1)]
datos_acotados[, Log_desv.cont.vs.adj.Acot:=log(datos$desv.cont.vs.adj.Acot+1)]
datos_acotados[, Log_Desv.inicioadju.Acot:=log(datos$Desv.inicioadju.Acot+1)]
datos_acotados[, Log_Desv.contpriadj.Acot:=log(datos$Desv.contpriadj.Acot+1)]
# datos_acotados[, Log_Desv.IniContPri.Acot:=log(datos$Desv.IniContPri.Acot+1)]
datos_acotados[, Log_CantModf_Plazo.Acot:=log(datos$CantModf_Plazo.Acot+1)]

datos_Log<- datos_acotados[,-c(4:10)][]

# MODELOS ------------------
## RPCA -----------------
#Caso de an?lisis para los an?malos entre 2018 y 2021
datos <- datos_Log[,-c(1:3)]
# colSums(is.na(datos))
#summary(datos)
#str(datos)
#names(datos)


pca <- prcomp(x = datos, center = TRUE,scale. = TRUE)

# Examinando los eigenvalues y la varianza acumulada para cada componente
get_eig(X = pca)

# Graficando los porcentajes de varianza explicada
fviz_eig(
  X         = pca,
  choice    = "variance", 
  addlabels = TRUE,
  ncp       = 21,
  ylim      = c(0, 100),
  main      = "Porcentaje de varianza explicada por componente",
  xlab      = "Componente",
  ylab      = "Porcentaje de varianza explicada",
  labelsize = 12
)

# Graficando los porcentajes de varianza acumulada
ggplot(data = get_eig(X = pca),
       aes(x = as.factor(1:nrow(get_eig(X = pca))),
           y = cumulative.variance.percent,
           group = 1)
) +
  geom_col(fill = "steelblue", color = "steelblue") +
  geom_line(color = "black", linetype = "solid") + 
  geom_point(shape = 19, color = "black") +
  geom_text(aes(label = paste0(round(cumulative.variance.percent, 1), "%")),
            size = 3, vjust = -0.5, hjust = 0.7) +
  geom_hline(yintercept = 90, color = "firebrick", linetype = "dashed") +
  labs(title = "Porcentaje de varianza acumulada PCA",
       x = "componente",
       y = "Porcentaje de varianza acumulada") +
  theme_bw()

# Contribucion de cada variable a los componentes

# Contribucion a la primera componente.
p1 <- fviz_contrib(X = pca, choice = "var", axes = 1, top = 6)
# Contribucion a la segunda componente.
p2 <- fviz_contrib(X = pca, choice = "var", axes = 2, top = 6)
# Contribucion conjunta a las 3 primeras componentes.
p3 <- fviz_contrib(X = pca, choice = "var", axes = 1:7, top = 6)

ggpubr::ggarrange(p1, p2, p3, nrow = 1)

# Reconstruccion de las observaciones iniciales

reconstruct_prcomp <- function(pca, comp = NULL){
  
  # Esta funcion reconstruye las mismas observaciones con las que se ha creado el 
  # PCA empleando únicamente determinadas componentes principales.
  
  # Parameters

  # pca: "prcomp"
  #   objeto prcomp con los resultados del PCA.
  #   
  # comp: "numeric"
  #   componentes principales empleadas en la reconstruccion.
  #   
  # Return

  # "matrix" con la reconstruccion de cada observacion. Las dimensiones de la 
  # matriz son las mismas que las de la matriz o dataframe con el que se estreno
  # el objeto pca.
  
  # Si no se especifica comp, se emplean todas las componentes.
  if (is.null(comp)) {
    comp <- seq_along(pca$sdev)
  }
  
  # Reconstruccion
  recon <- as.matrix(pca$x[, comp]) %*% t(as.matrix(pca$rotation[, comp]))
  
  # Si se ha aplicado centrado o escalado se revierte la trasformacion.
  if (pca$scale[1] != FALSE) {
    recon <- scale(recon , center = FALSE, scale = 1/pca$scale)
  }
  if (pca$center[1] != FALSE) {
    recon <- scale(recon , center = -1*pca$center, scale = FALSE)
  }
  
  return(recon)
}

# Aplicando la reconstruccion 

# Reconstruccion empleando las 6 primeras componentes (90% de la varianza explicada)
reconstruccion_pca <- reconstruct_prcomp(pca = pca, comp = 1:6)

# Calculo del error cuadratico medio de recostruccion
error_reconstruccion_pca <- apply(X = (reconstruccion_pca - datos)^2, MARGIN = 1, FUN = mean)

# Distribucion del error de reconstruccion
tibble(error_reconstruccion_pca) %>%
  ggplot(aes(x = error_reconstruccion_pca)) +
  geom_density(fill = "steelblue") +
  labs(title = "Distribucion de los errores de reconstruccion (PCA)",
       x = "Error de reconstruccion") +
  theme_bw()

rpca  <- PcaCov(x = datos, cov.control = CovControlMcd(), scale = TRUE, signflip = TRUE)

# Examinando los eigenvalues y la varianza acumulada para cada componente
rpca_eigen_var <- tibble(
  componente = seq_len(rpca$k),
  varianza   = rrcov::getSdev(rpca)^2,
  porcnt_varianza = 100 * varianza / sum(varianza),
  porcnt_varianza_acumulada = cumsum(porcnt_varianza)
)
rpca_eigen_var

# Graficando los porcentajes de varianza explicada

ggplot(data = rpca_eigen_var,
       aes(x = as.factor(componente),
           y = porcnt_varianza,
           group = 1)
) +
  geom_col(fill = "steelblue", color = "steelblue") +
  geom_line(color = "black", linetype = "solid") + 
  geom_point(shape = 19, color = "black") +
  geom_text(aes(label = paste0(round(porcnt_varianza), "%")),
            size = 3, vjust = -0.5, hjust = 0.7) +
  lims(y = c(0, 100)) +
  labs(title = "Porcentaje de varianza por componente PCA robusto",
       x = "componente",
       y = "Porcentaje de varianza explicada") +
  theme_bw()

# Graficando los porcentajes de varianza acumulada

ggplot(data = rpca_eigen_var,
       aes(x = as.factor(componente),
           y = porcnt_varianza_acumulada,
           group = 1)
) +
  geom_col(fill = "steelblue", color = "steelblue") +
  geom_line(color = "black", linetype = "solid") + 
  geom_point(shape = 19, color = "black") +
  geom_text(aes(label = paste0(round(porcnt_varianza_acumulada), "%")),
            size = 3, vjust = -0.5, hjust = 0.7) +
  geom_hline(yintercept = 90, color = "firebrick", linetype = "dashed") +
  lims(y = c(0, 100)) +
  labs(title = "Porcentaje de varianza acumulada PCA robusto",
       x = "componente",
       y = "Porcentaje de varianza acumulada") +
  theme_bw()

# Reconstruccion de las observaciones iniciales

reconstruct_PcaCov <- function(pca, comp = NULL){
  
  # Esta funci?n reconstruye las mismas observaciones con las que se ha creado el 
  # PCA empleando ?nicamente determinadas componentes principales.
  
  # Parameters

  # pca: "PcaCov"
  #   objeto PcaCov con los resultados del PCA.
  #   
  # comp: "numeric"
  #   componentes principales empleadas en la reconstrucci?n.
  #   
  # Return

  # "matrix" con la reconstrucci?n de cada observaci?n. Las dimensiones de la 
  # matriz son las mismas que las de la matriz o dataframe con el que se estren?
  # el objeto pca.
  
  # Si no se especifica comp, se emplean todas las componentes.
  if (is.null(comp)) {
    comp <- seq_len(pca$k)
  }
  
  # Reconstrucci?n
  recon <- as.matrix(pca$scores[, comp]) %*% t(as.matrix(pca$loadings[, comp]))
  
  # Si se ha aplicado centrado o escalado se revierte la trasformaci?n.
  recon <- scale(recon , center = FALSE, scale = 1/pca$scale)
  recon <- scale(recon , center = -1*pca$center, scale = FALSE)
  
  return(recon)
}

# Aplicando la reconstruccion 

# Reconstrucción empleando solamente un componente (100% de la varianza explicada)

reconstruccion_rpca <- reconstruct_PcaCov(pca = rpca, comp = 2)

# Cálculo del error cuadrático medio de recostruccion

error_reconstruccion_rpca <- apply(X = (reconstruccion_rpca - datos)^2, MARGIN = 1, FUN = mean)

# Distribución del error de reconstrucción
tibble(error_reconstruccion_rpca) %>%
  ggplot(aes(x = error_reconstruccion_rpca)) +
  geom_density(fill = "steelblue") +
  labs(title = "Distribución de los errores de reconstrucción (PCA)",
       x = "Error de reconstrucción") +
  theme_bw()

# Se añade el error de reconstrucción al dataframe original.

datos$error_reconstruccion_pca <- error_reconstruccion_pca
datos$error_reconstruccion_rpca <- error_reconstruccion_rpca
names(datos)

summary(datos$error_reconstruccion_pca)
summary(datos$error_reconstruccion_rpca)

# Para determinar los percentiles y distribucion del error para establecer un corte en el cual 

Deciles_pca <-quantile(datos$error_reconstruccion_pca, probs = seq(0, 1, 0.05), na.rm = FALSE)
Deciles_pca   

Deciles_rpca <-quantile(datos$error_reconstruccion_rpca, probs = seq(0, 1, 0.05), na.rm = FALSE)
Deciles_rpca  

Deciles_pca[20]
Deciles_rpca[20]

datos<- as.data.table(datos)

datos[,Anomalia_PCA := ifelse(error_reconstruccion_pca >= Deciles_pca[20],1,0)]
datos[,Anomalia_RPCA := ifelse(error_reconstruccion_rpca >= Deciles_rpca[20],1,0)]

#names(dt)
setnames(datos,old = c("error_reconstruccion_pca","error_reconstruccion_rpca"),new = c("Score_Pca","Score_Rpca"))

Cruce_anomalias <- datos[,.N, keyby = .(Anomalia_PCA,Anomalia_RPCA)][,Porcentaje:=(N/sum(N))*100][]
Cruce_anomalias

Resultados_RPCA <- data.frame(numeroActo= datos_Log$numeroActo,
                              numeroOferta= datos_Log$numeroOferta,
                              idLinea= datos_Log$idLinea,
                              score_RPCA= datos$Score_Rpca,  
                              anomalia_RPCA = datos$Anomalia_RPCA,
                              stringsAsFactors = FALSE)
## KMEANS ------------
datos <- datos_Log[,-c(1:3)]

datos$score_KMEANS <- KNN_AGG(dataset=datos[,-c(1:3)], k_min=10, k_max=15)
# Sort and find index for most outlying observations
names(datos$score_KMEANS) <- 1:nrow(datos)
sort(datos$score_KMEANS, decreasing = TRUE)
# Inspect the distribution of outlier scores
hist(datos$score_KMEANS)

datos$log.score_KMEANS<-log(datos$score_KMEANS, base = exp(1))
summary(datos$score_KMEANS)
head(datos)

hist(datos$log.score_KMEANS)
summary(datos$log.score_KMEANS)
min(datos$log.score_KMEANS)
max(datos$log.score_KMEANS)
#View(datos)

#Identificaci?n de valores extremos
Deciles_KMEANS <-quantile(datos$score_KMEANS, probs = seq(0, 1, 0.05), na.rm = FALSE)
Deciles_KMEANS  


datos[, Anomalo_KMEANS := ifelse(score_KMEANS >= Deciles_KMEANS[20], 1, 0)]

sum(datos$Anomalo_KMEANS) #12107 aproximadamente el 5.4% de los datos
#dim(datos) #224337  observaciones
#summary(datos)


#KMEANS_anomalias <- datos[ ,c(8,9)]
#Creando la base para guardar los elementos
Resultados_KMEANS <- data.frame(numeroActo= datos_Log$numeroActo,
                                numeroOferta= datos_Log$numeroOferta,
                                idLinea= datos_Log$idLinea,
                                score_KMEANS= datos$score_KMEANS,  
                                anomalia_KMEANS = datos$Anomalo_KMEANS,
                                stringsAsFactors = FALSE)

## ISO ---------------
datos <- datos_Log[,-c(1:3)]

# Quitamos las columnas que no necesitamos para el algoritmo.
#sort(set_final_if$Desv.Monto.Unit.Adj, decreasing = TRUE)
#dim(set_final_ifo)

##### ANÁLISIS SCI FOREST 
iso_sci <- isolation.forest(datos, ndim=2,
                            ntrees=500,
                            nthreads=1,
                            prob_pick_pooled_gain=0,
                            prob_pick_avg_gain=1)

#Ver cuales son las filas con los valores más algo
pred_sci<- predict(iso_sci, datos)

#sort(pred, decreasing = TRUE)
max_sci<- max(pred_sci)

Deciles_sci <-quantile(pred_sci, probs = seq(0, 1, 0.05), na.rm = FALSE)
Deciles_sci   

pred_sci_0.5<- sum(Deciles_sci[20] > 0.5)

#Identificación de los valores extremos
datos$outlier_sci_0.5<- 1*(pred_sci > Deciles_sci[20])
sum(datos$outlier_sci_0.5)

#Creando la base para guardar los elementos
Resultados_ISO <- data.frame(numeroActo= datos_Log$numeroActo,
                                           numeroOferta= datos_Log$numeroOferta,
                                           idLinea= datos_Log$idLinea,
                                           anomalia_iso= datos$outlier_sci_0.5, 
                                           score_iso= pred_sci,  
                                           stringsAsFactors = FALSE)


## LOF ---------------
datos <- datos_Log[,-c(1:3)]

### calculate LOF score with a neighborhood of 3 points
datos$outlier_score <- LOF(dataset=datos[,-c(1:3)], k=10)

# Sort and find index for most outlying observations
names(datos$outlier_score) <- 1:nrow(datos)
sort(datos$outlier_score, decreasing = TRUE)

# Inspect the distribution of outlier scores
hist(datos$outlier_score)
summary(datos$outlier_score)

# Para determinar los percentiles y distribucion del error para establecer un corte en el cual 
Deciles_LOF <-quantile(datos$outlier_score, probs = seq(0, 1, 0.05), na.rm = TRUE)
Deciles_LOF 


datos[, Anomalia_LOF := ifelse(datos$outlier_score >= Deciles_LOF[20],1,0)]

#Reemplazando los NA por 0, considerando que son NA porque la densidad del punto es demasiado alta
datos[is.na(datos)] <- 0

Conteo_anomalias <- datos[,.N, keyby = .(Anomalia_LOF)][,Porcentaje:=(N/sum(N))*100][]
Conteo_anomalias

#setwd("G:/Unidades compartidas/Fiscalizaci?n Preventiva de Compras P?blicas/Corrida_prueba_modelos/Resultados de los Modelos")
names(datos)


#Creando la base para guardar los elementos
Resultados_LOF <- data.frame(numeroActo= datos_Log$numeroActo,
                                           numeroOferta= datos_Log$numeroOferta,
                                           idLinea= datos_Log$idLinea,
                                           anomalia_LOF= datos$Anomalia_LOF, 
                                           score_LOF= datos$outlier_score,  
                                           stringsAsFactors = FALSE)


              #### LIMPIEZA DE DATOS
              # Lista de archivos a conservar
              objetos_a_conservar <- c("Set_Modelo_sin_NA", 
                                       "Indicadores_Plazo", "Indicadores_Alcance", "Indicadores_Costo",
                                       "Resultados_ISO","Resultados_KMEANS","Resultados_RPCA","Resultados_LOF",
                                       "LineasOfertadas", "Ofertas", "Costos", "SICOP", "tablas")
              objetos_totales <- ls()
              
              # Obtener la lista de objetos a eliminar
              objetos_a_eliminar <- setdiff(objetos_totales, objetos_a_conservar)
              rm(list = objetos_a_eliminar)
              

# SELECCIÓN FINAL -----------
## Instituciones relacionadas -----------
              
Instituciones <- data.table(sqlFetch(SICOP, "dbo.Instituciones",as.is=TRUE))
lineasadjudicadas <- data.table(sqlFetch(SICOP, "dbo.lineasadjudicadas",as.is=TRUE))
procedimientos <- data.table(sqlFetch(SICOP, "dbo.procedimientos",as.is=TRUE))
#jerarquiaClasificacionBS <- data.table(sqlFetch(SICOP, "dbo.jerarquiaClasificacionBS",as.is=TRUE))
#productobs <- data.table(sqlFetch(SICOP, "dbo.productobs",as.is=TRUE))
#adjudicaciones <- data.table(sqlFetch(SICOP, "dbo.Adjudicaciones",as.is=TRUE))
# LineasOfertadas <- data.table(sqlFetch(SICOP, "dbo.LineasOfertadas",as.is=TRUE))
# Ofertas <- data.table(sqlFetch(SICOP, "dbo.Ofertas",as.is=TRUE))
              
#names(Ofertas)
#names(lineasadjudicadas)
#names(procedimientos)
#names(Instituciones)
              
              
Prel_0 <- merge(lineasadjudicadas, procedimientos[,.(nro_sicop, nro_procedimiento, cedula_institucion)], by = "nro_sicop", all.x = TRUE)
Prel_1 <- merge(Prel_0, Instituciones[,.(cedula, nombre_institucion)], by.x = "cedula_institucion", by.y = "cedula", all.x = TRUE)
names(Prel_1)
head(Prel_1)
Prel_1[, anno:=substr(nro_procedimiento,1,4)]
              
Identificador_Instituciones <- Prel_1[, .(cedula_institucion, nro_acto, nro_oferta, nro_linea, anno, nombre_institucion)]
uniqueN(Identificador_Instituciones, by=c("nro_acto","nro_oferta","nro_linea"))
Identificador_Instituciones<- Identificador_Instituciones[(anno < 2023 & anno > 2018),]
colnames(Identificador_Instituciones)<- c("idInstitucion","numeroActo","numeroOferta","idLinea", "anno","Nombre_Institucion")
              
                          
            #### LIMPIEZA DE DATOS
            # Lista de archivos a conservar
            objetos_a_conservar <- c("Set_Modelo_sin_NA", 
                                     "Indicadores_Plazo", "Indicadores_Alcance", "Indicadores_Costo",
                                     "Resultados_ISO","Resultados_KMEANS","Resultados_RPCA","Resultados_LOF",
                                     "Identificador_Instituciones",
                                     "lineasadjudicadas","procedimientos","Instituciones",
                                     "LineasOfertadas", "Ofertas", "Costos", "SICOP", "tablas")
            objetos_totales <- ls()
            
            # Obtener la lista de objetos a eliminar
            objetos_a_eliminar <- setdiff(objetos_totales, objetos_a_conservar)
            rm(list = objetos_a_eliminar)

## Consolidación anomalías -------------------
lista <- list(Resultados_ISO, Resultados_KMEANS, Resultados_LOF, Resultados_RPCA)

# Los unimos en uno solo mediante una funcion dentro del comando Reduce
Set_final <- Reduce(function(...) merge(...,by = c("numeroActo","numeroOferta","idLinea"), all = TRUE), lista)
names(Set_final)
uniqueN(Set_final, by = c("numeroActo","numeroOferta","idLinea"))
colSums(is.na(Set_final))
summary(Set_final)

# Pegando el nombre de Institucion y el anno

Set_final <- merge(Set_final,Identificador_Instituciones, 
                          by = c("numeroActo","numeroOferta","idLinea"), all.x = TRUE)

# Exportando los datos 
setDT(Set_final)
Set_final[, Consenso := anomalia_KMEANS + anomalia_iso + anomalia_LOF + anomalia_RPCA]
summary(Set_final$Consenso)
#setwd("G:/Unidades compartidas/Fiscalizaci?n Preventiva de Compras P?blicas/Corrida_prueba_modelos/Resultados de los modelos")


Dist_consenso_NA <-Set_final[,.N, keyby = .(idInstitucion,Nombre_Institucion, Consenso)][order(-Consenso)][]

Dist_consenso_transpuesto_NA <- dcast(data = Dist_consenso_NA ,formula = idInstitucion + Nombre_Institucion ~ Consenso, value.var = "N")
Dist_consenso_transpuesto_NA[,Uno_o_mas := rowSums(Dist_consenso_transpuesto_NA[,c(4:7), with = FALSE],na.rm = TRUE)]
Dist_consenso_transpuesto_NA[,Uno_o_mas_Porcent := (Uno_o_mas/rowSums(Dist_consenso_transpuesto_NA[,c(3:7), with = FALSE],na.rm = TRUE))*100]
Dist_consenso_transpuesto_NA[order(-Uno_o_mas)][]
#View(Set_final_con_NA[idInstitucion=="3007045617" & Consenso == 4][])

#fwrite(Dist_consenso_transpuesto_NA,"Resultados_Consenso_set_nuevo_con_NA_2023_V2.csv",sep = ";",dec = ",")

#View(Set_final[idInstitucion=="4000042144" & Consenso == 3][])

#cor(Set_final_V2[,.(score_iso,score_KMEANS,score_LOF,score_RPCA)])

# Haciendo un resumen con la cantidad de unicos por las variables numeroActo y numeroOferta para Consenso igual a cero para cada institucion.
Unicos <- unique(Set_final[Consenso==0], by = c("numeroActo","numeroOferta","idInstitucion","Nombre_Institucion"))
Freq.Inst.unicos_0 <- Unicos[,.N, keyby = c("idInstitucion","Nombre_Institucion")][]
Freq.Inst.unicos_0

#dim(Unicos[idInstitucion=="4000042139"])
# Generando muestras aleatorias por cada institucion
# Esto se hace para los casos en los que la variable Consenso es igual a cero.

Set_final[Consenso==0,.N, keyby = .(idInstitucion,Nombre_Institucion)][order(N)][]# Para el INFOCOOP no se puede calcular la muestra
# aleatoria ya que solamente tiene un dato para cuando Consenso es igual a cero.



## Jerarquización de líneas ------------------
#CASO DEL ISO
puntaje_anomalia_ISO<-Set_final[anomalia_iso==1,]
Deciles_ISO <-quantile(puntaje_anomalia_ISO$score_iso, probs = seq(0, 1, 0.1), na.rm = FALSE)
Deciles_ISO[11]

Set_final[, Escala_Nota_ISO := ifelse((score_iso>=Deciles_ISO[1] & score_iso<=Deciles_ISO[2]),
                                     10,(ifelse((score_iso>Deciles_ISO[2] & score_iso<=Deciles_ISO[3]),
                                     20,ifelse((score_iso>Deciles_ISO[3] & score_iso<=Deciles_ISO[4]),
                                     30,ifelse((score_iso>Deciles_ISO[4] & score_iso<=Deciles_ISO[5]),
                                     40,ifelse((score_iso>Deciles_ISO[5] & score_iso<=Deciles_ISO[6]),
                                     50,ifelse((score_iso>Deciles_ISO[6] & score_iso<=Deciles_ISO[7]),
                                     60,ifelse((score_iso>Deciles_ISO[7] & score_iso<=Deciles_ISO[8]),
                                     70,ifelse((score_iso>Deciles_ISO[8] & score_iso<=Deciles_ISO[9]),
                                     80,ifelse((score_iso>Deciles_ISO[9] & score_iso<=Deciles_ISO[10]),
                                     90,ifelse((score_iso>Deciles_ISO[10] & score_iso<=Deciles_ISO[11]),
                                     100,0)))))))))))][, Escala_Nota_ISO := ifelse((score_iso>=Deciles_ISO[1] & score_iso<=Deciles_ISO[2]),
                                     10,(ifelse((score_iso>Deciles_ISO[2] & score_iso<=Deciles_ISO[3]),
                                                20,ifelse((score_iso>Deciles_ISO[3] & score_iso<=Deciles_ISO[4]),
                                                          30,ifelse((score_iso>Deciles_ISO[4] & score_iso<=Deciles_ISO[5]),
                                                                    40,ifelse((score_iso>Deciles_ISO[5] & score_iso<=Deciles_ISO[6]),
                                                                              50,ifelse((score_iso>Deciles_ISO[6] & score_iso<=Deciles_ISO[7]),
                                                                                        60,ifelse((score_iso>Deciles_ISO[7] & score_iso<=Deciles_ISO[8]),
                                                                                                  70,ifelse((score_iso>Deciles_ISO[8] & score_iso<=Deciles_ISO[9]),
                                                                                                            80,ifelse((score_iso>Deciles_ISO[9] & score_iso<=Deciles_ISO[10]),
                                                                                                                      90,ifelse((score_iso>Deciles_ISO[10] & score_iso<=Deciles_ISO[11]),
                                                                                                                                100,0)))))))))))]


Set_final[,.N, keyby =.(Escala_Nota_ISO,anomalia_iso)][order(-N)][,Porcentaje := (N/sum(N))*100][]

#CASO DEL KNN
puntaje_anomalia_KMEANS<-Set_final[anomalia_KMEANS ==1,]
Deciles_KMEANS <-quantile(puntaje_anomalia_KMEANS$score_KMEANS, probs = seq(0, 1, 0.1), na.rm = FALSE)
Deciles_KMEANS   

Set_final[, Escala_Nota_KMEANS := ifelse((score_KMEANS>=Deciles_KMEANS[1] & score_KMEANS<=Deciles_KMEANS[2]),
                                     10,(ifelse((score_KMEANS>Deciles_KMEANS[2] & score_KMEANS<=Deciles_KMEANS[3]),
                                                20,ifelse((score_KMEANS>Deciles_KMEANS[3] & score_KMEANS<=Deciles_KMEANS[4]),
                                                          30,ifelse((score_KMEANS>Deciles_KMEANS[4] & score_KMEANS<=Deciles_KMEANS[5]),
                                                                    40,ifelse((score_KMEANS>Deciles_KMEANS[5] & score_KMEANS<=Deciles_KMEANS[6]),
                                                                              50,ifelse((score_KMEANS>Deciles_KMEANS[6] & score_KMEANS<=Deciles_KMEANS[7]),
                                                                                        60,ifelse((score_KMEANS>Deciles_KMEANS[7] & score_KMEANS<=Deciles_KMEANS[8]),
                                                                                                  70,ifelse((score_KMEANS>Deciles_KMEANS[8] & score_KMEANS<=Deciles_KMEANS[9]),
                                                                                                            80,ifelse((score_KMEANS>Deciles_KMEANS[9] & score_KMEANS<=Deciles_KMEANS[10]),
                                                                                                                      90,ifelse((score_KMEANS>Deciles_KMEANS[10] & score_KMEANS<=Deciles_KMEANS[11]),
                                                                                                                                100,0)))))))))))]

Set_final[,.N, keyby =.(Escala_Nota_KMEANS, anomalia_KMEANS)][order(-N)][,Porcentaje := (N/sum(N))*100][]

#CASO DEL LOF
names(Set_final)
puntaje_anomalia_LOF<-Set_final[anomalia_LOF==1,]
Deciles_LOF <-quantile(puntaje_anomalia_LOF$score_LOF, probs = seq(0, 1, 0.1), na.rm = FALSE)
Deciles_LOF   


Set_final[, Escala_Nota_LOF := ifelse((score_LOF>=Deciles_LOF[1] & score_LOF<=Deciles_LOF[2]),
                                     10,(ifelse((score_LOF>Deciles_LOF[2] & score_LOF<=Deciles_LOF[3]),
                                                20,ifelse((score_LOF>Deciles_LOF[3] & score_LOF<=Deciles_LOF[4]),
                                                          30,ifelse((score_LOF>Deciles_LOF[4] & score_LOF<=Deciles_LOF[5]),
                                                                    40,ifelse((score_LOF>Deciles_LOF[5] & score_LOF<=Deciles_LOF[6]),
                                                                              50,ifelse((score_LOF>Deciles_LOF[6] & score_LOF<=Deciles_LOF[7]),
                                                                                        60,ifelse((score_LOF>Deciles_LOF[7] & score_LOF<=Deciles_LOF[8]),
                                                                                                  70,ifelse((score_LOF>Deciles_LOF[8] & score_LOF<=Deciles_LOF[9]),
                                                                                                            80,ifelse(score_LOF>=Deciles_LOF[9],
                                                                                                                      100,0))))))))))]

Set_final[,.N, keyby =.(Escala_Nota_LOF, anomalia_LOF)][order(-N)][,Porcentaje := (N/sum(N))*100][]

#CASO DEL RPCA
#names(Consenso)
puntaje_anomalia_RPCA<-Set_final[anomalia_RPCA==1,]
Deciles_RPCA <-quantile(puntaje_anomalia_RPCA$score_RPCA, probs = seq(0, 1, 0.1), na.rm = FALSE)
Deciles_RPCA   

Set_final[, Escala_Nota_RPCA := ifelse((score_RPCA>=Deciles_RPCA[1] & score_RPCA<=Deciles_RPCA[2]),
                                      10,(ifelse((score_RPCA>Deciles_RPCA[2] & score_RPCA<=Deciles_RPCA[3]),
                                                 20,ifelse((score_RPCA>Deciles_RPCA[3] & score_RPCA<=Deciles_RPCA[4]),
                                                           30,ifelse((score_RPCA>Deciles_RPCA[4] & score_RPCA<=Deciles_RPCA[5]),
                                                                     40,ifelse((score_RPCA>Deciles_RPCA[5] & score_RPCA<=Deciles_RPCA[6]),
                                                                               50,ifelse((score_RPCA>Deciles_RPCA[6] & score_RPCA<=Deciles_RPCA[7]),
                                                                                         60,ifelse((score_RPCA>Deciles_RPCA[7] & score_RPCA<=Deciles_RPCA[8]),
                                                                                                   70,ifelse((score_RPCA>Deciles_RPCA[8] & score_RPCA<=Deciles_RPCA[9]),
                                                                                                             80,ifelse((score_RPCA>Deciles_RPCA[9] & score_RPCA<=Deciles_RPCA[10]),
                                                                                                                       90,ifelse((score_RPCA>Deciles_RPCA[10] & score_RPCA<=Deciles_RPCA[11]),
                                                                                                                                 100,0)))))))))))]


Set_final[,.N, keyby = .(Escala_Nota_RPCA, anomalia_RPCA)][order(-N)][,Porcentaje := (N/sum(N))*100][]

Set_final[, Nota_final := Escala_Nota_ISO*0.25+Escala_Nota_KMEANS*0.25+Escala_Nota_LOF*0.25+Escala_Nota_RPCA*0.25]

## Inclusión de monto ---------------
costos <- data.table(sqlFetch(SICOP, "ds.costosCont",as.is=TRUE))
#head(costos)
#names(costos)
#names(costos_consenso)


costos[, precio_unitario_contratado_colones := ifelse(tipoMonedaContratada == "CRC", as.numeric(precioUnitarioContratado),as.numeric(precioUnitarioContratado)*as.numeric(tipoCambioCrcContratado)*(1/as.numeric(tipoCambioDolarContratado)))]
costos[, Monto_linea_colones := as.numeric(cantidadContratada) * precio_unitario_contratado_colones - as.numeric(descuentoContratado) + as.numeric(ivaContratado) + as.numeric(otrosImpuestosContratado) + as.numeric(acarreosContratado)]

Set_final<- merge(Set_final,
                        costos[,.(numeroActo,numeroOferta,idLinea,numeroProcedimiento, modalidadProcedimiento, Monto_linea_colones)],
                        by = c("numeroActo","idLinea","numeroOferta"),all.x = TRUE)



            #### LIMPIEZA DE DATOS
            # Lista de archivos a conservar
            objetos_a_conservar <- c("Set_Modelo_sin_NA", "Set_final",
                                     "Indicadores_Plazo", "Indicadores_Alcance", "Indicadores_Costo",
                                     "Resultados_ISO","Resultados_KMEANS","Resultados_RPCA","Resultados_LOF",
                                     "LineasOfertadas", "Ofertas", "Costos", "SICOP", "tablas")
            objetos_totales <- ls()
            
            # Obtener la lista de objetos a eliminar
            objetos_a_eliminar <- setdiff(objetos_totales, objetos_a_conservar)
            rm(list = objetos_a_eliminar)

#fwrite(Set_final, "Resultados_Finales.csv",sep = ";",dec = ".")
            
            
tablas$TABLE_NAME

lineasadjudicadas <- data.table(sqlFetch(SICOP, "dbo.lineasadjudicadas",as.is=TRUE))
proveedores <- data.table(sqlFetch(SICOP, "dbo.proveedores",as.is=TRUE))
jerarquiaClasificacionBS <- data.table(sqlFetch(SICOP, "dbo.jerarquiaClasificacionBS",as.is=TRUE))
identificacionbs <- data.table(sqlFetch(SICOP, "dbo.identificacionbs",as.is=TRUE))
clasificacionbs <- data.table(sqlFetch(SICOP, "dbo.clasificacionbs",as.is=TRUE))

clasificacionbs
names(identificacionbs)


names(lineasadjudicadas)
Set_final_V2<- merge(Set_final,
                  lineasadjudicadas[,.(nro_acto,nro_linea,nro_oferta, cedula_proveedor, codigo_producto)],
                  by.x = c("numeroActo","idLinea","numeroOferta"),
                  by.y = c("nro_acto","nro_linea","nro_oferta"), all.x = TRUE)

Set_final_V2<- merge(Set_final_V2,
                     proveedores[,.(cedula_proveedor, nombre_proveedor, tipo_proveedor, tamano_proveedor)],
                     by = c("cedula_proveedor"), all.x = TRUE)

Set_final_V2[,codigo_identificacion:=substr(codigo_producto,1,16)]

Set_final_V2<- merge(Set_final_V2,
                     identificacionbs[,.(codigo_identificacion, nombre_identificacion)],
                     by = c("codigo_identificacion"), all.x = TRUE)
names(Set_final_V2)
Set_final_V3<-Set_final_V2[,.(numeroProcedimiento, numeroActo, idLinea, numeroOferta, 
                  codigo_identificacion, nombre_identificacion, Nombre_Institucion, idInstitucion,
                  cedula_proveedor, nombre_proveedor, tipo_proveedor, tamano_proveedor,
                  anno, modalidadProcedimiento, Monto_linea_colones, Consenso)]

Set_final_V4<- merge(Set_final_V3,
                     Set_Modelo_sin_NA,
                     by = c("numeroActo","idLinea","numeroOferta"),
                     all.x = TRUE)
Set_final_V4<- merge(Set_final_V4,
                     Indicadores_Costo,
                     by = c("numeroActo","idLinea","numeroOferta"),
                     all.x = TRUE)

Set_final_V4<- merge(Set_final_V4,
                     Indicadores_Plazo,
                     by = c("numeroActo","idLinea","numeroOferta"),
                     all.x = TRUE)

names(Set_final_V4)
Set_final            


fwrite(Set_final_V4, "Resultados_Finales_Prueba2.csv",sep = ";",dec = ".")










              