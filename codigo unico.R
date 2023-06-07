#*********************************CODIGO DE ANALÍTICA DE DATOS PARA COMPRAS PÚBLICAS*******************************#
#**************************************************ELABORADO POR LA CGR*****************************************#

#**************************************INICIO**************************# 
#Instalacion de librerias

install.packages("data.table")
install.packages("RODBC")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("summarytools")
install.packages("GGally")
install.packages("DT")
install.packages("lubridate")
install.packages("venn")
install.packages("readxl")
install.packages("stringi")
install.packages("stringr")
install.packages("writexl")
install.packages("matrixStats")
install.packages("robustbase")
install.packages("rrcov")
install.packages("factoextra")
install.packages("h2o")
install.packages("DDoutlier")
install.packages("isotree")


#Cargando las librerias 

library(data.table)
library(RODBC)
library(ggplot2)
library(tidyverse)
library(summarytools)
library(GGally)
library(DT)
library(lubridate)
library(venn)
library(readxl)
library(stringi)
library(stringr)
library(writexl)
library(matrixStats)
library(robustbase)
library(rrcov)
library(factoextra)
library(h2o)
library(DDoutlier)
library(isotree)
library(dplyr)

options(scipen = 999)

#escoger donde estará ubicado el archivo y donde caen los archivos#
getwd()
setwd("C:/Users/humberto.perera/Documents/GitHub")

# Conexion SICOP y CASP
#se debe hacer un paso previo de Configuración de una conexión ODBC a base de datos MSSQL Server, esto requiere de permisos de administrador"

SICOP <-odbcConnect("ODBC_CGRSQL")
tablas <- sqlTables(SICOP, tableType = "TABLE")

tablas$TABLE_NAME


#**************************************INDICADORES DE PLAZO**************************# 


# Cargando las tablas necesarias 
Plazos <- data.table(sqlFetch(SICOP, "ds.plazos",as.is=TRUE))

# Cargando los datos

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

###************************** Indicador 1**************************###

# 1. Desviación de la cantidad de días entre la fecha de inicio y la fecha de adjudicación, respecto al "promedio" del bien.

# Primero se calcula los días promedio del bien 


Promedio_iniadj_16 <- Plazos[, .(Prom.inicioadju_16 = mean(difIniAdj,na.rm = TRUE)), keyby = .(codigoProductoAdjudicado_16)][]

# Pegando a Plazo el promedio del codigoProductoAdjudicado

Plazo.1 <- merge(x = Plazos, y = Promedio_iniadj_16,by = "codigoProductoAdjudicado_16",all.x = TRUE)

#names(Plazo.1)
Plazo.1[,.N, keyby = .(Prom.inicioadju_16)][]
summary(Plazo.1$Prom.inicioadju_16)

# Posteriormente se calcula la desviacion entre cada una de las adjudicaciones y el plazo promedio
Plazo.1[, Desv.inicioadju := difIniAdj/Prom.inicioadju_16][]

# Resumen de las variables relacionadas con la desviación
Plazo.1[, .N, keyby = .(codigoProductoAdjudicado, difIniAdj, Prom.inicioadju_16, Desv.inicioadju)][]
summary(Plazo.1$Desv.inicioadju)

# Cálculo del porcentaje de ocurrencia de cada valor de desviación
Plazo.1[, .N, keyby = Desv.inicioadju][order(-N)][, Porcentaje := (N / sum(N)) * 100][]

# Resumen de la variable de desviación
summary(Plazo.1$Desv.inicioadju)



###************************** Indicador 2**************************###


# 2.1. Desviación de la cantidad de días entre la fecha de Contrato (primera) y la fecha de adjudicación, respecto al 'promedio' del bien.
Promedio_contprirec_16 <- Plazos[, .(Prom.contprirec_16 = mean(difContPriRec,na.rm = TRUE)), keyby = .(codigoProductoAdjudicado_16)][]

uniqueN(Promedio_contprirec_16,by = "codigoProductoAdjudicado_16")


# Pegando a Plazo el promedio del codigoProductoAdjudicado

Plazo.1 <- merge(x = Plazo.1, y = Promedio_contprirec_16,by = "codigoProductoAdjudicado_16",all.x = TRUE)

names(Plazo.1)
Plazo.1[,.N, keyby = .(Prom.contprirec_16)][]
summary(Plazo.1$Prom.contprirec_16) 

# Posteriormente se calcula la desviacion entre cada una de las adjudicaciones y el plazo promedio
Plazo.1[, Desv.contprirec := difContPriRec/Prom.contprirec_16][]
Plazo.1[,.N, keyby = .(codigoProductoAdjudicado,difContPriRec,Prom.contprirec_16,Desv.contprirec)][]
Plazo.1[,.N, keyby = Desv.contprirec][order(-N)][,Porcentaje := (N/sum(N))*100][]
summary(Plazo.1$Desv.contprirec)

# 2.2. Grado de desviación entre la fecha de recepción y la formalización del último contrato.

# Cálculo del promedio de los días entre la fecha de recepción y la formalización del último contrato por bien.
Promedio_contultrec_16 <- Plazo.1[, .(Prom.contultrec_16 = mean(difContUltRec,na.rm = TRUE)), keyby = .(codigoProductoAdjudicado_16)][]

# conteo de valores únicos 
uniqueN(Promedio_contultrec_16,by = "codigoProductoAdjudicado_16")

# Pegando a Plazo el promedio del codigoProductoAdjudicado
Plazo.1 <- merge(x = Plazo.1,y = Promedio_contultrec_16,by = "codigoProductoAdjudicado_16",all.x = TRUE)

names(Plazo.1)
Plazo.1[,.N, keyby = .(Prom.contultrec_16)][]
summary(Plazo.1$Prom.contultrec_16) 

# Posteriormente se calcula la desviacion entre cada una de las adjudicaciones y el plazo promedio
Plazo.1[, Desv.contultrec := difContUltRec/Prom.contultrec_16][]
Plazo.1[,.N, keyby = .(codigoProductoAdjudicado,difContUltRec,Prom.contultrec_16,Desv.contultrec)][]

Plazo.1[,.N, keyby = Desv.contultrec][order(-N)][,Porcentaje := (N/sum(N))*100][]

summary(Plazo.1$Desv.contultrec)

###************************** Indicador 3**************************###

# 3.1. Grado de desviación entre la fecha de adjudicación y la formalización del último contrato.

#Cálculo del promedio de los días entre la fecha de adjudicación y la formalización del último contrato por bien
Promedio_contultadj_16 <- Plazos[, .(Prom.contultadj_16 = mean(difAdjContUlt,na.rm = TRUE)), keyby = .(codigoProductoAdjudicado_16)][]

uniqueN(Promedio_contultadj_16,by = "codigoProductoAdjudicado_16")

# Pegando a Plazo el promedio del codigoProductoAdjudicado

Plazo.1 <- merge(x = Plazo.1,y = Promedio_contultadj_16,by = "codigoProductoAdjudicado_16",all.x = TRUE)

names(Plazo.1)
Plazo.1[,.N, keyby = .(Prom.contultadj_16)][]
summary(Plazo.1$Prom.contultadj_16)  

# Posteriormente se calcula la desviacion entre cada una de las adjudicaciones y el plazo promedio
Plazo.1[, Desv.contultadj := difAdjContUlt/Prom.contultadj_16][]

Plazo.1[,.N, keyby = .(codigoProductoAdjudicado,difAdjContUlt,Prom.contultadj_16,Desv.contultadj)][]

#Número de observaciones por desviación y cálculo del porcentaje
Plazo.1[,.N, keyby = Desv.contultadj][order(-N)][,Porcentaje := (N/sum(N))*100][]


summary(Plazo.1$Desv.contultadj) 

# 3.2. Grado de desviaci?n entre la fecha de adjudicaci?n y la formalizaci?n del primer contrato

#Cálculo del promedio de los días entre la fecha de adjudicación y la formalización del primer contrato por bien
Promedio_contpriadj_16 <- Plazos[, .(Prom.contpriradj_16 = mean(difAdjContPri,na.rm = TRUE)), keyby = .(codigoProductoAdjudicado_16)][]

uniqueN(Promedio_contpriadj_16,by = "codigoProductoAdjudicado_16")

# Pegando a Plazo el promedio del codigoProductoAdjudicado

Plazo.1 <- merge(x = Plazo.1,y = Promedio_contpriadj_16,by = "codigoProductoAdjudicado_16",all.x = TRUE)

names(Plazo.1)
Plazo.1[,.N, keyby = .(Prom.contpriradj_16)][]
summary(Plazo.1$Prom.contpriradj_16) 


# Posteriormente se calcula la desviacion entre cada una de las adjudicaciones y el plazo promedio
Plazo.1[, Desv.contpriadj := difAdjContPri/Prom.contpriradj_16][]

#Agrupando y calculando el porcentaje para los valores de Desv.contpriadj
Plazo.1[,.N, keyby = Desv.contpriadj][order(-N)][,Porcentaje := (N/sum(N))*100][]

summary(Plazo.1$Desv.contpriadj) 

names(Plazo.1)
# Estadísticas resumidas para la columna Desv.inicioadju
summary(Plazo.1$Desv.inicioadju)

# Estadísticas resumidas para la columna Desv.contprirec
summary(Plazo.1$Desv.contprirec)

# Estadísticas resumidas para la columna Desv.contultrec
summary(Plazo.1$Desv.contultrec)

# Estadísticas resumidas para la columna Desv.contultadj
summary(Plazo.1$Desv.contultadj)

# Estadísticas resumidas para la columna Desv.contpriadj
summary(Plazo.1$Desv.contpriadj)

# Estadísticas resumidas para la columna cantModif
summary(Plazo.1$cantModif)

# Agrupando y calculando el porcentaje para los valores de cantModif
Plazo.1[,.N, keyby = cantModif][order(-N)][,Porcentaje := (N/sum(N))*100][]


###************************** Indicador 4**************************###

##### Diferencia entre adjudicación y primer contrato
Plazo.1[, difIniContPri := difIniAdj + difAdjContPri][]

Promedio_Ini_ContPri_16 <- Plazo.1[, .(Prom.Ini_ContPri_16 = mean(difIniContPri,na.rm = TRUE)), keyby = .(codigoProductoAdjudicado_16)][]

#Uniendo los datos
Plazo.1 <- merge(x = Plazo.1,y = Promedio_Ini_ContPri_16,by = "codigoProductoAdjudicado_16",all.x = TRUE)

# Calculando la columna Desv.IniContPri
Plazo.1[, Desv.IniContPri := difIniContPri/Prom.Ini_ContPri_16][]

# Agrupando y contando los valores para las columnas codigoProductoAdjudicado, difAdjContUlt, Prom.Ini_ContPri_16, y Desv.IniContPri
Plazo.1[,.N, keyby = .(codigoProductoAdjudicado,difAdjContUlt,Prom.Ini_ContPri_16,Desv.IniContPri)][]

# Agrupando y calculando el porcentaje para los valores de Desv.IniContPri
Plazo.1[,.N, keyby = Desv.IniContPri][order(-N)][,Porcentaje := (N/sum(N))*100][]

# Resumen de la columna Desv.IniContPri
summary(Plazo.1$Desv.IniContPri)

# Asignando los valores de la columna cantModif a la nueva columna CantModf_Plazo
Plazo.1$CantModf_Plazo <- Plazo.1$cantModif

# creando la tabla
Ind.Plazo <- Plazo.1[, .(numeroActo,numeroOferta,idLinea,Desv.inicioadju,Desv.contprirec,Desv.contultrec,Desv.contultadj,Desv.contpriadj,Desv.IniContPri,CantModf_Plazo)][]


# Manteniendo solamente las variables de identificacion y los indicadores

uniqueN(x = Plazo.1,by = c("numeroActo","numeroOferta","idLinea"))
Indicadores_Plazo <- Plazo.1[, .(numeroActo,numeroOferta,idLinea,Desv.inicioadju,Desv.contprirec,Desv.contultrec,Desv.contultadj,Desv.contpriadj,Desv.IniContPri,CantModf_Plazo)][]

#### LIMPIEZA DE DATOS PARA PROXIMA CORRIDA
# Lista de archivos a conservar
objetos_a_conservar <- c("Indicadores_Plazo", "SICOP", "tablas")
objetos_totales <- ls()

# Obtener la lista de objetos a eliminar
objetos_a_eliminar <- setdiff(objetos_totales, objetos_a_conservar)
rm(list = objetos_a_eliminar)


#**************************************INDICADORES DE PLAZO**************************# 
#Carga los datos de la tabla "ds.alcance" desde la conexión SICOP y los asigna a la variable Alcance
Alcance <- data.table(sqlFetch(SICOP, "ds.alcance",as.is=TRUE))

# vicularlo al espacio de trabajo
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

#Convirtiendo los códigos de producto de 24 a 16
Alcance$codigoProductoAdjudicado_16<- substr(Alcance$codigoProductoAdjudicado, 1, 16)
Alcance$codigoProductoContratado_16<- substr(Alcance$codigoProductoContratado, 1, 16)
Alcance$codigoProductoRecibido_16<- substr(Alcance$codigoProductoRecibido, 1, 16)


#limpiando los datos para incluir solo datos desde el 2018, y solo bienes
Alcance<- Alcance[(anno < 2023 & anno > 2019),]
Alcance[, tipo_bien:= substr(codigoProductoSolicitado,1,1)]
Alcance<- Alcance[Alcance$tipo_bien%in%c("1","2","3","4","5","6"), ]



###************************** Indicador 1**************************###



###************************** Indicador 2**************************###



###************************** Indicador 3**************************###


###************************** Indicador 3**************************###