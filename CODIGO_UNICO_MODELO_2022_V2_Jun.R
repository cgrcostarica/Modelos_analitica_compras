#*********************************CODIGO DE ANALÍTICA DE DATOS PARA COMPRAS PÚBLICAS*******************************#
#**************************************************ELABORADO POR LA CGR*****************************************#

#**************************************INICIO**************************# 
#Instalacion de librerias

install.packages("data.table", repos = "http://cran.us.r-project.org")
install.packages("RODBC", repos = "http://cran.us.r-project.org")
install.packages("ggplot2", repos = "http://cran.us.r-project.org")
install.packages("tidyverse",  repos = "http://cran.us.r-project.org")
install.packages("summarytools",  repos = "http://cran.us.r-project.org")
install.packages("GGally",  repos = "http://cran.us.r-project.org")
install.packages("DT",  repos = "http://cran.us.r-project.org")
install.packages("lubridate",  repos = "http://cran.us.r-project.org")
install.packages("venn",  repos = "http://cran.us.r-project.org")
install.packages("readxl",  repos = "http://cran.us.r-project.org")
install.packages("stringi",  repos = "http://cran.us.r-project.org")
install.packages("stringr",  repos = "http://cran.us.r-project.org")
install.packages("writexl",  repos = "http://cran.us.r-project.org")
install.packages("matrixStats",  repos = "http://cran.us.r-project.org")
install.packages("robustbase",  repos = "http://cran.us.r-project.org")
install.packages("rrcov",  repos = "http://cran.us.r-project.org")
install.packages("factoextra",  repos = "http://cran.us.r-project.org")
install.packages("h2o",  repos = "http://cran.us.r-project.org")
install.packages("DDoutlier",  repos = "http://cran.us.r-project.org")
install.packages("isotree",  repos = "http://cran.us.r-project.org")


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

# 3.2. Grado de desviaci?n entre la fecha de adjudicaci?n y la formalizaci?n del primer contrato ##JOSE PABLO##

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

##### Diferencia entre adjudicación y primer contrato ##JOSE PABLO##
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


#**************************************INDICADORES DE ALCANCE**************************# 
#*

#Carga los datos de la tabla "ds.alcance" desde la conexión SICOP y los asigna a la variable Alcance
Alcance <- data.table(sqlFetch(SICOP, "ds.alcance",as.is=TRUE))

# vicularlo al espacio de trabajo
attach(Alcance)

#Definiendo los valores como numéricos
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

# 1. Razón entre cantidad recibida y cantidad adjudicada 
# Se calcula la desviacion entre cada una de las adjudicaciones y su cantidad adjudicada y la cantidad recibida
Alcance[, Raz.cant.recibida.contratada := cantidadContratada/cantidadRecibida][]
#Manejo de datos infinitos
Alcance[, Raz.cant.recibida.contratada := ifelse(is.infinite(Raz.cant.recibida.contratada), 
                                                 cantidadContratada, 
                                                 Raz.cant.recibida.contratada)]
#agrupacion para hacer el indicador
Alcance[,.N, keyby = .(codigoProductoAdjudicado_16,cantidadContratada,cantidadRecibida,Raz.cant.recibida.contratada)][]
#descriptivos del indicador
summary(Alcance$Raz.cant.recibida.contratada) 


###************************** Indicador 2**************************###

# 2. Raz?n entre cantidad recibida y cantidad solicitada
Alcance[, Raz.recibida.solicitada := cantidadSolicitada/cantidadRecibida][]
#Manejo de LOS VALORES INFINITOS EN LOS DATOS
Alcance[, Raz.recibida.solicitada := ifelse(is.infinite(Raz.recibida.solicitada), 
                                            cantidadSolicitada, 
                                            Raz.recibida.solicitada)]

Alcance[,.N, keyby = .(codigoProductoAdjudicado_16,cantidadSolicitada,cantidadRecibida,Raz.recibida.solicitada)][]
Alcance[,.N, keyby = Raz.recibida.solicitada][order(-N)][,Porcentaje := (N/sum(N))*100][]

summary(Alcance$Raz.recibida.solicitada) 

###************************** Indicador 3**************************###

# 3. Codigo de producto recibido es igual a codigo de producto contratado. ##JOSE PABLO##
Alcance[, Cod.Cont.Rec := ifelse(codigoProductoContratado_16 == codigoProductoRecibido_16,1,0)][]
#View(Alcance)
summary(Alcance$Cod.Cont.Rec)

###************************** Indicador 4**************************###

#4 Cantidad de procedimientos adjudicados del mismo bien en la misma instituci?n durante el a?o
names(Alcance)

Alcance.1 <- unique(x = Alcance[,c(4,5,6,22), with = FALSE], by = c("anno", "idInstitucion","codigoProductoAdjudicado_16","idProcedimiento"))

Alcance.1[, Compras.unico := .N, by = .(anno, idInstitucion,codigoProductoAdjudicado_16)][order(-Compras.unico)]


###************************** Indicador 5**************************###

# 5. Raz?n entre cantidad adjudicada y cantidad contratada ##JOSE PABLO##
Alcance[, Raz.adjudicada.contratada := cantidadAdjudicada/cantidadContratada][]

#Manejo de LOS VALORES INFINITOS EN LOS DATOS
Alcance[, Raz.adjudicada.contratada := ifelse(is.infinite(Raz.adjudicada.contratada), cantidadAdjudicada, 
                                              Raz.adjudicada.contratada)]

Alcance[,.N, keyby = .(codigoProductoAdjudicado_16,cantidadAdjudicada,cantidadContratada,Raz.adjudicada.contratada)][]
Alcance[,.N, keyby = Raz.adjudicada.contratada][order(-N)][,Porcentaje := (N/sum(N))*100][]
summary(Alcance$Raz.adjudicada.contratada) 


###************************** Indicador 6**************************###

# 6. Raz?n entre cantidad solicitada y cantidad contratada ##JOSE PABLO##
Alcance[, Raz.solicitada.contratada := cantidadSolicitada/cantidadContratada][]

#Manejo de LOS VALORES INFINITOS EN LOS DATOS
Alcance[, Raz.solicitada.contratada := ifelse(is.infinite(Raz.solicitada.contratada), cantidadSolicitada, 
                                              Raz.solicitada.contratada)]


Alcance[,.N, keyby = .(codigoProductoAdjudicado_16,cantidadSolicitada,cantidadContratada,Raz.solicitada.contratada)][]
Alcance[,.N, keyby = Raz.solicitada.contratada][order(-N)][,Porcentaje := (N/sum(N))*100][]

summary(Alcance$Raz.solicitada.contratada)

names(Alcance)


###************************** Indicador 7**************************###

# 7. Codigo de producto adjudicado es igual a codigo de producto contratado. ##JOSE PABLO##
Alcance[, Cod.Cont.Adj := ifelse(codigoProductoContratado_16 == codigoProductoAdjudicado_16,1,0)][]

summary(Alcance$Cod.Cont.Adj)# todos son 1

###************************** Manteniendo solamente las variables de identificacion y los indicadores**************************###

names(Alcance)

Alcance.2 <- unique(x = Alcance.1, by = c("anno", "idInstitucion","codigoProductoAdjudicado_16"))

Alcance.3 <- merge(x = Alcance, y = Alcance.2[,-3,with = FALSE], by = c("anno", "idInstitucion","codigoProductoAdjudicado_16"),all.x = TRUE)
names(Alcance.3)
Alcance.3$CantModf_Alcance<-Alcance.3$cantModif


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



#**************************************INDICADORES DE COSTO**************************# 
#Carga los datos de la tabla "ds.acostos" desde la conexión SICOP y los asigna a la variable Costos
Costos <- data.table(sqlFetch(SICOP, "ds.costos",as.is=TRUE))
#Carga los datos de la tabla "dbo.lineas ofertadas" desde la conexión SICOP y los asigna a la variable lineas ofertadas
LineasOfertadas <- data.table(sqlFetch(SICOP, "dbo.LineasOfertadas",as.is=TRUE))
#Carga los datos de la tabla "dbo.Ofertas" desde la conexión SICOP y los asigna a la variable Ofertas
Ofertas <- data.table(sqlFetch(SICOP, "dbo.Ofertas",as.is=TRUE))

# Definir las variables que son caracteres 
glimpse(Costos)

# Obtiene los valores únicos de las variables "numeroActo", "numeroOferta" y "idLinea" en el dataframe Costos
uniqueN(x = Costos, by = c("numeroActo", "numeroOferta", "idLinea"))

# Cuenta el número de ocurrencias para cada combinación única de las variables "numeroActo", "numeroOferta" y "idLinea" en el dataframe Costos,
# y luego ordena los resultados en orden descendente.
Costos[,.N, keyby = c("numeroActo", "numeroOferta", "idLinea")][order(-N)][]

# Filtra el dataframe Costos para mantener solo las filas donde el valor de la variable "anno" es mayor a 2019 y menor a 2023.
Costos <- Costos[(anno < 2023 & anno > 2019),]

# Crea una nueva columna llamada "tipo_bien" en el dataframe Costos, que contiene los primeros caracteres de la columna "codigoProductoSolicitado".
Costos[, tipo_bien := substr(codigoProductoSolicitado, 1, 1)]

# Filtra el dataframe Costos para mantener solo las filas donde el valor de la columna "tipo_bien" se encuentra en el conjunto de valores ("1", "2", "3", "4", "5", "6").
Costos <- Costos[Costos$tipo_bien %in% c("1", "2", "3", "4", "5", "6"),]

# Calcula las dimensiones (número de filas y columnas) del dataframe Costos.
dim(Costos)

# Obtiene los valores únicos de las variables "numeroActo", "idLinea" y "numeroOferta" en el dataframe Costos.
uniqueN(x = Costos, by = c("numeroActo", "idLinea", "numeroOferta"))


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

# Pasando los datos a colones 
### dolares
Costos[,.N, keyby = .(tipoMonedaOfertada)][order(-N)][]
Costos[,.N, keyby = .(tipoMonedaAdjudicada)][order(-N)][]
Costos[tipoMonedaOfertada=="USD",.N, keyby = .(tipoCambioDolarOfertado)][]
Costos[tipoMonedaAdjudicada=="USD",.N, keyby = .(tipoCambioDolarAdjudicado)][]


# Cambia el precio OFERTADO a colones, sin importar el tipo de moneda.
Costos[, ":=" (Prec.Unit.Ofer.Col = ifelse(tipoMonedaOfertada == "CRC",
                                           yes = precioUnitarioOfertado,
                                           no = precioUnitarioOfertado * tipoCambioCrcOfertado * (1 / tipoCambioDolarOfertado)))][]

# Cuenta el número de filas en el dataframe Costos donde el valor de la columna Prec.Unit.Ofer.Col es igual a 0.
nrow(Costos[Prec.Unit.Ofer.Col == 0])

#tabla de frecuencias por Precio Unitario Ofertado en colones
Costos[,.N, keyby = .(Prec.Unit.Ofer.Col)][]
#Cuando la oferta es 0 y la modalidad es Cantidad definida, son 4 casos
Prec.Ofer.0 <- Costos[Prec.Unit.Ofer.Col==0 & modalidadProcedimiento=="Cantidad definida"][]

# Obtiene el recuento de filas agrupadas por la columna modalidadProcedimiento en el dataframe Costos.
Costos[,.N, keyby = .(modalidadProcedimiento)][] # Diferentes tipos de procedimiento

# Crea un nuevo dataframe llamado Verif.Prec.Unit.Ofer.Col que contiene las columnas tipoMonedaOfertada, Prec.Unit.Ofer.Col, precioUnitarioOfertado, tipoCambioCrcOfertado y tipoCambioDolarOfertado del dataframe Costos.
Verif.Prec.Unit.Ofer.Col <- Costos[,.(tipoMonedaOfertada,Prec.Unit.Ofer.Col,precioUnitarioOfertado,tipoCambioCrcOfertado,tipoCambioDolarOfertado)][]

# Filtra el dataframe Verif.Prec.Unit.Ofer.Col para mantener solo las filas donde el valor de la columna tipoMonedaOfertada es igual a "EUR".
Verif.Prec.Unit.Ofer.Col[tipoMonedaOfertada == "EUR"][]

# Cambia a colones el precio ADJUDICADO.
Costos[, Prec.Unit.Adj.Col := ifelse(tipoMonedaAdjudicada == "CRC", precioUnitarioAdjudicado, precioUnitarioAdjudicado * tipoCambioCrcAdjudicado * (1 / tipoCambioDolarAdjudicado))][]

# Obtiene el recuento de filas agrupadas por la columna Prec.Unit.Adj.Col en el dataframe Costos.
Costos[,.N, keyby = .(Prec.Unit.Adj.Col)][]

# Filtra el dataframe Costos para obtener las filas donde el valor de la columna Prec.Unit.Adj.Col es igual a 0 y la columna modalidadProcedimiento es igual a "Cantidad definida".
Prec.Adj.0 <- Costos[Prec.Unit.Adj.Col == 0 & modalidadProcedimiento == "Cantidad definida", ]

# Cuenta el número de filas en el dataframe Prec.Adj.0.
nrow(Prec.Adj.0) # Es igual a 0

# Filtra el dataframe Costos para obtener las filas donde el valor de la columna Prec.Unit.Adj.Col es igual a 0 y el valor de la columna precioUnitarioEstimado es igual a 0.
Costos[Prec.Unit.Adj.Col == 0 & precioUnitarioEstimado == 0, ]

# Crea un nuevo dataframe llamado Verif.Prec.Unit.Adj.Col que contiene las columnas tipoMonedaAdjudicada, Prec.Unit.Adj.Col, precioUnitarioAdjudicado, tipoCambioCrcAdjudicado y tipoCambioDolarAdjudicado del dataframe Costos.
Verif.Prec.Unit.Adj.Col <- Costos[, .(tipoMonedaAdjudicada, Prec.Unit.Adj.Col, precioUnitarioAdjudicado, tipoCambioCrcAdjudicado, tipoCambioDolarAdjudicado)][]

# Filtra el dataframe Verif.Prec.Unit.Adj.Col para mantener solo las filas donde el valor de la columna tipoMonedaAdjudicada es igual a "EUR".
Verif.Prec.Unit.Adj.Col[tipoMonedaAdjudicada == "EUR", ]



###************************** Indicador 1**************************###

# 1. Grado de desviación del monto unitario adjudicado de la línea con respecto al resto de las compras u ofertas del mismo bien

# Se realiza en dos pasos para mostrar más información.

# Paso 1: Calcular el precio promedio del bien
Promedio_Costos <- Costos[, .(Prom.precioUnitarioAdjudicado = mean(Prec.Unit.Adj.Col, na.rm = TRUE),
                              Conteo_Adjudicado = length(Prec.Unit.Adj.Col)),
                          keyby = .(codigoProductoAdjudicado_16)][]

# Obtener el recuento de valores únicos en la columna "codigoProductoAdjudicado_16" en el dataframe Promedio_Costos.
uniqueN(Promedio_Costos, by = "codigoProductoAdjudicado_16") 
# Esto verifica si coincide con los valores únicos en la columna "codigoProductoAdjudicado_16" en el dataframe Costos.

# Cambiar los precios adjudicados a colones o dólares, según el tipo de moneda adjudicada.
Costos[, Prec.Unit.Adj.Col := ifelse(tipoMonedaAdjudicada == "CRC",
                                     precioUnitarioAdjudicado,
                                     precioUnitarioAdjudicado * tipoCambioCrcAdjudicado * (1 / tipoCambioDolarAdjudicado))][]

# Calcular el porcentaje de veces que se ha adjudicado con un precio único, en función del conteo de adjudicaciones.
Promedio_Costos[,.N, keyby = Conteo_Adjudicado][, Porcentaje := (N/sum(N)) * 100][] 
# Esto muestra el porcentaje de veces que se ha adjudicado con un precio único en función del conteo de adjudicaciones.

# Cambiar a dólares con el tipo de cambio
LineasOfertadas[, precio_unitario_ofertado_colones := ifelse(tipo_moneda == "CRC",
                                                             as.numeric(precio_unitario_ofertado),
                                                             as.numeric(precio_unitario_ofertado) * as.numeric(tipo_cambio_crc) * (1 / as.numeric(tipo_cambio_dolar)))]

# Se une lineas adjudicadas con procedimientos
Prel_0 <- merge(LineasOfertadas, Ofertas[, .(nro_sicop, nro_oferta, elegible)],
                by = c("nro_sicop", "nro_oferta"),
                all.x = TRUE) 
# Se realiza una unión entre los dataframes LineasOfertadas y Ofertas, basada en las columnas "nro_sicop" y "nro_oferta". Se agrega la columna "elegible" para indicar si la oferta es elegible o no.

Prel_1 <- Prel_0[(elegible == "Sí"),] 
# Se filtran las filas donde la columna "elegible" es igual a "Sí", es decir, se mantienen solo las filas donde la oferta es elegible.

rm(Prel_0) 
# Se elimina el dataframe Prel_0 de la memoria para liberar espacio.

Prel_1 <- Prel_1[, codigo_producto_16 := substr(codigo_producto, 1, 16)] 
# Se crea una nueva columna "codigo_producto_16" que contiene los primeros 16 caracteres de la columna "codigo_producto".

Precios_ofertados <- Prel_1[, .(Promedio_ofertados = mean(precio_unitario_ofertado_colones, na.rm = TRUE),
                                Conteo_Oferta = length(precio_unitario_ofertado_colones)),
                            keyby = .(codigo_producto_16)] 
# Se calcula el promedio de los precios unitarios ofertados y el conteo de ofertas para cada código de producto de 16 caracteres.

Precios_ofertados[,.N, keyby = Conteo_Oferta][, Porcentaje := (N/sum(N))*100][] 
# Se calcula el porcentaje de ofertas únicas en función del conteo de ofertas.

Precios_compilados <- merge(x = Promedio_Costos,
                            y = Precios_ofertados,
                            by.x = "codigoProductoAdjudicado_16",
                            by.y = "codigo_producto_16",
                            all.x = TRUE) 
# Se realiza una unión entre los dataframes Promedio_Costos y Precios_ofertados, basada en las columnas "codigoProductoAdjudicado_16" y "codigo_producto_16". Se agregan los promedios de precios adjudicados y ofertados para cada código de producto.

Costos.1 <- merge(x = Costos, y = Precios_compilados, by = "codigoProductoAdjudicado_16", all.x = TRUE) 
# Se realiza una unión entre los dataframes Costos y Precios_compilados, basada en la columna "codigoProductoAdjudicado_16". Se agrega la información de precios compilados a los datos de costos.

Costos.1[,.N, keyby = codigoProductoAdjudicado_16][, Porcentaje := (N/sum(N))*100][order(-N)] 
# Se cuenta la frecuencia de cada código de producto adjudicado y se calcula el porcentaje correspondiente.

Costos.1[,.N, keyby = Conteo_Adjudicado][, Porcentaje := (N/sum(N))*100][] 
# Se cuenta la frecuencia de adjudicaciones para cada conteo de adjudicaciones y se calcula el porcentaje correspondiente.

Costos.1[,.N, keyby = Conteo_Oferta][, Porcentaje := (N/sum(N))*100][]
# Se cuenta la frecuencia de ofertas para cada conteo de ofertas y se calcula el porcentaje correspondiente.

Costos.1[Conteo_Adjudicado != 1 & Conteo_Oferta == 1]
# Se seleccionan las filas donde el conteo de adjudicaciones es diferente de 1 y el conteo de ofertas es igual a 1.

Costos.1[Conteo_Adjudicado > Conteo_Oferta]
# Se seleccionan las filas donde el conteo de adjudicaciones es mayor que el conteo de ofertas.

Costos.1[,.N, keyby = .(Prom.precioUnitarioAdjudicado)][]
# Se cuenta la frecuencia de los promedios de precios unitarios adjudicados.

summary(Costos.1$Prom.precioUnitarioAdjudicado)
# Se muestra un resumen estadístico del campo "Prom.precioUnitarioAdjudicado".

summary(Costos.1$Promedio_ofertados)
# Se muestra un resumen estadístico del campo "Promedio_ofertados".

Costos.1[, Desv.precioUnitarioAdjudicado := ifelse(Conteo_Adjudicado >= 3,
                                                   Prec.Unit.Adj.Col / Prom.precioUnitarioAdjudicado,
                                                   Prec.Unit.Adj.Col / ((Prom.precioUnitarioAdjudicado + Promedio_ofertados) / 2))]
# Se calcula el indicador de desviación (razón) entre cada adjudicación y el precio promedio, considerando distintos casos según el conteo de adjudicaciones.

nrow(Costos.1[Desv.precioUnitarioAdjudicado == 1])
# Se cuenta la cantidad de filas donde el indicador de desviación es igual a 1.

summary(Costos.1$Desv.precioUnitarioAdjudicado)
# Se muestra un resumen estadístico del campo "Desv.precioUnitarioAdjudicado".

Costos.1[,.N, keyby = .(codigoProductoAdjudicado_16,Prec.Unit.Adj.Col,Prom.precioUnitarioAdjudicado,Desv.precioUnitarioAdjudicado)][]
# Se cuentan las frecuencias de los códigos de producto, precios unitarios adjudicados, promedios de precios adjudicados y desviaciones.

(Costos.1[Desv.precioUnitarioAdjudicado == max(Desv.precioUnitarioAdjudicado, na.rm = TRUE),.(Prec.Unit.Adj.Col,Prom.precioUnitarioAdjudicado,Desv.precioUnitarioAdjudicado)])
# Se seleccionan las filas donde la desviación es máxima y se muestra información sobre los precios unitarios adjudicados, promedios de precios adjudicados y desviaciones correspondientes.

summary(Costos.1$Desv.precioUnitarioAdjudicado)
# Se muestra un resumen estadístico del campo "Desv.precioUnitarioAdjudicado".


###************************** Indicador 2**************************###

#2. Grado de desviación del monto unitario adjudicado de la linea con respecto a los precios de referencia (presupuestado en la solicitud)
Costos.1[, Desv.Monto.Unit.Adj := ifelse(precioUnitarioEstimado != 0,
                                         Prec.Unit.Adj.Col / precioUnitarioEstimado,
                                         Prec.Unit.Adj.Col)]
# Se calcula el indicador de desviación del monto unitario adjudicado con respecto al precio estimado, considerando distintos casos.

summary(Costos.1$Desv.Monto.Unit.Adj)
# Se muestra un resumen estadístico del campo "Desv.Monto.Unit.Adj".

summary(Costos.1$Desv.Monto.Unit.Adj[is.infinite(Costos.1$Desv.Monto.Unit.Adj) == FALSE])
# Se muestra un resumen estadístico del campo "Desv.Monto.Unit.Adj", excluyendo los valores infinitos.

Costos.1[,.(Prec.Unit.Adj.Col,precioUnitarioEstimado,Desv.Monto.Unit.Adj)][]
# Se seleccionan los campos relevantes y se muestran los datos correspondientes.

Costos.1[,.N, keyby = .(Desv.Monto.Unit.Adj)][order(-N)][]
# Se cuentan las frecuencias de las desviaciones del monto unitario adjudicado y se ordenan de mayor a menor.



###************************** Indicador 3**************************###

# 3. Monto adjudicado unitario de la linea se mantuvo igual al monto ofertado.
Costos.1[, Monto.Adj.igual := ifelse(Prec.Unit.Ofer.Col == 0,
                                     Prec.Unit.Adj.Col,
                                     Prec.Unit.Adj.Col / Prec.Unit.Ofer.Col)]
# Se calcula el indicador de si el monto unitario adjudicado de la línea se mantuvo igual al monto ofertado, considerando diferentes casos.

summary(Costos.1$Monto.Adj.igual)
# Se muestra un resumen estadístico del campo "Monto.Adj.igual".

Costos.1[,.N, keyby = .(Monto.Adj.igual)][order(-N)][]
# Se cuentan las frecuencias de las diferentes categorías del indicador y se ordenan de mayor a menor.

uniqueN(x = Costos,by = c("numeroActo","numeroOferta","idLinea")) #224337
uniqueN(x = Costos.1,by = c("numeroActo","numeroOferta","idLinea")) #224337
# Se verifica que las dimensiones sigan siendo las mismas antes y después de calcular el indicador.



###************************** Indicador 4**************************###

# 4. Grado de desviaci?n entre el monto de la linea adjudicada con respecto a la siguiente linea ofertada m?s baja
Costos.1[, Desv.Mas.Baja := ifelse(precioUnitarioMenorColones == 0,
                                   Prec.Unit.Adj.Col,
                                   Prec.Unit.Adj.Col / precioUnitarioMenorColones)]

summary(Costos.1$Desv.Mas.Baja)
# Se muestra un resumen estadístico del grado de desviación entre el monto de la línea adjudicada y el monto de la siguiente línea ofertada más baja.

Costos.1[,.N, keyby = .(Desv.Mas.Baja)][]
# Se cuentan las frecuencias de las diferentes categorías del indicador de desviación y se muestra el resultado.

nrow(Costos.1[Desv.Mas.Baja == 0,.(Prec.Unit.Adj.Col, precioUnitarioMenorColones, Desv.Mas.Baja)]) # 0
nrow(Costos.1[Desv.Mas.Baja == Inf,.(Prec.Unit.Adj.Col, precioUnitarioMenorColones, Desv.Mas.Baja)]) # 280
nrow(Costos.1[Desv.Mas.Baja == Inf,.(numeroActo, numeroOferta, idLinea, Prec.Unit.Adj.Col, precioUnitarioMenorColones, Desv.Mas.Baja)]) # Problemas con esto

Menor.Desv.Baja <- Costos.1[Desv.Mas.Baja < 1][]
Menor.Desv.Baja[,.N, keyby = .(tipoMonedaOfertada)][]
# Se filtran los casos en los que el precioUnitarioMenorColones es mayor al monto adjudicado y se cuentan las frecuencias por tipo de moneda ofertada.

uniqueN(x = Costos,by = c("numeroActo","numeroOferta","idLinea")) #224337
uniqueN(x = Costos.1,by = c("numeroActo","numeroOferta","idLinea"))
# Se verifica que las dimensiones sigan siendo las mismas antes y después de calcular el indicador.




###************************** Indicador 5**************************###

# 5. Grado de desviación del adjudicado vs el contratado. Como todos son adj/x lo voy a dejar

# Se establece la conexión con la base de datos SICOP y se obtiene la tabla "LineasContratadas"
# SICOP <-odbcConnect("SICOP", uid="casp", pwd="Casp2021*")
# tablas <- sqlTables(SICOP, tableType = "TABLE")
contrataciones <- data.table(sqlFetch(SICOP, "dbo.LineasContratadas", as.is=TRUE))

# Se calcula el precio unitario contratado en colones, convirtiendo los valores según la moneda y tipo de cambio
contrataciones[, precio_unitario_contratado_colones := ifelse(tipo_moneda == "CRC",
                                                              as.numeric(precio_unitario_contratado),
                                                              as.numeric(precio_unitario_contratado) * as.numeric(tipo_cambio_crc) * (1 / as.numeric(tipo_cambio_dolar)))]

# Se realiza un merge entre los datos de Costos.1 y las contrataciones utilizando ciertas columnas como claves
Costos.2 <- merge(x = Costos.1, y = contrataciones[, c("nro_acto","nro_sicop","nro_linea_cartel", "precio_unitario_contratado_colones")], 
                  by.x = c("numeroActo","idProcedimiento","idLinea"),
                  by.y = c("nro_acto","nro_sicop","nro_linea_cartel"), 
                  all.x = TRUE)

# Se calcula el número de combinaciones únicas en las contrataciones para verificar la unicidad de los datos
uniqueN(x = contrataciones, by = c("nro_acto","nro_sicop","nro_linea_contrato", "nro_linea_contrato","nro_linea_cartel")) #489964

# Se crea una columna "nombre_unico" concatenando ciertas columnas para identificar combinaciones únicas en Costos.2
Costos.2[, nombre_unico := do.call(paste, c(.SD, sep = "_")), .SDcols = c("numeroActo","numeroOferta","idLinea")]

# Se cuenta el número de veces que aparece cada combinación única en Costos.2 y se agrega la columna "Conteo_nombre"
Costos.2[, Conteo_nombre := length(anno), keyby = .(nombre_unico)]

# Se filtran los casos donde el precio unitario contratado es NA y no se repiten combinaciones únicas
Costos.2[Conteo_nombre == 1 & is.na(precio_unitario_contratado_colones), ]

# Se eliminan las filas con precios unitarios contratados NA
Costos.2 <- na.omit(Costos.2, cols = c("precio_unitario_contratado_colones"))

# Se vuelve a contar el número de veces que aparece cada combinación única después de eliminar los NA
Costos.2[, Conteo_nombre := length(anno), keyby = .(nombre_unico)]

# Se muestra el conteo del número de combinaciones únicas y su porcentaje con respecto al total
Costos.2[,.N, keyby = Conteo_nombre][order(-N)][, Porcentaje := (N/sum(N))*100][]

# Se muestra el conteo del número de combinaciones únicas y su porcentaje con respecto al total
Costos.2[,.N, keyby = Conteo_nombre][order(-N)][, Porcentaje := (N/sum(N))*100][]

# Se calcula el promedio del precio unitario contratado para cada combinación única
Promedio_contratado <- Costos.2[, .(Prom.contr = mean(precio_unitario_contratado_colones, na.rm = TRUE)), 
                                keyby = c("numeroActo", "numeroOferta", "idLinea")]

# Se realiza un merge entre Costos.2 y el promedio contratado utilizando ciertas columnas como claves
Costos.2 <- merge(x = Costos.2, y = Promedio_contratado, by = c("numeroActo", "numeroOferta", "idLinea"), all.x = TRUE)

# Se agrega una columna "Verif" que indica si el precio unitario contratado es igual al promedio contratado
Costos.2[, Verif := ifelse(Prom.contr == precio_unitario_contratado_colones, 0, 1)]

# Se muestra el promedio contratado, el precio unitario contratado y la columna Verif
Costos.2[, .(Prom.contr, precio_unitario_contratado_colones, Verif)]

# Se muestra el conteo del número de combinaciones únicas donde hay diferencias entre el precio contratado y el promedio
Costos.2[,.N, keyby = Verif][order(-N)][, Porcentaje := (N/sum(N))*100][]

# Se muestra cómo se distribuyen las combinaciones únicas con diferencias de precio
Costos.2[Verif == 1, .N, keyby = Conteo_nombre][order(-N)][, Porcentaje := (N/sum(N))*100][]

# Se verifica que el promedio contratado es igual al precio contratado para los casos únicos (98.8% o 233,832 combinaciones)
nrow(Costos.2[precio_unitario_contratado_colones == Prom.contr])

# Se realiza un merge entre Costos.1 y el promedio contratado, asignando el precio contratado a Costos.3
Costos.3 <- merge(x = Costos.1, y = Promedio_contratado, by = c("numeroActo", "numeroOferta", "idLinea"), all.x = TRUE)


###**************************Manteniendo variables relevantes**************************###

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

#**************************************Construcción de la Base de datos**************************# 

# Primero guardamos todos los archivos en una lista
lista <- list(Indicadores_Alcance, Indicadores_Costo, Indicadores_Plazo)

# Unimos los archivos en uno solo mediante la función `merge()` dentro del comando `Reduce()`
Set_final <- Reduce(function(...) merge(..., by = c("numeroActo","numeroOferta","idLinea"), all = TRUE), lista)

# Obtenemos el número de combinaciones únicas en las columnas "numeroActo", "numeroOferta" y "idLinea" de `Set_final`
uniqueN(Set_final, by = c("numeroActo","numeroOferta","idLinea")) #Dim 224337

# Calculamos la cantidad de valores NA en cada columna de `Set_final`
colSums(is.na(Set_final))

# ELIMINANDO LAS NA 
Set_final_2 <- Set_final[,c(1,2,3,7,9,15:17,21,23)]
Set_Modelo_sin_NA <- Set_final_2[na.omit(Set_final_2)]
Set_Modelo_sin_NA <- Set_Modelo_sin_NA[,c(1:10)]

#### LIMPIEZA DE DATOS ####

# Lista de archivos a conservar
objetos_a_conservar <- c("Set_Modelo_sin_NA", "Indicadores_Plazo", "Indicadores_Alcance", "Indicadores_Costo", 
                         "LineasOfertadas", "Ofertas", "Costos", "SICOP", "tablas")

# Obtenemos la lista de todos los objetos existentes en el entorno de trabajo
objetos_totales <- ls()

# Obtenemos la lista de objetos a eliminar
objetos_a_eliminar <- setdiff(objetos_totales, objetos_a_conservar)

# Eliminamos los objetos no deseados
rm(list = objetos_a_eliminar)

## Acotación ##
datos<-Set_Modelo_sin_NA

# Establecemos los percentiles para la validación de resultados utilizando la función `quantile()`
Deciles_Compras.Unico <- quantile(datos$Compras.unico, probs = seq(0, 1, 0.01), na.rm = FALSE)
Deciles_Raz.Solic.Contra <- quantile(datos$Raz.solicitada.contratada, probs = seq(0, 1, 0.01), na.rm = FALSE)
Deciles_Desv.Adj.Ofer.Baj <- quantile(datos$desv.adj.vs.oferta.mas.baja, probs = seq(0, 1, 0.01), na.rm = FALSE)
Deciles_Desv.Cont.Adju <- quantile(datos$desv.cont.vs.adj, probs = seq(0, 1, 0.01), na.rm = FALSE)
Deciles_Desv.Ini.Adj <- quantile(datos$Desv.inicioadju, probs = seq(0, 1, 0.01), na.rm = FALSE)
Deciles_Desv.Cont.Pri.Adj <- quantile(datos$Desv.contpriadj, probs = seq(0, 1, 0.01), na.rm = FALSE)
Deciles_Cant.Mod.Plazo <- quantile(datos$CantModf_Plazo, probs = seq(0, 1, 0.01), na.rm = FALSE)

# Acotación de variables en el dataframe 'datos'
# Para cada variable, se verifica si es mayor que el percentil 100 (máximo)
# Si es mayor, se asigna el valor del percentil 100 multiplicado por 2, de lo contrario se mantiene el valor original

datos[, Compras.unico.Acot:=ifelse(datos$Compras.unico > Deciles_Compras.Unico[100],Deciles_Compras.Unico[100]*2,Compras.unico)]
datos[, Raz.solicitada.contratada.Acot:=ifelse(datos$Raz.solicitada.contratada > Deciles_Raz.Solic.Contra[100],Deciles_Raz.Solic.Contra[100]*2,Raz.solicitada.contratada)]
datos[, desv.adj.vs.oferta.mas.baja.Acot:=ifelse(datos$desv.adj.vs.oferta.mas.baja > Deciles_Desv.Adj.Ofer.Baj[100],Deciles_Desv.Adj.Ofer.Baj[100]*2,desv.adj.vs.oferta.mas.baja)]
datos[, desv.cont.vs.adj.Acot:=ifelse(datos$desv.cont.vs.adj > Deciles_Desv.Cont.Adju[100],Deciles_Desv.Cont.Adju[100]*2,desv.cont.vs.adj)]
datos[, Desv.inicioadju.Acot:=ifelse(datos$Desv.inicioadju > Deciles_Desv.Ini.Adj[100],Deciles_Desv.Ini.Adj[100]*2,Desv.inicioadju)]
datos[, Desv.contpriadj.Acot:=ifelse(datos$Desv.contpriadj > Deciles_Desv.Cont.Pri.Adj[100],Deciles_Desv.Cont.Pri.Adj[100]*2,Desv.contpriadj)]
datos[, CantModf_Plazo.Acot:=ifelse(datos$CantModf_Plazo > Deciles_Cant.Mod.Plazo[100],Deciles_Cant.Mod.Plazo[100]*2,CantModf_Plazo)]


# Creación de un nuevo dataframe 'datos_acotados' que excluye las columnas 4 a 10 del dataframe 'datos'
datos_acotados <- datos[, -c(4:10)]

# Normalización de variables en el dataframe 'datos_acotados'
# Para cada variable, se calcula el logaritmo natural del valor original más 1

datos_acotados[, Log_Compras.unico.Acot:=log(datos$Compras.unico +1)]
datos_acotados[, Log_Raz.solicitada.contratada.Acot:=log(datos$Raz.solicitada.contratada.Acot+1)]
datos_acotados[, Log_desv.adj.vs.oferta.mas.baja.Acot:=log(datos$desv.adj.vs.oferta.mas.baja.Acot+1)]
datos_acotados[, Log_desv.cont.vs.adj.Acot:=log(datos$desv.cont.vs.adj.Acot+1)]
datos_acotados[, Log_Desv.inicioadju.Acot:=log(datos$Desv.inicioadju.Acot+1)]
datos_acotados[, Log_Desv.contpriadj.Acot:=log(datos$Desv.contpriadj.Acot+1)]
datos_acotados[, Log_CantModf_Plazo.Acot:=log(datos$CantModf_Plazo.Acot+1)]

# Creación de un nuevo dataframe 'datos_Log' que excluye las columnas 4 a 10 de 'datos_acotados'
datos_Log <- datos_acotados[, -c(4:10)]


#**************************************MODELOS**************************# 

###************************** Componentes principales robustos**************************###

#Caso de analisis de divergencias entre 2018 y 2021
datos <- datos_Log[,-c(1:3)]

# 1. Correr el modelo PCA 
pca <- prcomp(x = datos, center = TRUE,scale. = TRUE)

# Examinando los eigenvalues y la varianza acumulada para cada componente
get_eig(X = pca)

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

# 2. Usando la funcion de RPCA con la matriz de covarianzas
rpca  <- PcaCov(x = datos, cov.control = CovControlMcd(), scale = TRUE, signflip = TRUE)

# Examinando los eigenvalues y la varianza acumulada para cada componente
rpca_eigen_var <- tibble(
  componente = seq_len(rpca$k),
  varianza   = rrcov::getSdev(rpca)^2,
  porcnt_varianza = 100 * varianza / sum(varianza),
  porcnt_varianza_acumulada = cumsum(porcnt_varianza)
)
rpca_eigen_var

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

# Se añade el error de reconstrucción al dataframe original.
datos$error_reconstruccion_pca <- error_reconstruccion_pca
datos$error_reconstruccion_rpca <- error_reconstruccion_rpca

# Se obtienen los nombres de las variables en el dataframe.
names(datos)

# Se muestra un resumen estadístico del error de reconstrucción para PCA y RPCA.
summary(datos$error_reconstruccion_pca)
summary(datos$error_reconstruccion_rpca)

# Se calculan los percentiles del error de reconstrucción para establecer un umbral.
Deciles_pca <- quantile(datos$error_reconstruccion_pca, probs = seq(0, 1, 0.05), na.rm = FALSE)
Deciles_rpca <- quantile(datos$error_reconstruccion_rpca, probs = seq(0, 1, 0.05), na.rm = FALSE)

# Se muestra el valor del percentil 20 para PCA y RPCA.
Deciles_pca[20]
Deciles_rpca[20]

# Se convierte el dataframe en un objeto de tipo data.table.
datos <- as.data.table(datos)

# Se agrega una columna que indica si el error de reconstrucción para PCA supera el umbral establecido.
datos[, Anomalia_PCA := ifelse(error_reconstruccion_pca >= Deciles_pca[20], 1, 0)]

# Se agrega una columna que indica si el error de reconstrucción para RPCA supera el umbral establecido.
datos[, Anomalia_RPCA := ifelse(error_reconstruccion_rpca >= Deciles_rpca[20], 1, 0)]

# Se renombran las columnas "error_reconstruccion_pca" y "error_reconstruccion_rpca"
setnames(datos, old = c("error_reconstruccion_pca", "error_reconstruccion_rpca"), new = c("Score_Pca", "Score_Rpca"))

# Se realiza un cruce de las columnas Anomalia_PCA y Anomalia_RPCA para contar las ocurrencias
Cruce_anomalias <- datos[, .N, keyby = .(Anomalia_PCA, Anomalia_RPCA)][, Porcentaje := (N/sum(N)) * 100][]

# Se crea un dataframe de resultados con las variables relevantes
Resultados_RPCA <- data.frame(numeroActo = datos_Log$numeroActo,
                              numeroOferta = datos_Log$numeroOferta,
                              idLinea = datos_Log$idLinea,
                              score_RPCA = datos$Score_Rpca,
                              anomalia_RPCA = datos$Anomalia_RPCA,
                              stringsAsFactors = FALSE)

###************************** KMEANS**************************###

# Se eliminan las columnas 1 a 3 del dataframe datos_Log
datos <- datos_Log[, -c(1:3)]

# Se calcula el score_KMEANS utilizando el algoritmo KNN_AGG
datos$score_KMEANS <- KNN_AGG(dataset = datos[, -c(1:3)], k_min = 10, k_max = 15)

# Se asignan nombres consecutivos a los valores del score_KMEANS
names(datos$score_KMEANS) <- 1:nrow(datos)

# Se ordenan los valores del score_KMEANS en orden descendente
sorted_indices <- sort(datos$score_KMEANS, decreasing = TRUE)

# Se muestra el histograma de los valores del score_KMEANS
hist(datos$score_KMEANS)

# Se calcula el logaritmo natural del score_KMEANS y se asigna a la columna log.score_KMEANS
datos$log.score_KMEANS <- log(datos$score_KMEANS, base = exp(1))

# Se muestran estadísticas resumidas del score_KMEANS y las primeras filas del dataframe datos
summary(datos$score_KMEANS)
head(datos)

# Se muestra el histograma del log.score_KMEANS
hist(datos$log.score_KMEANS)

# Se muestran estadísticas resumidas del log.score_KMEANS, valor mínimo y máximo
summary(datos$log.score_KMEANS)
min(datos$log.score_KMEANS)
max(datos$log.score_KMEANS)

# Se calculan los percentiles del score_KMEANS para establecer un punto de corte
Deciles_KMEANS <- quantile(datos$score_KMEANS, probs = seq(0, 1, 0.05), na.rm = FALSE)
Deciles_KMEANS  
# Se asigna la etiqueta Anomalo_KMEANS a las observaciones que superan el percentil 20
datos[, Anomalo_KMEANS := ifelse(score_KMEANS >= Deciles_KMEANS[20], 1, 0)]

# Se calcula la suma de valores Anomalo_KMEANS
sum(datos$Anomalo_KMEANS)

#Creando la base para guardar los elementos
Resultados_KMEANS <- data.frame(numeroActo= datos_Log$numeroActo,
                                numeroOferta= datos_Log$numeroOferta,
                                idLinea= datos_Log$idLinea,
                                score_KMEANS= datos$score_KMEANS,  
                                anomalia_KMEANS = datos$Anomalo_KMEANS,
                                stringsAsFactors = FALSE)

###************************** ISOLATION FOREST**************************###

# Se eliminan las columnas 1 a 3 del dataframe datos_Log
datos <- datos_Log[, -c(1:3)]

# Se realiza el análisis con el algoritmo SCI Forest
iso_sci <- isolation.forest(datos, ndim = 2, ntrees = 500, nthreads = 1, prob_pick_pooled_gain = 0, prob_pick_avg_gain = 1)

# Se obtienen las predicciones del algoritmo SCI Forest
pred_sci <- predict(iso_sci, datos)

# Se encuentra el valor máximo de las predicciones
max_sci <- max(pred_sci)

# Se calculan los percentiles de las predicciones para establecer un punto de corte
Deciles_sci <- quantile(pred_sci, probs = seq(0, 1, 0.05), na.rm = FALSE)

# Se cuenta el número de valores mayores que 0.5 en el percentil 20
pred_sci_0.5 <- sum(Deciles_sci[20] > 0.5)

# Se asigna la etiqueta outlier_sci_0.5 a las observaciones que superan el percentil 20
datos$outlier_sci_0.5 <- 1 * (pred_sci > Deciles_sci[20])

# Se calcula la suma de valores outlier_sci_0.5
sum(datos$outlier_sci_0.5)

# Se crea un dataframe Resultados_ISO con los resultados del análisis SCI Forest
Resultados_ISO <- data.frame(numeroActo = datos_Log$numeroActo,
                             numeroOferta = datos_Log$numeroOferta,
                             idLinea = datos_Log$idLinea,
                             anomalia_iso = datos$outlier_sci_0.5,
                             score_iso = pred_sci,
                             stringsAsFactors = FALSE)


###************************** Local Outlier**************************###

# Se eliminan las columnas 1 a 3 del dataframe datos_Log
datos <- datos_Log[, -c(1:3)]

# Se calcula el LOF score con un vecindario de 10 puntos
datos$outlier_score <- LOF(dataset = datos[, -c(1:3)], k = 10)

# Se ordenan los puntajes de outliers en orden descendente
names(datos$outlier_score) <- 1:nrow(datos)
sort(datos$outlier_score, decreasing = TRUE)

# Se inspecciona la distribución de los puntajes de outliers
hist(datos$outlier_score)
summary(datos$outlier_score)

# Se calculan los percentiles de los puntajes de outliers para establecer un punto de corte
Deciles_LOF <- quantile(datos$outlier_score, probs = seq(0, 1, 0.05), na.rm = TRUE)

# Se asigna la etiqueta Anomalia_LOF a las observaciones que superan el percentil 20
datos[, Anomalia_LOF := ifelse(datos$outlier_score >= Deciles_LOF[20], 1, 0)]

# Se reemplazan los valores NA por 0, asumiendo que son NA debido a una alta densidad de puntos
datos[is.na(datos)] <- 0

# Se cuenta el número de valores Anomalia_LOF y se calcula el porcentaje
Conteo_anomalias <- datos[, .N, keyby = .(Anomalia_LOF)][, Porcentaje := (N / sum(N)) * 100][]
Conteo_anomalias

# Se crea un dataframe Resultados_LOF con los resultados del análisis LOF
Resultados_LOF <- data.frame(numeroActo = datos_Log$numeroActo,
                             numeroOferta = datos_Log$numeroOferta,
                             idLinea = datos_Log$idLinea,
                             anomalia_LOF = datos$Anomalia_LOF,
                             score_LOF = datos$outlier_score,
                             stringsAsFactors = FALSE)



###************************** LIMPIEZA DE LO PREVIO REALIZADO**************************###

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

## Instituciones relacionadas -----------

# Carga los datos de la tabla "dbo.Instituciones" desde la base de datos SICOP y los almacena en el objeto "Instituciones"
Instituciones <- data.table(sqlFetch(SICOP, "dbo.Instituciones", as.is = TRUE))

# Carga los datos de la tabla "dbo.lineasadjudicadas" desde la base de datos SICOP y los almacena en el objeto "lineasadjudicadas"
lineasadjudicadas <- data.table(sqlFetch(SICOP, "dbo.lineasadjudicadas", as.is = TRUE))

# Carga los datos de la tabla "dbo.procedimientos" desde la base de datos SICOP y los almacena en el objeto "procedimientos"
procedimientos <- data.table(sqlFetch(SICOP, "dbo.procedimientos", as.is = TRUE))

# Fusiona los data frames lineasadjudicadas y una selección de columnas del data frame procedimientos basado en la columna "nro_sicop".
# Utiliza all.x = TRUE para mantener todas las filas del data frame lineasadjudicadas.
Prel_0 <- merge(lineasadjudicadas, procedimientos[,.(nro_sicop, nro_procedimiento, cedula_institucion)], by = "nro_sicop", all.x = TRUE)

# Fusiona Prel_0 con el data frame Instituciones basado en las columnas "cedula_institucion" y "cedula".
# Utiliza all.x = TRUE para mantener todas las filas de Prel_0.
Prel_1 <- merge(Prel_0, Instituciones[,.(cedula, nombre_institucion)], by.x = "cedula_institucion", by.y = "cedula", all.x = TRUE)

# Devuelve los nombres de las columnas de Prel_1.
names(Prel_1)

# Muestra las primeras filas de Prel_1.
head(Prel_1)

# Agrega una nueva columna llamada "anno" a Prel_1, que contiene los primeros cuatro caracteres de la columna "nro_procedimiento".
Prel_1[, anno:=substr(nro_procedimiento,1,4)]

# Crea un nuevo data frame llamado Identificador_Instituciones que contiene las columnas seleccionadas de Prel_1.
Identificador_Instituciones <- Prel_1[, .(cedula_institucion, nro_acto, nro_oferta, nro_linea, anno, nombre_institucion)]

# Calcula el número de combinaciones únicas de "nro_acto", "nro_oferta" y "nro_linea" en Identificador_Instituciones.
uniqueN(Identificador_Instituciones, by=c("nro_acto","nro_oferta","nro_linea"))

# Filtra Identificador_Instituciones para incluir solo las filas donde el valor de "anno" es menor a 2023 y mayor a 2018.
Identificador_Instituciones <- Identificador_Instituciones[(anno < 2023 & anno > 2018),]

# Renombra las columnas de Identificador_Instituciones con nuevos nombres.
colnames(Identificador_Instituciones) <- c("idInstitucion","numeroActo","numeroOferta","idLinea", "anno","Nombre_Institucion")

#### LIMPIEZA DE DATOS ####

# Lista de archivos a conservar
objetos_a_conservar <- c("Set_Modelo_sin_NA", 
                         "Indicadores_Plazo", "Indicadores_Alcance", "Indicadores_Costo",
                         "Resultados_ISO","Resultados_KMEANS","Resultados_RPCA","Resultados_LOF",
                         "Identificador_Instituciones",
                         "lineasadjudicadas","procedimientos","Instituciones",
                         "LineasOfertadas", "Ofertas", "Costos", "SICOP", "tablas")

# Obtiene una lista de todos los nombres de objetos existentes en el entorno actual.
objetos_totales <- ls()

# Calcula la diferencia entre los nombres de objetos totales y los nombres de objetos a conservar.
objetos_a_eliminar <- setdiff(objetos_totales, objetos_a_conservar)

# Elimina los objetos que no se encuentran en la lista de objetos a conservar.
objetos_a_eliminar <- setdiff(objetos_totales, objetos_a_conservar)
rm(list = objetos_a_eliminar)


###************************** Consolidacion y priorización de lineas**************************###

# Creamos una lista que contiene los data frames de anomalías que queremos consolidar
lista <- list(Resultados_ISO, Resultados_KMEANS, Resultados_LOF, Resultados_RPCA)

# Combinamos los data frames en uno solo utilizando la función Reduce y merge
Set_final <- Reduce(function(...) merge(..., by = c("numeroActo", "numeroOferta", "idLinea"), all = TRUE), lista)

# Obtenemos los nombres de las columnas en el data frame consolidado
names(Set_final)

# Contamos el número de filas únicas en función de las columnas "numeroActo", "numeroOferta" e "idLinea"
uniqueN(Set_final, by = c("numeroActo", "numeroOferta", "idLinea"))

# Calculamos la suma de valores nulos en cada columna del data frame
colSums(is.na(Set_final))

# Mostramos un resumen estadístico del data frame consolidado
summary(Set_final)

# Unimos el data frame consolidado con el data frame "Identificador_Instituciones" utilizando las columnas "numeroActo", "numeroOferta" e "idLinea"
Set_final <- merge(Set_final, Identificador_Instituciones, by = c("numeroActo", "numeroOferta", "idLinea"), all.x = TRUE)

# Exportamos los datos
setDT(Set_final)

# Creamos una nueva columna llamada "Consenso" que suma las columnas de anomalías
Set_final[, Consenso := anomalia_KMEANS + anomalia_iso + anomalia_LOF + anomalia_RPCA]

# Mostramos un resumen de la columna "Consenso"
summary(Set_final$Consenso)

# Calcula el recuento de filas por combinación de "idInstitucion", "Nombre_Institucion" y "Consenso" en el data frame Set_final.
# Ordena los resultados en orden descendente según el valor de "Consenso".
Dist_consenso_NA <- Set_final[,.N, keyby = .(idInstitucion,Nombre_Institucion, Consenso)][order(-Consenso)][]

# Transpone el data frame Dist_consenso_NA, creando nuevas columnas para cada valor único de "Consenso".
Dist_consenso_transpuesto_NA <- dcast(data = Dist_consenso_NA, formula = idInstitucion + Nombre_Institucion ~ Consenso, value.var = "N")

# Calcula una nueva columna llamada "Uno_o_mas" en Dist_consenso_transpuesto_NA que contiene la suma de las columnas 4 a 7, omitiendo los valores NA.
Dist_consenso_transpuesto_NA[,Uno_o_mas := rowSums(Dist_consenso_transpuesto_NA[,c(4:7), with = FALSE], na.rm = TRUE)]

# Calcula una nueva columna llamada "Uno_o_mas_Porcent" en Dist_consenso_transpuesto_NA que representa el porcentaje de "Uno_o_mas" en relación a la suma de las columnas 3 a 7, omitiendo los valores NA.
Dist_consenso_transpuesto_NA[,Uno_o_mas_Porcent := (Uno_o_mas/rowSums(Dist_consenso_transpuesto_NA[,c(3:7), with = FALSE], na.rm = TRUE)) * 100]

# Ordena Dist_consenso_transpuesto_NA en orden descendente según el valor de "Uno_o_mas".
Dist_consenso_transpuesto_NA[order(-Uno_o_mas)]

# Resumen de la cantidad de registros únicos por las variables "numeroActo" y "numeroOferta" donde "Consenso" es igual a cero para cada institución.
Unicos <- unique(Set_final[Consenso == 0], by = c("numeroActo", "numeroOferta", "idInstitucion", "Nombre_Institucion"))
Freq.Inst.unicos_0 <- Unicos[,.N, keyby = c("idInstitucion", "Nombre_Institucion")]
Freq.Inst.unicos_0


# Generación de muestras aleatorias para cada institución.
# Esta operación se realiza para los casos en los que el valor de "Consenso" es igual a cero.
Set_final[Consenso == 0,.N, keyby = .(idInstitucion, Nombre_Institucion)][order(N)][]

## Jerarquizandoción de líneas ------------------

# CASO DEL ISO

# Filtra los registros en Set_final donde "anomalia_iso" es igual a 1 y los asigna a la variable puntaje_anomalia_ISO.
puntaje_anomalia_ISO <- Set_final[anomalia_iso == 1,]

# Calcula los deciles del campo "score_iso" en puntaje_anomalia_ISO.
Deciles_ISO <- quantile(puntaje_anomalia_ISO$score_iso, probs = seq(0, 1, 0.1), na.rm = FALSE)
Deciles_ISO[11]

# Asigna una escala de nota en base a los deciles para el campo "score_iso" en Set_final.
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

# Asignar el precio unitario contratado en colones basado en la moneda contratada.
# Si la moneda contratada es "CRC", el precio unitario contratado se asigna directamente.
# De lo contrario, se realiza la conversión utilizando los tipos de cambio y el precio unitario en dólares.
costos[, precio_unitario_contratado_colones := ifelse(tipoMonedaContratada == "CRC", as.numeric(precioUnitarioContratado), as.numeric(precioUnitarioContratado) * as.numeric(tipoCambioCrcContratado) * (1/as.numeric(tipoCambioDolarContratado)))]

# Calcular el monto de la línea en colones, considerando la cantidad contratada, descuento, IVA, otros impuestos y acarreos.
costos[, Monto_linea_colones := as.numeric(cantidadContratada) * precio_unitario_contratado_colones - as.numeric(descuentoContratado) + as.numeric(ivaContratado) + as.numeric(otrosImpuestosContratado) + as.numeric(acarreosContratado)]

# Combinar los datos de Set_final y costos en base a las columnas "numeroActo", "idLinea" y "numeroOferta".
Set_final <- merge(Set_final, costos[, .(numeroActo, numeroOferta, idLinea, numeroProcedimiento, modalidadProcedimiento, Monto_linea_colones)], by = c("numeroActo", "idLinea", "numeroOferta"), all.x = TRUE)

#### LIMPIEZA DE DATOS
# Lista de archivos a conservar
objetos_a_conservar <- c("Set_Modelo_sin_NA", "Set_final",
                         "Indicadores_Plazo", "Indicadores_Alcance", "Indicadores_Costo",
                         "Resultados_ISO", "Resultados_KMEANS", "Resultados_RPCA", "Resultados_LOF",
                         "LineasOfertadas", "Ofertas", "Costos", "SICOP", "tablas")
objetos_totales <- ls()

# Obtener la lista de objetos a eliminar
objetos_a_eliminar <- setdiff(objetos_totales, objetos_a_conservar)
rm(list = objetos_a_eliminar)

#fwrite(Set_final, "Resultados_Finales.csv", sep = ";", dec = ".")

# Acceder al nombre de las tablas en el objeto "tablas"
tablas$TABLE_NAME

# Obtener los datos de la tabla "lineasadjudicadas" desde la base de datos SICOP
lineasadjudicadas <- data.table(sqlFetch(SICOP, "dbo.lineasadjudicadas", as.is = TRUE))

# Obtener los datos de la tabla "proveedores" desde la base de datos SICOP
proveedores <- data.table(sqlFetch(SICOP, "dbo.proveedores", as.is = TRUE))

# Obtener los datos de la tabla "jerarquiaClasificacionBS" desde la base de datos SICOP
jerarquiaClasificacionBS <- data.table(sqlFetch(SICOP, "dbo.jerarquiaClasificacionBS", as.is = TRUE))

# Obtener los datos de la tabla "identificacionbs" desde la base de datos SICOP
identificacionbs <- data.table(sqlFetch(SICOP, "dbo.identificacionbs",as.is=TRUE))
clasificacionbs <- data.table(sqlFetch(SICOP, "dbo.clasificacionbs",as.is=TRUE))
                                        
# Combinar los datos de Set_final y lineasadjudicadas en base a las columnas "numeroActo", "idLinea" y "numeroOferta".
Set_final_V2<- merge(Set_final,
                     lineasadjudicadas[,.(nro_acto,nro_linea,nro_oferta, cedula_proveedor, codigo_producto)],
                     by.x = c("numeroActo","idLinea","numeroOferta"),
                     by.y = c("nro_acto","nro_linea","nro_oferta"), all.x = TRUE)

# Combinar los datos de Set_final_V2 y proveedores en base a la columna "cedula_proveedor".
Set_final_V2<- merge(Set_final_V2,
                     proveedores[,.(cedula_proveedor, nombre_proveedor, tipo_proveedor, tamano_proveedor)],
                     by = c("cedula_proveedor"), all.x = TRUE)

# Agregar una nueva columna "codigo_identificacion" en Set_final_V2, extrayendo los primeros 16 caracteres de "codigo_producto".
Set_final_V2[,codigo_identificacion:=substr(codigo_producto,1,16)]

# Combinar los datos de Set_final_V2 y identificacionbs en base a la columna "codigo_identificacion".
Set_final_V2<- merge(Set_final_V2,
                     identificacionbs[,.(codigo_identificacion, nombre_identificacion)],
                     by = c("codigo_identificacion"), all.x = TRUE)

# Seleccionar columnas específicas en Set_final_V2 y asignar el resultado a Set_final_V3.
Set_final_V3<-Set_final_V2[,.(numeroProcedimiento, numeroActo, idLinea, numeroOferta, 
                              codigo_identificacion, nombre_identificacion, Nombre_Institucion, idInstitucion,
                              cedula_proveedor, nombre_proveedor, tipo_proveedor, tamano_proveedor,
                              anno, modalidadProcedimiento, Monto_linea_colones, Consenso)]
# Combinar los datos de Set_final_V3 y Set_Modelo_sin_NA en base a las columnas "numeroActo", "idLinea" y "numeroOferta".
Set_final_V4<- merge(Set_final_V3,
                     Set_Modelo_sin_NA,
                     by = c("numeroActo","idLinea","numeroOferta"),
                     all.x = TRUE)

# Combinar los datos de Set_final_V4 y Indicadores_Costo en base a las columnas "numeroActo", "idLinea" y "numeroOferta".
Set_final_V4<- merge(Set_final_V4,
                     Indicadores_Costo,
                     by = c("numeroActo","idLinea","numeroOferta"),
                     all.x = TRUE)
# Combinar los datos de Set_final_V4 y Indicadores_Plazo en base a las columnas "numeroActo", "idLinea" y "numeroOferta".
Set_final_V4<- merge(Set_final_V4,
                     Indicadores_Plazo,
                     by = c("numeroActo","idLinea","numeroOferta"),
                     all.x = TRUE)
# Escribir el contenido de Set_final_V4 en un archivo CSV llamado "Resultados_Finales_Prueba2.csv".
fwrite(Set_final_V4, "Resultados_Finales_Prueba2.csv", sep = ";", dec = ".")


