#*********************************CODIGO DE ANALÍTICA DE DATOS PARA COMPRAS PÚBLICAS*******************************#
#**************************************************ELABORADO POR LA CGR*****************************************#

#**************************************INICIO**************************# 

# LIBRERIAS ----
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
#setwd("C:/Users/humberto.perera/Documents/GitHub")

# Conexion SICOP y CASP
#se debe hacer un paso previo de Configuración de una conexión ODBC a base de datos MSSQL Server, esto requiere de permisos de administrador"

#SICOP <-odbcConnect("ODBC_CGRSQL")
SICOP <-odbcConnect("Conexion_SICOP")
tablas <- sqlTables(SICOP, tableType = "TABLE")

#**************************************INDICADORES DE PLAZO**************************# 
# INDICADORES ----
## Plazo ----
# Cargando las tablas necesarias 
Plazos <- data.table(sqlFetch(SICOP, "ds.plazos",as.is=TRUE))

# Definición de tipo de variable
Plazos$difIniAdj<- as.numeric(Plazos$difIniAdj)
Plazos$difAdjContPri<- as.numeric(Plazos$difAdjContPri)
Plazos$difAdjContUlt<- as.numeric(Plazos$difAdjContUlt)
Plazos$difContPriRec<- as.numeric(Plazos$difContPriRec)
Plazos$difContUltRec<- as.numeric(Plazos$difContUltRec)
Plazos$cantModif<- as.numeric(Plazos$cantModif)

#limpiando los datos para incluir solo datos desde el 2020, y solo bienes
Plazos$codigoProductoAdjudicado_16<- substr(Plazos$codigoProductoAdjudicado, 1, 16)
Plazos<- Plazos[(anno < 2023 & anno > 2019),]
Plazos[, tipo_bien:= substr(codigoProductoSolicitado,1,1)]
Plazos<- Plazos[Plazos$tipo_bien%in%c("1","2","3","4","5","6"), ]

###************************** Indicador 1**************************###
### Indicador Desv.inicioadju ----
# 1. Desviación de la cantidad de días entre la fecha de inicio y la fecha de adjudicación, respecto al "promedio" del bien.
# Primero se calcula los días promedio del bien 
Promedio_iniadj_16 <- Plazos[, .(Prom.inicioadju_16 = mean(difIniAdj,na.rm = TRUE)), keyby = .(codigoProductoAdjudicado_16)][]

# Pegando a Plazo el promedio del codigoProductoAdjudicado
Plazo.1 <- merge(x = Plazos, y = Promedio_iniadj_16,by = "codigoProductoAdjudicado_16",all.x = TRUE)

# Posteriormente se calcula la desviacion entre cada una de las adjudicaciones y el plazo promedio
Plazo.1[, Desv.inicioadju := difIniAdj/Prom.inicioadju_16]


###************************** Indicador 2**************************###
### Indicador Desv.contpriadj ----
# Grado de desviación entre la fecha de adjudicación y la formalización del primer contrato

#Cálculo del promedio de los días entre la fecha de adjudicación y la formalización del primer contrato por bien
Promedio_contpriadj_16 <- Plazos[, .(Prom.contpriradj_16 = mean(difAdjContPri,na.rm = TRUE)), keyby = .(codigoProductoAdjudicado_16)][]

# Pegando a Plazo el promedio del codigoProductoAdjudicado
Plazo.1 <- merge(x = Plazo.1,y = Promedio_contpriadj_16,by = "codigoProductoAdjudicado_16",all.x = TRUE)

# Posteriormente se calcula la desviacion entre cada una de las adjudicaciones y el plazo promedio
Plazo.1[, Desv.contpriadj := difAdjContPri/Prom.contpriradj_16]


###************************** Indicador 4**************************###
### Indicador CantModf_Plazo ----
# Asignando los valores de la columna cantModif a la nueva columna CantModf_Plazo
Plazo.1$CantModf_Plazo <- Plazo.1$cantModif


# creando la tabla
Indicadores_Plazo <- Plazo.1[, .(numeroActo,numeroOferta,idLinea,Desv.inicioadju,Desv.contpriadj,CantModf_Plazo, difIniAdj, Prom.inicioadju_16, difAdjContPri, Prom.contpriradj_16)][]

#### Limpieza de datos
# Lista de archivos a conservar
objetos_a_conservar <- c("Indicadores_Plazo", "SICOP", "tablas")
objetos_totales <- ls()

# Obtener la lista de objetos a eliminar
objetos_a_eliminar <- setdiff(objetos_totales, objetos_a_conservar)
rm(list = objetos_a_eliminar)


#**************************************INDICADORES DE ALCANCE**************************# 
## Alcance ----
#Carga los datos de la tabla "ds.alcance" desde la conexión SICOP y los asigna a la variable Alcance
Alcance <- data.table(sqlFetch(SICOP, "ds.alcance",as.is=TRUE))

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
Alcance<- Alcance[Alcance$tipo_bien%in%c("1","2","3","4","5","6"),]


###************************** Indicador 1**************************###
### Indicador Raz.solicitada.contratada ------
# Cantidad de procedimientos adjudicados del mismo bien en la misma institución durante el año calendario
Alcance.1 <- unique(x = Alcance[,c(4,5,6,22), with = FALSE], by = c("anno", "idInstitucion","codigoProductoAdjudicado_16","idProcedimiento"))
Alcance.1[, Compras.unico := .N, by = .(anno, idInstitucion,codigoProductoAdjudicado_16)][order(-Compras.unico)]


###************************** Indicador 5**************************###
### Indicador Raz.solicitada.contratada ------
# 6. Razón entre cantidad solicitada y cantidad contratada
Alcance[, Raz.solicitada.contratada := cantidadSolicitada/cantidadContratada]

#Manejo de LOS VALORES INFINITOS EN LOS DATOS
Alcance[, Raz.solicitada.contratada := ifelse(is.infinite(Raz.solicitada.contratada), cantidadSolicitada, 
                                              Raz.solicitada.contratada)]


###************************** Manteniendo solamente las variables de identificacion y los indicadores**************************###
Alcance.2 <- unique(x = Alcance.1, by = c("anno", "idInstitucion","codigoProductoAdjudicado_16"))

Alcance.3 <- merge(x = Alcance, y = Alcance.2[,-3,with = FALSE], by = c("anno", "idInstitucion","codigoProductoAdjudicado_16"),all.x = TRUE)

Indicadores_Alcance <- Alcance.3[,.(numeroActo,numeroOferta,idLinea,
                                    Compras.unico, 
                                    Raz.solicitada.contratada, cantidadSolicitada, cantidadContratada)]

#### LIMPIEZA DE DATOS
# Lista de archivos a conservar
objetos_a_conservar <- c("Indicadores_Plazo", "Indicadores_Alcance", "SICOP", "tablas")
objetos_totales <- ls()

# Obtener la lista de objetos a eliminar
objetos_a_eliminar <- setdiff(objetos_totales, objetos_a_conservar)
rm(list = objetos_a_eliminar)


#**************************************INDICADORES DE COSTO**************************# 
## Costo -------------
#Carga los datos de interés
Costos <- data.table(sqlFetch(SICOP, "ds.costos",as.is=TRUE))
LineasOfertadas <- data.table(sqlFetch(SICOP, "dbo.LineasOfertadas",as.is=TRUE))
Ofertas <- data.table(sqlFetch(SICOP, "dbo.Ofertas",as.is=TRUE))

#Ordenamiento de la información de interés
Costos <- Costos[(anno < 2023 & anno > 2019),]
Costos[, tipo_bien := substr(codigoProductoSolicitado, 1, 1)]
Costos <- Costos[Costos$tipo_bien %in% c("1", "2", "3", "4", "5", "6"),]

# Establecimiento de las variables como numériccas
Costos$precioUnitarioEstimado<- as.numeric(Costos$precioUnitarioEstimado)
Costos$precioUnitarioOfertado<- as.numeric(Costos$precioUnitarioOfertado)
Costos$precioUnitarioAdjudicado<- as.numeric(Costos$precioUnitarioAdjudicado)
Costos$precioUnitarioMenorColones<- as.numeric(Costos$precioUnitarioMenorColones)
Costos$tipoCambioCrcOfertado<- as.numeric(Costos$tipoCambioCrcOfertado)
Costos$tipoCambioDolarOfertado<- as.numeric(Costos$tipoCambioDolarOfertado)
Costos$tipoCambioCrcAdjudicado<- as.numeric(Costos$tipoCambioCrcAdjudicado)
Costos$tipoCambioDolarAdjudicado<- as.numeric(Costos$tipoCambioDolarAdjudicado)


#Convirtiendo los códigos de producto de 24 a 16
Costos$codigoProductoAdjudicado_16<- substr(Costos$codigoProductoAdjudicado, 1, 16)
Costos$codigoProductoOfertado_16<- substr(Costos$codigoProductoOfertado, 1, 16)

# Cambia los montos a colones
Costos[, ":=" (Prec.Unit.Ofer.Col = ifelse(tipoMonedaOfertada == "CRC", yes = precioUnitarioOfertado, no = precioUnitarioOfertado * tipoCambioCrcOfertado * (1 / tipoCambioDolarOfertado)))]
Costos[, Prec.Unit.Adj.Col := ifelse(tipoMonedaAdjudicada == "CRC", precioUnitarioAdjudicado, precioUnitarioAdjudicado * tipoCambioCrcAdjudicado * (1 / tipoCambioDolarAdjudicado))]

###************************** Indicador 1**************************###
### Preparación de los datos Ind. 1-----           
# 1. Grado de desviación del monto unitario adjudicado de la línea con respecto al resto de las compras u ofertas del mismo bien

# Paso 1: Calcular el precio promedio del bien
Promedio_Costos <- Costos[, .(Prom.precioUnitarioAdjudicado = mean(Prec.Unit.Adj.Col, na.rm = TRUE),
                              Conteo_Adjudicado = length(Prec.Unit.Adj.Col)),
                          keyby = .(codigoProductoAdjudicado_16)]

# Obtener el recuento de valores únicos en la columna "codigoProductoAdjudicado_16" en el dataframe Promedio_Costos.

# Cambiar los precios adjudicados a colones o dólares, según el tipo de moneda adjudicada.
Costos[, Prec.Unit.Adj.Col := ifelse(tipoMonedaAdjudicada == "CRC",
                                     precioUnitarioAdjudicado,
                                     precioUnitarioAdjudicado * tipoCambioCrcAdjudicado * (1 / tipoCambioDolarAdjudicado))]

# Cambiar a dólares con el tipo de cambio
LineasOfertadas[, precio_unitario_ofertado_colones := ifelse(tipo_moneda == "CRC",
                                                             as.numeric(precio_unitario_ofertado),
                                                             as.numeric(precio_unitario_ofertado) * as.numeric(tipo_cambio_crc) * (1 / as.numeric(tipo_cambio_dolar)))]

# Se une lineas adjudicadas con procedimientos
Prel_0 <- merge(LineasOfertadas, Ofertas[, .(nro_sicop, nro_oferta, elegible)],
                by = c("nro_sicop", "nro_oferta"),
                all.x = TRUE) 

Prel_1 <- Prel_0[(elegible == "Sí"),] 
# Se filtran las filas donde la columna "elegible" es igual a "Sí", es decir, se mantienen solo las filas donde la oferta es elegible.

Prel_1 <- Prel_1[, codigo_producto_16 := substr(codigo_producto, 1, 16)] 
# Se crea una nueva columna "codigo_producto_16" que contiene los primeros 16 caracteres de la columna "codigo_producto".

Precios_ofertados <- Prel_1[, .(Promedio_ofertados = mean(precio_unitario_ofertado_colones, na.rm = TRUE),
                                Conteo_Oferta = length(precio_unitario_ofertado_colones)),
                            keyby = .(codigo_producto_16)] 

Precios_compilados <- merge(x = Promedio_Costos,
                            y = Precios_ofertados,
                            by.x = "codigoProductoAdjudicado_16",
                            by.y = "codigo_producto_16",
                            all.x = TRUE) 
# Se realiza una unión entre los dataframes Promedio_Costos y Precios_ofertados, basada en las columnas "codigoProductoAdjudicado_16" y "codigo_producto_16". Se agregan los promedios de precios adjudicados y ofertados para cada código de producto.

Costos.1 <- merge(x = Costos, y = Precios_compilados, by = "codigoProductoAdjudicado_16", all.x = TRUE) 
# Se realiza una unión entre los dataframes Costos y Precios_compilados, basada en la columna "codigoProductoAdjudicado_16". Se agrega la información de precios compilados a los datos de costos.


###************************** Indicador 1**************************###
### Indicador desv.adj.vs.oferta.mas.baja ------
# 4. Grado de desviación entre el monto de la linea adjudicada con respecto a la siguiente linea ofertada más baja
Costos.1[, Desv.Mas.Baja := ifelse(precioUnitarioMenorColones == 0,
                                   Prec.Unit.Adj.Col,
                                   Prec.Unit.Adj.Col / precioUnitarioMenorColones)]


Menor.Desv.Baja <- Costos.1[Desv.Mas.Baja < 1][]

###************************** Indicador 2**************************###
### Preparación de los datos Ind. 1-----           
# Grado de desviación del adjudicado vs el contratado.
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

# Se crea una columna "nombre_unico" concatenando ciertas columnas para identificar combinaciones únicas en Costos.2
Costos.2[, nombre_unico := do.call(paste, c(.SD, sep = "_")), .SDcols = c("numeroActo","numeroOferta","idLinea")]

# Se cuenta el número de veces que aparece cada combinación única en Costos.2 y se agrega la columna "Conteo_nombre"
Costos.2[, Conteo_nombre := length(anno), keyby = .(nombre_unico)]

# Se eliminan las filas con precios unitarios contratados NA
Costos.2 <- na.omit(Costos.2, cols = c("precio_unitario_contratado_colones"))

# Se vuelve a contar el número de veces que aparece cada combinación única después de eliminar los NA
Costos.2[, Conteo_nombre := length(anno), keyby = .(nombre_unico)]

# Se calcula el promedio del precio unitario contratado para cada combinación única
Promedio_contratado <- Costos.2[, .(Prom.contr = mean(precio_unitario_contratado_colones, na.rm = TRUE)), 
                                keyby = c("numeroActo", "numeroOferta", "idLinea")]

# Se realiza un merge entre Costos.2 y el promedio contratado utilizando ciertas columnas como claves
Costos.2 <- merge(x = Costos.2, y = Promedio_contratado, by = c("numeroActo", "numeroOferta", "idLinea"), all.x = TRUE)

# Se agrega una columna "Verif" que indica si el precio unitario contratado es igual al promedio contratado
Costos.2[, Verif := ifelse(Prom.contr == precio_unitario_contratado_colones, 0, 1)]

# Se realiza un merge entre Costos.1 y el promedio contratado, asignando el precio contratado a Costos.3
Costos.3 <- merge(x = Costos.1, y = Promedio_contratado, by = c("numeroActo", "numeroOferta", "idLinea"), all.x = TRUE)

### Indicador Desv.Monto.Unit.Cont --------
Costos.3[, Desv.Monto.Unit.Cont := (Prom.contr/Prec.Unit.Adj.Col)]

Indicadores_Costo <- Costos.3[,.(numeroActo,numeroOferta,
                                 idLinea, 
                                 Desv.Mas.Baja,
                                 Desv.Monto.Unit.Cont, 
                                 precioUnitarioMenorColones, 
                                 Prom.contr, 
                                 Prec.Unit.Adj.Col)]

names(Indicadores_Costo) <- c("numeroActo","numeroOferta","idLinea",
                              "desv.adj.vs.oferta.mas.baja",
                              "desv.cont.vs.adj", "Prec.Unit.Adj.Col", 
                              "precioUnitarioMenorColones", 
                              "Prom.contr")      

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

# Eliminando los valores de NA 
Set_Modelo_sin_NA <- Set_final[na.omit(Set_final)]
Set_Modelo_sin_NA <- Set_Modelo_sin_NA[,c(1:5, 8:9, 13:15)]

#### LIMPIEZA DE DATOS
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
# MODELOS ----
###************************** Componentes principales robustos**************************###
## Modelo RPCA ----
#Caso de analisis de divergencias entre 2018 y 2021
datos <- datos_Log[,-c(1:3)]

# Correr el modelo PCA 
pca <- prcomp(x = datos, center = TRUE,scale. = TRUE)

# Examinando los eigenvalues y la varianza acumulada para cada componente
#get_eig(X = pca)

# Reconstruccion de las observaciones iniciales

reconstruct_prcomp <- function(pca, comp = NULL){
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

rpca  <- PcaCov(x = datos, cov.control = CovControlMcd(), scale = TRUE, signflip = TRUE)

# Examinando los eigenvalues y la varianza acumulada para cada componente
rpca_eigen_var <- tibble(
  componente = seq_len(rpca$k),
  varianza   = rrcov::getSdev(rpca)^2,
  porcnt_varianza = 100 * varianza / sum(varianza),
  porcnt_varianza_acumulada = cumsum(porcnt_varianza)
)


# Reconstruccion de las observaciones iniciales

reconstruct_PcaCov <- function(pca, comp = NULL){
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

# Para determinar los percentiles y distribucion del error para establecer un corte en el cual 
Deciles_pca <-quantile(datos$error_reconstruccion_pca, probs = seq(0, 1, 0.05), na.rm = FALSE)
Deciles_rpca <-quantile(datos$error_reconstruccion_rpca, probs = seq(0, 1, 0.05), na.rm = FALSE)
setDT(datos)

datos[,Anomalia_PCA := ifelse(error_reconstruccion_pca >= Deciles_pca[20],1,0)]
datos[,Anomalia_RPCA := ifelse(error_reconstruccion_rpca >= Deciles_rpca[20],1,0)]

setnames(datos,old = c("error_reconstruccion_pca","error_reconstruccion_rpca"),new = c("Score_Pca","Score_Rpca"))

Resultados_RPCA <- data.frame(numeroActo= datos_Log$numeroActo,
                              numeroOferta= datos_Log$numeroOferta,
                              idLinea= datos_Log$idLinea,
                              score_RPCA= datos$Score_Rpca,  
                              anomalia_RPCA = datos$Anomalia_RPCA,
                              stringsAsFactors = FALSE)

###************************** Agregación de K Medias**************************###
## Modelo Agregación K Medias ------
# Se eliminan las columnas 1 a 3 del dataframe datos_Log
datos <- datos_Log[, -c(1:3)]

# Se calcula el score_KMEANS utilizando el algoritmo KNN_AGG
datos$score_KMEANS <- KNN_AGG(dataset = datos[, -c(1:3)], k_min = 10, k_max = 15)

# Se asignan nombres consecutivos a los valores del score_KMEANS
names(datos$score_KMEANS) <- 1:nrow(datos)

# Se ordenan los valores del score_KMEANS en orden descendente
sorted_indices <- sort(datos$score_KMEANS, decreasing = TRUE)

# Se calcula el logaritmo natural del score_KMEANS y se asigna a la columna log.score_KMEANS
datos$log.score_KMEANS <- log(datos$score_KMEANS, base = exp(1))

# Se calculan los percentiles del score_KMEANS para establecer un punto de corte
Deciles_KMEANS <- quantile(datos$score_KMEANS, probs = seq(0, 1, 0.05), na.rm = FALSE)

# Se asigna la etiqueta Anomalo_KMEANS a las observaciones que superan el percentil 20
datos[, Anomalo_KMEANS := ifelse(score_KMEANS >= Deciles_KMEANS[20], 1, 0)]

#Creando la base para guardar los elementos
Resultados_KMEANS <- data.frame(numeroActo= datos_Log$numeroActo,
                                numeroOferta= datos_Log$numeroOferta,
                                idLinea= datos_Log$idLinea,
                                score_KMEANS= datos$score_KMEANS,  
                                anomalia_KMEANS = datos$Anomalo_KMEANS,
                                stringsAsFactors = FALSE)

###************************** Bosques de Aislamiento **************************###
## Modelo ISO ------------
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

# Se crea un dataframe Resultados_ISO con los resultados del análisis SCI Forest
Resultados_ISO <- data.frame(numeroActo = datos_Log$numeroActo,
                             numeroOferta = datos_Log$numeroOferta,
                             idLinea = datos_Log$idLinea,
                             anomalia_iso = datos$outlier_sci_0.5,
                             score_iso = pred_sci,
                             stringsAsFactors = FALSE)


###************************** Valor Atípico Local **************************###
## Modelo LOF ----
# Se eliminan las columnas 1 a 3 del dataframe datos_Log
datos <- datos_Log[, -c(1:3)]

# Se calcula el LOF score con un vecindario de 10 puntos
datos$outlier_score <- LOF(dataset = datos[, -c(1:3)], k = 10)

# Se ordenan los puntajes de outliers en orden descendente
names(datos$outlier_score) <- 1:nrow(datos)
sort(datos$outlier_score, decreasing = TRUE)

# Se calculan los percentiles de los puntajes de outliers para establecer un punto de corte
Deciles_LOF <- quantile(datos$outlier_score, probs = seq(0, 1, 0.05), na.rm = TRUE)

# Se asigna la etiqueta Anomalia_LOF a las observaciones que superan el percentil 20
datos[, Anomalia_LOF := ifelse(datos$outlier_score >= Deciles_LOF[20], 1, 0)]

# Se reemplazan los valores NA por 0, asumiendo que son NA debido a una alta densidad de puntos
datos[is.na(datos)] <- 0

# Se cuenta el número de valores Anomalia_LOF y se calcula el porcentaje
Conteo_anomalias <- datos[, .N, keyby = .(Anomalia_LOF)][, Porcentaje := (N / sum(N)) * 100][]

# Se crea un dataframe Resultados_LOF con los resultados del análisis LOF
Resultados_LOF <- data.frame(numeroActo = datos_Log$numeroActo,
                             numeroOferta = datos_Log$numeroOferta,
                             idLinea = datos_Log$idLinea,
                             anomalia_LOF = datos$Anomalia_LOF,
                             score_LOF = datos$outlier_score,
                             stringsAsFactors = FALSE)


###************************** LIMPIEZA DE LO PREVIO REALIZADO**************************###
# Lista de archivos a conservar
objetos_a_conservar <- c("Set_Modelo_sin_NA", 
                         "Indicadores_Plazo", "Indicadores_Alcance", "Indicadores_Costo",
                         "Resultados_ISO","Resultados_KMEANS","Resultados_RPCA","Resultados_LOF",
                         "LineasOfertadas", "Ofertas", "Costos", "SICOP", "tablas")
objetos_totales <- ls()

# Obtener la lista de objetos a eliminar
objetos_a_eliminar <- setdiff(objetos_totales, objetos_a_conservar)
rm(list = objetos_a_eliminar)

# OTRA INFORMACIÓN -----------

# Carga los datos de la tabla "dbo.Instituciones" desde la base de datos SICOP y los almacena en el objeto "Instituciones"
Instituciones <- data.table(sqlFetch(SICOP, "dbo.Instituciones", as.is = TRUE))
lineasadjudicadas <- data.table(sqlFetch(SICOP, "dbo.lineasadjudicadas", as.is = TRUE))
procedimientos <- data.table(sqlFetch(SICOP, "dbo.procedimientos", as.is = TRUE))
jerarquiaClasificacionBS <- data.table(sqlFetch(SICOP, "dbo.jerarquiaClasificacionBS", as.is = TRUE))

# Fusiona los data frames lineasadjudicadas y una selección de columnas del data frame procedimientos basado en la columna "nro_sicop"
Prel_0 <- merge(lineasadjudicadas, procedimientos[,.(nro_sicop, nro_procedimiento, cedula_institucion)], by = "nro_sicop", all.x = TRUE)
Prel_1 <- merge(Prel_0, Instituciones[,.(cedula, nombre_institucion)], by.x = "cedula_institucion", by.y = "cedula", all.x = TRUE)
Prel_1[, anno:=substr(nro_procedimiento,1,4)]
Prel_1[, codigo_producto_8:=substr(codigo_producto,1,8)]
Prel_2 <- merge(Prel_1, jerarquiaClasificacionBS[,.(codigoClasificacion, nombreClasificacion, nombreFamilia)], by.x = "codigo_producto_8", by.y = "codigoClasificacion", all.x = TRUE)

Identificador_Instituciones <- Prel_2[, .(cedula_institucion, nro_acto, nro_oferta, nro_linea, anno, nombreClasificacion, nombreFamilia, nombre_institucion)]

# Filtra Identificador_Instituciones para incluir solo las filas donde el valor de "anno" es menor a 2023 y mayor a 2018.
Identificador_Instituciones <- Identificador_Instituciones[(anno < 2023 & anno > 2018),]

# Renombra las columnas de Identificador_Instituciones con nuevos nombres.
colnames(Identificador_Instituciones) <- c("idInstitucion","numeroActo","numeroOferta","idLinea", "anno", "nombreClasificacion", "nombreFamilia","Nombre_Institucion")

#### LIMPIEZA DE DATOS
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
## Consolidación ----
# Creamos una lista que contiene los data frames de anomalías que queremos consolidar
lista <- list(Resultados_ISO, Resultados_KMEANS, Resultados_LOF, Resultados_RPCA)

# Combinamos los data frames en uno solo utilizando la función Reduce y merge
Set_final <- Reduce(function(...) merge(..., by = c("numeroActo", "numeroOferta", "idLinea"), all = TRUE), lista)

# Unimos el data frame consolidado con el data frame "Identificador_Instituciones" utilizando las columnas "numeroActo", "numeroOferta" e "idLinea"
Set_final <- merge(Set_final, Identificador_Instituciones, by = c("numeroActo", "numeroOferta", "idLinea"), all.x = TRUE)
setDT(Set_final)

# Creamos una nueva columna llamada "Consenso" que suma las columnas de anomalías
Set_final[, Consenso := anomalia_KMEANS + anomalia_iso + anomalia_LOF + anomalia_RPCA]

# Calcula el recuento de filas por combinación de "idInstitucion", "Nombre_Institucion" y "Consenso" en el data frame Set_final.
Dist_consenso_NA <- Set_final[,.N, keyby = .(idInstitucion,Nombre_Institucion, Consenso)][order(-Consenso)][]

# Transpone el data frame Dist_consenso_NA, creando nuevas columnas para cada valor único de "Consenso".
Dist_consenso_transpuesto_NA <- dcast(data = Dist_consenso_NA, formula = idInstitucion + Nombre_Institucion ~ Consenso, value.var = "N")

# Calcula una nueva columna llamada "Uno_o_mas" en Dist_consenso_transpuesto_NA que contiene la suma de las columnas 4 a 7, omitiendo los valores NA.
Dist_consenso_transpuesto_NA[,Uno_o_mas := rowSums(Dist_consenso_transpuesto_NA[,c(4:7), with = FALSE], na.rm = TRUE)]

# Calcula una nueva columna llamada "Uno_o_mas_Porcent" en Dist_consenso_transpuesto_NA que representa el porcentaje de "Uno_o_mas" en relación a la suma de las columnas 3 a 7, omitiendo los valores NA.
Dist_consenso_transpuesto_NA[,Uno_o_mas_Porcent := (Uno_o_mas/rowSums(Dist_consenso_transpuesto_NA[,c(3:7), with = FALSE], na.rm = TRUE)) * 100]

# Resumen de la cantidad de registros únicos por las variables "numeroActo" y "numeroOferta" donde "Consenso" es igual a cero para cada institución.
Unicos <- unique(Set_final[Consenso == 0], by = c("numeroActo", "numeroOferta", "idInstitucion", "Nombre_Institucion"))
Freq.Inst.unicos_0 <- Unicos[,.N, keyby = c("idInstitucion", "Nombre_Institucion")]

# Generación de muestras aleatorias para cada institución.

## Jerarquizandoción de líneas ------------------
# Filtra los registros en Set_final donde "anomalia_iso" es igual a 1 y los asigna a la variable puntaje_anomalia_ISO.
puntaje_anomalia_ISO <- Set_final[anomalia_iso == 1,]

# Calcula los deciles del campo "score_iso" en puntaje_anomalia_ISO.
Deciles_ISO <- quantile(puntaje_anomalia_ISO$score_iso, probs = seq(0, 1, 0.1), na.rm = FALSE)

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


#CASO DEL KNN
puntaje_anomalia_KMEANS<-Set_final[anomalia_KMEANS ==1,]
Deciles_KMEANS <-quantile(puntaje_anomalia_KMEANS$score_KMEANS, probs = seq(0, 1, 0.1), na.rm = FALSE)

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


#CASO DEL LOF
puntaje_anomalia_LOF<-Set_final[anomalia_LOF==1,]
Deciles_LOF <-quantile(puntaje_anomalia_LOF$score_LOF, probs = seq(0, 1, 0.1), na.rm = FALSE)

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


#CASO DEL RPCA
puntaje_anomalia_RPCA<-Set_final[anomalia_RPCA==1,]
Deciles_RPCA <-quantile(puntaje_anomalia_RPCA$score_RPCA, probs = seq(0, 1, 0.1), na.rm = FALSE)

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


Set_final[, Nota_final := Escala_Nota_ISO*0.25+Escala_Nota_KMEANS*0.25+Escala_Nota_LOF*0.25+Escala_Nota_RPCA*0.25]


# Asignar el precio unitario contratado en colones basado en la moneda contratada.
costos <- data.table(sqlFetch(SICOP, "ds.costosCont",as.is=TRUE))

costos[, precio_unitario_contratado_colones := ifelse(tipoMonedaContratada == "CRC", as.numeric(precioUnitarioContratado), as.numeric(precioUnitarioContratado) * as.numeric(tipoCambioCrcContratado) * (1/as.numeric(tipoCambioDolarContratado)))]
costos[, Monto_linea_colones := as.numeric(cantidadContratada) * precio_unitario_contratado_colones - as.numeric(descuentoContratado) + as.numeric(ivaContratado) + as.numeric(otrosImpuestosContratado) + as.numeric(acarreosContratado)]

# Combinar los datos de Set_final y costos en base a las columnas "numeroActo", "idLinea" y "numeroOferta".
Set_final <- merge(Set_final, costos[, .(numeroActo, numeroOferta, idLinea, numeroProcedimiento, modalidadProcedimiento, Monto_linea_colones)], by = c("numeroActo", "idLinea", "numeroOferta"), all.x = TRUE)

#### LIMPIEZA DE DATOS
# Lista de archivos a conservar
objetos_a_conservar <- c("Set_Modelo_sin_NA", "Set_final",
                         "Indicadores_Plazo", "Indicadores_Alcance", "Indicadores_Costo", 
                         "Identificador_Instituciones",
                         "Resultados_ISO", "Resultados_KMEANS", "Resultados_RPCA", "Resultados_LOF",
                         "LineasOfertadas", "Ofertas", "Costos", "SICOP", "tablas")
objetos_totales <- ls()

# Obtener la lista de objetos a eliminar
objetos_a_eliminar <- setdiff(objetos_totales, objetos_a_conservar)
rm(list = objetos_a_eliminar)

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
                              codigo_identificacion, nombre_identificacion, nombreClasificacion, nombreFamilia, Nombre_Institucion, idInstitucion,
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
Set_final_V5<-Set_final_V4[,-c(21:25)]
colnames(Set_final_V5) <- c("numeroActo", "idLinea", "numeroOferta", "numeroProcedimiento", 
                            "codigo_identificacion", "nombre_identificacion", "nombreClasificacion", 
                            "nombreFamilia", "Nombre_Institucion", "idInstitucion", 
                            "cedula_proveedor", "nombre_proveedor", 
                            "tipo_proveedor", "tamano_proveedor", "anno", "modalidadProcedimiento", 
                            "Monto_linea_colones", "Consenso", "Compras.unico", 
                            "Raz.solicitada.contratada", "Compras.unico.Acot", "Raz.solicitada.contratada.Acot", 
                            "desv.adj.vs.oferta.mas.baja.Acot", "desv.cont.vs.adj.Acot", 
                            "Desv.inicioadju.Acot", "Desv.contpriadj.Acot", "CantModf_Plazo.Acot", 
                            "desv.adj.vs.oferta.mas.baja", "desv.cont.vs.adj", "Prec.Unit.Adj.Col",
                            "precioUnitarioMenorColones", "Prom.contr", "Desv.inicioadju", 
                            "Desv.contpriadj", "CantModf_Plazo", "difIniAdj", "Prom.inicioadju_16", 
                            "difAdjContPri", "Prom.contpriradj_16")

# Escribir el contenido de Set_final_V4 en un archivo CSV llamado "Resultados_Finales_Prueba2.csv".
fwrite(Set_final_V5, "Resultados_Finales_Prueba2.csv", sep = ";", dec = ".")


