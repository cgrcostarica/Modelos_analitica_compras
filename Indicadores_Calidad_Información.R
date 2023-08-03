# Librerias -----------------------------------------------
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
options(scipen = 999)



SICOP <-odbcConnect("ODBC_CGRSQL")
#SICOP <-odbcConnect("Conexion_SICOP")
tablas <- sqlTables(SICOP, tableType = "TABLE")

# Cargando tablas -----
Instituciones <- data.table(sqlFetch(SICOP, "dbo.Instituciones",as.is=TRUE))
lineasadjudicadas <- data.table(sqlFetch(SICOP, "dbo.lineasadjudicadas",as.is=TRUE))
procedimientos <- data.table(sqlFetch(SICOP, "dbo.procedimientos",as.is=TRUE))
adjudicaciones <- data.table(sqlFetch(SICOP, "dbo.Adjudicaciones",as.is=TRUE))
proveedores <- data.table(sqlFetch(SICOP, "dbo.proveedores",as.is=TRUE))
Ofertas <- data.table(sqlFetch(SICOP, "dbo.Ofertas",as.is=TRUE))
identificacionbs <- data.table(sqlFetch(SICOP, "dbo.identificacionbs",as.is=TRUE))
LineasProc <- data.table(sqlFetch(SICOP, "dbo.LineasProc",as.is=TRUE))
LineasContratadas <- data.table(sqlFetch(SICOP, "dbo.LineasContratadas",as.is=TRUE))
lineasofertadas <- data.table(sqlFetch(SICOP, "dbo.lineasofertadas",as.is=TRUE))
Contratos <- data.table(sqlFetch(SICOP, "dbo.Contratos",as.is=TRUE))
modulos <- data.table(sqlFetch(SICOP, "ds.modulos",as.is=TRUE))
Alcance <- data.table(sqlFetch(SICOP, "ds.alcance",as.is=TRUE))
objeciones <- data.table(sqlFetch(SICOP, "dbo.objeciones",as.is=TRUE))

# Creación de set ----
Base_General_1<- LineasProc[,.(nro_sicop, numero_linea)]
Base_General_2<- merge(Base_General_1,procedimientos[,.(nro_sicop,cedula_institucion,fecha_publicacion,nro_procedimiento,tipo_procedimiento, modalidad_procedimiento)],by = "nro_sicop",all.x = TRUE)
Base_General_3<- merge(Base_General_2,lineasadjudicadas[,.(nro_sicop,nro_acto,nro_oferta,nro_linea, precio_unitario_adjudicado, tipo_moneda, tipo_cambio_crc, tipo_cambio_dolar)], by.x = c("nro_sicop","numero_linea"),by.y = c("nro_sicop","nro_linea"),all.x = TRUE)
setnames(Base_General_3, c("tipo_moneda", "tipo_cambio_crc", "tipo_cambio_dolar"), c("tipo_moneda_adj", "tipo_cambio_crc_adj", "tipo_cambio_dolar_adj"))
Base_General_4<- merge(Base_General_3,adjudicaciones[,.(nro_sicop,nro_acto,fecha_adj_firme,desierto)], by = c("nro_sicop","nro_acto"),all.x = TRUE)
Base_General_4[, precio_unitario_adjudicado_colones := ifelse(tipo_moneda_adj == "CRC", as.numeric(precio_unitario_adjudicado),as.numeric(precio_unitario_adjudicado)*as.numeric(tipo_cambio_crc_adj)*(1/as.numeric(tipo_cambio_dolar_adj)))]
Base_General_5<- merge(Base_General_4,Ofertas[,.(nro_sicop,nro_oferta,fecha_presenta_oferta,cedula_proveedor,elegible)], by = c("nro_sicop","nro_oferta"),all.x = TRUE)
Base_General_6<- merge(Base_General_5,LineasContratadas[,.(nro_sicop,nro_acto,nro_contrato,nro_linea_cartel, secuencia, precio_unitario_contratado, tipo_moneda, tipo_cambio_crc, tipo_cambio_dolar)], by.x = c("nro_sicop","nro_acto","numero_linea"), by.y = c("nro_sicop","nro_acto","nro_linea_cartel"),all.x = TRUE)
setnames(Base_General_6, c("tipo_moneda", "tipo_cambio_crc", "tipo_cambio_dolar"), c("tipo_moneda_cont", "tipo_cambio_crc_cont", "tipo_cambio_dolar_cont"))
Base_General_7<- merge(Base_General_6, Contratos[,.(nro_contrato, secuencia, fecha_notificacion)], by =c("nro_contrato", "secuencia"),all.y = TRUE)
Base_General_7[, precio_unitario_contratado_colones := ifelse(tipo_moneda_cont == "CRC", as.numeric(precio_unitario_contratado),as.numeric(precio_unitario_contratado)*as.numeric(tipo_cambio_crc_cont)*(1/as.numeric(tipo_cambio_dolar_cont)))]
Base_General_8<- merge(Base_General_7,proveedores[,.(cedula_proveedor,fecha_constitucion, fecha_expiracion)], by=c("cedula_proveedor"), all.x = TRUE)
Base_General_9<- merge(Base_General_8,LineasProc[,.(nro_sicop,numero_linea, precio_unitario_estimado, tipo_moneda, tipo_cambio_crc, tipo_cambio_dolar)], by=c("nro_sicop","numero_linea"), all.x = TRUE)
setnames(Base_General_9, c("tipo_moneda", "tipo_cambio_crc", "tipo_cambio_dolar"), c("tipo_moneda_est", "tipo_cambio_crc_est", "tipo_cambio_dolar_est"))
Base_General_9[, precio_unitario_estimado_colones := ifelse(tipo_moneda_est == "CRC", as.numeric(precio_unitario_estimado),as.numeric(precio_unitario_estimado)*as.numeric(tipo_cambio_crc_est)*(1/as.numeric(tipo_cambio_dolar_est)))]
Base_General_10<- merge(Base_General_9,lineasofertadas[,.(nro_sicop,nro_oferta, nro_linea, precio_unitario_ofertado, tipo_moneda, tipo_cambio_crc, tipo_cambio_dolar)], by.x=c("nro_sicop","numero_linea", "nro_oferta"), by.y=c("nro_sicop","nro_linea","nro_oferta"), all.x = TRUE)
setnames(Base_General_10, c("tipo_moneda", "tipo_cambio_crc", "tipo_cambio_dolar"), c("tipo_moneda_ofer", "tipo_cambio_crc_ofer", "tipo_cambio_dolar_ofer"))
Base_General_10[, precio_unitario_ofertado_colones := ifelse(tipo_moneda_ofer == "CRC", as.numeric(precio_unitario_ofertado),as.numeric(precio_unitario_ofertado)*as.numeric(tipo_cambio_crc_ofer)*(1/as.numeric(tipo_cambio_dolar_ofer)))]
Base_General_10_solos_elegibles<-Base_General_10[elegible=="Sí",]
Base_General_10_solos_elegibles<-setDT(Base_General_10_solos_elegibles)
Promedio_Precio_Ofertas <- Base_General_10_solos_elegibles[, .(Prom_precio_Unitario_Ofertado = mean(precio_unitario_ofertado_colones, na.rm = TRUE), Conteo_Ofertas = length(precio_unitario_ofertado_colones)), keyby = .(nro_procedimiento, numero_linea)]
Base_General_11<- merge(Base_General_10,Promedio_Precio_Ofertas, by=c("nro_procedimiento","numero_linea"), all.x = TRUE)
Base_General_12<- merge(Base_General_11, Alcance[,.(numeroActo, numeroOferta, idLinea, idProcedimiento, cantModif, codigoProductoAdjudicado)], by.x=c("nro_acto", "nro_oferta","numero_linea"),by.y=c("numeroActo", "numeroOferta", "idLinea"), all.x = TRUE)
Base_General_13<- merge(Base_General_12, modulos, by.x=c("idProcedimiento", "numero_linea"), by.y=c("idProcedimiento", "idLinea"), all.x = TRUE)
Conteo_objeciones <- objeciones[, .(Total_objeciones = length(nro_recurso)), keyby = .(nro_sicop, linea_objetada)]
Base_General_14<- merge(Base_General_13, Conteo_objeciones, by.x=c("nro_sicop", "numero_linea"), by.y=c("nro_sicop", "linea_objetada"), all.x = TRUE)

#Establecimiento de las fechas con el formato fechas
Base_General_14$fecha_publicacion<-as.Date(Base_General_14$fecha_publicacion)
Base_General_14$fecha_presenta_oferta<-as.Date(Base_General_14$fecha_presenta_oferta)
Base_General_14$fecha_adj_firme<-as.Date(Base_General_14$fecha_adj_firme)
Base_General_14$fecha_contrato<-as.Date(Base_General_14$fecha_notificacion)
Base_General_14$fecha_constitucion<-as.Date(Base_General_14$fecha_constitucion)
Base_General_14$fecha_expiracion<-as.Date(Base_General_14$fecha_expiracion)

###************************** CONSISTENCIA **************************###
###************************** Indicadores fechas **************************###
## Indicadores fechas ----
### Ind. Fecha recepción menor a publicacion ----
Base_General_14[, Dif_publi_present_ofer:= (fecha_presenta_oferta-fecha_publicacion)]
Base_General_14[,.N, keyby = Dif_publi_present_ofer][order(-N)][,Porcentaje := (N/sum(N))*140][]
Sin_CD <- Base_General_14[Dif_publi_present_ofer<1 & tipo_procedimiento!="Contratación Directa", ][]
unique(Sin_CD$tipo_procedimiento)
Sin_CD[tipo_procedimiento=="Licitación Publica Nacional",]
unique(procedimientos$tipo_procedimiento)

### Ind. Fecha adjudicación menor a publicación ----
Base_General_14[, Dif_publi_adj_firme:= (fecha_adj_firme-fecha_publicacion)]
Base_General_14[,.N, keyby = Dif_publi_adj_firme][order(-N)][,Porcentaje := (N/sum(N))*140][]
Base_General_14[Dif_publi_adj_firme<1,][]

### Ind. Fecha adjudicación menor a oferta ----
Base_General_14[, Dif_present_ofer_adj_firme:= (fecha_adj_firme-fecha_presenta_oferta)]
Base_General_14[,.N, keyby = Dif_present_ofer_adj_firme][order(-N)][,Porcentaje := (N/sum(N))*140][]
Base_General_14[Dif_present_ofer_adj_firme<1,][]

### Ind. Fecha presentacion oferta mayor a fecha constitucion ----
Base_General_14[, Dif_oferta_mayor_constitucion:= (fecha_presenta_oferta-fecha_constitucion)]
Base_General_14[,.N, keyby = Dif_oferta_mayor_constitucion][order(-N)][,Porcentaje := (N/sum(N))*140][]
Base_General_14[Dif_oferta_mayor_constitucion<14,][]
#Base_General_14[cedula_proveedor==3101792759,]

### Ind. Fecha adjudicación menor a oferta ----
Base_General_14[, Dif_adju_mayor_expiracion:= (fecha_expiracion-fecha_adj_firme)]
Base_General_14[,.N, keyby = Dif_adju_mayor_expiracion][order(-N)][,Porcentaje := (N/sum(N))*140][]
Base_General_14[Dif_adju_mayor_expiracion<0,][]

###************************** PRECISIÓN **************************###
###************************** Indicadores precios **************************###
### Ind. Precio estimado menor a 2 colones ----
Base_General_14[, Estimado_mala_calidad := ifelse(precio_unitario_estimado_colones<2,1,0)]
Base_General_14[,.N, keyby = Estimado_mala_calidad][order(-N)][,Porcentaje := (N/sum(N))*140][]

### Ind. Diferencia precio adjudicado y contratado, con rango de 300% ----
Base_General_14[, Dif_Pre_Est_vs_Pre_Adju:= (precio_unitario_estimado_colones/precio_unitario_adjudicado_colones)]
Base_General_14[, Rango_Dif_Pre_Est_vs_Pre_Adju := ifelse(Dif_Pre_Est_vs_Pre_Adju <= -3 | Dif_Pre_Est_vs_Pre_Adju >= 3, 1, 0)]
Base_General_14[,.N, keyby = Rango_Dif_Pre_Est_vs_Pre_Adju][order(-N)][,Porcentaje := (N/sum(N))*140][]

### Ind. Diferencia precio adjudicado y contratado, con rango de 300% ----
Base_General_14[, Dif_Pre_Adj_vs_Pre_Contra:= (precio_unitario_adjudicado_colones/precio_unitario_contratado_colones)]
Base_General_14[, Rango_Dif_Pre_Adj_vs_Pre_Contra := ifelse(Dif_Pre_Adj_vs_Pre_Contra <= -3 | Dif_Pre_Adj_vs_Pre_Contra >= 3, 1, 0)]
Base_General_14[,.N, keyby = Rango_Dif_Pre_Adj_vs_Pre_Contra][order(-N)][,Porcentaje := (N/sum(N))*140][]

### Ind. Diferencia precio ofertado promedio y precio adjudicado ---- 
Base_General_14[, Dif_Prom_Ofer_vs_Pre_Adju:= (precio_unitario_adjudicado_colones/Prom_precio_Unitario_Ofertado)]
Base_General_14[, Rango_Dif_Prom_Ofer_vs_Pre_Adju := ifelse(Dif_Prom_Ofer_vs_Pre_Adju <= -3 | Dif_Prom_Ofer_vs_Pre_Adju >= 3, 1, 0)]
Base_General_14[,.N, keyby = Rango_Dif_Prom_Ofer_vs_Pre_Adju][order(-N)][,Porcentaje := (N/sum(N))*100][]

###************************** COMPLETITUD **************************###
###************************** Indicadores Completitud **************************###
## Completitud ----
### Ind. Completitud de expediente -----
Base_General_14[, Hitos_Ausentes := rowSums(is.na(.SD)), .SDcols = c("primerFechaAdjudicacion", "primerFechaContratacion", "primerFechaRecepcion")]
Base_General_14[, Completitud := ifelse(is.infinite(3/(3 - Hitos_Ausentes)), 0, 3/(3 - Hitos_Ausentes))]
Base_General_14[,.N, keyby = Hitos_Ausentes][order(-N)][,Porcentaje := (N/sum(N))*100][]

#names(Base_General_14)
#Completitud<- Base_General_14[!is.na(primerFechaAdjudicacion),]
#Resumen_completitud<-Completitud[,.N, keyby = .(Estimado_mala_calidad, cedula_institucion)][order(-N)][,Porcentaje := (N/sum(N))*100][]
#Resumen_completitud
#Resumen_completitud_ancha <- dcast(Resumen_completitud, cedula_institucion~ Estimado_mala_calidad, value.var = "Porcentaje")

#Completitud[precio_unitario_estimado_colones<1,]

###************************** Indicadores Cantidad de dígitos **************************###
## Largo digitos precio unitario adjudicado ----
Base_General_14[, Largo_Digito_Pre_Adju:=nchar(round(precio_unitario_adjudicado_colones,0))]
Base_General_14[, Largo_Digito_Pre_Ofer:=nchar(round(precio_unitario_ofertado_colones,0))]
Base_General_14[, Largo_Digito_Pre_Contr:=nchar(round(precio_unitario_contratado_colones,0))]
Base_General_14[, Dif_Lar_Cont_vs_Adju:= ifelse(Largo_Digito_Pre_Contr-Largo_Digito_Pre_Adju==0,0,1)]
Base_General_14[, anno:=substr(nro_procedimiento,1,4)]

Base_General_14[,.N, keyby = .(Dif_Lar_Cont_vs_Adju,anno)][order(-N)][,Porcentaje := (N/sum(N))*100][]
Base_General_14[Dif_Lar_Cont_vs_Adju==1,]

Cantidad_Secuencia <- Base_General_14[, .(Cantidad_Secuencia = length(secuencia)), keyby = .(nro_acto, nro_oferta, numero_linea)]

Base_General_15<- merge(Base_General_14, Cantidad_Secuencia, by=c("nro_acto", "nro_oferta", "numero_linea"), all.x = TRUE)

Base_General_15[Rango_Dif_Pre_Adj_vs_Pre_Contra==1 & Cantidad_Secuencia==1,]
Base_General_15[Rango_Dif_Prom_Ofer_vs_Pre_Adju==1 & Cantidad_Secuencia==1,]


Base_General_15[nro_procedimiento=="2021CE-000003-0000300001",]
contratado_dolares<-LineasContratadas[tipo_moneda=="USD",]
contratado_dolares[as.numeric(precio_unitario_contratado)>1000000,]
hist(as.numeric(contratado_dolares$precio_unitario_contratado))


names(Base_General_15)
Base_General_15[,Calidad_final:=(Estimado_mala_calidad
                                 +Rango_Dif_Pre_Adj_vs_Pre_Contra
                                 +Rango_Dif_Prom_Ofer_vs_Pre_Adju)]

names(Base_General_14$Total_objeciones)
table(Base_General_14$desierto)
names(Base_General_15)


#Selección final de indicadores ---------
Final_Calidad<-Base_General_15[,c("nro_acto", "numero_linea", "nro_oferta","nro_procedimiento", 
                                "cedula_institucion", "cedula_proveedor", "codigoProductoAdjudicado", 
                                "Total_objeciones", 
                                "Dif_publi_present_ofer", "fecha_presenta_oferta", "fecha_publicacion",
                                "Dif_oferta_mayor_constitucion", "fecha_constitucion",
                                "Dif_adju_mayor_expiracion", "fecha_expiracion", "fecha_adj_firme",
                                "Estimado_mala_calidad", "precio_unitario_estimado_colones",
                                "Rango_Dif_Prom_Ofer_vs_Pre_Adju", "precio_unitario_adjudicado_colones", "Prom_precio_Unitario_Ofertado",
                                "Completitud", "primerFechaAdjudicacion", "primerFechaContratacion", "primerFechaRecepcion",
                                "Dif_Lar_Cont_vs_Adju", "Largo_Digito_Pre_Contr", "Largo_Digito_Pre_Adju")]

colnames(Final_Calidad) <- c("numeroActo", "idLinea", "numeroOferta", "numeroProcedimiento", 
                            "idInstitucion", 
                            "cedula_proveedor", "codigo_identificacion", 
                            "Total_objeciones",
                            "Dif_publi_present_ofer", "fecha_presenta_oferta", "fecha_publicacion",
                            "Dif_oferta_mayor_constitucion", "fecha_constitucion",
                            "Dif_adju_mayor_expiracion", "fecha_expiracion", "fecha_adj_firme",
                            "Estimado_mala_calidad", "precio_unitario_estimado_colones",
                            "Rango_Dif_Prom_Ofer_vs_Pre_Adju", "precio_unitario_adjudicado_colones", "Prom_precio_Unitario_Ofertado",
                            "Completitud", "primerFechaAdjudicacion", "primerFechaContratacion", "primerFechaRecepcion",
                            "Dif_Lar_Cont_vs_Adju", "Largo_Digito_Pre_Contr", "Largo_Digito_Pre_Adju")

setDT(Final_Calidad)
Final_Calidad[,Anno:=substr(numeroProcedimiento,1,4)]
Final_Calidad[,Cod_16:=substr(codigo_identificacion,1,16)]
Final_Calidad[,Tipo_bien:=substr(codigo_identificacion,1,1)]


Final_Calidad_2<- Final_Calidad[(Anno < 2023 & Anno > 2019),]
Final_Calidad_3<- Final_Calidad_2[Final_Calidad_2$Tipo_bien%in%c("1","2","3","4","5","6"), ]

setwd("C:/Users/humberto.perera/Desktop")
fwrite(Final_Calidad_3, "Calidad.csv", sep = ";", dec = ".")
print("csv exportado")

#Carga de los datos en el servidor este paso no es necesario para usuarios externos
# Instalar y cargar el paquete RMySQL
#install.packages("RMySQL", repos = "http://cran.us.r-project.org")
library(DBI) 
library(RMySQL)

#install.packages("dotenv", repos = "http://cran.us.r-project.org")
library(dotenv)

# Cargar el archivo .env

dotenv::load_dot_env("C:/Users/humberto.perera/Desktop/credenciales.env")


host <- Sys.getenv("HOST")    # Direccion del servidor MySQL
usuario <- Sys.getenv("USUARIO") # Usuario de MySQL
contrasena <- Sys.getenv("PASS") # Contraseña del usuario
base_de_datos <- Sys.getenv("BD") # Nombre de la base de datos
puerto_str <- Sys.getenv("PUERTO") # Puerto especifico de MySQL
puerto <- as.integer(puerto_str)

# Establecer la conexion a la base de datos MySQL con el puerto especifico
con <- dbConnect(MySQL(), host = host, user = usuario, password = contrasena, dbname = base_de_datos, port = puerto)

# Escribir el contenido de Set_final_V5 en una tabla de la base de datos MySQL (reemplaza "nombre_de_la_tabla" con el nombre deseado para la tabla)
nombre_de_tabla <- "calidad"
dbWriteTable(con, name = nombre_de_tabla, value = Final_Calidad_3 , row.names = FALSE, overwrite=TRUE)

# Cerrar la conexion a la base de datos MySQL
dbDisconnect(con)

















