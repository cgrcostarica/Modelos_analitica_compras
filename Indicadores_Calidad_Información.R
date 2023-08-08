#*********************************CODIGO DE INDICADORES DE CALIDAD INFORMACION COMPRAS PUBLICAS*******************************#
#**************************************************ELABORADO POR LA CGR*****************************************#

#**************************************INICIO**************************# 

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

bases <- list("Instituciones","lineasadjudicadas","procedimientos","adjudicaciones","proveedores", "Ofertas",
              "identificacionbs", "LineasProc", "LineasContratadas","Contratos", "lineasofertadas", "modulos", "Alcance", "objeciones")

for (base in bases){
  base <- gsub("á", "a", base)
  base <- gsub("é", "e", base)
  base <- gsub("í", "i", base)
  base <- gsub("ó", "o", base)
  base <- gsub("ú", "u", base)
  base <- gsub("Á", "A", base)
  base <- gsub("É", "E", base)
  base <- gsub("Í", "I", base)
  base <- gsub("Ó", "O", base)
  base <- gsub("Ú", "U", base)
}


# Creación de set ----
Base_General_1<- LineasProc[,.(nro_sicop, numero_linea, codigo_identificacion)]
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
Base_General_14<- merge(Base_General_14, Instituciones, by.x=c("cedula_institucion"), by.y=c("cedula"), all.x = TRUE)
Base_General_14<- merge(Base_General_14, identificacionbs[,.(codigo_identificacion, nombre_identificacion)], by=c("codigo_identificacion"), all.x = TRUE)

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
Base_General_15<- merge(Base_General_15, proveedores[,.(cedula_proveedor, tipo_proveedor)], by=c("cedula_proveedor"), all.x = TRUE)

Base_General_15[Rango_Dif_Pre_Adj_vs_Pre_Contra==1 & Cantidad_Secuencia==1,]
Base_General_15[Rango_Dif_Prom_Ofer_vs_Pre_Adju==1 & Cantidad_Secuencia==1,]



Base_General_15[,Calidad_final:=(Estimado_mala_calidad
                                 +Rango_Dif_Pre_Adj_vs_Pre_Contra
                                 +Rango_Dif_Prom_Ofer_vs_Pre_Adju)]



#Selección final de indicadores ---------
Final_Calidad<-Base_General_15[,c("nro_acto", "numero_linea", "nro_oferta","nro_procedimiento", 
                                  "modalidad_procedimiento",
                                  "cedula_institucion", "nombre_institucion",  
                                  "cedula_proveedor",  "tipo_proveedor", 
                                  "codigoProductoAdjudicado", "nombre_identificacion",
                                  "Total_objeciones", 
                                  "Dif_publi_present_ofer", "fecha_presenta_oferta", "fecha_publicacion",
                                  "Dif_oferta_mayor_constitucion", "fecha_constitucion",
                                  "Dif_adju_mayor_expiracion", "fecha_expiracion", "fecha_adj_firme",
                                  "Estimado_mala_calidad", "precio_unitario_estimado_colones",
                                  "Rango_Dif_Prom_Ofer_vs_Pre_Adju", "precio_unitario_adjudicado_colones", "Prom_precio_Unitario_Ofertado",
                                  "Completitud", "primerFechaAdjudicacion", "primerFechaContratacion", "primerFechaRecepcion",
                                  "Dif_Lar_Cont_vs_Adju", "Largo_Digito_Pre_Contr", "Largo_Digito_Pre_Adju")]

#Reemplazando los valores de Total de objeciones de NA a cero, 
Final_Calidad$Total_objeciones <- replace(Final_Calidad$Total_objeciones, is.na(Final_Calidad$Total_objeciones), 0)


# Creando valores en deciles para la variable Dif_adju_mayor_expiracion
min_val <- min(Final_Calidad$Dif_adju_mayor_expiracion, na.rm = TRUE)
max_val <- max(Final_Calidad$Dif_adju_mayor_expiracion, na.rm = TRUE)
media_val <- median(Final_Calidad$Dif_adju_mayor_expiracion, na.rm = TRUE)

# Generar los deciles usando la función cut()
Final_Calidad$Dif_adju_mayor_expiracion<-as.numeric(Final_Calidad$Dif_adju_mayor_expiracion)

Final_Calidad[, Deciles_Dif_adju_mayor_expiracion := cut(Dif_adju_mayor_expiracion, 
                                                         breaks = quantile(Dif_adju_mayor_expiracion, probs = seq(0, 1, 0.1), na.rm = TRUE),
                                                         labels = FALSE, include.lowest = TRUE)]

# Definir los límites para los grupos
limites <- c(-0, 1, 365, 3651, 7301, 10951, 14601, 18251, Inf)

# Definir las etiquetas para los grupos
etiquetas <- c("Ya expirada la empresa", "0 años (menos de 365 días)", 
               "De 1  año (365 días) a 10 años (3650 días)", 
               "De 11  años (3651 días) a 20 años (7300 días)",
               "De 21  años (7301 días) a 30 años (10950 días)", 
               "De 31  años (10950 días) a 40 años (14600 días)",
               "De 40  años (14601 días) a 50 años (18250 días)",
               "Más de 50  años (18250 días)")

# Generar grupos usando la función cut()
Final_Calidad[, Grupos_Dif_adju_mayor_expiracion := cut(Dif_adju_mayor_expiracion, 
                                                        breaks = limites,
                                                        labels = etiquetas,
                                                        include.lowest = TRUE)]

Final_Calidad$Grupos_Dif_adju_mayor_expiracion <- ifelse(is.na(Final_Calidad$Grupos_Dif_adju_mayor_expiracion), 
                                                         "No es posible estimar el dato", 
                                                         Final_Calidad$Grupos_Dif_adju_mayor_expiracion)

#Establenciendo los rangos 
Final_Calidad$Dif_oferta_mayor_constitucion<-as.numeric(Final_Calidad$Dif_oferta_mayor_constitucion)

min_val <- min(Final_Calidad$Dif_oferta_mayor_constitucion, na.rm = TRUE)
max_val <- max(Final_Calidad$Dif_oferta_mayor_constitucion, na.rm = TRUE)
media_val <- median(Final_Calidad$Dif_oferta_mayor_constitucion, na.rm = TRUE)

# Definir los límites para los grupos
limites <- c(30, 180, 365, 3651, 7301, 10951, 14601, 18251, Inf)

# Definir las etiquetas para los grupos
etiquetas <- c("Menos de 1 mes", 
               "De medio año (180 días) a 1 año (365 días)",
               "De 1 año (365 días) a 10 años (3650 días)", 
               "De 11 años (3651 días) a 20 años (7300 días)",
               "De 21 años (7301 días) a 30 años (10950 días)", 
               "De 31 años (10950 días) a 40 años (14600 días)",
               "De 40 años (14601 días) a 50 años (18250 días)",
               "Más de 50 años (18250 días)")

# Generar grupos usando la función cut()
Final_Calidad[, Grupos_Dif_oferta_mayor_constitucion := cut(Dif_oferta_mayor_constitucion, 
                                                            breaks = limites,
                                                            labels = etiquetas,
                                                            include.lowest = TRUE)]

Final_Calidad$Grupos_Dif_oferta_mayor_constitucion <- ifelse(is.na(Final_Calidad$Grupos_Dif_oferta_mayor_constitucion), 
                                                             "No es posible estimar el dato", 
                                                             Final_Calidad$Grupos_Dif_oferta_mayor_constitucion)

#Selección final de indicadores ---------
Final_Calidad<-Final_Calidad[,c("nro_acto", "numero_linea", "nro_oferta","nro_procedimiento", 
                                "modalidad_procedimiento",
                                "cedula_institucion", "nombre_institucion",  
                                "cedula_proveedor",  "tipo_proveedor", 
                                "codigoProductoAdjudicado", "nombre_identificacion",
                                "Total_objeciones", 
                                "Dif_publi_present_ofer", "fecha_presenta_oferta", "fecha_publicacion",
                                "Dif_oferta_mayor_constitucion", "fecha_constitucion",
                                "Dif_adju_mayor_expiracion", "fecha_expiracion", "fecha_adj_firme",
                                "Estimado_mala_calidad", "precio_unitario_estimado_colones",
                                "Rango_Dif_Prom_Ofer_vs_Pre_Adju", "precio_unitario_adjudicado_colones", "Prom_precio_Unitario_Ofertado",
                                "Completitud", "primerFechaAdjudicacion", "primerFechaContratacion", "primerFechaRecepcion",
                                "Dif_Lar_Cont_vs_Adju", "Largo_Digito_Pre_Contr", "Largo_Digito_Pre_Adju",
                                "Grupos_Dif_adju_mayor_expiracion", "Grupos_Dif_oferta_mayor_constitucion")]

colnames(Final_Calidad) <- c("numeroActo", "idLinea", "numeroOferta", "numeroProcedimiento", 
                             "modalidad_procedimiento",
                             "idInstitucion", "nombre_institucion",  
                             "cedula_proveedor",  "tipo_proveedor", 
                             "codigo_identificacion", "nombre_identificacion",
                             "Total_objeciones",
                             "Dif_publi_present_ofer", "fecha_presenta_oferta", "fecha_publicacion",
                             "Dif_oferta_mayor_constitucion", "fecha_constitucion",
                             "Dif_adju_mayor_expiracion", "fecha_expiracion", "fecha_adj_firme",
                             "Estimado_mala_calidad", "precio_unitario_estimado_colones",
                             "Rango_Dif_Prom_Ofer_vs_Pre_Adju", "precio_unitario_adjudicado_colones", "Prom_precio_Unitario_Ofertado",
                             "Completitud", "primerFechaAdjudicacion", "primerFechaContratacion", "primerFechaRecepcion",
                             "Dif_Lar_Cont_vs_Adju", "Largo_Digito_Pre_Contr", "Largo_Digito_Pre_Adju",
                             "Grupos_Dif_adju_mayor_expiracion", "Grupos_Dif_oferta_mayor_constitucion")

setDT(Final_Calidad)
Final_Calidad[,Anno:=substr(numeroProcedimiento,1,4)]
Final_Calidad[,Cod_16:=substr(codigo_identificacion,1,16)]
Final_Calidad[,Tipo_bien:=substr(codigo_identificacion,1,1)]


Final_Calidad_2<- Final_Calidad[(Anno < 2023 & Anno > 2019),]
Final_Calidad_3<- Final_Calidad_2[Final_Calidad_2$Tipo_bien%in%c("1","2","3","4","5","6"), ]

Final_Calidad_3$Grupos_Dif_oferta_mayor_constitucion <- ifelse(is.na(Final_Calidad_3$Grupos_Dif_oferta_mayor_constitucion), 
                                                               "No es posible estimar el dato", 
                                                               Final_Calidad_3$Grupos_Dif_oferta_mayor_constitucion)

Final_Calidad_3$Grupos_Dif_adju_mayor_expiracion <- ifelse(is.na(Final_Calidad_3$Grupos_Dif_adju_mayor_expiracion), 
                                                           "No es posible estimar el dato", 
                                                           Final_Calidad_3$Grupos_Dif_adju_mayor_expiracion)

Final_Calidad_3[, Dif_Lar_Cont_vs_Adju := ifelse(is.na(Dif_Lar_Cont_vs_Adju), 
                                                 "No se puede calcular", 
                                                 Dif_Lar_Cont_vs_Adju)]

summary(is.na(Final_Calidad_3))


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

# Escribir el contenido de calidad en una tabla de la base de datos MySQL 
nombre_de_tabla <- "calidad"
dbWriteTable(con, name = nombre_de_tabla, value = Final_Calidad_3 , row.names = FALSE, overwrite=TRUE)

# Cerrar la conexion a la base de datos MySQL
dbDisconnect(con)

















