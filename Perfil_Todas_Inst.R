#***********************************************CODIGO DE PERFIL INSTITUCIONAL***************************************#
#**************************************************ELABORADO POR LA CGR*****************************************#

#**************************************INICIO**************************# 


# 1. Cargando las librerias -----------------------------------------------

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


# 2. Conexion SICOP y CASP  ---Este paso es exclusivo para la CGR, los usuarios externos pueden descargar cada tabla en el site---------------------------
SICOP <-odbcConnect("ODBC_CGRSQL")
#SICOP <-odbcConnect("Conexion_SICOP")
tablas <- sqlTables(SICOP, tableType = "TABLE")

# 4. Cargando las tablas necesarias -------------------------------
tablas$TABLE_NAME

Instituciones <- data.table(sqlFetch(SICOP, "dbo.Instituciones",as.is=TRUE))
lineasadjudicadas <- data.table(sqlFetch(SICOP, "dbo.lineasadjudicadas",as.is=TRUE))
procedimientos <- data.table(sqlFetch(SICOP, "dbo.procedimientos",as.is=TRUE))
adjudicaciones <- data.table(sqlFetch(SICOP, "dbo.Adjudicaciones",as.is=TRUE))
proveedores <- data.table(sqlFetch(SICOP, "dbo.proveedores",as.is=TRUE))
Ofertas <- data.table(sqlFetch(SICOP, "dbo.Ofertas",as.is=TRUE))
identificacionbs <- data.table(sqlFetch(SICOP, "dbo.identificacionbs",as.is=TRUE))
LineasProc <- data.table(sqlFetch(SICOP, "dbo.LineasProc",as.is=TRUE))
LineasContratadas <- data.table(sqlFetch(SICOP, "dbo.LineasContratadas",as.is=TRUE))
Contratos <- data.table(sqlFetch(SICOP, "dbo.Contratos",as.is=TRUE))

bases <- list("Instituciones","lineasadjudicadas","procedimientos","adjudicaciones","proveedores", "Ofertas",
              "identificacionbs", "LineasContratadas","Contratos")

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

tablas$TABLE_NAME


# Uniendo las bases para tener los plazos vinculados a una misma base --------------------

B_Plazos_1<- LineasProc[,.(nro_sicop, numero_linea)]
B_Plazos_2<- merge(B_Plazos_1,procedimientos[,.(nro_sicop,cedula_institucion,fecha_publicacion,nro_procedimiento,tipo_procedimiento, modalidad_procedimiento)],by = "nro_sicop",all.x = TRUE)
B_Plazos_3<- merge(B_Plazos_2,lineasadjudicadas[,.(nro_sicop,nro_acto,nro_oferta,nro_linea)], by.x = c("nro_sicop","numero_linea"),by.y = c("nro_sicop","nro_linea"),all.x = TRUE)
B_Plazos_4<- merge(B_Plazos_3,adjudicaciones[,.(nro_sicop,nro_acto,fecha_adj_firme,desierto)], by = c("nro_sicop","nro_acto"),all.x = TRUE)
B_Plazos_5<- merge(B_Plazos_4,Ofertas[,.(nro_sicop,nro_oferta,fecha_presenta_oferta,elegible)], by = c("nro_sicop","nro_oferta"),all.x = TRUE)
B_Plazos_6<- merge(B_Plazos_5,LineasContratadas[,.(nro_sicop,nro_acto,nro_contrato,nro_linea_cartel)], by.x = c("nro_sicop","nro_acto","numero_linea"), by.y = c("nro_sicop","nro_acto","nro_linea_cartel"),all.x = TRUE)
B_Plazos_7<- merge(B_Plazos_6,Contratos[,.(nro_sicop,nro_contrato,fecha_notificacion,secuencia)], by ="nro_contrato",all.y = TRUE)

names(B_Plazos_7)


#Establecimiento de las fechas con el formato fechas
B_Plazos_7$fecha_publicacion<-as.Date(B_Plazos_7$fecha_publicacion)
B_Plazos_7$fecha_presenta_oferta<-as.Date(B_Plazos_7$fecha_presenta_oferta)
B_Plazos_7$fecha_adj_firme<-as.Date(B_Plazos_7$fecha_adj_firme)
B_Plazos_7$fecha_contrato<-as.Date(B_Plazos_7$fecha_notificacion)


# Calculando la diferencia entre todas las fechas de inter?s --------------------
B_Plazos_7[, Dif_publi_present_ofer:= (fecha_presenta_oferta-fecha_publicacion)]
B_Plazos_7[, Dif_publi_adj_firme:= (fecha_adj_firme-fecha_publicacion)]
B_Plazos_7[, Dif_publi_contrato:= (fecha_contrato-fecha_publicacion)]
B_Plazos_7[, Dif_present_ofer_adj_firme:= (fecha_adj_firme-fecha_presenta_oferta)]
B_Plazos_7[, Dif_present_ofer_contrato:= (fecha_contrato-fecha_presenta_oferta)]
B_Plazos_7[, Dif_adj_firme_contrato:= (fecha_contrato-fecha_adj_firme)]

########################################
######?CUANTO DURAN LOS PROCEDIMIENTOS?#
########################################

B_Plazos_7[, anno:= substr(nro_procedimiento,1,4)]



Resumen_Plazos_Tipo_Procedimientos<- B_Plazos_7[,.(Dif_publi_adj_firme_Institucion=mean(Dif_publi_adj_firme, na.rm = TRUE),
                                                   Dif_publi_contrato_Institucion=mean(Dif_publi_contrato, na.rm = TRUE),
                                                   Dif_adj_firme_contrato_Institucion=mean(Dif_adj_firme_contrato, na.rm=TRUE),
                                                   Conteo_Institucion=length(nro_contrato)), 
                                                keyby=.(cedula_institucion,tipo_procedimiento,anno)]

Resumen_Plazos_Tipo_Modalidad<- B_Plazos_7[,.(Dif_publi_adj_firme_Institucion=mean(Dif_publi_adj_firme, na.rm = TRUE),
                                              Dif_publi_contrato_Institucion=mean(Dif_publi_contrato, na.rm = TRUE),
                                              Dif_adj_firme_contrato_Institucion=mean(Dif_adj_firme_contrato, na.rm=TRUE),
                                              Conteo_Institucion=length(nro_contrato)), 
                                           keyby=.(cedula_institucion,modalidad_procedimiento,anno)]

##############################################
######?CUANDO SE REALIZAN LOS PROCEDIMIENTOS?#
##############################################


B_Plazos_7[, mes:= month(fecha_publicacion)]
B_Plazos_7[, anno:= year(fecha_publicacion)]

Conteo_CUANDO<- B_Plazos_7[,.(Conteo_lineas=length(c(nro_acto,nro_oferta,numero_linea)),
                              Conteo_procedimientos=length(nro_procedimiento)), 
                           keyby=.(nro_procedimiento)]

B_Plazos_8<- merge(B_Plazos_7, Conteo_CUANDO, by ="nro_procedimiento", all.y = TRUE)


######################
######?QUE CONTRATAN?#
######################

Que_Contratan<- lineasadjudicadas


Que_Contratan$tipo_cambio_dolar <- as.numeric(Que_Contratan$tipo_cambio_dolar)
Que_Contratan$tipo_cambio_crc <- as.numeric(Que_Contratan$tipo_cambio_crc)
Que_Contratan$precio_unitario_adjudicado <- as.numeric(Que_Contratan$precio_unitario_adjudicado)


Que_Contratan[, Prec.Unit.Adj.Col := ifelse(tipo_moneda == "CRC",precio_unitario_adjudicado, precio_unitario_adjudicado*tipo_cambio_crc*(1/tipo_cambio_dolar))][]
#Que_Contratan[,.N, keyby = .(Prec.Unit.Adj.Col)][]
Que_Contratan[, Monto_linea_colones := as.numeric(cantidad_adjudicada) * (Prec.Unit.Adj.Col - as.numeric(descuento) + as.numeric(iva) + as.numeric(otros_impuestos) + as.numeric(acarreos))]
Que_Contratan[, codigo_producto_16 :=substr(codigo_producto,1,16)]
Que_Contratan[, codigo_producto_1 :=substr(codigo_producto,1,1)]


#Pegando la descripci?n de los bienes contratados
Que_Contratan_2<- merge(Que_Contratan, identificacionbs[,.(codigo_identificacion,nombre_identificacion)], 
                        by.x = "codigo_producto_16", by.y = "codigo_identificacion",
                        all.x = TRUE)

#Pegando la c?dula de la instituci?n y los tipos de procedimientos
Que_Contratan_3<- merge(Que_Contratan_2, procedimientos[,.(nro_sicop,cedula_institucion,
                                                           nro_procedimiento,tipo_procedimiento, 
                                                           modalidad_procedimiento)],by = "nro_sicop",all.x = TRUE)

Que_Contratan_4<- merge(Que_Contratan_3, proveedores[,.(cedula_proveedor,nombre_proveedor,
                                                        tipo_proveedor,tamano_proveedor, 
                                                        provincia,canton)],by = "cedula_proveedor",all.x = TRUE)


Factores_Que_Contratan<- Que_Contratan_4[, .(cedula_institucion, nro_procedimiento, nro_linea,
                                             codigo_producto_1,codigo_producto_16,
                                             cantidad_adjudicada, Prec.Unit.Adj.Col,
                                             Monto_linea_colones,nombre_identificacion,
                                             tipo_procedimiento,modalidad_procedimiento)]

Factores_A_Quien_Compran<- Que_Contratan_4[, .(cedula_institucion, nro_procedimiento, nro_linea,
                                               codigo_producto_1,codigo_producto_16,
                                               cantidad_adjudicada, Prec.Unit.Adj.Col,
                                               Monto_linea_colones,nombre_identificacion,
                                               tipo_procedimiento,modalidad_procedimiento,
                                               cedula_proveedor, nombre_proveedor,
                                               tipo_proveedor, tamano_proveedor, provincia,canton)]

Factores_A_Quien_Compran[, anno:= substr(nro_procedimiento,1,4)]


#######################
######ANALISIS MERCADO#
#######################


Alcance <- data.table(sqlFetch(SICOP, "ds.alcance",as.is=TRUE))
ofertas <- data.table(sqlFetch(SICOP, "dbo.ofertas",as.is=TRUE))
LineasOfertadas <- data.table(sqlFetch(SICOP, "dbo.LineasOfertadas",as.is=TRUE))

Alcance <- Alcance[,c("numeroActo","numeroOferta","numeroProcedimiento","idLinea","idProcedimiento","idInstitucion","anno","codigoProductoSolicitado","codigoProductoAdjudicado",
                      "codigoProductoContratado","codigoProductoRecibido")]
ofertas <- ofertas[,c("nro_sicop", "nro_oferta","cedula_proveedor","fecha_presenta_oferta","elegible")]
LineasOfertadas <- LineasOfertadas[,c("nro_sicop","nro_oferta","nro_linea","precio_unitario_ofertado", "cantidad_ofertada","codigo_producto")]

bases <- list("Alcance", "ofertas","LineasOfertadas")

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

#Convirtiendo los códigos de producto de 24 a 16
Alcance$codigoProductoAdjudicado_16 <- substr(Alcance$codigoProductoAdjudicado, 1, 16)
Alcance$codigoProductoContratado_16 <- substr(Alcance$codigoProductoContratado, 1, 16)
Alcance$codigoProductoRecibido_16 <- substr(Alcance$codigoProductoRecibido, 1, 16)
Alcance$codigoProductoSolicitado_16 <- substr(Alcance$codigoProductoSolicitado, 1, 16)

#limpiando los datos para incluir solo datos desde el 2018, y solo bienes
Alcance[, tipo_bien:= substr(codigoProductoSolicitado,1,1)]
Alcance[,.N, keyby = tipo_bien][order(-N)][,Porcentaje := (N/sum(N))*100][]
#Alcance <- Alcance[Alcance$tipo_bien %in% c("1","2","3","4","5","6"), ]

Base_General_ofertas <- merge(ofertas,LineasOfertadas,by=c("nro_sicop","nro_oferta"),all.y = T)
Base_General_ofertas <- merge(Base_General_ofertas,Alcance[,c("numeroActo","numeroOferta","idLinea","codigoProductoAdjudicado","numeroProcedimiento","idInstitucion")],by.x = c("nro_oferta","nro_linea"),by.y=c("numeroOferta","idLinea"),all.x = T)
Base_General_ofertas <- Base_General_ofertas[elegible=="Sí",]

DUPLICADOS <- Base_General_ofertas[duplicated(Base_General_ofertas),]
Base_General_ofertas <- Base_General_ofertas%>%  distinct()
Base_General_ofertas <- as.data.table(Base_General_ofertas)

Base_General_ofertas[, tipo_bien:= substr(codigo_producto,1,1)]
Base_General_ofertas[,.N, keyby = tipo_bien][order(-N)][,Porcentaje := (N/sum(N))*100][]
#Base_General_ofertas <- Base_General_ofertas[Base_General_ofertas$tipo_bien %in% c("1","2","3","4","5","6"), ]

Base_General_ofertas$nro_sicop <- as.numeric(Base_General_ofertas$nro_sicop)
Base_General_ofertas <- setkey(Base_General_ofertas,"fecha_presenta_oferta","nro_linea","numeroProcedimiento","nro_sicop")

Base_General_ofertas <- Base_General_ofertas %>% arrange(fecha_presenta_oferta,nro_linea,nro_sicop,nro_oferta) %>%
  group_by(nro_linea,nro_sicop,nro_oferta) %>%
  mutate(acto_final = ifelse(numeroActo==max(numeroActo),1,0))

Base_General_ofertas <- as.data.table(Base_General_ofertas)

Base_General_ofertas[,linea_adj:=ifelse(!is.na(numeroActo),1,0)][]
Base_General_ofertas[, proceso_unico:=paste0(nro_sicop,"-",nro_linea, sep="")]

Base_General_ofertas$codigo_producto_16 <-substr(Base_General_ofertas$codigo_producto,1,16)
Base_General_ofertas$codigo_producto_8 <-substr(Base_General_ofertas$codigo_producto_16,1,8)

Base_General_ofertas <- setkey(Base_General_ofertas,"fecha_presenta_oferta","nro_linea","numeroProcedimiento","nro_sicop")

Base_General_ofertas <- Base_General_ofertas %>% arrange(fecha_presenta_oferta,nro_linea,nro_sicop) %>%
  group_by(cedula_proveedor) %>%
  mutate(nro_oferta_proveedor = row_number())

Base_General_ofertas <- Base_General_ofertas %>% arrange(fecha_presenta_oferta,nro_linea,nro_sicop) %>%
  group_by(cedula_proveedor,codigo_producto_16) %>%
  mutate(nro_oferta_proveedor_prod= row_number())

Base_General_ofertas <- Base_General_ofertas %>% arrange(fecha_presenta_oferta,nro_linea,nro_sicop) %>%
  group_by(cedula_proveedor,idInstitucion) %>%
  mutate(nro_oferta_proveedor_inst = row_number())

Base_General_ofertas_2 <- Base_General_ofertas[(Base_General_ofertas$linea_adj==1 & Base_General_ofertas$acto_final==1),] %>% arrange(fecha_presenta_oferta,nro_linea,nro_sicop) %>%
  group_by(cedula_proveedor) %>%
  mutate(nro_adju_proveedor = row_number())

Base_General_ofertas_2$codigoProductoAdjudicado_16 <- substr(Base_General_ofertas_2$codigoProductoAdjudicado, 1, 16)

Base_General_ofertas_2 <- Base_General_ofertas_2[(Base_General_ofertas_2$linea_adj==1 & Base_General_ofertas_2$acto_final==1),] %>% arrange(fecha_presenta_oferta,nro_linea,nro_sicop) %>%
  group_by(cedula_proveedor,codigoProductoAdjudicado_16) %>%
  mutate(nro_adju_proveedor_prod = row_number())

Base_General_ofertas_2 <- Base_General_ofertas_2[(Base_General_ofertas_2$linea_adj==1 & Base_General_ofertas_2$acto_final==1),] %>% arrange(fecha_presenta_oferta,nro_linea,nro_sicop) %>%
  group_by(cedula_proveedor, idInstitucion) %>%
  mutate(nro_adju_proveedor_inst = row_number())

# MERCADO MACRO ------------
#"Conteo_ofertas_por_proceso","poder_2", "ihh", "VIC"
Base_General_ofertas_2 <- Base_General_ofertas_2 %>% group_by(proceso_unico) %>% mutate(Conteo_ofertas_por_proceso = length(nro_oferta))

##INDICADOR 23: Analisis de concentracion de mercado para cada artículo analizado------
# 16 digitos sacar el % adjudicaciones por proveedor (ponderar por cantidad). Con esto sacar IHH
Base_General_ofertas_2$cantidad_ofertada <- as.numeric(Base_General_ofertas_2$cantidad_ofertada)

#Cambio de base porque se reduce la dimensión

Base_General_ofertas_2 <- Base_General_ofertas_2[!is.na(Base_General_ofertas_2$numeroActo),] %>% group_by(codigo_producto_16) %>% mutate(poder = cantidad_ofertada/sum(cantidad_ofertada))
Base_General_ofertas_2 <- Base_General_ofertas_2[!is.na(Base_General_ofertas_2$numeroActo),] %>% group_by(codigo_producto_8) %>% mutate(poder8 = cantidad_ofertada/sum(cantidad_ofertada))

as.data.table(Base_General_ofertas_2)

Base_General_ofertas_2$codigo_producto_16 <- as.numeric(Base_General_ofertas_2$codigo_producto_16)
Base_General_ofertas_2$cedula_proveedor <- as.numeric(Base_General_ofertas_2$cedula_proveedor)

Base_General_ofertas_2 <- Base_General_ofertas_2 %>%
  group_by(codigo_producto_16, cedula_proveedor) %>%
  mutate(poder_2 = sum(poder))

Base_General_ofertas_2 <- Base_General_ofertas_2 %>%
  group_by(codigo_producto_8, cedula_proveedor) %>%
  mutate(poder8_2 = sum(poder8))

### Ind_12, Poder de Mercado
Base_General_ofertas_2<-as.data.table(Base_General_ofertas_2)

names(Base_General_ofertas_2)
Base_General_ofertas_2[, Conteo_Oferta_Cod16:=length(nro_oferta), keyby=.(codigo_producto_16)]

Base_General_ofertas_2[, Poder_Mercado_2:=ifelse((poder_2==1 & Conteo_Oferta_Cod16==1),0,poder_2)]

Base_General_ofertas_2[,.N, keyby=Poder_Mercado_2][order(-N)][,porcentaje:=(N/sum(N))*100][]

Base_General_ofertas_2[, Conteo_Oferta_Cod8:=length(nro_oferta), keyby=.(codigo_producto_8)]

Base_General_ofertas_2[, Poder_Mercado8_2:=ifelse((poder8_2==1 & Conteo_Oferta_Cod8==1),0,poder8_2)]

#Cambio de base porque se reduce la dimensión
Base_General_ofertas_1 <- Base_General_ofertas_2[,c("codigo_producto_16","cedula_proveedor","poder_2","poder8_2")] %>%
  distinct(codigo_producto_16,cedula_proveedor,poder_2) %>%
  group_by(codigo_producto_16) %>%
  mutate(ihh = sum(poder_2^2))

Base_General_ofertas_1.5 <-  Base_General_ofertas_2[,c("codigo_producto_8","cedula_proveedor","poder_2","poder8_2")] %>%
  distinct(codigo_producto_8,cedula_proveedor,poder8_2) %>%
  group_by(codigo_producto_8) %>%
  mutate(ihh8 = sum(poder8_2^2))

##INDICADOR 30: Tasa de victoria en los procesos de contratación participantes------

## VIC vs INSTITUCION Y vs PRODUCTO
#A TRAVES DEL TIEMPO
Base_General_ofertas_2$VIC <- ifelse((Base_General_ofertas_2$linea_adj==1 &Base_General_ofertas_2$acto_final==1),Base_General_ofertas_2$nro_adju_proveedor/Base_General_ofertas_2$nro_oferta_proveedor,0)
Base_General_ofertas_2$VIC_prod <- ifelse((Base_General_ofertas_2$linea_adj==1 &Base_General_ofertas_2$acto_final==1),Base_General_ofertas_2$nro_adju_proveedor_prod/Base_General_ofertas_2$nro_oferta_proveedor_prod,0)
Base_General_ofertas_2$VIC_inst <- ifelse((Base_General_ofertas_2$linea_adj==1 &Base_General_ofertas_2$acto_final==1),Base_General_ofertas_2$nro_adju_proveedor_inst/Base_General_ofertas_2$nro_oferta_proveedor_inst,0)

Base_General_ofertas_2[, VIC_2:=ifelse((VIC==1 & Conteo_Oferta_Cod16==1),0,VIC)]

Base_General_ofertas_2 <- Base_General_ofertas_2 %>% group_by(cedula_proveedor,codigo_producto_8) %>%
  mutate(VIC_ultimo = last(VIC_2),
         VIC_promedio = mean(VIC_2),
         VIC_var = var(VIC_2))

#GENERAL
VIC <- Base_General_ofertas_2 %>% group_by(cedula_proveedor) %>%
  filter(nro_adju_proveedor== max(nro_adju_proveedor)) %>% mutate(VIC_gen = VIC)

VIC <- VIC %>% group_by(cedula_proveedor,codigo_producto_16) %>%
  filter(nro_adju_proveedor== max(nro_adju_proveedor)) %>% mutate(VIC_prod_gen = VIC_prod)

VIC <- VIC %>% group_by(cedula_proveedor,idInstitucion) %>%
  filter(nro_adju_proveedor== max(nro_adju_proveedor)) %>% mutate(VIC_inst_gen = VIC_inst)

Base_General_ofertas_2 <- merge(Base_General_ofertas_2,Base_General_ofertas_1[,c("ihh","cedula_proveedor","codigo_producto_16")],by = c("cedula_proveedor","codigo_producto_16"), all.x = TRUE)
Base_General_ofertas_2 <- merge(Base_General_ofertas_2,Base_General_ofertas_1.5[,c("ihh8","cedula_proveedor","codigo_producto_8")],by = c("cedula_proveedor","codigo_producto_8"), all.x = TRUE)

Base_General_ofertas_2<-as.data.table(Base_General_ofertas_2)

Base_General_ofertas_2[, IHH_2:=ifelse((ihh8==1 & Conteo_Oferta_Cod8==1),0,ihh8)]
Base_General_ofertas_2 <- Base_General_ofertas_2 %>% group_by(codigo_producto_8) %>%
  mutate(IHH_ultimo = last(IHH_2),
         IHH_promedio = mean(IHH_2))

Base_Analisis_Mercado <- Base_General_ofertas_2[,c("numeroActo","nro_oferta","nro_sicop",
                                                   "linea_adj","idInstitucion","cedula_proveedor","codigo_producto_8",
                                                   "VIC_2","VIC_ultimo","VIC_promedio","VIC_var",
                                                   "Poder_Mercado8_2","IHH_2")]
setDT(Base_Analisis_Mercado)

########################################################################
########################################################################
########################################################################
########################################################################


#GUARDANDO LOS ARCHIVOS EN CSV

fwrite(x = Resumen_Plazos_Tipo_Modalidad, 
       file = "Perfil_Cuanto_tipo_modalidad.csv",sep = ";",dec = ",")

fwrite(x = Resumen_Plazos_Tipo_Procedimientos, 
       file = "Perfil_Cuanto_tipo_procedimiento.csv",sep = ";",dec = ",")



#CUANDO CONTRATAN
Cuando_Contrata<- B_Plazos_8[, .(nro_procedimiento,cedula_institucion,anno,mes, Conteo_lineas, Conteo_procedimientos)][]

#GUARDANDO LOS ARCHIVOS EN CSV

fwrite(x = Cuando_Contrata, 
       file = "Perfil_Cuando_Contrata.csv",sep = ";",dec = ",")

##############################################
######?COMO SE REALIZAN LOS PROCEDIMIENTOS?#
##############################################
#names(B_Plazos_8)

Como_Contratan<- B_Plazos_7[,.(Conteo_lineas=length(c(nro_acto,nro_oferta,numero_linea)),
                              Conteo_procedimientos=length(nro_procedimiento)), 
                              keyby=.(tipo_procedimiento, anno,cedula_institucion)]

fwrite(x = Como_Contratan, 
       file = "Perfil_Como_Contratan.csv",sep = ";",dec = ",")



#### QUE CONTRATAN ######

fwrite(x = Factores_A_Quien_Compran, 
       file = "Perfil_Base_Que_Quien_Contratan.csv",sep = ";",dec = ",")


Que_Contrata_codigo1<- Factores_A_Quien_Compran[,
                                                .(Conteo_lineas=length(c(nro_procedimiento,nro_linea)),
                                                  Conteo_procedimientos=length(nro_procedimiento),
                                                  total_gastado=sum(Monto_linea_colones, na.rm=TRUE)), 
                                                keyby=.(codigo_producto_1, anno,cedula_institucion)]

Que_Contrata_codigo16<- Factores_A_Quien_Compran[,
                                                 .(Conteo_lineas=length(c(nro_procedimiento,nro_linea)),
                                                   Conteo_procedimientos=length(nro_procedimiento),
                                                   total_gastado=sum(Monto_linea_colones, na.rm=TRUE)), 
                                                 keyby=.(codigo_producto_16, anno,cedula_institucion)]

fwrite(x = Que_Contrata_codigo1, 
       file = "Perfil_Que_Contrata_codigo1_.csv",sep = ";",dec = ",")

fwrite(x = Que_Contrata_codigo16, 
       file = "Perfil_Que_Contrata_codigo16.csv",sep = ";",dec = ",")


#### A QUIEN LE CONTRATAN
A_Quien_Contrata_Empresas<- Factores_A_Quien_Compran[,
                                                     .(Conteo_lineas=length(c(nro_procedimiento,nro_linea)),
                                                       Conteo_procedimientos=length(nro_procedimiento),
                                                       total_gastado=sum(Monto_linea_colones, na.rm=TRUE)), 
                                                     keyby=.(cedula_proveedor, anno,cedula_institucion)]

A_Quien_Contrata_Tipo_Empresas<- Factores_A_Quien_Compran[,
                                                          .(Conteo_lineas=length(c(nro_procedimiento,nro_linea)),
                                                            Conteo_procedimientos=length(nro_procedimiento),
                                                            total_gastado=sum(Monto_linea_colones, na.rm=TRUE)), 
                                                          keyby=.(tipo_proveedor, anno,cedula_institucion)]

A_Quien_Contrata_Tamano_Empresas<- Factores_A_Quien_Compran[,
                                                            .(Conteo_lineas=length(c(nro_procedimiento,nro_linea)),
                                                              Conteo_procedimientos=length(nro_procedimiento),
                                                              total_gastado=sum(Monto_linea_colones, na.rm=TRUE)), 
                                                            keyby=.(tamano_proveedor, anno,cedula_institucion)]

A_Quien_Contrata_provincia_Empresas<- Factores_A_Quien_Compran[,
                                                               .(Conteo_lineas=length(c(nro_procedimiento,nro_linea)),
                                                                 Conteo_procedimientos=length(nro_procedimiento),
                                                                 total_gastado=sum(Monto_linea_colones, na.rm=TRUE)), 
                                                               keyby=.(provincia, anno,cedula_institucion)]

A_Quien_Contrata_canton_Empresas<- Factores_A_Quien_Compran[,
                                                            .(Conteo_lineas=length(c(nro_procedimiento,nro_linea)),
                                                              Conteo_procedimientos=length(nro_procedimiento),
                                                              total_gastado=sum(Monto_linea_colones, na.rm=TRUE)), 
                                                            keyby=.(provincia, canton, anno,cedula_institucion)]

fwrite(x = A_Quien_Contrata_Empresas, 
       file = "Perfil_A_Quien_Contrata_Empresas.csv",sep = ";",dec = ",")

fwrite(x = A_Quien_Contrata_Tipo_Empresas, 
       file = "Perfil_A_Quien_Contrata_Tipo_Empresas.csv",sep = ";",dec = ",")

fwrite(x = A_Quien_Contrata_Tamano_Empresas, 
       file = "Perfil_A_Quien_Contrata_Tamano_Empresas.csv",sep = ";",dec = ",")

fwrite(x = A_Quien_Contrata_provincia_Empresas, 
       file = "Perfil_A_Quien_Contrata_provincia_Empresas.csv",sep = ";",dec = ",")

fwrite(x = A_Quien_Contrata_canton_Empresas, 
       file = "Perfil_A_Quien_Contrata_canton_Empresas.csv",sep = ";",dec = ",")

fwrite(x = Base_Analisis_Mercado, 
       file = "Perfil_Analisis_Mercado.csv",sep = ";",dec = ",")


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

# Escribir el contenido de las tablas en una tabla de la base de datos MySQL (reemplaza "nombre_de_la_tabla" con el nombre deseado para la tabla)
nombre_de_tabla <- "Perfil_Cuanto_tipo_modalidad"
dbWriteTable(con, name = nombre_de_tabla, value = Resumen_Plazos_Tipo_Modalidad , row.names = FALSE, overwrite=TRUE)

nombre_de_tabla <- "Perfil_Cuanto_tipo_procedimiento"
dbWriteTable(con, name = nombre_de_tabla, value = Resumen_Plazos_Tipo_Procedimientos , row.names = FALSE, overwrite=TRUE)

nombre_de_tabla <- "Perfil_Cuando_Contrata"
dbWriteTable(con, name = nombre_de_tabla, value = Cuando_Contrata , row.names = FALSE, overwrite=TRUE)

nombre_de_tabla <- "Perfil_Como_Contratan"
dbWriteTable(con, name = nombre_de_tabla, value = Como_Contratan , row.names = FALSE, overwrite=TRUE)

nombre_de_tabla <- "Perfil_Base_Que_Quien_Contratan"
dbWriteTable(con, name = nombre_de_tabla, value = Factores_A_Quien_Compran , row.names = FALSE, overwrite=TRUE)

nombre_de_tabla <- "Perfil_Que_Contrata_codigo1"
dbWriteTable(con, name = nombre_de_tabla, value = Que_Contrata_codigo1 , row.names = FALSE, overwrite=TRUE)

nombre_de_tabla <- "Perfil_Que_Contrata_codigo16"
dbWriteTable(con, name = nombre_de_tabla, value = Que_Contrata_codigo16 , row.names = FALSE, overwrite=TRUE)

nombre_de_tabla <- "Perfil_A_Quien_Contrata_Empresas"
dbWriteTable(con, name = nombre_de_tabla, value = A_Quien_Contrata_Empresas, row.names = FALSE, overwrite=TRUE)

nombre_de_tabla <- "Perfil_A_Quien_Contrata_Tipo_Empresas"
dbWriteTable(con, name = nombre_de_tabla, value = A_Quien_Contrata_Tipo_Empresas , row.names = FALSE, overwrite=TRUE)

nombre_de_tabla <- "Perfil_A_Quien_Contrata_Tamano_Empresas"
dbWriteTable(con, name = nombre_de_tabla, value = A_Quien_Contrata_Tamano_Empresas , row.names = FALSE, overwrite=TRUE)

nombre_de_tabla <- "Perfil_A_Quien_Contrata_provincia_Empresas"
dbWriteTable(con, name = nombre_de_tabla, value = A_Quien_Contrata_provincia_Empresas , row.names = FALSE, overwrite=TRUE)

nombre_de_tabla <- "Perfil_A_Quien_Contrata_canton_Empresas"
dbWriteTable(con, name = nombre_de_tabla, value = A_Quien_Contrata_canton_Empresas, row.names = FALSE, overwrite=TRUE)

nombre_de_tabla <- "Perfil_Analisis_Mercado"
dbWriteTable(con, name = nombre_de_tabla, value = Base_Analisis_Mercado, row.names = FALSE, overwrite=TRUE)


# Cerrar la conexion a la base de datos MySQL
dbDisconnect(con)

print("Final de Perfiles institucionales")