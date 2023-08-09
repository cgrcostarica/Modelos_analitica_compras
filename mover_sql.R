#*********************************CODIGO DPARA EL TRALASADO DE LAS TABLAS A INSTANCIA SQL*******************************#
#**************************************************ELABORADO POR LA CGR*****************************************#

#**************************************INICIO**************************# 

#Mover las bases de datos de un sql a otro 

SICOP <-odbcConnect("ODBC_CGRSQL")
tablas <- sqlTables(SICOP, tableType = "TABLE")


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


#pasarlas tablas a sql

for (i in 1:nrow(tablas)) {
  # Obtener el esquema y el nombre de la tabla
  esquema <- tablas[i, "TABLE_SCHEM"]
  nombre <- tablas[i, "TABLE_NAME"]
  
  # Leer los datos de la tabla en un objeto data frame
  datos <- data.table(sqlFetch(SICOP, sqtable = paste0(esquema, ".", nombre), as.is = TRUE))
  
  # Escribir el contenido del data frame en la tabla de la base de datos MySQL
  nombre_de_tabla <- nombre
  dbWriteTable(con, name = nombre_de_tabla, value = datos, row.names = FALSE, overwrite = TRUE)
}

# Cerrar la conexion a la base de datos MySQL
dbDisconnect(con)

print("Mover las tablas")