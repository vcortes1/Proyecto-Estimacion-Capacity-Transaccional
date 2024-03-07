#install.packages("rJava")
#install.packages("RJDBC")
#install.packages("data.table")
#install.packages("stringr")
#install.packages("dplyr")
#install.packages("scales")
#install.packages("reshape")
#install.packages("ggplot2")
#install.packages("lubridate")
#install.packages("xlsx")
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("readr")

library(rJava)
library(RJDBC)
library(data.table)
library(stringr)
library(dplyr)
library(scales)
library(reshape)
library(ggplot2)
library(lubridate)
library(xlsx)
library(tidyverse)
library(readxl)
library(readr)

#Creación y Path del Driver VDriver

vDriver = JDBC(driverClass="com.vertica.Driver", classPath="C:/Users/vcortesv/programas/vertica-jdbc-4.1.14.jar")

#Crear conexión a Vertica ***

conectateavertica = dbConnect(vDriver, "jdbc:vertica://172.16.20.10:5433","vcortesv","0UTyu19G44")


#Query en vertica para seleccionar las transacciones hechas en un año de una sucursal en específico
demanda_trx<-paste0("SELECT COD_SUCURSAL, NRO_CAJA, TO_CHAR(FECHA_TRX,'yyyy-mm-dd') AS FECHA_TRX, TO_CHAR(FECHA_TRX, 'HH24') AS HORA_TRX
FROM RIPLEY.TRX_CABECERA
WHERE COD_SUCURSAL in (10074) 
                  AND TO_CHAR(FECHA_TRX,'yyyy-mm-dd') BETWEEN '2023-01-01' AND '2023-12-31' ")

#Generación de dataframe llamada demanda_trx a partir de la query
demanda_trx = dbGetQuery(conectateavertica,demanda_trx)
dbDisconnect(conectateavertica)


#Se filtra el data frame por las horas de operación
demanda_trx <- demanda_trx %>%
  filter(HORA_TRX %in% c(12, 13, 14, 15, 16, 17, 18, 19, 20))


#Transformación a formato numérico la columna NRO_CAJA  
demanda_trx$NRO_CAJA <- as.numeric(as.character(demanda_trx$NRO_CAJA))

#Se genera la concatenación entre COD_SUCURSAL y NRO_CAJA (en demanda_trx)
demanda_trx$concat <- paste0(demanda_trx$COD_SUCURSAL, demanda_trx$NRO_CAJA)

#Se cruza tabla demanda_trx y MaestroCajas
demanda_trx =
  merge(
    x = demanda_trx,
    y = MaestroCajas,
    by = "concat",
    all.x = TRUE
  )


#Del cruce, se selecciona solo las columnas deseadas
demanda_trx <- demanda_trx[, c("FECHA_TRX", "HORA_TRX", "PISO", "TIPO_CAJA")]


#Función para agrupar datos por fecha, hora, piso y tipo de caja 
mi_funcion <- function(data) {
  resultado <- data %>%
    group_by(FECHA_TRX, HORA_TRX, PISO, TIPO_CAJA) %>%
    summarise(DEMANDA = n()) %>%
    arrange(FECHA_TRX, HORA_TRX, PISO, TIPO_CAJA)
  
  return(resultado)
}

#Llamada a la función para aplicarla sobre demanda_trx
demanda_trx <- mi_funcion(demanda_trx)

#Se filtra por los valores de TIPO_CAJA y  se excluye los valores NA de PISO
demanda_trx <- subset(demanda_trx, TIPO_CAJA %in% c("POS", "AA", "RPOS", "scan&go") & !is.na(PISO))

#conversión de clases de las columnas de demanda_trx
demanda_trx$FECHA_TRX <- as.Date(demanda_trx$FECHA_TRX, format = "%Y-%m-%d") 
demanda_trx$HORA_TRX <- as.integer(demanda_trx$HORA_TRX)
demanda_trx$PISO <- as.integer(demanda_trx$PISO)
demanda_trx$FECHA_TRX <- as.character(demanda_trx$FECHA_TRX) 
demanda_trx$TIPO_CAJA <- as.character(demanda_trx$TIPO_CAJA) 







