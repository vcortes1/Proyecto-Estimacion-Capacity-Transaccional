#install.packages("lpSolve")
#install.packages("ompr")
#install.packages("ompr.roi")
#install.packages("ROI.plugin.glpk")
#install.packages("lubridate")
#install.packages("ROI")

library(lubridate)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(lpSolve)
library(ROI)

#los paquetes anteriores tienen que ver con la manipulación de fechas y la optimización matemática

###################################################################################################################################################################################################################################################################################################################################################################################################################################
#CONJUNTOS

cajas <- c("POS","AA","RPOS","scan&go") #las 4 cajas utilizadas para el análisis
cajas_principales <- c("POS", "AA") #las 2 cajas que son utilizadas de manera fija
cajas_flexibles <- c("RPOS", "scan&go") #las 2 cajas que son utilizadas de manera flexible para los eventos
horas <- c(12, 13, 14, 15, 16, 17, 18, 19, 20) #rango de horas de operación (se acotó para disminuir tiempos de cómputo) (HORA_TRX)
pisos <- -1:6  # pisos del -1 a 6 (PISO) #total de pisos del universo de sucursales 
pisos_sin_cero <- pisos[pisos != 0] #total de pisos excluyendo el cero, que es donde se carga la cap flexible


periodo_enero_octubre <- seq(ymd("2023-01-01"), ymd("2023-10-31"), by = "day") #periodo de enero a octubre (FECHA_TRX)
periodo_noviembre_diciembre <- seq(ymd("2023-11-01"), ymd("2023-12-31"), by = "day") #periodo de noviembre a diciembre (FECHA_TRX)
periodo_evento_diamadre <- seq(ymd("2023-05-11"), by = "day", length.out = 3) #periodo del día del madre que dura 3 días comenzando el 11 de mayo (FECHA_TRX)
periodo_evento_dianiño <- seq(ymd("2023-08-03"), by = "day", length.out = 3) #periodo del día del niño 
periodo_evento_diamayo <- seq(ymd("2023-05-15"), by = "day", length.out = 3) #periodo del día del evento de mediados de mayo 
periodo_evento_marzo <- seq(ymd("2023-03-03"), by = "day", length.out = 3) #periodo del día del evento en marzo
periodo_evento_inicioabril <- seq(ymd("2023-03-30"), by = "day", length.out = 3) #periodo del evento de inicio de abril
periodo_evento_finabril <- seq(ymd("2023-04-28"), by = "day", length.out = 3) #periodo del evento de fines de abril
periodo_evento_mayo <- seq(ymd("2023-05-05"), by = "day", length.out = 3) #periodo del día del evento de mayo 
periodo_evento_diapadre <- seq(ymd("2023-06-15"), by = "day", length.out = 3) #periodo del día del padre

periodo_noviembre <- seq(ymd("2023-11-01"), ymd("2023-11-30"), by = "day") #periodo de noviembre 
periodo_diciembre <- seq(ymd("2023-12-01"), ymd("2023-12-31"), by = "day") #periodo de diciembre





# Combina todos los periodos de eventos dentro del periodo enero-octubre en un único vector
todos_los_eventos <- c(periodo_evento_diamadre, 
                       periodo_evento_dianiño, 
                       periodo_evento_diamayo, 
                       periodo_evento_marzo, 
                       periodo_evento_inicioabril, 
                       periodo_evento_finabril,
                       periodo_evento_mayo,
                       periodo_evento_diapadre
                   
)
# Crear un vector de fechas excluyendo los días de eventos
periodo_enero_octubre_sin_eventos <- setdiff(periodo_enero_octubre, todos_los_eventos)



# Convertir cada elemento del vector a fecha
fechas = as.Date(periodo_enero_octubre_sin_eventos, origin = "1970-01-01")



###################################################################################################################################################################################################################################################################################################################################################################################################################################
#PARÁMETROS
  
costos_fijos <- c(POS = 3000, AA = 1000, RPOS = 250, `scan&go` = 50) #costos fijos de instalación por tipo de caja
costos_variables <- c(POS = 20, AA = 6, RPOS = 20, `scan&go` = 1) #costos variables de operación (remuneración) por tipo de caja (por día)
capacidad_por_hora <- 17  #capacidad de las cajas por tipo, por hora (cantidad de transacciones que puede realizar en 1hr)


#Demanda transaccional
source("C:/Users/vcortesv/programas/codigos R/Demanda_trx.R") #llamado al script que genera data demanda_trx

#ajustar la demanda por un factor, en caso de que se prevea que puede existir un aumento o disminución de la demanda
factor_ajuste <- 1.0
demanda_trx$DEMANDA <- demanda_trx$DEMANDA * factor_ajuste

periodo_total <- as.Date(c(periodo_enero_octubre, periodo_noviembre_diciembre)) #periodo_total = periodo_enero_octubre + periodo_noviembre_diciembre
demanda_trx <- demanda_trx[demanda_trx$FECHA_TRX %in% periodo_total, ]  #todas las fechas están dentro del rango de periodo_total
periodo_total_chars <- as.character(periodo_total) #las fechas de periodo_total y demanda_trx$FECHA_TRX a caracteres para la comparación





###################################################################################################################################################################################################################################################################################################################################################################################################################################
#VARIABLES DE DECISIÓN
  

#las variables de decisión son los "resultados del modelo"

modelo <- MIPModel() 

modelo <- modelo %>% 
  add_variable(x1_constante[c, p], type = "integer", lb = 0, c = cajas_principales, p = pisos_sin_cero) #cantidad de cajas principales a instalar en el periodo de enero a octubre

modelo <- modelo %>% 
  add_variable(x2_constante[c, p], type = "integer", lb = 0, c = cajas_principales, p = pisos_sin_cero) #cantidad de cajas principales a instalar en el periodo de noviembre a diciembre

modelo <- modelo %>% 
  add_variable(x3_flexible_diamadre[c, p], type = "integer", lb = 0, c = cajas_flexibles, p = pisos) #cantidad de cajas flexibles a instalar en el periodo de evento del día de la madre

modelo <- modelo %>% 
  add_variable(x3_flexible_dianiño[c, p], type = "integer", lb = 0, c = cajas_flexibles, p = pisos) #cantidad de cajas flexibles a instalar en el periodo de evento del día del niño

modelo <- modelo %>% 
  add_variable(x3_flexible_diamayo[c, p], type = "integer", lb = 0, c = cajas_flexibles, p = pisos) #cantidad de cajas flexibles a instalar en el periodo de evento denominado diamayo

modelo <- modelo %>% 
  add_variable(x3_flexible_marzo[c, p], type = "integer", lb = 0, c = cajas_flexibles, p = pisos) #cantidad de cajas flexibles a instalar en el periodo de evento de marzo

modelo <- modelo %>% 
  add_variable(x3_flexible_inicioabril[c, p], type = "integer", lb = 0, c = cajas_flexibles, p = pisos) #cantidad de cajas flexibles a instalar en el periodo de evento de inicio de abril

modelo <- modelo %>% 
  add_variable(x3_flexible_finabril[c, p], type = "integer", lb = 0, c = cajas_flexibles, p = pisos) #cantidad de cajas flexibles a instalar en el periodo de evento de fines de abril

modelo <- modelo %>% 
  add_variable(x3_flexible_mayo[c, p], type = "integer", lb = 0, c = cajas_flexibles, p = pisos) #cantidad de cajas flexibles a instalar en el periodo de evento de mayo

modelo <- modelo %>% 
  add_variable(x3_flexible_diapadre[c, p], type = "integer", lb = 0, c = cajas_flexibles, p = pisos) #cantidad de cajas flexibles a instalar en el periodo de evento del día del padre

modelo <- modelo %>% 
  add_variable(x3_flexible_nov[c, p], type = "integer", lb = 0, c = cajas_flexibles, p = pisos) #cantidad de cajas flexibles a instalar en el periodo de evento de noviembre

modelo <- modelo %>% 
  add_variable(x3_flexible_dic[c, p], type = "integer", lb = 0, c = cajas_flexibles, p = pisos) #cantidad de cajas flexibles a instalar en el periodo de evento de diciembre





###################################################################################################################################################################################################################################################################################################################################################################################################################################
#RESTRICCIONES


#A continuación se detallan las restricciones del modelo. La R1 y R2 son para las cajas principales, y de la R3  a la R11 son para las cajas flexibles

  
#convertir los números de fecha a fechas y luego a caracteres en el formato correcto
periodo_enero_octubre_sin_eventos <- as.character(as.Date(periodo_enero_octubre_sin_eventos, origin = "1970-01-01"))

#función para encontrar la demanda basada en el percentil 95
encontrar_demanda_percentil95 <- function(subdata) {
  resultados <- data.frame(Piso = integer(), Caja = character(), DemandaPercentil95 = numeric(), Fecha = as.Date(character()), Hora = integer(), stringsAsFactors = FALSE)
  
  for(piso in unique(subdata$PISO)) {
    for(caja in unique(subdata$TIPO_CAJA)) {
      datos_filtrados <- subdata[subdata$PISO == piso & subdata$TIPO_CAJA == caja, ]
      
      if(nrow(datos_filtrados) > 0 && any(!is.na(datos_filtrados$DEMANDA))) {
        demanda_percentil95 <- quantile(datos_filtrados$DEMANDA, probs = 0.95, na.rm = TRUE)
        idx_max <- which.max(datos_filtrados$DEMANDA)
        
        fecha_max <- datos_filtrados$FECHA_TRX[idx_max]
        hora_max <- datos_filtrados$HORA_TRX[idx_max]
      } else {
        demanda_percentil95 <- NA
        fecha_max <- NA
        hora_max <- NA
      }
      
      resultados <- rbind(resultados, data.frame(Piso = piso, Caja = caja, DemandaPercentil95 = demanda_percentil95, Fecha = fecha_max, Hora = hora_max))
    }
  }
  return(resultados)
}

#preparar los datos para el período enero-octubre sin eventos
subdata_enero_octubre <- demanda_trx[demanda_trx$FECHA_TRX %in% periodo_enero_octubre_sin_eventos, ]
percentiles_enero_octubre <- encontrar_demanda_percentil95(subdata_enero_octubre)

#R1: Satisfacción de la demanda transaccional entre enero y octubre basada en el percentil 95
for(i_p in 1:length(pisos_sin_cero)) {
  p <- pisos_sin_cero[i_p]
  if(!is.na(p)) {
    for(i_c in 1:length(cajas_principales)) {
      c <- cajas_principales[i_c]
      if(!is.na(c)) {
        
        percentiles_filtrados <- percentiles_enero_octubre[percentiles_enero_octubre$Piso == p & percentiles_enero_octubre$Caja == c, ]
        
        if(nrow(percentiles_filtrados) > 0 && any(!is.na(percentiles_filtrados$DemandaPercentil95))) {
          
          demanda_percentil95 <- max(percentiles_filtrados$DemandaPercentil95, na.rm = TRUE)
          
          if(!is.na(demanda_percentil95) && demanda_percentil95 > 0) {
            
            modelo <- modelo %>%
              add_constraint(x1_constante[c, p] * capacidad_por_hora >= ceiling(demanda_percentil95))
          }
        }
      }
    }
  }
}



#preparar los datos para el período nov-dic sin eventos
subdata_nov_dic <- demanda_trx[demanda_trx$FECHA_TRX %in% periodo_noviembre_diciembre, ]

encontrar_demanda_percentil90 <- function(subdata) {
  resultados <- data.frame(Piso = integer(), Caja = character(), DemandaPercentil90 = numeric(), stringsAsFactors = FALSE)
  
  for(piso in unique(subdata$PISO)) {
    for(caja in unique(subdata$TIPO_CAJA)) {
      datos_filtrados <- subdata[subdata$PISO == piso & subdata$TIPO_CAJA == caja, ]
      
      if(nrow(datos_filtrados) > 0 && any(!is.na(datos_filtrados$DEMANDA))) {
        demanda_percentil90 <- quantile(datos_filtrados$DEMANDA, probs = 0.9, na.rm = TRUE)
      } else {
        demanda_percentil90 <- NA
      }
      
      resultados <- rbind(resultados, data.frame(Piso = piso, Caja = caja, DemandaPercentil90 = demanda_percentil90))
    }
  }
  return(resultados)
}

#'subdata_nov_dic' contenga los datos correctos para este período
percentiles_nov_dic <- encontrar_demanda_percentil90(subdata_nov_dic)

#R2: Satisfacción de la demanda transaccional entre noviembre y diciembre basada en el percentil 90
for(i_p in 1:length(pisos_sin_cero)) {
  p <- pisos_sin_cero[i_p]
  if(!is.na(p)) {
    for(i_c in 1:length(cajas_principales)) {
      c <- cajas_principales[i_c]
      if(!is.na(c)) {
        percentiles_filtrados <- percentiles_nov_dic[percentiles_nov_dic$Piso == p & percentiles_nov_dic$Caja == c, ]
        if(nrow(percentiles_filtrados) > 0 && any(!is.na(percentiles_filtrados$DemandaPercentil90))) {
          demanda_percentil90 <- max(percentiles_filtrados$DemandaPercentil90, na.rm = TRUE)
          if(!is.na(demanda_percentil90) && demanda_percentil90 > 0) {
            modelo <- modelo %>%
              add_constraint(x2_constante[c, p] * capacidad_por_hora >= ceiling(demanda_percentil90))
          }
        }
      }
    }
  }
}



#convertir los números de fecha a fechas y luego a caracteres en el formato correcto
periodo_evento_diamadre <- as.character(as.Date(periodo_evento_diamadre, origin = "1970-01-01"))

#preparar los datos para el período dia madre 
subdata_dia_madre <- demanda_trx[demanda_trx$FECHA_TRX %in% periodo_evento_diamadre, ]
percentiles_dia_madre <- encontrar_demanda_percentil95(subdata_dia_madre)

#R3: Satisfacción de la demanda transaccional en el evento día de la madre al percentil 95 
for(i_p in 1:length(pisos)) {
  p <- pisos[i_p]
  if(!is.na(p)) {
    for(i_c in 1:length(cajas_flexibles)) {
      c <- cajas_flexibles[i_c]
      if(!is.na(c)) {
        
        percentiles_filtrados <- percentiles_dia_madre[percentiles_dia_madre$Piso == p & percentiles_dia_madre$Caja == c, ]
        
        if(nrow(percentiles_filtrados) > 0 && any(!is.na(percentiles_filtrados$DemandaPercentil95))) {
          
          demanda_percentil95 <- max(percentiles_filtrados$DemandaPercentil95, na.rm = TRUE)
          
          if(!is.na(demanda_percentil95) && demanda_percentil95 > 0) {
            
            modelo <- modelo %>%
              add_constraint(x3_flexible_diamadre[c, p] * capacidad_por_hora >= ceiling(demanda_percentil95))
          }
        }
      }
    }
  }
}



#convertir los números de fecha a fechas y luego a caracteres en el formato correcto
periodo_evento_dianiño <- as.character(as.Date(periodo_evento_dianiño, origin = "1970-01-01"))

#preparar los datos para el período dia niño
subdata_dia_niño <- demanda_trx[demanda_trx$FECHA_TRX %in% periodo_evento_dianiño, ]
percentiles_dia_niño <- encontrar_demanda_percentil95(subdata_dia_niño)

#R4: Satisfacción de la demanda transaccional en el evento día del niño al percentil 95 
for(i_p in 1:length(pisos)) {
  p <- pisos[i_p]
  if(!is.na(p)) {
    for(i_c in 1:length(cajas_flexibles)) {
      c <- cajas_flexibles[i_c]
      if(!is.na(c)) {
        
        percentiles_filtrados <- percentiles_dia_niño[percentiles_dia_niño$Piso == p & percentiles_dia_niño$Caja == c, ]
        
        if(nrow(percentiles_filtrados) > 0 && any(!is.na(percentiles_filtrados$DemandaPercentil95))) {
          
          demanda_percentil95 <- max(percentiles_filtrados$DemandaPercentil95, na.rm = TRUE)
          
          if(!is.na(demanda_percentil95) && demanda_percentil95 > 0) {
            
            modelo <- modelo %>%
              add_constraint(x3_flexible_dianiño[c, p] * capacidad_por_hora >= ceiling(demanda_percentil95))
          }
        }
      }
    }
  }
}



#convertir los números de fecha a fechas y luego a caracteres en el formato correcto
periodo_evento_diamayo <- as.character(as.Date(periodo_evento_diamayo, origin = "1970-01-01"))

#preparar los datos para el período dia mayo
subdata_dia_mayo <- demanda_trx[demanda_trx$FECHA_TRX %in% periodo_evento_diamayo, ]
percentiles_dia_mayo <- encontrar_demanda_percentil95(subdata_dia_mayo)

#R5: Satisfacción de la demanda transaccional en el evento día mayo al percentil 95 
for(i_p in 1:length(pisos)) {
  p <- pisos[i_p]
  if(!is.na(p)) {
    for(i_c in 1:length(cajas_flexibles)) {
      c <- cajas_flexibles[i_c]
      if(!is.na(c)) {
        
        percentiles_filtrados <- percentiles_dia_mayo[percentiles_dia_mayo$Piso == p & percentiles_dia_mayo$Caja == c, ]
        
        if(nrow(percentiles_filtrados) > 0 && any(!is.na(percentiles_filtrados$DemandaPercentil95))) {
          
          demanda_percentil95 <- max(percentiles_filtrados$DemandaPercentil95, na.rm = TRUE)
          
          if(!is.na(demanda_percentil95) && demanda_percentil95 > 0) {
            
            modelo <- modelo %>%
              add_constraint(x3_flexible_diamayo[c, p] * capacidad_por_hora >= ceiling(demanda_percentil95))
          }
        }
      }
    }
  }
}



#convertir los números de fecha a fechas y luego a caracteres en el formato correcto
periodo_evento_marzo <- as.character(as.Date(periodo_evento_marzo, origin = "1970-01-01"))

#preparar los datos para el período evento marzo
subdata_evento_marzo <- demanda_trx[demanda_trx$FECHA_TRX %in% periodo_evento_marzo, ]
percentiles_evento_marzo <- encontrar_demanda_percentil95(subdata_evento_marzo)

#R6: Satisfacción de la demanda transaccional en el evento días marzo al percentil 95 
for(i_p in 1:length(pisos)) {
  p <- pisos[i_p]
  if(!is.na(p)) {
    for(i_c in 1:length(cajas_flexibles)) {
      c <- cajas_flexibles[i_c]
      if(!is.na(c)) {
        
        percentiles_filtrados <- percentiles_evento_marzo[percentiles_evento_marzo$Piso == p & percentiles_evento_marzo$Caja == c, ]
        
        if(nrow(percentiles_filtrados) > 0 && any(!is.na(percentiles_filtrados$DemandaPercentil95))) {
          
          demanda_percentil95 <- max(percentiles_filtrados$DemandaPercentil95, na.rm = TRUE)
          
          if(!is.na(demanda_percentil95) && demanda_percentil95 > 0) {
            
            modelo <- modelo %>%
              add_constraint(x3_flexible_marzo[c, p] * capacidad_por_hora >= ceiling(demanda_percentil95))
          }
        }
      }
    }
  }
}



#convertir los números de fecha a fechas y luego a caracteres en el formato correcto
periodo_evento_finabril<- as.character(as.Date(periodo_evento_finabril, origin = "1970-01-01"))

#preparar los datos para el período evento finoabril
subdata_evento_finabril <- demanda_trx[demanda_trx$FECHA_TRX %in% periodo_evento_finabril, ]
percentiles_evento_finabril <- encontrar_demanda_percentil95(subdata_evento_finabril)

#R7: Satisfacción de la demanda transaccional en el evento días finabril al percentil 95 
for(i_p in 1:length(pisos)) {
  p <- pisos[i_p]
  if(!is.na(p)) {
    for(i_c in 1:length(cajas_flexibles)) {
      c <- cajas_flexibles[i_c]
      if(!is.na(c)) {
        
        percentiles_filtrados <- percentiles_evento_finabril[percentiles_evento_finabril$Piso == p & percentiles_evento_finabril$Caja == c, ]
        
        if(nrow(percentiles_filtrados) > 0 && any(!is.na(percentiles_filtrados$DemandaPercentil95))) {
          
          demanda_percentil95 <- max(percentiles_filtrados$DemandaPercentil95, na.rm = TRUE)
          
          if(!is.na(demanda_percentil95) && demanda_percentil95 > 0) {
            
            modelo <- modelo %>%
              add_constraint(x3_flexible_finabril[c, p] * capacidad_por_hora >= ceiling(demanda_percentil95))
          }
        }
      }
    }
  }
}



#convertir los números de fecha a fechas y luego a caracteres en el formato correcto
periodo_evento_mayo<- as.character(as.Date(periodo_evento_mayo, origin = "1970-01-01"))

#preparar los datos para el período evento mayo
subdata_evento_mayo <- demanda_trx[demanda_trx$FECHA_TRX %in% periodo_evento_mayo, ]
percentiles_evento_mayo <- encontrar_demanda_percentil95(subdata_evento_mayo)

#R8: Satisfacción de la demanda transaccional en el evento mayo al percentil 95 
for(i_p in 1:length(pisos)) {
  p <- pisos[i_p]
  if(!is.na(p)) {
    for(i_c in 1:length(cajas_flexibles)) {
      c <- cajas_flexibles[i_c]
      if(!is.na(c)) {
        
        percentiles_filtrados <- percentiles_evento_mayo[percentiles_evento_mayo$Piso == p & percentiles_evento_mayo$Caja == c, ]
        
        if(nrow(percentiles_filtrados) > 0 && any(!is.na(percentiles_filtrados$DemandaPercentil95))) {
          
          demanda_percentil95 <- max(percentiles_filtrados$DemandaPercentil95, na.rm = TRUE)
          
          if(!is.na(demanda_percentil95) && demanda_percentil95 > 0) {
            
            modelo <- modelo %>%
              add_constraint(x3_flexible_mayo[c, p] * capacidad_por_hora >= ceiling(demanda_percentil95))
          }
        }
      }
    }
  }
}



#convertir los números de fecha a fechas y luego a caracteres en el formato correcto
periodo_evento_diapadre<- as.character(as.Date(periodo_evento_diapadre, origin = "1970-01-01"))

#preparar los datos para el período evento dia del padre
subdata_evento_diapadre <- demanda_trx[demanda_trx$FECHA_TRX %in% periodo_evento_diapadre, ]
percentiles_evento_diapadre <- encontrar_demanda_percentil95(subdata_evento_diapadre)

#R9: Satisfacción de la demanda transaccional en el evento dia del padre al percentil 95 
for(i_p in 1:length(pisos)) {
  p <- pisos[i_p]
  if(!is.na(p)) {
    for(i_c in 1:length(cajas_flexibles)) {
      c <- cajas_flexibles[i_c]
      if(!is.na(c)) {
        
        percentiles_filtrados <- percentiles_evento_diapadre[percentiles_evento_diapadre$Piso == p & percentiles_evento_diapadre$Caja == c, ]
        
        if(nrow(percentiles_filtrados) > 0 && any(!is.na(percentiles_filtrados$DemandaPercentil95))) {
          
          demanda_percentil95 <- max(percentiles_filtrados$DemandaPercentil95, na.rm = TRUE)
          
          if(!is.na(demanda_percentil95) && demanda_percentil95 > 0) {
            
            modelo <- modelo %>%
              add_constraint(x3_flexible_diapadre[c, p] * capacidad_por_hora >= ceiling(demanda_percentil95))
          }
        }
      }
    }
  }
}



#convertir los números de fecha a fechas y luego a caracteres en el formato correcto
periodo_evento_nov<- as.character(as.Date(periodo_evento_nov, origin = "1970-01-01"))

#preparar los datos para el período evento de noviembre
subdata_evento_nov <- demanda_trx[demanda_trx$FECHA_TRX %in% periodo_evento_nov, ]
percentiles_evento_nov <- encontrar_demanda_percentil95(subdata_evento_nov)

#R10: Satisfacción de la demanda transaccional en el evento de noviembre
for(i_p in 1:length(pisos)) {
  p <- pisos[i_p]
  if(!is.na(p)) {
    for(i_c in 1:length(cajas_flexibles)) {
      c <- cajas_flexibles[i_c]
      if(!is.na(c)) {
        
        percentiles_filtrados <- percentiles_evento_nov[percentiles_evento_nov$Piso == p & percentiles_evento_nov$Caja == c, ]
        
        if(nrow(percentiles_filtrados) > 0 && any(!is.na(percentiles_filtrados$DemandaPercentil95))) {
          
          demanda_percentil95 <- max(percentiles_filtrados$DemandaPercentil95, na.rm = TRUE)
          
          if(!is.na(demanda_percentil95) && demanda_percentil95 > 0) {
            
            modelo <- modelo %>%
              add_constraint(x3_flexible_nov[c, p] * capacidad_por_hora >= ceiling(demanda_percentil95))
          }
        }
      }
    }
  }
}



#convertir los números de fecha a fechas y luego a caracteres en el formato correcto
periodo_evento_dic<- as.character(as.Date(periodo_evento_dic, origin = "1970-01-01"))

#preparar los datos para el período evento de diciembre
subdata_evento_dic <- demanda_trx[demanda_trx$FECHA_TRX %in% periodo_evento_dic, ]
percentiles_evento_dic <- encontrar_demanda_percentil95(subdata_evento_dic)

#R11: Satisfacción de la demanda transaccional en el evento de diciembre
for(i_p in 1:length(pisos)) {
  p <- pisos[i_p]
  if(!is.na(p)) {
    for(i_c in 1:length(cajas_flexibles)) {
      c <- cajas_flexibles[i_c]
      if(!is.na(c)) {
        
        percentiles_filtrados <- percentiles_evento_dic[percentiles_evento_dic$Piso == p & percentiles_evento_dic$Caja == c, ]
        
        if(nrow(percentiles_filtrados) > 0 && any(!is.na(percentiles_filtrados$DemandaPercentil95))) {
          
          demanda_percentil95 <- max(percentiles_filtrados$DemandaPercentil95, na.rm = TRUE)
          
          if(!is.na(demanda_percentil95) && demanda_percentil95 > 0) {
            
            modelo <- modelo %>%
              add_constraint(x3_flexible_dic[c, p] * capacidad_por_hora >= ceiling(demanda_percentil95))
          }
        }
      }
    }
  }
}
###################################################################################################################################################################################################################################################################################################################################################################################################################################
#FUNCIÓN OBJETIVO
  

#cálculo de la función objetivo, en función de la minimización tanto de los costos fijos y costos variables para cada una de las variables de decisión del modelo

modelo <- modelo %>% 
  set_objective(
      # Costos para x1_constante (cajas regulares durante todo el año)
      sum_expr(costos_fijos[c] * x1_constante[c, p], c = cajas_principales, p = pisos_sin_cero) +
      sum_expr(costos_variables[c] * x1_constante[c, p] * length(periodo_enero_octubre), c = cajas_principales, p = pisos_sin_cero) +
      
      # Costos para x2_constante (cajas regulares durante nov y dic)
      sum_expr(costos_fijos[c] * x2_constante[c, p], c = cajas_principales, p = pisos_sin_cero) +
      sum_expr(costos_variables[c] * x2_constante[c, p] * length(periodo_noviembre_diciembre), c = cajas_principales, p = pisos_sin_cero) +
      
      # Costos para x3_flexible_diamadre (cajas flexibles durante el día de la madre)
       sum_expr(costos_fijos[c] * x3_flexible_diamadre[c, p], c = cajas_flexibles, p = pisos) +
       sum_expr(costos_variables[c] * x3_flexible_diamadre[c, p] * length(periodo_evento_diamadre), c = cajas_flexibles, p = pisos) +
    
      # Costos para x3_flexible_dianiño (cajas flexibles durante el día del niño)
      sum_expr(costos_fijos[c] * x3_flexible_dianiño[c, p], c = cajas_flexibles, p = pisos) +
      sum_expr(costos_variables[c] * x3_flexible_dianiño[c, p] * length(periodo_evento_dianiño), c = cajas_flexibles, p = pisos) +
    
     # Costos para x3_flexible_diamayo (cajas flexibles durante el día mayo)
      sum_expr(costos_fijos[c] * x3_flexible_diamayo[c, p], c = cajas_flexibles, p = pisos) +
      sum_expr(costos_variables[c] * x3_flexible_diamayo[c, p] * length(periodo_evento_diamayo), c = cajas_flexibles, p = pisos) +
      
      # Costos para x3_flexible_marzo (cajas flexibles durante el evento en marzo)
      sum_expr(costos_fijos[c] * x3_flexible_marzo[c, p], c = cajas_flexibles, p = pisos) +
      sum_expr(costos_variables[c] * x3_flexible_marzo[c, p] * length(periodo_evento_marzo), c = cajas_flexibles, p = pisos) +
      
      # Costos para x3_flexible_inicioabril (cajas flexibles durante el evento de inicio de abril)
      sum_expr(costos_fijos[c] * x3_flexible_inicioabril[c, p], c = cajas_flexibles, p = pisos) +
      sum_expr(costos_variables[c] * x3_flexible_inicioabril[c, p] * length(periodo_evento_inicioabril), c = cajas_flexibles, p = pisos) +
      
      # Costos para x3_flexible_finabril (cajas flexibles durante el evento de fines de abril)
      sum_expr(costos_fijos[c] * x3_flexible_finabril[c, p], c = cajas_flexibles, p = pisos) +
      sum_expr(costos_variables[c] * x3_flexible_finabril[c, p] * length(periodo_evento_finabril), c = cajas_flexibles, p = pisos) +
      
      # Costos para x3_flexible_mayo (cajas flexibles durante el evento en mayo)
      sum_expr(costos_fijos[c] * x3_flexible_mayo[c, p], c = cajas_flexibles, p = pisos) +
      sum_expr(costos_variables[c] * x3_flexible_mayo[c, p] * length(periodo_evento_mayo), c = cajas_flexibles, p = pisos) +
      
      # Costos para x3_flexible_diapadre (cajas flexibles durante el evento en el día del padre)
      sum_expr(costos_fijos[c] * x3_flexible_diapadre[c, p], c = cajas_flexibles, p = pisos) +
      sum_expr(costos_variables[c] * x3_flexible_diapadre[c, p] * length(periodo_evento_diapadre), c = cajas_flexibles, p = pisos) +
      
      # Costos para x3_flexible_nov (cajas flexibles durante el evento de noviembre)
      sum_expr(costos_fijos[c] * x3_flexible_nov[c, p], c = cajas_flexibles, p = pisos) +
      sum_expr(costos_variables[c] * x3_flexible_nov[c, p] * length(periodo_evento_nov), c = cajas_flexibles, p = pisos) +
      
      # Costos para x3_flexible_dic (cajas flexibles durante el evento de diciembre)
      sum_expr(costos_fijos[c] * x3_flexible_dic[c, p], c = cajas_flexibles, p = pisos) +
      sum_expr(costos_variables[c] * x3_flexible_dic[c, p] * length(periodo_evento_dic), c = cajas_flexibles, p = pisos),
    
    
    sense = "min"  # Minimizar los costos totales
  )





###################################################################################################################################################################################################################################################################################################################################################################################################################################
#EJECUCIÓN MODELO
  

resultado <- tryCatch({
  solve_model(modelo, with_ROI(solver = "glpk", verbose = TRUE))
}, error = function(e) {
  cat("Error al resolver el modelo: ", e$message, "\n")
})



###################################################################################################################################################################################################################################################################################################################################################################################################################################
#RESULTADOS
  
#generación de resultados para cada una de las variables de decisión

resultados_enero_octubre <- get_solution(resultado, x1_constante[c, p]) 
resultados_noviembre_diciembre <- get_solution(resultado, x2_constante[c, p]) 
resultados_dia_madre <- get_solution(resultado, x3_flexible_diamadre[c, p]) 
resultados_dia_niño <- get_solution(resultado, x3_flexible_dianiño[c, p]) 
resultados_dia_mayo <- get_solution(resultado, x3_flexible_diamayo[c, p]) 
resultados_evento_marzo <- get_solution(resultado, x3_flexible_marzo[c, p]) 
resultados_evento_inicioabril <- get_solution(resultado, x3_flexible_inicioabril[c, p]) 
resultados_evento_finabril <- get_solution(resultado, x3_flexible_finabril[c, p]) 
resultados_evento_mayo <- get_solution(resultado, x3_flexible_mayo[c, p])
resultados_evento_diapadre <- get_solution(resultado, x3_flexible_diapadre[c, p]) 
resultados_evento_nov <- get_solution(resultado, x3_flexible_nov[c, p]) 
resultados_evento_dic <- get_solution(resultado, x3_flexible_dic[c, p]) 


#conservar solo las filas cuyo valor en la columna 'p' coincida con algún valor en la lista de pisos únicos de demanda_trx

resultados_enero_octubre <- resultados_enero_octubre[resultados_enero_octubre$p %in% unique(demanda_trx$PISO), ]
resultados_noviembre_diciembre <- resultados_noviembre_diciembre[resultados_noviembre_diciembre$p %in% unique(demanda_trx$PISO), ]
resultados_dia_madre <- resultados_dia_madre[resultados_dia_madre$p %in% unique(demanda_trx$PISO), ]
resultados_dia_niño <- resultados_dia_niño[resultados_dia_niño$p %in% unique(demanda_trx$PISO), ]
resultados_dia_mayo <- resultados_dia_mayo[resultados_dia_mayo$p %in% unique(demanda_trx$PISO), ]
resultados_evento_marzo <- resultados_evento_marzo[resultados_evento_marzo$p %in% unique(demanda_trx$PISO), ]
resultados_evento_inicioabril <- resultados_evento_inicioabril[resultados_evento_inicioabril$p %in% unique(demanda_trx$PISO), ]
resultados_evento_finabril <- resultados_evento_finabril[resultados_evento_finabril$p %in% unique(demanda_trx$PISO), ]
resultados_evento_mayo <- resultados_evento_mayo[resultados_evento_mayo$p %in% unique(demanda_trx$PISO), ]
resultados_evento_diapadre <- resultados_evento_diapadre[resultados_evento_diapadre$p %in% unique(demanda_trx$PISO), ]
resultados_evento_nov <- resultados_evento_nov[resultados_evento_nov$p %in% unique(demanda_trx$PISO), ]
resultados_evento_dic <- resultados_evento_dic[resultados_evento_dic$p %in% unique(demanda_trx$PISO), ]


#observar resultados

print(resultados_enero_octubre) 
print(resultados_noviembre_diciembre)
print(resultados_dia_madre)
print(resultados_dia_niño)
print(resultados_dia_mayo)
print(resultados_evento_marzo)
print(resultados_evento_inicioabril)
print(resultados_evento_finabril)
print(resultados_evento_mayo)
print(resultados_evento_diapadre)
print(resultados_evento_nov)
print(resultados_evento_dic)




