"______________________________ Control de calidad de Datos ______________________________"
# Autor: Jonnathan Landi
# Fecha de creación: 2023-10-05
# Fecha de modificación: 2023-10-05
# ----------------------------------------------------------------------------------------
# Objetivo del Script
# crear un control de calidad de datos, en los cuales se cumpla los siguientes condiciones:
# - (1) Importar archivos de diferentes formatos (csv, txt, dat)
# - (2) Eliminar Fechas duplicados (Eliminar toda la fila si la fecha esta duplicada)
# - (3) Eliminar datos con fechas NA (Eliminar toda la fila si la fecha tiene campos Na)
# - (4) Colocar encabezado con los nombres de las variables en el formato establecido
# - (5) Agrupar los datos cada 5 minutos
# - (6) Colocar NA en aquellos campos que desborden los rangos establecidos
# - (7) La variable máxima no puede ser < mínima y la media no puede ser < mínima ni > máxima
# - (8) VER LA POSIBILIDAD DE EJECUTAR LAS DIFERENTES ESTACIONES POR SEPARADO
# ----------------------------------------------------------------------------------------

##########################################################################################

# Librerías necesarias -------------------------------------------------------------------
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(tools)
library(svDialogs)
##########################################################################################

# Objetivo 1 ------------------------------------------------------------------------
Control_calidad = function() {
  sms = dlg_message("Seleccione el archivo a importar", dlgtype = "ok")
  Data = dlg_open(title = "Selecione el archivo",default = getwd())$res
  extension = file_ext(Data)
  nombre = file_path_sans_ext(basename(Data))
  assign("nombre", nombre, envir = .GlobalEnv)
  
  if (extension %in% c("dat", "txt")) {
    Stat_Name  = fread(paste(Data, sep = "/"),     
                       header = TRUE, skip = 1, fill = TRUE, sep = ",") # modificar skip = 0 en caso de errores
  } else {
    Stat_Name = fread(paste(Data, sep = "/"), sep = ",")
  }
  
  if (!("TIMESTAMP" %in% colnames(Stat_Name))) {
    colnames(Stat_Name)[1] = "TIMESTAMP"
  }
  
  Stat_Name = Stat_Name %>% mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S"))
  
  VvEr = c("TS", "RN", "Avg", "%")
  VeDa = c(t(head(Stat_Name, n = 2)))
  if (any(VvEr %in% VeDa)) {            # elimino filas con valores que no sean numéricos
    Stat_Name = Stat_Name[-(1:2),]
  }
  
  Stat_Name = Stat_Name[order(Stat_Name$TIMESTAMP),] # ordeno en función de la fecha

# Objetivo 2 -----------------------------------------------------------------------------
  Stat_Name = distinct(Stat_Name, TIMESTAMP, .keep_all = TRUE) # Elimino datos duplicados

# Objetivo 3 -----------------------------------------------------------------------------
  Stat_Name = Stat_Name %>% filter(!is.na(TIMESTAMP)) # elimino filas con fechas Na
  
# Objetivo 4 -----------------------------------------------------------------------------
  # BUGS PRESENTES, ALGORITMO EN DESARROLLO  
  # nombres_columnas = colnames(Stat_Name)
  # tipo_1 = c("TempAire_Max", "TempAire_Min", "TempAire_Avg")
  # tipo_2 = c("temperature_Max", "temperature_Min", "temperature_Avg")
  # nuevos_nombres_columnas = c("TIMESTAMP", "TempAire_Max", "TempAire_Min", "TempAire_Avg")
  # 
  # for (i in seq_along(nuevos_nombres_columnas)) {
  #   old_name = colnames(Stat_Name)[i]  # Nombre actual
  #   if (old_name %in% tipo_2) {
  #     colnames(Stat_Name)[colnames(Stat_Name) == old_name] <- nuevos_nombres_columnas[i]  # Renombrar
  #     Temp_max = "TempAire_Max"
  #     Temp_min = "TempAire_Min"
  #     Temp_Avg = "TempAire_Avg"
  #   }
  # }
  # 
  # if (any(tipo_1 %in% nombres_columnas)) {
  #   Temp_max = "TempAire_Max"
  #   Temp_min = "TempAire_Min"
  #   Temp_Avg = "TempAire_Avg"
  #   
  # } else if (any(tipo_2 %in% nombres_columnas)) {
  #   Temp_max = "temperature_Max"
  #   Temp_min = "temperature_Min"
  #   Temp_Avg = "temperature_Avg"
  # } 
  # 
  # assign("Stat_Name", Stat_Name, envir = .GlobalEnv)
  Stat_Name = Stat_Name %>% select(TIMESTAMP, TempAire_Max, TempAire_Avg, TempAire_Min)
  Stat_Name[,2:4] <- lapply(Stat_Name[,2:4], as.numeric)
# Objetivo 6 -----------------------------------------------------------------------------
  Ind_desbordamiento = which(Stat_Name$TempAire_Max > 45 | Stat_Name$TempAire_Max  < -10 |
                               Stat_Name$TempAire_Min > 45 | Stat_Name$TempAire_Min  < -10 |
                               Stat_Name$TempAire_Avg > 45 | Stat_Name$TempAire_Avg  < -10)
  
  Ruido = Stat_Name[Ind_desbordamiento, ]
  assign("Data.fuera.rango ", Ruido, envir = .GlobalEnv)
  
  # Colocamos Na en los campos que desborden los rangos establecidos
  Stat_Name = Stat_Name %>%
    mutate(
      TempAire_Max = ifelse(TempAire_Max > 45 | TempAire_Max < -10, NA, TempAire_Max),
      TempAire_Min = ifelse(TempAire_Min > 45 | TempAire_Min < -10, NA, TempAire_Min),
      TempAire_Avg = ifelse(TempAire_Avg > 45 | TempAire_Avg < -10, NA, TempAire_Avg))

# Objetivo 7 -----------------------------------------------------------------------------
  Ind_erroneos = which(Stat_Name$TempAire_Max < Stat_Name$TempAire_Min | 
                         Stat_Name$TempAire_Max < Stat_Name$TempAire_Avg |
                         Stat_Name$TempAire_Max == Stat_Name$TempAire_Min |
                         Stat_Name$TempAire_Max == Stat_Name$TempAire_Avg |
                         
                         Stat_Name$TempAire_Min == Stat_Name$TempAire_Avg |
                         Stat_Name$TempAire_Min > Stat_Name$TempAire_Avg |
                         Stat_Name$TempAire_Min == Stat_Name$TempAire_Max |
                         Stat_Name$TempAire_Min > Stat_Name$TempAire_Max |
                         
                         Stat_Name$TempAire_Avg == Stat_Name$TempAire_Max |
                         Stat_Name$TempAire_Avg == Stat_Name$TempAire_Min |
                         Stat_Name$TempAire_Avg < Stat_Name$TempAire_Min |
                         Stat_Name$TempAire_Avg > Stat_Name$TempAire_Max)
  
  Ruido.2 = Stat_Name[Ind_erroneos, ]
  assign("Data.Erroneos", Ruido.2, envir = .GlobalEnv)
  
  # Colocamos Na en los campos que desborden los rangos establecidos
  Stat_Name = Stat_Name %>%
    mutate(
      TempAire_Max = ifelse(Stat_Name$TempAire_Max < Stat_Name$TempAire_Min | 
                              Stat_Name$TempAire_Max < Stat_Name$TempAire_Avg |
                              Stat_Name$TempAire_Max == Stat_Name$TempAire_Min |
                              Stat_Name$TempAire_Max == Stat_Name$TempAire_Avg, NA, TempAire_Max),
      TempAire_Min = ifelse(Stat_Name$TempAire_Min == Stat_Name$TempAire_Avg |
                              Stat_Name$TempAire_Min > Stat_Name$TempAire_Avg |
                              Stat_Name$TempAire_Min == Stat_Name$TempAire_Max |
                              Stat_Name$TempAire_Min > Stat_Name$TempAire_Max, NA, TempAire_Min),
      TempAire_Avg = ifelse(Stat_Name$TempAire_Avg == Stat_Name$TempAire_Max |
                              Stat_Name$TempAire_Avg == Stat_Name$TempAire_Min |
                              Stat_Name$TempAire_Avg < Stat_Name$TempAire_Min |
                              Stat_Name$TempAire_Avg > Stat_Name$TempAire_Max, NA, TempAire_Avg))
  
# Objetivo 5 -----------------------------------------------------------------------------
  rdate = ceiling_date(x = Stat_Name$TIMESTAMP, unit = "5 minutes")
  temp = cbind(rdate, Stat_Name)
  names(temp)[1] = "TIMESTAMP_5"
  
  assign("Rangos_Agrupa", temp , envir = .GlobalEnv)
  
  Stat_Name_5min = aggregate(x = list(temp$TempAire_Max, temp$TempAire_Avg, temp$TempAire_Min), 
                             by = list(temp$TIMESTAMP_5), 
                             FUN = mean,  na.rm = TRUE)
  
  colnames(Stat_Name_5min) = c("TIMESTAMP", "TempAire_Max", "TempAire_Avg", "TempAire_Min") # Cambiar nombre de encabezado
  
  assign("Stat_Name_5min", Stat_Name_5min, envir = .GlobalEnv)
  
# Guardar_Archivo ------------------------------------------------------------------------
  nombre_archivo = file_path_sans_ext(basename(Data))
  Data_Save = "direccion donde se va a guardar los archivos"
  Stat_Name_5min$TIMESTAMP =  as.character(Stat_Name_5min$TIMESTAMP)
  Stat_Name_5min$TIMESTAMP[!grepl(":", Stat_Name_5min$TIMESTAMP)] <- paste(Stat_Name_5min$TIMESTAMP[!grepl(":", Stat_Name_5min$TIMESTAMP)], "00:00:00")
  write.csv(Stat_Name_5min, file = file.path(Data_Save, paste(nombre_archivo, ".csv", sep = "")), row.names = FALSE)
  
} # cierra la funcion


Inicio = Control_calidad ()


