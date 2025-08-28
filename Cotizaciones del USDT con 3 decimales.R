

# Autor: Luis Fernado Flores
# Fecha: 25/07/2025
# Descripción: Script para obtener datos del USDT desde la página "https://www.dolarbluebolivia.click/"
#              y calcular promedios con tres decimales en distintos periodos.

##################################################################################
#                                   LIBRERÍAS                                    #
##################################################################################
library(dplyr)
library(lubridate)
library(readxl)
library(writexl)
library(readr)
library(httr)



##################################################################################
#                                   DATOS                                        #
##################################################################################
#OPCIÓN 1 DESDE LA PÁGINA

url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vR2lRBAKrqBFtv_Y8glwaBq28banI80eg3wTOE9Y63LR8iVOjVhpxS3dpeBiqREYM3z1TgA0fdg_h7B/pub?gid=0&single=true&output=csv"

df <- read_delim(
  url,
  delim = ",",
  locale = locale(decimal_mark = ","),
  skip = 2,
  col_names = c("fecha", "oficial_compra", "oficial_venta", "usdtBuy", "usdtSell"),
  quote = '"',
  trim_ws = TRUE
)
df <- df %>%
  mutate(fecha = dmy_hms(fecha))



#OPCIÓN 2 DESDE EL ARCHIVO CVS DESCARGADO

#df <- read_excel("Dirección/DATABASE D.xlsx")
#colnames(df) <- c("fecha", "usdtBuy", "usdtSell")



##################################################################################
#                          PROCESAMIENTO DE DATOS                                #
##################################################################################
# Agrupación por día
serie_dia <- df %>%
  mutate(grupo = floor_date(fecha, "day")) %>%
  group_by(grupo) %>%
  summarise(
    promedio_buy = round(mean(usdtBuy, na.rm = TRUE), 3),
    promedio_sell = round(mean(usdtSell, na.rm = TRUE), 3)
  )

# Agrupación por 4 horas
serie_4h <- df %>%
  mutate(grupo = floor_date(fecha, "4 hours")) %>%
  group_by(grupo) %>%
  summarise(
    promedio_buy = round(mean(usdtBuy, na.rm = TRUE), 3),
    promedio_sell = round(mean(usdtSell, na.rm = TRUE), 3)
  )

# Agrupación por 30 minutos
serie_30min <- df %>%
  mutate(grupo = floor_date(fecha, "30 minutes")) %>%
  group_by(grupo) %>%
  summarise(
    promedio_buy = round(mean(usdtBuy, na.rm = TRUE), 3),
    promedio_sell = round(mean(usdtSell, na.rm = TRUE), 3)
  )

# Agrupación por 15 minutos
serie_15min <- df %>%
  mutate(grupo = floor_date(fecha, "15 minutes")) %>%
  group_by(grupo) %>%
  summarise(
    promedio_buy = round(mean(usdtBuy, na.rm = TRUE), 3),
    promedio_sell = round(mean(usdtSell, na.rm = TRUE), 3)
  )


##################################################################################
#                          Exportar como Excel                                   #
##################################################################################

lista_series <- list(
  "Por Día"        = serie_dia,
  "Cada 4 Horas"   = serie_4h,
  "Cada 30 Min"    = serie_30min,
  "Cada 15 Min"    = serie_15min
)

# Exportar todas las hojas en un único archivo Excel
write_xlsx(lista_series, "Dirección/datos_usdt.xlsx")
