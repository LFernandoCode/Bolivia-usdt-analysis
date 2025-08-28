# Autor: Luis Fernando Flores
# Fecha: 25/07/2025
# Descripción: Este script obtiene las cotizaciones de USDT desde la página 
#              "https://usdtbol.com/" mediante web scraping. 
#              Crea una base de datos con fechas y precios, calcula el RSI de 14 
#              periodos y genera gráficos del RSI y del precio en Bs/USDT, incluyendo 
#              líneas de sobrecompra y sobreventa.

##################################################################################
#                                   LIBRERÍAS                                    #
##################################################################################
library(jsonlite)
library(httr)
library(writexl)
library(TTR)
library(tidyverse)

##################################################################################
#                                   CONFIGURACIÓN                                #
##################################################################################
#dirección dash 
res <- GET(
      url = "https://usdtbol.com/api/41j11x65l36h?callback=jQuery36009479087033522224_1744686594356&_=1744686594357",
      add_headers(
            `accept` = "text/javascript, application/javascript, application/ecmascript, application/x-ecmascript, */*; q=0.01",
            `accept-language` = "en-US,en;q=0.9,es;q=0.8",
            `priority` = "u=1, i",
            `referer` = "https://usdtbol.com/",
            `sec-ch-ua` = '"Google Chrome";v="135", "Not-A.Brand";v="8", "Chromium";v="135"',
            `sec-ch-ua-mobile` = "?0",
            `sec-ch-ua-platform` = '"Windows"',
            `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/135.0.0.0 Safari/537.36"
      ),
      set_cookies(
            `PHPSESSID` = "hruf6e6g8e42f1hfuol1lq8mdj",
            `_ga` = "GA1.1.1450998458.1744684017",
            `_ga_22FMPXJDWD` = "GS1.1.1744684017.1.1.1744686593.0.0.0"
      )
)

##################################################################################
#                                  EXTRACCIÓN DE DATOS                             #
##################################################################################

texto <- content(res, "text")

json_str <- sub("^[^(]*\\((\\{.*\\})\\);?$", "\\1", texto)  
datos <- fromJSON(json_str)

##################################################################################
#                               FORMATEO Y BASE DE DATOS                         #
##################################################################################
#Para convertir al formato hora y crear base 
tiempos <- datos$times
fechas <- as.POSIXct(tiempos, origin = "1970-01-01", tz = "UTC")
cotizacion <- datos$list

base <- data.frame(
      fechas=fechas,
      cotizacion=cotizacion
)

#exportar como excel(cambiar la dirección)
#write_xlsx(base,"DIRECCIÓN EN LA PC/usdt.xlsx") 

##################################################################################
#                               PROCESAMIENTO DE DATOS                           #
##################################################################################
bsh <- base %>% 
      mutate(fechash=as.Date(fechas))
maximos <- bsh %>% 
      group_by(fechash) %>% 
      filter(cotizacion==max(cotizacion,na.rm=TRUE)) %>% 
      ungroup()

rsi <- RSI(maximos$cotizacion,n=14, wilder=FALSE)
brsi <- data.frame(fecha = maximos$fechash[14:length(maximos$fechas)], rsi = rsi[14:length(rsi)])

"TABLA 1" <- data.frame()

##################################################################################
#                                   GRAFICOS                                   #
##################################################################################
#PARA GRAFICAR SOLO RSI
plot(maximos$fechas, rsi, type = "l", col = "blue", main = "RSI (n=14)", ylab = "RSI", xlab = "", xaxt = "n")

# Añadir las etiquetas personalizadas de los meses
axis(1, at = seq(min(maximos$fechas), max(maximos$fechas), by = "month"), 
     labels = format(seq(min(maximos$fechas), max(maximos$fechas), by = "month"), "%b %d"), 
     las = 2)  # "las = 2" para rotar las etiquetas

# Añadir las líneas de sobrecompra y sobreventa
abline(h = 70, col = "red", lty = 2)  # Línea de sobrecompra
abline(h = 30, col = "green", lty = 2)  # Línea de sobreventa

# Divide la ventana en 2 filas, 1 columna
par(mfrow = c(2, 1))

# Primer gráfico: RSI
plot(maximos$fechas, rsi, type = "l", col = "blue", main = "RSI (n=14)", 
     ylab = "RSI", xlab = "", xaxt = "n")
axis(1, at = seq(min(maximos$fechas), max(maximos$fechas), by = "month"), 
     labels = format(seq(min(maximos$fechas), max(maximos$fechas), by = "month"), "%b %d"), las = 2)
abline(h = 70, col = "red", lty = 2)
abline(h = 30, col = "green", lty = 2)

# Segundo gráfico: base
plot(base, type = "l", main = "Bs/USDT",  ylab = "Bs", xlab = "", xaxt = "n")
axis(1, at = seq(min(maximos$fechas), max(maximos$fechas), by = "month"), 
     labels = format(seq(min(maximos$fechas), max(maximos$fechas), by = "month"), "%b %d"), las = 2)
