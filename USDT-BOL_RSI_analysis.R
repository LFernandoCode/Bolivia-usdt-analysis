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
cotización <- datos$list

base <- data.frame(
      fechas=fechas,
      cotización=cotización
)

##################################################################################
#                               PROCESAMIENTO DE DATOS                           #
##################################################################################
bsh <- base %>% 
      mutate(fechash=as.Date(fechas))
maximos <- bsh %>% 
      group_by(fechash) %>% 
      slice_max(order_by = cotización, n = 1, with_ties = FALSE) %>% 
      ungroup()


rsi <- RSI(maximos$cotización,n=14)
brsi <- data.frame(fecha = maximos$fechash[15:length(maximos$fechas)], rsi = rsi[15:length(rsi)])
   
# AJUSTE DE BASES 
maximos <- maximos %>% 
      rename(fecha = fechash)
base2 <- left_join(brsi,maximos, by= "fecha")  

#exportar como excel(cambiar la dirección)
#write_xlsx(base2,"DIRECCIÓN EN LA PC/usdt.xlsx")

##################################################################################
#                                   GRAFICOS                                   #
################################################################################## 

while (dev.cur() > 1) dev.off()

#                 Grafico RSI
ggplot(base2, aes(x = fecha, y = rsi)) +
      geom_line(color = "#2C77BF", size = 1.2) +
      geom_hline(yintercept = 70, linetype = "solid", color = "red") +
      geom_hline(yintercept = 30, linetype = "solid", color = "green") +
      labs(title = "",
           x = "", y = "RSI") +
      scale_x_date(date_labels = "%d-%b-%y", date_breaks = "3 weeks") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))


#                 Grafico cotización
ggplot(base2, aes(x = fecha, y = cotización)) +
      geom_line(color = "#1f77b4", size = 1.0) +
      labs(title = "",
           subtitle = "",
           x = NULL, y = "Bolivianos (Bs)") +
      scale_x_date(date_labels = "%d-%b-%y", date_breaks = "3 weeks") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
      theme_minimal(base_size = 14) +   
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

