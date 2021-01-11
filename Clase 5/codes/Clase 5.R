# Elaborado por: Eduard F. Martinez Gonzalez
# Fecha: 9 de septiembre de 2020
# Nota: No se usan acentos ni caracteres especiales para evitar conflictos entre los diferentes sistemas operativos.

#--------------------------#
# 0. Configuración inicial #
#--------------------------#

#### 0.0 Hoy veremos...
rstudioapi::viewer(url = "Intro-clase-5.html")

#### 0.1 Limpiar la consola, el entorno y fijar directorio de trabajo
cat("\f")
rm(list=ls())
setwd("~/Downloads/Universidad/Taller de R/Clases/clase_5")
getwd()

#### 0.2 Instalar las librerias que vamos a usar en la clase de hoy
for ( paquete in c('tidyverse','reshape2','data.table') ){
      if (length(grep(paquete,installed.packages()[,1])) == 0 ){ install.packages(paquete) ; print(paste0("La libreria ", "'", paquete ,"'", " ha sido instalada."))}
      else { print(paste0("La libreria ", "'", paquete ,"'", " ya esta instalada."))}
      rm(paquete)
}

#### 0.3 Llamar las librerias
library('tidyverse') ; library('reshape2') ; library('data.table')

#------------------------------#
# 1. Transponer bases de datos #
#------------------------------#

#### 1.0. Veamos la intuicion primero
rstudioapi::viewer(url = "help/Help-Reshape.html")

#### 1.1. Podemos obtener ayuda adiccional aqui
browseURL(url = "http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/", browser = getOption("browser")) # Paquete reshape2
browseURL(url = "https://seananderson.ca/2013/10/19/reshape/", browser = getOption("browser")) # Paquete reshape2
browseURL(url = "https://cloud.r-project.org/web/packages/data.table/vignettes/datatable-reshape.html", browser = getOption("browser")) # Paquete data.table

#### 1.2. Cargar e inspeccionar las bases de datos
browseURL(url = "https://estadisticas.cepal.org/cepalstat/tabulador/ConsultaIntegrada.asp?", browser = getOption("browser")) # Fuente de los datos
ocupa = readRDS(file = 'data/original/tasa ocupados por sexo.rds') # Ojo esto es la tasa de ocupacion
summary(ocupa)
table(ocupa$country)
table(ocupa$country,is.na(ocupa$tasa)) # el segundo argumento tiene en cuenta la segunda variable
# contra la que se va a tabular

#### 1.3 Long a wide
ocupa_wide = reshape2::dcast(data = ocupa, formula =  country + year ~ clase , value.var="tasa")
View(ocupa_wide)

#### 1.4 Wide a long
parlamento = readxl::read_excel(path = 'data/original/mujeres_parlamento.xlsx')
summary(parlamento)

parlamento_long = reshape2::melt(data = parlamento, id.vars = c('country') , value.name="mujeres")
View(parlamento_long)

#### 1.4.1 Veamos la diferencia entre hombres y mujeres
parlamento_long = mutate(parlamento_long, hombres = 100 - mujeres)
colnames(parlamento_long)[2]='year'
'Veamos la distribucion'
summary(parlamento_long$hombres) # La mediana!!!

"Mediana de los ultimos 10 years en los paises de america latina"
parlamento_long %>% group_by(country) %>% summarize(m_hombres = median(hombres) , m_mujeres = median(mujeres))
results=parlamento_long %>% group_by(country) %>% summarize(m_hombres = median(hombres) , m_mujeres = median(mujeres))
parlamento_long %>% group_by(country) %>% summarize(m_hombres = median(hombres) , m_mujeres = median(mujeres)) %>% sort(decreasing = T)
results=results[order(results$m_hombres,decreasing = T),]
"Promedio por year"
parlamento_long %>% group_by(year) %>% summarize(m_hombres = mean(hombres) , m_mujeres = mean(mujeres))

#### 1.5.1 Hagamos el mismo ejercicio pero para tasa de desocupacion

"Promedio de los ultimos 10 yeasr en los paises de america latina"
ocupa_wide %>% group_by(country) %>% summarize(m_hombres = median(hombres) , m_mujeres = median(mujeres))

"inspeccionemos los NA"
table(ocupa_wide$country,is.na(ocupa_wide$hombres)) %>% addmargins(.,2) 
table(ocupa_wide$country,is.na(ocupa_wide$mujeres)) %>% addmargins(.,2) 

'Eliminemos los NA y hagamoslo de nuevo'
descript = subset(ocupa_wide,is.na(mujeres) == F)
descript %>% group_by(country) %>% summarize(m_hombres = median(hombres) , m_mujeres = median(mujeres))

#----------------#
# 2. Fechas en R #
#----------------#

#### Limpiar el entorno
rm(list = ls())

#### 2.0. Funciones para inspeccionar
is.Date <- function(x) inherits(x, "Date")
is.POSIXct <- function(x) inherits(x, "POSIXct")

#### 2.1. Cargar la base de datos
browseURL(url = "https://www.policia.gov.co/grupo-información-criminalidad/estadistica-delictiva", browser = getOption("browser")) # Fuente de los datos
siedco = readRDS(file = "data/original/SIEDCO.rds")
str(siedco)

#### 2.2 Inspeccionemos los datos
is.character(siedco$fecha)
is.POSIXct(siedco$fecha_1)
is.Date(siedco$fecha_2)

#### 2.3 Convertir de character a fecha
siedco  = siedco %>% mutate(fecha_3 = as.POSIXct(fecha, format = "%Y-%m-%d", tz = "UTC"), # De Character a POSIXct
                            fecha_4 = as.Date(fecha, "%Y-%m-%d")) # De Character a Date
str(siedco)

#### 2.4 Convertir de fecha a character
siedco  = siedco %>% mutate(fecha_5 = as.character(fecha_1, format = "%Y-%m-%d"), # De POSIXct a Character
                            fecha_6 = as.character(fecha_2, format = "%Y-%m-%d")) # De Date a Character
is.character(siedco$fecha_5)
is.character(siedco$fecha_6)

### 2.5 Extraer el year y el mes de una variable de fecha
siedco  = siedco %>% mutate(year_month = format(as.Date(fecha_1), "%Y-%m"), # Extraer el mes y el year
                            year = format(as.Date(fecha_1), "%Y"),
                            month = format(as.Date(fecha_1), "%m")) # Extraer el year

#### 2.6 Operaciones entre fechas
as.Date(x = "2020-09-09","%Y-%m-%d") %>% as.numeric()
siedco  = siedco %>% mutate(diferencia = 18514 - as.numeric(fecha_2))


