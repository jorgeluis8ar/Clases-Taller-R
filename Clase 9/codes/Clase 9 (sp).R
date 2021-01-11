# Elaborado por: Eduard F. Martinez Gonzalez
# Fecha: 14 de octubre de 2020
# Nota: No se usan acentos ni caracteres especiales para evitar conflictos entre los diferentes sistemas operativos.

#--------------------------#
# 0. Configuración inicial #
#--------------------------#

#### 0.0 Hoy veremos...
rstudioapi::viewer(url = 'Intro-clase-9.html')

#### 0.1 Limpiar la consola, el entorno y fijar directorio de trabajo
cat('\f')
rm(list=ls())
options('scipen'=100, 'digits'=4) # Forzar a R a no usar e+
setwd('~/Dropbox/teaching/Taller de R/GitHub/Clases/Clase 9') # Cambiar este directorio
getwd()

#### 0.2 Instalar las librerias que vamos a usar en la clase de hoy
paquetes = c('tidyverse','sp','raster','rgdal','rgeos','grid','png')
for ( paquete in paquetes){
      if (length(grep(paquete,installed.packages()[,1])) == 0 ){ install.packages(paquete) ; print(paste0('La libreria ', '"' , paquete , '"' , ' ha sido instalada.'))}
      else { print(paste0('La libreria ', '"', paquete ,'"', ' ya está instalada.'))}
      rm(paquete)
}

#### 0.3 Llamar las librerias
sapply(paquetes,require,character.only=T) 

#----------------------#
# 1. Importar archivos #
#----------------------#
"Los datos vectoriales u OGR data (Poligonos, lineas o puntos), se importan a R usando la funcion
readOGR del paquete rgdal"

# Importar un SpatialPolygonDataframe
"Antes de importar el archivo, puedo inspeccionar el Shpaefile usando ogrInfo()"
ogrInfo(dsn = "data/original/mapas",layer = "Colombia_wgs84")
"Importemos el shapefile"
colombia <- readOGR(dsn = "data/original", layer = "Colombia_wgs84")
"Tipo de objeto"
class(colombia)
"Obtener el SRC del objeto"
proj4string(colombia)
colombia@proj4string
"Caja de coordenadas"
colombia@bbox
"Observar los datos"
colombia@data
"Atributos"
colombia %>% str() # Ojo explicar paso a paso cada atributo espacial
"Inspeccionar capa"
plot(colombia[33,]) # Ploteando solo el objeto 33
colombia[33,]@data # Obteniendo la informacion del polignno 33
plot(colombia[30:33,]) # Ploteando solo el objeto 33
colombia[30:33,]@data 

# Importar un SpatialLineDataframe
ogrInfo("data/original/mapas","Viaferrea")
"Observen que no es necesario indicar que es una capa de lineas, sino que se carga
igual a como se cargo el shapefile con los poligonos de los departamentos de Colombia"
vias <- readOGR(dsn = "data/original/mapas/polygon", layer = "Viaferrea")
vias@proj4string
vias[1,] %>% plot()
vias[1,]@data

# Importar datos de lluvias
"Importemos el dataframe que contiene los puntos con su coordenada"
load("data/original/otros/lluvias en bogota.Rdata")

"veamos la base de datos"
View(lluvias)
class(lluvias)
summary(lluvias$x)
summary(lluvias$y)

"Para espacializar informacion, usaremos las coordenadas que estan en la base de datos"
dev.off()
grid.raster(readPNG("help/Espacializar informacion.png")) 
"Observen que aunque no asigne un CRS, se pueden visualizar espacialmente la informacion,
pero no significa que esa sea la ubicacion real de los puntos, veamos:"
coordinates(lluvias) = ~ x + y
proj4string(lluvias)
plot(lluvias)

"Puede pasar que cuando se asigna el CRS bajo el que fueron tomadas las coordenadas,
la ubicacion geografica de los puntos cambie, veamos la imagen"
dev.off()
grid.raster(readPNG("help/Aisgnar SRC.png")) 
"Los datos fueron tomados usando el CRS de Bogota, asi que vamos a tomar ese CRS y se o vamos a
asignar al objeto lluvias"
bogota <- readOGR(dsn = "data/original/mapas", layer = "Bogota")
bogota@proj4string
lluvias@proj4string <- bogota@proj4string
lluvias@proj4string

"Veamos como se ve la capa de puntos, junto con la capa de Bogota"
plot(bogota)
plot(bogota,border="red")
plot(bogota,border="red",col="blue")


"Es importante la opcion add = TRUE, para indicarle a R que quiero plotear los dos objetos en un solo grafico"
plot(bogota,col="red")
plot(lluvias, col = "blue" , alpha = 0.01 , pch=16 , cex=0.4, add=TRUE)  

# Exportar objetos espaciales
writeOGR(obj = lluvias, layer = 'lluvias en Bogota', dsn = 'datos/procesados/mapas', driver="ESRI Shapefile" , overwrite_layer = TRUE)

#-----------------------------------------#
# 3. Atributos de los archivos espaciales #
#----------------------------------------#
"Al igual que con los dataframe a los Spatial...Dataframe tambien se les puedo editar los nombres de las variables"
colnames(lluvias)
colnames(lluvias@data)

"Puedo extraer la base de datos del objeto espacial"
colombia_data <- colombia@data

"Puedo realizar operaciones con las columnas y filas de los dataframe"
table(lluvias$year)
table(lluvias@data$year)
summary(lluvias@data$month)
table(colombia@data$name_dpto)

"Puedo filtrar por caracteristicas de las filas"
cundinamarca <- colombia[colombia$name_dpto == "Cundinamarca",]
plot(cundinamarca)
cundinamarca2 <- colombia[9,]
peques <- colombia[colombia@data$AREA_OFICI <= 20848,]
peques@data

"Puedo seleccionar columnas"
head(colombia@data)
colombia <- colombia[,c(1,3)]
head(colombia@data)

#------------------------------------#
# 4. Visualizar informacion espacial #
#------------------------------------#
"Estos graficos nos permiten visualizar inicialmente la informacion"
plot(cundinamarca,col="green")
plot(cundinamarca,border="green")
plot(bogota,col="gray",add=TRUE)
plot(lluvias,col = "blue" , alpha = 0.01 , pch=16 , cex=0.4, add=TRUE)

#-------------------------------------#
# 5. Reproyectar informacion espacial #
#-------------------------------------#
"Importante antes de plotear o de hacer cualquier operacion a nivel espacial,
se debe verificar, que todos los objetos espaciales tengan la misma proyeccion"
plot(vias)
plot(cundinamarca,col="red",add=TRUE)

"Veamos que esta pasando"
dev.off()
grid.raster(readPNG("help/Proyeccion errada.png")) 

"verificamos la proyeccion de cada una"
proj4string(vias)
proj4string(cundinamarca)

"Llevemos la capa de vias a la proyeccion de "
dev.off()
grid.raster(readPNG("help/Reproyectar.png")) 
vias <- spTransform(vias, CRSobj = cundinamarca@proj4string)

"verificamos la proyeccion de cada una"
proj4string(vias)
proj4string(cundinamarca)

"Volvamos a plotear las capas"
plot(vias,col="blue")
plot(cundinamarca,col="red",add=TRUE)

#--------------------------------#
# 6. Agregar informacion a mapas #
#--------------------------------#
"Carguemos los datos a pegar en el shapefile"
datos_clima <- read.table("data/original/otros/datos_clima.csv",sep = ";",stringsAsFactors = FALSE) %>% 
               .[,2:5] %>% as.data.frame(.)
colnames(datos_clima) <- c("codepto","area","calida","templada")
datos_clima <- datos_clima[2:nrow(datos_clima),]

"Peguemos los datos"
head(colombia@data)
colombia@data$cod1 <- as.numeric(colombia@data$cod_dpto)
colombia@data$cod_dpto <- as.character(colombia@data$cod_dpto) %>% as.numeric()
colombia <- merge(x = colombia , y = datos_clima , by.x = "cod_dpto" , by.y = "codepto" , all.x=TRUE)
head(colombia@data)

"Generemos una variable que sea el porcentaje del area calida y el porcentaje del area templada"
colombia@data <- mutate(colombia@data, p_calida = calida/area*100)
colombia@data

"Oops! no genera variables con character, convirtamos las variables en numeric"
colombia@data$area <- gsub(",",".",colombia@data$area) %>% as.numeric(.)
colombia@data$calida <- gsub(",",".",colombia@data$calida) %>% as.numeric(.)
colombia@data$templada <- gsub(",",".",colombia@data$templada) %>% as.numeric(.)
colombia@data <- mutate(colombia@data, p_calida = calida/area*100)
colombia@data <- mutate(colombia@data, p_templada = templada/area*100)
colombia@data

#-------------------------------------------------#
# 7. ¿Donde encuentro informacion espacial libre? #
#-------------------------------------------------#
"
* datos_abiertos_bogota <- https://datosabiertos.bogota.gov.co/dataset

* sigot <- http://sigotvg.igac.gov.co:8080

* dane <- https://geoportal.dane.gov.co/servicios/descarga-y-metadatos/descarga-mgn-marco-geoestadistico-nacional/

* otras 1 <- https://www.medellin.gov.co/geomedellin/index.hyg

* Otras secretarias tienen informacion espacial online 
"