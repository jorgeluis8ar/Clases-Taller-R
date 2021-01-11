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
setwd("~/Documents/Universidad/Taller de R/Clases/clase_9")
getwd()

#### 0.2 Instalar las librerias que vamos a usar en la clase de hoy
paquetes = c('tidyverse','sf','raster','grid','png','osmdata')
for ( paquete in paquetes){
      if (length(grep(paquete,installed.packages()[,1])) == 0 ){ install.packages(paquete) ; print(paste0('La libreria ', '"', paquete ,'"', ' ha sido instalada.'))}
      else { print(paste0('La libreria ', '"', paquete ,'"', ' ya está instalada.'))}
      rm(paquete)
}

#### 0.3 Llamar las librerias
sapply(paquetes,require,character.only=T) 

#------------------------#
# 1. Importar shapefiles #
#------------------------#

#### 1.0. Veamos la intuicion primero
rstudioapi::viewer(url = "help/Intro-a-la-información-espacial.html")

#### 1.0.1. sp vs sf (Edzer Pebesma)
rstudioapi::viewer(url = "help/Applied Spatial Data Analysis with R.pdf") # Libro
browseURL(url = "https://github.com/edzer", browser = getOption("browser")) # Edzer Pebesma

#### 1.0.2. Podemos obtener ayuda adiccional aqui
browseURL(url = 'https://github.com/r-spatial/sf', browser = getOption("browser")) # Paquete SF en GitHub
browseURL(url = 'https://cran.r-project.org/web/packages/sf/sf.pdf', browser = getOption("browser")) # Paquete SF
rstudioapi::viewer(url = "help/sf functions.pdf") # Guia de sf

#### 1.1. Importar un shapefile con Polygonos
quilla = st_read(dsn = 'data/original/mapas/polygon',layer='neigh_barranquilla') 
# No se le debe colocar dar la extension del archivo, R carga los 4 de una vez
quilla # veamos lo que contiene el objeto
class(quilla) # Tipo de objeto
nrow(quilla)

#### 1.1.2. Pintemos el objeto
ggplot() + geom_sf(data = quilla,color='blue') + theme_classic()

#### 1.1.3. Geometria del objeto
quilla$geometry[[2]]
quilla$geometry[[2]][1]
ggplot() + geom_sf(data = quilla[2,],color='red')

#### 1.1.4. Otros atributos
st_bbox(quilla) # Caja de coordenadas
quilla %>% str() # Atributo de cada variable

#### 1.1.5. Obtener el CRS del objeto
st_crs(quilla) 
crs(quilla)
st_crs(quilla)
#### 1.2. Importar un shapefile con Lineas
vias <- st_read(dsn = 'data/original/mapas/line', layer = 'Viaferrea') # Observen que no es necesario indicar que es una capa de lineas
vias
class(vias)
ggplot() + geom_sf(data = vias,color='black')+theme_bw()

#### 1.3. Importar un shapefile con puntos
points <- st_read(dsn = 'data/original/mapas/point', layer = 'points_barranquilla',stringsAsFactors=F) # La opcion stringsAsFactors a veces es necesaria
points
class(points)
ggplot() + geom_sf(data = points[1:50,],color='black') + theme_bw()

#-------------------------#
# 2. Generar un shapefile #
#-------------------------#

#### 2.1. Convertir un dataframe con puntos en un objeto sf
dev.off()
grid.raster(readPNG('help/graphs/Espacializar informacion.png')) 
browseURL(url = 'https://www.google.es/maps/?hl=es', browser = getOption("browser")) # Google maps

#### 2.1.1. Creando dataframe
data = data.frame(name = c('Universidad de los Andes','Portal del Norte') , long = c(-74.0673157,-74.0534864) , lat = c(4.6024665,4.7561092))

#### 2.1.2. Convertir de dataframe a sf
data_sf <- st_as_sf(x = data, coords = c("long", "lat"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#### 2.1.3. Veamos que tenemos
#leaflet() %>% addTiles() %>% addPolygons(data = data_sf)
ggplot() + geom_sf(data = st_read(dsn = 'data/original/mapas/polygon/',layer = 'sec_bogota'),color='#7fc0ff') + 
           geom_sf(data = data_sf,color = 'red') + theme_bw()

#### 2.1.4. Exportar un shpefile
st_write(obj = data_sf,dsn = 'data/procesada/mapas/Puntos Bogota.shp',driver='ESRI Shapefile',delete_layer = T)
saveRDS(object = data_sf,file = 'data/procesada/mapas/Puntos Bogota.rds') 

#---------------------#
# 3. Reasignar un CRS #
#---------------------#

#### 3.0. Veamos la intuicion
dev.off()
grid.raster(readPNG('help/graphs/Proyeccion errada.png')) 

#### 3.1. Veamos un ejemplo
cundinamarca = st_read(dsn = 'data/original/mapas/polygon/',layer = 'Colombia_wgs84',stringsAsFactors=F) %>% 
               subset(name_dpto == 'Cundinamarca')
cundinamarca
ggplot() + geom_sf(data = cundinamarca,color='gray')

#### 3.2. Veamos las CRS de cada objeto
st_crs(vias)
st_crs(cundinamarca)


#### 3.3. Cambiemos la CRS de vias
dev.off()
grid.raster(readPNG('help/graphs/Reproyectar.png')) 
vias$geometry[[1]]
vias <- st_transform(x = vias,crs = crs(cundinamarca))
vias$geometry[[1]]

#### 3.4. Veamos nuevamente las CRS de cada objeto
crs(vias)
crs(cundinamarca)

#### 3.5. Vias en ferreas en Cundinamarca
ggplot() + geom_sf(data = cundinamarca,color='gray') + geom_sf(data = vias,color='red') +theme_bw()



