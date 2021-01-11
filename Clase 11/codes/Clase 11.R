# Elaborado por: Eduard F. Martinez Gonzalez
# Fecha: 28 de octubre de 2020
# Nota: No se usan acentos ni caracteres especiales para evitar conflictos entre los diferentes sistemas operativos.

#--------------------------#
# 0. Configuración inicial #
#--------------------------#

#### 0.0. Hoy veremos...
rstudioapi::viewer(url = 'Intro-clase-11.html')

#### 0.1. Limpiar la consola, el entorno y fijar directorio de trabajo
cat('\f')
rm(list=ls())
options('scipen'=100, 'digits'=4) # Forzar a R a no usar e+
setwd("~/Documents/Universidad/Taller de R/Clases/clase_11")
getwd()

#### 0.2. Instalar las librerias que vamos a usar en la clase de hoy
paquetes = c("tidyverse",'sf','grid','png')
for ( paquete in paquetes){
      if (length(grep(paquete,installed.packages()[,1])) == 0 ){ install.packages(paquete) ; print(paste0('La libreria ', '"', paquete ,'"', ' ha sido instalada.'))}
      else { print(paste0('La libreria ', '"', paquete ,'"', ' ya está instalada.'))}
      rm(paquete)
}

#### 0.3. Llamar las librerias
sapply(paquetes,require,character.only=T) 
rm(paquetes)
browseURL(url = "https://geocompr.robinlovelace.net/geometric-operations.html", browser = getOption("browser")) # Geometric operations 
browseURL(url = "https://github.com/rstudio/cheatsheets/blob/master/sf.pdf", browser = getOption("browser")) # sf package 

#### 0.4. Importar bases de datos
browseURL(url = "https://geoportal.dane.gov.co/servicios/descarga-y-metadatos/descarga-mgn-marco-geoestadistico-nacional/", browser = getOption("browser")) # MGN
manzana = st_read(dsn = 'data/original/MGN_URB_MANZANA.shp')
via = st_read(dsn = 'data/original/VIAS.shp')
toponimia = st_read(dsn = 'data/original/MGN_URB_TOPONIMIA.shp')

browseURL(url = "https://datosabiertos.bogota.gov.co/dataset/unidad-de-planeamiento-bogota-d-c", browser = getOption("browser")) # UPZ 
upz <- st_read(dsn = "data/original/UPZ.shp",stringsAsFactors = F) %>% 
       subset(as.character(.$UPlTipo)=="1" & UPlNombre %in% c('TEUSAQUILLO','CHAPINERO','LA MACARENA','LAS NIEVES','SAGRADO CORAZON')) %>% 
       dplyr::select(UPlNombre,UPlCodigo)

#---------------#
# 1. Distancias #
#---------------#

#### 1.0. Distancia euclidiana vs Distancia Geodesica
browseURL(url = "https://mgimond.github.io/Spatial/coordinate-systems.html", browser = getOption("browser")) # Distancia euclidiana o geodesica
browseURL(url = "http://www.ggspatial.co.uk/geodesic-distances-how-long-is-that-line-again/", browser = getOption("browser")) # Distancia euclidiana o geodesica

#### 1.1. Distancia euclidiana
dev.off()
grid.raster(readPNG('help/geodesic_distance.png'))
dev.off()
grid.raster(readPNG('help/euclidian_distance.png'))

#### 1.2. Cargar bases de datos
points = data.frame(name = c('Universidad de los Andes','Biblioteca Nacional') , long = c(-74.0673157,-74.0706628) , lat = c(4.6024665,4.60959)) %>%
         st_as_sf(x = ., coords = c("long", "lat"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
points
ggplot() + geom_sf(data = upz , col='red') + geom_sf_text(data = upz , aes(label=UPlNombre),size=2) + 
geom_sf(data = points) + geom_sf_text(data = points,aes(label=name),size=2,col='blue') + theme_bw()

#### 1.3. Calcular la distancia
st_distance(points)

#### 1.4. CRS
dev.off()
grid.raster(readPNG('help/diff_crs.png'))
st_distance(points,upz) # Oops!
points=st_transform(points,crs=4686)
upz = st_transform(x = upz,crs = st_crs(points)) # Cambiar el CRS

st_distance(points,upz) # Hagamoslo de nuevo (explicar las filas y columnas)

points
upz

apply(st_distance(points,upz),1,min) # Punto mas cercano

#### 1.5. ruta mas cercana
browseURL(url = "https://docs.ropensci.org/stplanr/", browser = getOption("browser")) 
browseURL(url = "https://rjournal.github.io/archive/2018/RJ-2018-053/RJ-2018-053.pdf", browser = getOption("browser")) 
#stplanr
#---------------#
# 2. Centroides #
#---------------#

#### 2.0. Intuicion
dev.off()
grid.raster(readPNG('help/st_centroid.png'))

#### 2.1. st_centroid
st_centroid(manzana)
c_manzana = st_centroid(x = manzana,of_largest_polygon = T)
c_manzana

#### 2.1.1. Veamos que tenemos
ggplot() + geom_sf(data = manzana , col='red', fill = 'red' , alpha=0.3 , size=0.2) + 
geom_sf(data = c_manzana,size=0.3,col='cyan') + theme_bw()

#### 2.2. Manual
a = upz$geometry[[5]][[1]] %>% data.frame()
summary(a)[4,]
st_centroid(upz)

#-------------#
# 3. Clipping #
#-------------#

#### 3.1. Intuicion
dev.off()
grid.raster(readPNG('help/st_crop.png'))
dev.off()
grid.raster(readPNG('help/st_intersection.png'))
dev.off()
grid.raster(readPNG('help/st_difference.png'))

#### 3.2. Vias en teusaquillo
ggplot() + geom_sf(data = via,col='red',size=0.1) + geom_sf(data = upz[1,],fill=NA,col='black')

#### 3.2.1. Crop
via=st_transform(x = via,crs=st_crs(upz))
crop_via = st_crop(via,upz[1,])
ggplot() + geom_sf(data = crop_via,col='red',size=0.1) + geom_sf(data = upz[1,],fill=NA,col='black')

#### 3.2.2. Intersection
inter_via = st_intersection(via,upz[1,])
ggplot() + geom_sf(data = inter_via,col='blue',size=0.1) + geom_sf(data = upz[1,],fill=NA,col='black')

#### 3.2.3. Crop vs Intersect
ggplot() + geom_sf(data = crop_via,col='red',size=0.1) + 
           geom_sf(data = inter_via,col='blue',size=0.1)

#### 3.4. Diferencias
print('Veremos un ejemplo mas adelante')

#-----------#
# 4. Buffer #
#-----------#

#### 4.1. Intuicion
dev.off()
grid.raster(readPNG('help/st_buffer.png')) # Tomado de https://geocompr.robinlovelace.net/geometric-operations.html#geo-vec

#### 4.2. Buffer
b_teusa = st_buffer(x = upz[1,] , dist = 0.05) # Ojo con el CRS
ggplot() + geom_sf(data = upz[1,],fill=NA,col='black') + geom_sf(data = b_teusa,col='blue',fill=NA)

upz = st_transform(x = upz,crs='+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs')
b_teusa = st_buffer(x = upz[1,] , dist = 200)
ggplot() + geom_sf(data = upz[1,],fill=NA,col='black') + geom_sf(data = b_teusa,col='blue',fill=NA)

#### 4.3. Dejemos solo la zona de influencia
buffer = st_difference(b_teusa,upz[1,])
ggplot() + geom_sf(data = upz[1,],fill=NA,col='black',alpha=0.3) + geom_sf(data = buffer,col='blue',fill='cyan',alpha=0.3)

#----------------------------------#
# 5. Otras operaciones geometricas #
#----------------------------------#

#### 5.1. Disolver un sf 
#Pasar de 5 obejtos a 1
oriente = st_union(x = upz)
ggplot() + geom_sf(data = oriente,fill='gray',col='red',alpha=0.3)

#### 5.2. Largo de un objeto
st_length(via)
via = mutate(via, largo = st_length(via)) # Como variable

st_length(inter_via)
st_length(inter_via) %>% as.numeric() %>% sum()

#### 5.3. Area de un sf
st_area(upz)
upz = mutate(upz , area_upz = st_area(upz)) # Como variable

st_area(upz) %>% as.numeric() %>% sum()

#### 5.4. Agregar info de un sf a otro sf
c_manzana = st_transform(x = c_manzana,crs='+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs')
c_manzana = st_join(x = c_manzana,y = upz)
c_manzana

ggplot() + geom_sf(data = upz,fill=NA,col='black',size=0.2) + geom_sf(data = c_manzana,aes(col=UPlNombre),size=0.1)

#---------------#
# 6. Aplicacion #
#---------------#

print('Intentemos responder la siguiente pregunta: Que variables pueden estar asociadas a la desicion de tomar en arriendo o no un inmueble?')

#### 6.1. Genarar base de datos
set.seed(20201028)
inmuebles = subset(c_manzana, as.logical(1:nrow(c_manzana) %in% sample(x = nrow(c_manzana),size = nrow(c_manzana)*0.3))) %>%
            mutate(arrendado = runif(nrow(.),0,1) %>% round()) 
table(inmuebles$arrendado)

#### 6.1.1 Veamos que tenemos
ggplot() + geom_sf(data = manzana,fill=NA,col='black',size=0.2) + 
geom_sf(data = inmuebles,aes(col=as.character(arrendado)),size=0.4) + theme_bw()

#### 6.2. Agregar covariables
toponimia = st_transform(x = toponimia , crs = '+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs')
cai = subset(toponimia,CSIMBOL %in% c('020202','020204'))
ips = subset(toponimia,CSIMBOL %in% c('021001','021002','021003','021004'))
educ = subset(toponimia,CSIMBOL %in% c('020901','020902','020903'))
finan = subset(toponimia,CSIMBOL == '020501')
vias_p = subset(via,type=='primary')
vias_p = st_transform(x = vias_p , crs = '+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs') 
inmuebles = st_transform(x = inmuebles , crs = '+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs') 

#### 6.2.1. Agregar distancia a vias principales y otros servicios
f_dist = function(n){
         inmueble_n = inmuebles[n,]
         
         # Distancia a vias
         dist_vias = st_distance(inmueble_n,vias_p) %>% as.numeric() %>% min()
         
         # Distancia promedio a IE, IPS, CAI, Financieros
         dist_ie = st_distance(inmueble_n,educ) %>% as.numeric() %>% mean()
         dist_ips = st_distance(inmueble_n,ips) %>% as.numeric() %>% mean()
         dist_finan = st_distance(inmueble_n,finan) %>% as.numeric() %>% mean()
         dist_cai = st_distance(inmueble_n,cai) %>% as.numeric() %>% mean()
         
         df = data.frame(MANZ_CCNCT = inmueble_n$MANZ_CCNCT , dist_vias=dist_vias , dist_ie=dist_ie , dist_finan=dist_finan , dist_cai=dist_cai , dist_ips=dist_ips)
}
covariables = lapply(1:nrow(inmuebles), function(x) f_dist(n = x)) %>% data.table::rbindlist(use.names = T) %>% data.frame()

#### 6.3. Estimaciones
inmuebles = merge(inmuebles,covariables,'MANZ_CCNCT',all.x=T) %>% data.frame()
lm(arrendado ~ dist_vias + dist_ie + dist_finan + dist_cai + dist_ips + as.factor(UPlNombre) , data = inmuebles) %>% summary()


#### Task
print('Como agregar el area de la manzana a cada inmueble?') # Usen st_join()
print('Como agregar informacion de los vecinos?') # Piensen en una funcion en la que usan buffers
