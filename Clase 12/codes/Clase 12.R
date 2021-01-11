# Elaborado por: Eduard F. Martinez Gonzalez
# Fecha: 4 de noviembre de 2020
# Nota: No se usan acentos ni caracteres especiales para evitar conflictos entre los diferentes sistemas operativos.

#--------------------------#
# 0. Configuración inicial #
#--------------------------#

#### 0.1. Limpiar la consola, el entorno y fijar directorio de trabajo
cat('\f')
rm(list=ls())
options('scipen'=100, 'digits'=4) # Forzar a R a no usar e+
setwd("~/Documents/Universidad/Taller de R/Clases/clase_12")
getwd()

#### 0.2. Instalar las librerias que vamos a usar en la clase de hoy
paquetes = c('tidyverse','rgdal','sf','sp','raster','viridis')
for ( paquete in paquetes){
      if (length(grep(paquete,installed.packages()[,1])) == 0 ){ install.packages(paquete) ; print(paste0('La libreria ', '"', paquete ,'"', ' ha sido instalada.'))}
      else { print(paste0('La libreria ', '"', paquete ,'"', ' ya está instalada.'))}
      rm(paquete)
}

#### 0.3. Llamar las librerias
sapply(paquetes,require,character.only=T) 
rm(paquetes)

#### 0.4. Hoy veremos...
rstudioapi::viewer(url = 'Intro-clase-12.html')

#----------------------#
#  3. Importar raster  #
#----------------------#

### 3.1. Verifiquemos los atributos del raster a cargar
GDALinfo("data/original/siac/magdalena_deforestacion_1990_2000.tif")

### 3.2. Importar raster
browseURL(url = "http://www.siac.gov.co", browser = getOption("browser")) # Fuente
deforestacion = raster("data/original/siac/magdalena_deforestacion_1990_2000.tif")

### 3.2.1 plot basico del raster
plot(deforestacion)
rstudioapi::viewer(url = 'help/figures/simbolos.png')

### 3.2.1. Vamos a darle contexto a los datos
magdalena = st_read(dsn= 'data/original/mgn/MGN_Municipio.shp') %>% 
            subset(MPIO_CCDGO %in% c('189','980','053'))
plot(magdalena,border="blue",col=NA,add=T)

### 3.3. Atributos del raster
deforestacion

#### 3.3.1. Nombres de las bandas
names(deforestacion)
names(deforestacion) = "cobertura_vegetal"

#### 3.3.2. Extension
st_bbox(deforestacion)
deforestacion@extent

#### 3.3.3. Proyeccion
deforestacion@crs
crs(deforestacion)
st_crs(deforestacion)

#### 3.3.4. Valores de los pixeles (ver diccionario)
minValue(deforestacion$cobertura_vegetal)  
maxValue(deforestacion$cobertura_vegetal)  
values(deforestacion)

### 3.3. Trabajar con los values de los pixeles
values(deforestacion)[1] <- 0
values(deforestacion)[is.na(values(deforestacion))==T] <- 0

#### 3.3.1. Descriptivas
values(deforestacion) %>% table()
values(deforestacion) %>% summary()

### 3.4. Exportar raster
writeRaster(x = deforestacion,filename = "data/procesada/magdalena_deforestacion_1990_2000.tif",overwrite=TRUE)

### 3.5. Puedo reproyectar un raster?
proj4string(deforestacion)
deforestacion_pr <- raster::projectRaster(deforestacion,crs = '+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs')
proj4string(deforestacion_pr)

#### 3.5.1. Veamos que pasa con los valores de los pixeles
minValue(deforestacion_pr) ; maxValue(deforestacion_pr) # Ups!

#------------------------------#
#  4. Operaciones geometricas  #
#------------------------------#

### 4.0. Limpiemos el entorno
rm(list = ls())

### 4.1. Importar raster de luces
browseURL(url = "https://ngdc.noaa.gov/eog/viirs/download_dnb_composites.html", browser = getOption("browser")) # Fuente
rstudioapi::viewer(url = 'data/original/NOAA/colombia_202004.tif')
luces = raster(x = 'data/original/NOAA/colombia_202004.tif')
luces
cataca = st_read(dsn= 'data/original/mgn/MGN_Municipio.shp') %>% subset(MPIO_CCDGO %in% c('053'))

### 4.2. Cliping un raster  
l_cataca = crop(luces,cataca) # Que hice mal?
crs(luces) 
crs(cataca)

ggplot() + geom_tile(data = as.data.frame(l_cataca, xy=TRUE), aes(y=y,x=x,fill=colombia_202004)) + 
scale_fill_distiller(palette='Spectral',na.value = 'gray') + 
geom_sf(data =cataca,color = 'black',fill=NA) + theme_bw()
   
l_cataca = crop(luces,cataca) %>% mask(cataca)
ggplot() + geom_tile(data = as.data.frame(l_cataca, xy=TRUE), aes(y=y,x=x,fill=colombia_202004)) + 
scale_fill_distiller(palette='Spectral',na.value = 'gray') + 
geom_sf(data =cataca,color = 'black',fill=NA) + theme_bw()

### 4.3. Extraer los valores de un raster
data = values(l_cataca) %>% .[is.na(.)==F]
summary(data)

### 4.4. Raster a datos vectoriales

### 4.4.1. Raster a puntos
point_1 = rasterToPoints(l_cataca)
point_2 = rasterToPoints(l_cataca,spatial = T) %>% st_as_sf()
ggplot() + geom_sf(data = point_2 , aes(color=colombia_202004),size=0.5,shape=23)

### 4.4.2. Raster a polygonos
polygon = rasterToPolygons(l_cataca) %>% st_as_sf()
ggplot() + geom_sf(data = polygon , aes(fill=colombia_202004)) +
scale_fill_distiller(palette='Spectral',na.value = 'gray')

### 4.5. Exportar sf
saveRDS(object = polygon , file = 'data/procesada/Sf luces Aracataca.rds')

#-------------------------------------------#
#  5. Trabajar con raster de varias bandas  #
#-------------------------------------------#

### 5.0. Como se ve un RGB?
dev.off()
plotRGB(stack("help/figures/rgb_raster.png"),r = 1, g = 2, b = 3) # Imagen tomada de https://www.neonscience.org

### 5.1. Importar raster
browseURL(url = 'https://data.neonscience.org/apps/browse', browser = getOption("browser")) # Fuente
GDALinfo("data/original/neon/HARV_RGB_Ortho.tif")
banda_r <- raster(x = "data/original/neon/HARV_RGB_Ortho.tif") # Como no le vamos a indicar que banda cargar, el va a cargar por 'default' la banda 1, es decir la roja

### 5.2. Veamos que tenemos
paleta_col <- gray.colors(n = 100, start = 0.0,end = 1.0,alpha = NULL) 
plot(banda_r, col=paleta_col, axes=FALSE, main="Imagen RGB - Banda 1 (roja)") 

### 5.2.1.  En un RGB podemos tener 255*255*255 posibles combinaciones, es decir 16.581.375 colores
minValue(banda_r) ; maxValue(banda_r)

### 5.3. Importando las otras bandas
banda_g <- raster("data/original/neon/HARV_RGB_Ortho.tif", band = 2)
plot(banda_g,col=paleta_col,axes=FALSE, main="Imagen RGB - Banda 2 (verde)") 

### 5.4. Las tres bandas de una sola vez

### 5.4.1. Cargando cada banda por aparte y apliando
RGB_apilado = stack(raster("data/original/neon/HARV_RGB_Ortho.tif",band=1),
                    raster("data/original/neon/HARV_RGB_Ortho.tif",band=2),
                    raster("data/original/neon/HARV_RGB_Ortho.tif",band=3))
RGB_apilado

### 5.4.2. Usando stack
RGB_stack = stack("data/original/neon/HARV_RGB_Ortho.tif")

### 5.4.3. Podriamos usar la opcion brick que es mas eficiente
RGB_brick <- brick(x = "data/original/neon/HARV_RGB_Ortho.tif")
object.size(RGB_brick)
object.size(RGB_stack)
object.size(RGB_apilado)

### 5.4.4. Veamos que tenemos
dev.off()
plotRGB(RGB_brick, r = 1, g = 2, b = 3)

### 5.5. Veamos los atribustos
RGB_stack

### 5.5.1. names
names(RGB_stack)
names(RGB_stack) = c('red','green','blue') 
names(RGB_stack)

### 5.5.1. Layers
RGB_stack@layers
RGB_apilado@layers
RGB_apilado[[1]]

### 5.6. Extraer atributos
point_RGB = rasterToPoints(RGB_stack,spatial = T) %>% st_as_sf()

#-----------------#
#  6. Aplicacion  #
#-----------------#


raster(extract())
