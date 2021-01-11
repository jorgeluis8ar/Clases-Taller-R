"
Eduard Martinez
30-03-2020
"

### Limpiar consola, entorno, fijar directorio y cargar paquetes
cat("\f")
rm(list=ls())
options("scipen"=100, "digits"=4) # Forzar a R a no usar e+

### Fijar directorio y cargar paquetes
setwd("~/Documents/Universidad/Taller de R/Clases/clase_13")
packages <- c("tidyverse","raster","sf","xml2","rvest")
sapply(packages,require,character.only=T)

### Notas sobre datos de luces
"
The version 1 monthly series is run globally using two different configurations: 

* The first excludes any data impacted by stray light. 
* The second includes these data if the radiance vales have undergone the stray-light correction procedure (Reference).

These two configurations are denoted in the filenames as 'vcm' and 'vcmsl' respectively. 
The 'vcmsl' version, that includes the stray-light corrected data, will have more data coverage toward the poles,
but will be of reduced quality. It is up to the users to determine which set is best for their applications. 
The annual versions are only made with the 'vcm' version, excluding any data impacted by stray light."

#---------------------------------------#
# Descargando raster de luces mensaules #
#---------------------------------------#

### Importando Shape de Colombia
country = st_read(dsn = 'data/original/Colombia disolve.shp')

### Obtenemos los URL de los raster a descargar
page <- "https://eogdata.mines.edu/pages/download_dnb_composites_iframe.html"
lista_link <- read_html(page) %>% html_nodes("a") %>% html_attr('href') %>% as.character() %>%
              .[grep("75N180W",.)] %>% .[grep("vcmcfg",.)] %>% 
              .[c(grep("npp_2018",.),grep("npp_2019",.),grep("npp_2020",.))]

### Funcion que descarga y corta los raster para Colombia
download_raster <- function(url){
                   # Descargando el raster 
                   download.file(url,destfile = "data/original/Temporal.tgz")
  
                   # Descomprimiendo el .tgz
                   file <- untar("data/original/Temporal.tgz",list=TRUE) %>% .[grep("avg_rade9h.tif",.)]
                   untar("data/original/Temporal.tgz", files = file )
                   
                   # Leyendo el raster
                   raster_completo <- raster(x = file)
                   country <- st_transform(country,crs = crs(raster_completo))
                   raster_country <- crop(raster_completo,country) %>% mask(country)
                   
                   # Exportar raster
                   date = substr(file,11,16)
                   writeRaster(x = raster_country , filename = paste0("data/night light/colombia_",date,".tif") , overwrite = TRUE )
                   
                   # Remover archivos
                   file.remove("data/original/Temporal.tgz")
                   file.remove(file)
}
lapply(lista_link,download_raster)  


### Apilando datos de raster
files = list.files('data/night light/') %>% paste0('data/night light/',.)

### Dejando pixeles de Antioquia
bogota = st_read(dsn = 'data/original/bogota_habited.shp')
bogota = st_transform(x = bogota,crs = crs(raster(x = files[1])))
stack_raster = lapply(files, function(y) raster(x = y) %>% crop(.,bogota)) %>% stack()
names(stack_raster) = gsub('colombia_','date_',names(stack_raster) )

### Raster to point
polygon_light = as(stack_raster, 'SpatialPolygonsDataFrame') %>% 
                st_as_sf() %>% .[bogota,] %>% mutate(ID_pixel = 1:nrow(.))

#### Agregar informacion de Lotes a pixeles
lotes = readRDS(file = 'data/original/Uso lotes.rds')
lotes = st_transform(x = lotes,crs = crs(points))
data_light = st_join(polygon_light,lotes) %>% data.frame(data_light) %>% group_by(ID_pixel) %>%
             summarise(residencial=sum(rsdncl_),rest_hotel=sum(rst_ht_),
                       educac=sum(edccn_r),comercio=sum(comrc_r),indstria=sum(indstr_),
                       oficinas=sum(oficn_r),otros=sum(otros_r))

#### Colapsando informacion a nivel de pixel
data = merge(data.frame(polygon_light),data_light,'ID_pixel') %>% 
       dplyr::select(-geometry) %>% .[,c("ID_pixel","residencial","rest_hotel","educac","comercio","indstr_","oficinas","otros","date_201901","date_201902",
                                         "date_201903","date_201904","date_202001","date_202002","date_202003","date_202004")]  %>%
       reshape2::melt(id.vars = c("ID_pixel","residencial","rest_hotel" ,"educac","comercio","indstr_","oficinas","otros") , value.name="luminosidad" )  %>% 
       mutate(date = as.character(variable) %>% gsub('date_','',.) %>% as.numeric())  %>% dplyr::select(-variable)
saveRDS(object = data ,file = 'data/procesada/Night light.rds')       



