# clase_2


## Descargar la clase

Para descargar las clases debes seguir los siguientes pasos:

```{r}
# 1. Establecer el directorio de trabajo en el que quieres descargar la carpeta
setwd("~/Downloads")

# 2. Descargar el repositorio
download.file(url = "https://github.com/taller-R/clase_2/archive/master.zip", 
              destfile = "clase_2.zip")

# 3. Cambiar nuevamente el directorio de trabajo
setwd("~/Downloads/clase_2-master")

# 4. Inspeccionar archivos en el directorio 
list.files()
```
O puedes seguir estas otras [instruciones](https://eduard-martinez.github.io/blog/github/clonar_github.html).  

## Notas
* Los vídeos de la clase se encuentran [aquí](https://www.dropbox.com/sh/1s8odr6rrc64acl/AAAhWXHkq8w7_iMl3cW00kjfa?dl=0). 

* Por favor hacer todas las correcciones ortográficas a este y los demas archivos .Rmd del repositorio.
