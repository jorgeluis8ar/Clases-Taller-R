# Elaborado por: Eduard F. Martinez Gonzalez
# Fecha: 23 de septiembre de 2020
# Nota: No se usan acentos ni caracteres especiales para evitar conflictos entre los diferentes sistemas operativos.

#--------------------------#
# 0. Configuración inicial #
#--------------------------#

#### 0.0 Hoy veremos...
rstudioapi::viewer(url = "Intro-clase-8.html")

#### 0.1 Limpiar la consola, el entorno y fijar directorio de trabajo
cat("\f")
rm(list=ls())
options("scipen"=100, "digits"=4) # Forzar a R a no usar e+
setwd("~/Documents/Universidad/Taller de R/Clases/clase_8")
getwd()

#### 0.2 Instalar las librerias que vamos a usar en la clase de hoy
paquetes = c("tidyverse",'data.table')
for ( paquete in paquetes){
      if (length(grep(paquete,installed.packages()[,1])) == 0 ){ install.packages(paquete) ; print(paste0("La libreria ", "'", paquete ,"'", " ha sido instalada."))}
      else { print(paste0("La libreria ", "'", paquete ,"'", " ya está instalada."))}
      rm(paquete)
}

#### 0.3 Llamar las librerias
sapply(paquetes,require,character.only=T) 

#-----------------------#
# 1. Funcion 'function' #
#-----------------------#

#### 1.0. Veamos la intuicion primero
rstudioapi::viewer(url = "help/functions.html")

#### 1.0.1 Podemos obtener ayuda adiccional aqui
browseURL(url = "https://www.datamentor.io/r-programming/function/", browser = getOption("browser")) # Help 1
browseURL(url = "https://swcarpentry.github.io/r-novice-inflammation/02-func-R/", browser = getOption("browser")) # Help 2

#### 1.1. Ejemplos de una funcion

f_caracter <- function(x){ 
           x = x %>% toupper(.) %>% trimws(.)
return(x)
}
f_caracter(" Buenos Dias ")

#### 1.1.1. Apliquemos la funcion f_caracter sobre un vector de caracteres
vector_caracteres <- c(" Eduard Martinez "," Taller de R y estadistica "," Hola ","Buenas tardes ")

# Usando un loop
for (i in vector_caracteres){
     print(i)
     print(f_caracter(i))  
}

for (i in 1:length(vector_caracteres)){
     print(i)
     print(f_caracter(vector_caracteres[i]))  
}

# Usando lapply
lapply(vector_caracteres,f_caracter)


#### 1.2.  Sino indico la opcion return(), la funcion me devuelve el elemento que se escuentra en la ultima linea del cuerpo
betas_ols <- function(x,y){ 
          vector_unos <- matrix(data = 1, nrow = length(y) , ncol = 1)
          x <- cbind(vector_unos,x)
          beta <- solve(t(x)%*%x)%*%t(x)%*%y
beta   
}

"Probemos nuestra funcion, primero generemos una matrix con las variables independientes y un vector con la variable dependiente"
indep <- matrix(rnorm(20,100,50), nrow = 10, ncol = 2)
depen <- matrix(10:19, nrow = 10, ncol = 1)

"Ahora vamos a hacer la estimacion usando la funcion que creamos"
betas_ols(x = indep, y = depen)

"Verifiquemos los resultados usando la funcion lm() de el paquete 'stats'"
lm(depen ~ indep)

#------------------------------------------------#
# 2. Funciones de control dentro de una funcion. #
#------------------------------------------------#
browseURL(url = "https://mauricioanderson.com/curso-r-debugging/", browser = getOption("browser")) # Muy bueno!

#### 2.1. Opcion warnings
funcion_rc <- function(numero){
          if (numero >= 0){
          numero <- sqrt(numero)  
          return(numero)
          }  
}
funcion_rc(25)
funcion_rc(0)

"Es importante indicarle a la fucnion si se da el caso de no cumplir una de las condiciones, es decir, 
se debe indicar que debe hacer la funcion en caso de que no se cumpla una condicion excluyente.
Por ejemplo apliquemos la funcion a un numero negativo"
funcion_rc(-100)

"Reescribamos la funcion"
funcion_rc <- function(numero){
  
            # Si es mayor o igual a cero aplicar la raiz cuadrada
            if (numero >= 0){
                resultado <- sqrt(numero)  
            return(resultado)
            }  
            
            # Si es menor a cero
            if (numero < 0){
                warning(paste("Advertencia! ",numero," es un numero negativo, se reporta la raiz cuadrada del valor absoluto (",abs(numero),")",sep = ""))
              resultado <- abs(numero) %>% sqrt(.)  
            return(resultado)
            }
}
funcion_rc(-100)
funcion_rc(0)

#### 2.1. Opcion stop()
"Cuando obtenemos un 'Error' en la consola despues de eejcutar una funcion, nos indica que no 
aplico la funcion y debemos verificar que es lo que no esta bien"
"Reescribamos la funcion"
funcion_palabra <- function(palabra){
  
                    # Si es un caracter
                    if (is.character(palabra) == TRUE){
                      palabra <- trimws(palabra) %>% toupper(.)  
                      return(palabra)
                    }  
                    
                    # Si no es caracter
                    if (is.character(palabra) == FALSE){
                        stop(paste("Advertencia! el elemento",palabra," no es un del tipo caracter"))
                    }
}
funcion_palabra("hola")
funcion_palabra(100)


#------------------------------------------------#
# 3. Funciones de control dentro de una funcion. #
#------------------------------------------------#
browseURL(url = "https://mauricioanderson.com/curso-r-debugging/", browser = getOption("browser")) # Muy bueno!

### 3.1. Veamos traceback()
indep <- matrix(rnorm(20,100,50), nrow = 10, ncol = 2)
depen <- matrix(100:109, nrow = 10, ncol = 1)

'Ejecutemos nuestra funcion'
betas_ols(x = indep, y = depen)
traceback() # Me muestra la lista de funciones que se ejecutan hasta antes de cometerse el error

### 3.2. Veamos debug() y undebug() revisen el enlace que les dejo arriba
browseURL(url = "https://mauricioanderson.com/curso-r-debugging/", browser = getOption("browser")) 


#--------------------------#
# 4. Construir una funcion #
#--------------------------#

### Construir en la clase esta funcion paso a paso...

### Como cargar archivos de SIEDCO
cargar_siedco <- function(path_file){
                 base <- readxl::read_excel(path = path_file)
                 names <- base[9,] %>% as.character() %>% tolower() %>% gsub(" ","",.)
                 colnames(base) <- names
                 base <- base[9:nrow(base),]
                 return(base)
}
file_archivos <- list.files("data/original/") %>% paste0("data/original/",.)
lista_data = lapply(file_archivos,function(x) cargar_siedco(path_file = x)) 
database = lista_data %>% data.table::rbindlist(use.names = T,fill = T)
  
#----------#
# 5. Bonos #
#----------#

### Revisen estas funciones en la casa
"Carguemos algunas funciones que estan en el script Funciones.R"
source("codes/Funciones.R") # Asi se lee un script de 

# Apliquemos la funcion caracter_vector   
caracter_vector("Hola",3)
caracter_vector("Hola",3)
caracter_vector("Hola",4)
caracter_vector("Hola",5)

# Usemos la base de datos de hurtos
hurtos <- readRDS("data/procesada/hurtos 2018.rds")
str(hurtos)

"Cuales son los valores no numericos de la variable edad"
nonumedad <- extrac_nonumeric_vector(hurtos[,"edad"])
nonumedad
nonumfecha <- extrac_nonumeric_vector(hurtos[,"fecha"])
nonumfecha

# Hagamos unas decsriptivas de la base de datos
descriptiva <- descriptive_function(hurtos)
descriptiva
