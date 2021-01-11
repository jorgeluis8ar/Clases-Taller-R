# Elaborado por: Eduard F. Martinez Gonzalez
# Fecha: 16 de septiembre de 2020
# Nota: No se usan acentos ni caracteres especiales para evitar conflictos entre los diferentes sistemas operativos.

#--------------------------#
# 0. Configuración inicial #
#--------------------------#

#### 0.0 Hoy veremos...
rstudioapi::viewer(url = "Intro-clase-6.html")

#### 0.1 Limpiar la consola, el entorno y fijar directorio de trabajo
cat("\f")
rm(list=ls())
options("scipen"=100, "digits"=4) # Forzar a R a no usar e+
setwd("~/Documents/Universidad/Taller de R/Clases/clase_6")
getwd()

#### 0.2 Instalar las librerias que vamos a usar en la clase de hoy
for ( paquete in c("tidyverse",'viridis','forcats') ){
        if (length(grep(paquete,installed.packages()[,1])) == 0 ){ install.packages(paquete) ; print(paste0("La libreria ", "'", paquete ,"'", " ha sido instalada."))}
        else { print(paste0("La libreria ", "'", paquete ,"'", " ya está instalada."))}
        rm(paquete)
}

#### 0.3 Llamar las librerias
library('tidyverse') ; library('viridis') ; library('forcats')

#-------------------#
# 1. Graficos en R  #
#-------------------#

#### 1.0. Veamos la intuicion primero
rstudioapi::viewer(url = "help/Help-ggplot.html")

#### 1.0.1 Podemos obtener ayuda adiccional aqui
browseURL(url = "https://www.r-graph-gallery.com", browser = getOption("browser")) # Galeria de graficos en R
browseURL(url = "https://www.data-to-viz.com/index.html", browser = getOption("browser")) # Como elegir un grafico
browseURL(url = "https://www.data-to-viz.com/caveats.html", browser = getOption("browser")) # Topicos de ggplot()
browseURL(url = "https://ggplot2.tidyverse.org/reference/theme.html", browser = getOption("browser")) # Argumentos de theme()
browseURL(url = "https://ggplot2.tidyverse.org/reference/ggtheme.html", browser = getOption("browser")) # Temas en R
browseURL(url = "https://www.data-to-viz.com/caveat/overplotting.html", browser = getOption("browser")) # Overplotting
browseURL(url = "http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf", browser = getOption("browser")) # Colores en R
browseURL(url = "http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf", browser = getOption("browser")) # Colores en R

#-----------------# 
# 1.1 Histogramas #
#-----------------# 

#### 1.1.1 Load data
geih = readRDS(file = 'data/procesada/geih nacional.rds')
colnames(geih) %>% sort()
p6500 # ingreso
dpto

#### 1.1.2 Histogramas con la funcion hist del paquete base 
hist(geih$p6500)
hist(geih$p6500, col ="#FFCCFF" , border = 'red') 
hist(geih$p6500, col ="#69b3a2" , ylab = "Frecuencia" , 
                                  xlab = "Ingresos",
                                  main = "Histograma de \n los ingresos") # Etiquetas en los ejes

#### 1.1.3 Histogramas usando ggplot()
## los datos se pueden poner tanto en la funcion ggplot o geo_gihistogram
ggplot(data = geih, aes(x=p6500)) + geom_histogram() # Podemos escribirlo de esta manera
ggplot() + geom_histogram(data = geih, aes(x=p6500)) # O de esta manera
print('Explicar diferencia')

##### 1.1.3.1 Creemos un objeto al que le podamos agregar atributos
browseURL(url = "https://www.data-to-viz.com/caveats.html", browser = getOption("browser")) # Topicos de ggplot()
ggplot() + geom_histogram(data = geih, aes(x=p6500) , colour = "#69b3a2" )
p = ggplot() + geom_histogram(data = geih, aes(x=p6500) , colour = "#69b3a2" , fill = '#FFCCFF') # Ver objeto en environment
p

##### 1.1.3.2 Agreguemos los label
p + ylab('Frecuencia') + xlab('Ingresos') + ggtitle("Histograma de los ingresos")
p = p + labs(title = "Histograma de los ingresos", subtitle = "(2018)",
             caption = "Fuente: GEIH.",x = "Ingresos",y = "Frecuencia")
p 

##### 1.1.3.3 Agreguemos un tema
browseURL(url = "https://ggplot2.tidyverse.org/reference/ggtheme.html", browser = getOption("browser")) # Temas en R
p + theme_bw()
p + theme_minimal()
p + theme_light()
theme_propio = theme(plot.title = element_text(hjust = 0.5,size = 20) , legend.text = element_text(size = 10) , axis.text = element_text(size = 10)) 

#### 1.1.4. Histogramas por grupos
ggplot() + geom_histogram(data = geih, aes(x=p6500,group=p6020,colour=p6020,fill = p6020),alpha=0.5)+
        theme_propio

ggplot() + geom_histogram(data = geih, aes(x=p6500,group=p6020,colour=p6020,fill = p6020),alpha=0.5) +   
theme_light()

#### 1.1.5. Definir colores
geih = mutate(geih , sexo = ifelse(p6020==1,'Hombre','Mujer')) 

ggplot() + geom_histogram(data = geih, aes(x=p6500,group=sexo,colour=sexo,fill = sexo),alpha=0.5) +   
        scale_fill_manual(values=c("#69b3a2", "#404080")) + ## this defines te fill color of the hisotgram
        scale_color_manual(values=c("#69b3a2", "#404080")) + theme_light() # this defines the line color 

#-------------------------#
# 1.2. Graficos de barras #
#-------------------------#

#### 1.2.1. Cantidad de personas
geih %>% group_by(dpto) %>% summarize(total = sum(fex_c_2011)) %>%
        ggplot(aes(x=dpto, y=total)) +
        geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
        coord_flip() + xlab('') + ylab("Cantidad de personas") + theme_bw()

#### 1.2.2 Ordenar de mayor a menor
# no es necesario crear un objeto para hacer un grafico
# es suficiente con utilizar la funcion pipe
geih %>% group_by(dpto) %>% summarize(total = sum(fex_c_2011)) %>%
        mutate(dpto = fct_reorder(dpto, desc(total))) %>% ## la funcion fct_reorder permite reordenar los valores
        ggplot() + geom_bar(aes(x=dpto, y=total),stat="identity", fill="#f68060", alpha=.6, width=.4) +
        coord_flip() + xlab("") + theme_bw() + theme_propio
        
#### 1.2.3 Dos graficos a la vez
geih %>% group_by(p6050) %>% summarize(total = sum(fex_c_2011)) 

geih %>% group_by(p6050) %>% summarize(total = sum(fex_c_2011)) %>% arrange(p6050) %>% # Reorganizamos los valores para agregar labels con la funcion arrange
         mutate(name =as.factor(x = c("Jefe-hogar","Pareja","Hijo","Nieto","Otro-pariente","Empleado","Trabajador", "Pensionista","Otro-no-pariente"))) %>%
         ggplot(aes(x=name, y=total)) + geom_segment(aes(xend=name, yend=0)) +
         geom_point(size=4, color="green",shape=2) +
         theme_light() + coord_flip() + xlab('') + ylab('Cantidades') + theme_propio

# si se queire pintar vaiors graficos los datos se ponen en la funcion ggplot
# y los atributos de cambian en las funciones espciiii4īficas
#-----------------------------#
# 1.3. Graficos de dispersion #
#-----------------------------#

#### 1.3.1 Scaterplot sencillo
geih %>% group_by(dpto) %>% summarize(salario = weighted.mean(x = p6500,w = fex_c_2011))
#Para arreglar los missing values se puede hacer un subset de la data sin NA
## subset(geih, is.na(P6500)==F) %>%
geih %>% group_by(dpto) %>% summarize(salario = weighted.mean(x = p6500,w = fex_c_2011,na.rm=T))
geih %>% group_by(dpto) %>% summarize(salario = weighted.mean(x = p6500,w = fex_c_2011,na.rm=T)) %>%
         ggplot() + geom_point(aes(x=dpto,y=salario),colour='darkblue',shape=10,size=3) + theme_classic() # Cambio el size

geih %>% group_by(dpto) %>% summarize(salario = weighted.mean(x = p6500,w = fex_c_2011,na.rm=T)) %>%
        ggplot() + geom_point(aes(x=dpto,y=salario),shape=4,colour='darkblue',size=3) + theme_bw() # Cambio el tipo de shape


#### 1.3.1 Scaterplot por gurpos
geih %>% group_by(dpto,sexo) %>% summarize(salario = weighted.mean(x = p6500,w = fex_c_2011,na.rm=T)) 

geih %>% group_by(dpto,sexo) %>% summarize(salario = weighted.mean(x = p6500,w = fex_c_2011,na.rm=T)) %>%
        ggplot() + geom_point(aes(x=dpto,y=salario,group=sexo,colour=sexo),size=3) + theme_bw() # color por tipo


geih %>% group_by(dpto,sexo) %>% summarize(salario = weighted.mean(x = p6500,w = fex_c_2011,na.rm=T)) %>%
        ggplot() + geom_point(aes(x=dpto,y=salario,group=sexo,shape=sexo),color="red",size=3) + theme_light() # shape por tipo


geih %>% group_by(dpto,sexo) %>% summarize(salario = weighted.mean(x = p6500,w = fex_c_2011,na.rm=T)) %>%
        ggplot() + geom_point(aes(x=dpto,y=salario,group=sexo,shape=sexo,color=sexo),size=3) + theme_light() # color y shape por tipo

#-------------------------#
# 1.4. Graficos de lineas #
#-------------------------#
## Estos datos vienen del banco mundial
gini = readRDS(file = "data/procesada/gini.rds")

### Creemos la etiqueta del grafico
etiquetas <- labs(title = "Coeficiente de Gini",
                  subtitle = "(1980-2018)",
                  caption = "Fuente: Banco Mundial. Elaboración propia.",
                  y = "Coeficiente de Gini",
                  x = "Year")

#### 1.4.1 Solo lineas
ggplot() + geom_line(data = gini,aes(x=year, y=gini, group=country, color=country))

#### 1.4.2 Rellenemos los espacios con la funcion fill que ya hemos visto en clases"
gini <- gini %>% group_by(country) %>% fill(gini, .direction = "down") 
ggplot() + geom_line(data = gini,aes(x=year, y=gini, group=country, color=country))

#### 1.4.3 Lineas y puntos
print('Fijense que cuando creo 2 tipos de graficos en uno, la data la fijo en ggplot')
ggplot(gini, aes(x=year, y=gini, group=country, color=country)) + 
geom_line() + geom_point(color='black') +  theme_bw() + etiquetas + theme_propio

#### 1.4.3 Un solo grupo de lineas
colombia <- gini %>% subset(country == "Colombia") %>% 
            ggplot(data = .,aes(x=year, y=gini)) + geom_line(color="blue") + geom_point(color='black') +
            theme_bw() + theme_propio  + etiquetas

argentina <- gini %>% subset(country %in% c("Colombia","Argentina")) %>% 
            ggplot(aes(x=year, y=gini, group=country, color=country)) + geom_line() + geom_point(color='black') +
            theme_bw() + theme_propio + etiquetas
argentina


#-------------------#
# Exportar graficos #
#-------------------#

### Como jpeg
argentina
ggsave(plot= argentina , file = "results/Gini en Colombia y Argentina.jpeg")

### Como PNG
argentina
ggsave(plot= argentina , file = "results/Gini en Colombia y Argentina.png", width = 7, height = 5)

### Como PDF
colombia
ggsave(file = "results/Gini en Colombia.pdf") # Si no le indicamos el plot a exportar, exportara el ultimo que este en el visor

