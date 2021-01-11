### Created by: Jorge Luis Ochoa Rincon
## Date: Wednesday 2dn, September 2020

#--------------------------#
# 1. Parte I               #
#--------------------------#

#### 0.1 Cleaning the console, environment and fixing the working directory
cat("\f")
rm(list=ls())
setwd("~/Downloads/Universidad/Taller de R/Clases/clase_4")
getwd()

library('tidyverse');library(readxl)

#### 1.1 Loading the data sets

cg_resto=read.csv2(file="data/originales/Resto - Caracteristicas generales (Personas).csv", header = T, sep = ';')
deso_resto=read.csv2(file = "~/Downloads/Universidad/Taller de R/Clases/clase_4/data/originales/Resto - Desocupados.csv", header = T , sep = ';')
cg_cabecera=read.csv2(file ="~/Downloads/Universidad/Taller de R/Clases/clase_4/data/originales/Cabecera - Caracteristicas generales (Personas).csv", header = T , sep = ';')
deso_cabecera=read.csv2(file ="~/Downloads/Universidad/Taller de R/Clases/clase_4/data/originales/Cabecera - Desocupados.csv", header = T , sep = ';')
ft_cabecera=read.csv2(file = "task/data/Cabecera - Fuerza de trabajo.csv",header = T,sep = ';')
ft_resto=read.csv2(file = 'task/data/Resto - Fuerza de trabajo.csv',header = T,sep = ';')
in_cabecera=read.csv2(file = 'task/data/Resto - Inactivos.csv',header = T,sep = ';')
in_resto=read.csv2(file = 'task/data/Resto - Inactivos.csv',header = T,sep = ';')
#### 1.2 Cleaning and getting everything in order
colnames(cg_resto)=tolower(colnames(cg_resto))
colnames(cg_cabecera)=tolower(colnames(cg_cabecera))
colnames(deso_resto)=tolower(colnames(deso_resto))
colnames(deso_cabecera)=tolower(colnames(deso_cabecera))
colnames(ft_cabecera)=tolower(colnames(ft_cabecera))
colnames(ft_resto)=tolower(colnames(ft_resto))
colnames(in_cabecera)=tolower(colnames(in_cabecera))
colnames(in_resto)=tolower(colnames(in_resto))

cg_resto= cg_resto[,c('directorio','secuencia_p','orden','p6020','p6040','fex_c_2011')]
cg_cabecera= cg_cabecera[,c('directorio','secuencia_p','orden','p6020','p6040','fex_c_2011')]

deso_resto=deso_resto[,c('directorio','secuencia_p','orden')]
deso_cabecera=deso_cabecera[,c('directorio','secuencia_p','orden')]

cg_cabecera=mutate(cg_cabecera,urbano=1)

deso_resto=mutate(deso_resto,desocupado=1)
deso_cabecera=mutate(deso_cabecera,desocupado=1)

#### 1.3 Merging datasets
# The code has the option all.x=T because we would like to add observations within
# the dataset
cabecera=merge(x=cg_cabecera,y =deso_cabecera,by = c('directorio','secuencia_p','orden'), all = TRUE)

resto=merge(x = cg_resto,y = deso_resto,by = c('directorio','secuencia_p','orden'), all = TRUE)


cabecera$desocupado=ifelse(test = is.na(cabecera$desocupado)==F,yes=1,no=0)
resto$desocupado=ifelse(test = is.na(resto$desocupado)==F,yes=1,no=0)
cabecera %>% group_by(desocupado) %>%summarise(sum(desocupado))
resto %>% group_by(desocupado) %>%summarise(sum(desocupado))
resto %>% group_by(desocupado) %>%summarise(sum(fex_c_2011))
head(cabecera)

nacional= plyr::rbind.fill(cabecera,resto)

#### 1.4 Unemployment rate
people2=nacional  %>% group_by(p6040) %>% summarise(total=sum(fex_c_2011)) %>% summarise(total)
nacional2=subset(x = nacional,subset = p6040>12)
unemployed=nacional2 %>% group_by(desocupado) %>% summarise(total=sum(fex_c_2011)) #%>% summarise(total)
people =nacional2 %>% group_by(p6040) %>% summarise(total=sum(fex_c_2011)) %>% summarise(total)
(unemployed[2,2]/sum(people))*100
## Para calcular correctamente la tasa de desempleo se debe encontrar la PEA
ft_cabecera=ft_cabecera[,c(1,2,3,22)]
ft_resto=ft_resto[,c(1,2,3,27)]
ft_nacional= plyr::rbind.fill(ft_cabecera,ft_resto)
total_1=ft_cabecera %>% summarise(total=sum(fex_c_2011))
(unemployed[2,2]/total_1)*100

in_cabecera=in_cabecera[,c(1,2,3,28)]
ft_resto=ft_resto[,c(1,2,3,28)]
in_nacional= plyr::rbind.fill(in_cabecera,in_resto)
total_2=in_nacional %>% summarise(total=sum(fex_c_2011))
(unemployed[2,2]/sum(people)-total_2)*100
#--------------------------#
# 1. Parte II              #
#--------------------------#
cat("\f")
rm(list=ls())
#rstudioapi::viewer(url = "task/Task.html")
censo_2018=read.csv2(file = 'task/data/censo2018.csv',sep = ',',stringsAsFactors = FALSE)
censo_2018 = censo_2018[,c(2,4,7)]
colnames(censo_2018)=c("codigo","departamento","poblacion")
censo_2018 = censo_2018[nchar(censo_2018$codigo)>2,] %>% .[7:(nrow(.)-3),]
censo_2018$poblacion = as.numeric(censo_2018$poblacion)

proyeccion = readRDS(file = 'data/procesados/proyecciones DANE.rds')
proyeccion2018=subset(x = proyeccion,year==2018)
proyeccion2018=dplyr::select(.data = proyeccion2018,-year)
rm(proyeccion)
colnames(proyeccion2018)[3]="proyeccion"

total=merge(x = censo_2018,y = proyeccion2018,by ="codigo")
total=mutate(total,diferencia=poblacion-proyeccion)

total$percentage=(total$diferencia/total$poblacion)*100

#--------------------------#
# 1. Parte III             #
#--------------------------#  

cat("\f")
rm(list=ls())

excel_sheets('task/data/homicidios-2020.xlsx')
homicidios=read_excel(path = 'task/data/homicidios-2020.xlsx',sheet = 'Sheet1',col_names = TRUE)
colnames(homicidios)=c('Departamento','Municipio','Codigo','Armas_medios','mes','Descripcion_conducta','Genero','Grupo_edad','Cantidad')
homicidios= homicidios[10:nrow(homicidios),]

bogota=subset(homicidios,Municipio=="BOGOTÁ D.C. (CT)")
str(bogota)

bogota$mes=ifelse(test = (bogota$mes=='ABRILIL')==T,yes='ABRIL',no=bogota$mes)
bogota$Cantidad=as.numeric(bogota$Cantidad)
bogota %>% group_by(mes) %>% summarise(muertes=sum(Cantidad))



