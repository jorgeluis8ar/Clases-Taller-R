# Elaborado por: Eduard F. Martinez Gonzalez
# Fecha: 11 de noviembre de 2020
# Nota: No se usan acentos ni caracteres especiales para evitar conflictos entre los diferentes sistemas operativos.

#--------------------------#
# 0. Configuración inicial #
#--------------------------#

#### 0.1. Limpiar la consola, el entorno y fijar directorio de trabajo
cat('\f')
rm(list=ls())
options('scipen'=100, 'digits'=4) # Forzar a R a no usar e+
setwd("~/Documents/Universidad/Taller de R/Clases/clase_13")
getwd()

#### 0.2. Instalar las librerias que vamos a usar en la clase de hoy
paquetes = c('tidyverse','lfe','plm','margins','stargazer','outreg')
for ( paquete in paquetes){
      if (length(grep(paquete,installed.packages()[,1])) == 0 ){ install.packages(paquete)}
      else { print(paste0('La libreria ', '"', paquete ,'"', ' ya está instalada.'))}
      rm(paquete)
}

#### 0.3. Llamar las librerias
sapply(paquetes,require,character.only=T) 
rm(paquetes)

#### 0.4. Hoy veremos...
rstudioapi::viewer(url = 'Intro-clase-13.html')

#--------#
# 1. OLS #
#--------#

### 1.1. Cargar bases de datos
covid = readRDS(file = 'data/procesada/Casos por UPZ.rds')

### 1.1.1 Generemos variable de tratamiento
rstudioapi::viewer(url = 'help/figures/cuarentenas.jpg')
treated = c(2,3,4,5,6,14,18,19)
control = c(11,10,12)
covid = mutate(covid , treatment = ifelse(cod_localidad %in% treated,1,
                                          ifelse(cod_localidad %in% control,0,NA)))

### 1.1.2. Regresion lineal
?lm
ols <- lm(formula = casos_08 ~ treatment + casos_07 +factor(cod_localidad) , data = covid , subset = is.na(treatment)==F)
ols
summary(ols) 

### 1.1.3. Que contiene el objeto OLS
View(ols)
ols$call # modelo estimado
ols$coefficients #  un vector con los coeficientes del modelo 
ols$na.action # observaciones para las que hay NA 
summary(ols)$r.squared # R^2
summary(ols)$adj.r.squared # R^2 ajustado

ols$residuals # un vector con los residuales del modelo
summary(ols$residuals)
hist(ols$residuals)

### 1.1.4. Valores predichos
ols %>% predict()
covid$y_gorro = predict(object = ols,newdata=covid)

#---------------------------------------------------#
# 2. Maximum Likelihood (Logit, Probit, Count Data) #
#---------------------------------------------------#

### 2.1. Cargar bases de datos
geih = readRDS(file = 'data/procesada/GEIH Junio 2019-2020.rds') %>%
       subset(f_trabajo==1 & is.na(inactivo)==T) 
geih = lapply(geih,function(x) x = ifelse(is.na(x)==T,0,x)) %>% data.frame()

### 2.2. Estimaciones
geih = mutate(geih , fex_c_2011 = as.character(fex_c_2011) %>% gsub(',','.',.) %>% as.numeric()/12, p6020 = ifelse(p6020==1,1,0))
modelo = as.formula('ocupado ~ p6020 + p6040 + urbano + esc')
logit = glm(modelo, subset = year == 2020, data = geih, family = binomial(link = "logit") ) 
logit %>% summary()

probit = glm(modelo, subset = year == 2020, data = geih, family = binomial(link = "probit") , weights = fex_c_2011) 
probit %>% summary()

### 2.3. Varios modelos (2^k modelos)
indepent <- c("p6020","p6040","esc","poly(esc,2)","urbano")
matriz <- expand.grid(c(TRUE,FALSE), c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE)) 
all_models <- apply(matriz, 1, function(x) as.formula(paste(c("ocupado ~ 1", indepent[x]),collapse=" + ")))

### 2.3.1. Estimaciones
all_results = lapply(all_models, function(x) glm(x, subset=year==2020, data=geih, family=binomial(link="probit")))
all_results

#---------------------#
# 3. Datos panel y IV #
#---------------------#

### 3.0. Limpiar datos de luces nocturnas
source(file = 'codes/Descargar y procesar datos de luces.R') # Ojo no correr esta linea en clases

### 3.1. Cargar bases de dato
data_light = readRDS(file = 'data/procesada/Night light.rds') %>% mutate(treatment = ifelse(date>=202003,1,0))
for(i in c('residencial','comercio','indstr_')){data_light[,paste0('d_',i)] = data_light[,i]*data_light$treatment}

### 3.2. Dos caminos para estimar datos panel
model_panel = as.formula('luminosidad ~ d_comercio + d_indstr_ + d_residencial')
   
### 3.2.1. plm()
#rstudioapi::viewer(url = 'help/pdf/Panel with many fixed effects in R.pdf')
plm_model = plm(model_panel,data = data_light ,model = 'within' , effect = 'twoways' , index = c('ID_pixel','date')) 
plm_model %>% summary()
   
### 3.2.2. felm()
#rstudioapi::viewer(url = 'help/pdf/Regressions | R for Stata Users.pdf')
felm_model = felm(luminosidad ~ d_comercio + d_indstr_ + d_residencial | date + ID_pixel |  0 | ID_pixel , data = data_light)
felm_model %>% summary()

#----------------------#
# 4. After estimations #
#----------------------#

### 4.1. Funcion 'outreg'
"fitlist = Lista con los outcomes de las regresiones
digits = numero de decimales
alpha = vector con los niveles de significancia
bracket = estadisticas a mostrar entre parentesis
starred = estadisticas a poner estrellas de significancia
robust = if TRUE, robust standard error is used
small = if TRUE, small sample parameter distribution is used
constlast = if TRUE, intercept is moved to the end of coefficient list
norepeat = if TRUE, repeated variable names are replaced by a empty string"
ols %>% summary()
outreg(fitlist = ols, digits = 4L, alpha = c(0.1, 0.05, 0.01),bracket = c("se"), starred = c("coef"), 
       robust = FALSE, small = TRUE,constlast = FALSE, norepeat = TRUE,)

### 4.2. Varios modelos a la vez"
list_models = list(all_results[[1]],all_results[[2]],all_results[[3]],all_results[[4]])
outreg(list_models)
outreg(all_results)

### 4.2.1. Cambiar los nombres de los modelos"
r_outreg = outreg(setNames(list_models, c('Modelo 1', 'Modelo 2', 'Modelo 3','Modelo 4')))
r_outreg

### 4.3. Funcion stargazer
stargazer(all_results[[1]],all_results[[2]],all_results[[3]],all_results[[4]],
          dep.var.caption = '' , dep.var.labels=c('Cluster') ,
          model.names = F , column.labels = c('Modelo 1','Modelo 2','Modelo 3','Modelo 4') ,
          covariate.labels = c('Wind') , #keep = c('sum_wind'),
          type = "text" , digits=4 , title="Night Lights Estimation" ,
          out = 'results/Resultados 1.tex',keep.stat = c('n','rsq','adj.rsq'),
          add.lines=list(c("Year FE","Yes","Yes"),c("Pixel FE","Yes","Yes")))

outreg_model = rockchalk::outreg(all_results[[1]],title = "Night Lights Estimation", float = TRUE)
outreg_model
cat(outreg_model,file = 'results/Resultados 2.tex')


### 4.4. Coefplot()



