pacman::p_load(tidyverse,
               sjmisc,
               srvyr,
               survey,
               dplyr)
library(leaflet)
library(reshape)
library(leaflet.extras)
library(rworldxtra)
library(raster)
library(sf) 
library(haven)
datos <- read_sav("C:/Users/danny/OneDrive/Escritorio/Casen 2017.sav")
# Como humanos, preferimos trabajar con etiquetas que con códigos. ypch es ingreso percapita

############# FRECUENCIA MAS POBRES X REGION X FACTOR EXPANSION
dataset<- as_factor(datos, only_labelled = TRUE)

a <- dataset$region
b <- dataset$pobreza

cross_tab =  xtabs(dataset$expr ~ unlist(a) + unlist(b),aggregate(dataset$expr ~ unlist(a)+unlist(b),dataset,mean))
tabla <- as.data.frame(cross_tab)
d <-tabla[!(tabla$Freq == 0),] 
d$Prob <- (region_pbb$Freq/ sum(region_pbb$Freq)) * 100
region_pbb<-head(d,16)

##############SUMA TOTAL % DE POBREZA 
sumas_grupales <-aggregate(d$Freq, by=list(d$unlist.b.), FUN = sum, na.rm = TRUE)
names(sumas_grupales)[1] <- "frec_pob"
names(sumas_grupales)[2] <- "cate_pob"

sumas_grupales

sumas_grupales$prob <- prop.table(sumas_grupales$cate_pob)
sumas_grupales$prob <- round(prop.table(sumas_grupales$cate_pob), 3)*100
sumas_grupales

#######mapa q no sirve
#leaflet() %>% addTiles() %>% addCircles(data = datos, lat = ~expr, 
                                       # lng = ~region)
#Creamos el mapa en una variable en tamaña normal.
#map = leaflet() %>%     #Mapa vacío
 # addTiles() %>%    #Cargamos el Mapa de referencia (Base) por defecto
  #setView(lng = -70.636818,lat=-33.437311, zoom = 18) #Cargamos la Config. del mapa.
#Lo mostramos por pantalla.
#map                                        

#map = leaflet() %>%     #Mapa vacío
#  addTiles() %>%    #Cargamos el Mapa de referencia (Base) por defecto
#  setView(lat= latitude, lng= ~ longitude)  %>% #Cargamos la Config. del mapa.
#  addMarkers(region_po) #Cargamos un marcador
#Lo mostramos por pantalla.
#map
#Otras opciones
#----------------------------------------------------------------
##Vulnerabilidad familiar  POBREZA + POBL. MARGINADA
Vulnerabilidad_zona<- datos %>% 
  group_by(zona,sexo) %>% 
  frq(pobreza)
Vulnerabilidad_zona

  datos %>% 
  group_by(zona,sexo,pobreza) %>% 
  summarise(n = n()) %>% 
  mutate(zona = as.factor(zona),
         sexo = as.factor(sexo),
         pobreza = as.factor(pobreza))%>% 
  ggplot(aes(fill = pobreza,y = n,x = sexo))+
  geom_bar(stat = 'identity',position = 'dodge')+
  facet_wrap(~zona)

  #----------------------------------------------------------------
##Vulnerabilidad DISCAPACIDAD 
Vulnerabilidad_discapa<- datos %>% 
  group_by(ch4) %>% 
  frq(pobreza)
Vulnerabilidad_discapa

datos %>% 
  group_by(ch4,sexo,pobreza) %>% 
  summarise(n = n()) %>% 
  mutate(zona = as.factor(ch4),
         sexo = as.factor(sexo),
         pobreza = as.factor(pobreza))%>% 
  ggplot(aes(fill = pobreza,y = n,x = sexo))+
  geom_bar(stat = 'identity',position = 'dodge')+
  facet_wrap(~ch4)

#----------------------------------------------------------------

##Vulnerabilidad  POR CONDICIÓN DE GÉNERO
Vulnerabilidad_sexo<- datos %>% 
  group_by(sexo) %>% 
  frq(pobreza)
Vulnerabilidad_sexo

datos %>% 
  group_by(sexo,pobreza) %>% 
  summarise(n = n()) %>% 
  mutate(sexo = as.factor(sexo),
         pobreza = as.factor(pobreza))%>% 
  ggplot(aes(fill = pobreza,y = n,x = sexo))+
  geom_bar(stat = 'identity',position = 'dodge')



#----------------------------------------------------------------
##Vulnerabilidad INFANTIL EN EDUCACIÓN
Vulnerabilidad_educ<- datos %>% 
  group_by(educ) %>% 
  frq(pobreza)
Vulnerabilidad_educ

datos %>% 
  group_by(educ,sexo,pobreza) %>% 
  summarise(n = n()) %>% 
  mutate(educ = as.factor(educ),
         sexo = as.factor(sexo),
         pobreza = as.factor(pobreza))%>% 
  ggplot(aes(fill = pobreza,y = n,x = sexo))+
  geom_bar(stat = 'identity',position = 'dodge')+
  facet_wrap(~educ)


#----------------------------------------------------------------
##Vulnerabilidad prevision s12
datos %>% 
  group_by(s12,pobreza) %>% 
  summarise(n = n()) %>% 
  mutate(s12 = as.factor(s12),
         pobreza = as.factor(pobreza))%>% 
  ggplot(aes(fill = pobreza,y = n,x = s12))+
  geom_bar(stat = 'identity',position = 'dodge')


Vulnerabilidad_previsión<- datos %>% 
  group_by(s12) %>% 
  frq(pobreza)


######VULNERABILIDAD POR REGION
datos %>% 
  group_by(region_po,pobreza) %>% 
  summarise(n = n()) %>% 
  mutate(region_po = as.factor(region_po),
         pobreza = as.factor(pobreza))%>% 
  ggplot(aes(fill = pobreza,y = n,x = s12))+
  geom_bar(stat = 'identity',position = 'dodge')


#vulnerabilidad = Vulnerabilidad_discapa+ Vulnerabilidad_educ +Vulnerabilidad_previsión + Vulnerabilidad_sexo + Vulnerabilidad_zona
#view(data)
#dim(data)
#head(data) ytoth: ingresos total del hogar

frq(datos$region)
frq(datos$pobreza)
frq(datos$sexo)

descr(datos$expr) #Ponderador regional con respecto al fator de expansión
sum(datos$expr) #Total de la población
descr(datos$varstrat) #Estrato de varianza
descr(datos$varunit) #Conglomerado de varianza
descr(datos$ytoth)
descr(datos$ytotcor)
datos %>% 
  group_by(sexo) %>% #Espeficicamos que agruparemos por sexo
  summarise(media = mean(ypch)) #Creamos una columna llamada media, calculando la media ingresos

datos %>% 
  group_by(sexo) %>% 
  frq(pobreza)


datos %>% 
  group_by(sexo) %>% 
  descr(ypch)

#Crear variables de tamaño de estratos para corregir por población finita 
data <- datos %>% 
  group_by(varstrat) %>% #Agrupando por varstrat
  mutate(stratn = sum(expr)) %>%  #Calculamos el total de personas por estrato
  ungroup() #desagrupamos

casen_regional <- data %>% #Creamos un nuevo objeto llamado casen_regional con la información de data
  as_survey_design(ids = varunit, #Aplicamos diseño muestral, especificando los ids a partir de varunit,
                   strata = varstrat,#los estratos a partir de varstrat,
                   fpc = stratn, #especificando que la estimación es con una población finita
                   weights = expr) #y los ponderadores con factor de expansión

#Comparamos nuestra nueva variable casen_regional con la data original
casen_regional %>% #Con casen_regional
  summarise(ing_medio = survey_mean(ypch, na.rm=T)) #Calculamos el ingreso medio poblacional

data %>% #Con data
  summarise(ing_medio = mean(ypch, na.rm=T)) #Calculamos el ingreso medio poblacional

casen_regional %>%#Con casen_regional
  summarise(ing_medio = survey_mean(ypch, vartype = "ci", na.rm=T)) #Calculamos el 

#Calculamos al 95% y 99% de confianza
casen_regional %>% 
  summarise(ing_medio95 = survey_mean(ypch, vartype = "ci", level = .95, na.rm=T), #Al 95%
            ing_medio99 = survey_mean(ypch, vartype = "ci", level = .99, na.rm=T)) #Al 99%

#Agrupando con casen regional
casen_regional %>% 
  group_by(sexo) %>% 
  summarise(ing_medio = survey_mean(ypch, vartype = "ci", na.rm=T)) #Calculamos el ingreso medio poblacional, y sus intervalos de confianza
###############################



ingreso_region <-casen_regional %>% 
  group_by(region) %>% 
  summarise(ing_medio = survey_mean(ypch, vartype = "ci", na.rm=T)) #Calculamos el ingreso medio poblacional, y sus intervalos de confianza

grafi_ingresoregion <-casen_regional %>% 
  group_by(region) %>% 
  summarise(ing_medio = survey_mean(ypch, vartype = "ci", na.rm=T)) %>% 
  mutate(region = as.factor(region))%>% 
  ggplot(aes(fill = pobreza,y = n,x = region))+
  geom_bar(stat = 'identity',position = 'dodge')

  

## Generar tabla
vulnerabilidad <- casen_regional %>% 
  group_by(pobreza) %>% 
  summarise(ing_medio = survey_mean(ytot, vartype = "ci", na.rm = T)) %>% 
  ungroup()  
#CUANTO ESTAN RESIVIENDO CADA UNO POR PERCAPITA ES DECIR CADA INDIVIDUO CUANTO TIENE AL MES
ing_vulnerabilidad <- vulnerabilidad %>% 
  mutate('Vulnerables extremos' = c(.$ing_medio[1], .$ing_medio_low[1], .$ing_medio_upp[1],0), # Extraemos los valores correspondientes a la primera fila en cada una de nuestras variables
         'Vulnerables no extremos' = c(.$ing_medio[2], .$ing_medio_low[2], .$ing_medio_upp[2],0), # Extraemos los valores correspondientes a la segunda fila en cada una de nuestras variables
         'No Vulnerables' = c(.$ing_medio[3], .$ing_medio_low[3], .$ing_medio_upp[3],0)) %>% # Extraemos los valores correspondientes a la tercera fila en cada una de nuestras variables
  select('Vulnerables extremos', 'Vulnerables no extremos', 'No Vulnerables')

head(ing_vulnerabilidad)
summary(data$pobreza)
summary(data$region)


#Estimamos porcentajes con mutate()
#Para estimar totales
casen_regional %>% #Con casen_regional
  group_by(pobreza) %>% #Agrupamos por pobreza
  summarise(prop = survey_prop(na.rm = T), #Calculamos las proporciones
            total = survey_total(na.rm=T))%>% #Y el total por categorías
  mutate(per = prop*100) #Creamos una nueva columna multiplicando las proporciones *100 para obtener porcentajes

#Incorporamos los límites de los intervalos en porcentajes
casen_regional %>% #Con casen_regional
  group_by(pobreza) %>% #Agrupamos por pobreza
  summarise(prop = survey_prop(vartype = "ci", na.rm = T)) %>% #Incorporamos intervalos de confianza
  mutate(prop = prop*100, #Multiplicamos las proporciones *100,
         prop_low = prop_low*100, #así como el límite inferior 
         prop_upp = prop_upp*100) #y superior, para obtener porcentajes

#Incluir el total
casen_regional %>% #Con casen_regional
  group_by(pobreza) %>% #Agrupamos por pobreza
  summarise(prop = survey_prop(vartype = "ci", na.rm = T), #Calculamos las proporciones con intervalos de confianza
            total = survey_total(vartype = "ci", na.rm=T)) %>% #Así como el total por categoría
  mutate(prop = prop*100, #Multiplicamos las proporciones *100,
         prop_low = prop_low*100, #así como el límite inferior 
         prop_upp = prop_upp*100) #y superior, para obtener porcentajes

#Agrupar dos variables categóricas
casen_regional %>% #Creamos un objeto llamado pobreza_reg con datos de casen_regional
  group_by(pobreza, sexo) %>% #Agrupamos por pobreza y sexo
  summarise(prop = survey_prop(vartype = "ci", na.rm = T), #Calculamos las proporciones con intervalos de confianza
            total = survey_total(vartype = "ci", na.rm=T)) %>% #Así como el total por categoría
  mutate(prop = prop*100)



## Generar tabla -----------------------------------------------------------

vulnerabilidad_sexo <- casen_regional %>%
  group_by(sexo, pobreza) %>% 
  summarise(prop = survey_prop(vartype = "ci", na.rm = T), #Calculamos las proporciones con intervalos de confianza
            total = survey_total(vartype = "ci", na.rm=T)) %>% #Así como el total por categoría
  mutate(per = prop*100) %>%  #Multiplicamos las proporciones *100 para obtener porcentajes
  ungroup() #desagrupamos

vulnerabilidad_sexo_p <- vulnerabilidad_sexo %>% 
  mutate(sexo = c('Hombre', 'Mujer', 0, 0, 0,0 ), 
         'Vulnerables extremos' = c(.$per[1], .$per[4], 0, 0, 0, 0), 
         'Vulnerables no extremos' = c(.$per[2], .$per[5], 0, 0, 0, 0), 
         'No Vulnerables' = c(.$per[3], .$per[6], 0, 0, 0, 0)) %>%   
  select(-c(pobreza, starts_with('prop'), starts_with('total'), per)) %>%  
  filter(sexo != 0) # Filtramos las filas con 0 de relleno
