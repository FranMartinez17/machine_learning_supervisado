rm(list=ls())
library(readr)
library(janitor)
library(dplyr)
library (ggplot2)
library (purrr)

#'
#' **Ejercicio 1**

df_mm<-read_csv("C:/Users/franc/Documents/Master/Herraminetas de programación para ciencia de datos/R/Code/Trabajo/mktmix.csv")
df_mm<-clean_names(df_mm)

#'
#' **Ejercicio 2**

#Hay 104 filas y 9 columnas
nrow(df_mm)
ncol(df_mm)

#La clase de base price y dicount es numeric
class(df_mm$base_price)
class (df_mm$discount)

#La columna Base Price creo que proporciona el primer precio establecido por la 
#empresa y discount la cantidad descontada al precio base

#'
#' **Ejercicio 3**

df_mm<-df_mm %>% 
  mutate (newspaper_inserts= if_else (is.na(df_mm$newspaper_inserts==TRUE),0,1))
class(df_mm$newspaper_inserts)

#'
#' **Ejercicio 4**
df_mm %>% 
  distinct(website_campaign)

#La columna website campaign contiene lo valores "Facebook", "Twitter", "Website Campaign"

df_mm<-df_mm %>% 
  mutate (website_campaign= if_else (is.na(df_mm$website_campaign==TRUE),"not available",df_mm$website_campaign)) 
df_mm<-df_mm %>%  
  mutate (twitter=if_else (df_mm$website_campaign=="Twitter",1,0)) %>% 
  mutate (facebook=if_else (df_mm$website_campaign=="Facebook",1,0)) %>% 
  mutate (website_campaign_2=if_else (df_mm$website_campaign=="Website Campaign",1,0))

#'
#' **Ejercicio 5**

sum(df_mm$facebook)
sum(df_mm$twitter)

#Hubo 4 semanas de campaña en Facebook y otras 4 semanas en Twitter 

#'
#' **Ejercicio 6**

sum(df_mm$tv<50)
#Durante 3 semanas la inversión en tv fue menor de 50grp

#' 
#' **Ejercicio7** 

radio_na<-df_mm %>% 
  mutate (radio=(if_else (is.na(df_mm$radio==TRUE),0,df_mm$radio)))

radio_na<-radio_na %>% 
  mutate (radio=(if_else ((radio==0),"NO","YES")))
          
radio_na %>% 
  group_by (radio) %>% 
  summarise(mean(tv))
#* Primero he modificado la tabla para dividir los valores de la columna radio en
#* "NO" (sin inversión) y "YES" (con inversión) ya que había valores NA y 0. 
#* Después he utilizado group by para calcular la media de la inversión de cada valor.
#'
#' **Ejercicio 8**

ggplot()+
  geom_line (aes(y=df_mm$new_vol_sales, x= 1:104))

#'
#' **Ejercicio 9**

df_mm %>% 
  ggplot()+
  geom_histogram(aes(new_vol_sales))
df_mm %>% 
  ggplot()+
  geom_boxplot(aes(y=new_vol_sales))

#* En el boxplot se puede apreciar que la mediana esta cerca de la línea de 20.000, 
#* aproximandamente 19.990. Respecto a la media, opino que es mayor a la mediana,
#* ya que creo que la linea de la mediana está un poco por debajo de la mitad de la
#* caja, ademásde haber valores máximos que pueden distorsionar la media y hacerla mayor

#'
#' **Ejercicio 10**

df_media <- df_mm %>% 
  select(tv,radio,stout)

library(tidyr)
df_media <- df_media %>%
  pivot_longer(everything())

ggplot(df_media)+
  geom_line(aes(y=value, x=1:312))+
  facet_wrap(~name,scales = "free_y", ncol=1, strip.position ="right")

#He utilizado 1:312 en el eje x debido a que es el número de filas de df_media

#En los gráficos se puede apreciar un tendencia positiva en la inversión en la 
#publicidad exterior. En la televisión, después de una tendencia negativa a partir 
#de la mitad del gráfico parece que en las útimas 60 semanas aproximadamente han 
#vuelto a apostar por ello. En la radio sucede algo parecido, y después de un 
#periodo de reducción de inversión en este medio desde la semana 210 aproximadamente, 
#se ha vuelto a incrementar

#'
#' **Ejercicio 11**

df_mm %>% 
  ggplot()+
  geom_point(aes(y=new_vol_sales, x=in_store))+
  geom_smooth(aes(y=new_vol_sales, x=in_store))

#Se puede apreciar un aumento en el volumen de ventas a medida que aumenta el stock.
#Siendo cierto que la correlación tampoco es muy grande, ya que hay datos muy dispersos

#'
#' **Ejercicio 12**

df_mm %>% 
  ggplot()+
  geom_point(aes(y=new_vol_sales, x=in_store, col=as.factor(newspaper_inserts)))

df_mm %>% 
  ggplot()+
  geom_point(aes(y=new_vol_sales, x=in_store, col=(tv)))

#'
#' **Ejercicio 13**

df_mm %>% 
  mutate(discount_truefalse=discount>0) %>%
  group_by(discount_truefalse) %>% 
  summarise(media_precio=mean(base_price)) %>% 
  ggplot()+
  geom_col(aes(discount_truefalse, media_precio))

#'
#' **Ejercicio 14  **
  
mis_categorias<-function(categoria){
  df_mm2<- df_mm %>% 
    select (all_of(categoria), new_vol_sales)
  
  my_model <- lm(new_vol_sales ~ ., data = df_mm2)

  r<-summary(my_model)$adj.r.squared
  return (r)
}

mis_categorias(c("tv","radio"))

#'
#' **Ejercicio 15**

lista<- list(c("base_price", "radio", "tv", "stout"), 
             c("base_price", "in_store", "discount", "radio", "tv", "stout"), 
             c("in_store", "discount"))
map_dbl(lista, mis_categorias)

# El segundo vector es el que ha conseguido mejor resultado con un 0,784, seguido 
# por el primero con 0,69. El tercero muestra que no hay ninguna correlación entre
# los descuentos y productos en tienda.

