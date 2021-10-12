###########################
### Proyecto 1 ############
### Tarea 1 ###############
### Alexis Carrillo #######
###########################

#----Limpiar el espacio de Trabajo---#
rm(list = ls(all.names =  TRUE)) 
gc()

#--- Liberias necesarias----#
library(carData)
library(car)
library(effects)
library(alr4)
library(ggplot2)
library(corrplot)
library(GGally)
library(gridExtra)
library(dplyr)
library(fastDummies)
library(normtest)


#---Setting up Workspace---#
data <- as.data.frame(UN11)
data_clean <- data[,c(3,4,5,6)]

lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- data %>% 
    ggplot(mapping = mapping) +
    geom_point() +
    geom_smooth(method = method, color = "grey", ...)
  p
}


# 1) Realiza un analisis descriptivo de los datos, ¿que observas de cada variable?, ¿existe
# relacion?, ¿de que tipo?

data_clean %>%
  cor(method='pearson')

data %>%
  ggpairs(
    lower=list(continuous = wrap(lowerFn, method = 'lm')),
    diag=list(continuous='barDiag'),
    axisLabels='none'
    )

# La variable Fertility (Fertilidad) esta sesgada a la izquierda, lo que indica que la mayoria 
# de los valores de fertilidad son de bajos.
# La variable ppgdp igualment esta sesgada a la izquierda.
# La variable Life Expectancy tiene un sesgo a la derecha, lo que indica valores mas altos
# en promedio
# La variable pctUrban es dispersa y parece centrada.
# Los mas notable es la correlacion negativa entre LifeExp y Fertility. El resto de correlaciones
# Parecen no ser significativas, puesto que son medianamente correlacionadas, excepto ppgdp que 
# Aunque parezca estar nolinealmente corracionacionada con las otras variables.

data_clean %>%
  cor(method='spearman')


data %>%
  ggpairs(
    lower=list(continuous = wrap(lowerFn, method = 'lm')),
    upper = list(continuous = wrap("cor", method = "spearman")),
    diag=list(continuous='barDiag'),
    axisLabels='none'
  )
  
# Usando la correlacion de spearman podemos ver que en efecto, ppgdp esta no linealmente
# correlacionada con las otras variables.

# 2) Ajusta el modelo de regresion lineal multiple. (Identifica la variable dependiente y elige
#las variables explicativas. Muestra la ecuaci ́on de la regresi ́on, los par ́ametros estimados
#e interpreta los par ́ametros estimados).

# Tomemos la esperanza de vida como la variable dependiente. Puesto que es la que podemos explicar
# en relacion a los otros (Viendo los datos graficos)

# Para elegir adeacuadamente las variables es necesario que las estudiemos y veamos que cumplen
# Con los supuestos del modelo de regresion lineal.

# Consideremos inicialmente a fertility, ppgdp, pctUrban, region y group como los regresores.

# Lo primeor es que cada pais es independiente de otro, por lo que
# Las observaciones cumplen la incorrelacion

# Lo segundo es que, dado las correlaciones anteriores, podemos suponer que
# Los regresores tienen independencia lineal entre ellos.

# Podemos notar que ppgdp no tiene una relacion lineal con la variable
# LifeExp, por lo que habre que transformarla para cumplir este supuesto

data['ln_ppgdp']=log(data['ppgdp'])
data_clean['ln_ppgdp']=log(data_clean['ppgdp'])

# Veamoslo graficamente

plot1 <- data %>%
  ggplot(aes(x=ln_ppgdp,y=lifeExpF))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)

plot2 <- data %>%
  ggplot(aes(x=ppgdp,y=lifeExpF))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)

grid.arrange(plot1, plot2, ncol=2)

# Ahora tenemos una relacion lineal.
# Lo siguiente es ver que las variables sigan una distribucion normal.
# Utilicemos Shapiro-Wilks  sobre las variables standarizadas para verifiques esto

shapiro.test(scale(data$fertility))
hist(scale(data$fertility))

shapiro.test(scale(data$ln_ppgdp))
hist(scale(data$ln_ppgdp))

shapiro.test(scale(data$pctUrban))
hist(scale(data$pctUrban))

# Lo que notamos es que en todas falla la normalidad (p_Values <= 0.05)
# No obstante este no es un impedimento para la regresion lineal, puesto que
# El supuesto necesario es que los errores sean normales, supondremos
# Esto como cierto, porque nuestras variables explicativas pueden ser estocasticas

# Por ultimo, elijamos nuestras variables explicativas (A.K.A Regresores)
# Para esto veamos como se comporta cada una de ellas

plot1 <- data %>%
  ggplot(aes(x=ln_ppgdp,y=lifeExpF))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)

plot2 <- data %>%
  ggplot(aes(x=fertility,y=lifeExpF))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)

plot3 <- data %>%
  ggplot(aes(x=pctUrban,y=lifeExpF))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)

grid.arrange(plot1, plot2, plot3, ncol=3)

# Todas parecen estar linealmente relaciones, porque lo que usaremos estas como nuestras
# Variables explicativas

# Veamos el siguiente grupo de variables

plot1 <- data %>%
  ggplot(aes(x=group,y=lifeExpF))+
  geom_boxplot()+
  geom_smooth(method='lm', formula= y~x)

plot2 <- data %>%
  ggplot(aes(x=region,y=lifeExpF))+
  geom_boxplot()+
  geom_smooth(method='lm', formula= y~x)

grid.arrange(plot1, plot2, ncol=2)

# Estas variables parecen estar lo suficientemente diferenciables (Excepto Oceania y Atlatic
# Por lo que ignoraremos Atlantic) para poder ser consideradas igual como variables explicativas 

# Entonces nuestras variables explicativas seran las mencionadas en el inicio

# Creemos nuestro modelo

data_new <- data %>%
                dummy_cols(select_columns = c('region','group'))

data_new['region_Latin_Amer']<-data_new['region_Latin Amer']
data_new['region_North_America']<-data_new['region_North America']

model <- data_new %>%
  lm(formula=lifeExpF ~ ln_ppgdp+fertility+pctUrban+region_Africa+region_Asia+
       region_Caribbean+region_Europe+region_Latin_Amer+region_North_America+
       region_Oceania+group_oecd+group_africa+group_other)

# Mostremos el modelo
summary(model)
model$coefficients

# Notemos que no es posible estimar los valores de group_africa y group_other
# Por lo que los ignoraremos

model <- data_new %>%
  lm(formula=lifeExpF ~ ln_ppgdp+fertility+pctUrban+region_Africa+region_Asia+
       region_Caribbean+region_Europe+region_Latin_Amer+region_North_America+
       region_Oceania+group_oecd)

# Mostremos el nuevo modelo
summary(model)
model$coefficients


# 3)Realiza inferencia sobre los parametros estimados. (Pruebas de hipotesis, intervalos de
# confianza.)

# Veamos las pruebas de hipotesis por Summary
summary(model)

# Notemos que:
# pctUrban p_value=0.345055> 0.05, porque que no rechazamos la hipotesis de que sea 0
# region_Africa p_value=0.931857 > 0.05, porque que no rechazamos la hipotesis de que sea 0
# region_Asia  p_value=0.107726  > 0.05, porque que no rechazamos la hipotesis de que sea 0
# region_Caribbean  p_value=0.081284  > 0.05, porque que no rechazamos la hipotesis de que sea 0
# region_Europe  p_value=0.117166  > 0.05, porque que no rechazamos la hipotesis de que sea 0
# region_North_America  p_value=0.220372  > 0.05, porque que no rechazamos la hipotesis de que sea 0
# region_Oceania  p_value=0.149202   > 0.05, porque que no rechazamos la hipotesis de que sea 0

#Veamos que sucede con el modelo si ignoramos estas variables

model_new <- data_new %>%
  lm(formula=lifeExpF ~ ln_ppgdp+fertility+region_Latin_Amer+group_oecd)

# Mostremos el nuevo modelo
summary(model_new)
model_new$coefficients

# El modelo 'empeora', esto es porque dado el gran numero de variables que tenemos
# La mayoria de estos estimadores pueden ser 0 solo por suerte; Si consideramos el modelo
# Como un todo, es cuando el p-value toma significado, ahi podemos suponer que alguna de las 
# variables anteriores puede ser tomada como 0

# Veamos los intervalos de confianza

model %>%
  confint(level=0.95)

# El 2.5% corresponde al intervalor menor y el 97.5% al intervalo mayor.
# Estos son los invertavalos al 95% de confianza

# Sobre las pruebas de hipotesis: El modelo muestra un p-value=2.2e^-16 para el test F
# Lo que indica significancia en la predicion del modelo en conjunto con todas las
# variables independientes.
# Si observamos tenemos un estadistico R^2-adjusted=0.8036 lo que indica que
# individualmente las variables explicativas son significativas para el modelo.


# 4) Realiza el analisis de varianza a traves de la tabla ANOVA, interpreta los resultados.

anova=aov(model)
summary(anova)

# Los p-value de la tabla anova indica que las medias no necesariamente son distintas
# lo que indicaria que un regresor (Variable explicativa) proviene de la misma
# Poblacion que otra (lo que es equivalente a que su coeficiente sea 0).

# 5) Obtengan el coeficiente de determinacion (R^2), e interprete los resultados.

summary(model)

# El coeficiente de determinacion es R^2=0.8145, esto indica que dadas nuestras variables ind.
# podemos explicar el 81.45% de la variabilidad de lifeExpF (la variable dependiente)


# 6) Realiza un analisis de residuales, usando graficos y las pruebas necesarias
# interpreten sus resultados.



plot1 <- data %>%
  ggplot(aes(x=fertility,y=model$residuals))+
  geom_hline(yintercept = 0)+
  geom_point()+
  geom_smooth(method = "loess")

plot2 <- data %>%
  ggplot(aes(x=ln_ppgdp,y=model$residuals))+
  geom_hline(yintercept = 0)+
  geom_point()+
  geom_smooth(method = "loess")

plot3 <- data %>%
  ggplot(aes(x=pctUrban,y=model$residuals))+
  geom_hline(yintercept = 0)+
  geom_point()+
  geom_smooth(method = "loess")
  

grid.arrange(plot1, plot2, plot3)

# Dada la regresion parece que se cumple la hipotesis de media 0

plot1 <- data %>%
  ggplot(aes(x=region,y=model$residuals))+
  geom_boxplot()

plot2 <- data %>%
  ggplot(aes(x=group,y=model$residuals))+
  geom_boxplot()

grid.arrange(plot1, plot2, ncol=2)

# Estas igual parecen cumplir la hiptesis de media 0
# Veamos si se cumple

mean(model$residuals)

# En efecto se cumple la hipotesis de media 0

# Veamos la distribucion de los residuales
# Utilizemos Jarque-Bera para probar que se distribuyen normal

qqPlot(model$residuals)

jb.norm.test(model$residuals)
hist(scale(model$residuals))

# Observamos que p-value=2.8e^-7 < 0.05 por lo que  rechazamos que
# Los datos se distribuyan normal con un 95% de confianza
# Lo que nos indica que no se cumple la hipotesis de errores con
# Distribucion normal.