###########################
### Proyecto 1 ############
### Tarea 2 ###############
### Alexis Carrillo #######
###########################

#----Limpiar el espacio de Trabajo---#
rm(list = ls(all.names =  TRUE)) 
gc()

#--- Tabla ---#
# Primero crearemos la tabla necesaria
# Para determinar el como se dividen los datos
SIDA <- c(11,14,12,32)
NoSIDA <- c(52,93,43,81)
Treatment <- factor(c('AZT','AZT','NoAZT','NoAZT'))
Raza=factor(c('N','B','N','B'))

df <- data.frame(SIDA,NoSIDA,Treatment,Raza)
show(df)

#--- Modelo ---#
# Apliquemos un modelo logisto para determinar
# la significancia del tratamiento
# Como queremos medir la significancia habra que hacerlo 
# Tomando en cuenta la cantidad de casos (SIDA, NoSIDA)

model <- glm(cbind(SIDA,NoSIDA)~Treatment+Raza,family = binomial(link='logit'))

# Observemos los resultados

summary(model)


# Lo que nos interesa es ver la significancia del AZT
# Entonces tendriamos que ver que el coeficiente estimado
# Para AZT no sea 0, es decir la siguiente prueba de Hipotesis

#               H_0: B_1 = 0 vs H_a: B_1 != 0

# Viendo los resultados teniamos que p-value= 0.00991
# Como 0.05 > 0.00991 tenemos: 
# Rechazamos la hipotesis nula con significancia del 5%, es decir
# Aceptamos que el coeficiente de AZT es distinto de O con probabilidad del 95%

# Lo que indica que en efecto: hay significancia en el tratamiento con AZT
# para los infectados de VIH para el desarrollo de SIDA.

# Algo que notamos es que es fuertemente implicado que RAZA tiene un
# Coeficiente de 0 con probabilidad del 95%.
# Observemos que sucede si no la consideramos

SIDA_new <- c(25,44)
NoSIDA_new <- c(145,124)
Treatment_NEW <- factor(c('AZT','NoAZT'))

df_new <- data.frame(SIDA_new,NoSIDA_new,Treatment_NEW)
show(df_new)

# Veamos el modelo

model_new <- glm(cbind(SIDA_new,NoSIDA_new)~Treatment_NEW,family = binomial(link='logit'))
summary(model_new)

# Comparemos ambos modelos

summary(model)
summary(model_new)

# Veamos la significancia de los modelos
with(model,null.deviance-deviance)
with(model,df.null-df.residual)
with(model,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail = FALSE))

# Notamos que el modelo tiene un p-value=0.03 lo cual indica que si es un modelo
# significativo con significancia del 5% (Mejor que un modelo vacio)

with(model,null.deviance-deviance)
with(model_new,df.null-df.residual)
with(model_new,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail = FALSE))

# Pero este modelo sin considerar la raza tiene un p-value=0.008
# Lo cual indicaria que igual es un modelo significativo con significancia del 5%
# (Mejor que un modelo vacio), pero es mucho mejor que el anterior.
