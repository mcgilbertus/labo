rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")

#------------------------------------------------------------------------------
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
          by= agrupa ]
}
#------------------------------------------------------------------------------

#Aqui se debe poner la carpeta de la computadora local
setwd("c:\\archi\\ernesto\\mmd\\Aplicaciones_a_Economia_y_Finanzas\\")  #Establezco el Working Directory
#cargo los datos

total = 0
for (semilla in c(300017, 300073, 300149, 300187, 300239))
{
  dataset  <- fread("./datasets/paquete_premium_202011.csv")
  
  #particiono estratificadamente el dataset
  particionar( dataset, division=c(50,50), agrupa="clase_ternaria", seed= semilla )  #Cambiar por la primer semilla de cada uno !
  
  param_basicos  <- list( "cp"=         0,  #complejidad minima
                          "minsplit"=  10,  #minima cantidad de registros en un nodo para hacer el split
                          "minbucket"=  2,  #minima cantidad de registros en una hoja
                          "maxdepth"=  10 ) #profundidad máxima del arbol
  
  #genero el modelo
  modelo  <- rpart("clase_ternaria ~ .",     #quiero predecir clase_ternaria a partir del resto
                   data= dataset[ fold==1],  #fold==1  es training,  el 70% de los datos
                   xval= 0,
                   control=  param_basicos )  #aqui van los parametros
  
  
  #aplico el modelo a los datos de testing
  prediccion  <- predict( modelo,   #el modelo que genere recien
                          dataset[ fold==2],  #fold==2  es testing, el 30% de los datos
                          type= "prob") #type= "prob"  es que devuelva la probabilidad
  
  #prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  #cada columna es el vector de probabilidades 
  
  #agrego una columna que es la de las ganancias
  dataset[  , ganancia :=  ifelse( clase_ternaria=="BAJA+2", 59000, -1000 ) ]
  
  #para testing agrego la probabilidad
  dataset[ fold==2 , prob_baja2 := prediccion[, "BAJA+2"] ]
  
  #calculo la ganancia en testing  qu es fold==2
  ganancia_test  <- dataset[ fold==2 & prob_baja2 >  1/60, sum(ganancia) ]
  
  #escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada  <-  ganancia_test / 0.5
  
  total <- total + ganancia_test_normalizada
  cat( paste(ganancia_test_normalizada, "\n" ))
}
cat(total/5.0)

