rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")
require("parallel")

ksemillas  <- c(300017, 300073, 300149, 300187, 300239) #reemplazar por las propias semillas

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

ArbolEstimarGanancia  <- function( semilla, param_basicos )
{
  #particiono estratificadamente el dataset
  particionar( dataset, division=c(70,30), agrupa="clase_ternaria", seed= semilla )  #Cambiar por la primer semilla de cada uno !

  #genero el modelo
  modelo  <- rpart("clase_ternaria ~ .",     #quiero predecir clase_ternaria a partir del resto
                   data= dataset[ fold==1],  #fold==1  es training,  el 70% de los datos
                   xval= 0,
                   control= param_basicos )  #aqui van los parametros del arbol

  #aplico el modelo a los datos de testing
  prediccion  <- predict( modelo,   #el modelo que genere recien
                          dataset[ fold==2],  #fold==2  es testing, el 30% de los datos
                          type= "prob") #type= "prob"  es que devuelva la probabilidad

  #prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  #cada columna es el vector de probabilidades 


  #calculo la ganancia en testing  qu es fold==2
  ganancia_test  <- dataset[ fold==2, 
                             sum( ifelse( prediccion[, "BAJA+2"]  >  1/60,
                                         ifelse( clase_ternaria=="BAJA+2", 59000, -1000 ),
                                         0 ) )]

  #escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada  <-  ganancia_test / 0.3

  return( ganancia_test_normalizada )
}
#------------------------------------------------------------------------------

#Aqui se debe poner la carpeta de la computadora local
setwd("c:\\archi\\ernesto\\mmd\\Aplicaciones_a_Economia_y_Finanzas\\")   #Establezco el Working Directory
#cargo los datos

dataset  <- fread("./datasets/paquete_premium_202011.csv")

param_basicos  <- list( "cp"=         -0.5,  #complejidad minima
                        "minsplit"=  900,  #minima cantidad de registros en un nodo para hacer el split
                        "minbucket"=  440,  #minima cantidad de registros en una hoja
                        "maxdepth"=  5 ) #profundidad máxima del arbol

# Ejemplo: un solo llamado, con la semilla 17
# ArbolEstimarGanancia( 17, param_basicos )  

#### minbucket debe ser <= que la mitad de minsplit
for (param_basicos$cp in c())
{
  for (param_basicos$minsplit in c())
  {
    for (param_basicos$minbucket in c(param_basicos$minsplit/10,...)) # ver que valores pueden ser
    {
      for (param_basicos$maxdepth in c(4:13))
      {
        # la funcion mcmapply  llama a la funcion ArbolEstimarGanancia  tantas veces como valores tenga el vector  ksemillas
        ganancias  <- mcmapply( ArbolEstimarGanancia, 
                                ksemillas,   #paso el vector de semillas, que debe ser el primer parametro de la funcion ArbolEstimarGanancia
                                MoreArgs= list( param_basicos),  #aqui paso el segundo parametro
                                SIMPLIFY= FALSE,
                                mc.cores= 1 )  #se puede subir a 5 si posee Linux o Mac OS
        
        #muestro la lista de las ganancias en testing para la particion realizada con cada semilla
        ganancias
        
        #paso la lista a vector
        unlist(ganancias)
        
        #finalmente calculo la media (promedio)  de las ganancias
        mean( unlist(ganancias) )
      }
    }
  }
}
