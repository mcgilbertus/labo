#Optimizacion Bayesiana de hiperparametros de  ranger  (Random Forest)

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection


require("data.table")
require("rlist")
require("yaml")

require("ranger")
require("randomForest")  #solo se usa para imputar nulos
require("parallel")



ksemilla_azar  <- 102191  #Aqui poner la propia semilla

#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./work/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg ), ext )

  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )

    cat( linea, file= archivo )
  }

  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )

  cat( linea, file= archivo, append= TRUE )  #grabo al archivo

  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 
# particionar( data=dataset, division=c(1,1,1,1,1), agrupa=clase_ternaria, seed=semilla)   divide el dataset en 5 particiones

particionar  <- function( data, division, agrupa="", campo="fold", start=1, seed=NA )
{
  if( !is.na( seed)  )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x ) }, division, seq( from=start, length.out=length(division) )  ) )

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
           by= agrupa ]
}
#------------------------------------------------------------------------------

ranger_Simple  <- function( fold_test, pdata, param )
{
  #genero el modelo

  set.seed(ksemilla_azar)

  modelo  <- ranger( formula= "clase_binaria ~ .",
                     data=  pdata[ fold!= fold_test], 
                     probability=   TRUE,  #para que devuelva las probabilidades
                     num.trees=     param$num.trees,
                     mtry=          param$mtry,
                     min.node.size= param$min.node.size,
                     max.depth=     param$max.depth
                 )

  prediccion  <- predict( modelo, pdata[ fold == fold_test] )

  ganancia_testing  <- pdata[ fold==fold_test,
                              sum( (prediccion$predictions[ ,"POS" ] > 1/60) *
                                    ifelse( clase_binaria=="POS", 59000, -1000)  ) ]

  return( ganancia_testing )
}
#------------------------------------------------------------------------------

ranger_CrossValidation  <- function( data, param, pcampos_buenos, qfolds, pagrupa, semilla )
{
  divi  <- rep( 1, qfolds )
  particionar( data, divi, seed=semilla, agrupa=pagrupa )

  ganancias  <- mcmapply( ranger_Simple, 
                          seq(qfolds), # 1 2 3 4 5  
                          MoreArgs= list( data, param), 
                          SIMPLIFY= FALSE,
                          mc.cores= 1 )   #dejar esto en  1, porque ranger ya corre en paralelo

  data[ , fold := NULL ]   #elimino el campo fold

  #devuelvo la ganancia promedio normalizada
  ganancia_promedio  <- mean( unlist( ganancias ) )
  ganancia_promedio_normalizada  <- ganancia_promedio * qfolds

  return( ganancia_promedio_normalizada )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales

EstimarGanancia_ranger  <- function( x )
{
   GLOBAL_iteracion  <<- GLOBAL_iteracion + 1

   xval_folds  <- 5   # 5-fold cross validation

   ganancia  <- ranger_CrossValidation( dataset, 
                                        param= x,
                                        qfolds= xval_folds, 
                                        pagrupa= "clase_binaria",
                                        semilla= ksemilla_azar )

   #logueo 
   xx  <- x
   xx$xval_folds  <-  xval_folds
   xx$ganancia  <- ganancia
   xx$iteracion  <- GLOBAL_iteracion
   loguear( xx, arch= klog )

   return( ganancia )
}
#------------------------------------------------------------------------------
#Aqui comienza el programa

#Aqui se debe poner la carpeta de la computadora local
setwd("d:\\archi\\ernesto\\mmd\\Aplicaciones_a_Economia_y_Finanzas")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar el modelo
dataset  <- fread("./datasets/paquete_premium_202011.csv", stringsAsFactors= TRUE)   #donde entreno


#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/HT4310/", showWarnings = FALSE )
setwd("d:\\archi\\ernesto\\mmd\\Aplicaciones_a_Economia_y_Finanzas\\labo\\exp\\HT4310\\")   #Establezco el Working Directory DEL EXPERIMENTO

#en este archivo quedan el resultados
klog        <- "HT431.txt"


GLOBAL_iteracion  <- 0   #inicializo la variable global

#si ya existe el archivo log, traigo hasta donde llegue
if( file.exists(klog) )
{
  tabla_log  <- fread( klog )
  GLOBAL_iteracion  <- nrow( tabla_log )
}



#paso a trabajar con clase binaria POS={BAJA+2}   NEG={BAJA+1, CONTINUA}
dataset[ , clase_binaria := as.factor(ifelse( clase_ternaria=="BAJA+2", "POS", "NEG" )) ]
dataset[ , clase_ternaria := NULL ]  #elimino la clase_ternaria, ya no la necesito


#imputo los nulos, ya que ranger no acepta nulos
#Leo Breiman, ¿por que le temias a los nulos?
dataset  <- na.roughfix( dataset )


#llamo con los valores default
x  <- list( "num.trees" = 500,
            "max.depth" = NULL,
            "min.node_size" = NULL,
            "mtry"= NULL )

EstimarGanancia_ranger( x )

