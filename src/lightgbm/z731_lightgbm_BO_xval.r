#corre en la Google Cloud,  si el dataset de entrada es muy grande, debera poner mas memoria RAM
#    8 vCPU
#  128 GB  memoria RAM
#  256 GB  espacio en disco


# Este script esta pensado para correr en Google Cloud
# Optimizacion Bayesiana de hiperparametros de  lightgbm, con el metodo TRADICIONAL de los hiperparametros originales de lightgbm
# 5-fold cross validation
# la probabilidad de corte es un hiperparametro

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")

require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


kBO_iter  <- 100   #cantidad de iteraciones de la Optimizacion Bayesiana

#Aqui se cargan los hiperparametros
hs <- makeParamSet( 
         makeNumericParam("learning_rate",    lower=  0.01 , upper=     0.3),
         makeNumericParam("feature_fraction", lower=  0.2  , upper=     1.0),
         makeIntegerParam("min_data_in_leaf", lower=  1    , upper= 20000),
         makeIntegerParam("num_leaves",       lower= 16L   , upper=  2048),
         makeNumericParam("prob_corte",       lower= 1/120 , upper=  1/20)  #esto sera visto en clase en gran detalle
        )


kprefijo       <- "HT731"
ksemilla_azar  <- 102191  #Aqui poner la propia semilla
kdataset       <- "./datasets/paquete_premium_ext_721.csv.gz"

#donde entrenar
ktrain_mes_desde    <- 201912        #mes desde donde entreno
ktrain_mes_hasta    <- 202011        #mes hasta donde entreno, inclusive
ktrain_meses_malos  <- c( 202006 )   #meses a excluir del entrenamiento


kexperimento   <- paste0( kprefijo, "0" )
kbayesiana     <- "BO.RDATA"
klog           <- "BO_log.txt"

#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./exp/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )

  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )

    cat( linea, file=archivo )
  }

  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )

  cat( linea, file=archivo, append=TRUE )  #grabo al archivo

  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------
#esta funcion calcula internamente la ganancia de la prediccion probs
fganancia_logistic_lightgbm   <- function( probs, datos) 
{
  vlabels  <- get_field(datos, "label")

  gan  <- sum( (probs > PROB_CORTE  ) *
               ifelse( vlabels== 1, 59000, -1000 ) )


  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales, la semilla del mal ...

EstimarGanancia_lightgbm  <- function( x )
{
  gc()  #libero memoria

  #llevo el registro de la iteracion por la que voy
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1

  PROB_CORTE <<- x$prob_corte   #asigno la variable global

  kfolds  <- 5   # cantidad de folds para cross validation

  param_basicos  <- list( objective= "binary",
                          metric= "custom",
                          first_metric_only= TRUE,
                          boost_from_average= TRUE,
                          feature_pre_filter= FALSE,
                          verbosity= -100,
                          seed= ksemilla_azar,
                          max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                          min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                          lambda_l1= 0.0,         #por ahora, lo dejo fijo
                          lambda_l2= 0.0,         #por ahora, lo dejo fijo
                          max_bin= 31,            #por ahora, lo dejo fijo
                          num_iterations= 9999,    #un numero muy grande, lo limita early_stopping_rounds
                          force_row_wise= TRUE    #para que los alumnos no se atemoricen con tantos warning
                        )

  #el parametro discolo, que depende de otro
  param_variable  <- list(  early_stopping_rounds= as.integer(50 + 5/x$learning_rate) )

  param_completo  <- c( param_basicos, param_variable, x )

  set.seed( ksemilla_azar )
  modelocv  <- lgb.cv( data= dtrain,
                       eval= fganancia_logistic_lightgbm,
                       stratified= TRUE, #sobre el cross validation
                       nfold= kfolds,    #folds del cross validation
                       param= param_completo,
                       verbose= -100
                      )

  #obtengo la ganancia
  ganancia  <- unlist(modelocv$record_evals$valid$ganancia$eval)[ modelocv$best_iter ]

  ganancia_normalizada  <-  ganancia* kfolds     #normailizo la ganancia

  #el lenguaje R permite asignarle ATRIBUTOS a cualquier variable
  attr(ganancia_normalizada ,"extras" )  <- list("num_iterations"= modelocv$best_iter)  #esta es la forma de devolver un parametro extra

  param_completo$num_iterations <- modelocv$best_iter  #asigno el mejor num_iterations
  param_completo["early_stopping_rounds"]  <- NULL     #elimino de la lista el componente  "early_stopping_rounds"

  #si es una ganancia superadora, genero e imprimo ESA importancia de variables
  if( ganancia_normalizada > GLOBAL_ganancia )
  {
    GLOBAL_ganancia  <<- ganancia_normalizada
    modelo  <-  lgb.train( data= dtrain,
                           eval= fganancia_logistic_lightgbm,
                           param= param_completo,
                           verbose= -100
                         )

    tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
    archivo_importancia  <- paste0( "impo_",  sprintf( "%03d", GLOBAL_iteracion ), ".txt" )

    fwrite( tb_importancia, 
            file= archivo_importancia, 
           sep= "\t" )

  }

  #logueo 
  xx  <- param_completo
  xx$ganancia  <- ganancia_normalizada   #le agrego la ganancia
  
  xx$experimento  <- kexperimento
  xx$cols         <- ncol( dtrain )
  xx$rows         <- nrow( dtrain )

  xx$iteracion <- GLOBAL_iteracion

  loguear( xx, arch= klog )

  return( ganancia )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

#Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar el modelo
dataset  <- fread( kdataset )

#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0("./exp/", kexperimento, "/" ), showWarnings = FALSE )
setwd( paste0("./exp/", kexperimento, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO



GLOBAL_iteracion  <- 0   #inicializo la variable global
GLOBAL_ganancia   <- 0

#si ya existe el archivo log, traigo hasta donde llegue
if( file.exists(klog) )
{
  tabla_log  <- fread( klog )
  GLOBAL_iteracion  <- nrow( tabla_log )
  GLOBAL_ganancia   <- tabla_log[ , max( ganancia ) ]
}


#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( clase_ternaria=="BAJA+2", 1L, 0L) ]


#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )


#--------------------------------------
#elijo donde voy a entrenar

dataset[ , train  := 0L ]
dataset[ foto_mes >= ktrain_mes_desde & 
         foto_mes <= ktrain_mes_hasta &  
         ! (foto_mes %in% ktrain_meses_malos ) ,
         train  := 1L ]

#--------------------------------------

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ train == 1L, campos_buenos, with=FALSE]),
                        label= dataset[ train==1L, clase01]  )



#Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar  <- EstimarGanancia_lightgbm   #la funcion que voy a maximizar

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar, #la funcion que voy a maximizar
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  hs,     #definido al comienzo del programa
              has.simple.signature = FALSE   #paso los parametros en una lista
             )

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)  #se graba cada 600 segundos
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )   #cantidad de iteraciones
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI() )

#establezco la funcion que busca el maximo
surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if( !file.exists( kbayesiana ) ) {
  run  <- mbo(obj.fun, learner= surr.km, control= ctrl)
} else {
  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista
}


quit( save="no" )

