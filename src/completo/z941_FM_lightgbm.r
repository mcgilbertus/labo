#Generacion del Final Model
#extraigo automaticamente los mejores parametros de la Bayesian Optimization, del archivo BOlog.txt
#genero el modelo entrenando en los datos train_final
# debe quedar MUY CLARO  que el entrenamiento final  se hace sobre un dataset DISTINTO a donde hice la  B.O.
#   en particular si a ka B.O. la hice sobre un dataset con undersampling de los CONTINUA, ahora entreno considerando TODOS los CONTINUA
# ya NO hago Bayesian Optimization

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")


require("primes")
require("lightgbm")


source( "~/labo/src/lib/exp_lib.r" )


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

exp_iniciar( )


#cargo el dataset que tiene el dataset de training final
nom_arch  <- exp_nombre_archivo( PARAM$files$input$dentrada )
dataset  <- fread( nom_arch )

dataset[ , clase01 := ifelse( get( PARAM$const$campo_clase ) %in% PARAM$clase_train_POS, 1, 0 )  ]

campos_buenos  <- setdiff( colnames(dataset),
                           c( PARAM$const$campo_clase, "clase01") )

#cargo la salida de la optimizacin bayesiana
#y la ordeno por ganancia descendente
nom_arch  <- exp_nombre_archivo( PARAM$files$input$BOlog )
tb_log   <- fread( nom_arch )
setorder( tb_log,  -ganancia )


#tabla donde guardo el resultado final
tb_modelos  <- data.table( archivo=  character(),
                           iteracion_bayesiana= integer(),
                           ganancia= numeric() )


#genero un modelo para cada uno de las modelos_qty MEJORES iteraciones de la Bayesian Optimization
for( i in  1:PARAM$modelos_qty )
{
  parametros  <- as.list( copy( tb_log[ i ] ) )
  iteracion_bayesiana  <- parametros$iteracion_bayesiana

  archivo_modelo  <- paste0( PARAM$files$output$FMmodelo ,
                             sprintf( "%03d", iteracion_bayesiana ), 
                             ".model" )

  tb_modelos  <- rbind( tb_modelos,
                        list( archivo_modelo,
                              parametros$iteracion_bayesiana,
                              parametros$ganancia ) )

  
  #creo CADA VEZ el dataset de lightgbm
  dtrain  <- lgb.Dataset( data=    data.matrix( dataset[ , campos_buenos, with=FALSE] ),
                          label=   dataset[ , clase01],
                          weight=  dataset[ , ifelse( get( PARAM$const$campo_clase ) %in% PARAM$clase_test_POS, 1.0000001, 1.0)],
                          free_raw_data= FALSE
                        )

  iteracion_bayesiana  <- parametros$iteracion_bayesiana

  #elimino los parametros que no son de lightgbm
  parametros$experimento  <- NULL
  parametros$cols         <- NULL
  parametros$rows         <- NULL
  parametros$fecha        <- NULL
  parametros$prob_corte   <- NULL
  parametros$estimulos    <- NULL
  parametros$ganancia     <- NULL
  parametros$iteracion_bayesiana  <- NULL
  
  #genero el modelo entrenando en los datos finales
  set.seed( parametros$seed )
  modelo_final  <- lightgbm( data= dtrain,
                             param=  parametros,
                             verbose= -100 )

  #grabo el modelo, achivo .model
  lgb.save( modelo_final,
            file= archivo_modelo )

  #creo y grabo la importancia de variables
  tb_importancia  <- as.data.table( lgb.importance( modelo_final ) )
  fwrite( tb_importancia,
          file= paste0( PARAM$files$output$FMimportancia, 
                        sprintf( "%03d", iteracion_bayesiana ),
                        ".txt" ),
          sep= "\t" )

  fwrite( tb_modelos,
          file= PARAM$files$output$tb_modelos,
          sep= "\t" )

  #borro y limpio la memoria para la vuelta sigueinte del for
  rm( tb_importancia )
  rm( modelo_final)
  rm( parametros )
  rm( dtrain )
  gc()
}


# grabo catalogo   ------------------------------------------------------------
# es lo ultimo que hago, indica que llegue a generar la salida
#no todos los archivos generados pasan al catalogo

exp_catalog_add( action= "FE",
                 type=   "file",
                 key=    "tb_modelos",
                 value = PARAM$files$output$tb_modelos  )

#finalizo el experimento
#HouseKeeping
exp_finalizar( )
