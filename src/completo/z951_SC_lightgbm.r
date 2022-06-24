#Generacion del Scoring sobre un archivo ( future )
#  el scoring es agregar la probabilidad ( no importa Kaggle en esta etapa )
#  estos archivos que tienen < numero_de_cliente, foto_mes, probabilidad >  
#  se utilizaran para generar ensembles en la  clase Opcional
#utilizo un modelo ya generado y lo aplico a los datos del futuro

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

require("lightgbm")

source( "~/labo/src/lib/exp_lib.r" )

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

exp_iniciar( )


#cargo el dataset que tiene los datos del futuro
nom_arch  <- exp_nombre_archivo( PARAM$files$input$dentrada )
dataset   <- fread( nom_arch )

#cargo el modelo
nom_arch    <- exp_nombre_archivo( PARAM$files$input$modelos )
tb_modelos  <- fread( nom_arch )


setorderv( tb_modelos, PARAM$const$campo_ganancia, -1 )

tb_predicciones  <- data.table( archivo= character(),
                                iteracion_bayesiana= integer(),
                                ganancia=  numeric() )


#para cada uno de los modelos  .model
for( i in 1:nrow(tb_modelos) )
{
  reg  <- as.list( copy(tb_modelos[i] ))

  #cargo el modelo
  nom_arch  <- paste0( EXP$environment$exp_dir,
                       EXP$experiment$requires[1],
                       "/",
                       reg$archivo )

  modelo    <- lgb.load( nom_arch )

  prediccion  <- predict( modelo,
                          data.matrix( dataset ) )

  tb_prediccion  <- dataset[  , PARAM$const$campos_pk,  with=FALSE ]
  tb_prediccion[ , prob := prediccion ]


  nom_pred  <- paste0( PARAM$files$output$prefijo,
                       sprintf( "%03d", reg$iteracion_bayesiana),
                       ".csv"  )

  fwrite( tb_prediccion,
          file= nom_pred,
          sep= "\t" )

  #agrego y grabo
  tb_predicciones  <- rbind( tb_predicciones,
                             list( nom_pred, reg$iteracion_bayesiana, reg$ganancia ) )

  fwrite( tb_predicciones,
          file=  PARAM$files$output$tb_predicciones,
          sep= "\t" )

}


# grabo catalogo   ------------------------------------------------------------
# es lo ultimo que hago, indica que llegue a generar la salida
# en este caso, una table que tiene los nombres de TODOS los archivos scoreados

exp_catalog_add( action= "SC",
                 type=   "file",
                 key=    "predicciones",
                 value = PARAM$files$output$tb_predicciones  )


#finalizo el experimento
#HouseKeeping
exp_finalizar( )
