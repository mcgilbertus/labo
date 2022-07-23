#Por favor, cuando llame al experimento ZZ que invoca a este script, hagalo en una virtual machine nornal, inmortal, que no sea spot

#El paso de  HT, Hyperparameter Tuning, devuelve una cantidad optima de arboles en el lightgbm, el hiperparametro num_iterations
#sin embargo. empiricamente se ha visto que ese valor NO es lo mejor que se puede utilizar cuando se va correr ese lightgbm como parte de un semillerio
# el semillerio da mejor con mas iteraciones del lightgbm
# por eso, se generan multiples archivos con más iteraciones
# y al final se debera utilizar el Public Leaderboard para elegir uno

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

#genero un vector de una cantidad de PARAM$semillerio  de semillas,  buscando numeros primos al azar
primos  <- generate_primes(min=100000, max=1000000)  #genero TODOS los numeros primos entre 100k y 1M
set.seed( PARAM$semilla_primos ) #seteo la semilla que controla al sample de los primos
ksemillas  <- sample(primos)[ 1:PARAM$semillerio ]   #me quedo con PARAM$semillerio primos al azar


#cargo el dataset que tiene el dataset de training final
nom_arch  <- exp_nombre_archivo( PARAM$files$input$dtrain_final )
dataset  <- fread( nom_arch )

dataset[ , clase01 := ifelse( get( PARAM$const$campo_clase ) %in% PARAM$clase_train_POS, 1, 0 )  ]

campos_buenos  <- setdiff( colnames(dataset),
                           c( PARAM$const$campo_clase, "clase01") )


#cargo el dataset que tiene los datos del futuro
nom_arch  <- exp_nombre_archivo( PARAM$files$input$dfuture )
dfuture  <- fread( nom_arch )


#cargo la salida de la optimizacion bayesiana
#y la ordeno por ganancia descendente
nom_arch  <- exp_nombre_archivo( PARAM$files$input$BOlog )
tb_log   <- fread( nom_arch )
setorder( tb_log,  -ganancia )


#tabla donde guardo el resultado final
tb_modelos  <- data.table( archivo=  character(),
                           iteracion_bayesiana= integer(),
                           ganancia= numeric() )

tb_predicciones  <- data.table( archivo= character(),
                                iteracion_bayesiana= integer(),
                                ganancia=  numeric() )

tb_submits  <- data.table( archivo= character(),
                           iteracion_bayesiana=  integer(),
                           ganancia=  numeric(),
                           corte=  integer() )



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
  ganancia  <- parametros$ganancia

  #trafo  ratio_num_leaves
  hojas_maximo  <- nrow( dtrain ) / parametros$min_data_in_leaf
  parametros$num_leaves  <-   as.integer( parametros$ratio_num_leaves * hojas_maximo )
  parametros$ratio_num_leaves  <- NULL

  #elimino los parametros que no son de lightgbm
  parametros$fecha       <- NULL
  parametros$prob_corte  <- NULL
  parametros$estimulos   <- NULL
  parametros$ganancia    <- NULL
  parametros$iteracion_bayesiana  <- NULL
  parametros$experimento  <- NULL
  parametros$rows         <- NULL
  parametros$cols         <- NULL

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

  #genero la prediccion, Scoring
  prediccion  <- predict( modelo_final,
                          data.matrix( dfuture ) )

  tb_prediccion  <- dfuture[  , PARAM$const$campos_pk,  with=FALSE ]
  tb_prediccion[ , prob := prediccion ]


  nom_pred  <- paste0( PARAM$files$output$prefijo_pred,
                       sprintf( "%03d", iteracion_bayesiana),
                       ".csv"  )

  fwrite( tb_prediccion,
          file= nom_pred,
          sep= "\t" )

  #agrego y grabo la prediccion
  tb_predicciones  <- rbind( tb_predicciones,
                             list( nom_pred, iteracion_bayesiana, ganancia ) )

  fwrite( tb_predicciones,
          file=  PARAM$files$output$tb_predicciones,
          sep= "\t" )


  #genero los archivos para Kaggle
  cortes  <- seq( from= PARAM$KA_start,
                  to=   PARAM$KA_end,
                  by=   PARAM$KA_step )


  setorder( tb_prediccion, -prob )

  for( corte in cortes )
  {
    tb_prediccion[  , (PARAM$const$campo_pred) := 0L ]
    tb_prediccion[ 1:corte, (PARAM$const$campo_pred) := 1L ]

    nom_submit  <- paste0( EXP$experiment$name, 
                           "_",
                           sprintf( "%03d", iteracion_bayesiana ),
                           "_",
                           sprintf( "%05d", corte ),
                           ".csv" )

    fwrite(  tb_prediccion[ , c( PARAM$const$campo_id, PARAM$const$campo_pred) , with=FALSE ],
             file= nom_submit,
             sep= "," )


    #grabo la tabla de los nombres de los submits
    tb_submits  <- rbind( tb_submits,
                          list( nom_submit,
                                iteracion_bayesiana,
                                ganancia,
                                corte )  )

    fwrite( tb_submits,
            file= PARAM$files$output$tb_submits,
            sep= "\t"  )
  }

  #Hasta este punto fue exactamente lo mismo que antes
  #Pero ahora, empiezo a acumular predicciones

  tb_prediccion_semillerio  <- dfuture[  , PARAM$const$campos_pk,  with=FALSE ]
  #le asigno el ranking
  kdesplazamientos  <- c( 0, 100,200,300,400)
  for( desp in kdesplazamientos  )  tb_prediccion_semillerio[ , paste0( "pred_acumulada", desp)  := 0 ]

  original_num_iterations  <- parametros$num_iterations
  parametros$num_iterations  <- parametros$num_iterations + max( kdesplazamientos )

  #itero por las semillas acumulando la prediccion
  sem  <- 0
  for( vsemilla in ksemillas )
  {
    sem  <- sem + 1
    cat( sem, "  " ) 

    parametros$seed  <- vsemilla
    set.seed( parametros$seed )
    modelo_final_semillerio  <- lightgbm( data= dtrain,
                                          param=  parametros,
                                          verbose= -100 )

    #genero la prediccion como ranking
    for( desp in kdesplazamientos )
    {
      prediccion_semillerio  <- frank( predict( object= modelo_final_semillerio, 
                                                newdata= data.matrix( dfuture ),
                                                num_iteration=  original_num_iterations + desp
                                                ),
                                       ties.method= "random" )

      #acumulo el ranking de la prediccion
      tb_prediccion_semillerio[ , paste0("pred_acumulada", desp) := get(paste0("pred_acumulada", desp)) + prediccion_semillerio ]
    }
  }
  cat( "\n" )

  nom_pred_semillerio  <- paste0( PARAM$files$output$prefijo_pred_semillerio,
                                  sprintf( "%03d", iteracion_bayesiana),
                                  ".csv"  )

  fwrite( tb_prediccion_semillerio,
          file= nom_pred_semillerio,
          sep= "\t" )


  tb_predicciones  <- rbind( tb_predicciones,
                             list( nom_pred_semillerio, iteracion_bayesiana, ganancia ) )

  fwrite( tb_predicciones,
          file=  PARAM$files$output$tb_predicciones,
          sep= "\t" )

  
  for( desp in kdesplazamientos  )
  {
    #ahora, a la prediccion de semillerio la corto en distintos umbrales y genero el archivo para Kaggle
    setorderv( tb_prediccion_semillerio,  paste0("pred_acumulada", desp), -1 )

    for( corte in cortes )
    {
      tb_prediccion_semillerio[  , (PARAM$const$campo_pred) := 0L ]
      tb_prediccion_semillerio[ 1:corte, (PARAM$const$campo_pred) := 1L ]

      nom_submit  <- paste0( EXP$experiment$name, 
                             "_semillerio_",
                             sprintf( "%03d", iteracion_bayesiana ),
                             "_",
                             "a",
                             original_num_iterations  + desp,
                             "_",
                             sprintf( "%05d", corte ),
                             ".csv" )

      fwrite(  tb_prediccion_semillerio[ , c( PARAM$const$campo_id, PARAM$const$campo_pred) , with=FALSE ],
               file= nom_submit,
               sep= "," )


      #grabo la tabla de los nombres de los submits
      tb_submits  <- rbind( tb_submits,
                            list( nom_submit,
                                  iteracion_bayesiana,
                                  ganancia,
                                  corte )  )

      fwrite( tb_submits,
              file= PARAM$files$output$tb_submits,
              sep= "\t"  )
    }
  }



  #borro y limpio la memoria para la vuelta sigueinte del for
  rm( prediccion_semillerio  )
  rm( modelo_final_semillerio )
  rm( tb_prediccion_semillerio )
  rm( tb_prediccion )
  rm( tb_importancia )
  rm( prediccion )
  rm( modelo_final )
  rm( parametros )
  rm( dtrain )
  gc()
}


# grabo catalogo   ------------------------------------------------------------
# es lo ultimo que hago, indica que llegue a generar la salida
#no todos los archivos generados pasan al catalogo

exp_catalog_add( action= "FM",
                 type=   "file",
                 key=    "tb_modelos",
                 value = PARAM$files$output$tb_modelos  )

exp_catalog_add( action= "SC",
                 type=   "file",
                 key=    "predicciones",
                 value = PARAM$files$output$tb_predicciones  )

exp_catalog_add( action= "KA",
                 type=   "file",
                 key=    "kaggle_submits",
                 value = PARAM$files$output$tb_submits  )


#finalizo el experimento
#HouseKeeping
exp_finalizar( )
