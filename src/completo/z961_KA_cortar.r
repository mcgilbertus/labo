#Generacion del la Final Prediction  para Kaggle
#Simplemente toma archivos scoreados de 202101 ( el mes sin clase )  con  <numero_cliente, foto_mes, probabilidad >
# los ordena por ganancia descendente
#  y genera multiples salidas, cada una de ellas enviendo estimulo a los primeros  N registros
#   ( los N que tienen mayor probabilidad de ser BAJA+2 )

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

source( "~/labo/src/lib/exp_lib.r" )

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

exp_iniciar( )


#cargo el dataset que tiene las predicciones
nom_arch  <- exp_nombre_archivo( PARAM$files$input$tb_predicciones )
tb_predicciones  <- fread( nom_arch )

setorderv( tb_predicciones, PARAM$const$campo_ganancia, -1 )


tb_submits  <- data.table( archivo= character(),
                           iteracion_bayesiana=  integer(),
                           ganancia=  numeric(),
                           corte=  integer() )

cortes  <- seq( from= PARAM$start,
                to=   PARAM$end,
                by=   PARAM$step )


for( i in  1:nrow(tb_predicciones) )
{

  reg  <- tb_predicciones[ i ]

  nom_arch  <- paste0( EXP$environment$exp_dir,
                       EXP$experiment$requires[1],
                       "/",
                       reg$archivo )

  dataset   <- fread( nom_arch )

  #orden por probabilidad descendente
  setorder(  dataset, -prob )

  for( corte in cortes )
  {
    dataset[  , (PARAM$const$campo_pred) := 0L ]
    dataset[ 1:corte, (PARAM$const$campo_pred) := 1L ]

    nom_submit  <- paste0( EXP$experiment$name, 
                           "_",
                           sprintf( "%03d", reg$iteracion_bayesiana ),
                           "_",
                           sprintf( "%05d", corte ),
                           ".csv" )

    fwrite(  dataset[ , c( PARAM$const$campo_id, PARAM$const$campo_pred) , with=FALSE ],
             file= nom_submit,
             sep= "," )


    tb_submits  <- rbind( tb_submits,
                          list( nom_submit,
                                reg$iteracion_bayesiana,
                                reg$ganancia,
                                corte )  )

    fwrite( tb_submits,
            file= PARAM$files$output$tb_submits,
            sep= "\t"  )
  }

  Sys.sleep( 5 )  #para que no se pisen las fechas
}




# grabo catalogo   ------------------------------------------------------------
# es lo ultimo que hago, indica que llegue a generar la salida
# en este caso guardo la tabla con los nombres de los submits a Kaggle

exp_catalog_add( action= "KA",
                 type=   "file",
                 key=    "kaggle_submits",
                 value = PARAM$files$output$tb_submits  )


#finalizo el experimento
#HouseKeeping
exp_finalizar( )

