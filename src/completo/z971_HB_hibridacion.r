#Hibridacion de semillerios

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")


source( "~/labo/src/lib/exp_lib.r" )


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

exp_iniciar( )

#acomodo las tb_predicciones  in situ
for( i in 1:nrow( tb_catalogo[ key=="predicciones" ] )  )
{
  exp_carpeta  <- paste0( EXP$environment$exp_dir , tb_catalogo[ key=="predicciones", experiment ][i] , "/" )
  semillerios  <- list.files( path= exp_carpeta, pattern= PARAM$files$output$prefijo_pred_semillerio )

  tb_predicciones  <- fread( tb_catalogo[ key=="predicciones", value ][i] )

  semillerios_faltan  <- setdiff( semillerios,  tb_predicciones$archivo )
  if( length(semillerios_faltan) > 0 )
  {
    for( archivito  in  semillerios_faltan )
    {
      iter  <-  as.integer( substr(  archivito, 30, 32 ) )
      gan  <- tb_predicciones[ iteracion_bayesiana==iter, min(ganancia) ]
      tb_predicciones  <- rbind( tb_predicciones,  list( archivito, iter, gan ) ) 
    }
  }

  fwrite( tb_predicciones,
          file= tb_catalogo[ key=="predicciones", value ][i],
          sep= "\t" )
}


#En tb_final quedara la hibridacion

for( i in 1:nrow( tb_catalogo[ key=="predicciones" ] )  )
{
  exp_carpeta  <- paste0( EXP$environment$exp_dir , tb_catalogo[ key=="predicciones", experiment ][i] , "/" )
  semillerios  <- list.files( path= exp_carpeta, pattern= PARAM$files$output$prefijo_pred_semillerio )

  tb_predicciones  <- fread( tb_catalogo[ key=="predicciones", value ][i] )

  for( archivo in  tb_predicciones[ archivo %like% "semillerio", archivo ]  )
  {
    tb_prediccion <- fread( paste0( exp_carpeta, archivo ) )

    if( ! exists( "tb_final" ) )
    {
      #si no existe tb_final, le asigno el primer semillerio
      tb_final <- copy( tb_prediccion )
    }  else {

      #si ya existe tb_final, le sumo en pred_acumulado el ranking del nuevo semillerio
      columnas_pk <- colnames( tb_final )[ 1:(length(colnames( tb_final ))-1 ) ]

      #Aqui hago la suma
      tb_final[ tb_prediccion,
                on= columnas_pk,
                pred_acumulada :=  pred_acumulada + i.pred_acumulada ]
    }

  }
}


#grabo la nueva hibridacion
nom_pred_semillerio  <- "futuro_prediccion_hibridacion.csv"

fwrite( tb_final,
        file= "futuro_prediccion_hibridacion.csv",
        sep= "," )


tb_predicciones  <- data.table( archivo= character(),
                                iteracion_bayesiana= integer(),
                                ganancia=  numeric() )

tb_predicciones  <- rbind( tb_predicciones,
                           list( nom_pred_semillerio, -1, -1 ) )

fwrite( tb_predicciones,
        file=  PARAM$files$output$tb_predicciones,
        sep= "\t" )



setorder(  tb_final,  - pred_acumulada )

cortes  <- seq( from= PARAM$KA_start,
                to=   PARAM$KA_end,
                by=   PARAM$KA_step )


prefijo_submit <- "hibridacion"
for( nombre_exp in  tb_catalogo[ key=="predicciones", experiment ]  )  prefijo_submit <- paste0( prefijo_submit, "_", nombre_exp)


tb_submits  <- data.table( archivo= character(),
                           iteracion_bayesiana=  integer(),
                           ganancia=  numeric(),
                           corte=  integer() )


#genero los cortes
for( corte in cortes )
{
  tb_final[  , (PARAM$const$campo_pred) := 0L ]
  tb_final[ 1:corte, (PARAM$const$campo_pred) := 1L ]

  nom_submit  <- paste0( prefijo_submit,
                         "_",
                         sprintf( "%05d", corte ),
                         ".csv" )

  fwrite(  tb_final[ , c( PARAM$const$campo_id, PARAM$const$campo_pred) , with=FALSE ],
           file= nom_submit,
           sep= "," )


  #grabo la tabla de los nombres de los submits
  tb_submits  <- rbind( tb_submits,
                        list( nom_submit,
                              -1,
                              -1,
                              corte )  )

  fwrite( tb_submits,
          file= PARAM$files$output$tb_submits,
          sep= "\t"  )
}


# grabo catalogo   ------------------------------------------------------------
# es lo ultimo que hago, indica que llegue a generar la salida
#no todos los archivos generados pasan al catalogo

exp_catalog_add( action= "HBSC",
                 type=   "file",
                 key=    "predicciones",
                 value = PARAM$files$output$tb_predicciones  )

exp_catalog_add( action= "HBKA",
                 type=   "file",
                 key=    "kaggle_submits",
                 value = PARAM$files$output$tb_submits  )


#finalizo el experimento
#HouseKeeping
exp_finalizar( )
