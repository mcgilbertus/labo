#Arma la Training Strategy
#parte del dataset con feature engineering
#  1. dataset de future, donde se va a aplicar el modelo, son los datos que NO tiene clase
#  2. dataset de la train_strategy, donde estan marcados los registros de training, validation y testing
#        puede no considerar algunos meses, hacer undersampling de las clases, etc
#        hace falta mucha creatividad para esta estapa, decidir donde entrar es MUY ESTRATEGICO y esta sometido a las limitaciones de procesamiento
#  3. dataset de  train_final  donde se va a entrenar el modelo final una vez que se tengan los mejores hiperparametros de la Bayesian Optimization


#Necesita para correr en Google Cloud
# 64 GB de memoria RAM
#300 GB de espacio en el disco local
#  8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

source( "~/labo/src/lib/exp_lib.r" )

#------------------------------------------------------------------------------
#particiona en el dataset una seccion  del yaml

aplicar_particion  <- function( seccion )
{
  columna_nueva  <- paste0( "part_", seccion)
  dataset[  , (columna_nueva) := 0L ]

  if( length( PARAM[[seccion]]$periodos ) > 0 )
  {
    dataset[ get( PARAM$const$periodo ) %in%  PARAM[[seccion]]$periodos ,
             (columna_nueva)  := 1L ]
  } else {

     dataset[ get( PARAM$const$periodo ) >= PARAM[[seccion]]$rango$desde  &  get(PARAM$const$periodo)  <= PARAM[[seccion]]$rango$hasta,
              (columna_nueva)  := 1L ]

  }

  if( length( PARAM[[seccion]]$excluir ) > 0 )
  {
    dataset[ get( PARAM$const$periodo ) %in%  PARAM[[seccion]]$excluir , 
             (columna_nueva) := 0L ]
  }


  if( "undersampling" %in% names( PARAM[[seccion]] ) )
  {
    for( clase_valor  in  PARAM[[seccion]]$undersampling )
    {

       dataset[ get(columna_nueva)==1L & get(PARAM$const$clase) == clase_valor$clase  & part_azar > clase_valor$prob,
                (columna_nueva) := 0L  ]
                  
    }
  }

}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
#el input es SIEMPRE un dataset con Feature Engineering

exp_iniciar( )

#cargo el dataset
nom_arch  <- exp_nombre_archivo( PARAM$files$input$dentrada )
dataset   <- fread( nom_arch )


#ordeno el dataset por <foto_mes, numero_de_cliente> 
setorderv( dataset, PARAM$const$campos_sort )


set.seed( PARAM$semilla )  #uso la semilla
dataset[ , part_azar  := runif( nrow(dataset) ) ]   #genero variable azar, con la que voy a particionar


#hago las particiones de cada seccion
#las secciones son  future,   train, validate, test,  train_final

for( seccion  in  PARAM$const$secciones )
{
  aplicar_particion( seccion )
}


dataset[ , part_azar := NULL ]  #ya no necesito el campo  part_azar

psecciones  <- paste0( "part_", PARAM$const$secciones )

#genero el archivo de control, que DEBE ser revisado
tb_control  <- dataset[ , .N, 
                        psecciones]

fwrite( tb_control,
        file= PARAM$files$output$control,
        sep= "\t" )



#Grabo Future
if( 0 < dataset[ part_future>0, .N ] )
{
  #Grabo future
  fwrite( dataset[ part_future>0,
                   setdiff( colnames( dataset ) , 
                            c( psecciones, PARAM$const$clase ) ),
                   with= FALSE ] ,
        file= PARAM$files$output$future_data,
        logical01 = TRUE,
        sep= "," )
}


#Grabo train_strategy
if( 0 < dataset[ part_train>0 | part_validate>0 | part_test>0, .N ] )
{
  fwrite( dataset[ part_train>0 | part_validate>0 | part_test>0,
                   setdiff( colnames( dataset ) , c("part_future","part_train_final") ),
                   with= FALSE ] ,
          file= PARAM$files$output$train_strategy,
          logical01 = TRUE,
          sep= "," )
}


#Grabo train_final
if( 0 < dataset[ part_train_final > 0, .N ] )
{
  fwrite( dataset[ part_train_final > 0,
                   setdiff( colnames( dataset ),
                            psecciones ),
                   with= FALSE ] ,
          file= PARAM$files$output$train_final,
          logical01 = TRUE,
          sep= "," )
}



#grabo catalogo   -------------------------------------------------------------
# es lo ultimo que hago, indica que llegue a generar la salida
# no todos los archivos generados pasan al catalogo

exp_catalog_add( action= "TS",
                 type=   "file",
                 key=    "train_strategy",
                 value = PARAM$files$output$train_strategy )

exp_catalog_add( action= "TS",
                 type=   "file",
                 key=    "train_final",
                 value = PARAM$files$output$train_final )

exp_catalog_add( action= "TS",
                 type=   "file",
                 key=    "future_data",
                 value = PARAM$files$output$future_data )


#finalizo el experimento
#HouseKeeping
exp_finalizar( )
