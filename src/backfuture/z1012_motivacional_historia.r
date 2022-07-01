# Experimento DOS,  variabilidad de la semilla
#  entrena en   { [202001, 202011] menos 202006 }
#  entrena con  lags1  y  delta_lag1

# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM
# 256 GB espacio en disco
# tiempo de corrida 28 horas

# el resultado queda en  el bucket en  ./exp/EX10120/ 


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")

#Se utilizan las semillas de alumnos de ITBA2022  cohorte A
ksemillasITBA  <- c( 100003, 100019, 111317, 111323, 131071,  #Tobias Lichtig
                     200891, 445157, 574817, 700001, 899401,  #Bianca Balzarini
                     130729, 312679, 573341, 730487, 997973,  #Loris Sciacchitano
                     103087, 103391, 103613, 104087, 104309,  #Fabian Ghi
                     200443, 200461, 200467, 200569, 200587,  #Martin Ezequiel Ferradas
                     123997, 107971, 111997, 221987, 334991,  #Juan Leiva
                     128339, 131707, 135391, 136337, 137947,  #Ramiro Vozzi
                     111599, 111611, 111623, 111637, 111641,  #Kevin Hansen
                     122697, 222634, 331623, 441637, 551641,  #Justo Luis Erize
                     100049, 100129, 100151, 235951, 297457,  #Ariel Krysa
                     240007, 625643, 825329, 910421, 345689,  #Rodrigo Fondato
                     102667, 102983, 103969, 104327, 104089,  #Maria Daniela Raffo Mastricola
                     333333, 343434, 353535, 363636, 373737,  #Tomas Garcia
                     958381, 391873, 735389, 486037, 260747,  #Martin Kazimierski
                      52511,  52517,  52529,  52541,  52543,  #Natalia Debandi   mmmmmm
                     973033, 971171, 773117, 777979, 337313,  #Manuel Martinez
                     102199, 102433, 103483, 104033, 104323,  #Hernan Martinez Reumann
                     104233, 102523, 102451, 101573, 101531,  #Casal Juan Pablo
                     100169, 100183, 100189, 100193, 100207,  #Manuel David Vargas Russo
                     200001, 200007, 500001, 999001, 999007,  #Manuel Cassiani
                     230701, 301255, 595781, 198723, 379107,  #Flavia Munafo
                     131303, 141403, 151531, 161611, 171713,  #Daniela Canete
                     525387, 444022, 788065, 609573, 247440,  #Carolina Dieguez
                     280103, 200227, 100693, 130399, 150649,  #Diego Pabon
                     102191, 200177, 410551, 552581, 892237   #Gustavo Denicolay
                   )

#------------------------------------------------------------------------------
#esta funcion calcula internamente la ganancia de la prediccion probs

fganancia_lightgbm   <- function( probs, datos) 
{
  vlabels  <- get_field(datos, "label")
  vpesos   <- get_field(datos, "weight")

  tbl  <- as.data.table( list( "prob"= probs,
                               "peso"= vpesos,
                               "clase01"= vlabels ) )
  
  setorder( tbl, -prob )
  tbl[ , gan_acum := cumsum( ifelse( clase01== 1 & peso>1, 59000, -1000 ) ) ]

  gan  <- max( tbl$gan_acum )

  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui comienza el programa

#Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/paquete_premium.csv.gz", stringsAsFactors= TRUE)


#ordeno el dataset por  < numero_de_cliente, foto_mes > ´para calcular los lags
setorder( dataset, numero_de_cliente, foto_mes )

#creo los campos lags de orden 1
columnas_lag  <- setdiff( colnames(dataset), c("numero_de_cliente","foto_mes","clase_ternaria") )
nlag  <- 1      #orden del lag
sufijo  <- "_lag1"
dataset[ , paste0( columnas_lag, sufijo) := shift( .SD, nlag, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= columnas_lag ]

#creo los delta lags
sufijodelta  <- paste0( "_delta", nlag )

#uso un espantoso for para crear los delta lags
for( vcol in columnas_lag )
{
  dataset[,  paste0(vcol, sufijodelta) := get( vcol)  - get(paste0( vcol, sufijo))]
}

#--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#--------------------------------------

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )

#establezco donde entreno, desde enero a noviembre,  SIN  junio-2020
#entreno en la UNION de 11 meses
dataset[ , train  := 0L ]
dataset[ foto_mes >= 202001 & foto_mes<=202011 & foto_mes != 202006, 
         train  := 1L ]

#--------------------------------------
#creo las carpetas donde van los resultados
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/EX10120/", showWarnings = FALSE )
setwd( "./exp/EX10120/" )


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data=   data.matrix(  dataset[ train==1L, campos_buenos, with=FALSE]),
                        label=  dataset[ train==1L, clase01],
                        weight= dataset[ train==1L, ifelse(clase_ternaria=="BAJA+2", 1.000000001, 1.0)] ) 

#el dataset donde aplico el modelo,  enero-2021
dapply  <- copy( dataset[ foto_mes== 202101 ] )

#borro el dataset original y hago espacio
rm( dataset )
gc()

#Tabla donde guardo las ganancia del 5-fold Cross Validation
tabla_cv  <- data.table( semilla=  integer(),
                         ganancia= numeric() )


for( semilla  in  ksemillasITBA )
{

  #estos hiperparametos salen de la corrida motivacional Tarea para el Hogar TRES
  #  script  z652_lightgbm_motivacional.r
  parametros  <- list( objective=             "binary",
                       metric=                "custom",
                       max_bin=                  31,
                       learning_rate=             0.0300696989,
                       num_iterations=          567,
                       num_leaves=             1002,
                       min_data_in_leaf=       6263,
                       feature_fraction=          0.9100319271,
                       seed=                  semilla,         # Esta es la semilla que va variando !
                       verbose=                -100
                     )


  #aqui hago el cross validation
  modelo_cv  <- lgb.cv( data= dtrain,
                        eval= fganancia_lightgbm,
                        stratified= TRUE, #sobre el cross validation
                        nfold= 5,         #folds del cross validation
                        param= parametros
                      )

  ganancia_cv  <- unlist(modelo_cv$record_evals$valid$ganancia$eval)[ 567 ]  #tomo la ULTIMA iteracion
  ganancia_cv_normalizada  <-  ganancia_cv* 5     #normailizo la ganancia , 5 es la cantidad de folds

  #agrego a tabla_cv
  tabla_cv  <- rbind( tabla_cv, list( "semilla"=  semilla,
                                      "ganancia"= ganancia_cv_normalizada ) )

  fwrite( tabla_cv,
          file= "tabla_cv.txt",
          sep= "\t" )


  #genero el modelo entrenando en TODOS los datos
  modelo_final  <- lgb.train( data= dtrain,
                              param= parametros )

  #--------------------------------------

  #aplico el modelo a los datos nuevos
  prediccion  <- predict( modelo_final, 
                          data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )

  #genero la tabla de probabilidades
  tb_entrega  <- dapply[ , list( numero_de_cliente ) ]
  tb_entrega[ , prob :=  prediccion ]

  #gabo la prediccion del modelo, que contiene las probabilidades
  #este archivo NO se pude subir a Kaggle, primero hay que poner los 1's y 0's
  fwrite( tb_entrega, 
          file= paste0( "modelo712_", semilla ,".csv" ),
          sep= "\t" )

}
