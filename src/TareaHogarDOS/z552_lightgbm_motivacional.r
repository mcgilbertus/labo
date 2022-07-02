# LightGBM  Motivacional
# para motivar a los alumnos a hacer la  "Tarea Hogar DOS"
# viendo que desde el inicio de la tarea logran ganancias superadoras
# la salida queda en  "./labo/exp/KA552/KA_552_001.csv"

#los DOS puntos novedosos que se ven en este script
# 1. Se entrena  con  POS = { BAJA+1, BAJA+2 }    los BAJA+1 en realidad estan mas enfermos que los BAJA+2
#    Era forzar mucho al algoritmo agrupar los BAJA¿1 con los CONTINUA 
# 2. El punto anterior obliga a buscar una probabilidad de corte DISTINTA  a 1/60

# utilizar la primer semilla propia

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")


ksemilla  <- 300017  #poner aqui la PRIMERA de sus cinco semillas

#Aqui se debe poner la carpeta de la computadora local
setwd("d:\\archi\\ernesto\\mmd\\Aplicaciones_a_Economia_y_Finanzas")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/paquete_premium_202011.csv", stringsAsFactors= TRUE)


#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01 )


#genero el modelo con los parametros por default
#estos hiperparametros  salieron de una Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=        "binary",
                                   max_bin=              31,
                                   learning_rate=     0.067,
                                   num_iterations=      128,
                                   num_leaves=          100,
                                   min_data_in_leaf=   1700,
                                   feature_fraction=   0.37,
                                   seed=               ksemilla   #aqui se utiliza SU primer semilla
                                  )
                    )

#aplico el modelo a los datos sin clase
dapply  <- fread("./datasets/paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )


#Genero la entrega para Kaggle
#Atencion ya NO corto por  1/60,  sino que busque el punto de corte optimo
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= as.integer(prediccion > 0.023)   ) ) #ATENCION  no es  1/60

#guardo el resultado
#creo las carpetas
dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/KA5520/", showWarnings = FALSE )
setwd( "./labo/exp/KA5520/" )

archivo_salida  <- "KA_552_001.csv"

#genero el archivo para Kaggle
fwrite( entrega, 
        file= archivo_salida, 
        sep= "," )


#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "552_importancia_001.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )


#cuento cuantos 1's tiene la prediccion
#cuantos estimulos estoy enviando para retener clientes
entrega[  , sum( Predicted ) ]
