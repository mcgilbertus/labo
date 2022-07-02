#esqueleto de grid search
#se espera que los alumnos completen lo que falta para recorrer TODOS cuatro los hiperparametros

rm(list = ls())  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")
require("parallel")

ksemillas  <- c(300017, 300073, 300149, 300187, 300239)

#------------------------------------------------------------------------------
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA)
{
  if (!is.na(seed)) set.seed(seed)
  
  bloque  <- unlist(mapply(
    function(x, y) {
      rep(y, x)
    },
    division,  
    seq(from = start, length.out = length(division))))
  
  data[, (campo) :=  sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
       by = agrupa]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia  <- function(semilla, param_basicos)
{
  #particiono estratificadamente el dataset
  particionar(data=dataset, division = c(70, 30), agrupa = "clase_ternaria", seed = semilla)
  
  #genero el modelo
  modelo <- rpart("clase_ternaria ~ .",   #quiero predecir clase_ternaria a partir del resto
      data = dataset[fold == 1], #fold==1  es training,  el 70% de los datos
      xval = 0,
      control = param_basicos)   #aqui van los parametros del arbol
  
  #aplico el modelo a los datos de testing
  prediccion <- predict(modelo,   #el modelo que genere recien
                        dataset[fold == 2],  #fold==2  es testing, el 30% de los datos
                        type = "prob") #type= "prob"  es que devuelva la probabilidad
  
  #prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  #cada columna es el vector de probabilidades
  
  
  #calculo la ganancia en testing que es fold==2
  ganancia_test <- dataset[fold == 2, 
                           sum(ifelse(prediccion[, "BAJA+2"]  >  1 / 60,
                                      ifelse(clase_ternaria == "BAJA+2", 59000,-1000),0))]
  
  #escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / 0.3
  
  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------

ArbolesMontecarlo  <- function(semillas, param_basicos)
{
  #la funcion mcmapply  llama a la funcion ArbolEstimarGanancia tantas veces como valores tenga el vector semillas
  ganancias  <- mcmapply(
    ArbolEstimarGanancia,
    semillas, #paso el vector de semillas, que debe ser el primer parametro de la funcion ArbolEstimarGanancia
    MoreArgs = list(param_basicos), #aqui paso el segundo parametro
    SIMPLIFY = FALSE,
    mc.cores = 1)  #se puede subir a 5 si posee Linux o Mac OS
  
  ganancia_promedio <- mean(unlist(ganancias))
  
  return(ganancia_promedio)
}
#------------------------------------------------------------------------------

#Aqui se debe poner la carpeta de la computadora local
setwd("d:\\archi\\ernesto\\mmd\\Aplicaciones_a_Economia_y_Finanzas")   #Establezco el Working Directory

#cargo los datos
dataset  <- fread("./datasets/paquete_premium_202011.csv")


#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("./labo/exp/",  showWarnings = FALSE)
dir.create("./labo/exp/HT2020/", showWarnings = FALSE)
archivo_salida  <- "./labo/exp/HT2020/gridsearch.txt"

#Escribo los titulos al archivo donde van a quedar los resultados
#atencion que si ya existe el archivo, esta instruccion LO SOBREESCRIBE, y lo que estaba antes se pierde
#la forma que no suceda lo anterior es con append=TRUE
cat(
  file = archivo_salida,
  sep = "",
  "cp",
  "\t",
  "max_depth",
  "\t",
  "min_split",
  "\t",
  "min_bucket",
  "\t",
  "ganancia_promedio",
  "\n"
)

#params <- list("cp"=0.01,"minsplit"=100,"minbucket"=20,"maxdepth"=5)
#ArbolesMontecarlo(ksemillas, params)
#ArbolEstimarGanancia(ksemillas[1],params)


#itero por los loops anidados para cada hiperparametro
for (vmax_depth  in  c(4, 8, 12, 16, 20))
{
  for (vcp in c(-1)) #, 0.05, 0.1, 0.5, 0, -0.5))
  {
    for (vmin_split  in  c(1000, 800, 600, 400, 200, 100, 50, 20, 10))
    {
      for (vmin_bucket in seq(from=3, to=trunc(vmin_split/2), by=trunc(vmin_split/10)))
      {
        param_basicos  <-
          list(
            "cp" = vcp, #complejidad minima
            "minsplit" = vmin_split, #minima cantidad de registros en un nodo para hacer el split
            "minbucket" = vmin_bucket, #minima cantidad de registros en una hoja
            "maxdepth" = vmax_depth) #profundidad máxima del arbol
        print(param_basicos)
        #print(paste(names(param_basicos),param_basicos,sep = ': '))

        ganancia_promedio <- ArbolesMontecarlo(ksemillas, param_basicos)
        
        print(paste("ganancia: ",ganancia_promedio,"\n"))
        
        #escribo los resultados al archivo de salida
        cat(
          file = archivo_salida,
          append = TRUE,
          sep = "",
          vcp,
          "\t",
          vmax_depth,
          "\t",
          vmin_split,
          "\t",
          vmin_bucket,
          "\t",
          ganancia_promedio,
          "\n"
        )
        
      }
    }
  }
}
