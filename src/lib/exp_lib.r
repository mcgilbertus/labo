#libreria del ambiente de experimentos para correr en Google Cloud
# solo correr en Google Cloud, intencionalmente NO corre en Windows
#en un ambiente productivo esto deberia ser un paquete de R
#  ahora es un simple script .R para facilitar la vida de los alumnos

require("rlist")
require("yaml")
require("data.table")
require("mlflow")


#para que muestre el lugar del error
options(show.error.locations = TRUE)

#para que se DETENGA  ante un error, y muestre el stack
options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})


#------------------------------------------------------------------------------
#variables globales

EXP_MLFLOW_INICIADO  <- FALSE

#------------------------------------------------------------------------------
#inicializo el ambiente de mlflow

exp_mlflow_iniciar  <- function()
{
  #leo uri, usuario y password
  MLFLOW  <<- read_yaml( "/media/expshared/mlflow.yml" )

  Sys.setenv( MLFLOW_TRACKING_USERNAME= MLFLOW$tracking_username )
  Sys.setenv( MLFLOW_TRACKING_PASSWORD= MLFLOW$tracking_password )
  mlflow_set_tracking_uri( MLFLOW$tracking_uri )

  Sys.setenv( PATH=paste0( "/home/", Sys.info()["user"], "/.local/bin:", 
                           Sys.getenv("PATH")) )

  Sys.setenv(MLFLOW_BIN= Sys.which("mlflow") )
  Sys.setenv(MLFLOW_PYTHON_BIN= Sys.which("python3") )
  Sys.setenv(MLFLOW_TRACKING_URI= MLFLOW$tracking_uri, intern= TRUE )
}
#------------------------------------------------------------------------------

exp_mlflow_script_start  <- function()
{

  res  <- read_yaml( "run.yml" )

  mlflow_log_param( run_id= res$run_uuid, 
                    key= "SCRIPT_START", 
                    value= format(Sys.time(), "%Y%m%d %H%M%S") )
}
#------------------------------------------------------------------------------

exp_mlflow_script_end  <- function()
{

  res  <- read_yaml( "run.yml" )

  mlflow_log_param( run_id= res$run_uuid, 
                    key= "SCRIPT_END", 
                    value= format(Sys.time(), "%Y%m%d %H%M%S") )
}
#------------------------------------------------------------------------------


#Estoy al inicio del log, luego de grabar los titulos
#inicializo el ambiente de mlflow

exp_mlflow_iniciar_log  <- function()
{
  #creo el experimento
  user_st  <-  Sys.info()["user"]
  FE_st    <- tb_catalogo[ action=="FE" , experiment ]
  TS_st    <- tb_catalogo[ action=="TS" , unique( experiment ) ]
  exp_st   <- paste0( "/", user_st, "/", FE_st, "/", TS_st )
  mlflow_set_experiment( exp_st )   #si ya esta creado, no pasa nada

  #agrego tags  al  experimento, si ya estan creados no pasa nada
  mlflow_set_experiment_tag( key= "user", value= user_st )
  mlflow_set_experiment_tag( key= "FE", value= "FE_st" )
  mlflow_set_experiment_tag( key= "TS", value= "TS_st" )

  #inicio el  run
  #res  <- mlflow_start_run( nested= TRUE )

  #agrego tags al  run
  #HT_st  <- EXP$experiment$name
  #mlflow_set_tag( key="HT", value= HT_st )
  #mlflow_log_artifact( paste0( getwd(), "/", EXP$experiment$name, ".yml") )

}
#------------------------------------------------------------------------------

exp_finalizar  <- function( suicide= TRUE)
{
  #termino de correr el script
  exp_mlflow_script_end()

  #cierro el run  de mlflow  en caso que exista
  if( exists( "MLFLOW_LOG" ) )  mlflow_end_run( run_id= MLFLOW_LOG$run_uuid, status= "FINISHED" )
  
  #agrego al log.txt que el script R termino
  st  <- paste0( EXP$experiment$name, "\t",
                 format(Sys.time(), "%Y%m%d %H%M%S"),"\t",
                 "SCRIPT_END", "\n" )

  cat( st,
       file=   "log.txt",
       append= TRUE  )

  quit( save= "no" )  #salgo de R
}
#------------------------------------------------------------------------------
#cargo los catalogos de los experimentos requeridos por mi experimento

exp_cargar_catalogos  <- function( )
{
  pref  <- EXP$environment$exp_dir

  for( depende  in EXP$experiment$requires )
  {
    nom_arch  <-  paste0( pref,
                          depende,
                          "/",
                          EXP$environment$catalog  )

    tb_cataloguito  <- fread( nom_arch )

    if( !exists( "tb_catalogo" ) )   {
      tb_catalogo  <<- tb_cataloguito
    } else {
      tb_catalogo  <<- rbind( tb_catalogo, tb_cataloguito )
    }
  }

  tb_catalogo[  , distance  := distance + 1 ]
  setorder( tb_catalogo, distance )
}

#------------------------------------------------------------------------------
#genero el nombre de un archivo a partir del catalogo y la key que esta en files$input

exp_nombre_archivo  <- function( clave )
{
  nom_arch <- as.character( NA )

  if( tb_catalogo[ distance==2 & key==clave, .N ]  >= 1 )   {
    nom_arch  <- tb_catalogo[ distance==2 & key==clave, value ]
  }  else {

    if( tb_catalogo[ distance>2 & key==clave, .N ]  >= 1 )
    {
      tbl  <- tb_catalogo[ distance>2 & key==clave, ]
      nom_arch  <- tbl[ 1, value ]
    }
  }

  return( nom_arch)
}

#------------------------------------------------------------------------------

exp_trace_log  <- function()
{
  malos  <- 0
  total  <- sys.nframe()
  #for( i in  1:total ) 
  #{
  #  malos <- malos +  startsWith( deparse(sys.calls()[[i]]), "exp_" )
  #}

  if( sum(malos) >= 0 )
  {
    cat( sys.nframe() -5,  # 1 va a significar programa principal
         "\t",
         deparse(sys.calls()[[sys.nframe()-5]]),
         "\n",
         file= "~/log/tracelog.txt",
         append= TRUE)
  }
}
#------------------------------------------------------------------------------
#inicio de los experimentos
#debe ser llamado al comienzo de un script que usa experimentos

exp_iniciar  <- function( exp_nombre= NA )
{

  exp_mlflow_iniciar()       #inicio del ambiente MLFlow
  exp_mlflow_script_start()  #inicio el run del script

  #Hook de trace() en las funciones que me interesan
  #exp_trace_hook()  #inicio el tracing  posibles  <- as.vector( lsf.str(pos= 1) )   # 1 para que sea el environment abuelo que llamo a esta funcion
  posibles  <- as.vector( lsf.str(pos= 1) )   # 1 para que sea el environment abuelo que llamo a esta funcion
  posibles  <- posibles[ !( startsWith( posibles , "exp_" )) ]
  posibles  <- setdiff( posibles, c( "fganancia_lgbm_meseta", "fganancia_lgbm_mesetaCV" ) )

  for( fu in  posibles  )
  {
    trace( what= fu,
           tracer= exp_trace_log,
           print= FALSE)
  }


  adicionales  <- c( "require", "library", "setwd", "set.seed", "runif" )
  if( isNamespaceLoaded("data.table") )  adicionales <- c( adicionales, "fread", "fwrite") 
  if( isNamespaceLoaded("lightgbm") )    adicionales <- c( adicionales, "lgb.train", "lgb.cv", "lgb.importance", "lgb.Dataset" ) 
  if( isNamespaceLoaded("mlr") )         adicionales <- c( adicionales, "makeLearner" ) 
  if( isNamespaceLoaded("mlrMBO") )      adicionales <- c( adicionales, "mbo", "mboContinue", "setMBOControlInfill", "setMBOControlTermination", "makeMBOControl", "configureMlr" ) 
  if( isNamespaceLoaded("smoof") )       adicionales <- c( adicionales, "makeSingleObjectiveFunction" ) 


  for( fu in adicionales )
  {
    trace( what= fu,
           tracer= exp_trace_log,
           print= FALSE )
  }

  #leo los parametros con los que se llamo al script  R
  if( is.na(exp_nombre ) ) {
    args  <- commandArgs( trailingOnly= TRUE ) 
  }  else {
    args  <- c( exp_nombre )
  }

  #el primer parametro tiene el nombre del  yml
  EXP  <<- read_yaml( paste0( args[1], ".yml") )
  EXP$experiment$name  <<- args[1]

  PARAM  <<- EXP$param

  exp_cargar_catalogos()

  #escribo al log de experimentos que comenzo el SCRIPT
  linea  <- paste0( EXP$experiment$name,
                    "\t",
                    format(Sys.time(), "%Y%m%d %H%M%S"),
                    "\t",
                   "SCRIPT_START\n" )

  cat( linea,
       file= "log.txt",
       append= TRUE )

}
#------------------------------------------------------------------------------
#para homogeneidad en donde un experimento solo requiere experimentos
# el dataset inicial se carga como un  experimento, y se copia a su carpeta DD0001
# tiene incluso su propio catalogo y log.txt

exp_crear_DT0001  <- function( experimento, carpeta_datasets, dataset_inicial )
{
  if( !dir.exists( carpeta_datasets ) )  raise_error( paste0( "No exists la carpeta: ", carpeta_datasets ) )

  archivo_original  <- paste0( carpeta_datasets, dataset_inicial )
  if( !file.exists( archivo_original ) )  raise_error( paste0( "No exists el archivo : ", archivo_original ) )

  carpeta_destino  <- paste0( experimento$environment$exp_dir, "DT0001/" )
  if( !dir.exists( carpeta_destino ) )
  {
    res  <- dir.create(  carpeta_destino,  showWarnings= FALSE )
    if( res == FALSE )  raise_error( paste0( "No se pudo crear la carpeta: ", carpeta_destino ))
  }

  #copio el archivo
  archivo_destino  <- paste0( carpeta_destino, dataset_inicial )
  if( !file.exists( archivo_destino )  )
  {
    res  <- file.copy( archivo_original, carpeta_destino )
    if( res==FALSE )  raise_error( paste0( "No se pudo crear el archivo: ", archivo_destino ))
  }

  #creo el catalogo
  archivo_catalogo  <- paste0( carpeta_destino, "catalogo.txt" )
  if( !file.exists( archivo_catalogo ) )
  {
    tb_catalogo  <- list( distance= 1L,
                          action= "DT",
                          experiment= "DT0001",
                          type= "file",
                          key= "dataset",
                          value= paste0( carpeta_destino, dataset_inicial ) )

    fwrite( as.data.table( tb_catalogo ),
            file= archivo_catalogo,
            sep= "\t" )

    #creo el log
    tb_log  <- list( experiment= "DT0001",
                     timestamp= format(Sys.time(), "%Y%m%d %H%M%S"),
                     event= "SCRIPT_END" )

    fwrite( as.data.table( tb_log ),
            file= paste0( carpeta_destino, "log.txt" ),
            sep= "\t" )

    #quizas en este punto se deba agregar el log.txt el SH_END ... quien sabe ...
  }

}
#------------------------------------------------------------------------------
#verifico que los requires existan y tengan catalogos

exp_verificar_requires  <- function( experimento )
{
  requeridos  <- experimento$experiment$requires

  #verifico los experimentos
  for( requerido in requeridos )
  {
     carpeta  <- paste0( experimento$environment$exp_dir, requerido, "/" )
     if( !dir.exists( carpeta ) )  raise_error( paste0( "En  experimento.requires  no existe el experimento: ", requerido ) )

  }

  #verifico la existencia de catalogo.txt
  for( requerido in requeridos )
  {
     catalogo  <- paste0( experimento$environment$exp_dir, requerido, "/", "catalogo.txt" )
     if( !file.exists( catalogo ) )  raise_error( paste0( "En  experimento.requires  no existe el archivo: ", requerido ) )

     tb_cataloguito  <-  fread( catalogo )
     if( nrow( tb_cataloguito ) == 0 )  raise_error( paste0( "En experimento.requires  hay CERO regisrtros en: ", requerido ) )
  }


  for( requerido  in requeridos )
  {
    catalogo  <- paste0( experimento$environment$exp_dir, requerido, "/", "catalogo.txt" )
    tb_cataloguito  <- fread( catalogo )

    if( !exists( "tb_catalogo" ) )   {
      tb_catalogo  <- tb_cataloguito
    } else {
      tb_catalogo  <- rbind( tb_catalogo, tb_cataloguito )
    }
  }

  tb_catalogo[  , distance  :=  distance + 1 ]
  setorder( tb_catalogo, distance )

  if( "files" %in%  names( experimento[["param"]] ) )
  {
    if( "input"  %in%  names( experimento[["param"]][["files"]] ) )
    {
       for( input_file  in  names( experimento$param$files$input ) )
       {
          clave  <- experimento$param$files$input[[ input_file ]]
          if( tb_catalogo[ distance==2 & key==clave, .N ]  >= 1 )   {
            arch_vect  <- tb_catalogo[ distance==2 & key==clave, value ]
          } else  {
           if( tb_catalogo[ distance>2 & key==clave, .N ]  >= 1 )  {
             tbl  <- tb_catalogo[ distance>2 & key==clave, ]
             arch_vect  <- tbl[ 1, value ]
           }
          }

         for( arch  in arch_vect )
         {
           if( !file.exists( arch ) )  raise_error( paste0("En param.files.input.", input_file, " no existe el archivo ", arch ) )
         }
       }
    }
  }

}
#------------------------------------------------------------------------------
#la funcion que escribe el error y DENTIENE la ejecuacion

raise_error  <- function( perror )
{
  stop( perror, call. = FALSE)
}
#------------------------------------------------------------------------------
#dato un vector de palabras,  verifica que existan esas secciones en la lista

exp_verificar_secciones  <- function( lista, palabras, sufijo )
{
  componentes  <- names( lista )
  for( palabra in palabras )
  {
    if(  !(palabra  %in%  componentes ) ) raise_error( paste0( "No existe la seccion: ", palabra, "  ",  sufijo ) )
  }
}
#------------------------------------------------------------------------------
# verifica que exista en disco la carpeta indicada por la lista$seccion1$seccion2

exp_verificar_seccion_carpeta  <- function( lista, seccion1, seccion2 )
{
  carpeta  <- lista[[seccion1]][[seccion2]]

  if( !dir.exists( carpeta ) ) raise_error( paste0( "En ", seccion1, ".", seccion2, " no existe la carpeta : " , carpeta ) )
}
#------------------------------------------------------------------------------
# verifica que lista$seccion1$seccion2 sea un numero entero y este en  [ desde, hasta ]

exp_verificar_seccion_integer  <- function( lista, seccion1, seccion2, desde, hasta )
{
  valor  <- lista[[seccion1]][[seccion2]]

  if( !is.integer( valor )  )  raise_error( paste0( "el valor de ", seccion1, ".", seccion2, " es : ", valor, " PERO debe ser un numero entero"  ) )
  if( valor < desde  | valor > hasta )  raise_error( paste0( "el valor de ", seccion1, ".", seccion2, " es : ", valor, " PERO debe estar entre ", desde, " y ",  hasta  ) )
}
#------------------------------------------------------------------------------
#verifica el entorno de experimentos, y el experimento  exp_name

exp_verificar  <- function( exp_name, repo_dir, exp_dir )
{
  if( is.na(exp_name) )  raise_error( "Nombre de experimento vacio" )
  if( is.na(repo_dir) )  raise_error( "Nombre de repo vacio" )
  if( is.na(exp_dir) )   raise_error( "Nombre del exp_dir vacio" )

  if( !dir.exists( repo_dir ) )  raise_error( paste0( "No existe la carpeta del repositorio: " , repo_dir ) )
  if( !dir.exists( exp_dir ) )   raise_error( paste0( "No existe la carpeta de los experimentos: " , exp_dir ) )

  repo_exps_dir  <- paste0( repo_dir, "exp/" )
  if( !dir.exists( repo_exps_dir ) )   raise_error( paste0( "En el repo no existe la carpeta de los experimentos: " , repo_exps_dir ) )

  repo_exp_dir  <- paste0( repo_dir, "exp/", exp_name, "/" )
  if( !dir.exists( repo_exp_dir ) )   raise_error( paste0( "No existe en el repositorio la carpeta del experimento: " , repo_exp_dir ) )

  archivo_experimento  <- paste0( repo_exp_dir, exp_name, ".yml" )
  if( !file.exists( archivo_experimento ) )
  {
    archivos  <- list.files( path= repo_exp_dir, pattern= "*.yml" )
    if( length( archivos ) == 0 )  raise_error( paste0( "No existe ningun archivo .yml en la carpeta : " , repo_exp_dir ) )
    if( length( archivos )  > 1 )  raise_error( paste0( "Existen muchos archivos .yml en la carpeta : " , repo_exp_dir ) )

    if( length( archivos ) == 1 )  archivo_experimento  <- paste0( repo_exp_dir, archivos[1] )
  }

  #cargo el experimento
  experimento  <- read_yaml( archivo_experimento )

  #Verifico sintaxis de la raiz
  exp_verificar_secciones( experimento, 
                           c( "googlecloud", "experiment", "param", "environment" ),
                           "a nivel raiz en el yml del experimento" )

  #Verifico sintaxis del environment
  exp_verificar_secciones( experimento[["environment"]], 
                           c( "repo_dir", "exp_dir", "catalog" ),
                           "en la seccion environment" )

  #Verifico sintaxis de experiment
  exp_verificar_secciones( experimento[["experiment"]], 
                           c( "script", "name", "requires" ),
                           "en la seccion experiment" )

  #Verifico sintaxis de googlecloud
  exp_verificar_secciones( experimento[["googlecloud"]], 
                           c( "RAM", "vCPU", "disk" ),
                           "en la seccion googlecloud" )

  #Verifico semantica del environment
  exp_verificar_seccion_carpeta( experimento, "environment", "repo_dir" )
  exp_verificar_seccion_carpeta( experimento, "environment", "exp_dir" )

  #Verifico semantica de google cloud
  exp_verificar_seccion_integer( experimento, "googlecloud", "RAM",    2, 512 )
  exp_verificar_seccion_integer( experimento, "googlecloud", "vCPU",   2,  32 )
  exp_verificar_seccion_integer( experimento, "googlecloud", "disk", 256, 512 )

  #Verifico semantica de experiment
  script  <- paste0( experimento[["environment"]][["repo_dir"]], experimento[["experiment"]][["script"]] )
  if( !file.exists( script ) )  raise_error( paste0( "No existe el script  experiment.script : " , script ) )

  #Dataset original como experimento DT0001 , si ya existe, no se recrea
  exp_crear_DT0001( experimento, "~/buckets/b1/datasets/", "paquete_premium.csv.gz" )

  #verifico requires
  exp_verificar_requires( experimento )

}
#------------------------------------------------------------------------------
#CORRE un Experimento
#esto esta armado para Linux

exp_start  <- function( exp_name= NA, deletevm= TRUE, repo_dir= "~/labo/", exp_dir= "~/buckets/b1/exp/" )
{
  exp_verificar( exp_name, repo_dir, exp_dir )

  #si ya existe la carpeta del experimento,  aborto
  exp_exp_dir  <- paste0( exp_dir, exp_name, "/" )
  if( dir.exists( exp_exp_dir ) )   raise_error( paste0( "debe llamar a  exp_restart() , ya existe la carpeta: " , exp_exp_dir ) )

  #creo la carpeta del experimento generalmente en  ~/buckets/b1/exp
  res  <- dir.create( exp_exp_dir,  showWarnings= FALSE )
  if( res == FALSE )  raise_error( paste0( "No se pudo crear la carpeta: ", exp_exp_dir )) 

  #creo la carpeta compartida

  user_dir  <- paste0( "/media/expshared/" , Sys.info()["user"] )
  dir.create( user_dir,  showWarnings= FALSE )

  userexp_dir  <- paste0( "/media/expshared/" , Sys.info()["user"], "/exp/" )
  dir.create( userexp_dir,  showWarnings= FALSE )
  
  shared_dir  <- paste0( "/media/expshared/" , Sys.info()["user"], "/exp/", exp_name , "/" )
  dir.create( shared_dir,  showWarnings= FALSE )

  #copio el archivo del experimento
  repo_exp_dir  <- paste0( repo_dir, "exp/", exp_name, "/" )
  archivo_experimento  <- paste0( exp_name, ".yml" )
  if( !file.exists( paste0( repo_exp_dir, archivo_experimento ) ) )
  {
    archivos  <- list.files( path= repo_exp_dir, pattern= "*.yml" )
    if( length( archivos ) == 0 )  raise_error( paste0( "No existe ningun archivo .yml en la carpeta : " , repo_exp_dir ) )
    if( length( archivos )  > 1 )  raise_error( paste0( "Existen muchos archivos .yml en la carpeta : " , repo_exp_dir ) )

    if( length( archivos ) == 1 )  archivo_experimento  <- paste0( archivos[1] )
  }

  archivo_original  <- paste0( repo_exp_dir, archivo_experimento )
  #copio el .yml del experimento generalmente a  ~/buckets/b1/exp/<experimento>
  res  <- file.copy( archivo_original, exp_exp_dir )
  archivo_destino  <- paste0( exp_exp_dir, archivo_experimento )
  if( res==FALSE )  raise_error( paste0( "No se pudo crear el archivo: ", archivo_destino )) 
  #dejo readonly el archivo
  Sys.chmod( archivo_destino, mode = "444", use_umask = TRUE)

  #copio el .yml del experimento generalmente a  ~/media/expshared/<usuario>/exp/<experimento>
  res  <- file.copy( archivo_original, shared_dir, overwrite= TRUE )
  archivo_destino  <- paste0( shared_dir, archivo_experimento )


  #me paro en la carpeta del experimento
  setwd( exp_exp_dir )

  #cargo el experimento
  experimento  <- read_yaml( archivo_experimento )
  script  <- paste0( experimento[["environment"]][["repo_dir"]], experimento[["experiment"]][["script"]] )

  #creo el shell script que voy a correr
  
  linea1  <- "tabulador=\"\t\"\n"
  linea2  <- paste0( "exp_name=", exp_name, "\n" )
  linea3  <- "echo \"experiment\ttimestamp\tevent\"  >  log.txt \n"
  linea4  <- "fecha0=$(date +\"%Y%m%d %H%M%S\") \n"
  linea5  <- "echo \"$exp_name\"\"$tabulador\"\"$fecha0\"\"$tabulador\"\"SH_START\" >> log.txt \n"

  linea6  <- paste0( "Rscript --vanilla ",
                      experimento[["environment"]][["repo_dir"]], 
                      "src/lib/exp_run_init.r",
                      "  ",
                      exp_name,
                      "\n" )

  #esta es la llamada al script de R que hace el trabajo
  linea7  <- paste0( "Rscript --vanilla ",
                     script,
                     "  " ,
                     exp_name,
                     "  2>&1 | tee outfile \n" )

  linea8  <- "cp  ~/log/*.txt  ./  \n"

  linea9  <- paste0( "Rscript --vanilla ", 
                      experimento[["environment"]][["repo_dir"]], 
                      "src/lib/exp_run_end.r",
                      "\n"  )
  
  linea10  <- "fecha1=$(date +\"%Y%m%d %H%M%S\") \n"
  linea11  <- "echo \"$exp_name\"\"$tabulador\"\"$fecha1\"\"$tabulador\"\"SH_END\" >> log.txt \n"

  #esta linea debe cambiarse por un rsync
  linea12  <- paste0( "find ./ ! -name \"*.gz\" ! -name . -exec cp -prt ",  shared_dir, "  {} +  \n")

  linea13  <- "\n#suicidio\n" 
  linea14  <- "export NAME=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/name -H 'Metadata-Flavor: Google') \n"
  linea15  <- "export ZONE=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/zone -H 'Metadata-Flavor: Google') \n"
  linea16  <- "gcloud --quiet compute instances delete $NAME --zone=$ZONE \n"
  linea_suicidio  <- paste0( linea13, linea14, linea15, linea16)
  if( deletevm==FALSE ) linea_suicidio <- "\n"

  comando  <- paste0( linea1, linea2, linea3, linea4, linea5,
                      linea6,
                      linea7,
                      linea8,
                      linea9, 
                      linea10, linea11, linea12, 
                      linea_suicidio )


  shell_script  <- paste0( exp_name, ".sh" )
  cat( comando, 
       file= shell_script )

  #doy permisos de ejecucion al shell script
  Sys.chmod( shell_script, mode = "544", use_umask = TRUE)

  #ejecuto en linux el script recien creado
  system( paste0( "./", shell_script ) )
}
#------------------------------------------------------------------------------
#Hace restart de un experimento

exp_restart  <- function( exp_name= NA, repo_dir= "~/labo/", exp_dir= "~/buckets/b1/exp/" )
{
  exp_verificar(  exp_name, repo_dir, exp_dir )

  #si no existe la carepta del experimento,  aborto
  exp_exp_dir  <- paste0( exp_dir, exp_name, "/" )
  if( !dir.exists( exp_exp_dir ) )   raise_error( paste0( "debe llamar a  exp_start() , ya que NO existe la carpeta: " , exp_exp_dir ) )

  archivo_experimento  <- paste0( exp_name, ".yml" )
  if( !file.exists( paste0( exp_exp_dir, archivo_experimento ) ) )
  {
    archivos  <- list.files( path= exp_exp_dir, pattern= "*.yml" )
    if( length( archivos ) == 0 )  raise_error( paste0( "No existe ningun archivo .yml en la carpeta : " , exp_exp_dir ) )
    if( length( archivos )  > 1 )  raise_error( paste0( "Existen muchos archivos .yml en la carpeta : " , exp_exp_dir ) )

    if( length( archivos ) == 1 )  archivo_experimento  <- paste0( archivos[1] )
  }

  #me paro en la carpeta del experimento
  setwd( exp_exp_dir )

  #cargo el experimento
  experimento  <- read_yaml( archivo_experimento )

  #veo si el experimento se puede restartaear
  if( "restart" %in%  names( experimento$experiment ) )
  {
    if( experimento$experiment$restart == FALSE )  raise_error( "El experimento no se puede restart experiment$restart=FALSE" )
  } else {
    raise_error( "El experimento no se puede restart, falta seccion experiment$restart" )
  }

  shell_script  <- paste0( exp_name, ".sh" )
  if( !file.exists( shell_script ) )   raise_error(  pste0("NO existe el script ejecutable ", shell_script) )


  #veo que no haya terminado
  tb_log  <- fread( "log.txt" )
  if( tb_log[ event== "SCRIPT_END",  .N ]  > 0 )  raise_error( "El experimento no se puede restart, existe evnto SCRIPT_END  en  log.txt" )

  #ahora todo dice que se puede restartear
  #podria estar corriendo !!  controlar esto es un ToDo , pero debera tener en cuenta que la maquinas virtuales se caen 

  #ejecuto el script recien creado
  system( paste0( "./", shell_script ) )

}

#------------------------------------------------------------------------------

exp_catalog_add  <- function( action, type, key, value )
{
  #si es un file, le agrego la ruta al inicio
  value_new  <- value
  if( type== "file" )
  {
    value_new  <- paste0( EXP$environment$exp_dir, 
                          EXP$experiment$name,
                          "/",
                          value )
  }

  #armo el registro que voy a agregar al catalogo
  catalogo  <- list( "distance"= 1,
                     "action"= action,
                     "experiment"=  EXP$experiment$name,
                     "type"= type,
                     "key"= key,
                     "value" =  value_new )

  #agrego el registro al catalogo
  tb_catalogo  <<- rbind( tb_catalogo, catalogo ) 

  #escribo a disco el catalogo
  fwrite( tb_catalogo,
          file= EXP$environment$catalog,
          sep=  "\t" )

}
#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos
#aqui se agregara  mlflow

exp_log  <- function( reg, arch=NA, folder="./", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )

  #Inicio mlflow de ser necesario
  if( ! EXP_MLFLOW_INICIADO )
  {
    exp_mlflow_iniciar_log( )
    EXP_MLFLOW_INICIADO  <<- TRUE
    MLFLOW_LOG  <<- mlflow_start_run( nested= TRUE )
  }


  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "experimento\t",
                      "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )

    cat( linea, file=archivo )
  }

  linea  <- paste0( EXP$experiment$name, "\t",
                    format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )

  cat( linea, file=archivo, append=TRUE )  #grabo al archivo

  if( verbose )  cat( linea )   #imprimo por pantalla

  #grabo mlflow

  #inicio el  run
  #res  <- mlflow_end_run( nested= TRUE )  VIEJO 

  #agrego tags al  run
  HT_st  <- EXP$experiment$name
  mlflow_set_tag( key="HT", value= HT_st )
  
  #seteo el nombre del experimento
  mlflow_log_param( "experimento", EXP$experiment$name )

  for( nombre  in  names( reg ) )
  {

    if( is.character( reg[[ nombre ]] )  ) {

      mlflow_log_param( nombre, reg[[ nombre ]] )

    } else {
    
      if( nombre != "ganancia"  )  mlflow_log_param( nombre, reg[[ nombre ]] )
      if( nombre == "ganancia"  )  mlflow_log_metric( nombre, reg[[ nombre ]] )
    }

  }

  #finalizo el  run
  mlflow_end_run( )

  #incio el run de la siguiente vuelta
  mlflow_start_run( nested= TRUE )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


# source( "~/labo/src/lib/exp_lib.r" )
# exp_start( "8101FEa" )
# exp_start( "8101TSa" )

# exp_start( "8101HTa" )
# exp_restart( "8101HTa" )

# exp_start( "8101FMa" )
# exp_start( "8101SCa" )
# exp_start( "8101KAa" )

# exp_start( "8101ZZa", deletevm= FALSE )
