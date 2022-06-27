require("rlist")
require("yaml")
require("data.table")
require("mlflow")


options(show.error.locations = TRUE)


options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})


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
#------------------------------------------------------------------------------
#Aqui empieza el programa

exp_mlflow_iniciar()

args  <- commandArgs( trailingOnly= TRUE ) 

#creo el experimento del usuario
user_st  <-  Sys.info()["user"]
exp_st   <- paste0( "/runs" )
mlflow_set_experiment( exp_st )   #si ya esta creado, no pasa nada

#Seteo el tag del nombre de usuario
mlflow_set_experiment_tag( key= "user", value= user_st )

res  <- mlflow_start_run( nested= TRUE )

mlflow_log_param( key= "user", value= user_st )
mlflow_log_param( key= "experiment", value= args[1] )
mlflow_log_param( key= "machine",    value= Sys.info()["nodename"] )
mlflow_log_param( key= "SH_START",   value= format(Sys.time(), "%Y%m%d %H%M%S") )

#grabo el id del experimento
write_yaml( res,  file= "run.yml") 

archivos  <- list.files( path = ".", pattern="yml" )
for( archivo in archivos )  mlflow_log_artifact( path= archivo )


quit( save= "no" )
