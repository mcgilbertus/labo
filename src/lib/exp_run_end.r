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


res  <- read_yaml( "run.yml" )

mlflow_log_param( run_id= res$run_uuid, 
                  key= "SH_END", 
                  value= format(Sys.time(), "%Y%m%d %H%M%S") )

if( file.exists( "log.txt" ) )  mlflow_log_artifact( run_id= res$run_uuid, path= "./log.txt" )
if( file.exists( "tracelog.txt" ) )  mlflow_log_artifact( run_id= res$run_uuid, path= "./tracelog.txt" )
if( file.exists( "catalogo.txt" ) )  mlflow_log_artifact( run_id= res$run_uuid, path= "./catalogo.txt" )
if( file.exists( "BO_log.txt" ) )    mlflow_log_artifact( run_id= res$run_uuid, path= "./BO_log.txt" )
if( file.exists( "tb_modelos.txt" ) )    mlflow_log_artifact( run_id= res$run_uuid, path= "./tb_modelos.txt" )
if( file.exists( "tb_predicciones.txt" ) )    mlflow_log_artifact( run_id= res$run_uuid, path= "./tb_predicciones.txt" )
if( file.exists( "tb_submits.txt" ) )    mlflow_log_artifact( run_id= res$run_uuid, path= "./tb_submits.txt" )



if( file.exists( "outfile" ) )
{
  file.copy( "outfile", "outfile.txt" )
  if( file.exists( "outfile.txt" ) )   mlflow_log_artifact( run_id= res$run_uuid, path= "./outfile.txt" )
}


archivos  <- list.files(path = ".", pattern="yml")
for( archivo in archivos )  mlflow_log_artifact( run_id= res$run_uuid, path= archivo )

archivos  <- list.files(path = ".", pattern="impo_")
for( archivo in archivos )  mlflow_log_artifact( run_id= res$run_uuid, path= archivo )

archivos  <- list.files(path = ".", pattern="FM_importance_")
for( archivo in archivos )  mlflow_log_artifact( run_id= res$run_uuid, path= archivo )

#archivos  <- list.files(path = ".", pattern="futuro_prediccion_")
#for( archivo in archivos )  mlflow_log_artifact( run_id= res$run_uuid, path= archivo )


#finalizo el experimento
mlflow_end_run( run_id= res$run_uuid )

quit( save= "no" )
