#Necesita para correr en Google Cloud
#256 GB de memoria RAM
#300 GB de espacio en el disco local
#8 vCPU

#Este script esta dedicado con mucho amor a UTN 2022  Parana

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("Rcpp")

require("lightgbm")

options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})

kdataset_salida  <- "paquete_premium_ext_722.csv.gz"
kcampos_fijos  <- c( "numero_de_cliente", "clase_ternaria", "foto_mes" )

#------------------------------------------------------------------------------

ReportarCampos  <- function( dataset )
{
  cat( deparse(sys.calls()[[sys.nframe()-1]]),  #el nombre de la funcion desde donde se llamo
       "La cantidad de campos es ", 
       ncol(dataset) ,
       "\n" )
}
#------------------------------------------------------------------------------
#Agrega al dataset una variable que va de 1 a 12, el mes, para que el modelo aprenda estacionalidad

AgregarMes  <- function( dataset )
{
  gc()
  dataset[  , mes := foto_mes %% 100 ]
  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#Elimina las variables que uno supone hace Data Drifting

DriftEliminar  <- function( dataset, variables )
{
  gc()
  dataset[  , c(variables) := NULL ]
  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#Autor:  Santiago Dellachiesa, UAustral 2021
#A las variables que tienen nulos, les agrega una nueva variable el dummy de is es nulo o no {0, 1}

DummiesNA  <- function( dataset )
{
  gc()
  nulos  <- colSums( is.na(dataset[ foto_mes %in% 202101 ]) )  #cuento la cantidad de nulos por columna
  colsconNA  <- names( which(  nulos > 0 ) )

  dataset[ , paste0( colsconNA, "_isNA") :=  lapply( .SD,  is.na ),
             .SDcols= colsconNA]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------

CorregirCampoMes  <- function( pcampo, pmeses )
{
  tbl <- dataset[  ,  list( "v1" = shift( get(pcampo), 1, type="lag" ),
                            "v2" = shift( get(pcampo), 1, type="lead" )
                         ), 
                   by=numero_de_cliente ]
  
  tbl[ , numero_de_cliente := NULL ]
  tbl[ , promedio := rowMeans( tbl,  na.rm=TRUE ) ]
  
  dataset[ ,  
           paste0(pcampo) := ifelse( !(foto_mes %in% pmeses),
                                     get( pcampo),
                                     tbl$promedio ) ]

}
#------------------------------------------------------------------------------
# reemplaza cada variable ROTA  (variable, foto_mes)  con el promedio entre  ( mes_anterior, mes_posterior )
# en honor a Claudio Castillo,  honorable y fiel centinela de la Estadistica Clasica

CorregirClaudioCastillo  <- function( dataset )
{
  CorregirCampoMes( "internet",     c(201801,202006, 202010, 202011, 202012, 202101) )
  CorregirCampoMes( "thomebanking", c(201801,202006) )
  CorregirCampoMes( "chomebanking_trx", c(201801, 201910, 202006) )
  CorregirCampoMes( "tcallcenter", c(201801, 201806, 202006) )
  CorregirCampoMes( "ccallcenter_trx", c(201801, 201806, 202006) )
  CorregirCampoMes( "cprestamos_personales", c(201801,202006) )
  CorregirCampoMes( "mprestamos_personales", c(201801,202006) )
  CorregirCampoMes( "mprestamos_hipotecarios", c(201801,202006) )
  CorregirCampoMes( "ccajas_trx", c(201801,202006) )
  CorregirCampoMes( "ccajas_consultas", c(201801,202006) )
  CorregirCampoMes( "ccajas_depositos", c(201801,202006) )
  CorregirCampoMes( "ccajas_extracciones", c(201801,202006) )
  CorregirCampoMes( "ccajas_otras", c(201801,202006) )

  CorregirCampoMes( "ctarjeta_visa_debitos_automaticos", c(201904) )
  CorregirCampoMes( "mtarjeta_visa_debitos_automaticos", c(201904,201905) )
  CorregirCampoMes( "Visa_mfinanciacion_limite", c(201904) )

  CorregirCampoMes( "mrentabilidad", c(201905, 201910, 202006) )
  CorregirCampoMes( "mrentabilidad_annual", c(201905, 201910, 202006) )
  CorregirCampoMes( "mcomisiones", c(201905, 201910, 202006) )
  CorregirCampoMes( "mpasivos_margen", c(201905, 201910, 202006) )
  CorregirCampoMes( "mactivos_margen", c(201905, 201910, 202006) )
  CorregirCampoMes( "ccomisiones_otras", c(201905, 201910, 202006) )
  CorregirCampoMes( "mcomisiones_otras", c(201905, 201910, 202006) )

  CorregirCampoMes( "ctarjeta_visa_descuentos", c(201910) )
  CorregirCampoMes( "ctarjeta_master_descuentos", c(201910) )
  CorregirCampoMes( "mtarjeta_visa_descuentos", c(201910) )
  CorregirCampoMes( "mtarjeta_master_descuentos", c(201910) )
  CorregirCampoMes( "ccajeros_propios_descuentos", c(201910) )
  CorregirCampoMes( "mcajeros_propios_descuentos", c(201910) )

  CorregirCampoMes( "cliente_vip", c(201911) )

  CorregirCampoMes( "active_quarter", c(202006) )
  CorregirCampoMes( "mcuentas_saldo", c(202006) )
  CorregirCampoMes( "ctarjeta_debito_trx", c(202006) )
  CorregirCampoMes( "mautoservicio", c(202006) )
  CorregirCampoMes( "ctarjeta_visa_trx", c(202006) )
  CorregirCampoMes( "ctarjeta_visa_trx", c(202006) )
  CorregirCampoMes( "cextraccion_autoservicio", c(202006) )
  CorregirCampoMes( "mextraccion_autoservicio", c(202006) )
  CorregirCampoMes( "ccheques_depositados", c(202006) )
  CorregirCampoMes( "mcheques_depositados", c(202006) )
  CorregirCampoMes( "mcheques_depositados", c(202006) )
  CorregirCampoMes( "mcheques_depositados", c(202006) )
  CorregirCampoMes( "mcheques_depositados", c(202006) )
  CorregirCampoMes( "mcheques_depositados", c(202006) )
  CorregirCampoMes( "mcheques_depositados", c(202006) )
  CorregirCampoMes( "mcheques_depositados", c(202006) )
  CorregirCampoMes( "catm_trx", c(202006) )
  CorregirCampoMes( "matm", c(202006) )
  CorregirCampoMes( "catm_trx_other", c(202006) )
  CorregirCampoMes( "matm_other", c(202006) )
  CorregirCampoMes( "tmobile_app", c(202006) )
  CorregirCampoMes( "cmobile_app_trx", c(202006) )

  CorregirCampoMes( "tmobile_app", c(202006, 202009, 202010, 202011, 202012, 202101) )

}
#------------------------------------------------------------------------------
#Corrige poniendo a NA las variables que en ese mes estan dañadas
#  lo mas aberrante que un Estadistico de la Escuela Clasica podria escuchar, Ronald Fisher  se revuelve en su tumba

CorregirNA  <- function( dataset )
{
  gc()
  #acomodo los errores del dataset

  dataset[ foto_mes==201801,  internet   := NA ]
  dataset[ foto_mes==201801,  thomebanking   := NA ]
  dataset[ foto_mes==201801,  chomebanking_trx   := NA ]
  dataset[ foto_mes==201801,  tcallcenter   := NA ]
  dataset[ foto_mes==201801,  ccallcenter_trx   := NA ]
  dataset[ foto_mes==201801,  cprestamos_personales   := NA ]
  dataset[ foto_mes==201801,  mprestamos_personales   := NA ]
  dataset[ foto_mes==201801,  mprestamos_hipotecarios  := NA ]
  dataset[ foto_mes==201801,  ccajas_trx   := NA ]
  dataset[ foto_mes==201801,  ccajas_consultas   := NA ]
  dataset[ foto_mes==201801,  ccajas_depositos   := NA ]
  dataset[ foto_mes==201801,  ccajas_extracciones   := NA ]
  dataset[ foto_mes==201801,  ccajas_otras   := NA ]

  dataset[ foto_mes==201806,  tcallcenter   :=  NA ]
  dataset[ foto_mes==201806,  ccallcenter_trx   :=  NA ]

  dataset[ foto_mes==201904,  ctarjeta_visa_debitos_automaticos  :=  NA ]
  dataset[ foto_mes==201904,  mtarjeta_visa_debitos_automaticos := NA ]
  dataset[ foto_mes==201904,  Visa_mfinanciacion_limite := NA ]

  dataset[ foto_mes==201905,  mrentabilidad     := NA ]
  dataset[ foto_mes==201905,  mrentabilidad_annual     := NA ]
  dataset[ foto_mes==201905,  mcomisiones      := NA ]
  dataset[ foto_mes==201905,  mpasivos_margen  := NA ]
  dataset[ foto_mes==201905,  mactivos_margen  := NA ]
  dataset[ foto_mes==201905,  ctarjeta_visa_debitos_automaticos  := NA ]
  dataset[ foto_mes==201905,  ccomisiones_otras := NA ]
  dataset[ foto_mes==201905,  mcomisiones_otras := NA ]

  dataset[ foto_mes==201910,  mpasivos_margen   := NA ]
  dataset[ foto_mes==201910,  mactivos_margen   := NA ]
  dataset[ foto_mes==201910,  ccomisiones_otras := NA ]
  dataset[ foto_mes==201910,  mcomisiones_otras := NA ]
  dataset[ foto_mes==201910,  mcomisiones       := NA ]
  dataset[ foto_mes==201910,  mrentabilidad     := NA ]
  dataset[ foto_mes==201910,  mrentabilidad_annual        := NA ]
  dataset[ foto_mes==201910,  chomebanking_trx  := NA ]
  dataset[ foto_mes==201910,  ctarjeta_visa_descuentos    := NA ]
  dataset[ foto_mes==201910,  ctarjeta_master_descuentos  := NA ]
  dataset[ foto_mes==201910,  mtarjeta_visa_descuentos    := NA ]
  dataset[ foto_mes==201910,  mtarjeta_master_descuentos  := NA ]
  dataset[ foto_mes==201910,  ccajeros_propios_descuentos := NA ]
  dataset[ foto_mes==201910,  mcajeros_propios_descuentos := NA ]

  dataset[ foto_mes==202001,  cliente_vip   := NA ]

  dataset[ foto_mes==202006,  active_quarter   := NA ]
  dataset[ foto_mes==202006,  internet   := NA ]
  dataset[ foto_mes==202006,  mrentabilidad   := NA ]
  dataset[ foto_mes==202006,  mrentabilidad_annual   := NA ]
  dataset[ foto_mes==202006,  mcomisiones   := NA ]
  dataset[ foto_mes==202006,  mactivos_margen   := NA ]
  dataset[ foto_mes==202006,  mpasivos_margen   := NA ]
  dataset[ foto_mes==202006,  mcuentas_saldo   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_debito_trx   := NA ]
  dataset[ foto_mes==202006,  mautoservicio   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_visa_trx   := NA ]
  dataset[ foto_mes==202006,  mtarjeta_visa_consumo   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_master_trx   := NA ]
  dataset[ foto_mes==202006,  mtarjeta_master_consumo   := NA ]
  dataset[ foto_mes==202006,  ccomisiones_otras   := NA ]
  dataset[ foto_mes==202006,  mcomisiones_otras   := NA ]
  dataset[ foto_mes==202006,  cextraccion_autoservicio   := NA ]
  dataset[ foto_mes==202006,  mextraccion_autoservicio   := NA ]
  dataset[ foto_mes==202006,  ccheques_depositados   := NA ]
  dataset[ foto_mes==202006,  mcheques_depositados   := NA ]
  dataset[ foto_mes==202006,  ccheques_emitidos   := NA ]
  dataset[ foto_mes==202006,  mcheques_emitidos   := NA ]
  dataset[ foto_mes==202006,  ccheques_depositados_rechazados   := NA ]
  dataset[ foto_mes==202006,  mcheques_depositados_rechazados   := NA ]
  dataset[ foto_mes==202006,  ccheques_emitidos_rechazados   := NA ]
  dataset[ foto_mes==202006,  mcheques_emitidos_rechazados   := NA ]
  dataset[ foto_mes==202006,  tcallcenter   := NA ]
  dataset[ foto_mes==202006,  ccallcenter_trx   := NA ]
  dataset[ foto_mes==202006,  thomebanking   := NA ]
  dataset[ foto_mes==202006,  chomebanking_trx   := NA ]
  dataset[ foto_mes==202006,  ccajas_trx   := NA ]
  dataset[ foto_mes==202006,  ccajas_consultas   := NA ]
  dataset[ foto_mes==202006,  ccajas_depositos   := NA ]
  dataset[ foto_mes==202006,  ccajas_extracciones   := NA ]
  dataset[ foto_mes==202006,  ccajas_otras   := NA ]
  dataset[ foto_mes==202006,  catm_trx   := NA ]
  dataset[ foto_mes==202006,  matm   := NA ]
  dataset[ foto_mes==202006,  catm_trx_other   := NA ]
  dataset[ foto_mes==202006,  matm_other   := NA ]
  dataset[ foto_mes==202006,  ctrx_quarter   := NA ]
  dataset[ foto_mes==202006,  tmobile_app   := NA ]
  dataset[ foto_mes==202006,  cmobile_app_trx   := NA ]


  dataset[ foto_mes==202010,  internet  := NA ]
  dataset[ foto_mes==202011,  internet  := NA ]
  dataset[ foto_mes==202012,  internet  := NA ]
  dataset[ foto_mes==202101,  internet  := NA ]

  dataset[ foto_mes==202009,  tmobile_app  := NA ]
  dataset[ foto_mes==202010,  tmobile_app  := NA ]
  dataset[ foto_mes==202011,  tmobile_app  := NA ]
  dataset[ foto_mes==202012,  tmobile_app  := NA ]
  dataset[ foto_mes==202101,  tmobile_app  := NA ]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
# Funcion que tiene DOS metodos posibles   "AsignarNA"  o "ClaudioCastillo"

CorregirVariablesROTAS  <- function( dataset, metodo )
{
  metodos_disponibles  <- c( "AsignarNA", "ClaudioCastillo" )
  if( !( metodo  %in%  metodos_disponibles ) )    stop( "ERROR, los unicos metodos diponibles son  AsignarNA o  ClaudioCastillo \n" )
  
  if( metodo== "AsignarNA" )    CorregirNA( dataset ) 
   else
        if( metodo== "ClaudioCastillo" )  CorregirClaudioCastillo( dataset )
  
}
#------------------------------------------------------------------------------
#Esta es la parte que los alumnos deben desplegar todo su ingenio

AgregarVariables  <- function( dataset )
{
  gc()
  #INICIO de la seccion donde se deben hacer cambios con variables nuevas

  #creo un ctr_quarter que tenga en cuenta cuando los clientes hace 3 menos meses que estan
  dataset[  , ctrx_quarter_normalizado := ctrx_quarter ]
  dataset[ cliente_antiguedad==1 , ctrx_quarter_normalizado := ctrx_quarter * 5 ]
  dataset[ cliente_antiguedad==2 , ctrx_quarter_normalizado := ctrx_quarter * 2 ]
  dataset[ cliente_antiguedad==3 , ctrx_quarter_normalizado := ctrx_quarter * 1.2 ]

  #variable extraida de una tesis de maestria de Irlanda
  dataset[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]

  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status
  dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  dataset[ , mv_status02       := Master_status +  Visa_status ]
  dataset[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
  dataset[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
  dataset[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]

  dataset[ , mv_status06       := ifelse( is.na(Visa_status), 
                                          ifelse( is.na(Master_status), 10, Master_status), 
                                          Visa_status)  ]

  dataset[ , mv_status07       := ifelse( is.na(Master_status), 
                                          ifelse( is.na(Visa_status), 10, Visa_status), 
                                          Master_status)  ]


  #combino MasterCard y Visa
  dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

  dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
  dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
  dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dataset[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

  #a partir de aqui juego con la suma de Mastercard y Visa
  dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
  dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
  dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
  dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
  dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
  dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
  dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
  dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
  dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
  dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
  dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
  dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]

  #Aqui debe usted agregar sus propias nuevas variables

  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
  infinitos_qty  <- sum( unlist( infinitos) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <<- NA
  }


  #valvula de seguridad para evitar valores NaN  que es 0/0
  #paso los NaN a 0 , decision polemica si las hay
  #se invita a asignar un valor razonable segun la semantica del campo creado
  nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
  nans_qty  <- sum( unlist( nans) )
  if( nans_qty > 0 )
  {
    cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
    cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <<- 0
  }

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#esta funcion supone que dataset esta ordenado por   <numero_de_cliente, foto_mes>
#calcula el lag y el delta lag

Lags  <- function( cols, nlag, deltas )
{
  gc()
  sufijo  <- paste0( "_lag", nlag )

  dataset[ , paste0( cols, sufijo) := shift(.SD, nlag, NA, "lag"), 
             by= numero_de_cliente, 
             .SDcols= cols]

  #agrego los deltas de los lags, con un "for" nada elegante
  if( deltas )
  {
    sufijodelta  <- paste0( "_delta", nlag )

    for( vcol in cols )
    {
     dataset[,  paste0(vcol, sufijodelta) := get( vcol)  - get(paste0( vcol, sufijo))]
    }
  }

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#Por favor  Paula Kozak , podes llevar una vida plena y feliz sin necesidad de aprender el lenguaje C

#se calculan para los 6 meses previos el minimo, maximo y tendencia calculada con cuadrados minimos
#la formula de calculo de la tendencia puede verse en https://stats.libretexts.org/Bookshelves/Introductory_Statistics/Book%3A_Introductory_Statistics_(Shafer_and_Zhang)/10%3A_Correlation_and_Regression/10.04%3A_The_Least_Squares_Regression_Line
#para la maxíma velocidad esta funcion esta escrita en lenguaje C, y no en la porqueria de R o Python

cppFunction('NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde ) 
{
  /* Aqui se cargan los valores para la regresion */
  double  x[100] ;
  double  y[100] ;

  int n = pcolumna.size();
  NumericVector out( 5*n );

  for(int i = 0; i < n; i++)
  {
    //lag
    if( pdesde[i]-1 < i )  out[ i + 4*n ]  =  pcolumna[i-1] ;
    else                   out[ i + 4*n ]  =  NA_REAL ;


    int  libre    = 0 ;
    int  xvalor   = 1 ;

    for( int j= pdesde[i]-1;  j<=i; j++ )
    {
       double a = pcolumna[j] ;

       if( !R_IsNA( a ) ) 
       {
          y[ libre ]= a ;
          x[ libre ]= xvalor ;
          libre++ ;
       }

       xvalor++ ;
    }

    /* Si hay al menos dos valores */
    if( libre > 1 )
    {
      double  xsum  = x[0] ;
      double  ysum  = y[0] ;
      double  xysum = xsum * ysum ;
      double  xxsum = xsum * xsum ;
      double  vmin  = y[0] ;
      double  vmax  = y[0] ;

      for( int h=1; h<libre; h++)
      { 
        xsum  += x[h] ;
        ysum  += y[h] ; 
        xysum += x[h]*y[h] ;
        xxsum += x[h]*x[h] ;

        if( y[h] < vmin )  vmin = y[h] ;
        if( y[h] > vmax )  vmax = y[h] ;
      }

      out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum) ;
      out[ i + n ]    =  vmin ;
      out[ i + 2*n ]  =  vmax ;
      out[ i + 3*n ]  =  ysum / libre ;
    }
    else
    {
      out[ i       ]  =  NA_REAL ; 
      out[ i + n   ]  =  NA_REAL ;
      out[ i + 2*n ]  =  NA_REAL ;
      out[ i + 3*n ]  =  NA_REAL ;
    }
  }

  return  out;
}')

#------------------------------------------------------------------------------
#calcula la tendencia de las variables cols de los ultimos 6 meses
#la tendencia es la pendiente de la recta que ajusta por cuadrados minimos
#La funcionalidad de ratioavg es autoria de  Daiana Sparta,  UAustral  2021

TendenciaYmuchomas  <- function( dataset, cols, ventana=6, tendencia=TRUE, minimo=TRUE, maximo=TRUE, promedio=TRUE, 
                                 ratioavg=FALSE, ratiomax=FALSE)
{
  gc()
  #Esta es la cantidad de meses que utilizo para la historia
  ventana_regresion  <- ventana

  last  <- nrow( dataset )

  #creo el vector_desde que indica cada ventana
  #de esta forma se acelera el procesamiento ya que lo hago una sola vez
  vector_ids   <- dataset$numero_de_cliente

  vector_desde  <- seq( -ventana_regresion+2,  nrow(dataset)-ventana_regresion+1 )
  vector_desde[ 1:ventana_regresion ]  <-  1

  for( i in 2:last )  if( vector_ids[ i-1 ] !=  vector_ids[ i ] ) {  vector_desde[i] <-  i }
  for( i in 2:last )  if( vector_desde[i] < vector_desde[i-1] )  {  vector_desde[i] <-  vector_desde[i-1] }

  for(  campo  in   cols )
  {
    nueva_col     <- fhistC( dataset[ , get(campo) ], vector_desde ) 

    if(tendencia)  dataset[ , paste0( campo, "_tend", ventana) := nueva_col[ (0*last +1):(1*last) ]  ]
    if(minimo)     dataset[ , paste0( campo, "_min", ventana)  := nueva_col[ (1*last +1):(2*last) ]  ]
    if(maximo)     dataset[ , paste0( campo, "_max", ventana)  := nueva_col[ (2*last +1):(3*last) ]  ]
    if(promedio)   dataset[ , paste0( campo, "_avg", ventana)  := nueva_col[ (3*last +1):(4*last) ]  ]
    if(ratioavg)   dataset[ , paste0( campo, "_ratioavg", ventana)  := get(campo) /nueva_col[ (3*last +1):(4*last) ]  ]
    if(ratiomax)   dataset[ , paste0( campo, "_ratiomax", ventana)  := get(campo) /nueva_col[ (2*last +1):(3*last) ]  ]
  }

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
VPOS_CORTE  <- c()

fganancia_lgbm_meseta  <- function(probs, datos) 
{
  vlabels  <- get_field(datos, "label")
  vpesos   <- get_field(datos, "weight")

  tbl  <- as.data.table( list( "prob"=probs, "gan"= ifelse( vlabels==1 & vpesos > 1, 60000, -1000 ) ) )

  setorder( tbl, -prob )
  tbl[ , posicion := .I ]
  tbl[ , gan_acum :=  cumsum( gan ) ]
  setorder( tbl, -gan_acum )   #voy por la meseta

  gan  <- mean( tbl[ 1:500,  gan_acum] )  #meseta de tamaño 500

  pos_meseta  <- tbl[ 1:500,  median(posicion)]
  VPOS_CORTE  <<- c( VPOS_CORTE, pos_meseta )

  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#Elimina del dataset las variables que :
#   Metodo  "Kozak" :  elimina las variables que estan por debajo del primer canarito, es el metodo mas agresivo
#   Metodo  "CapaGeologica" :  elimina las variables que estan por debajo de la capa geologica de canaritos_ratio
#   Metodo  "Shai" : elimina las variables que elimina estan por debajo del ultimno canarito, es el metodo mas conservador
# canaritos_ratio indica cuantos canaritos se agregan, como ratio de  ncol(dataset)

#se puede llamar varias veces, luego de agregar muchas variables nuevas, para ir reduciendo la cantidad de variables
# y así hacer lugar a nuevas variables importantes

GVEZ <- 1 

CanaritosAsesinos  <- function( metodo="CapaGeologica", canaritos_ratio=0.2 )
{
  metodos_disponibles  <- c("CapaGeologica", "Kozak", "Shai" )
  if( !( metodo %in%  metodos_disponibles ) )   stop( "ERROR, los unicos metodos diponibles son  Kozak , CapaGeologica, Shai \n" )

  gc()
  ReportarCampos( dataset )
  dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

  for( i  in 1:(ncol(dataset)*canaritos_ratio))  dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset))]

  campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01" ) )

  azar  <- runif( nrow(dataset) )
  dataset[ , entrenamiento := foto_mes>= 202001 &  foto_mes<= 202010 &  foto_mes!=202006 & ( clase01==1 | azar < 0.10 ) ]

  dtrain  <- lgb.Dataset( data=    data.matrix(  dataset[ entrenamiento==TRUE, campos_buenos, with=FALSE]),
                          label=   dataset[ entrenamiento==TRUE, clase01],
                          weight=  dataset[ entrenamiento==TRUE, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)],
                          free_raw_data= FALSE
                        )

  dvalid  <- lgb.Dataset( data=    data.matrix(  dataset[ foto_mes==202011, campos_buenos, with=FALSE]),
                          label=   dataset[ foto_mes==202011, clase01],
                          weight=  dataset[ foto_mes==202011, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)],
                          free_raw_data= FALSE
                          )


  param <- list( objective= "binary",
                 metric= "custom",
                 first_metric_only= TRUE,
                 boost_from_average= TRUE,
                 feature_pre_filter= FALSE,
                 verbosity= -100,
                 seed= 999983,
                 max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                 min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                 lambda_l1= 0.0,         #por ahora, lo dejo fijo
                 lambda_l2= 0.0,         #por ahora, lo dejo fijo
                 max_bin= 31,            #por ahora, lo dejo fijo
                 num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                 force_row_wise= TRUE,    #para que los alumnos no se atemoricen con tantos warning
                 learning_rate= 0.065, 
                 feature_fraction= 1.0,   #lo seteo en 1 para que las primeras variables del dataset no se vean opacadas
                 min_data_in_leaf= 260,
                 num_leaves= 60,
               # num_threads= 8,
                 early_stopping_rounds= 200 )

  modelo  <- lgb.train( data= dtrain,
                        valids= list( valid= dvalid ),
                        eval= fganancia_lgbm_meseta,
                        param= param,
                        verbose= -100 )

  tb_importancia  <- lgb.importance( model= modelo )
  tb_importancia[  , pos := .I ]

  fwrite( tb_importancia, 
          file= paste0( "impo_", GVEZ ,".txt"),
          sep= "\t" )

  GVEZ  <<- GVEZ + 1

  #Aqui decido el umbral segun cual de los tres metodos se utiliza
  if( metodo== "Kozak" )         umbral  <- tb_importancia[ Feature %like% "canarito", min(pos)  ]
  if( metodo== "CapaGeologica" ) umbral  <- tb_importancia[ Feature %like% "canarito", median(pos) + sd(pos) ]  #Atencion corto en la mediana !!
  if( metodo== "Shai" )          umbral  <- tb_importancia[ Feature %like% "canarito", max(pos)  ]
  
  col_utiles  <- tb_importancia[ pos < umbral & !( Feature %like% "canarito"),  Feature ]
  col_utiles  <-  unique( c( col_utiles,  c("numero_de_cliente","foto_mes","clase_ternaria","mes") ) )
  col_inutiles  <- setdiff( colnames(dataset), col_utiles )

  dataset[  ,  (col_inutiles) := NULL ]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#calcula el ranking de la funcion

Rankeador  <- function( cols )
{
  gc()
  sufijo  <- "_rank" 

  for( vcol in cols )
  {
     dataset[ , paste0( vcol, sufijo) := frank( get(vcol), ties.method= "random")/ .N, 
                by= foto_mes ]
  }

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

setwd( "~/buckets/b1/datasets/" )

#cargo el dataset
dataset   <- fread( "paquete_premium.csv.gz" )

#ordeno el dataset por <numero_de_cliente, foto_mes> para poder hacer lags
setorder( dataset, numero_de_cliente, foto_mes )


AgregarMes( dataset )  #agrego el mes del año  1,2 3,..,12 , sirve por si hay estacionalidad

DummiesNA( dataset )  #esta linea debe ir ANTES de Corregir  !!


#hay DOS metodos  ClaudioCastillo  o  AsignarNA
CorregirVariablesROTAS( dataset,  metodo= "ClaudioCastillo" )  #esta linea debe ir DESPUES de  DummiesNA

#--------------------------------------

#Agrega las variables manuales
AgregarVariables( dataset )

#--------------------------------------
#Esta primera parte es muuuy  artesanal  y discutible  ya que hay multiples formas de hacerlo

cols_lagueables  <- copy( setdiff( colnames(dataset), kcampos_fijos ) )

#Para mi, el metodo Kozak de matar todo lo que queda por debajo del primer canarito es demasiado agresivo
#  pero bueno, vamos a darle una oportunidad, hay que experimentar

#Comentar si no se quiera que funcione
Lags( cols_lagueables, 1, deltas= TRUE )   #calculo los lags de orden  1

#elimino las variables poco importantes
# Hay TRES metodos disponibles  Kozak, CapaGeologica, Shai
CanaritosAsesinos( metodo="Kozak" )
cols_lagueables  <- intersect( cols_lagueables,  colnames(dataset) )

#--------------
Lags( cols_lagueables, 2, deltas= TRUE )   #calculo los lags de orden  2
CanaritosAsesinos( metodo="Kozak" )
cols_lagueables  <- intersect( cols_lagueables,  colnames(dataset) )
#--------------
# "La Gran Sergio Civico", me entusiasme agregando lags

Lags( cols_lagueables, 3, deltas= TRUE )   #calculo los lags de orden  3
CanaritosAsesinos( metodo="Kozak" )
cols_lagueables  <- intersect( cols_lagueables,  colnames(dataset) )

Lags( cols_lagueables, 4, deltas= TRUE )   #calculo los lags de orden  3
CanaritosAsesinos( metodo="Kozak" )
cols_lagueables  <- intersect( cols_lagueables,  colnames(dataset) )

#--------------


#Rankea las variables dentro de cada mes
#setorder( dataset, foto_mes, numero_de_cliente )
#Rankeador( cols_lagueables )
#setorder( dataset, numero_de_cliente, foto_mes )

#reacomodo las cols_lagueables ya que muchas de las originales han desaparecido
cols_lagueables  <- intersect( cols_lagueables,  colnames(dataset) )


#Tendencia y Promedio con ventana de 6 meses -----------------------
TendenciaYmuchomas( dataset, 
                    cols=      cols_lagueables,
                    ventana=   6,
                    tendencia= TRUE,
                    minimo=    FALSE,
                    maximo=    FALSE,
                    promedio=  TRUE,
                    ratioavg=  FALSE,
                    ratiomax=  FALSE
                  )

CanaritosAsesinos( metodo="Kozak" )
cols_lagueables  <- intersect( cols_lagueables,  colnames(dataset) )


#Tendencia y Promedio con ventana de 3 meses -----------------------
TendenciaYmuchomas( dataset, 
                    cols=      cols_lagueables,
                    ventana=   3,
                    tendencia= TRUE,
                    minimo=    FALSE,
                    maximo=    FALSE,
                    promedio=  TRUE,
                    ratioavg=  FALSE,
                    ratiomax=  FALSE
                  )

CanaritosAsesinos( metodo="Kozak" )
cols_lagueables  <- intersect( cols_lagueables,  colnames(dataset) )


#al fin, he terminado. Housekeeping final

#dejo la clase como ultimo campo
nuevo_orden  <- c( setdiff( colnames( dataset ) , "clase_ternaria" ) , "clase_ternaria" )
setcolorder( dataset, nuevo_orden )


#Grabo el dataset
fwrite( dataset,
        kdataset_salida,
        logical01= TRUE,
        sep= "," )


