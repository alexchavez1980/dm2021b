#Necesita para correr en Google Cloud
#64 GB de memoria RAM
#256 GB de espacio en el disco local
#8 vCPU


#Feature Engineering
#creo nuevas variables dentro del mismo mes
#Condimentar a gusto con nuevas variables

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")



#Establezco el Working Directory
setwd( "~/buckets/b1/" )


EnriquecerDataset <- function( dataset , arch_destino )
{
  columnas_originales <-  copy(colnames( dataset ))
  
  dataset[ ,  canarito_runif :=  runif( nrow(dataset) ) ] #agrego una variable canarito, random distribucion uniforme en el intervalo [0,1]
  dataset[ ,  canarito_sample :=  sample( nrow(dataset) ) ] #agrego una variable canarito, random distribucion uniforme en el intervalo [0,1]
  
  dataset[ , mrentabilidad_mult := mrentabilidad*100]
  dataset[ , mrentabilidad_log := log(mrentabilidad)]
  dataset[ , mrentabilidad_bin := ifelse(mrentabilidad < 0,0,1)]
  dataset[ , mrentabilidad_vol := ifelse(mrentabilidad < 0,mrentabilidad*(-1),mrentabilidad)]
  
  dataset[ , mcuentas_saldo_mult := mcuentas_saldo*100]
  dataset[ , mcuentas_saldo_log := log(mcuentas_saldo)]
  dataset[ , mcuentas_saldo_bin := ifelse(mcuentas_saldo < 0,0,1)]
  dataset[ , mcuentas_saldo_vol := ifelse(mcuentas_saldo < 0,mcuentas_saldo*(-1),mcuentas_saldo)]
  
  dataset[ , mpayroll_log := log(mpayroll)]
  dataset[ , mpayroll_bin := ifelse(mpayroll < 0,0,1)]
  dataset[ , mpayroll_vol := ifelse(mpayroll < 0,mpayroll*(-1),mpayroll)]
  
  dataset[ , cpayroll_trx_mult := cpayroll_trx*100]
  dataset[ , cpayroll_trx_log := log(cpayroll_trx)]
  dataset[ , cpayroll_trx_bin := ifelse(cpayroll_trx < 0,0,1)]
  dataset[ , cpayroll_trx_vol := ifelse(cpayroll_trx < 0,cpayroll_trx*(-1),cpayroll_trx)]
  
  dataset[ , mcuenta_debitos_automaticos_mult := mcuenta_debitos_automaticos*100]
  dataset[ , mcuenta_debitos_automaticos_log := log(mcuenta_debitos_automaticos)]
  dataset[ , mcuenta_debitos_automaticos_bin := ifelse(mcuenta_debitos_automaticos < 0,0,1)]  
  dataset[ , mcuenta_debitos_automaticos_vol := ifelse(mcuenta_debitos_automaticos < 0,mcuenta_debitos_automaticos*(-1),mcuenta_debitos_automaticos)]
  
  dataset[ , mtransferencias_recibidas_mult := mtransferencias_recibidas*100]
  dataset[ , mtransferencias_recibidas_log := log(mtransferencias_recibidas)]
  dataset[ , mtransferencias_recibidas_bin := ifelse(mtransferencias_recibidas < 0,0,1)]
  dataset[ , mtransferencias_recibidas_vol := ifelse(mtransferencias_recibidas < 0,mtransferencias_recibidas*(-1),mtransferencias_recibidas)]
  
  dataset[ , mextraccion_autoservicio_mult := mextraccion_autoservicio*100]
  dataset[ , mextraccion_autoservicio_log := log(mextraccion_autoservicio)]
  dataset[ , mextraccion_autoservicio_bin := ifelse(mextraccion_autoservicio < 0,0,1)]
  dataset[ , mextraccion_autoservicio_vol := ifelse(mextraccion_autoservicio < 0,mextraccion_autoservicio*(-1),mextraccion_autoservicio)]
  
  dataset[ , ctrx_quarter_mult := ctrx_quarter*100]
  
  #Promedio de donde se mueve el dinero
  dataset[ , prom_5_var := (mcuentas_saldo + mpayroll + mcuenta_debitos_automaticos + mtransferencias_recibidas + mextraccion_autoservicio)/5]
  dataset[ , prom_5_var_mult := prom_5_var*100]  
  
  #Promedio de donde se mueve el dinero DEL VOLÚMEN
  #OJO: Acá hay una buena.
  dataset[ , prom_5_var_vol := (mcuentas_saldo_vol + mpayroll_vol + mcuenta_debitos_automaticos_vol + mtransferencias_recibidas_vol + mextraccion_autoservicio_vol)/5]
  dataset[ , prom_5_var_vol_mult := prom_5_var_vol*100]    
  
  #Logaritmear todas las que representan ganancia para el banco.
  #  dataset[ , mrentabilidad_annual_log := log(mrentabilidad_annual)]
  #  dataset[ , mcomisiones_log := log(mcomisiones)]
  #  dataset[ , mactivos_margen_log := log(mactivos_margen)]
  #  dataset[ , mpasivos_margen_log := log(mpasivos_margen)]
  
  
  # Master_status y Visa_status  
  # { 0,  6, 7, 9 }   indica el estado de la cuenta de la tarjeta de crédito. 
  # 0 abierta,  6 en proceso de cierre, 7 en proceso avanzado de cierre, 
  # 9 cuenta cerrada.   Una cuenta cerrada puede volver a abrirse !!
  # Si las junto, y las acomodo logarítmicamente, puede clasificar.
  dataset[ , MyV_status_prom := (Master_status + Visa_status)/2]
  dataset[ , MyV_status_prom_log := (log(Master_status + Visa_status)/2)]
  dataset[ , MyV_status_prom_mult := (log(Master_status + Visa_status)/2)*100]
  
  # cpayroll_trx
  # Cantidad de Acreditaciones de Haberes en relación de depencia que 
  # le hicieron al cliente en ese mes.  
  # Un cliente puede estar en relacion de dependencia con mas de una empresa. 
  # Una empresa puede hacerle VARIOS depósitos al mismo empleado durante el mes.  
  # Solamente se consideran las acreditaciones de empresas que tienen un contrato con el banco.
  
  dataset[ , cpayroll_trx_cuad := (cpayroll_trx)^2]

  #INICIO de la seccion donde se deben hacer cambios con variables nuevas
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

  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
  infinitos_qty  <- sum( unlist( infinitos) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <- NA
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
    dataset[mapply(is.nan, dataset)] <- 0
  }

  #FIN de la seccion donde se deben hacer cambios con variables nuevas

  columnas_extendidas <-  copy( setdiff(  colnames(dataset), columnas_originales ) )

  #grabo con nombre extendido
  fwrite( dataset,
          file=arch_destino,
          sep= "," )
}
#------------------------------------------------------------------------------

dir.create( "./datasets/" )

#lectura rapida del dataset  usando fread  de la libreria  data.table
dataset1  <- fread("./datasetsOri/paquete_premium.csv.gz")

EnriquecerDataset( dataset1, "./datasets/paquete_premium_ext.csv.gz" )

quit( save="no")
