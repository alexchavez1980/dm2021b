#Feature Engineering
#creo nuevas variables dentro del mismo mes
#Condimentar a gusto con nuevas variables
#COMENTARIO ALEX: VOY A COMENZAR A AJUSTAR VARIABLES

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")

#Establezco el Working Directory
#setwd( "~/buckets/b1/crudo" )
setwd("D:\\Alex\\Estudio\\Esp_Ciencia_Datos\\02_ MD")

EnriquecerDataset <- function( dataset , arch_destino )
{
  columnas_originales <-  copy(colnames( dataset ))

  #Pruebo con mrentabilidad
  
  dataset[ , mrentabilidad_mult := mrentabilidad*100]
  #dataset[ , mrentabilidad_log := log(mrentabilidad)]
  #dataset[ , mrentabilidad_bin := ifelse( mrentabilidad < 0 , 0, 1)]
  
  #INICIO de la seccion donde se deben hacer cambios con variables nuevas
  
  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status
  
  
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
dataset1  <- fread("./datasetsOri/paquete_premium_202011.csv")
dataset2  <- fread("./datasetsOri/paquete_premium_202101.csv")

#View(dataset1)
#View(dataset2)

EnriquecerDataset( dataset1, "./datasets/paquete_premium_202011_ext.csv" )
EnriquecerDataset( dataset2, "./datasets/paquete_premium_202101_ext.csv" )

quit( save="no")
