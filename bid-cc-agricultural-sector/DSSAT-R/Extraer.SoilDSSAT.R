#JRV extraer soil data from 30m grid
#Dec 2016

Extraer.SoilDSSAT <- function(input, pixel, path) {
  #position <- Codigo_identificadorSoil + 1   ## Where it coincides with the raster ID
  #posicion <- which(Soil_profile[, 1] == position)
  cell_id <- xy_Ref$CELL30M[which(round(xy_Ref$x,5) == round(input$climate$long[pixel],5) & round(xy_Ref$y,5) == round(input$climate$lat[pixel],5))]
  p_profile <- profileMatrix[which(profileMatrix$CELL30M == cell_id),]
  
  #if missing then use generic (HC_GEN0011), else take from WI.SOL
  if (nrow(p_profile) == 0) {
    Wise_Position <- Cod_Ref_and_Position_Generic[11,2]
    return(make_soilfile(in_data,Soil_Generic[Wise_Position:length(Soil_Generic)], path))
  } else {
    #celdas_id_Wise <- Soil_profile[posicion, ]                                    ## Cells and percentage of soil File DSSAT 
    Posicion_Pct <- which(p_profile[, "rep_res"] == max(p_profile[, "rep_res"]))   ## profile with max. share for pixel
    Ref_for_Soil <- p_profile[Posicion_Pct,2]
    Ref_for_Soil <- p_profile[Posicion_Pct,2][1]
    condicion <- which(Cod_Ref_and_Position[,1] == paste(Ref_for_Soil))
    
    #if profile indeed exists
    if (length(condicion) >= 1) {
      Wise_Position <- Cod_Ref_and_Position[which(Cod_Ref_and_Position[, 1] == paste(Ref_for_Soil)), ]
      return(make_soilfile(in_data, wise[Wise_Position[, 2]:length(wise)], path))
    }
    
    #else use generic
    if (length(condicion) == 0) {
      Wise_Position <- Cod_Ref_and_Position_Generic[which(Cod_Ref_and_Position_Generic[, 1] == paste(Ref_for_Soil)), 2]
      return(make_soilfile(in_data,Soil_Generic[Wise_Position:length(Soil_Generic)], path))
    }
  }
}
