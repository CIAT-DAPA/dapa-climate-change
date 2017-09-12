#Julian Ramirez Villegas
#CIAT, 2010

require(raster)
require(rgdal)
require(fields)

setwd("D:/Proyects/Uniban/_scripts")

cat("\n")
cat("\n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
cat("\n Ajuste de superficies usando Thin Plate Spline \n")
cat(" CIAT, 2010 \n")
cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
cat("\n")

cat("------------------------------------------------------------------------------\n")
cat("Ejecutar función como: crearSuperficie(inputData, dem, outputSurf, plotSrf=F) \n")
cat("------------------------------------------------------------------------------\n")
cat("**inputData es un archivo csv con n filas y 3 columnas (valores,x,y) separado \n");cat("  por coma \n")
cat("**dem es el modelo de elevación digital del terreno como ASCII GRID \n")
cat("**outputSurf es el nombre del raster de salida interpolado \n")
cat("**plotSrf es un falor logico (TRUE/FALSE) para graficar/no graficar resultado \n");cat("  (grid interpolado) \n")
cat("------------------------------------------------------------------------------\n")

inputData <- "D:/Proyects/Uniban/_scripts/ph.csv"
dem <- "D:/Proyects/Uniban/variables-originales/dem/AAIGrid-UTM-18N/alt.asc"
outputSurf <- "D:/Proyects/Uniban/_scripts/test.asc"

or <- crearSuperficie(inputData, dem, outputSurf, plotSrf=T)

#Defining the function

crearSuperficie <- function(inputData, dem, outputSurf, plotSrf=F) {
	
	#Verificar input
	#inputData debe ser archivo csv con:
		#nrow = numero de puntos
		#ncol=3
		#primera columna es valor de la variable a interpolar
		#segunda columna es longitud del punto (X)
		#tercera columna es latitud del punto (Y)
	
	cat("\n")
	cat("Procesando \n")
	cat("Verificando datos de entrada \n")
	
	if (!file.exists(inputData)) {
		stop("El archivo de entrada de datos no fue encontrado")
	}
	
	#dem debe ser un ASCII grid con valores de elevación mismo sistema de coordenadas que el inputData
	
	if (!file.exists(dem)) {
		stop("El raster que corresponde al DEM no fue encontrado")
	}
	
	#outputSurf debe ser un nombre de raster cualquiera con ruta incluída
	
	#Load data
	
	cat("Cargando datos \n")
	
	datos <- read.csv(inputData)
	dem <- raster(dem)
	dem <- readAll(dem)
	
	msk <- raster(dem)
	msk[] <- 1
	msk <- mask(msk, dem)
	
	if (ncol(datos) != 3) {
		stop("El archivo de datos no contiene numero adecuado de columnas")
	}
	
	cat("Verificando si el archivo tiene valores NA \n")
	
	datos <- datos[which(!is.na(datos[,1])),]
	
	#Extract elevation
	
	cat("Extrayendo valores de elevación para ajuste \n")
	
	demvals <- extract(dem, datos[,2:3])
	
	depvar <- datos[,1]
	
	indvars <- matrix(ncol=3, nrow=nrow(datos))
	indvars[,1] <- datos[,2] 	#Lon
	indvars[,2] <- datos[,3] 	#Lat
	indvars[,3] <- demvals 		#Alt
	
	#Fit TPS
	
	cat("Ajustando superficie \n")
	
	fit <- Tps(indvars, depvar, m=2)
	
	#Interpolate
	
	cat("Interpolando superficie \n")
	
	result <- interpolate(dem, fit, xyOnly=F, progress="text")
	
	#Writing result
	
	cat("Escribiendo resultado \n")
	
	writeres <- writeRaster(result, outputSurf, overwrite=TRUE, format="ascii")
	
	if (plotSrf) {
		cat("Graficando \n")
		jpeg("00-chart.jpg")
		plot(result)
		points(datos[,2:3],pch=20,col="blue")
		dev.off()
	}
	
	#Get data from result to compare
	
	cat("Sacando estadisticos de prueba! \n")
	
	intvals <- extract(result, datos[,2:3])
	sqdif <- (depvar - intvals)^2
	dff <- (depvar - intvals)^2 / (max(depvar) ^ 2) * 100
	jpeg("01-SQDiff-Histograma.jpg")
	hist(dff,freq=T,breaks=20,xlab="Diferencia de cuadrados real - predicho (% de maximo)", main="Histograma")
	dev.off()
	
	jpeg("02-XYChart-MeasvsPred.jpg")
	plot(depvar,intvals,main="Dispersion real vs predicho", xlab="Predicho", ylab="Medido")
	rval <- cor(depvar,intvals)
	linfit <- lm(intvals ~ depvar)
	lines(depvar,linfit$fitted.values)
	text((min(depvar)+max(intvals)/5),max(intvals),paste("R=",round(rval,4)),font=2)
	dev.off()
	
	#Creating eval matrix
	
	evmx <- matrix(nrow=length(depvar),ncol=7)
	evmx[,1] <- datos[,2] 			#X
	evmx[,2] <- datos[,3]			#Y
	evmx[,3] <- demvals				#Alt
	evmx[,4] <- datos[,1]			#VALS
	evmx[,5] <- intvals				#VALS_TPS-PREDICTED
	evmx[,6] <- sqdif				#sq-diff
	evmx[,7] <- dff					#div_per_max
	
	evmx <- as.data.frame(evmx)
	names(evmx) <- c("X","Y","Alt","Valores","Valores-predichos-TPS","diferencia-cuadrados","diferencia-cuadrados-rel-max")
	
	otmx <- write.csv(evmx,"00-metricos-TPS.csv", quote=F, row.names=F)
	
	cat("Terminado! \n")
	
	return(result)
	
}

