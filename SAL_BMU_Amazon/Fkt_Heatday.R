########################################
##
## Hitzewellentage berechnen
## Hitzewelledefinition: mind. 3Tage 
## 	mit >=thres°C Tageshöchsttemp.
##
##
## Carsten Walther@pik-potsdam.de
########################################


heatday <- function(tempdata,cnt,thres) {
	heatd <- array(0,0)
	heatd <- which(tempdata>=thres)
	lht <- length(heatd) 			## HWD die Erste !!
	num <- 0					## Gesamtanzahl Hitzewellentage 1
	if(lht>2) {					## nur wenn mehr als zwei Hitzetage existieren
	    	htn <- 1				## durchläuft Hitzetage
		while(htn <= lht-2) {         ## mit allen Tagen >=30°
	      	cor <- 1                ## Zählkorrektur
	       	if(heatd[htn]==(heatd[htn+1]-1)&heatd[htn]==(heatd[htn+2]-2)) { 	## wenn drei Tage hintereinander ...
	           		num <- num + 3 ; cor <- cor + 2                            	## Hitzewelletage und Korrektur 
	            	for (jz in (htn+3):(lht)) {                                	## vierter Tag der Hitzewelle
		            	if(is.na(heatd[jz])!=T) {                             ## nur wenn Wert für jz vorhanden
	                			if(heatd[htn]==(heatd[jz]-jz+htn)) { num <- num + 1 ; cor <- cor + 1 } 
	              		}
	     			}
	      	}
			htn <- htn + cor 
	     	}
	}
	return(num/cnt)
}
