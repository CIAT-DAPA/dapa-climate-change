###############################################
# Copy

# where from
from="/mnt/GIS-HD722/climate_change/IPCC_CMIP3"

# where to
to="/data1/analogues_pine/data"

# years
years="2020_2049 2040_2069"

# sres
sress="A1B A2 B1"

for sres in A1B A2 B1
do
  for gcm in `ls $from/SRES_$sres/downscaled/Global_5min/`
  do
    for year in $years
    do
       for var in tmean tmin tmax prec
       do
          for month in $(seq 1 12)
          do
            cp -Rv $from/SRES_${sres}/downscaled/Global_5min/${gcm}/${year}/${var}_${month} $to/${sres}_${year}_${gcm}_${var}_${month}
            #echo $sres\_$year\_$gcm.zip > ~/geodata2/TNC_global_plants/data/proj/$sres\_$year\_$gcm.zip
         done 
      done
    done
  done
done

###############################################
# with R
i <- 'tmin'
i <- 'tmean'
i <- 'prec'

library(plyr)
library(stringr)
library(raster)


f <- list.files()

for (j in f[grepl(i, f)]) {
    if (i == "tmean") {
      this <- raster(j)

      e <- extent(-168, 123, -49, 56) # xmin xmas ymin ymax
      this <- crop(this, e)     
      this <- this/10
      writeRaster(this, filename=str_c(j, ".tif"))
    } else if (i == "prec") {
      this <- raster(j)

      e <- extent(-168, 123, -49, 56) # xmin xmas ymin ymax
      this <- crop(this, e)     
      this <- this/10
      writeRaster(this, filename=str_c(j, ".tif"))
    } else if (i == "tmin")
      tmin <- raster(j)

      jmax <- gsub("tmin","tmax", j)
      tmax <- raster(jmax)

      e <- extent(-168, 123, -49, 56) # xmin xmas ymin ymax
      
      dtr <- tmin - tmax
      dtr <- crop(dtr, e)
         
      dtr <- dtr/10
      writeRaster(dtr, filename=str_c(gsub("tmin","dtr",j), ".tif"))     
    }
}


# get all SRES
sres <- str_split(f,"_")
sres <- sapply(sres, function(x) x[1])
sres <- unique(sres)

# get all YEARS
yrs <- str_split(f,"_")
yrs <- sapply(yrs, function(x) str_c(x[2],"_",x[3]))
yrs <- unique(yrs)

# get all GCMS
gcms <- str_split(f,"_")
gcms <- sapply(gcms, function(x) str_c(x[4:(length(x)-2)], collapse="_"))
gcms <- unique(gcms)



# Import to GRASS Futur
g.region res=00:02:30n=90 s=-60 w=-180 e=180 
g.mapset mapset=futur -c

count=0

for i in *
do
  r.in.gdal in=$i out=$i
  count=$(echo $count + 1 | bc)
  echo $count
done


###############################################
# Calc dtr futur

for tmin in $(g.mlist pattern="*tmin*" type=rast)
do
  
  tmax=$(echo $tmin | sed 's/tmin/tmax/')
  dtr=$(echo $tmin | sed 's/tmin/dtr/')
  
  r.mapcalc "$dtr = $tmax - $tmin"
  
done


g.region res=00:05
for i in `g.mlist type=rast pattern="*dtr*"`
do
   r.mapcalc "tmp=$i/10"
   r.mapcalc $i=tmp
done

g.region res=00:05
for i in `g.mlist type=rast pattern="*tmean*"`
do
   r.mapcalc "tmp=$i/10"
   r.mapcalc $i=tmp
done

g.mremove rast="*_tmin*" -f
g.mremove rast="*_tmax*" -f


#######################################################
# Values at XY POI

r.mapcalc "MASK=mask_bin"

for i in $(g.mlist type=rast pattern="*")
do  
	cat poi_xy.txt | r.what input=$i fs=, | cut -f4 -d, > poi/${i}_poi.txt
  echo $i
done

#######################################################
# Values at XY REF

for i in $(g.mlist type=rast pattern="*")
do
  cat sites/20110613_all_trials.csv | awk -F, 'NR>1{print $6,$5}' | r.what input=$i fs=, | cut -f4 -d, > data_run/${i}_ref.txt
  echo $i
done


##################################
##################################
##################################
### Current

# Start GRASS in the $to directory 


# import 
g.region n=90 s=-60 w=-180 e=180 res=00:05

for i in *.bil
do
   r.in.bin byte=2 anull=-9999 north=90 south=-60 east=180 west=-180 rows=1800 cols=4320 in=$i out=$i -s --o
done 

# rename
for i in $(seq 1 12)
do
   g.rename rast=prec$i.bil,current_prec_$i
   g.remove rast=prec$i.bil

   g.rename rast=tmin$i.bil,current_tmin_$i
   g.remove rast=tmin$i.bil

   g.rename rast=tmax$i.bil,current_tmax_$i
   g.remove rast=tmax$i.bil

   g.rename rast=tmean$i.bil,current_tmean_$i
   g.remove rast=tmeani.bil
done

for i in $(seq 1 19)
do
   g.rename rast=bio$i.bil,current_bio_$i
   g.remove rast=bio$i.bil
done


# devide by 10
for i in `g.mlist type=rast pattern="current_tm*" `
do
   r.mapcalc "tmp=$i/10"
   r.mapcalc $i=tmp
done

g.remove rast=tmp

 
# calc dtr

for i in $(seq 1 12)
do
   r.mapcalc "current_dtr_$i=current_tmax_$i-current_tmin_$i"
done

# remove min max
g.mremove rast="*_tmin*" -f
g.mremove rast="*_tmax*" -f

# export current
g.mapset mapset=current
g.region res=0.5

for i in $(g.mlist type=rast patter="*" mapset="current")
do
  r.out.arc in=$i out=$(echo $i | tr '[:upper:]' '[:lower:]').asc
done

# rename bio_x to bio_x_1
for i in current_bio*.asc
do
  mv $i $(echo $i | cut -f1 -d.)_1.asc
done

###############################################
# Calc dtr
g.region res=00:10
for i in A1B_2020_2049 A1B_2040_2069
do
  g.mapset mapset=$i
  for j in $(g.mlist type=rast pattern="*tmean*")
  do
    r.mapcalc "tmp=$j/10"
    r.mapcalc $j=tmp
  done
done





# A1B_2030 
g.mapset mapset=A1B_2020_2049
g.region res=2

for i in $(g.mlist type=rast patter="*")
do
  r.out.arc in=$i out=$(echo $i | tr '[:upper:]' '[:lower:]').asc
done

# A1B_2050 current
g.mapset mapset=A1B_2040_2069
g.region res=2

for i in $(g.mlist type=rast patter="*")
do
  r.out.arc in=$i out=out=$(echo $i | tr '[:upper:]' '[:lower:]').asc
done

#######################################################
# Values at XY POI

for i in $(g.mlist type=rast pattern="*")
do
  cat sites/poi_xy.txt | r.what input=$i fs=, | cut -f4 -d, > data_run/${i}_poi.txt
  echo $i
done

#######################################################
# Values at XY REF

for i in $(g.mlist type=rast pattern="c*")
do
  cat sites/20110613_all_trials.csv | awk -F, 'NR>1{print $6,$5}' | r.what input=$i fs=, | cut -f4 -d, > data_run/${i}_ref.txt
  echo $i
done



#######################################################
#######################################################
#######################################################
# R ANALYSIS

# todo
# 1. get ref poitns matrix
# 2. test

library(snowfall)
library(stringr)

f <- list.files()
tmean <- f[grepl("tmean", f)]
tmean <- tmean[!grepl("current", tmean)]
tmean <- tmean[grepl("tmean_1_poi.txt", tmean)]

tmean <- str_sub(tmean, 1,-16)




sfInit(parallel=TRUE, cpus=22, type="SOCK")
sfExport("disPts")
sfExport("ccafsMPoints")
sfLibrary(stringr)


system.time(sfSapply(tmean, function(x) disPts(base=x, "fwd")))
 
sfStop()


disPts <- function(base, direction) {


  if (direction == "bwd") { 
    ref.base <- "current_"  # ref is current
    poi.base <- base
  } else if (direction== "fwd") {
    ref.base <- base  # ref is future
    poi.base <- "current_"
  }

  # ref  
  ref.tmean <- as.matrix(do.call(cbind, lapply(str_c(ref.base, "tmean_", 1:12, "_ref.txt"), read.table)))
  ref.dtr <- as.matrix(do.call(cbind, lapply(str_c(ref.base, "dtr_", 1:12, "_ref.txt"), read.table)))
  ref.prec <- as.matrix(do.call(cbind, lapply(str_c(ref.base, "prec_", 1:12, "_ref.txt"), read.table)))


  # poi 
  poi.tmean <- as.matrix(do.call(cbind, lapply(str_c(poi.base, "tmean_", 1:12, "_poi.txt"), read.table)))
  poi.dtr <- as.matrix(do.call(cbind, lapply(str_c(poi.base, "dtr_", 1:12, "_poi.txt"), read.table)))
  poi.prec <- as.matrix(do.call(cbind, lapply(str_c(poi.base, "prec_", 1:12, "_poi.txt"), read.table)))

  poi.tmean <- apply(poi.tmean,2,as.numeric)
  poi.dtr <- apply(poi.dtr,2,as.numeric)
  poi.prec  <- apply(poi.prec,2,as.numeric)

  # create a roll  
  roll.v <- c()
  months <- 1:12
  for (i in 1:length(months)) {
    roll.v <- c(roll.v, (months[c(i:length(months), 0:(i-1))]))  
  }

  roll <- matrix(data=roll.v, ncol=length(months), byrow=TRUE)

  for (h in 1:nrow(ref.tmean)) {
   
    results <- matrix(rep(NA, (12*nrow(poi.tmean))), ncol=12)

    for (j in 1:12) {
      results[, j] <- ccafsMPoints(ref.t=list(ref.tmean[h, roll[j, ]],ref.prec[h, roll[j, ]]), 
                        poi.t=list(poi.tmean,poi.prec), 
                        ref.w=list(ref.dtr[h, roll[j, ]], rep(1,12)),
                        poi.w=list(poi.dtr, matrix(rep(1, (12*nrow(poi.dtr))),ncol=12)))
    }

    results <- apply(results,1,min)   
    
    write.table(results, str_c("../", direction, '/', base, "s-", h, ".txt", sep=""), row.names=F, col.names=F, quote=F)
  }
 
}

# ------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------ #

#' Calculate ccafs dissimilarity between points with climate information
#'
#' @param ref.t: vector with values of vars at reference point
#' @param poi.t: matrix with values of vars at poi
#' @param ref.w: vector with values of weights at reference point
#' @param poi.w: matrix with values of weights at poi
#' @param z: number parameter (default 2, equal to euclidean distance)
#' @return A vector of length nrow(poi.t), containing dissimilarity
#' @export

ccafsMPoints <- function(ref.t, poi.t, ref.w, poi.w, z=2) {
  # Packages
  require(stringr)  # for enhance string operations
  
  # Number of variables
  nvars <- length(ref.t)
  ndivisions <- ncol(poi.t[[1]])
  
  # Validity testing
  if (length(ref.t) != length(poi.t)) {
    stop(str_c("ref has ", length(ref.t), " elements and poi has ", 
      length(poi.t), " elements, they need to be equal"))
  }
  
  # TODO list validity
   
  # dissimilarity
  # 1. substract, from each column of the poi, substract the correspongind
  #    reference vale
  tmp <- lapply(1:nvars, function(x) (t(poi.t[[x]]) - ref.t[[x]]))

  # 2. to the power z
  tmp <- lapply(tmp, function(x) x^z)

  # 3.  Weights, if all weights are the same, so nothing, just assign the weight
  #     the weight as it is, if they are different, devide project by base
  wei <- list()

  for (x in 1:nvars) {  
  
    # 3a. devide weights
    if (!all(poi.w[[x]] == ref.w[[x]])) {
      wei[[x]] <- t(poi.w[[x]]) / ref.w[[x]]
    } else {
     # 3b.  leave it as it is
      wei[[x]] <- t(poi.w[[x]])
    }
  }
 
  # 4. multiply by weights
  tmp <- lapply(1:nvars, function(x) tmp[[x]] * wei[[x]])

  # 5. sum accross devisions, transpose back
  tmp <- rowSums(do.call(cbind, lapply(tmp,t)))

  # 6. take the zth root
  tmp <- tmp^(1/z)
  
  return(tmp)
}


# ------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------ #
# Get rownumber of ref points                                                    #
# ------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------ #

# in grass

cat sites/20110613_all_trials.csv | awk -F, 'NR>1{print $6,$5}' | r.what input=$i fs=, | cut -f1,2 -d, > sites/ref_sites_row_numbers.txt

# in R
poi <- read.table("poi_xy.txt")
ref <- read.table("ref_sites_row_numbers.txt", sep=",")

rows <- c()

for (i in 1:nrow(ref)) {
	rows <- c(rows, which(poi[,1] == ref[i,1] & poi[,2] == ref[i,2])[1])
}

write.table(rows, "which_rows_ref.txt", col.names=F, row.names=F, quote=F)

# ------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------ #
# For each point provinence and trial                                            #
# ------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------ #


#@ ff <- list.files(path="../fwd", pattern="A2_2040_2069_mpi_echam5_s*", full=T)

ff <- list.files(path="../fwd", pattern="*s-41.txt", full=T)

ff <- str_sub(ff, 1, -7)


findPT <- function(path, disToRef, ref) {

	ffull <- do.call(cbind, lapply(str_c(path, "1:67", read.table))

#@	rows <- read.table("./sites/which_rows_ref.txt")
#@	rows <- 1000:1066

	ref <- read.csv("./sites/20110613_all_trials.csv")

	res <- matrix(rep(NA, (nrow(ffull) * length(unique(ref[,1]))))

	for (i in unique(ref[,1])) {

		where <- which(ref[,1]==i)

		meanNotTop <- rowMeans(ffull[,where[-1]])
		minNotTop <-  apply(ffull[,where[-1]], 1, which.min)
		res[,i] <- ifelse(meanNotTop < ffull[,where[1]], minNotTop, where[1])	

	}
	# write matrix
}



