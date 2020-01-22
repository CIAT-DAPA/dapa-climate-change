###############################
# Proposito: Pasos para downscaling CMIP6
# Autor: Jaime Tarapues
# Fechas: 2020
# jaime.tm8@gmail.com
# Se recomienda correr desde Linux, htCondor
############################


##########################
# 01 - organizar archivos
##########################
#dirbase="/dapadfs/workspace_cluster_13/ADAA/CMIP6/raw"
dirbase="X:/ADAA/CMIP6/raw"
outdir="X:/ADAA/CMIP6/monthly"
listNC <-  list.files(dirbase, recursive = FALSE, full.names = FALSE,pattern = ".nc") 
for(nc in listNC){
  var=tolower(sapply(strsplit(nc, '[_]'), "[[", 1))   
  gcm=tolower(sapply(strsplit(nc, '[_]'), "[[", 3))
  periodo=gsub(".nc","",tolower(sapply(strsplit(nc, '[_]'), "[[", 7)))
  yi=sapply(strsplit(periodo, '[-]'), "[[", 1)
  yf=sapply(strsplit(periodo, '[-]'), "[[", 2)    
  ens=tolower(sapply(strsplit(nc, '[_]'), "[[", 5))
  grilla=tolower(sapply(strsplit(nc, '[_]'), "[[", 6))
  rcp=tolower(sapply(strsplit(nc, '[_]'), "[[", 4))
  
  outrcp=paste0(outdir,"/",rcp)
  dir.create(outrcp,showWarnings=FALSE,recursive = T)
  file.copy(paste0(dirbase,"/",nc), outrcp,overwrite = F)
  #system(paste0("mv -r ",,)) # linux
  #system(paste0("robocopy ",,)) # win
  
}
#######################################
# 02 Invetario de variales y periodos
#################################
library(ncdf4)
dirbase="X:/ADAA/CMIP6/monthly"
outdir="X:/ADAA/CMIP6/monthly"
listNC <-  list.files(dirbase, recursive = T, full.names = T,pattern = ".nc") 
reporte=paste0(outdir,"/Inventory_monthly.txt")
opnFile <- file(reporte, open="a",encoding = "UTF-8")
cat(paste("file","rcp","gcm","ens","grilla","var","yi","yf","xres","yres","ndates",sep="\t"), file=opnFile)
for(nc in listNC){
  var=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 1))   
  gcm=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 3))
  periodo=gsub(".nc","",tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 7)))
  yi=sapply(strsplit(periodo, '[-]'), "[[", 1)
  yf=sapply(strsplit(periodo, '[-]'), "[[", 2)    
  ens=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 5))
  grilla=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 6))
  rcp=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 4))
  ncin <- nc_open(nc)
  lon <- ncvar_get(ncin,"lon")
  tiempo <- ncvar_get(ncin,"time")
  lat <- ncvar_get(ncin,"lat")
  xres=(lon[dim(lon)]-lon[1])/dim(lon)
  yres=(lon[dim(lat)]-lon[1])/dim(lat)
  cat("\n",file=opnFile)
  cat(paste(basename(nc),rcp,gcm,ens,grilla,var,yi,yf,xres,yres,dim(tiempo),sep="\t"), file=opnFile) 
}
close.connection(opnFile)

###########################
# 03 calcular los promedios
#    Se debe correr en linux con cdo
###########################
dirbase="/dapadfs/workspace_cluster_13/ADAA/CMIP6/monthly/"#"X:/ADAA/CMIP6/monthly"
outdir="/dapadfs/workspace_cluster_13/ADAA/CMIP6/monthly_/"
mask1deg="/dapadfs/workspace_cluster_13/ADAA/CMIP6/_scripts/buff_md.nc"

listrcp <-  list.dirs(dirbase,recursive = F,full.names = F) 
for(rcp in listrcp){
  listNC <-  list.files(paste0(dirbase,"/",rcp), recursive = F, full.names = T,pattern = ".nc")   
  for(nc in listNC){
    var=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 1))   
    gcm=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 3))
    periodo=gsub(".nc","",tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 7)))
    yi=sapply(strsplit(periodo, '[-]'), "[[", 1)
    yf=sapply(strsplit(periodo, '[-]'), "[[", 2)    
    ens=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 5))
    grilla=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 6))
    rcp=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 4))  
    
    outrcp=paste0(outdir,"/",rcp,"/",gcm,"/",ens,"/",grilla,"/average")
    dir.create(outrcp,showWarnings=FALSE,recursive = T)
    
    if (rcp == "historical"){
      periodList <- c("1961", "1971")
    } else {
      periodList <- c("2020","2070") # "2030", "2040", "2050", "2060", 
    }  
    if(rcp=="historical" || rcp=="ssp126" || var=="tasmin"){
      for (period in periodList) {
        
        # Define start and end year
        staYear <- as.integer(period)
        endYear <- as.integer(period) + 29
        cat(".>Processing: ",basename(nc),staYear,endYear)
        selyearnc=paste0(outrcp,"/",var,"_",rcp,"_",gcm,"_",staYear,"_",endYear,"_sely.nc")
        conver=paste0(outrcp,"/",var,"_",rcp,"_",gcm,"_",staYear,"_",endYear,"_conver.nc")
        avgclim=paste0(outrcp,"/",var,"_",rcp,"_",gcm,"_",staYear,"_",endYear,".nc")
        ## calculo de promedio conversion de unidades y remapping a 1 grado
        if(!file.exists(avgclim)){
          system(paste0("cdo selyear,",staYear,"/",endYear," ",nc," ",selyearnc), intern = TRUE)

          ## conversion de unidades y enteros
          if (var=="tasmin" || var=="tasmax"){
            system(paste0("cdo -subc,273.15 ",selyearnc," ",conver), intern = TRUE)
            system(paste0("cdo ymonmean ",conver," ",avgclim), intern = TRUE) 
          }
          if (var=="pr"){
            system(paste0("cdo -mulc,86400 ",selyearnc," ",conver), intern = TRUE)
            system(paste0("cdo ymonsum ",conver," ",avgclim), intern = TRUE)   
          }
          if (var=="rsds"){
            system(paste0("cdo -mulc,0.0864 ",selyearnc," ",conver), intern = TRUE)
            system(paste0("cdo ymonmean ",conver," ",avgclim), intern = TRUE) 
          }    
          unlink(c(selyearnc,conver))
        }
        
        ### calculo de anomalias
        if (rcp != "historical"){
          periodListHist <- c("1961", "1971")
          for (periodH in periodListHist) {
            staYearHist <- as.integer(periodH)
            endYearHist <- as.integer(periodH) + 29  
            
            diranom=paste0(outdir,"/",rcp,"/",gcm,"/",ens,"/",grilla,"/anomalies_",as.integer(periodH)+14,"/",staYear,"_",endYear)
            dir.create(diranom,showWarnings=FALSE,recursive = T)    
            avgclimHist=paste0(outdir,"/historical/",gcm,"/",ens,"/",grilla,"/average/",var,"_historical_",gcm,"_",staYearHist,"_",endYearHist,".nc")
            tempAnom=paste0(diranom,"/",var,"_anom.nc")
            tempAnomPr=paste0(diranom,"/",var,"_anomPr.nc")
            temphist=paste0(diranom,"/",var,"_anomHist.nc")
            tempbox=paste0(diranom,"/",var,"_box.nc")
            sumnc=paste0(diranom,"/",var,"_sum.nc")
            mulnc=paste0(diranom,"/",var,"_mul.nc")
            donenc=paste0(diranom,"/",var,".nc")
            remap1deg=paste0(diranom,"/",var,"_remp.nc")
            
            if(var=="pr"||var=="rsds"){
              system(paste0("cdo sub ",avgclim," ",avgclimHist," ",tempAnomPr), intern = TRUE)
              system(paste0("cdo addc,0.5 ",avgclimHist," ",temphist), intern = TRUE)
              system(paste0("cdo div ",tempAnomPr," ",temphist," ",tempAnom), intern = TRUE)
              
            }else{
              system(paste0("cdo sub ",avgclim," ",avgclimHist," ",tempAnom), intern = TRUE)
              
            }
            
            splitmonNC=paste0(diranom,"/",var,"_")
            system(paste0("cdo splitmon ",tempAnom," ",splitmonNC), intern = TRUE)
            
            
            ## extract mask
            # system(paste0("cdo remapbil,/dapadfs/workspace_cluster_13/ADAA/CMIP6/_scripts/rr.txt ",tempAnom," ",remap1deg), intern = TRUE)
            # system(paste0("cdo sellonlatbox,0,360,-60,90 ",remap1deg," ",tempbox)) 
            # 
            
            
          }
          
          
          
          
        }
        
        
        
        

        
        cat("..done!\n")
      }    
    }
    
  }
}


#########################



ncListVar <- list.files(dirgcm,pattern = "SDII",recursive = T,full.names = TRUE)

nc="/mnt/data_climatewizard/AR5_Global_Daily_25k/out_stats/ensemble/SDII_BCSD_historical_ensemble_1950-2005_monthly.nc"
outdir="/mnt/data_climatewizard/AR5_Global_Daily_25k/out_stats_tiff" # 
gcm="ensemble"
dirtif <- paste0(outdir, "/", gcm)
tifFact=paste0(dirtif,'/',sapply(strsplit(basename(nc), '[.]'), "[[", 1),'_fact.tif')
tifint=paste0(dirtif,'/',sapply(strsplit(basename(nc), '[.]'), "[[", 1),'_int32.tif')
tif=paste0(dirtif,'/',sapply(strsplit(basename(nc), '[.]'), "[[", 1),'.tif')

prec=stack(lapply(nc, FUN=brick))
prec=rotate(prec)
prec=crop(prec,raster(mask))
prec=mask(prec, raster(mask))      
writeRaster(prec*100,tifFact , format='GTiff', overwrite=T)
#system(paste0("gdal_translate -ot Int16 -a_nodata -9999 -of GTiff ",tifFact," ",tifint),intern = T)
system(paste0("gdal_translate -ot Int32 -of GTiff ",tifFact," ",tifint),intern = T)
#system(paste0("gdal_translate -co COMPRESS=DEFLATE -co PREDICTOR=1 ",tifraw," ",tif),intern = T)
system(paste0("gdal_translate -co COMPRESS=DEFLATE -co PREDICTOR=1 ",tifint," ",tif),intern = T)
unlink(tifFact) 
unlink(tifint)

### para calcular precipitacion
dirbase="/mnt/data_climatewizard/AR5_Global_Daily_25k/" #"/mnt/climatewizard/data/AR5_Global_Daily_25k/" # "F:/ClimateWizard/data/AR5_Global_Daily_25k/"# 
rcplist=c("historical","rcp45","rcp85")
gcmlist=list.dirs(paste0(dirbase,'out_stats/'), recursive = FALSE, full.names = FALSE) 

for(model in gcmlist){
    ncList <- list.files(paste0(dirbase,"/out_stats/",model,"/"),pattern="pr",recursive = F,full.names = F)
    for(rcp in rcplist){    
      ncList2 <- grep(rcp, ncList, value=TRUE, fixed=TRUE)
      if(length(ncList2)==12){
        period=gsub(".nc", "",sapply(strsplit(basename(ncList2[1]), '[_]'), "[[", 5))
        outmon=paste0(dirbase,'out_stats/',model,"/pr_BCSD_",rcp,"_",model,"_",period,"_monthly.nc")
        outyear=paste0(dirbase,'out_stats/',model,"/pr_BCSD_",rcp,"_",model,"_",period,".nc")
        cat(model,rcp)
        if(!file.exists(outmon)){
          system(paste0("cdo mergetime ",paste(paste0(dirbase,"/out_stats/",model,"/",ncList2), collapse = " ")," ",outmon))
        }
        if(!file.exists(outyear)){
          system(paste0("cdo yearsum ",outmon," ",outyear))
        }
        cat("...done! \n")
      }
    }  
    # ncList <- list.files(dirbase,pattern=paste0("pr_day_BCSD_",rcp,"_r1i1p1_",model),recursive = F,full.names = T)
    # yi=gsub(".nc", "",sapply(strsplit(basename(ncList[1]), '[_]'), "[[", 7))
    # yf=gsub(".nc", "",sapply(strsplit(basename(ncList[length(ncList)]), '[_]'), "[[", 7)) 
    # outfile=paste0(dirbase,'out_stats/',model,"/pr_BCSD_rcp45_ACCESS1-0_",yi,"-",yf,"_.nc")
    # prmm=paste0(dirbase,'out_stats/',model,"/pr-mm_BCSD_rcp45_ACCESS1-0_",yi,"-",yf,".nc")
    # cat(model,rcp)
    # system(paste0("cdo mergetime ",paste(ncList, collapse = " ")," ",outfile))
    # system(paste0("cdo -m 1e+20 -mulc,86400 ",outfile," ",prmm))
    

}


### para rotar y cortar por mascara datos NC

## create mask
library(raster)

dirbase="/dapadfs/workspace_cluster_13/ADAA/CMIP6"
gridtxt=paste0(dirbase,"/_scripts/mask1km.txt")
grid1deg=paste0(dirbase,"/_scripts/mask1deg.txt")
mask1km=paste0(dirbase,"/_scripts/mask1km.nc")
mask1deg=paste0(dirbase,"/_scripts/mask1deg.nc")

rmask1km=raster(grid1deg)
mask1deg=raster("X:/ADAA/CMIP6/_scripts/mask1deg.nc")
buff=raster("X:/ADAA/CMIP6/_scripts/mask_buf.tif")
bb <- extent(mask1deg)
r2=raster(extent(0, 360, -60, 90),nrows=150, ncols=360)
r2[]=1
shpbuf=shapefile("X:/ADAA/CMIP6/_scripts/shp/world_buff.shp")
y=mask(r2,shpbuf)
extent(y) <- extent(-180, 180, -60, 90)
rr <- rotate(y)
writeRaster(r2,"X:/ADAA/CMIP6/_scripts/rr.nc",format="CDF",overwrite=TRUE)

dx=raster("X:/ADAA/CMIP6/monthly/ssp585/canesm5/r1i1p1f1/gn/anomalies_1985/2070_2099/tasmin.nc")
dy=raster("X:/ADAA/CMIP6/_scripts/buff_md.nc")
dy=raster("X:/ADAA/CMIP6/_scripts/target.nc")
dz=raster("X:/ADAA/CMIP6/monthly/ssp585/canesm5/r1i1p1f1/gn/anomalies_1985/2070_2099/tasmin.nc")

vc=raster("X:/ADAA/CMIP6/monthly_/ssp126/canesm5/r1i1p1f1/gn/anomalies_1985/2070_2099/tasmin_anom.nc")
plot(vc)
writeRaster(rotate(vc),"X:/ADAA/CMIP6/_scripts/test_anom2.nc",format="CDF")

plot(dz)
plot(shpbuf,add=T)

extent(buff) <- bb
r <- setExtent(buff, bb, keepres=TRUE)
plot(mask1deg)
NAvalue(r)

e <- extent(-180, 0, 0, 90)
mask1kmA=crop(rmask1km,e)
writeRaster(mask1kmA,"X:/ADAA/CMIP6/_scripts/mask1km_A.nc",format="CDF")
# writeRaster(rmask1km,"X:/ADAA/CMIP6/_scripts/mask1deg.nc",format="CDF")
# cdo griddes /dapadfs/workspace_cluster_13/ADAA/CMIP6/_scripts/mask1deg.nc > /dapadfs/workspace_cluster_13/ADAA/CMIP6/_scripts/mask1deg.txt


outdir="/dapadfs/workspace_cluster_13/ADAA/CMIP6/test/" #"/mnt/data_climatewizard/AR5_Global_Daily_25k/out_stats_cut/" #"F:/ClimateWizard/data/AR5_Global_Daily_25k/out_stats_cut/" #
gcmi=20#1#
gcmf=23#5#
gcmlist <-  list.dirs(dirbase, recursive = FALSE, full.names = FALSE) 
check_ndate=c()

cut_mask=function(outdir,gcm2,var,type,rcp,yi,yf,mon){
  if(mon==F){
    donenc=paste0(outdir,gcm2,"/",var,"_",type,"_",rcp,"_",gcm2,"_",yi,"-",yf,".nc")
  }else{
    donenc=paste0(outdir,gcm2,"/",var,"_",type,"_",rcp,"_",gcm2,"_",yi,"-",yf,"_monthly.nc")
  }
  
  if(!file.exists(donenc)){
    nc=paste0(dirbase,"/raw/pr_Amon_BCC-ESM1_historical_r1i1p1f1_gn_185001-201412.nc")
    gcm2="BCC-ESM1"
    var="pr"
    type="dwn"
    rcp="historical"
    yi=1985
    yf=2005
    dir.create(paste0(outdir,gcm2),showWarnings=FALSE)
    selyearnc=paste0(outdir,gcm2,"/",var,"_",type,"_",rcp,"_",gcm2,"_",yi,"_",yf,"_sel.nc")
    rotnc=paste0(outdir,gcm2,"/",gsub(".nc", "_rot.nc", basename(nc)))
    sumnc=paste0(outdir,gcm2,"/",gsub(".nc", "_sum.nc", basename(nc)))
    masknc=paste0(outdir,gcm2,"/",gsub(".nc", "_mask.nc", basename(nc)))
    nck=paste0(outdir,gcm2,"/",var,"_",type,"_",rcp,"_",gcm2,"_",yi,"-",yf,"_k.nc")
    remap1deg=paste0(outdir,gcm2,"/",gsub(".nc", "_remp1deg.nc", basename(nc)))
    donenc=paste0(outdir,gcm2,"/",var,"_",type,"_",rcp,"_",gcm2,"_",yi,"-",yf,".nc")
    donenc2=paste0(outdir,gcm2,"/",var,"_",type,"_",rcp,"_",gcm2,"_",yi,"-",yf,"2.nc")
    donenc3=paste0(outdir,gcm2,"/",var,"_",type,"_",rcp,"_",gcm2,"_",yi,"-",yf,"3.nc")
    donenc4=paste0(outdir,gcm2,"/",var,"_",type,"_",rcp,"_",gcm2,"_",yi,"-",yf,"4.nc")
    donenc5=paste0(outdir,gcm2,"/",var,"_",type,"_",rcp,"_",gcm2,"_",yi,"-",yf,"5.nc")
    donenc6=paste0(outdir,gcm2,"/",var,"_",type,"_",rcp,"_",gcm2,"_",yi,"-",yf,"6.txt")
    
    system(paste0("cdo selyear,",yi,"/",yf," ",nc," ",selyearnc), intern = TRUE)
    system(paste0("cdo remapbil,r360x180 ",selyearnc," ",remap1deg), intern = TRUE)
    system(paste0("cdo remapbil,",grid1deg," ",remap1deg," ",rotnc), intern = TRUE)
    ## mask
    system(paste0("cdo add ",mask1deg," ",rotnc," ",sumnc), intern = TRUE)
    system(paste0("cdo mul ",mask1deg," ",sumnc," ",masknc), intern = TRUE)
    system(paste0("cdo sub ",masknc," ",mask1deg," ",donenc), intern = TRUE)    
    ## conversion de unidades y enteros
    if (var=="tasmin" || var=="tasmax"){
      system(paste0("cdo -subc,273.15 ",donenc," ",donenc2), intern = TRUE)
      system(paste0("cdo ymonmean ",donenc2," ",donenc3), intern = TRUE) 
      unlink(nck)
    }if (var=="pr"){
      system(paste0("cdo -mulc,86400 ",donenc," ",donenc2), intern = TRUE)
      system(paste0("cdo ymonsum ",donenc2," ",donenc3), intern = TRUE)   
    }if (var=="rsds"){
      system(paste0("cdo -mulc,0.0864 ",donenc," ",donenc2), intern = TRUE)
      system(paste0("cdo ymonmean ",donenc2," ",donenc3), intern = TRUE) 
    } 
    #split
    #sellonlatbox,lon1,lon2,lat1,lat2 infile outfile
    system(paste0("cdo sellonlatbox,-180,0,90,0 ",donenc3," ",donenc4), intern = TRUE) 
    system(paste0("cdo sellonlatbox,-180,0,90,0 ",mask1km," ",donenc5), intern = TRUE) 
    system(paste0("cdo griddes ",donenc5," > ",donenc6), intern = TRUE) 
    
    unlink(c(selyearnc,rotnc,sumnc,masknc))
  }  
}



filtro=".nc" #"pr_"#
for(gcm in gcmlist[gcmi:gcmf]){ #[gcmi:gcmf]
  dirnc=paste0(dirbase,gcm)
  ncListVar <- list.files(dirnc,pattern=paste0(filtro),recursive = FALSE,full.names = TRUE)
  for(nc in ncListVar){
    t1 <- try(system(paste0("cdo ndate ",nc), intern = TRUE))
    n=as.numeric(t1[1])
    
    
    var=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 1))   
    ext=sapply(strsplit(basename(nc), '[.]'), "[[", 2)
    type=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 2))
    rcp=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 3))
    gcm2=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 4))
    
    if(rcp!="historical" && ext=="monthly"){
      if(n>1079){
        cat("-->Run ",basename(nc))
        periodlist=c("2026-2045","2081-2099","2046-2065")
        for(period in periodlist){
          yi=sapply(strsplit(period, '[-]'), "[[", 1)
          yf=sapply(strsplit(period, '[-]'), "[[", 2)        
          cut_mask(outdir,gcm2,var,type,rcp,yi,yf,T)
        }
      }
    }
    if(rcp=="historical" && ext=="monthly"){
      if(n>671){
        #unlink(nc)
        #check_ndate=c(check_ndate,nc)
        cat("-->Run ",basename(nc))
        yi=1961
        yf=1999  
        cut_mask(outdir,gcm2,var,type,rcp,yi,yf,T)
      }
    }   
    if(rcp=="historical" && ext=="nc"){
      if(n>55 && n <58){
        #unlink(nc)
        #check_ndate=c(check_ndate,nc)
        dir.create(paste0(outdir,gcm2),showWarnings=FALSE)
        yi=1961
        yf=1999        
        cut_mask(outdir,gcm2,var,type,rcp,yi,yf,F)
        #write.table(inDir, file = paste0(dirname(nc),"/maskdone.txt"), sep = "\t",row.names = TRUE, col.names = NA)
      }
    } 
    if(rcp!="historical" && ext=="nc"){
      if(n>89 && n <96){
        #unlink(nc)
        #check_ndate=c(check_ndate,nc)
        dir.create(paste0(outdir,gcm2),showWarnings=FALSE)
        periodlist=c("2026-2045","2081-2099","2046-2065")
        for(period in periodlist){
          yi=sapply(strsplit(period, '[-]'), "[[", 1)
          yf=sapply(strsplit(period, '[-]'), "[[", 2)
          donenc=paste0(outdir,gcm2,"/",var,"_",type,"_",rcp,"_",gcm2,"_",yi,"-",yf,".nc")
          if(!file.exists(donenc)){
            selyearnc=paste0(outdir,gcm2,"/",var,"_",type,"_",rcp,"_",gcm2,"_",yi,"_",yf,"_sel.nc")
            rotnc=paste0(outdir,gcm2,"/",gsub(".nc", "_rot.nc", basename(nc)))
            sumnc=paste0(outdir,gcm2,"/",gsub(".nc", "_sum.nc", basename(nc)))
            masknc=paste0(outdir,gcm2,"/",gsub(".nc", "_mask.nc", basename(nc)))
            nck=paste0(outdir,gcm2,"/",var,"_",type,"_",rcp,"_",gcm2,"_",yi,"-",yf,"_k.nc")
            system(paste0("cdo selyear,",yi,"/",yf," ",nc," ",selyearnc), intern = TRUE)
            system(paste0("cdo remapbil,/mnt/climatewizard/data/temp/mask25km.txt ",selyearnc," ",rotnc), intern = TRUE)
            system(paste0("cdo add /mnt/climatewizard/data/temp/mask1.nc ",rotnc," ",sumnc), intern = TRUE)
            system(paste0("cdo mul /mnt/climatewizard/data/temp/mask25km.nc ",sumnc," ",masknc), intern = TRUE)
            if (var=="txavg" || var=="tnavg" || var=="txx" || var=="tnn" || var=="tas" || var=="tasmin" || var=="tasmax" || substr(var, 1, 3)=="tas"|| substr(var, 1, 3)=="tnn"|| substr(var, 1, 3)=="txx"){
              system(paste0("cdo sub ",masknc," /mnt/climatewizard/data/temp/mask25km.nc ",nck), intern = TRUE)
              system(paste0("cdo -subc,273.15 ",nck," ",donenc), intern = TRUE)
              unlink(nck)
            }else{
              system(paste0("cdo sub ",masknc," /mnt/climatewizard/data/temp/mask25km.nc ",donenc), intern = TRUE)
            }            
            unlink(c(selyearnc,rotnc,sumnc,masknc))
          }          
        }
      }
    }    
    if(rcp=="historical" && ext=="quarter"){
      if(n<500){
        #unlink(nc)
        #check_ndate=c(check_ndate,nc)
        cat("..ERROR! \n")
      }
    }
    if(rcp!="historical" && ext=="quarter"){
      if(n<895){
        #unlink(nc)
        #check_ndate=c(check_ndate,nc)
        cat("..ERROR! \n")
      }
    }  
    if(rcp=="historical" && ext=="seasonal"){
      if(n<200){
        #unlink(nc)
        #check_ndate=c(check_ndate,nc)
        cat("..ERROR! \n")
      }
    }   
    if(rcp!="historical" && ext=="seasonal"){
      if(n<260){
        #unlink(nc)
        #check_ndate=c(check_ndate,nc)
        cat("..ERROR! \n")
      }
    }    
    cat("\n")
  }
}



# cambiar nombre de variable de archivo ncdf
dirbase="/mnt/climatewizard/data/AR5_Global_Daily_25k/out_stats_cut/"
for(gcm in gcmlist[gcmi:gcmf]){
  dirnc=paste0(dirbase,gcm)
  ncListVar <- list.files(dirnc,pattern=paste0("pr_"),recursive = FALSE,full.names = TRUE)
  for(nc in ncListVar){
    var=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 1))  
    t2 <- try(system(paste0("cdo showname ",nc), intern = TRUE))
    t2 <- sapply(strsplit(basename(t2), '[ ]'), "[[", 2)
    if(var!=t2){
      cat(basename(nc),"\n")
      try(system(paste0("ncrename -h -v ", t2,",",var," ",nc), intern = TRUE)) 
    }
  }
}
#cambiar nombre variable CDD
dirbase="/mnt/climatewizard/data/AR5_Global_Daily_25k/out_stats_cut/"
gcmi=1#1#
gcmf=1#5#
gcmlist <-  list.dirs(dirbase, recursive = FALSE, full.names = FALSE) 
for(gcm in gcmlist){ #[gcmi:gcmf]
  dirnc=paste0(dirbase,gcm)
  ncListVar <- list.files(dirnc,pattern=paste0("cdd_"),recursive = FALSE,full.names = TRUE)
  for(nc in ncListVar){
    var=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 1))  
    t2 <- try(system(paste0("cdo showname ",nc), intern = TRUE))
    t2 <- sapply(strsplit(basename(t2), '[ ]'), "[[", 2)
    if(var!=t2 & t2=="consecutive_dry_days_index_per_time_period"){
      cat(basename(nc),"\n")
      try(system(paste0("ncrename -h -v ", t2,",",var," ",nc), intern = TRUE)) 
    }
  }
}

# copiar archivos
dirnc="F:/ClimateWizard/data/AR5_Global_Daily_25k"
target="M:/ClimateWizard/RAW"
ncListVar <- list.files(dirnc,pattern=paste0(".nc"),recursive = FALSE,full.names = TRUE)
for(nc in ncListVar){
  file.copy(nc,target)
  if(file.exists(paste0(target,'/',basename(nc)))){
    unlink(nc)
  }
}


### pruebas
library(raster)
library(lubridate)
library(plotly)
nc="F:/ClimateWizard/data/AR5_Global_Daily_25k/out_stats_tiff/access1-0/split/tas_bcsd_historical_access1-0_1950-1960_monthly_split.tif"

prec=lapply(nc, FUN=brick)
prec=stack(prec)
extr=(extract(prec,cbind(7.43,46.94))/100)-273.15
fechas=format(seq(ISOdate(1950,1,1), ISOdate(1960,12,1), "month"), format="%Y/%m/%d")

sc<-matrix(nrow=nrow(t(extr)),ncol=4)
sc<-as.data.frame(sc)
sc[,1]<-year(fechas)
sc[,2]<-month(fechas)
sc[,3]<-day(fechas)
sc[,4]<-t(extr)
aggregate(sc[,4],list(sc[,2]),FUN=function(x) {mean(x, na.rm=T)})



