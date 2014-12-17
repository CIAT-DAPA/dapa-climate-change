require(raster)
require(ncdf)

### Archivos cortados y partidos en diarios (compactados en archivos mensuales)

## prec diaria raw
pr <- raster("N:/bid/gcm_raw_res/bcc_csm1_1/1971_2000/by-month/pr_1971_01.nc")
plot(pr * 86400)

## tasmax diaria raw
tasmax <- raster("N:/bid/gcm_raw_res/bcc_csm1_1/1971_2000/by-month/tasmax_1971_01.nc")
plot(tasmax - 273.15)
hist(tasmax - 273.15)

## tasmin diaria raw
tasmin <- raster("N:/bid/gcm_raw_res/bcc_csm1_1/1971_2000/by-month/tasmin_1971_01.nc")
plot(tasmin - 273.15)
hist(tasmin - 273.15)

## dtr diaria raw
plot(tasmax - tasmin)
hist(tasmax - tasmin)

## rsds diaria raw
rsds <- raster("N:/bid/gcm_raw_res/bcc_csm1_1/1971_2000/by-month/rsds_1971_01.nc")
plot(rsds)
hist(rsds)


### Archivos cortados y partidos en diarios (compactados en archivos mensuales), y reggrided a 0.5deg

## prec diaria raw
pr <- raster("N:/bid/gcm_0_5deg_lat/bcc_csm1_1/1971_2000/by-month/prec_1971_01.nc")
plot(pr)

## tasmax diaria raw
tasmax <- raster("N:/bid/gcm_0_5deg_lat/bcc_csm1_1/1971_2000/by-month/tmax_1971_01.nc")
plot(tasmax)
hist(tasmax)

## tasmin diaria raw
tasmin <- raster("N:/bid/gcm_0_5deg_lat/bcc_csm1_1/1971_2000/by-month/tmin_1971_01.nc")
plot(tasmin)
hist(tasmin)

## dtr diaria raw
plot(tasmax - tasmin)
hist(tasmax - tasmin)

## rsds diaria raw
rsds <- raster("N:/bid/gcm_0_5deg_lat/bcc_csm1_1/1971_2000/by-month/rsds_1971_01.nc")
plot(rsds)
hist(rsds)



### Archivos dairios promediados y calc std por mes

## prec avg/std
pr_avg <- raster("N:/bid/gcm_0_5deg_lat/bcc_csm1_1/1971_2000/prec_1971_2000_01_avg.nc")
pr_std <- raster("N:/bid/gcm_0_5deg_lat/bcc_csm1_1/1971_2000/prec_1971_2000_01_std.nc")
plot(pr_std)
hist(pr_avg)

## tasmax avg/std
tmax_avg <- raster("N:/bid/gcm_0_5deg_lat/bcc_csm1_1/1971_2000/tmax_1971_2000_01_avg.nc")
tmax_std <- raster("N:/bid/gcm_0_5deg_lat/bcc_csm1_1/1971_2000/tmax_1971_2000_01_std.nc")
plot(tmax_avg)
hist(tmax_std)

## tasmin avg/std
tmin_avg <- raster("N:/bid/gcm_0_5deg_lat/bcc_csm1_1/1971_2000/tmin_1971_2000_01_avg.nc")
tmin_std <- raster("N:/bid/gcm_0_5deg_lat/bcc_csm1_1/1971_2000/tmin_1971_2000_01_std.nc")
plot(tmin_std)
hist(tmin_avg)

## dtr diaria raw
plot(tmax_avg - tmin_avg)
hist(tmax_avg - tmin_avg)

## rsds avg/std
rsds_avg <- raster("N:/bid/gcm_0_5deg_lat/bcc_csm1_1/1971_2000/rsds_1971_2000_01_avg.nc")
rsds_std <- raster("N:/bid/gcm_0_5deg_lat/bcc_csm1_1/1971_2000/rsds_1971_2000_01_std.nc")
plot(rsds_avg)
hist(rsds_avg)


## ---- FUTURE ------

### Archivos dairios promediados y calc std por mes

## prec daily future
pr_fut <- raster("W:/bid-cc-agricultural-sector/01-climate-data/gcm_0_5deg_lat/bcc_csm1_1/2020_2049/by-month/prec_2020_01.nc")
plot(pr_fut - pr)
hist(pr_fut - pr)

## prec daily future
tmax_fut <- raster("W:/bid-cc-agricultural-sector/01-climate-data/gcm_0_5deg_lat/bcc_csm1_1/2020_2049/by-month/tmax_2020_01.nc")
plot(tmax_fut - tasmax)
hist(tmax_fut - tasmax)

## prec daily future
tmin_fut <- raster("W:/bid-cc-agricultural-sector/01-climate-data/gcm_0_5deg_lat/bcc_csm1_1/2020_2049/by-month/tmin_2020_01.nc")
plot(tmin_fut - tasmin)
hist(tmin_fut - tasmin)

plot(tmax_fut - tmin_fut)
hist(tmax_fut - tmin_fut)

## prec daily future
rsds_fut <- raster("W:/bid-cc-agricultural-sector/01-climate-data/gcm_0_5deg_lat/bcc_csm1_1/2020_2049/by-month/rsds_2020_01.nc")
plot(rsds_fut - rsds)
hist(rsds_fut - rsds)



### Archivos diarios promediados y calc std por mes

## prec daily future
pr_fut_avg <- raster("W:/bid-cc-agricultural-sector/01-climate-data/gcm_0_5deg_lat/bcc_csm1_1/2020_2049/prec_2020_2049_01_avg.nc")
plot(pr_fut_avg - pr_avg)
hist(pr_fut_avg - pr_avg)

## prec daily future
tmax_fut_avg <- raster("W:/bid-cc-agricultural-sector/01-climate-data/gcm_0_5deg_lat/bcc_csm1_1/2020_2049/tmax_2020_2049_01_avg.nc")
plot(tmax_fut_avg - tmax_avg)
hist(tmax_fut_avg - tmax_avg)

## prec daily future
tmin_fut_avg <- raster("W:/bid-cc-agricultural-sector/01-climate-data/gcm_0_5deg_lat/bcc_csm1_1/2020_2049/tmin_2020_2049_01_avg.nc")
plot(tmin_fut_avg - tmin_avg)
hist(tmin_fut_avg - tmin_avg)

## prec daily future
rsds_fut_avg <- raster("W:/bid-cc-agricultural-sector/01-climate-data/gcm_0_5deg_lat/bcc_csm1_1/2020_2049/rsds_2020_2049_01_avg.nc")
plot(rsds_fut_avg - rsds_avg)
hist(rsds_fut_avg - rsds_avg)




###----- BC --------
  
bc_prec <- raster("N:/bid/bc_0_5deg_lat/bcc_csm1_1/1971_2000/by_month/prec_1971_01.nc", band=2)
plot(bc_prec)
hist(bc_prec)
(bc_prec)

gcm.his.day.stack <- raster("N:/bid/gcm_0_5deg_lat/bcc_csm1_1/1971_2000/by-month/tmax_1971_01.nc")
gcm.his.avg <- raster("N:/bid/gcm_0_5deg_lat/bcc_csm1_1/1971_2000/tmax_1971_2000_01_avg.nc")
gcm.his.std <- raster("N:/bid/gcm_0_5deg_lat/bcc_csm1_1/1971_2000/tmax_1971_2000_01_std.nc")

wfd.his.avg <- raster("S:/observed/gridded_products/wfd/nc-files/wfd_0_5_deg_lat/Tmax_daily_WFD/lat_Tmax_daily_WFD_1971_2000_01_avg.nc")
wfd.his.std <- raster("S:/observed/gridded_products/wfd/nc-files/wfd_0_5_deg_lat/Tmax_daily_WFD/lat_Tmax_daily_WFD_1971_2000_01_std.nc")

xmin(wfd.his.avg) <- xmin(wfd.his.avg)-360
xmax(wfd.his.avg) <- xmax(wfd.his.avg)-360

xmin(wfd.his.std) <- xmin(wfd.his.std)-360
xmax(wfd.his.std) <- xmax(wfd.his.std)-360

wfd.his.avg <- wfd.his.avg - 273.15

gcm.his.bc <- wfd.his.avg + ( (wfd.his.std / gcm.his.std) * (gcm.his.day.stack - gcm.his.avg) )

plot(tasmax)
plot(tmax_avg)
plot(wfd.his.avg)
plot(wfd.his.std)
plot(tmax_std)

plot(gcm.his.bc)

tmin_prec <- raster("N:/bid/bc_0_5deg_lat/bcc_csm1_1/1971_2000/by_month/tmax_1971_01.nc", band=1)

hist(tmax_prec)
(bc_prec)

  





###----- BC Future--------

bc_prec <- raster("N:/bid/bc_0_5deg_lat/bcc_csm1_1/1971_2000/by_month/tmax_1971_01.nc", band=1)
bc_prec_fut <- raster("W:/bid-cc-agricultural-sector/01-climate-data/bc_0_5deg_lat/bcc_csm1_1/2020_2049/by_month/tmax_2020_01.nc", band=1)

plot(bc_prec)
plot(bc_prec_fut)
hist(bc_prec - bc_prec_fut)

hist(bc_prec)
(bc_prec)

gcm.his.day.stack <- raster("N:/bid/gcm_0_5deg_lat/bcc_csm1_1/1971_2000/by-month/tmax_1971_01.nc")
gcm.his.avg <- raster("N:/bid/gcm_0_5deg_lat/bcc_csm1_1/1971_2000/tmax_1971_2000_01_avg.nc")
gcm.his.std <- raster("N:/bid/gcm_0_5deg_lat/bcc_csm1_1/1971_2000/tmax_1971_2000_01_std.nc")

wfd.his.avg <- raster("S:/observed/gridded_products/wfd/nc-files/wfd_0_5_deg_lat/Tmax_daily_WFD/lat_Tmax_daily_WFD_1971_2000_01_avg.nc")
wfd.his.std <- raster("S:/observed/gridded_products/wfd/nc-files/wfd_0_5_deg_lat/Tmax_daily_WFD/lat_Tmax_daily_WFD_1971_2000_01_std.nc")

xmin(wfd.his.avg) <- xmin(wfd.his.avg)-360
xmax(wfd.his.avg) <- xmax(wfd.his.avg)-360

xmin(wfd.his.std) <- xmin(wfd.his.std)-360
xmax(wfd.his.std) <- xmax(wfd.his.std)-360

wfd.his.avg <- wfd.his.avg - 273.15

gcm.his.bc <- wfd.his.avg + ( (wfd.his.std / gcm.his.std) * (gcm.his.day.stack - gcm.his.avg) )

plot(tasmax)
plot(tmax_avg)
plot(wfd.his.avg)
plot(wfd.his.std)
plot(tmax_std)

plot(gcm.his.bc)

tmin_prec <- raster("N:/bid/bc_0_5deg_lat/bcc_csm1_1/1971_2000/by_month/tmax_1971_01.nc", band=1)

hist(tmax_prec)
(bc_prec)

