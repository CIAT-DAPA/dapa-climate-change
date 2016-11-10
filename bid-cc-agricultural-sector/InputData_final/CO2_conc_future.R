#Add RCP4.5 future CO2 concentrations to Mauna Loa curve in DSSAT

co2.d = read.table('C:/DSSAT46/StandardData/CO2046_fut.txt',header=T,sep='\t')  #DSSAT data

#just interpolating forward
hist=1:dim(co2.d)[1]
fut = seq(697,(696+415),1)
mod = lm(co2.d[,3]~hist+eval(hist^2),x=T)
co2.fut = predict(mod,data.frame(hist=fut,hist.2=fut^2))
plot(c(co2.d[,3],co2.fut))
points(co2.d[,3],pch=20,col='red')

#Using annual values from RCP4.5 to fill future
rcp45 = read.table('D:/Tobackup/BID/RCP45_MIDYEAR_CONCENTRATIONS2.txt',header=T)
#select 1971 to 2049, repeat monthly
ind.yr = which(rcp45$YEARS>=1971 & rcp45$YEARS<=2049)
co2_rcp45 = rcp45[ind.yr,'CO2']
co2.rep = sort(rep(co2_rcp45,12))

#plot 2 time series
ind.hist = which(co2.d$YEAR>=1971 & co2.d$YEAR<=2049)
plot(co2.rep)
points(co2.d[ind.hist,'CO2'],pch=20,col='red')
lines(c(co2.d[ind.hist,3],co2.fut),lty=2)  #other interpolation for comparison

#join past & future
ind.yr = which(rcp45$YEARS>=2015 & rcp45$YEARS<=2049)
co2_rcp45 = rcp45[ind.yr,'CO2']  #redo from 2015
co2.rep = sort(rep(co2_rcp45,12))
co2.fut = co2.rep[6:length(co2.rep)]  #cut out first 5 months of 2015
YEAR = c(rep(2015,7),sort(rep(seq(2016,2049),12))) #7 months in 2015 + 2016 to 2049
DOY= c(co2.d$DOY[6:12],rep(co2.d$DOY[1:12],34))  #repeat DOY for same period
test = data.frame(YEAR=YEAR,DOY=DOY,CO2=co2.fut)
co2.new = rbind(co2.d[1:689,],test)  #mauna loa to May 2015, then RCP4.5 after
plot(co2.new$CO2)

#save co2.new and then add header information for DSSAT manually
library(gdata)
write.fwf(co2.new,file='C:/DSSAT46/StandardData/CO2046_fut2.txt',quote=F,sep='  ',na='-99.99')
#need to figure out how to make this fixed width...