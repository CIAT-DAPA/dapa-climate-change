#load libraries
library('rworldmap')
library('raster')

#process country data first (map to pixels)
test = data(gridCountriesNumeric)
test = gridCountriesNumeric@data
test2 = coordinates(gridCountriesNumeric)
countries_pixel = data.frame(long=test2[,1],lat=test2[,2],code=test$country.asc)
LatAm_countries = c(660,28,32,533,44,52,84,60,68,535,76,92,136,152,170,188,192,531,  #from http://unstats.un.org/unsd/methods/m49/m49alpha.htm
                    212,214,218,222,238,254,308,312,320,328,332,340,388,474,862,484,
                    500,558,591,600,604,630,652,659,662,663,534,670,740,780,796,850,858,840)
ind_LatAm = match(countries_pixel$code,LatAm_countries)
pixels_LatAm = countries_pixel[which(is.na(ind_LatAm)==F),]
pixels_LatAm$country='nada'
for (j in 1:dim(pixels_LatAm)[1])  {
  pixels_LatAm$country[j] = isoToName(pixels_LatAm$code[j],lookup=getMap()@data)
  print(j)
}
pixels_LatAm$country[which(pixels_LatAm$code==312)]='Guadeloupe'  #hard-code these 2 pixels
pixels_LatAm$country[which(pixels_LatAm$code==254)]='French Guiana'

#Modify master coordinate list (from climate data) to add country
load('Z:/14-ObjectsR/id_coordinates.Rdat')
ind_country = match(paste(pixels_LatAm$long,pixels_LatAm$lat),paste(id_pixel$x,id_pixel$y))
bad = which(is.na(ind_country))
pixels_LatAm = pixels_LatAm[-bad,]
ind_country = ind_country[-bad]
id_pixel[ind_country,'country'] = pixels_LatAm$country

#figure out which pixels to run for each crop & flag in id_pixel
crops = c('Maize','Potatoes','Rice','Soybeans','Wheat')
for (j in 1:5)  {  #loop through 5 crops (for now)
  eval(parse(text=paste('load("Z:/08-Cells_toRun/',crops[j],'.loc.cal.Rdat")',sep='')))
  eval(parse(text=paste('crop.cal = ',crops[j],'.loc.cal',sep='')))  #rename to crop.cal
  
  #find cells with no MIRCA or fertilizer data
  cells_bad = which((is.na(crop.cal$mirca.rf.start)&is.na(crop.cal$mirca.irr.start))|is.na(crop.cal$N.app))
  crop.cal = crop.cal[-cells_bad,]
  #cut out cells in US
  ind_cal = match(paste(crop.cal$Longitude,crop.cal$Latitude),paste(id_pixel$x,id_pixel$y))
  crop.cal$country = id_pixel[ind_cal,'country']
  ind_US = which(crop.cal$country=='United States of America')
  crop.cal = crop.cal[-ind_US,]
  #Add column per crop to id_pixel
  ind_crop = match(paste(crop.cal$Longitude,crop.cal$Latitude),paste(id_pixel$x,id_pixel$y))
  eval(parse(text=paste('id_pixel[ind_crop,"',crops[j],'"] = 1',sep='')))
}

#look for coastline cells without country and set USA to NA
crops = apply(id_pixel[,5:9],1,sum,na.rm=T)  #calculate number of crops per pixel
ind_noCountry = which(crops>0 & is.na(id_pixel$country))  #find cells with crops but no country
id_pixel[ind_noCountry,]  #print list

#plot points with missing country
Map_LatinAmerica<-shapefile("Z:/03-Map_LatinAmerica/Latino_America.shp")
par(mar=c(2.1,2.1,2.1,1.1))
plot(Map_LatinAmerica)
points(id_pixel[ind_noCountry,'x'],id_pixel[ind_noCountry,'y'],pch=20,col='red')

#fix manually with Google Earth!
id_pixel[924,'country']='Mexico'
id_pixel[949,'country']='Mexico'
id_pixel[975,'country']='Mexico'
id_pixel[1005,'country']='Cuba'
id_pixel[1038,'country']='Cuba'
id_pixel[1077,'country']='Cuba'
id_pixel[1147,'country']='Cuba'
id_pixel[1298,'country']='Mexico'
id_pixel[1299,'country']='Mexico'
id_pixel[1354,'country']='Haiti'
id_pixel[1402,'country']='Belize'
id_pixel[1482,'country']='Mexico'
id_pixel[1493,'country']='Belize'
id_pixel[1501,'country']='Mexico'
id_pixel[1632,'country']='Nicaragua'
id_pixel[1711,'country']='Costa Rica'
id_pixel[1732,'country']='Venezuela'
id_pixel[1801,'country']='Venezuela'
id_pixel[1832,'country']='Panama'
id_pixel[1922,'country']='Panama'
id_pixel[2184,'country']='French Guiana'
id_pixel[2185,'country']='French Guiana'
id_pixel[2186,'country']='French Guiana'
id_pixel[2187,'country']='French Guiana'
id_pixel[2234,'country']='French Guiana'
id_pixel[2235,'country']='French Guiana'
id_pixel[2236,'country']='French Guiana'
id_pixel[2237,'country']='French Guiana'
id_pixel[2238,'country']='French Guiana'
id_pixel[2286,'country']='French Guiana'
id_pixel[2287,'country']='French Guiana'
id_pixel[2288,'country']='French Guiana'
id_pixel[2289,'country']='French Guiana'
id_pixel[2290,'country']='French Guiana'
id_pixel[2291,'country']='French Guiana'
id_pixel[2341,'country']='French Guiana'
id_pixel[2393,'country']='French Guiana'
id_pixel[2394,'country']='French Guiana'
id_pixel[2395,'country']='French Guiana'
id_pixel[2447,'country']='French Guiana'
id_pixel[2448,'country']='French Guiana'
id_pixel[2750,'country']='Brazil'
id_pixel[3039,'country']='Brazil'
id_pixel[3432,'country']='Brazil'
id_pixel[4436,'country']='Brazil'
id_pixel[4534,'country']='Brazil'
id_pixel[4699,'country']='Brazil'
id_pixel[4936,'country']='Brazil'
id_pixel[5253,'country']='Peru'
id_pixel[5783,'country']='Brazil'
id_pixel[5788,'country']='Bolivia'
id_pixel[5845,'country']='Brazil'
id_pixel[5912,'country']='Bolivia'
id_pixel[5913,'country']='Bolivia'
id_pixel[5914,'country']='Bolivia'
id_pixel[5968,'country']='Brazil'
id_pixel[6205,'country']='Brazil'
id_pixel[6370,'country']='Brazil'
id_pixel[6467,'country']='Brazil'
id_pixel[6833,'country']='Brazil'
id_pixel[6878,'country']='Brazil'
id_pixel[6963,'country']='Brazil'
id_pixel[6964,'country']='Brazil'
id_pixel[6965,'country']='Brazil'
id_pixel[6984,'country']='Argentina'
id_pixel[7050,'country']='Brazil'
id_pixel[7092,'country']='Brazil'
id_pixel[7133,'country']='Brazil'
id_pixel[7171,'country']='Uruguay'
id_pixel[7287,'country']='Uruguay'
id_pixel[7321,'country']='Uruguay'
id_pixel[7556,'country']='Argentina'
id_pixel[7584,'country']='Argentina'
id_pixel[7585,'country']='Argentina'
id_pixel[7610,'country']='Argentina'
id_pixel[7708,'country']='Chile'
id_pixel[8107,'country']='Argentina'
id_pixel[8120,'country']='Chile'
id_pixel[8121,'country']='Chile'

#set USA back to NA
ind_US = which(id_pixel$country=='United States of America')
id_pixel[ind_US,'country'] = NA
#set crops to NA for 2 points on US coastline manually
US_coast = c(483,588)
id_pixel[US_coast,"Maize"] = NA
id_pixel[US_coast,"Potatoes"] = NA
id_pixel[US_coast,"Rice"] = NA
id_pixel[US_coast,"Soybeans"] = NA
id_pixel[US_coast,"Wheat"] = NA

#save results to file
save(id_pixel,file='Z:/14-ObjectsR/id_coordinates.country.crops.Rdat')

#plot crop maps
ind_maize = which(id_pixel$Maize==1)
ind_potatoes = which(id_pixel$Potatoes==1)
ind_rice = which(id_pixel$Rice==1)
ind_soybeans = which(id_pixel$Soybeans==1)
ind_wheat = which(id_pixel$Wheat==1)

plot(Map_LatinAmerica)
points(id_pixel[ind_wheat,'x'],id_pixel[ind_wheat,'y'],pch=20,col='orange')
title('Wheat')
