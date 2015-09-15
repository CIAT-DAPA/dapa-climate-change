#Julian Ramirez-Villegas
#University of Leeds / CCAFS / CIAT
#August 2015
stop("!")

#Fig 1. study area, weather station, sub-region and soil profiles

library(sp); library(maptools); library(raster); library(rgeos)

#directories
wd <- "/nfs/a101/earjr/rice-future-tpe"
obs_dir <- paste(wd,"/obs_meteorology",sep="")
gcm_dir <- paste(wd,"/gcm_meteorology",sep="")
fig_odir <- paste(wd,"/figures",sep="")
if (!file.exists(fig_odir)) {dir.create(fig_odir)}

source(paste(wd,"/scripts/thiessen_polygons.R",sep=""))

#location list
loc_list <- read.csv(paste(obs_dir,"/all_wst_locs.csv",sep=""))

#load Brazil shapefile
bra_shp <- readRDS(paste(wd,"/data/BRA_adm1.rds",sep=""))
bra_shp <- bra_shp[which(bra_shp$ID_1 == 9 | bra_shp$ID_1 == 12 | bra_shp$ID_1 == 22 | bra_shp$ID_1 == 27),]

#construct list of final weather stations
loc_list$allyears <- T
for (wst in loc_list$id) {
  #wst <- paste(loc_list$id[1])
  wst_name <- gsub(".","",wst,fixed=T)
  wst_odir <- paste(gcm_dir,"/loc_",wst_name,"/obs",sep="")
  if (!file.exists(wst_odir)) {loc_list$allyears[which(loc_list$id == wst)] <- F}
}
loc_list <- loc_list[which(loc_list$allyears),]

#number of stations per state from paper: 3 TO; 7 RO; 16 MT; 25 GO
#remove CALDAS NOVAS (.IPGO.00007), as it was not used in Heinemann et al. (2015) JXB
loc_list <- loc_list[which(loc_list$id != ".IPGO.00007"),]
row.names(loc_list) <- 1:nrow(loc_list)

#load soil data
soil_data <- read.csv(paste(wd,"/data/BrazilSoilDB_08VI05_NAfilled.csv",sep=""))
soil_data <- soil_data[,c("OrgProfID","Lat","Long","SoilDepth","HzSimb","HzDeIn","HzDeFn","HzVal","Silt","Clay","Sand")]
soil_pts <- unique(soil_data[,c("Long","Lat")])
names(soil_pts) <- c("x","y")

#convert to sp object
soil_pts <- SpatialPoints(soil_pts)
proj4string(soil_pts)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#select points which are inside any of the polygons
pts_in <- soil_pts[bra_shp] #over(soil_pts, bra_shp)

#create thiessen polygons from weather stations
wst_xy <- loc_list[,c("lon","lat")]; names(wst_xy) <- c("x","y")
thiepol <- voronoipolygons(wst_xy)
proj4string(thiepol)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#intersect thiessen polygons with state polygons
inters <- gIntersection(thiepol, bra_shp, byid=T)

#produce plot
library(ggplot2); library(gridExtra); library(grid)

#weather station locations
wst_loc <- loc_list; wst_loc$label <- "Weather station"
soil_loc <- as.data.frame(pts_in@coords); soil_loc$label <- "Soil profile"

#TPE map
viz <- ggplot() + geom_polygon(data=inters, aes(x=long, y=lat, group=group), colour='blue', fill=NA)
viz <- viz + labs(x="", y="", title="")
viz <- viz + geom_point(data=soil_loc, aes(x=x, y=y), alpha=0.75, colour="grey 50") #color=label (inside aes)
viz <- viz + geom_polygon(data=bra_shp, aes(x=long, y=lat, group=group), fill=NA, colour="black",size=1.1)
viz <- viz + geom_point(data=wst_loc, aes(x=lon, y=lat), size=3.5, colour="red") #color=label (inside aes)
#viz <- viz + scale_colour_manual(name='', values=c('Soil profile'='grey 50','Weather station'='red'))
#viz <- viz + scale_linetype_manual(name='', values=c('Sub-region'='blue','State'='black'))
viz <- viz + scale_x_continuous(breaks=seq(-90,40, 5), labels=c(paste(seq(-90,40, 5),"E", sep="")))
viz <- viz + scale_y_continuous(breaks=seq(-60,20, 5), labels=c(paste(seq(-60,20, 5),"N", sep="")))
viz <- viz + theme(panel.background=element_rect(fill="white",colour="black",size=0.8),
                   legend.key=element_rect(fill='white',size=1.5),legend.title=element_blank(),
                   axis.text.y=element_text(angle = 90, hjust=0.5),
                   axis.line=element_line(colour="grey 50"))
viz <- viz + coord_equal(ratio=1)
#print(viz)

#inset map of South America
pol <- data.frame(xmin=-85,xmax=-30 ,ymin=-60 ,ymax=13.8)
data(wrld_simpl)
coun_sam <- wrld_simpl[which(wrld_simpl$ISO3 %in% c("BRA","COL","ARG","PRY","ECU","PER","VEN","BOL","CHL","URY","GUY","SUR","GUF","PAN","CRI","NIC")),]
coun_sam <- crop(coun_sam, extent(as.numeric(pol)))

p2 <- ggplot() + geom_polygon(data=coun_sam, aes(long,lat,group=group),colour="black",fill="grey 80")
p2 <- p2 + geom_polygon(data=bra_shp, aes(x=long, y=lat, group=group), fill=NA, colour="red",size=0.75)
p2 <- p2 + coord_equal()+theme_bw()+labs(x=NULL,y=NULL)
p2 <- p2 + scale_x_continuous(breaks=seq(-90,40, 10), labels=c(paste(seq(-90,40, 10),"E", sep="")))
p2 <- p2 + scale_y_continuous(breaks=seq(-60,20, 10), labels=c(paste(seq(-60,20, 10),"N", sep="")))
p2 <- p2 + geom_rect(data = pol, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha=0, colour="black", size = 1, linetype=1)
p2 <- p2 + theme(axis.text.x =element_blank(),axis.text.y= element_blank(), axis.ticks=element_blank(),axis.title.x =element_blank(),axis.title.y= element_blank())
#print(p2)

#print file
pdf(file=paste(fig_odir,"/fig_1.pdf",sep=""),height=7,width=10)
grid.newpage()
v1 <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2 <- viewport(width = 0.37, height = 0.37, x = 0.19, y = 0.265) #plot area for the inset map
print(viz, vp=v1) 
print(p2, vp=v2)
dev.off()

#convert to PNG
setwd(fig_odir)
system(paste("convert -verbose -density 300 fig_1.pdf -quality 100 -sharpen 0x1.0 -alpha off fig_1.png"))
setwd("~")
