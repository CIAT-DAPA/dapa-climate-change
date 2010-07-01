require(maptools)
require(raster)

#xmin,xmax,ymin,ymax

xn <- -30
xx <- 30
yn <- -40
yx <- 40

wd <- 20

xt <- extent(c(xn,xx,yn,yx))
rs <- raster(nrow=180,ncol=360)
rs[] <- 1:ncell(rs)

rs <- crop(rs, xt)

rel <- ncol(rs)/nrow(rs)
ht <- wd/rel

sh <- readShapePoly("world_adm0.shp")

np <- 1
ht <- ht/np

pdf("test.pdf", height=ht, width=wd)
par(mfrow=(c(1,np)))
plot(rs)
plot(sh, add=T)
dev.off()

