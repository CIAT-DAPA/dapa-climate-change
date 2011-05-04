# Plot with grid

# show vieport
pdf("test_layout.pdf", width=8.3,height=11.7)
grid.show.layout(grid.layout(6,4,widths=unit(c(20,60,120,10),"mm"),heights=unit(c(17,50,50,80,80,20),"mm")))
dev.off()

# create report
#----------------------------------------------------------------------------------------------#
# helper
#----------------------------------------------------------------------------------------------#

mtbg  <- function() grid.rect(gp=gpar(fill="grey85", col="white"))
mdl   <- function() grid.lines(c(0,1), c(0,0), gp=gpar(col="grey85"))

# current vs futur

lp <- function(params, var){
  require(akima)
  
  # points for interpolation
  ipoints <- with(params,seq(12/params$ndivisions,12,length.out=ndivisions))
  
  # extract futur climate
  res <- data.frame(x=rep(NA,200))
  
  for (i in params$gcms) {
    ll <- aspline(y=params$ref_values[[str_c(i,"_",var)]],x=ipoints,n=200)
   
    res[,1] <- ll$x
    res[,i] <- ll$y
  }
  
  l <- ncol(res)
  
  # get min, min and mean for each month
  res$min   <- apply(res[,2:l],1,min)
  res$max   <- apply(res[,2:l],1,max)
  res$mean  <- apply(res[,2:l],1,mean)
  
  cur <- aspline(y=params$ref_values[[str_c('current_',var)]],x=ipoints,n=200)
    
  ymin <- min(c(res$min,cur$y))
  ymax <- max(c(res$max,cur$y))
  
  # create plot region
    pushViewport(plotViewport(margins=c(2.5,2.5,0.9,0.5)))
    pushViewport(dataViewport(res[,1],c(ymin,ymax)))
  
    # plot x and y axis
    grid.yaxis(gp=gpar(cex=0.8))
    grid.xaxis(gp=gpar(cex=0.8), at=1:12, label=c("J","F","M","A","M","J","J","A","S","O","N","D"))
 
    # plot variation of models
    grid.polygon(x=c(res[,1],rev(res[,1])), y=c(res$min, rev(res$max)), default.units="native", gp=gpar(fill="grey90",col="grey90"))
    
    # plot mean
    grid.lines(x=res[,1],y=res$mean, default.units="native", gp=gpar(lty=2))
    
    # plot points
    for (x in 1:12) grid.points(x,res$mean[which(floor(res[,1])==x)][1], gp=gpar(cex=0.6), pch=18)
    
    # plot current
    grid.lines(x=cur$x, y=cur$y, default.units="native")
    
    # plot points
    for (x in 1:12) grid.points(x,cur$y[which(floor(cur$x)==x)][1], gp=gpar(cex=0.6), pch=18)


    # plot growing season
    if (with(params, growing.season[1] < growing.season[length(growing.season)])) {
      with(params,grid.lines(x=c(growing.season[1],growing.season[length(growing.season)]), y=c(ymin,ymin), default.units="native", gp=gpar(lwd=2, col="darkgreen")))
    } else {
      with(params,grid.lines(x=c(growing.season[1],12), y=c(ymin,ymin), default.units="native", gp=gpar(lwd=2, col="darkgreen")))
      with(params,grid.lines(x=c(1,growing.season[length(growing.season)]), y=c(ymin,ymin), default.units="native", gp=gpar(lwd=2, col="darkgreen")))
    }
    
  upViewport(3)
    
}

# plot grids

nm <- function(params,r){
  
  pushViewport(plotViewport(margins=c(2.5,2.5,0.9,0.5)))
  pushViewport(dataViewport(c(xmin(r),xmax(r)), c(ymin(r),ymax(r))))
  
  grid.xaxis(at=seq(xmin(r),xmax(r),length.out=7), gp=gpar(cex=0.8))
  grid.yaxis(at=seq(ymin(r),ymax(r),length.out=7),gp=gpar(cex=0.8))
  
  # make color range
  r.v   <- getValues(r)
  r.vu  <- unique(r)
  r.vu  <- r.vu[order(r.vu)]
  cols  <- data.frame(or.values=r.vu,co=rainbow(length(r.vu)))
  
  cols <- with(cols, co[match(r.v, or.values)])
  
  gGrad <- matrix(cols,nrow=r@nrows,byrow=T)
  
  grid.raster(gGrad,interpolate=F,width=unit(r@ncols,"native"), height=unit(r@nrows,"native"))
  
  m <- map("world", plot=F, xlim=c(xmin(r),xmax(r)), ylim=c(ymin(r),ymax(r)))
  grid.lines(x=m$x,y=m$y, default.units="native")
  
  # points
  grid.points(x=params$x, y=params$y, gp=gpar(col="red"), pch=18)
  grid.points(x=params$x, y=params$y, gp=gpar(cex=0.5), pch=20)
  
  upViewport(3)
}

textbox <- function(title, text){
  pushViewport(viewport(width=0.9, height=0.9))
  grid.rect(gp=gpar(col="white"))
  
  grid.text(title, x=0.5, y=0.9, gp=gpar(cex=1.2), just="center")
  grid.lines(x=c(0.1,0.9), y=c(0.8,0.8), gp=gpar(col="white"))
  
  grid.text(text,just=c("left","top"), x=0.1,y=0.7, gp=gpar(cex=.8))
  
  upViewport()
}

#----------------------------------------------------------------------------------------------#
# Create Report
#----------------------------------------------------------------------------------------------#
ccafs.report <- function(params=ccafs.params, r1=mean,r2=cv) {
  if (params$paper=="a4") {
    width   <- 8.3
    height  <- 11.7
  }# else if (params$paper=="letter") {
  #  width   <- 8.5
  #  height  <- 11
  #}
  
  pdf("test_pdf.pdf",width=width,height=height)
    pushViewport(viewport(layout=grid.layout(6,4,widths=unit(c(20,60,120,10),"mm"),heights=unit(c(17,50,50,80,80,20),"mm"))))
  
    # plot header
    pushViewport(viewport(layout.pos.col=3, layout.pos.row=1, height=0.9))
      grid.text(paste("Date:", date(), "\n for Colombia"), just="right", x=0.9, 
        gp=gpar(cex=0.7))
    upViewport()
  
    # plot tmp_text
    pushViewport(viewport(layout.pos.col=2, layout.pos.row=2))
    mtbg()
    textbox("Mean Temperature", str_c("current mean: ", round(mean(params$ref_values$current_tmean),2), 
    "\ncurrent sd: ", round(sd(params$ref_values$current_tmean),2)))
    upViewport()
    
    # plot tmp_plot
    pushViewport(viewport(layout.pos.col=3, layout.pos.row=2))
    mdl()
    lp(params, "tmean")
  
     # plot prec_text
    pushViewport(viewport(layout.pos.col=2, layout.pos.row=3))
    mtbg()  
    textbox("Mean Rain", str_c("current mean: ", round(mean(params$ref_values$current_prec),2), 
      "\ncurrent sd: ", round(sd(params$ref_values$current_prec),2)))
    upViewport()
    
    # plot prec_plott
    pushViewport(viewport(layout.pos.col=3, layout.pos.row=3))
    mdl()
    lp(params, "prec")
    
    # plot ccafs_text
    pushViewport(viewport(layout.pos.col=2, layout.pos.row=4))
    mtbg()
    textbox("CCAFS 1", "text here")
    upViewport()
    
    # plot raster1
    pushViewport(viewport(layout.pos.col=3, layout.pos.row=4))
    mdl()
    nm(params,r)
    
    # plot temp_text
    pushViewport(viewport(layout.pos.col=2, layout.pos.row=5))
    mtbg()
    textbox("CCAFS 2", "text here")
    upViewport()
    
    # plot raster2
    pushViewport(viewport(layout.pos.col=3, layout.pos.row=5))
    mdl()
  
    nm(params,r)
    
    # small print
    pushViewport(viewport(layout.pos.col=3, layout.pos.row=6, width=0.9, height=0.9))
      grid.text("Parameters: model: xys;lon: 34; lat: 59.6; year: 2030; sres: A1B; gcms: many gcms", 
        gp=gpar(cex=0.7), x=1, just="right")
    upViewport()
  dev.off()
}
