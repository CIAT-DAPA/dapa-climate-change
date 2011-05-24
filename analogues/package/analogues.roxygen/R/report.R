#' Create a report
#'
#' @param params an object of the class AnalogueParameters
#' @param r a \code{list} of rasters
#' @param r_lab \code{vector} of labels corresponding to \code{r}
#' @param pdf_name path and name to output pdf
#' @return prints a report and save it as pdf
#' @export
#' @examples
#' report(ccafs_params, list(results[[1]]), "Test", "test.pdf")


#----------------------------------------------------------------------------------------------#
# Create Report
#----------------------------------------------------------------------------------------------#
report <- function(params=ccafs_params, r=list(), r_lab="", pdf_name="test_pdf") {

    width   <- 8.3
    height  <- 11.7
  
  pdf(pdf_name,width=width,height=height)
  
    # create Title
    grid.newpage()
    pushViewport(viewport(layout=grid.layout(3,1,widths=unit(210,"mm"),heights=unit(c(87,130,80),"mm"))))
      
      pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
        grid.text("Analogues Results",x=0.5,y=0.3, gp=gpar(cex=4, fontfamily="serif"))
      upViewport()
      
      m <- map("world", plot=F)
      
      pushViewport(viewport(layout.pos.col=1, layout.pos.row=2))
      pushViewport(plotViewport(margins=c(2.5,2.5,0.9,0.5)))
      pushViewport(dataViewport(m$x,m$y))
        grid.lines(x=m$x,y=m$y, default.units="native")
        grid.points(params$x, params$y, pch=20, gp=gpar(col="red", cex=1))
      upViewport()
      upViewport()
      upViewport()
      
      # get logo
      ccafs_logo <- get(".ccafs_logo", env=globalenv())
      ciat_logo <- get(".ciat_logo", env=globalenv())
      
      pushViewport(viewport(layout.pos.col=1, layout.pos.row=3))
        grid.raster(ccafs_logo, x=0.7,y=0.5,width=0.3,height=0.3)
        grid.raster(ciat_logo, x=0.3,y=0.5, width=.3,height=0.3)
        grid.text("Provided by",x=0.5,y=0.9, gp=gpar(cex=1, fontfamily="serif"))
      upViewport()
    
    upViewport()
   
    # figure out how many pages are needed for variables and for rasters
    
    # no pages
    npvars <- ceiling(length(params$vars)/5)
    nprast <- ceiling(length(r)/3)
    
    #which page is one var plotted on
    wpvars <- rep(1:npvars,each=5)[1:length(params$var)]
    wpvars <- data.frame(var=1:length(params$var),which_page=wpvars)
    
    wprast <- rep(1:nprast,each=3)[1:length(r)]
    wprast <- data.frame(rast=1:length(r),which_page=wprast)
    
    # count pages
    cpage <- 2
    tpage <- npvars + nprast + 1
    
    for (page in 1:npvars) {
      grid.newpage()
      pushViewport(viewport(layout=grid.layout(8,4,widths=unit(c(20,60,120,10),"mm"),heights=unit(c(17,50,50,50,50,50,15,15),"mm"))))
  
      # plot header
      header()
  
      ## plot variables
      rowpos <- 2
      
      for (var in wpvars[wpvars$which_page==page,'var']) {
        # plot tmp_text
        pushViewport(viewport(layout.pos.col=2, layout.pos.row=rowpos))
        mtbg()
        textbox(str_c("Summary for ",params$vars[var]), 
        str_c("current mean: ", round(mean(params$ref_train[[which(params$idx_vars==var & params$idx_gcms==1)]]),2), 
        "\ncurrent sd: ", round(sd(params$ref_train[[which(params$idx_vars==var & params$idx_gcms==1)]]),2),
        "\n",
        "\nfuture mean: ", round(mean(sapply(2:length(params$gcms), 
          function(x) mean(params$ref_train[[which(params$idx_vars==var & params$idx_gcms==x)]]))),2),
        "\nfuture sd: ", round(mean(sapply(2:length(params$gcms), 
          function(x) sd(params$ref_train[[which(params$idx_vars==var & params$idx_gcms==x)]]))),2),
        "\nfuture sd among gcms: ", round(sd(sapply(2:length(params$gcms), 
          function(x) mean(params$ref_train[[which(params$idx_vars==var & params$idx_gcms==x)]]))),2)
        ))
        upViewport()
            
        # plot tmp_plot
        pushViewport(viewport(layout.pos.col=3, layout.pos.row=rowpos))
        mdl()
      
        cfplot(params, var)
        
        rowpos <- rowpos + 1
      }
      glegend(7)
      footer(8,cpage,tpage,params)
      cpage <- cpage + 1
    }  
    
    if (length(r) > 0) {
      for (page in 1:nprast) {
        grid.newpage()
        pushViewport(viewport(layout=grid.layout(6,4,widths=unit(c(20,60,120,10),"mm"),heights=unit(c(17,80,80,80,20),"mm"))))
    
        # plot header
        header()
   
        rowpos <- 2
   
        for (rast in wprast[wprast$which_page==page,'rast']) {
          # plot ccafs_text
  
          rast_plot(r[[rast]],params, r_lab[rast], rowpos)
          
          rowpos <- rowpos + 1
        }
        footer(6,cpage,tpage,params)
        cpage <- cpage + 1
      }
    }
  dev.off()
}


## legend
glegend <- function(row) {
  pushViewport(viewport(layout.pos.col=2, layout.pos.row=row))
        mtbg()
        grid.text("Legend",x=0.22, gp=gpar(cex=0.95),just="left")
  upViewport()
  
  pushViewport(viewport(layout.pos.col=3, layout.pos.row=row, height=0.9))
  mdl()
    # current
    grid.lines(x=c(0.05,0.15),y=c(0.5,0.5), default.units="native", gp=gpar(lty=1))
    grid.points(x=0.1,y=0.5,gp=gpar(cex=0.6),pch=18)
    grid.text("current",x=0.17, gp=gpar(cex=0.8),just="left")
    
    # future
    grid.lines(x=c(0.35,0.45),y=c(0.5,0.5), default.units="native", gp=gpar(lty=2))
    grid.points(x=0.4,y=0.5, gp=gpar(cex=0.6), pch=20)
    grid.text("future",x=0.47, gp=gpar(cex=0.8),just="left")
    
    # growing season
    grid.lines(x=c(0.6,0.7),y=c(0.5,0.5), default.units="native", gp=gpar(lwd=2,col="darkgreen"))
    grid.text("growing season",x=0.72, gp=gpar(cex=0.8),just="left")
  upViewport()
}

## header
header <- function() {
  pushViewport(viewport(layout.pos.col=3, layout.pos.row=1, height=0.9))
    grid.text(paste("Date:", date()), just="right", x=0.9, 
    gp=gpar(cex=0.7))
  upViewport()
}

## footer
footer <- function(row,current,of,params) {
  pushViewport(viewport(layout.pos.col=3, layout.pos.row=row, width=0.9, height=0.9))
      grid.text(str_c("Year: ", params$year, ", SRES ",params$scenario, ", GCMS", str_c(params$gcms, collapse=", "),
      "\n Page ", current, " of ", of), gp=gpar(cex=0.7), x=1, just="right")
  upViewport()
}
## make grey background
mtbg  <- function() grid.rect(gp=gpar(fill="grey85", col="white"))

## make lines
mdl   <- function() grid.lines(c(0,1), c(0,0), gp=gpar(col="grey85"))

## current vs future plot
cfplot <- function(params, var){
  require(akima)    
    # points for interpolation
    ipoints <- with(params,seq(12/params$ndivisions,12,length.out=ndivisions))
  
    # extract futur climate
    res <- data.frame(x=rep(NA,200))
  
    for (i in 2:length(params$gcms)) {
      ll <- aspline(y=params$ref_train[[which(params$idx_vars==var & params$idx_gcms==i)]],x=ipoints,n=200)
   
      res[,1] <- ll$x
      res[,i] <- ll$y
    }
  
    l <- ncol(res)
  
    # get min, min and mean for each month
    if ( l > 2) {
      res$min   <- apply(res[,2:l],1,min)
      res$max   <- apply(res[,2:l],1,max)
      res$mean  <- apply(res[,2:l],1,mean)
    } else {
      res$min   <- res[,2]
      res$max   <- res[,2]
      res$mean  <- res[,2]
    }
  
    cur <- aspline(y=params$ref_train[[which(params$idx_vars==var & params$idx_gcms==1)]],x=ipoints,n=200)
    
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
    for (x in 1:12) grid.points(x,res$mean[which(floor(res[,1])==x)][1], gp=gpar(cex=0.6), pch=20)
    
    # plot current
    grid.lines(x=cur$x, y=cur$y, default.units="native")
    
    # plot points
    for (x in 1:12) grid.points(x,cur$y[which(floor(cur$x)==x)][1], gp=gpar(cex=0.6), pch=18)


    # plot growing season
    if (with(params, growing_season[1] < growing_season[length(growing_season)])) {
      with(params,grid.lines(x=c(growing_season[1],growing_season[length(growing_season)]), y=c(ymin,ymin), default.units="native", gp=gpar(lwd=2, col="darkgreen")))
    } else {
      with(params,grid.lines(x=c(growing_season[1],12), y=c(ymin,ymin), default.units="native", gp=gpar(lwd=2, col="darkgreen")))
      with(params,grid.lines(x=c(1,growing_season[length(growing_season)]), y=c(ymin,ymin), default.units="native", gp=gpar(lwd=2, col="darkgreen")))
    }
    
  upViewport(3)   
}

## plot a raster

rast_plot <- function(object, ...) {
  UseMethod("rast_plot",)
}

rast_plot.RasterLayer <- function(r,params, r_lab, rowpos){
  
  pushViewport(viewport(layout.pos.col=2, layout.pos.row=rowpos))
    mtbg()
    textbox(r_lab, str_c("xmin: ", xmin(r), 
      "\nxmax: ", xmax(r),
      "\nymin: ", ymin(r),
      "\nymax: ", ymax(r),
      "\nresolution: ", res(r)))
  upViewport()
        
  # plot raster1
  pushViewport(viewport(layout.pos.col=3, layout.pos.row=rowpos))
    mdl()
  
    pushViewport(plotViewport(margins=c(2.5,2.5,0.9,0.5)))
  
      # make sure the viewport has proportion of 2:1
      xmin <- xmin(r)
      xmax <- xmax(r)
      ymin <- ymin(r)
      ymax <- ymax(r)
    
      xrange <- xmax - xmin
      mxrange <- xrange / 2 # middle of old range
      yrange <- ymax - ymin
    
      if (xrange / yrange != 2) {
        xrange <- yrange * 2
      
        # make new xrange
        xmin <- mxrange - (xrange / 2)
        xmax <- mxrange + (xrange / 2)
      }
  
      pushViewport(dataViewport(c(xmin,xmax), c(ymin,ymax)))
  
        # axis
        grid.xaxis(at=round(seq(xmin,xmax,length.out=7),0), gp=gpar(cex=0.8))
        grid.yaxis(at=round(seq(ymin,ymax,length.out=5),0),gp=gpar(cex=0.8))
        
        # make color range
        r.v   <- getValues(r)
        r.vu  <- unique(r.v)
        r.vu  <- r.vu[order(r.vu)]
        cols  <- data.frame(or.values=r.vu,co=rainbow(length(r.vu)))
        cols[(ncol(cols)+1),] <- c(NA,NA)
      
        cols <- with(cols, co[match(r.v, or.values)])
      
        gGrad <- matrix(cols,nrow=r@nrows,byrow=T)
      
        grid.raster(gGrad,x=unit((xmin+(xmax-xmin)/2), "native"),y=unit((ymin+(ymax-ymin)/2),"native"),interpolate=F)
      
        # some work needed here
        m <- map("world", plot=F, xlim=c(xmin,xmax), ylim=c(ymin,ymax))
        grid.lines(x=m$x,y=m$y, default.units="native")
        
        # points
        grid.points(x=params$x, y=params$y, gp=gpar(col="red"), pch=18)
        grid.points(x=params$x, y=params$y, gp=gpar(cex=0.5), pch=20)
      
    upViewport(3)
}

## create a textbox
textbox <- function(title, text){
  pushViewport(viewport(width=0.9, height=0.9))
  grid.rect(gp=gpar(col="white"))
  
  grid.text(title, x=0.5, y=0.9, gp=gpar(cex=1.2), just="center")
  grid.lines(x=c(0.1,0.9), y=c(0.8,0.8), gp=gpar(col="white"))
  
  grid.text(text,just=c("left","top"), x=0.1,y=0.7, gp=gpar(cex=.8))
  
  upViewport()
}