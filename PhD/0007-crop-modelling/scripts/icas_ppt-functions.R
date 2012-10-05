#Julian Ramirez-Villegas
#University of Leeds / CCAFS / CIAT
#October 2012

#make a map wrapping spplot
make_spplot <- function(prs,pcols,pbrks,ht=1000,tiffName="test.tif",add_items=NULL) {
  #required package
  require(maptools); data(wrld_simpl)
  require(raster); require(sp)
  
  #some plot details
  wld <- list("sp.polygons",wrld_simpl,lwd=0.8,first=F)
  grat <- gridlines(wrld_simpl, easts=seq(-180,180,by=5), norths=seq(-90,90,by=5))
  grli <- list("sp.lines",grat,lwd=0.5,lty=2,first=F)
  
  #create layout
  if (is.null(add_items)) {
    layt <- list(wld,grli)
  } else {
    layt <- list(add_items,wld,grli)
  }
  
  
  #plot summaries. Make a nice map out from all this
  brks.lab <- round(pbrks,1)
  
  #dimensions of plot
  #ht <- 1000
  fct <- (prs@extent@xmin-prs@extent@xmax)/(prs@extent@ymin-prs@extent@ymax)
  wt <- ht*(fct+.1)
  
  this_plot <- spplot(prs,sp.layout=layt,col.regions=pcols,
                      par.settings=list(fontsize=list(text=12)),
                      at=pbrks,pretty=brks)
  
  #final plotting
  tiff(tiffName,res=300,compression="lzw",height=ht,width=wt)
  print(this_plot)
  dev.off()
}

