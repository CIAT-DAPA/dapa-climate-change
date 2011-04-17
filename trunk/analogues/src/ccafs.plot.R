ccafs.plot <- function(params,r){
  pdf("test_rep.pdf", paper="a4")
  layout(matrix(c(1,2,3,4,4),5,1,byrow=T),height=c(1,1.5,1.5,3),TRUE)
  
  par(mar=c(0,0,0,0))
  plot(0,0,type="n", yaxt="n", xaxt="n", ylab="", xlab="", main="", frame=F, xlim=c(0,100), ylim=c(0,50))
  text(3,45,"Results", cex=2.2,pos=4)
  
  text(3,30,str_c("Lon: ",params$x),pos=4)
  text(3,23,str_c("Lat: ",params$y),pos=4)
  text(3,16,str_c("Mean Temp: ",mean(params$ref_values$current_tmean)),pos=4)
  text(3,9,str_c("Mean Rain: ", mean(params$ref_values$current_prec)),pos=4)
  text(3,2,"Elevation: unkown :)",pos=4)
  
  text(56,30,str_c("SRES: ",toupper(params$scenario)),pos=4)
  text(56,23,str_c("Year: ",params$year),pos=4)
  text(56,16,str_c("#GCM: ",length(params$gcms)),pos=4)
  text(56,9,str_c("Analogue: ",params$method),pos=4)
  
  par(mar=c(2,4,2,1))
  uplot(params,"tmean")
  
  par(mar=c(2,4,2,1))
  uplot(params,"prec")
  
  par(mar=c(3,3,4,1))
  plot(0, type="n", ylim=c(-90,90), xlim=c(-180,180), asp=1,xaxt="n", yaxt="n")
  axis(1, ticks=seq(-180,180,10), labels=seq(-180,180,30), at=seq(-180,180,30))
  axis(2, ticks=seq(-90,90,30), labels=seq(-90,90,30), at=seq(-90,90,30),las=T)
  map("world", add=T, fill=T, col="grey91")
  abline(h=0,lty=2,col="grey84")
  points(params$x,params$y,col="red")
  plot(r,add=T)
  dev.off()
}

uplot <- function(params, var) {

  res <- data.frame(x=rep(NA,200))
  
  for (i in params$gcms) {
   ll <- aspline(y=params$ref_values[[str_c(i,"_",var)]],x=1:12,n=200)
   
   res[,1] <- ll$x
   res[,i] <- ll$y
  }
  l <- ncol(res)
  res$min <- apply(res[,2:l],1,min)
  res$max <- apply(res[,2:l],1,max)
  res$mean <- apply(res[,2:l],1,mean)
  
  cur <- aspline(y=params$ref_values[[str_c('current_',var)]],x=1:12,n=200)
  
  ymin <- min(c(res$min,cur$y))
  
  with(res, plot(x,mean,type="l",ylab=var,xaxt="n",las=T,ylim=c(ymin,max(c(res$max,cur$y)))))
  
  # plot growing season
  if (with(params, growing.season[1] < growing.season[length(growing.season)])) {
    with(params,lines(c(growing.season[1],growing.season[length(growing.season)]), c(ymin,ymin), lwd=2, col="darkgreen"))
  } else {
    with(params,lines(c(growing.season[1],12), c(ymin,ymin), lwd=2, col="darkgreen"))
    with(params,lines(c(1,growing.season[length(growing.season)]), c(ymin,ymin), lwd=2, col="darkgreen"))
  }
  
  axis(1,ticks=1:12,at=1:12,labels=c("J","F","M","A","M","J","J","A","S","O","N","D"))
  with(res, polygon(c(x,rev(x)),c(min,rev(max)),col="grey89",border="grey89"))
  with(res, lines(x,mean))
  lines(cur,col="red")
}