#JRV
#UoL / CCAFS / CIAT
#Jan 2014

#testing of HTS parameter values for inclusion into maize model

tcrmin <- 35
tlimmin <- 40
tlint <- 50
tcslope <- 0.35
tlslope <- 3.0

idurmax <- 6
ibamax <- 6
iaamax <- 12

##############################################################################
## effect before anthesis
##############################################################################
calc_ba <- function(ibamax,tcrmin,tlimmin,tcslope,tlslope,tlint) {
  idapflwr <- 0
  tcrit <- c(); tlim <- c()
  for (idra in (idapflwr-ibamax):0) {
    #idra <- ((idapflwr-ibamax):0)[1]
    tcrit1 <- tcrmin + tcslope * (abs(idra) - ibamax)
    tcrit <- c(tcrit,tcrit1)
    
    tlim1 <- tlint + tlslope * (abs(idra) - ibamax)
    if (tlim1 <= tlimmin) {tlim1 <- tlimmin}
    tlim <- c(tlim,tlim1)
  }
  return(list(idra=((idapflwr-ibamax):0),tcrit=tcrit,tlim=tlim))
}

###
#ensemble of hts parameterisations before anthesis
###

#sensitive
ba1 <- calc_ba(ibamax=6,tcrmin=36,tlimmin=40,tcslope=0.0,tlslope=2,tlint=50)
plot(ba1$idra,ba1$tlim,ty="l",ylim=c(30,60))
for (i in seq(2,2.5,by=0.1)) {
  bax <- calc_ba(ibamax=6,tcrmin=36,tlimmin=40,tcslope=0.0,tlslope=i,tlint=50)
  lines(bax$idra,bax$tlim,col="blue")
}

ba1 <- calc_ba(ibamax=6,tcrmin=36,tlimmin=40,tcslope=0.0,tlslope=2,tlint=50)
lines(ba1$idra,ba1$tcrit,ty="l")
for (i in seq(0,0.5,by=0.05)) {
  bax <- calc_ba(ibamax=6,tcrmin=36,tlimmin=40,tcslope=i,tlslope=2,tlint=50)
  lines(bax$idra,bax$tcrit,col="red")
}
abline(h=33)


#moderately tolerant
ba2 <- calc_ba(ibamax=6,tcrmin=40,tlimmin=45,tcslope=0.0,tlslope=1.2,tlint=55)
plot(ba2$idra,ba2$tlim,ty="l",ylim=c(30,60))
for (i in seq(1.2,1.7,by=0.1)) {
  bax <- calc_ba(ibamax=6,tcrmin=40,tlimmin=45,tcslope=0.0,tlslope=i,tlint=55)
  lines(bax$idra,bax$tlim,col="blue")
}

ba2 <- calc_ba(ibamax=6,tcrmin=40,tlimmin=45,tcslope=0.0,tlslope=1.2,tlint=55)
lines(ba2$idra,ba2$tcrit)
for (i in seq(0,0.7,by=0.05)) {
  bax <- calc_ba(ibamax=6,tcrmin=40,tlimmin=45,tcslope=i,tlslope=1.2,tlint=55)
  lines(bax$idra,bax$tcrit,col="red")
}
abline(h=36)


#tolerant
ba2 <- calc_ba(ibamax=6,tcrmin=42,tlimmin=50,tcslope=0.0,tlslope=1,tlint=60)
plot(ba2$idra,ba2$tlim,ty="l",ylim=c(30,60))
for (i in seq(1,1.5,by=0.1)) {
  bax <- calc_ba(ibamax=6,tcrmin=42,tlimmin=50,tcslope=0.0,tlslope=i,tlint=60)
  lines(bax$idra,bax$tlim,col="blue")
}

ba3 <- calc_ba(ibamax=6,tcrmin=42,tlimmin=50,tcslope=0.0,tlslope=1,tlint=60)
lines(ba3$idra,ba3$tcrit)
for (i in seq(0,0.7,by=0.05)) {
  bax <- calc_ba(ibamax=6,tcrmin=42,tlimmin=50,tcslope=i,tlslope=1,tlint=60)
  lines(bax$idra,bax$tcrit,col="red")
}
abline(h=38)


##############################################################################
## effect after anthesis

calc_aa <- function(iaamax,idurmax,tcrmin,tlimmin) {
  idapflwr <- 0
  tcrit <- c(); tlim <- c()
  for (idra in (idapflwr+1):iaamax) {
    #idra <- (idapflwr:iaamax)[1]
    for (idur in 1:idurmax) {
      #idur <- 0
      tcrit1 <- 37.5 + 1.8 * abs(idra) - 3 * idur
      tcrit1 <- max(c(tcrit1,tcrmin))
      tcrit <- c(tcrit,tcrit1)
      
      tlim1 <- 48.8 + 0.75 * abs(idra) - 1.5 * idur
      if (tlim1 <= tlimmin) {tlim1 <- tlimmin}
      tlim <- c(tlim,tlim1)
    }
  }
  tmat <- expand.grid(idur=(1:idurmax),idra=((idapflwr+1):iaamax))
  tmat$tcrit <- tcrit
  tmat$tlim <- tlim
  return(tmat)
}

#sensitive
aa1 <- calc_aa(iaamax=12,idurmax=6,tcrmin=36,tlimmin=40)
imat1 <- matrix(aa1$tcrit,nrow=6,ncol=12,byrow=F)
x11()
persp(unique(aa1$idur),unique(aa1$idra),imat1,theta=30,phi=30,zlim=c(36,60),
      xlab="dur",ylab="day",zlab="tcrit",ticktype="detailed",col="grey 80")

imat2 <- matrix(aa1$tlim,nrow=6,ncol=12,byrow=F)
x11()
persp(unique(aa1$idur),unique(aa1$idra),imat2,theta=30,phi=30,zlim=c(36,60),
      xlab="dur",ylab="day",zlab="tlim",ticktype="detailed",col="grey 80")


#moderately tolerant
aa2 <- calc_aa(iaamax=12,idurmax=6,tcrmin=40,tlimmin=45)
imat1 <- matrix(aa2$tcrit,nrow=6,ncol=12,byrow=F)
x11()
persp(unique(aa2$idur),unique(aa2$idra),imat1,theta=30,phi=30,zlim=c(36,60),
      xlab="dur",ylab="day",zlab="tcrit",ticktype="detailed",col="grey 80")

imat2 <- matrix(aa2$tlim,nrow=6,ncol=12,byrow=F)
x11()
persp(unique(aa2$idur),unique(aa2$idra),imat2,theta=30,phi=30,zlim=c(36,60),
      xlab="dur",ylab="day",zlab="tlim",ticktype="detailed",col="grey 80")

#tolerant
aa3 <- calc_aa(iaamax=12,idurmax=6,tcrmin=42,tlimmin=50)
imat1 <- matrix(aa3$tcrit,nrow=6,ncol=12,byrow=F)
x11()
persp(unique(aa3$idur),unique(aa3$idra),imat1,theta=30,phi=30,zlim=c(36,60),
      xlab="dur",ylab="day",zlab="tcrit",ticktype="detailed",col="grey 80")

imat2 <- matrix(aa3$tlim,nrow=6,ncol=12,byrow=F)
x11()
persp(unique(aa3$idur),unique(aa3$idra),imat2,theta=30,phi=30,zlim=c(36,60),
      xlab="dur",ylab="day",zlab="tlim",ticktype="detailed",col="grey 80")


