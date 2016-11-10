#yields
load('D:/Tobackup/BID/FAOSTAT_validation/ylds_soya_optVariety_old.Rdat')
yld.old = yld.country.m

load('D:/Tobackup/BID/FAOSTAT_validation/ylds_soya_optVariety.Rdat')
yld.new = yld.country.m

barplot(rbind(yld.old[,10],yld.new[,10]),beside=T,xlab='',xaxt='n')
abline(h=0)
text(seq(1,dim(yld.new)[1]*3,by=3), par("usr")[3]-0.25, 
     srt = 55, adj= 1, xpd = TRUE,
     labels = paste(rownames(yld.new)), cex=0.8)
legend('topleft',c('Old','New'),fill=c('gray30','gray90'))

#crop failures
load('D:/Tobackup/BID/FAOSTAT_validation/country.areaFail_soya_optVariety_old.Rdat')
fail.old = fail.compare

load('D:/Tobackup/BID/FAOSTAT_validation/country.areaFail_soya_optVariety.Rdat')
fail.new = fail.compare

barplot(rbind(fail.old[,1],fail.new[,1]),beside=T,xlab='',xaxt='n')
abline(h=0)
text(seq(1,dim(fail.compare)[1]*3,by=3), par("usr")[3]-0.25, 
     srt = 55, adj= 1, xpd = TRUE,
     labels = paste(rownames(fail.compare)), cex=0.8)
legend('topright',c('Old','New'),fill=c('gray30','gray90'))

#yield changes at country scale
load('D:/Tobackup/BID/FAOSTAT_validation/country.pctCh_soya_optVariety_old.Rdat')
pct.old = pct.ch.country
pct.old.CI=pct.ch.country.CI

load('D:/Tobackup/BID/FAOSTAT_validation/country.pctCh_soya_optVariety.Rdat')
pct.new = pct.ch.country
pct.new.CI=pct.ch.country.CI

# barplot(rbind(pct.old,pct.new),beside=T,xlab='',xaxt='n')
# abline(h=0)
# text(seq(1,length(pct.new)*3,by=3), par("usr")[3]+0.25, 
#      srt = 55, adj= 1, xpd = TRUE,
#      labels = paste(names(pct.new)), cex=0.8)
# legend('topleft',c('Old','New'),fill=c('gray30','gray90'))

library('gplots')
barplot2(rbind(pct.old,pct.new),beside=T,xlab='',xaxt='n',plot.ci=T,ci.l=rbind(pct.old.CI[1,],pct.new.CI[1,]),ci.u = rbind(pct.old.CI[2,],pct.new.CI[2,]))
abline(h=0)
text(seq(1,length(pct.new)*3,by=3), par("usr")[3]+0.25, 
     srt = 55, adj= 1, xpd = TRUE,
     labels = paste(names(pct.new)), cex=0.8)
legend('bottomright',c('Old','New'),fill=c('red','yellow'))

#yield changes at FPU scale
load('D:/Tobackup/BID/FAOSTAT_validation/country.pctCh_soya_optVariety_FPU_old.Rdat')
pct.old = pct.ch.FPU
pct.old.CI=pct.ch.FPU.CI

load('D:/Tobackup/BID/FAOSTAT_validation/country.pctCh_soya_optVariety_FPU.Rdat')
pct.new = pct.ch.FPU
pct.new.CI=pct.ch.FPU.CI

library('gplots')
barplot2(rbind(pct.old,pct.new),beside=T,xlab='',xaxt='n',plot.ci=T,ci.l=rbind(pct.old.CI[1,],pct.new.CI[1,]),ci.u = rbind(pct.old.CI[2,],pct.new.CI[2,]))
abline(h=0)
text(seq(1,length(pct.new)*3,by=3), par("usr")[3]+0.25, 
     srt = 55, adj= 1, xpd = TRUE,
     labels = paste(names(pct.new)), cex=0.8)
legend('topleft',c('Old','New'),fill=c('red','yellow'))


