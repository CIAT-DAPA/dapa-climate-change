#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Dec 2012

##########################################################################
##########################################################################
#### filtering function
filt_fun <- function(x,this_data,totcases) {
  dur <- x[1]; tds <- x[2]; hit <- x[3]; wat <- x[4]; pup <- x[5]
  rad <- x[6]
  
  tmpdata <- this_data
  
  #duration
  if (!is.na(dur)) {
    if (dur) {
      tmpdata <- tmpdata[which(tmpdata$PRJ.DUR > tmpdata$HIS.DUR),]
    } else {
      tmpdata <- tmpdata[which(tmpdata$PRJ.DUR < tmpdata$HIS.DUR),]
    }
  }
  
  #tds
  if (!is.na(tds)) {
    if (tds) {
      tmpdata <- tmpdata[which(tmpdata$PRJ.STG == 4),]
    } else {
      tmpdata <- tmpdata[which(tmpdata$PRJ.STG == 6),]
    }
  }
  
  #hit
  if (!is.na(hit)) {
    if (hit) {
      tmpdata <- tmpdata[which(tmpdata$PRJ.HIT == 1),]
    } else {
      tmpdata <- tmpdata[which(tmpdata$PRJ.HIT < 1),]
    }
  }
  
  #wat
  if (!is.na(wat)) {
    if (wat) {
      tmpdata <- tmpdata[which(tmpdata$PRJ.WAT == 1),]
    } else {
      tmpdata <- tmpdata[which(tmpdata$PRJ.WAT < 1),]
    }
  }
  
  #pup
  if (!is.na(pup)) {
    if (pup) {
      tmpdata <- tmpdata[which(tmpdata$PRJ.PUP > tmpdata$HIS.PUP),]
    } else {
      tmpdata <- tmpdata[which(tmpdata$PRJ.PUP < tmpdata$HIS.PUP),]
    }
  }
  
  #rad
  if (!is.na(rad)) {
    if (rad) {
      tmpdata <- tmpdata[which(tmpdata$PRJ.RAD > tmpdata$HIS.RAD),]
    } else {
      tmpdata <- tmpdata[which(tmpdata$PRJ.RAD < tmpdata$HIS.RAD),]
    }
  }
  
  v_me <- mean(tmpdata$PRJ.YCH,na.rm=T)
  v_sd <- sd(tmpdata$PRJ.YCH,na.rm=T)
  v_ct <- length(tmpdata$PRJ.YCH)
  v_pc <- v_ct/totcases*100
  v_pt <- v_ct/nrow(this_data)*100
  
  return(c(v_me,v_sd,v_ct,v_pc,v_pt))
}



