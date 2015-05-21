library(ggplot2)
library(zoo)
library(reshape)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

historicalComparisonLineChart <- function(var="rsds"){
  
  #Arguments
  iDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_extracts"
  stFile <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/sample_type_climates_without_1.csv"

  # Load data
  stFile <- read.csv(stFile)
  
  bc <- read.csv(paste(iDir, "/bc_monthly_", var, ".csv", sep=""),header=T)
  wfd <- read.csv(paste(iDir, "/wfd_monthly_", var, ".csv", sep=""),header=T) 
  gcm <- read.csv(paste(iDir, "/gcm_raw_monthly_", var, ".csv", sep=""),header=T) 
  
  # Modify and arrangement
  gcm$X8019 <- NULL
  bc$X8019 <- NULL
  wfd$X8019 <- NULL
  wfd <- cbind("wfd", wfd)
  colnames(wfd)[1] <- "GCM"
  
  # Select years
  sel_bc_his <- bc[which(1980 <= bc$Year & bc$Year <= 1989),]
  rownames(sel_bc_his) <- NULL
  sel_bc_fut <- bc[which(2030 <= bc$Year & bc$Year <= 2039),]
  rownames(sel_bc_fut) <- NULL
  sel_wfd <- wfd[which(1980 <= wfd$Year & wfd$Year <= 1989),]
  rownames(sel_wfd) <- NULL
  sel_gcm_his <- gcm[which(1980 <= gcm$Year & gcm$Year <= 1989),]
  rownames(sel_gcm_his) <- NULL
  sel_gcm_fut <- gcm[which(2030 <= gcm$Year & gcm$Year <= 2039),]
  rownames(sel_gcm_fut) <- NULL
  
  
  # Set dates
  dates_bc_his <- as.Date(as.yearmon((paste0(sel_bc_his$Year,"-",sel_bc_his$Month))))
  dates_bc_fut <- as.Date(as.yearmon((paste0(sel_bc_fut$Year,"-",sel_bc_fut$Month))))
  dates_wfd <- as.Date(as.yearmon((paste0(sel_wfd$Year,"-",sel_wfd$Month))))
  dates_gcm_his <- as.Date(as.yearmon((paste0(sel_gcm_his$Year,"-",sel_gcm_his$Month))))
  dates_gcm_fut <- as.Date(as.yearmon((paste0(sel_gcm_fut$Year,"-",sel_gcm_fut$Month))))
  
  sel_bc_his <- cbind(sel_bc_his, dates_bc_his)
  sel_bc_fut <- cbind(sel_bc_fut, dates_bc_fut)
  sel_gcm_his <- cbind(sel_gcm_his, dates_gcm_his)
  sel_gcm_fut <- cbind(sel_gcm_fut, dates_gcm_fut)
  sel_wfd <- cbind(sel_wfd, dates_wfd)
  
  avg_bc_his <- aggregate(sel_bc_his[,4:length(sel_bc_his)], by=list(sel_bc_his$Year, sel_bc_his$Month), FUN="mean")
  dates_avg_bc_his <- as.Date(as.yearmon((paste0(avg_bc_his[,1],"-",avg_bc_his[,2]))))
  
  avg_bc_fut <- aggregate(sel_bc_fut[,4:length(sel_bc_fut)], by=list(sel_bc_fut$Year, sel_bc_fut$Month), FUN="mean")
  dates_avg_bc_fut <- as.Date(as.yearmon((paste0(avg_bc_fut[,1],"-",avg_bc_fut[,2]))))
  
  avg_gcm_his <- aggregate(sel_gcm_his[,4:length(sel_gcm_his)], by=list(sel_gcm_his$Year, sel_gcm_his$Month), FUN="mean")
  dates_avg_gcm_his <- as.Date(as.yearmon((paste0(avg_gcm_his[,1],"-",avg_gcm_his[,2]))))
  
  avg_gcm_fut <- aggregate(sel_gcm_fut[,4:length(sel_gcm_fut)], by=list(sel_gcm_fut$Year, sel_gcm_fut$Month), FUN="mean")
  dates_avg_gcm_fut <- as.Date(as.yearmon((paste0(avg_gcm_fut[,1],"-",avg_gcm_fut[,2]))))
  
  #Colors
  blue="#1F78B4";blue2="#A6CEE3";green="#33A02C";green2="#B2DF8A";red="#E31A1C";red2="#FB9A99";orange="#FF7F00";orange2="#FDBF6F"

  if (var == "prec"){ylabel <- "Precipitation (mm/month)"; limit = c(0,1000)}
  if (var == "tmin"){ylabel <- "Min. Temperature (°C)"; limit = c(-10, 25)}
  if (var == "tmax"){ylabel <- "Max. Temperature (°C)"; limit = c(0, 40)}
  if (var == "rsds"){ylabel <- "Shortwave Sol. Radiation (W/m2)"; limit = c(0, 400)}
  
  ## Loop plots

#   for (i in 1:nrow(stFile)){
  
  i <- 1
  classId <- paste("X", stFile$Cell_ID_prec_rsds[i], sep="")
  p1 <- ggplot() + 
    geom_point(data=sel_bc_his, aes_string("dates_bc_his", paste(classId)), colour=red2, fill=red , size=2) +
    geom_line(data=sel_wfd, aes_string("dates_wfd", paste(classId)), colour=orange, size=1) +
    geom_line(data=avg_gcm_his, aes_string("dates_avg_gcm_his", paste(classId)), colour=blue, size=1) +
    geom_line(data=avg_bc_his, aes_string("dates_avg_bc_his", paste(classId)), colour=red, size=1) +
    theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.ticks.x=element_blank(), plot.title = element_text(hjust = 0.97, vjust=-5), axis.title.y = element_text(size = rel(0.8))) +
    scale_y_continuous(limits = limit) +
    ggtitle(stFile$Type[i]) +
    labs(x="Date (months)", y=ylabel)
    
  i <- 2
  classId <- paste("X", stFile$Cell_ID_prec_rsds[i], sep="")
  p2 <- ggplot() + 
    geom_point(data=sel_bc_his, aes_string("dates_bc_his", paste(classId)), colour=red2, fill=red , size=2) +
    geom_line(data=sel_wfd, aes_string("dates_wfd", paste(classId)), colour=orange, size=1) +
    geom_line(data=avg_gcm_his, aes_string("dates_avg_gcm_his", paste(classId)), colour=blue, size=1) +
    geom_line(data=avg_bc_his, aes_string("dates_avg_bc_his", paste(classId)), colour=red, size=1) +
    theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.ticks.x=element_blank(), plot.title = element_text(hjust = 0.97, vjust=-5), axis.title.y = element_text(size = rel(0.8))) +
    scale_y_continuous(limits = limit) +
    ggtitle(stFile$Type[i]) +
    labs(x="Date (months)", y=ylabel)

  i <- 5
  classId <- paste("X", stFile$Cell_ID_prec_rsds[i], sep="")
  p3 <- ggplot() + 
    geom_point(data=sel_bc_his, aes_string("dates_bc_his", paste(classId)), colour=red2, fill=red , size=2) +
    geom_line(data=sel_wfd, aes_string("dates_wfd", paste(classId)), colour=orange, size=1) +
    geom_line(data=avg_gcm_his, aes_string("dates_avg_gcm_his", paste(classId)), colour=blue, size=1) +
    geom_line(data=avg_bc_his, aes_string("dates_avg_bc_his", paste(classId)), colour=red, size=1) +
    theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.ticks.x=element_blank(), plot.title = element_text(hjust = 0.97, vjust=-5), axis.title.y = element_text(size = rel(0.8))) +
    scale_y_continuous(limits = limit) +
    ggtitle(stFile$Type[i]) +
    labs(x="Date (months)", y=ylabel)


  
  i <- 12
  classId <- paste("X", stFile$Cell_ID_prec_rsds[i], sep="")
  p4 <- ggplot() + 
    geom_point(data=sel_bc_his, aes_string("dates_bc_his", paste(classId)), colour=red2, fill=red , size=2) +
    geom_line(data=sel_wfd, aes_string("dates_wfd", paste(classId)), colour=orange, size=1) +
    geom_line(data=avg_gcm_his, aes_string("dates_avg_gcm_his", paste(classId)), colour=blue, size=1) +
    geom_line(data=avg_bc_his, aes_string("dates_avg_bc_his", paste(classId)), colour=red, size=1) +
    theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.ticks.x=element_blank(), plot.title = element_text(hjust = 0.97, vjust=-5), axis.title.y = element_text(size = rel(0.8))) +
    scale_y_continuous(limits = limit) +
    ggtitle(stFile$Type[i]) +
    labs(x="Date (months)", y=ylabel)

  i <- 14
  classId <- paste("X", stFile$Cell_ID_prec_rsds[i], sep="")
  p5 <- ggplot() + 
    geom_point(data=sel_bc_his, aes_string("dates_bc_his", paste(classId)), colour=red2, fill=red , size=2) +
    geom_line(data=sel_wfd, aes_string("dates_wfd", paste(classId)), colour=orange, size=1) +
    geom_line(data=avg_gcm_his, aes_string("dates_avg_gcm_his", paste(classId)), colour=blue, size=1) +
    geom_line(data=avg_bc_his, aes_string("dates_avg_bc_his", paste(classId)), colour=red, size=1) +
    theme(panel.background = element_rect(fill = 'gray92'), plot.title = element_text(hjust = 0.97, vjust=-5), axis.title.y = element_text(size = rel(0.8))) +
    scale_y_continuous(limits = limit) +
    ggtitle(stFile$Type[i]) +
    labs(x="Date (months)", y=ylabel)
  
  tiff(paste(iDir, "/plots/", var, "_hist_line_chart_v2.tif", sep=""), width=1000, height=1200, pointsize=8, compression='lzw',res=100)
    multiplot(p1, p2, p3, p4, p5, cols=1)
  dev.off()
    
  #     ggsave(filename = paste(iDir, "/plots/", var, "_hist_line_chart_", stFile$Type[i], ".png", sep=""), width=12, height=8, dpi = 100)

    
  #   #Merge in one set
  #   all_data <-cbind("Dataset"=c(rep("bc",nrow(sel_bc)),rep("gcm",nrow(sel_gcm)),rep("wfd",nrow(sel_wfd))),rbind(sel_bc,sel_gcm,sel_wfd))
  #   all_data <- melt(all_data,id=c("Dataset","GCM","Year","Month"))
  #   dates <- as.Date(as.yearmon((paste0(data$Year,"-",data$Month))))
  #   
  #   
  #   p <- ggplot() + 
  #     geom_point(data=all_data[which(all_data$GCM != "wfd"),], aes(as.Date(dates[1:nrow(all_data[which(all_data$GCM != "wfd"),])]), value,colour=Dataset),size=0.2, shape=1) + 
  #     scale_fill_manual(values=c(red, blue)) +
  #     facet_wrap(~variable,scales="free_y", ncol=1)
  #   
  #   print(p)
    
    
    
  #   stat_smooth(data=sel_bc, aes(as.Date(dates_bc), X2854), colour="sky blue", size=1, se=F,method=loess,level=0.99, fullrange = T) +
  #     # geom_smooth(data=sel_wfd, aes(as.Date(dates_wfd), X2854), fill="red", colour="red", size=1, level=0.98) + 
  #     geom_line(data=sel_gcm, aes(as.Date(dates_gcm), X2854), colour="green", size=1)
  #   
    
    #   years.gcm.hist = 1971:2000
    #   years.gcm.fut = 2020:2049
    #   
    #   months = c(paste(0,1:9,sep=''),10:12)
    #   
    #   dates.gcm.hist = seq(as.Date('1971-01-01'),as.Date('2000-12-31'),by=1)
    #   dates.gcm.fut = seq(as.Date('2020-01-01'),as.Date('2049-12-31'),by=1)
    #   
    #   ## Smoothed
    #   p <- ggplot() + 
    # #     geom_point(data=selHist, aes(as.Date(dates), X2854), fill="blue", colour="blue", size=0.1) +
    #     geom_smooth(data=sel_bc, aes(as.Date(dates_bc), X2854), fill="blue", colour="blue", size=1, level=0.99, method=loess) +
    #     # geom_smooth(data=sel_wfd, aes(as.Date(dates_wfd), X2854), fill="red", colour="red", size=1, level=0.98) + 
    #     geom_smooth(data=sel_gcm, aes(as.Date(dates_gcm), X2854), fill="green", colour="green", size=1, level=0.99, method=loess)
    #   
    #   print(p)
  
}

frequencyRainfallComparison <- function(var="tmax"){
  
  #Arguments
  iDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_extracts"
  stFile <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/sample_type_climates_without_1.csv"
  
  # Load data
  stFile <- read.csv(stFile)
  
  bc <- read.csv(paste(iDir, "/bc_monthly_", var, "_freq.csv", sep=""),header=T)
  wfd <- read.csv(paste(iDir, "/wfd_monthly_", var, "_freq.csv", sep=""),header=T) 
  gcm <- read.csv(paste(iDir, "/gcm_raw_monthly_", var, "_freq.csv", sep=""),header=T) 
  
  # Modify and arrangement
  gcm$X8019 <- NULL
  wfd$X8019 <- NULL
  wfd <- cbind("wfd", wfd)
  colnames(wfd)[1] <- "GCM"
  
  # Select years
  sel_bc_hist <- bc[which(1971 <= bc$Year & bc$Year <= 2000),]
  rownames(sel_bc_hist) <- NULL
  sel_bc_fut <- bc[which(2020 <= bc$Year & bc$Year <= 2049),]
  rownames(sel_bc_fut) <- NULL
  
  sel_wfd <- wfd
  
  sel_gcm_hist <- gcm[which(1971 <= gcm$Year & gcm$Year <= 2000),]
  sel_gcm_fut <- gcm[which(2020 <= gcm$Year & gcm$Year <= 2049),]
#   
#   # Set dates
#   dates_bc <- as.Date(as.yearmon((paste0(sel_bc$Year,"-",sel_bc$Month))))
#   dates_wfd <- as.Date(as.yearmon((paste0(sel_wfd$Year,"-",sel_wfd$Month))))
#   dates_gcm <- as.Date(as.yearmon((paste0(sel_gcm$Year,"-",sel_gcm$Month))))
#   
  #Colors
  blue="#1F78B4";blue2="#A6CEE3";green="#33A02C";green2="#B2DF8A";red="#E31A1C";red2="#FB9A99";orange="#FF7F00";orange2="#FDBF6F"
  
  if (var == "prec"){ylabel <- "Rainfall Frequency\n (Days/Month)"}
  if (var == "tmin"){ylabel <- "Hot days Frequency (Days/Month)"}
  if (var == "tmax"){ylabel <- "Max. Temperature (°C)"}
  if (var == "rsds"){ylabel <- "Shortwave Sol. Radiation (W/m2)"}
  
  # Rename months
  f_names <- list("1"="January", "2"="February", "3"="March", "4"="April", "5"="May", "6"="June", "7"="July", "8"="August", "9"="September", "10"="October", "11"="November", "12"="December")
  f_labeller <- function(variable, value){return(f_names[value])}
    
  #Merge in one set
  allData <- cbind("Dataset"=c(rep("Obs.(WFD)",nrow(sel_wfd)),
                   rep("GCM His.",nrow(sel_gcm_hist)), 
                   rep("GCM Fut.",nrow(sel_gcm_fut)), 
                   rep("GCM_BC His.",nrow(sel_bc_hist)),
                   rep("GCM_BC Fut.",nrow(sel_bc_fut))),
                   rbind(sel_wfd, sel_gcm_hist, sel_gcm_fut, sel_bc_hist, sel_bc_fut))
  allData <- melt(allData,id=c("Dataset","GCM","Year","Month"))
  
  ## Loop by location
#   for (i in 1:nrow(stFile)){
    
    i <- 1
    classId <- paste("X", stFile$Cell_ID_prec_rsds[i], sep="")
    allDataSel <- allData[which(allData$variable == classId),]
    
    p1 <- ggplot(data=allDataSel, aes(Dataset, value, fill=Dataset)) + 
      theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), legend.title=element_text(size=rel(1.4)), axis.title.y = element_text(size = rel(0.8))) +
      scale_fill_manual(name=stFile$Type[i], values=c(blue, blue2, red, red2,orange2)) +
      geom_boxplot(outlier.size = 1) +
      labs(x="Months", y=ylabel) +
      facet_grid(~Month,scales="free_y", drop=T, labeller=f_labeller)
    
    i <- 2
    classId <- paste("X", stFile$Cell_ID_prec_rsds[i], sep="")
    allDataSel <- allData[which(allData$variable == classId),]
    
    p2 <- ggplot(data=allDataSel, aes(Dataset, value, fill=Dataset)) + 
      theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), legend.title=element_text(size=rel(1.4)), axis.title.y = element_text(size = rel(0.8))) +
      scale_fill_manual(name=stFile$Type[i], values=c(blue, blue2, red, red2, orange2)) +
      geom_boxplot(outlier.size = 1) +
      labs(x="Months", y=ylabel) +
      facet_grid(~Month,scales="free_y", drop=T, labeller=f_labeller)
    
#     i <- 5
#     classId <- paste("X", stFile$Cell_ID_prec_rsds[i], sep="")
#     allDataSel <- allData[which(allData$variable == classId),]
#     
#     p3 <- ggplot(data=allDataSel, aes(Dataset, value, fill=Dataset)) + 
#       theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), legend.title=element_text(size=rel(1.4)), axis.title.y = element_text(size = rel(0.8))) +
#       scale_fill_manual(name=stFile$Type[i], values=c(blue, blue2, red, red2, orange2)) +
#       geom_boxplot(outlier.size = 1) +
#       labs(x="Months", y=ylabel) +
#       facet_grid(~Month,scales="free_y", drop=T, labeller=f_labeller)
#     
#     i <- 12
#     classId <- paste("X", stFile$Cell_ID_prec_rsds[i], sep="")
#     allDataSel <- allData[which(allData$variable == classId),]
#     
#     p4 <- ggplot(data=allDataSel, aes(Dataset, value, fill=Dataset)) + 
#       theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), legend.title=element_text(size=rel(1.4)), axis.title.y = element_text(size = rel(0.8))) +
#       scale_fill_manual(name=stFile$Type[i], values=c(blue, blue2, red, red2, orange2)) +
#       geom_boxplot(outlier.size = 1) +
#       labs(x="Months", y=ylabel) +
#       facet_grid(~Month,scales="free_y", drop=T, labeller=f_labeller)
    
    
    i <- 14
    classId <- paste("X", stFile$Cell_ID_prec_rsds[i], sep="")
    allDataSel <- allData[which(allData$variable == classId),]
    
    p5 <- ggplot(data=allDataSel, aes(Dataset, value, fill=Dataset)) + 
      theme(panel.background = element_rect(fill = 'gray92'), axis.text.x=element_blank(), axis.ticks=element_blank(), legend.title=element_text(size=rel(1.4)), axis.title.y = element_text(size = rel(0.8))) +
      scale_fill_manual(name=stFile$Type[i], values=c(blue, blue2, red, red2, orange2)) +
      geom_boxplot(outlier.size = 1) +
      labs(x="Months", y=ylabel) +
      facet_grid(~Month,scales="free_y", drop=T, labeller=f_labeller)
    
    tiff(paste(iDir, "/plots/", var, "_freq_chart.tif", sep=""), width=1000, height=800, pointsize=8, compression='lzw',res=100)
    multiplot(p1, p2, p5, cols=1)
    dev.off()
    

# theme(text = element_text(size=15, colour = "black"),
#       axis.text.x = element_text(angle=0, vjust=1, colour = "black"),
#       axis.text.y = element_text(angle=0, vjust=1, colour = "black"),
#       axis.line = element_line(colour = 'black', size = 1),
#       axis.ticks = element_line(colour = 'black', size = 1),
#       axis.ticks.length = unit(0.3, "cm"),
#       axis.title.y=element_text(vjust=0.4),
#       legend.position = "none") + 
#   
#   theme(axis.line=element_blank(),
#         axis.text.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         legend.position="none",
#         panel.background=element_blank(),
#         panel.border=element_blank(),
#         panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         plot.background=element_blank())

#     ggsave(filename = paste(iDir, "/plots/", var, "_frequency_", stFile$Type[i], ".png", sep=""), width=12, height=3, dpi = 100)
    
#   }
  
}

stdRainfallComparisonMontly <- function(var="prec"){
  
  #Arguments
  iDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_extracts"
  stFile <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/sample_type_climates_without_1.csv"
  
  # Load data
  stFile <- read.csv(stFile)
  
  bc <- read.csv(paste(iDir, "/bc_monthly_", var, "_std.csv", sep=""),header=T)
  wfd <- read.csv(paste(iDir, "/wfd_monthly_", var, "_std.csv", sep=""),header=T) 
  gcm <- read.csv(paste(iDir, "/gcm_raw_monthly_", var, "_std.csv", sep=""),header=T) 
  
  # Modify and arrangement
  gcm$X8019 <- NULL
  wfd$X8019 <- NULL
  wfd <- cbind("wfd", wfd)
  colnames(wfd)[1] <- "GCM"
  
  # Select years
  sel_bc_hist <- bc[which(1971 <= bc$Year & bc$Year <= 2000),]
  rownames(sel_bc_hist) <- NULL
  sel_bc_fut <- bc[which(2020 <= bc$Year & bc$Year <= 2049),]
  rownames(sel_bc_fut) <- NULL
  
  sel_wfd <- wfd
  
  sel_gcm_hist <- gcm[which(1971 <= gcm$Year & gcm$Year <= 2000),]
  sel_gcm_fut <- gcm[which(2020 <= gcm$Year & gcm$Year <= 2049),]
  #   
  #   # Set dates
  #   dates_bc <- as.Date(as.yearmon((paste0(sel_bc$Year,"-",sel_bc$Month))))
  #   dates_wfd <- as.Date(as.yearmon((paste0(sel_wfd$Year,"-",sel_wfd$Month))))
  #   dates_gcm <- as.Date(as.yearmon((paste0(sel_gcm$Year,"-",sel_gcm$Month))))
  #   
  #Colors
  blue="#1F78B4";blue2="#A6CEE3";green="#33A02C";green2="#B2DF8A";red="#E31A1C";red2="#FB9A99";orange="#FF7F00";orange2="#FDBF6F"
  
  if (var == "prec"){ylabel <- "STD Daily Rainfall (mm/day)"}
  if (var == "tmin"){ylabel <- "STD Daily Min. Temperature (°C)"}
  if (var == "tmax"){ylabel <- "STD Daily Max. Temperature (°C)"}
  if (var == "rsds"){ylabel <- "STD Daily Shortwave Sol. Radiation (W/m2)"}
  
  # Rename months
  f_names <- list("1"="January", "2"="February", "3"="March", "4"="April", "5"="May", "6"="June", "7"="July", "8"="August", "9"="September", "10"="October", "11"="November", "12"="December")
  f_labeller <- function(variable, value){return(f_names[value])}
  
  #Merge in one set
  allData <- cbind("Dataset"=c(rep("WFD",nrow(sel_wfd)),
                               rep("GCM His",nrow(sel_gcm_hist)), 
                               rep("GCM Fut",nrow(sel_gcm_fut)), 
                               rep("BC His",nrow(sel_bc_hist)),
                               rep("BC Fut",nrow(sel_bc_fut))),
                   rbind(sel_wfd, sel_gcm_hist, sel_gcm_fut, sel_bc_hist, sel_bc_fut))
  allData <- melt(allData,id=c("Dataset","GCM","Year","Month"))
  
  ## Loop by location
  for (i in 1:nrow(stFile)){
    
    classId <- paste("X", stFile$Cell_ID_prec_rsds[i], sep="")
    
    allDataSel <- allData[which(allData$variable == classId),]
    
    ggplot(data=allDataSel, aes(Dataset, value, fill=Dataset)) + 
      theme(panel.background = element_rect(fill = 'gray92'), axis.text.x=element_blank(), axis.ticks=element_blank()) +
      scale_fill_manual(values=c(red, red2, blue, blue2, orange2)) +
      geom_boxplot() +
      labs(x="Months", y=ylabel, title=paste(stFile$Type[i])) +
      facet_grid(~Month,scales="free_y", drop=T, labeller=f_labeller)
    
    ggsave(filename = paste(iDir, "/plots/", var, "_stddaily_", stFile$Type[i], ".png", sep=""), width=12, height=3, dpi = 100)
    
  }
  
}

stdComparisonMontly <- function(var="rsds"){
  
  #Arguments
  iDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_extracts"
  stFile <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/sample_type_climates_without_1.csv"
  
  # Load data
  stFile <- read.csv(stFile)
  
  bc <- read.csv(paste(iDir, "/bc_monthly_", var, "_std.csv", sep=""),header=T)
  wfd <- read.csv(paste(iDir, "/wfd_monthly_", var, "_std.csv", sep=""),header=T) 
#   gcm <- read.csv(paste(iDir, "/gcm_raw_monthly_", var, "_std.csv", sep=""),header=T) 
  
  # Modify and arrangement
#   gcm$X8019 <- NULL
  wfd$X8019 <- NULL
  bc$X8019 <- NULL
  wfd <- cbind("wfd", wfd)
  colnames(wfd)[1] <- "GCM"
  
  # Select years
  sel_bc_hist <- bc[which(1971 <= bc$Year & bc$Year <= 2000),]
  rownames(sel_bc_hist) <- NULL
  sel_bc_fut <- bc[which(2020 <= bc$Year & bc$Year <= 2049),]
  rownames(sel_bc_fut) <- NULL
  
  sel_wfd <- wfd
  
#   sel_gcm_hist <- gcm[which(1971 <= gcm$Year & gcm$Year <= 2000),]
#   sel_gcm_fut <- gcm[which(2020 <= gcm$Year & gcm$Year <= 2049),]
  #   
  #   # Set dates
  #   dates_bc <- as.Date(as.yearmon((paste0(sel_bc$Year,"-",sel_bc$Month))))
  #   dates_wfd <- as.Date(as.yearmon((paste0(sel_wfd$Year,"-",sel_wfd$Month))))
  #   dates_gcm <- as.Date(as.yearmon((paste0(sel_gcm$Year,"-",sel_gcm$Month))))
  #   
  #Colors
  blue="#1F78B4";blue2="#A6CEE3";green="#33A02C";green2="#B2DF8A";red="#E31A1C";red2="#FB9A99";orange="#FF7F00";orange2="#FDBF6F"
  
  if (var == "prec"){ylabel <- "STD Daily Rainfall (mm/day)"}
  if (var == "tmin"){ylabel <- "STD Daily Min. Temperature (°C)"}
  if (var == "tmax"){ylabel <- "STD Daily Max. Temperature (°C)"}
  if (var == "rsds"){ylabel <- "STD Daily Shortwave Sol. Radiation (W/m2)"}
  
  # Rename months
  f_names <- list("1"="January", "2"="February", "3"="March", "4"="April", "5"="May", "6"="June", "7"="July", "8"="August", "9"="September", "10"="October", "11"="November", "12"="December")
  f_labeller <- function(variable, value){return(f_names[value])}
  
  #Merge in one set
  allData <- cbind("Dataset"=c(rep("WFD",nrow(sel_wfd)),
#                                rep("GCM His",nrow(sel_gcm_hist)), 
#                                rep("GCM Fut",nrow(sel_gcm_fut)), 
                               rep("BC His",nrow(sel_bc_hist)),
                               rep("BC Fut",nrow(sel_bc_fut))),
                   rbind(sel_wfd, 
#                          sel_gcm_hist, 
#                          sel_gcm_fut, 
                         sel_bc_hist, 
                         sel_bc_fut))
  allData <- melt(allData,id=c("Dataset","GCM","Year","Month"))
  
  ## Loop by location
  for (i in 1:nrow(stFile)){
    
    classId <- paste("X", stFile$Cell_ID_prec_rsds[i], sep="")
    
    allDataSel <- allData[which(allData$variable == classId),]
    
    ggplot(data=allDataSel, aes(Dataset, value, fill=Dataset)) + 
      theme(panel.background = element_rect(fill = 'gray92'), axis.text.x=element_blank(), axis.ticks=element_blank()) +
      scale_fill_manual(values=c(red, 
                                 red2, 
#                                  blue, 
#                                  blue2, 
                                 orange2)) +
      geom_boxplot() +
      labs(x="Months", y=ylabel, title=paste(stFile$Type[i])) +
      facet_grid(~Month,scales="free_y", drop=T, labeller=f_labeller)
    
    ggsave(filename = paste(iDir, "/plots/", var, "_stddaily_", stFile$Type[i], ".png", sep=""), width=12, height=3, dpi = 100)
    
  }
  
}

stdRainfallComparisonMontly30yr <- function(var="rsds"){
  
  #Arguments
  iDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_extracts"
  stFile <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/sample_type_climates_without_1.csv"
  
  # Load data
  stFile <- read.csv(stFile)
  
  bc_his <- read.csv(paste(iDir, "/bc_monthly_", var, "_std_30yrhis.csv", sep=""),header=T)
  bc_fut <- read.csv(paste(iDir, "/bc_monthly_", var, "_std_30yrfut.csv", sep=""),header=T)
  wfd <- read.csv(paste(iDir, "/wfd_monthly_", var, "_std_30yr.csv", sep=""),header=T) 
  gcm_his <- read.csv(paste(iDir, "/gcm_raw_monthly_", var, "_std_30yrhis.csv", sep=""),header=T) 
  gcm_fut <- read.csv(paste(iDir, "/gcm_raw_monthly_", var, "_std_30yrfut.csv", sep=""),header=T) 
  
  # Modify and arrangement
  gcm_his$X8019 <- NULL
  gcm_fut$X8019 <- NULL
  bc_his$X8019 <- NULL
  bc_fut$X8019 <- NULL
  wfd <- cbind("wfd", wfd)
  colnames(wfd)[1] <- "GCM"
  
  #Colors
  blue="#1F78B4";blue2="#A6CEE3";green="#33A02C";green2="#B2DF8A";red="#E31A1C";red2="#FB9A99";orange="#FF7F00";orange2="#FDBF6F"
  
  # Rename months
  f_names <- list("1"="January", "2"="February", "3"="March", "4"="April", "5"="May", "6"="June", "7"="July", "8"="August", "9"="September", "10"="October", "11"="November", "12"="December")
  f_labeller <- function(variable, value){return(f_names[value])}
  
  #Merge in one set
  allData <- cbind("Dataset"=c(rep("Obs. (WFD)",nrow(wfd)),
                               rep("GCM His.",nrow(gcm_his)), 
                               rep("GCM Fut.",nrow(gcm_fut)), 
                               rep("GCM_BC His.",nrow(bc_his)),
                               rep("GCM_BC Fut.",nrow(bc_fut))),
                   rbind(wfd, gcm_his, gcm_fut, bc_his, bc_fut))
  allData <- melt(allData,id=c("Dataset","GCM","Month"))
  
  
  if (var == "prec"){
    ylabel <- "Interannual Variability\n (mm/month)"
#     limit=c(0,200)
  }
  if (var == "tmin"){
    ylabel <- "Interannual Variability\n (°C)"
    limit=c(0,2)
  }
  if (var == "tmax"){
    ylabel <- "Interannual Variability\n (°C)"
    limit=c(0,4)
  }
  if (var == "rsds"){
    ylabel <- "Interannual Variability\n (W/m2)"
    limit=c(0,60)
  }
    
  ## Loop by location
#   for (i in 1:nrow(stFile)){
    
    i <- 1
    classId <- paste("X", stFile$Cell_ID_prec_rsds[i], sep="")
    allDataSel <- allData[which(allData$variable == classId),]
    
    p1 <- ggplot(data=allDataSel, aes(Dataset, value, fill=Dataset)) + 
      theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), legend.title=element_text(size=rel(1.4))) +
      scale_fill_manual(name=stFile$Type[i], values=c(blue, blue2, red, red2, orange2)) +
      geom_boxplot(outlier.size = 1) +
      labs(x="Months", y=ylabel) +
#       scale_y_continuous(limits = limit) +
      theme(axis.title.y = element_text(size = rel(0.8))) +
      facet_grid(~Month,scales="free_y", drop=T, labeller=f_labeller)
    
    i <- 2
    classId <- paste("X", stFile$Cell_ID_prec_rsds[i], sep="")
    allDataSel <- allData[which(allData$variable == classId),]
    
    p2 <- ggplot(data=allDataSel, aes(Dataset, value, fill=Dataset)) + 
      theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), legend.title=element_text(size=rel(1.4))) +
      scale_fill_manual(name=stFile$Type[i], values=c(blue, blue2, red, red2, orange2)) +
      geom_boxplot(outlier.size = 1) +
      labs(x="Months", y=ylabel) +
#       scale_y_continuous(limits = limit) +
      theme(axis.title.y = element_text(size = rel(0.8))) +
      facet_grid(~Month,scales="free_y", drop=T, labeller=f_labeller)
    
    i <- 5
    classId <- paste("X", stFile$Cell_ID_prec_rsds[i], sep="")
    allDataSel <- allData[which(allData$variable == classId),]
    
    p3 <- ggplot(data=allDataSel, aes(Dataset, value, fill=Dataset)) + 
      theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), legend.title=element_text(size=rel(1.4))) +
      scale_fill_manual(name=stFile$Type[i], values=c(blue, blue2, red, red2, orange2)) +
      geom_boxplot(outlier.size = 1) +
      labs(x="Months", y=ylabel) +
#       scale_y_continuous(limits = limit) +
      theme(axis.title.y = element_text(size = rel(0.8))) +
      facet_grid(~Month,scales="free_y", drop=T, labeller=f_labeller)
    
    i <- 12
    classId <- paste("X", stFile$Cell_ID_prec_rsds[i], sep="")
    allDataSel <- allData[which(allData$variable == classId),]
    
    p4 <- ggplot(data=allDataSel, aes(Dataset, value, fill=Dataset)) + 
      theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), legend.title=element_text(size=rel(1.4))) +
      scale_fill_manual(name=stFile$Type[i], values=c(blue, blue2, red, red2, orange2)) +
      geom_boxplot(outlier.size = 1) +
      labs(x="Months", y=ylabel) +
#       scale_y_continuous(limits = limit) +
      theme(axis.title.y = element_text(size = rel(0.8))) +
      facet_grid(~Month,scales="free_y", drop=T, labeller=f_labeller)
    
    i <- 14
    classId <- paste("X", stFile$Cell_ID_prec_rsds[i], sep="")
    allDataSel <- allData[which(allData$variable == classId),]
    
    p5 <- ggplot(data=allDataSel, aes(Dataset, value, fill=Dataset)) + 
      theme(panel.background = element_rect(fill = 'gray92'), axis.text.x=element_blank(), axis.ticks=element_blank(), legend.title=element_text(size=rel(1.4))) +
      scale_fill_manual(name=stFile$Type[i], values=c(blue, blue2, red, red2, orange2)) +
      geom_boxplot(outlier.size = 1) +
      labs(x="Months", y=ylabel) +
#       scale_y_continuous(limits = limit) +    
      theme(axis.title.y = element_text(size = rel(0.8))) +
      facet_grid(~Month,scales="free_y", drop=T, labeller=f_labeller)

    
    tiff(paste(iDir, "/plots/", var, "_std30yr_chart_v2.tif", sep=""), width=1000, height=1200, pointsize=8, compression='lzw',res=100)
    multiplot(p1, p2, p3, p4, p5, cols=1)
    dev.off()
    
    
#     ggsave(filename = paste(iDir, "/plots/", var, "_stdmonthly30yr_", stFile$Type[i], ".png", sep=""), width=12, height=3, dpi = 100)
    
#   }
  
}

stdComparisonMontly30yr <- function(var="rsds"){
  
  #Arguments
  iDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_extracts"
  stFile <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/sample_type_climates_without_1.csv"
  
  # Load data
  stFile <- read.csv(stFile)
  
  bc_his <- read.csv(paste(iDir, "/bc_monthly_", var, "_std_30yrhis.csv", sep=""),header=T)
  bc_fut <- read.csv(paste(iDir, "/bc_monthly_", var, "_std_30yrfut.csv", sep=""),header=T)
  wfd <- read.csv(paste(iDir, "/wfd_monthly_", var, "_std_30yr.csv", sep=""),header=T) 
#   gcm_his <- read.csv(paste(iDir, "/gcm_raw_monthly_", var, "_std_30yrhis.csv", sep=""),header=T) 
#   gcm_fut <- read.csv(paste(iDir, "/gcm_raw_monthly_", var, "_std_30yrfut.csv", sep=""),header=T) 
  
  # Modify and arrangement
#   gcm_his$X8019 <- NULL
#   gcm_fut$X8019 <- NULL
  bc_his$X8019 <- NULL
  bc_fut$X8019 <- NULL
  wfd <- cbind("wfd", wfd)
  colnames(wfd)[1] <- "GCM"
  
  #Colors
  blue="#1F78B4";blue2="#A6CEE3";green="#33A02C";green2="#B2DF8A";red="#E31A1C";red2="#FB9A99";orange="#FF7F00";orange2="#FDBF6F"
  
  if (var == "prec"){ylabel <- "STD Daily Rainfall (mm/day)"}
  if (var == "tmin"){ylabel <- "STD Daily Min. Temperature (°C)"}
  if (var == "tmax"){ylabel <- "STD Daily Max. Temperature (°C)"}
  if (var == "rsds"){ylabel <- "STD Daily Shortwave Sol. Radiation (W/m2)"}
  
  # Rename months
  f_names <- list("1"="January", "2"="February", "3"="March", "4"="April", "5"="May", "6"="June", "7"="July", "8"="August", "9"="September", "10"="October", "11"="November", "12"="December")
  f_labeller <- function(variable, value){return(f_names[value])}
  
  #Merge in one set
  allData <- cbind("Dataset"=c(rep("WFD",nrow(wfd)),
#                                rep("GCM His",nrow(gcm_his)), 
#                                rep("GCM Fut",nrow(gcm_fut)), 
                               rep("BC His",nrow(bc_his)),
                               rep("BC Fut",nrow(bc_fut))),
                   rbind(wfd, 
#                          gcm_his, 
#                          gcm_fut, 
                         bc_his, 
                         bc_fut))
  allData <- melt(allData,id=c("Dataset","GCM","Month"))
  
  ## Loop by location
  for (i in 1:nrow(stFile)){
    
    classId <- paste("X", stFile$Cell_ID_prec_rsds[i], sep="")
    
    allDataSel <- allData[which(allData$variable == classId),]
    
    ggplot(data=allDataSel, aes(Dataset, value, fill=Dataset)) + 
      theme(panel.background = element_rect(fill = 'gray92'), axis.text.x=element_blank(), axis.ticks=element_blank()) +
      scale_fill_manual(values=c(red, red2, blue, blue2, orange2)) +
      geom_boxplot() +
      labs(x="Months", y=ylabel, title=paste(stFile$Type[i])) +
      facet_grid(~Month,scales="free_y", drop=T, labeller=f_labeller)
    
    ggsave(filename = paste(iDir, "/plots/", var, "_stdmonthly30yr_", stFile$Type[i], ".png", sep=""), width=12, height=3, dpi = 100)
    
  }
  
}

changesLineChart <- function(var="rsds"){
  
  #Arguments
  iDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_extracts"
  stFile <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/sample_type_climates_without_1.csv"
  
  # Load data
  stFile <- read.csv(stFile)
  
  bc <- read.csv(paste(iDir, "/bc_monthly_", var, "_chg.csv", sep=""),header=T)
  
  # Modify and arrangement
  bc$X8019 <- NULL
    
  # Set dates
  dates_bc <- as.Date(as.yearmon((paste0(bc$Year,"-",bc$Month))))

  sel_bc <- cbind(bc, dates_bc)

  avg_bc <- aggregate(sel_bc[,4:length(sel_bc)], by=list(sel_bc$Year, sel_bc$Month), FUN="mean")
  dates_avg_bc <- as.Date(as.yearmon((paste0(avg_bc[,1],"-",avg_bc[,2]))))
  
  #Colors
  blue="#1F78B4";blue2="#A6CEE3";green="#33A02C";green2="#B2DF8A";red="#E31A1C";red2="#FB9A99";orange="#FF7F00";orange2="#FDBF6F"
  
  if (var == "prec"){ylabel <- "Precipitation Change (%)"; limit = c(-200,200)}
  if (var == "tmin"){ylabel <- "Min. Temperature Change (°C)"; limit = c(-7, 7)}
  if (var == "tmax"){ylabel <- "Max. Temperature Change (°C)"; limit = c(-7, 7)}
  if (var == "rsds"){ylabel <- "Shortwave Sol. Radiation \n Change (W/m2)"; limit = c(-50, 50)}
  
  ## Loop plots
  
  #   for (i in 1:nrow(stFile)){
  
  i <- 1
  classId <- paste("X", stFile$Cell_ID_prec_rsds[i], sep="")
  p1 <- ggplot() + 
    geom_point(data=sel_bc, aes_string("dates_bc", paste(classId)), colour=green2, fill=green , size=2) +
    geom_line(data=avg_bc, aes_string("dates_avg_bc", paste(classId)), colour=green, size=1) +
    theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.ticks.x=element_blank(), plot.title = element_text(hjust = 0.97, vjust=-5), axis.title.y = element_text(size = rel(0.8))) +
    scale_y_continuous(limits = limit) +
    ggtitle(stFile$Type[i]) +
    labs(x="Date (months)", y=ylabel)
  
  i <- 2
  classId <- paste("X", stFile$Cell_ID_prec_rsds[i], sep="")
  p2 <- ggplot() + 
    geom_point(data=sel_bc, aes_string("dates_bc", paste(classId)), colour=green2, fill=green , size=2) +
    geom_line(data=avg_bc, aes_string("dates_avg_bc", paste(classId)), colour=green, size=1) +
    theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.ticks.x=element_blank(), plot.title = element_text(hjust = 0.97, vjust=-5), axis.title.y = element_text(size = rel(0.8))) +
    scale_y_continuous(limits = limit) +
    ggtitle(stFile$Type[i]) +
    labs(x="Date (months)", y=ylabel)
  
  i <- 5
  classId <- paste("X", stFile$Cell_ID_prec_rsds[i], sep="")
  p3 <- ggplot() + 
    geom_point(data=sel_bc, aes_string("dates_bc", paste(classId)), colour=green2, fill=green , size=2) +
    geom_line(data=avg_bc, aes_string("dates_avg_bc", paste(classId)), colour=green, size=1) +
    theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.ticks.x=element_blank(), plot.title = element_text(hjust = 0.97, vjust=-5), axis.title.y = element_text(size = rel(0.8))) +
    scale_y_continuous(limits = limit) +
    ggtitle(stFile$Type[i]) +
    labs(x="Date (months)", y=ylabel)
  
  
  
  i <- 12
  classId <- paste("X", stFile$Cell_ID_prec_rsds[i], sep="")
  p4 <- ggplot() + 
    geom_point(data=sel_bc, aes_string("dates_bc", paste(classId)), colour=green2, fill=green , size=2) +
    geom_line(data=avg_bc, aes_string("dates_avg_bc", paste(classId)), colour=green, size=1) +
    theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.ticks.x=element_blank(), plot.title = element_text(hjust = 0.97, vjust=-5), axis.title.y = element_text(size = rel(0.8))) +
    scale_y_continuous(limits = limit) +
    ggtitle(stFile$Type[i]) +
    labs(x="Date (months)", y=ylabel)
  
  i <- 14
  classId <- paste("X", stFile$Cell_ID_prec_rsds[i], sep="")
  p5 <- ggplot() + 
    geom_point(data=sel_bc, aes_string("dates_bc", paste(classId)), colour=green2, fill=green , size=2) +
    geom_line(data=avg_bc, aes_string("dates_avg_bc", paste(classId)), colour=green, size=1) +
    theme(panel.background = element_rect(fill = 'gray92'), plot.title = element_text(hjust = 0.97, vjust=-5), axis.title.y = element_text(size = rel(0.8))) +
    scale_y_continuous(limits = limit) +
    ggtitle(stFile$Type[i]) +
    labs(x="Date (months)", y=ylabel)
  
  tiff(paste(iDir, "/plots/", var, "_chg_line_chart.tif", sep=""), width=1000, height=1200, pointsize=8, compression='lzw',res=100)
  multiplot(p1, p2, p3, p4, p5, cols=1)
  dev.off()
  
  
}
