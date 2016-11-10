load('D:/Tobackup/BID/matrices_cultivo/Wheat_secano.Rdat')
ind.next = which(crop_secano$mirca.end < crop_secano$mirca.start)
duration = crop_secano$mirca.end - crop_secano$mirca.start
duration[ind.next] = duration[ind.next] + 365

par(mai=c(0.5,0.5,0.5,0.5))


sowDates = sort(unique(crop_secano$mirca.start))
color_scale = colorRampPalette(c('red','orange','green','blue','purple'), space="rgb")(length(sowDates)) 

plot(Map_LatinAmerica)
for (j in 1:length(sowDates))  {
  ind.sow = which(crop_secano$mirca.start==sowDates[j])
  points(crop_secano$x[ind.sow],crop_secano$y[ind.sow],pch=20,cex=0.6,col=color_scale[j])
}
legend('bottomleft',as.character(sowDates),col=color_scale,pch=20,cex=1.25)
title('Trigo secano')
