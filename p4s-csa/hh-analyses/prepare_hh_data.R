#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Oct 2015
stop("!")

#load libraries
library(foreign)

#working directory
wd <- "~/Leeds-work/p4s-csa/hh-analyses"

### To get in the data on field sizes in R:
#ecvmaas1_p2_en.sav, "s1" = section 1
#plots size info in ha in: data_land$as01q09/10000
#ms00q06: Completion of Household Questionnaire
#attr(data_comp,"variable.labels") to get questions in questionnaire
#menage: household number
#grappe: cluster
#as01q03: field number
#as01q05: parcel number
#as01q09: GPS measurement of parcel (m2)
data_land <- read.spss(paste(wd,"/hh_data/ecvmaas1_p2_en.sav",sep=""), use.value.labels=T, 
                       to.data.frame=T)
data_land <- data_land[which(!is.na(data_land$ms00q06)),]
data_land <- data_land[which(data_land$ms00q06 == "Complete Questionnaire"),]
data_land <- data_land[,c("hid","passage","grappe","menage","as01qa","as01q03","as01q04","as01q05","as01q09")]
names(data_land) <- c("hhid","wave","cluster","household_id","field_parcel_number","field_number",
                      "locality","parcel_number","parcel_area_m2")

#read the cleaned up data
hh_data <- read.csv(paste(wd,"/hh_data/LSMS_Niger_ResultsforRobertII.csv",sep=""))


### To get in the data on crop yields in R:
#ecvmaas2e_p2_en.sav, "s2e" = section 2E
#crops grown in data_crop$as02eq06
#crop yields in data_crop$as02eq07a
#crops yield unit in data_crop$as02eq07b
#crops sold in kg in data_crop$as02eq12c
#sales value per crop in FCFA in data_crop$as02eq13
data_crop <- read.spss(paste(wd,"/hh_data/ecvmaas2e_p2_en.sav",sep=""), use.value.labels=T, 
                     to.data.frame=T)
crop_yield_unit <- c('Kg','Gram','Cope','Bunch','Tiya','Tongolo','50kg sack','100kg sack',
                     'Basket','Unit','Planche','DK')
conversion_unit_to_kg <- c(1,0.001,0.1,10,0.1,0.1,50,100,5,1,0.1,0.1)

#convert production to kg
data_crop$as02eq07a_kg <- NA
for (i in 1:length(crop_yield_unit)) {
  #i <- 1
  index <- (data_crop$as02eq07b==crop_yield_unit[i] & !is.na(data_crop$as02eq07b))
  data_crop$as02eq07a_kg[index] <- data_crop$as02eq07a[index] * conversion_unit_to_kg[i]
}

data_crop <- data_crop[which(!is.na(data_crop$ms00q06)),]
data_crop <- data_crop[which(data_crop$ms00q06 == "Complete Questionnaire"),]
data_crop <- data_crop[,c("hid","passage","grappe","menage","as02eq0","as02eq01","as02eq02",
                          "as02eq03","as02eq04","as02eq06","as02eq07a","as02eq07a_kg","as02eq07b",
                          "as02eq07c","as02eq08","as02eq09","as02eq10")]
names(data_crop) <- c("hhid","wave","cluster","household_id","field_crop_number","field_number",
                      "locality","parcel_number","worked","crop","prod","prod_kg_conv","prod_unit",
                      "prod_kg_rep","lost_part","percent_lost","reason_lost")

#remove worked="No", and remove "worked" as field
data_crop <- data_crop[which(data_crop$worked == "Yes"),]
data_crop$worked <- NULL

#put 0 in prod_kg_rep and prod_kg_conv where percent_lost == 100
#put "Kg" where percent_lost == 100
data_crop$prod_kg_conv[which(data_crop$percent_lost == 100)] <- 0
data_crop$prod_kg_rep[which(data_crop$percent_lost == 100)] <- 0
data_crop$prod_unit[which(data_crop$percent_lost == 100)] <- "Kg"

#put 0 in prod_kg_rep and prod_kg_rep where prod == 0
data_crop$prod_kg_conv[which(data_crop$prod == 0)] <- 0
data_crop$prod_kg_rep[which(data_crop$prod == 0)] <- 0

#assign NA to values of parcel_area_m2 == 999999 | 999998 in data_land
data_land$parcel_area_m2[which(data_land$parcel_area_m2 == 999999)] <- NA
data_land$parcel_area_m2[which(data_land$parcel_area_m2 == 999998)] <- NA
data_land <- data_land[which(!is.na(data_land$parcel_area_m2)),]

#create fields in data_crop and data_land for merging, and merge
data_land$id_merge <- paste("hh",data_land$hhid,"_f",data_land$field_number,"_p",data_land$parcel_number,sep="")
data_crop$id_merge <- paste("hh",data_crop$hhid,"_f",data_crop$field_number,"_p",data_crop$parcel_number,sep="")
data_all <- merge(data_crop, data_land, by="id_merge", all.x=F, all.y=F)

#write data_land and data_crop
write.csv(data_crop,paste(wd,"/hh_data/data_crops_all.csv",sep=""),row.names=F)
write.csv(data_land,paste(wd,"/hh_data/data_land_all.csv",sep=""),row.names=F)
write.csv(data_all,paste(wd,"/hh_data/data_merged_all.csv",sep=""),row.names=F)


