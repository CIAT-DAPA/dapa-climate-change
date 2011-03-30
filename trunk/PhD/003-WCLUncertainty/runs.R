source("anuTesting.R")

rd <- "E:/climate-data-assessment/mask-srtm"
std <- "E:/climate-data-assessment/input-data"
od <- "E:/climate-data-assessment/outputs/cleansing"

cs <- cleansing(anuDir="C:/anu/Anuspl43/bin", rDir=rd, stDir=std, oDir=od, vn="rain", ntiles=4, round=1, unix=F)

####Linux folders
source("tileCreation.R")

rd <- "/home/jramirez/climate-data-assessment/mask-srtm"
std <- "/home/jramirez/climate-data-assessment/input-data"
od <- "/home/jramirez/climate-data-assessment/mask-srtm"

createTiles(rd, std, od, vn="rain", ntiles=4, overlap=1500)

