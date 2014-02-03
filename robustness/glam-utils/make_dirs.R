#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#create input directories for a glam run
create_dirs <- function(glam_dir) {
  if (!file.exists(glam_dir)) {dir.create(glam_dir,recursive=T)}
  if (!file.exists(paste(glam_dir,"/inputs",sep=""))) {dir.create(paste(glam_dir,"/inputs",sep=""))}
  if (!file.exists(paste(glam_dir,"/output",sep=""))) {dir.create(paste(glam_dir,"/output",sep=""))}
  if (!file.exists(paste(glam_dir,"/output/daily",sep=""))) {dir.create(paste(glam_dir,"/output/daily",sep=""))}
  if (!file.exists(paste(glam_dir,"/inputs/ascii",sep=""))) {dir.create(paste(glam_dir,"/inputs/ascii",sep=""))}
  if (!file.exists(paste(glam_dir,"/inputs/ascii/soil",sep=""))) {dir.create(paste(glam_dir,"/inputs/ascii/soil",sep=""))}
  if (!file.exists(paste(glam_dir,"/inputs/ascii/sow",sep=""))) {dir.create(paste(glam_dir,"/inputs/ascii/sow",sep=""))}
  if (!file.exists(paste(glam_dir,"/inputs/ascii/obs",sep=""))) {dir.create(paste(glam_dir,"/inputs/ascii/obs",sep=""))}
  if (!file.exists(paste(glam_dir,"/inputs/ascii/wth",sep=""))) {dir.create(paste(glam_dir,"/inputs/ascii/wth",sep=""))}
  return(glam_dir)
}


