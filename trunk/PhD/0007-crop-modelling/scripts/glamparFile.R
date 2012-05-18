#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#Read in a dummy GLAM parameter file and create a new one based on a new parameter for
#running and optimising GLAM
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
source(paste(src.dir,"/glamParFile-functions.R",sep=""))

bDir <- "F:/PhD-work/crop-modelling/GLAM"

cropName <- "gnut"
cropDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")

parDir <- paste(cropDir,"/params",sep="")
dumFile <- paste(parDir,"/glam-r2-param-",tolower(cropName),"-dum.txt",sep="")

dumFile <- "D:/glam-r2-param-gnut-hyp.txt"

#get the model parameters
params <- GLAM_get_par(dumFile,retain=c("all"))

#now what i need to do is build a function to write the parameter file
#based on the object 'params'. This can easily be done if i have checks
#in the function for each bit of the parameter file that needs to be written
#(i.e. these need to be with proper names in the params list). If they're not
#then they should be loaded from a dummy default file

pf <- file("D:/glam-r2-param-gnut-test.txt",open="w")
cat("******* GLAM-R2 parameter file.                          Comments                                       *********\n",file=pf)

#yield gap parameter line
if (!"glam_param.ygp" %in% names(params)) {
  glam_param.ygp <- get_ygp(dumFile)
} else {
  glam_param.ygp <- params$glam_param.ygp
}

write_line(glam_param.ygp,con=pf,format="short")

#general simulation controls
if (!"glam_param.sim_ctr" %in% names(params)) {
  glam_param.sim_ctr <- get_sc(dumFile)
} else {
  glam_param.sim_ctr <- params$glam_param.sim_ctr
}

cat(paste(sprintf("%-12s","YGP_METH"),
          sprintf("%-11s",glam_param.sim_ctr$YGP_METH),
          sprintf("%-7d",glam_param.sim_ctr$MMNO),
          sprintf("%-9d",glam_param.sim_ctr$IMERF),
          sprintf("%-9d",glam_param.sim_ctr$ISHF),
          sprintf("%-9d",glam_param.sim_ctr$IUPT),"\n",
          sprintf("%-12s","NDSLA"),
          sprintf("%-11d",glam_param.sim_ctr$NDSLA),
          sprintf("%-7d",glam_param.sim_ctr$SLA_INI),
          sprintf("%-9d",glam_param.sim_ctr$ZSMAX),
          sprintf("%-9.1f",glam_param.sim_ctr$SMLON),
          sprintf("%-9s","x"),"\n",
          sprintf("%-12s","TETRS"),
          sprintf("%-11s",glam_param.sim_ctr$TETRS),
          sprintf("%-16s",glam_param.sim_ctr$RunID),
          sprintf("%-9d",glam_param.sim_ctr$IVMETH),
          sprintf("%-9d",glam_param.sim_ctr$IC02),"\n\n",
          sep=""),file=pf)
cat("*MODEL MANAGEMENT\n",file=pf)
cat("Name        Value      Meth   Min      Max      NVAL     Comments\n",file=pf)

close(pf)


