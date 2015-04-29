library(ff)
# Jaime Tarapues
# Como mejorar el rendimiento de memoria cuando se trabaja con grandes conjuntos de datos en R
# Mas info: http://ff.r-forge.r-project.org/bit&ff2.1-2_WU_Vienna2010.pdf
# http://www.bytemining.com/wp-content/uploads/2010/08/r_hpc_II.pdf


# prueba:
a1=array(1:12, dim=c(3000,4000))
a2=matrix(1:12, 3,4, byrow=TRUE) 

b1=ff(1:12, dim=c(3000,4000)) 
b2=ff(1:12, dim=c(3,4), dimorder=c(2,1)) 


memory.limit()
print( object.size( a1 ) , units = "Mb" )
print( object.size( b1 ) , units = "Mb" )
# memory.limit(size=1800)
# memory.size()



#####################################################################################

# fi <- ff(vmode="integer", length=10)
# fb <- ff(vmode="byte", length=10)
# rb <- byte(10) # in R this is integer
# fb <- ff(rb)
# vmode(ri)
# vmode(fi)
# vmode(rb)
# vmode(fb)
# cbind(.rambytes, .ffbytes)[c("integer","byte"),]
# 
# 
# rf <- factor(levels= c("A","T","G","C"))
# length(rf) <- 10
# rf
# 
# frf <- ff(rf)
# length(frf) <- 1e8
# frf
# frf[11:1e8] <- NA
# ff(vmode="quad", length=1e8, levels=c("A","T","G","C"))
# ff(vmode="quad", length=10
#    , levels=c("A","B","C","D"), ordered=TRUE)
# ff(Sys.Date()+0:9, length=10)
# ff(Sys.time()+0:9, length=10)
# ff(0:9, ramclass="Date")
# ff(0:9, ramclass=c("POSIXt", "POSIXct"))
# str(ff(as.POSIXct(as.POSIXlt(Sys.time(), "GMT")), length=12))
# 
# 
# str(chunk(fb))
# args(chunk.default)
# args(chunk.ff_vector)
# getOption("ffbatchbytes") / 1024^2 / memory.limit()
# 
# 
# rd <- double(100)
# rd[] <- runif(100) 
# fd <- ff(vmode="double", length=1e8)
# system.time(
#   for (i in chunk(fd)) fd[i] <- runif(sum(i))
# )
# system.time(
#   s <- lapply( chunk(fd)
#                , function(i)quantile(fd[i], c(0.05, 0.95)) )
# )
# crbind(s)