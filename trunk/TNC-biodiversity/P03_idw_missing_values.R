idw.coast <- function(df, missing=-9999) {

   # df - data.frame with thre columns: x,y,z
   # missing - value that indicates interpolation is required
   
   require(gstat)

   names(df) <- c("x","y","z")
   df.ok <- df[df[,3]!=missing,]
   df.m <- df[df[,3]==missing,]

   coordinates(df.ok) = ~x+y
   coordinates(df.m) = ~x+y

   filled <- idw(z~1,df.ok, df.m, nmax=3)

   df.ok <- as.data.frame(df.ok)
   filled <- as.data.frame(filled)

   filled <- filled[,1:3]
   names(filled) <- names(df.ok)

   df.r <- rbind(df.ok, filled)

   return(df.r)
   
}
