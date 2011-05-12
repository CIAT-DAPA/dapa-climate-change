ccafs.dissimilarity <- function(cdata=cdata, params=params, new.direction=NA,new.x=NA,new.y=NA, new.growing.season=NA) {
  
  # overwrite direction
  if (!is.na(new.direction)) {
   params$direction <- new.direction
  }
  
  # create roll
  roll.v <- c()
  months <- 1:params$ndivisions
  for (i in 1:length(months)) roll.v <- c(roll.v,(months[c(i:length(months),0:(i-1))]))  
  roll <- matrix(data=roll.v,ncol=length(months),byrow=T)
  
  # cut roll to the actual growin period
  roll <- roll[,params$growing.season]
  
  if (!params$across.year) roll <- roll[1,,drop=FALSE]
  
  # overwrite default x and y
  if (!is.na(new.x) & !is.na(new.y)) {
   params$x <- new.x
   params$y <- new.y
  }
 
  if (!is.na(new.growing.season)) {
   params$growing.season <- new.growing.season
  }
 
  
  ## extract reference values
  # to save the reference values also into the parameters
  params$ref_values <- ccafs.make.ref(type="training",cdata=ccafs.data, params=ccafs.params, new.x=NA,new.y=NA, new.growing.season=NA)
  params$ref_weights <- ccafs.make.ref(type="weights",cdata=ccafs.data, params=ccafs.params, new.x=NA,new.y=NA, new.growing.season=NA)

  if (params$direction=="backwd" | params$direction=="backward") {

      results <- lapply(2:(length(params$gcms)+1),function(x) calc.dis(roll=roll,params=params,cdata,base=x,project=1)) # problem with no default value for delta
    
  } else if (params$direction=="forwd" | params$direction=="forward"){
    
    results <- lapply(2:(length(params$gcms)+1),function(x) calc.dis(roll=roll,params=params,cdata,base=1,project=x)) # problem with no default value for delta
    
  } else if (params$direction=="current") {
    results <- calc.dis(roll=roll,params=params,cdata,base=1,project=1) # problem with no default value for delta
    
  } else { stop("no directions was chosen") }
  
  return(results)
}
