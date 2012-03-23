#Julian Ramirez-Villegas
#2012

#Estimates potential evapotranspiration based on the Priestley-Taylor equation
#based on Challinor et al. (2004)

#constants
albedo <- 0.2
vpd_cte <- 0.7

#soil heat flux parameters
a_eslope=611.2
b_eslope=17.67
c_eslope=243.5

#transform solar radiation from W/m2 to MJ/m2
#DAILYRAD(K)=DAILYRAD(K)*60*60*24/1000000

peest <- function(srad,tmin,tmax) {
  tmean <- (tmin+tmax)/2
  
  #net radiation
  rn = (1-albedo) * srad
  
  #soil heat flux
  eslope=a_eslope*b_eslope*c_eslope/(tmean+c_eslope)^2*exp(b_eslope*tmean/(tmean+c_eslope))
  
  #estimate vpd
  esat_min=0.61120*exp((17.67*tmin)/(tmin+243.5))     
  esat_max=0.61120*exp((17.67*tmax)/(tmax+243.5))     
  vpd=vpd_cte*(esat_max-esat_min) #kPa
  
  #Priestley-Taylor
  pt_const=1.26
  pt_fact=1
  vpd_ref=1
  psycho=62
  rho_w=997
  rlat_ht=2.26E6
  
  pt_coef=pt_fact*pt_const
  pt_coef = 1 + (pt_coef-1) * vpd / vpd_ref
  
  #*10^6? To convert fluxes MJ to J
  #rlat_ht? Latent heat flux to water flux
  #100/rho_w? Kg/m^2 to cm
  et_max=pt_coef * rn * eslope/(eslope+psycho) * 10^6 / rlat_ht * 100/rho_w
  return(et_max)
}


#the two functions below estimate the ea/ep
#based on Jones (1987)
#ea/ep: actual to potential evapotranspiration ratio
eabyep_calc <- function(soilcp=100,cropfc=1,avail=50,rain,evap) {
  avail <- min(avail,soilcp)
  eratio <- eabyep(soilcp,avail,rain,evap)
  demand <- eratio*cropfc*evap
  result <- avail + rain - demand
  runoff <- result - soilcp
  avail <- min(c(soilcp,result))
  avail <- max(c(avail,0))
  runoff <- max(runoff,0)
  
  out <- data.frame(AVAIL=avail,ERATIO=eratio,RAIN=rain,EVAP=evap,RUNOFF=runoff)
  
  return(out)
}

#ea/ep function
eabyep <- function(soilcp,avail) {
  percwt <- min(c(100,avail/soilcp*100))
  percwt <- max(c(1,percwt))
  eratio <- min(c(percwt/(97-3.868*sqrt(soilcp)),1))
  return(eratio)
}

