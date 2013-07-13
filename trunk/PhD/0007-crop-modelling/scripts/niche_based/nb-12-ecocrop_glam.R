#Julian Ramirez-Villegas
#July 2013
#UoL / CCAFS / CIAT

#(1) The suitability scale was divided into 11 classes: values equal to 
#zero and 10 equally-spaced classes (of 10 % range each class). 
#A scatter plot with mean suitability and yield values across classes was produced. 

#a. load list of glam runs
#b. load pot_yield_rfd.csv from ./ecg_analyses/exp-$/tables
#c. calculate time-ybar, normalise using y-min/(max-min)
#d. load the ecocrop runs and construct a data.frame with each of the selected ecocrop runs





#(2) The normalised yield scale was divided into 11 equally-spaced classes. 
#A scatter plot with mean yield and suitability values was produced.

#(3) GLAM’s simulated potential yield was then regressed against suitability
#using a three types of regressions: (a) linear, (b) log-linear, where log(Y)
#is regressed against suitability, and (c) robust regression (Maronna et al., 2006).
#Robust regression was used in order to assess the influence of outliers that may
#arise from errors in the structure of either suitability models or GLAM so as to 
#be able to detect the yield-suitability signal more clearly (Serra-Diaz et al., 2013). 
#Residuals of these regressions were then regressed against climatological means 
#and variances of 17 intra-seasonal agro-meteorological indicators (AMIs, Table 7.4) 
#derived from GLAM’s simulated daily output.



