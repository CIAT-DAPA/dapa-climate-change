#function to determine if a year is leap year
#taken from https://www.r-bloggers.com/leap-years/

is.leapyear=function(year){
  #http://en.wikipedia.org/wiki/Leap_year
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}
