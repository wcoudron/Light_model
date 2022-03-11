#####################################
#Calculation Modules (Spitters)
#####################################
highest_point_sun <- function(lon, winsumT, timezone){
  highest_point <- 12*60 - 4*lon + winsumT*60 + timezone*60
  return(highest_point) #output in min
}

EqnOfTime <- function(day){
  w <- 2*pi*(day - 1)/365
  return(229.18*(0.000075+0.001858*cos(w)-0.032077*sin(w)-0.014615*cos(2*w)-0.04089*sin(2*w))) #in minutes
}

calcSpittersExtraTer <- function(timeinterval, lon, lat, winsumT, timezone, day, hour){
  HPS <- (highest_point_sun(lon, winsumT, timezone)-EqnOfTime(day))/60
  decr <- asin(0.39795 * cos(0.2163108 + 2*atan(0.9671396 * tan(0.00860*(day - 186)))))
  D <- (24 - 24/pi * acos((sin(0.8333*pi/180) + sin(lat * pi/180)*sin(decr))/(cos(lat*pi/180)*cos(decr)))) #day length in hours
  n <- round(D/timeinterval) #number of light nodes, or time interval for which light calculations are performed
  latr <- lat*RAD #latitude in radians
  tsr <- HPS - D/2 #hour of sunrise
  tss <- HPS + D/2 #hour of sunset
  delt = D/n*3600 #time interval in seconds for 1 calculation period (is actually same as time interval)
  S0 = 1367*(1+0.033*cos(2*pi*(day+10)/365)) #extra-terrestrial irradiance joules per square meter per second
  
  part_day <- as.integer(hour/timeinterval - tsr)
  
  tang = 2*pi/360*(tsr+((part_day-1)+0.5)*delt/3600-HPS)*15
  beta = asin(sin(latr)*sin(decr)+cos(latr)*cos(decr)*cos(tang))
  S0d = S0*sin(beta)*delt
  
  S0d <- S0d/1000000 #daily extra terrestrial radiation from J/m2/s to MJ/m2/d
  return(S0d)
}

calcSpittersSkyTrans <- function(timeinterval, lon, lat, winsumT, timezone, day, hour, Radiation){
  HPS <- (highest_point_sun(lon, winsumT, timezone)-EqnOfTime(day))/60
  decr <- asin(0.39795 * cos(0.2163108 + 2*atan(0.9671396 * tan(0.00860*(day - 186)))))
  D <- (24 - 24/pi * acos((sin(0.8333*pi/180) + sin(lat * pi/180)*sin(decr))/(cos(lat*pi/180)*cos(decr)))) #day length in hours
  n <- round(D/timeinterval) #number of light nodes, or time interval for which light calculations are performed
  latr <- lat*RAD #latitude in radians
  tsr <- HPS - D/2 #hour of sunrise
  tss <- HPS + D/2 #hour of sunset
  delt = D/n*3600 #time interval in seconds for 1 calculation period (is actually same as time interval)
  S0 = 1367*(1+0.033*cos(2*pi*(day+10)/365)) #extra-terrestrial irradiance joules per square meter per second
  
  part_day <- as.integer(hour/timeinterval - tsr)
  
  tang = 2*pi/360*(tsr+((part_day-1)+0.5)*delt/3600-HPS)*15
  beta = asin(sin(latr)*sin(decr)+cos(latr)*cos(decr)*cos(tang))
  S0d = S0*sin(beta)*delt
  
  S0d <- S0d/1000000 #daily extra terrestrial radiation from J/m2/s to MJ/m2/d
  SkyTrans = Radiation/S0d#Radiation/S0d(1.47-(0.847-1.61*sin(beta)+1.04*sin(beta)^2))/1.66 #((1.47-(0.847-1.61*sin(beta)+1.04*sin(beta)^2))/1.66*S0d)
  return(SkyTrans)
}

calcDiffAndDirectHourly <- function(timeinterval, lon, lat, winsumT, timezone, day, hour, Radiation) {
  HPS <- (highest_point_sun(lon, winsumT, timezone)-EqnOfTime(day))/60
  decr <- asin(0.39795 * cos(0.2163108 + 2*atan(0.9671396 * tan(0.00860*(day - 186)))))
  D <- (24 - 24/pi * acos((sin(0.8333*pi/180) + sin(lat * pi/180)*sin(decr))/(cos(lat*pi/180)*cos(decr)))) #day length in hours
  n <- round(D/timeinterval) #number of light nodes, or time interval for which light calculations are performed
  latr <- lat*RAD #latitude in radians
  tsr <- HPS - D/2 #hour of sunrise
  tss <- HPS + D/2 #hour of sunset
  delt = D/n*3600 #time interval in seconds for 1 calculation period (is actually same as time interval)
  S0 = 1367*(1+0.033*cos(2*pi*(day+10)/365)) #extra-terrestrial irradiance joules per square meter per second
  
  part_day <- as.integer(hour/timeinterval - tsr)
  
  tang = 2*pi/360*(tsr+((part_day-1)+0.5)*delt/3600-HPS)*15
  beta = asin(sin(latr)*sin(decr)+cos(latr)*cos(decr)*cos(tang))
  S0d = S0*sin(beta)*delt
  
  S0d <- S0d/1000000 #daily extra terrestrial radiation from J/m2/s to MJ/m2/d
  #calculation based on de Jong (1980), (eq 20.Spitters et al., 1986)
  R = 0.847-1.61*sin(beta)+1.04*sin(beta)^2
  K = (1.47-R)/1.66
  if(Radiation/S0d < 0.22) {
    return(1)
  } else if(Radiation/S0d < 0.35) {
    return(1-6.4*(Radiation/S0d-0.22)^2)
  } else if(Radiation/S0d < K) {
    return(1.47-1.66*Radiation/S0d)
  } else {
    return(R)
  }
}

 calcSpittersDiff <- function(timeinterval, lon, lat, winsumT, timezone, day, hour, Radiation) { #daily diffuse radiation
   HPS <- (highest_point_sun(lon, winsumT, timezone)-EqnOfTime(day))/60

   decr <- asin(0.39795 * cos(0.2163108 + 2*atan(0.9671396 * tan(0.00860*(day - 186)))))
   latr <- lat*RAD #latitude in radians
   a <- sin(latr)*sin(decr)
   b <- cos(latr)*cos(decr)
   
   D <- (24 - 24/pi * acos((sin(0.8333*pi/180) + sin(lat * pi/180)*sin(decr))/(cos(lat*pi/180)*cos(decr)))) #day length in hours
   n <- round(D/timeinterval) #number of light nodes, or time interval for which light calculations are performed   tsr <- HPS - D/2 #hour of sunrise
   tsr <- HPS - D/2 #hour of sunrise
   tss <- HPS + D/2 #hour of sunset
   delt = D/n*3600 #time interval in seconds for 1 calculation period (is actually same as time interval)
   S0 = 1367*(1+0.033*cos(2*pi*(day+10)/365)) #extra-terrestrial irradiance joules per square meter per second
   
   part_day <- as.integer(hour/timeinterval - tsr)
   
   tang = 2*pi/360*(tsr+((part_day-1)+0.5)*delt/3600-HPS)*15
   beta = asin(sin(latr)*sin(decr)+cos(latr)*cos(decr)*cos(tang))
   S0d = S0*sin(beta)*delt
   
   S0d <- S0d/1000000 #daily extra terrestrial radiation from J/m2/s to MJ/m2/d
   Sdfd = calcDiffAndDirectHourly(timeinterval, lon, lat, winsumT, timezone, day, hour, Radiation)*calcSpittersSkyTrans(timeinterval, lon, lat, winsumT, timezone, day, hour, Radiation)*S0d #diffuse radiation on a horizontal plane in MJ/m2/d
 return(Sdfd)
}