# reference:
#  2005 ASHRAE Handbook - Fundamentals (SI)
#  Chapter 6 - Psychrometrics
#
 
# .p_ws_c
# 
# Internal function used to calculate the saturation pressure
#
# @param t   numeric: temperature (°C)
# @param cf  numeric: set of coefficients from ASHRAE, 
#                     depending on temperature 
#
# @return    numeric: saturation pressure (Pa)
#
.p_ws_c <- function(t, cf){
  
  # convert to Kelvin
  t <- t + 273.15
  
  # determine the logarithm of the saturation pressure
  ln_p_ws <- cf[1]/t +
    ((((cf[6]*t) + cf[5])*t + cf[4])*t + cf[3])*t + cf[2] +
    cf[7]*log(t)
 
  # determine the saturation pressure
  p_ws <- exp(ln_p_ws)
  
}
  

# .h_evap 
# 
# Internal function
#
# Enthalpy of evaporation of water (kJ/kg)
# 
# @param t numeric temperature (°C)
# 
# @return numeric enthalpy of evaporation of water (kJ/kg)   
# 
.h_evap <- function(t) 4.186*t # kJ/kg


# .h_sub
#
# Internal function
# 
# enthalpy of sublimation of water (kJ/kg)
# 
# @param t numeric temperature (°C)
# 
# @return numeric enthalpy of sublimation of water (kJ/kg)   
# 
.h_sub <- function(t) -333.4 + 2.1*t # kJ/kg

# .error_wetbulb
# 
# given guess for saturation temperature, 
# plus temperature, relative humidity, pressure,
# determine error in energy balance
# 
# @param t_sat numeric saturation temperaure (degC) (guess for wet-bulb)
# @param t numeric temperature (degC)
# @param rh numeric relative humidity (1)
# @param p_atm numeric atmospheric pressure (Pa)
# 
.error_wetbulb <- function(t_sat, t, rh, p_atm=101325){
  
  h <- h_t_rh(t, rh, p_atm)
  dw <- w_t_rh(t_sat, 1, p_atm) - w_t_rh(t, rh, p_atm)
  h_change <- ifelse(t_sat < 0, .h_sub(t_sat), .h_evap(t_sat))
  h_sat <- h_t_rh(t_sat, 1, p_atm)
  
  resid <- h + dw*h_change - h_sat 
  
  return(resid)
}

# .t_wb_single
# 
# for a single set of conditions, determine wet-bulb temperature
# 
# @param t numeric temperature (degC)
# @param rh numeric relative humidity (1)
# @param p_atm numeric atmospheric pressure (Pa)
# 
# @return numeric wet-bulb temperature (degC)
# 
.t_wb_single <- function(t, rh, p_atm = 101325){
  
  if (any(is.na(c(t, rh, p_atm)))) return (NA)
  
  t_wb <- uniroot(.error_wetbulb, t=t, rh=rh, lower = -100, upper = 65)$root
  
  return(t_wb)
  
}


