# reference:
#  2005 ASHRAE Handbook - Fundamentals (SI)
#  Chapter 6 - Psychrometrics
#

#' p_atm_z
#' 
#' Pressure as function of elevation/altitude and sea-level pressure
#' 
#' Pressure at elevation depends upon the height above sea level and the 
#' pressure at sea level. In weather reports, generally, sea-level pressure is given.
#' 
#' Reference:  2005 ASHRAE Handbook - Fundamentals (SI) ch.6 (3)
#' 
#' @param z       numeric: elevation/altitude (m)
#' @param p_sl    numeric: pressure at sea level, per weather report (Pa), 
#'                         default 101235 Pa
#' 
#' @return        numeric pressure (Pa)
#' @export
#' 
p_atm_z <- function(z, p_sl = 101325) {
  p_sl * (1 - 2.25577e-5 * z)^5.2559 
}

#' p_ws_t
#' 
#' Saturation pressure of water in air as function of temperature (°C)
#' 
#' The saturation pressure of water is the pressure at or above which water
#' would condense from from the air. It is a function of temperature only.
#' 
#' For this function, valid temperature inputs are between -100 and 200 °C.
#' 
#' Reference:  2005 ASHRAE Handbook - Fundamentals ch.6 (5), (6)
#' 
#' @param t     numeric: temperature, -100 <= t <= 200 (°C)
#' 
#' @return      numeric: saturation pressure of water (Pa)
#' @export
#' 
p_ws_t <- function(t){
 
  # validate the input
  if (any(!(-100 <= t & t <= 200)))
    stop("Temperature out of range: -100 <= t <= 200")  
  
  # define sets of coefficients 
  cf_cold <- c(-5.6745359e+03,
               6.3925247e+00,
               -9.6778430e-03,
               6.2215701e-07,
               2.0747825e-09,
               -9.4840240e-13,
               4.1635019e+00)
  
  cf_warm <- c(-5.8002206e+03,
               1.3914993e+00,
               -4.8640239e-02,
               4.1764768e-05,
               -1.4452093e-08,
               0,
               6.5459673e+00)
    
  # determine the saturation vapor pressure,
  #   use the set of coefficients according to the temperature
  p_ws <- ifelse(t < 0, .p_ws_c(t, cf_cold), .p_ws_c(t, cf_warm))
  
  p_ws  
}

#' w_pw
#' 
#' Humidity ratio as function of vapor pressure, and atmospheric pressure.
#' 
#' The humudity ratio is the ratio mass of water vapor to the mass of dry air. 
#'
#' @param p_w   numeric: vapor pressure of water (Pa)
#' @param p_atm numeric: atmospheric pressure (Pa), default 101235 Pa
#' 
#' Reference:  2005 ASHRAE Handbook - Fundamentals ch.6 (3)
#' 
#' @return numeric: humidity ratio (1)
#' @export
#' 
w_pw <- function(p_w, p_atm = 101325) 0.62198 * p_w / (p_atm - p_w)

#' w_t_rh
#' 
#' Humidity ratio as function of (dry-bulb) temperature and relative humidity
#' 
#' The humudity ratio is the ratio mass of water vapor to the mass of dry air. 
#' 
#' @param t       numeric: temperature (°C)
#' @param p_atm   numeric: atmospheric pressure (Pa), default 101235 Pa
#' 
#' @return numeric: humidity ratio (1)
#' @export
#' 
w_t_rh <- function(t, rh, p_atm = 101325){
  
  # saturation vapor pressure is a function of temperature
  p_ws <- p_ws_t(t)
  
  # find the vapor pressure using the realtive humidity
  #   relative humidity is the ratio of vapor pressure to the 
  #   saturation vapor pressure, at a given temperature
  p_w <- rh*p_ws
  
  # humidity ratio depends on the vapor pressure, and the atmospheric pressure
  w <- w_pw(p_w, p_atm)
  
  w
}

#' h_t_w
#' 
#' Enthalpy of moist air as function of temperature and humidity ratio
#' 
#' The reference conditions, where enthalpy is 0 kJ/kg, 
#' are zero-moisture air at 0 °C.
#' 
#' @param t   numeric: temperaure (°C)
#' @param w   numeric: humidity ratio (1)
#' 
#' @return    numeric: enthalpy of dry air (kJ/kg)
#' @export
#' 
h_t_w <- function(t, w) 1.006*t + w*(2501 + 1.86*t) 

#' h_t_rh
#' 
#' The reference conditions, where enthalpy is 0 kJ/kg, 
#' are zero-moisture air at 0 °C.
#' 
#' @param t     numeric: temperaure (°C)
#' @param rh    numeric: relative humidity (1)
#' @param p_atm numeric: atmospheric pressure (Pa)
#' 
#' @return      numeric: enthalpy of dry air (kJ/kg)
#' @export
#' 
h_t_rh <- function(t, rh, p_atm = 101325){
  w <- w_t_rh(t, rh, p_atm)
  h <- h_t_w(t, w)
  
  h
} 

#' t_wb
#' 
#' given a set of temperatures, relative humidities, pressures,
#' determine wet-bulb temperatures
#' 
#' @param t numeric temperature (°C)
#' @param rh numeric relative humidity (1)
#' @param p_atm numeric atmospheric pressure (Pa)
#' 
#' @return numeric wet-bulb temperature (°C)
#' 
#' @export
#' 
t_wb <- function(t, rh, p_atm = 101325){
  
  t_wb <- mapply(.t_wb_single, t = t, rh = rh, p_atm = p_atm)
  
  return(t_wb)
}
