## ----, echo = FALSE, message = FALSE-------------------------------------
library(humidr)

## ------------------------------------------------------------------------
elevation <- 265 # elevation (m)

## ------------------------------------------------------------------------
t_db <- 16.7 # dry-bulb temperauture (°C)
rh <- 0.96   # relative humidity (1)
p_sl <- 101380.0 # sea-level pressure (Pa)

## ------------------------------------------------------------------------
p_atm <- p_atm_z(z = elevation, p_sl = p_sl) # atmospheric pressure (Pa)
p_atm

## ------------------------------------------------------------------------
w <- w_t_rh(t = t_db, rh = rh, p_atm = p_atm)  # humidity ratio (1)
h_moist <- h_t_w(t = t_db, w = w)              # enthalpy (kJ/kg) 
t_wb <- t_wb(t = t_db, rh = rh, p_atm = p_atm) # wet-bulb temperature (°C)

w
h_moist
t_wb

## ------------------------------------------------------------------------
library(dplyr)

wx_today <- data.frame(
  t_db = c(25.0, 25.0, 22.8, 21.1, 16.7),
  rh = c(0.54, 0.54, 0.62, 0.68, 0.96),
  p_sl = c(101310.0, 101260.0, 101240.0, 101310.0, 101380.0)
)

wx_today <- 
  wx_today %>%
  mutate(
    w = w_t_rh(t_db, rh, p_atm),
    h_moist = h_t_w(t_db, w),
    t_wb = t_wb(t_db, rh, p_atm)
  )

## ----results='asis'------------------------------------------------------
knitr::kable(wx_today)

