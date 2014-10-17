context("psychrometrics")

test_that("pressure-elevation", {
  # ASHRAE ch. 6, table 1
  expect_equal(p_atm_z(c(0, 500, 1000)), c(101325, 95461, 89875), tolerance=1.e-5 )
  expect_equal(p_atm_z(0, p_sl=100000), 100000, tolerance=1.e-5 )
})

test_that("saturation pressure", {
  # ASHRAE ch. 6, table 3
  expect_equal(p_ws_t(c(-10, 5, 20)), c(259.90, 872.5, 2338.8), tolerance=1.e-4 )
})

test_that("humidity ratio at p_std", {
  # ASHRAE ch. 6, table 2
  t <- c(-10, 5, 20)
  
  # saturation
  expect_equal(w_pw(p_ws_t(t)), c(1.6062e-3, 5.424e-3, 1.4758e-2), tolerance=5.e-3)
  expect_equal(w_t_rh(t, rh=1), c(1.6062e-3, 5.424e-3, 1.4758e-2), tolerance=5.e-3)
  
  # dry air
  expect_equal(w_t_rh(t, rh=0), c(0, 0, 0), tolerance=5.e-3)  
})

test_that("specific enthalpy at p_std", {
  # ASHRAE ch. 6, table 2
  t <- c(-10, 5, 20)
  
  # saturation
  expect_equal(h_t_rh(t, 1), c(-6.072, 18.639, 57.555), tolerance=5.e-3)
  
  # dry air
  expect_equal(h_t_rh(t, 0), c(-10.057, 5.029, 20.121), tolerance=5.e-3)
  
})

test_that("wet-bulb temperature at p_std", {
  # ASHRAE ch. 6, process - tougher to verify on web
  # http://www.srh.noaa.gov/epz/?n=wxcalc_rh
  t <- c(-10, 5, 20)
  
  # saturation (dry_bulb == wet_bulb)
  expect_equal(t_wb(t, 1), t, tolerance=5.e-3)
  
  # dry_air 
  expect_equal(t_wb(t, 0), c(-13.34, -3.16, 5.84), tolerance=5.e-3)
  
})

test_that("NA handling", {
  expect_true(is.na(p_ws_t(NA)))
  
  expect_true(is.na(t_wb(NA, 1)))
})

test_that("fail!", {
  expect_equal(0, 1)
})