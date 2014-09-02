\name{p_atm_z}
\alias{p_atm_z}
\title{p_atm_z}
\usage{
p_atm_z(z, p_sl = 101325)
}
\arguments{
  \item{z}{numeric: elevation/altitude (m)}

  \item{p_sl}{numeric: pressure at sea level, per weather
  report (Pa), default 101235 Pa}
}
\value{
numeric pressure (Pa)
}
\description{
Pressure as function of elevation/altitude and sea-level
pressure
}
\details{
Pressure at elevation depends upon the height above sea
level and the pressure at sea level. In weather reports,
generally, sea-level pressure is given.

Reference: 2005 ASHRAE Handbook - Fundamentals (SI) ch.6
(3)
}

