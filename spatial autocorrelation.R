#test for spatial dependence in the residuals
library(rethinking)
library(tigris)
library(spdep)

#get residuals from regression (regression and figure 3 file)
mu_m <- link(m)
m_mean <- apply( mu_m, 2, mean)
m_resid <- d$S-m_mean
d$m_resid <- m_resid

#download counties shapefile and merge
sf_county <- counties(cb=T, year=2020 )

D <- merge(sf_county,d, by.x=c("GEOID"), by.y=c("fips"), all=FALSE)

#get neighbors for queen contiguity and convert to listw object
nb <- poly2nb(D, queen = TRUE) 
nbw <- nb2listw(nb, style = "W",zero.policy=TRUE)

#apply the global Moran's test 
moran.test(D$m_resid, nbw,
                     alternative = "greater")
