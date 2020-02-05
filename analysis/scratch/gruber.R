# Based on:
# Gruber, S. (2012). Derivation and analysis of a high-resolution estimate of
# global permafrost zonation. The Cryosphere, 6(1), 221–233.
# http://dx.doi.org/https://doi.org/10.5194/tc-6-221-2012
 
mu <- 4.38
sig2 <- 6.43
curve(1 - pnorm(x, -mu, sqrt(sig2)), -12, 2,
      xlab = expression(MAAT ~ (degree * C)),
      ylab = "Permafrost area fraction")
