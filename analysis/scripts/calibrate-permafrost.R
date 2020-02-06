kessler_m <- 0.172
kessler_b <- 0.8 * kessler_m + 1
kessler_line <- function(x) pmin(kessler_b - kessler_m * x, 1)
cdfun <- function(x, mu, sig) plnorm(x, mu, sig, lower.tail = FALSE)
pfit <- function(pars) {
  mu <- pars[1]
  sig <- pars[2]
  x <- seq(0.01, 6, 0.1)
  yline <- kessler_line(x)
  yfun <- cdfun(x, mu, sig)
  ## sum((yline - yfun) ^ 2 * (1 / x^1.8))
  sum((yline - yfun) ^ 2)
}
fit <- optim(c(3.5, 2), pfit)
curve(kessler_line(x), 0, 6, col = "blue", ylim = c(0, 1))
curve(cdfun(x, fit$par[1], fit$par[2]), 0, 6, add = TRUE)
abline(h = c(0, 1), lty = 2)
