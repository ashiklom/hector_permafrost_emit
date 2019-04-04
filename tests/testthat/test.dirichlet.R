context("Dirichlet distribution")

alpha <- rep(0.5, 3)
draws <- rdirichlet(1000, alpha)

test_that("Dirichlet draws are positive and always sum to 1.", {
  expect_true(all(draws < 1))
  expect_true(all(draws > 0))
  expect_true(all.equal(rowSums(draws), rep(1, nrow(draws))))
})

test_that("Dirichlet densities are finite.", {
  d_dens <- ddirichlet(draws, alpha)
  expect_true(is.finite(mean(d_dens)))
})
