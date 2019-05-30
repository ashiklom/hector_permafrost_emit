context("hector_with_params function")

test_that("Function works", {
  r1 <- hector_with_params(beta = 0.4)
  r2 <- hector_with_params(beta = 0.7)
  expect_s3_class(r1, "data.frame")
  expect_s3_class(r2, "data.frame")
  expect_true(all(
    subset(r1, variable == "Ca")[["value"]] >
      subset(r2, variable == "Ca")[["value"]]
  ))
})
