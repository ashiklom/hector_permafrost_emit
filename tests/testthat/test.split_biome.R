context("Run Hector with split biome")

test_that("Hector split_biome works.", {
  result <- hector_with_params(
    global.beta = 0.9,
    global.q10_rh = 1.8,
    core = split_biome("boreal", frac_veg = 0.2)
  )
  expect_s3_class(result, "data.frame")
  expect_true(!any(is.na(result[["value"]])))
})
