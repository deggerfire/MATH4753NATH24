test_that("Check mu", {
  expect_equal(myncurve(mu=10,sigma=5, a=6)$mu, 10)
})

test_that("Check sigma", {
  expect_equal(myncurve(mu=10,sigma=5, a=6)$sigma, 5)
})

test_that("Check prob", {
  expect_equal(myncurve(mu=10,sigma=5, a=6)$prob, 0.2119)
})
