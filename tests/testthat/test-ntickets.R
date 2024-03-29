test_that("N = 200 test", {
  out <- ntickets(N=200,gamma = 0.02, p = 0.95)
  expect_equal(out$nd           , 205)
  expect_equal(round(out$nc, 2) , 204.32)
  expect_equal(out$N            , 200)
  expect_equal(out$p            , .95)
  expect_equal(out$gamma        , 0.02)
})

test_that("N = 400 test", {
  out <- ntickets(N=400,gamma = 0.02, p = 0.95)
  expect_equal(out$nd           , 412)
  expect_equal(round(out$nc, 2) , 412.02)
  expect_equal(out$N            , 400)
  expect_equal(out$p            , .95)
  expect_equal(out$gamma        , 0.02)
})
