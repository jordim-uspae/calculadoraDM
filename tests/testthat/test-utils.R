test_that("round_excel arrodoneix com Excel", {
  round_excel <- calculadoraMeritada:::round_excel
  expect_equal(round_excel(984.7349999999999, 2), 984.73)
  expect_equal(round_excel(2.5, 0), 3)
  expect_equal(round_excel(-2.5, 0), -3)
})

test_that("num0 substitueix NA per 0", {
  num0 <- calculadoraMeritada:::num0
  expect_equal(num0(NA), 0)
  expect_equal(num0(5), 5)
})
