test_that("calc_u_marginalitat calcula l'excés correctament", {
  expect_equal(calc_u_marginalitat(100, 120), 0.2)
  expect_equal(calc_u_marginalitat(100, 90), 0)
  expect_true(is.na(calc_u_marginalitat(0, 120)))
})

test_that("hospi factura al 100% per sota del llindar minim", {
  expect_equal(hospi(QC = 100, tarifa = 500, QF = 70, reingressos = 0), 35000)
})

test_that("hospi és vectoritzada", {
  res <- hospi(QC = c(100, 100), tarifa = c(500, 500), QF = c(70, 130), reingressos = c(0, 0))
  expect_length(res, 2)
})
