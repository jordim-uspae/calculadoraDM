test_that("calc_u_marginalitat calcula l'excés correctament", {
  expect_equal(calc_u_marginalitat(100, 120), 0.2)
  expect_equal(calc_u_marginalitat(100, 90), 0)
  expect_true(is.na(calc_u_marginalitat(0, 120)))
})

test_that("HA_hospitalitzacions factura al 100% per sota del llindar minim", {
  expect_equal(HA_hospitalitzacions(QC = 100, QF = 70, reingressos = 0, tarifa = 500), 35000)
})

test_that("HA_hospitalitzacions és vectoritzada", {
  res <- HA_hospitalitzacions(QC = c(100, 100), QF = c(70, 130), reingressos = c(0, 0), tarifa = c(500, 500))
  expect_length(res, 2)
})
