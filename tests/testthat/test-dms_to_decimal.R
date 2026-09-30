test_that("coordinates in degrees, minutes and seconds are converted", {
  x <- c("22º22´55´´S", "53°45'W", "12 30 15.5 S", "S 8°30.5'", "43º40'49''O",
         "12d30m15sS", "114°39′0″E", "9º41´31´´ N", "-12°30'", "0º0'0''")
  expect_equal(dms_to_decimal(x),
               c(-22.381944, -53.75, -12.504306, -8.508333, -43.680278,
                 -12.504167, 114.65, 9.691944, -12.5, 0))
})

test_that("decimal degrees are kept", {
  expect_equal(dms_to_decimal(c("-55.602222", "12,5", "-12.97")), c(-55.602222, 12.5, -12.97))
  expect_equal(dms_to_decimal(-38.5), -38.5)
})

test_that("the hemisphere can be given for coordinates without it", {
  x <- c("22º22´55´´", "1º42´12´´", "-3°")
  expect_equal(dms_to_decimal(x), c(22.381944, 1.703333, -3))
  expect_equal(dms_to_decimal(x, type = "latitude", hemisphere = "S"),
               c(-22.381944, -1.703333, -3))
  expect_error(dms_to_decimal(x, hemisphere = "X"), "must be one of")
})

test_that("invalid coordinates are set to NA with a warning", {
  x <- c("60º6´61´´W", "48º18´60´´", "95°N", "texto", "12°30'W, 5", NA, "")
  expect_warning(r <- dms_to_decimal(x), "5 coordinate\\(s\\) could not be converted")
  expect_true(all(is.na(r)))
  expect_true(is.na(suppressWarnings(dms_to_decimal("100°", type = "latitude"))))
  expect_equal(dms_to_decimal("100°", type = "longitude"), 100)
})
