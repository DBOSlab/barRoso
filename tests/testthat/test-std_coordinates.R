test_that("coordinates are fixed and flagged", {
  skip_if_not_installed("bdc")
  skip_if_not_installed("CoordinateCleaner")
  skip_if_not_installed("rnaturalearthhires")

  df <- data.frame(scientificName = "Ormosia arborea",
                   country = c("Brazil", "Brazil", "Brasil", "Suriname", "Brazil", "Brazil", "Brazil"),
                   decimalLatitude = c("-12.9714", "10.2", "-9.610833", "-4.5", NA, "95", "Bloqueada"),
                   decimalLongitude = c("-38.5014", "-59.35", "65.467778", "-56.833333", NA, "-40", "Bloqueada"))
  r <- suppressMessages(std_coordinates(df, check_space = FALSE))

  expect_equal(r$decimalLatitudeOriginal, df$decimalLatitude)
  expect_equal(r$decimalLatitude[1:4], c(-12.9714, -10.2, -9.610833, 4.5))
  expect_equal(r$decimalLongitude[3], -65.467778)
  expect_equal(r$coordinates_transposed, c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
  expect_equal(r$.coordinates_empty, c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE))
  expect_equal(r$.coordinates_outOfRange, c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE))
  expect_true(is.na(r$coordinateIssues[1]))
  expect_match(r$coordinateIssues[2], "transposed, fixed \\(sign of latitude inverted\\)")
  expect_match(r$coordinateIssues[7], "not numeric")
})

test_that("coordinates outside the informed country are flagged", {
  skip_if_not_installed("bdc")
  skip_if_not_installed("rnaturalearthhires")

  df <- data.frame(country = c("Ecuador", "Brazil", "Brazil"),
                   decimalLatitude = c(-3.5, -3.5, -12.97),
                   decimalLongitude = c(-78.17, -78.17, -38.5))
  r <- suppressMessages(std_coordinates(df, fix_transposed = FALSE, check_space = FALSE))
  expect_equal(r$.coordinates_country_inconsistent, c(TRUE, FALSE, TRUE))
})

test_that("coordinates at capitals, institutions, zeros and equal values are flagged", {
  skip_if_not_installed("bdc")
  skip_if_not_installed("CoordinateCleaner")
  skip_if_not_installed("rnaturalearthhires")

  # Brasília (capital), Kew Gardens (institution), 0/0 and equal coordinates
  df <- data.frame(scientificName = "Ormosia arborea",
                   country = c("Brazil", "United Kingdom", "Brazil", "Brazil", "Brazil"),
                   decimalLatitude = c(-15.7801, 51.4787, 0, -10.5, -12.9714),
                   decimalLongitude = c(-47.9292, -0.2956, 0, -10.5, -38.5014))
  r <- suppressMessages(std_coordinates(df))
  expect_false(r$.cap[1])
  expect_false(r$.inst[2])
  expect_false(r$.zer[3])
  expect_false(r$.equ[4])
  expect_true(all(r[5, c(".cap", ".cen", ".inst", ".zer", ".equ", ".gbf")] == TRUE))
  expect_match(r$coordinateIssues[1], "country capital")
  expect_match(r$coordinateIssues[3], "zero coordinates")
  expect_true(is.na(r$coordinateIssues[5]))
})

test_that("std_coordinates checks its input", {
  skip_if_not_installed("bdc")
  expect_error(std_coordinates(data.frame(lat = 1)), "not found")
  expect_warning(
    r <- suppressMessages(std_coordinates(data.frame(decimalLatitude = -12.97,
                                                     decimalLongitude = -38.5),
                                          check_space = FALSE)),
    "Column 'country' not found")
  expect_false("coordinates_transposed" %in% names(r))
  r <- suppressMessages(std_coordinates(data.frame(country = "Brazil", decimalLatitude = -12.97,
                                                   decimalLongitude = -38.5),
                                        check_space = FALSE, rm_original_column = TRUE))
  expect_false(any(grepl("Original$", names(r))))
})
