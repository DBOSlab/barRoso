test_that("days, months and years become whole numbers, and wrong dates are fixed or flagged", {
  df <- data.frame(day = c("07", "7", "00", "31", NA, "29", "15", "5", "29", "6.0"),
                   month = c("06", "23", "Jun", "IV", NA, "2", "14", "setembro", "2", "Ago."),
                   year = c("1995", "2001", "1987.0", "2010", NA, "2001", "1999", "3020", "2000", "1960"),
                   eventDate = c(NA, NA, NA, NA, "1995-07-23T00:00:00", NA, NA, NA, NA, NA))
  expect_message(r <- std_dates(df), "6 dates fixed or flagged")
  expect_equal(r$day, c(7L, 23L, NA, NA, 23L, NA, 15L, 5L, 29L, 6L))
  expect_equal(r$month, c(6L, 7L, 6L, 4L, 7L, 2L, NA, 9L, 2L, 8L))
  expect_equal(r$year, c(1995L, 2001L, 1987L, 2010L, 1995L, 2001L, 1999L, NA, 2000L, 1960L))
  expect_equal(r$dateIssues, c(NA, "day and month swapped", NA, "invalid day", "date from eventDate",
                               "invalid day", "invalid month", "invalid year", NA, NA))
  expect_equal(names(r), c("day", "month", "year", "dateIssues", "eventDate"))
})

test_that("missing values come from eventDate of the same year, and originals can be kept", {
  df <- data.frame(day = c(NA, NA, NA), month = c(NA, "5", NA), year = c(NA, "1990", "1991"),
                   eventDate = c("23/7/1995", "1990-05-12/1990-05-14", "1990-05-12"))
  r <- suppressMessages(std_dates(df, rm_original_column = FALSE))
  expect_equal(r$day, c(23L, 12L, NA))
  expect_equal(r$month, c(7L, 5L, NA))
  expect_equal(r$year, c(1995L, 1990L, 1991L))
  expect_equal(r$monthOriginal, df$month)
  expect_equal(names(r)[1:2], c("dayOriginal", "day"))

  # Only eventDate, with other column names
  r <- suppressMessages(std_dates(data.frame(fecha = "2001-02-03"), colname_eventDate = "fecha"))
  expect_equal(c(r$day, r$month, r$year), c(3L, 2L, 2001L))
  expect_error(std_dates(data.frame(x = 1)), "No date columns")
  expect_error(std_dates(df, rm_original_column = NA), "TRUE or FALSE")
})

test_that("month names, Roman numerals and month lengths are recognized", {
  expect_equal(barRoso:::.month_number(c("Jan", "fevereiro", "Marzo", "xii", "Iunius", "13", "abc")),
               c(1L, 2L, 3L, 12L, 6L, 13L, NA))
  expect_equal(barRoso:::.days_in_month(c(2, 2, 2, 4, NA), c(1900, 2000, NA, 2001, 2001)),
               c(28, 29, 29, 30, 31))
})

test_that("barroso_std standardizes the dates before finding duplicates", {
  df <- data.frame(recordedBy = "D. Cardoso", recordNumber = NA, family = "Fabaceae", genus = "Ormosia",
                   specificEpithet = "arborea", day = c("05", "5"), month = c("03", "3"),
                   year = "2010", collectionCode = c("RB", "K"))
  r <- suppressMessages(barroso_std(df, index_herbariorum = FALSE))
  expect_true(all(r$duplicate))
  expect_equal(r$month, c(3L, 3L))
})
