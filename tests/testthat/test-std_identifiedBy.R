test_that("determiner names are standardized and kept in the same cell", {
  df <- data.frame(identifiedBy = c("Cardoso, DBOS", "D.Cardoso & L.P.Queiroz",
                                    "Lobato, LC; Soares, CRA; Silva, CA",
                                    "Fernandes, A.; et al.", "R.Barneby (NY) 1994-06 en K",
                                    "J.E. Meireles & H.C. de Lima/VI-2011",
                                    "David Keil   Oct 1968", "Davis, Marla",
                                    "Richard et O. Poncy", "JAGG",
                                    "NO DISPONIBLE", "Anónimo", "Administrador"))
  r <- suppressMessages(std_identifiedBy(df))
  expect_equal(r$identifiedByOriginal, df$identifiedBy)
  expect_equal(r$identifiedBy,
               c("D. Cardoso", "D. Cardoso & L. P. Queiroz",
                 "L. C. Lobato, C. R. A. Soares & C. A. Silva",
                 "A. Fernandes et al.", "R. Barneby", "J. E. Meireles & H. C. Lima",
                 "D. Keil", "M. Davis", "Richard & O. Poncy", "JAGG", NA, NA, NA))
})

test_that("std_identifiedBy works when there are no determiners", {
  r <- suppressMessages(std_identifiedBy(data.frame(identifiedBy = c(NA, NA))))
  expect_equal(r$identifiedBy, c(NA_character_, NA_character_))
  expect_error(std_identifiedBy(data.frame(x = 1)), "not found")
})
