test_that("names are compared with their initials and last surname", {
  p <- barRoso:::.collector_parts(c("L. P. Queiroz", "A. Delgado-Salinas", "A. Delgado Salinas",
                                    "A. D. Salinas", "C. F. P. von Martius", "C.-I. Peng", "Dui",
                                    "P. E. E. Plantae Portoricenses Sintenis", NA))
  expect_equal(p[[1]], list(initials = c("L", "P"), written = c("L", "P"), surname = "queiroz"))
  expect_equal(p[[2]], list(initials = c("A", "D"), written = "A", surname = "salinas"))
  expect_equal(p[[3]], p[[2]])
  expect_equal(p[[4]]$initials, c("A", "D"))
  expect_equal(p[[5]]$surname, "martius")
  expect_equal(p[[6]]$initials, c("C", "I"))
  expect_true(all(vapply(p[7:9], is.null, logical(1))))
  fit <- barRoso:::.initials_fit
  expect_true(fit("L", c("L", "P")))
  expect_true(fit(c("D", "S"), c("D", "B", "O", "S")))
  expect_false(fit(c("L", "M"), c("L", "P")))
  expect_false(fit(c("D", "S"), "D"))
  expect_false(fit("P", c("L", "P")))
})

test_that("the same collector written in other ways gets the most complete name", {
  coll <- c("L. Queiroz", "L. P. Queiroz", "L. Queiroz", "A. D. Salinas", "A. Delgado-Salinas",
            "A. Delgado Salinas", "A. Delgado-Salinas", "D. Cardoso", "D. Queiroz Cardoso",
            "F. Falcon", "F. Falcon", "F. Falcón", "L. Queiroz", "L. Queiroz")
  num <- c("12456", "12456", "12456", "500", "500", "500", "501", "2344", "2344",
           "7", "7", "7", "999", NA)
  y <- c(2001, 2001, 1990, 1985, NA, NA, 1985, 2008, 2008, 1990, 1990, 1990, 2001, 2001)
  m <- c(5, 5, 1, 3, NA, NA, 3, 9, 9, 1, 1, 1, 5, 5)
  d <- c(3, 3, 1, 2, NA, NA, 2, 30, 30, 1, 1, 1, 3, 3)
  mu <- c(NA, "Mucugê", "Lima", "Oaxaca", "Oaxaca", "Puebla", "Oaxaca", "Una", "Una",
          NA, NA, NA, "Mucugê", "Mucugê")
  r <- barRoso:::.match_collectors(coll, num, y, m, d, mu)
  expect_equal(as.character(r),
               c("L. P. Queiroz", "L. P. Queiroz",
                 # No same date or municipality
                 "L. Queiroz",
                 # Same municipality; the most frequent form of the same initials
                 "A. Delgado-Salinas", "A. Delgado-Salinas",
                 # Another municipality and no date
                 "A. Delgado Salinas", "A. Delgado-Salinas",
                 # "Queiroz" is a surname, not a written initial
                 "D. Cardoso", "D. Queiroz Cardoso",
                 # With accents
                 "F. Falcón", "F. Falcón", "F. Falcón",
                 # Other numbers, or without number
                 "L. Queiroz", "L. Queiroz"))
  expect_equal(sum(attr(r, "changed")), 4)
  expect_true("L. Queiroz → L. P. Queiroz" %in% attr(r, "examples"))

  # Two different complete names: the collector cannot be known
  r <- barRoso:::.match_collectors(c("L. Queiroz", "L. P. Queiroz", "L. M. Queiroz"),
                                   rep("1", 3), rep(2001, 3), rep(5, 3), rep(3, 3), rep(NA, 3))
  expect_equal(as.character(r), c("L. Queiroz", "L. P. Queiroz", "L. M. Queiroz"))
  expect_equal(attr(r, "ambiguous"), 1)

  # Fixed records are not changed, but their names are taken
  r <- barRoso:::.match_collectors(c("L. Queiroz", "L. P. Queiroz"), c("1", "1"), c(2001, 2001),
                                   c(5, 5), c(3, 3), c(NA, NA), fixed = c(TRUE, FALSE))
  expect_equal(as.character(r), c("L. Queiroz", "L. P. Queiroz"))
  expect_equal(as.character(barRoso:::.match_collectors(NA, NA, NA, NA, NA, NA)), NA_character_)
  expect_equal(as.character(barRoso:::.match_collectors("Dui", "1", NA, NA, NA, NA)), "Dui")
})

test_that("barroso_std finds the duplicates of collectors written in other ways", {
  df <- data.frame(recordedBy = c("Queiroz, L.", "Queiroz, L.P.", "Delgado-Salinas, A.", "A. D. Salinas"),
                   recordNumber = c("12456", "12456", "500", "500"),
                   family = "Fabaceae", genus = "Ormosia", specificEpithet = "arborea",
                   year = c("2001", "2001", NA, "1985"), month = c("5", "05", NA, "3"),
                   day = c("3", "3", NA, "2"), municipality = c("Mucugê", NA, "Oaxaca", "Oaxaca"),
                   collectionCode = c("HUEFS", "K", "MEXU", "NY"))
  expect_message(r <- barroso_std(df, index_herbariorum = FALSE),
                 "2 records of collectors written in other ways got their most complete name")
  expect_setequal(r$recordedBy, c("L. P. Queiroz", "A. Delgado-Salinas"))
  expect_true(all(r$duplicate))
  r <- suppressMessages(barroso_std(df, index_herbariorum = FALSE, match_collectors = FALSE))
  expect_false(any(r$duplicate))
  expect_error(barroso_std(df, match_collectors = NA), "TRUE or FALSE")
})

test_that("new records are matched with the collectors of a database written in other ways", {
  db <- data.frame(recordedBy = "L. Queiroz", recordNumber = "12456", family = "Fabaceae",
                   genus = "Ormosia", specificEpithet = "arborea", year = 2001, month = 5, day = 3,
                   municipality = "Mucugê", collectionCode = "HUEFS", catalogNumber = "HUEFS1",
                   duplicate = FALSE)
  new <- data.frame(recordedBy = c("Queiroz, L.P.", "Queiroz, L."), recordNumber = c("12456", "77"),
                    family = "Fabaceae", genus = "Ormosia", specificEpithet = "arborea",
                    year = c("2001", "2005"), month = c("5", "1"), day = c("3", "1"),
                    municipality = "Mucugê", collectionCode = "K", catalogNumber = c("K1", "K2"))
  r <- suppressMessages(barroso_std(new, rm_duplicates = TRUE, index_herbariorum = FALSE, database = db))
  # L. P. Queiroz 12456 at K is a duplicate of the record of the database, whose name is kept
  expect_equal(nrow(r), 2)
  expect_equal(r$recordedBy[!r$newRecord], "L. Queiroz")
  expect_equal(r$duplicateCollectionCodes[!r$newRecord], "HUEFS | K")
  # A new record gets the most complete name of the database
  db$recordedBy <- "L. P. Queiroz"
  new$catalogNumber[1] <- "K3"
  new$recordedBy[1] <- "Queiroz, L."
  r <- suppressMessages(barroso_std(new, index_herbariorum = FALSE, database = db))
  expect_equal(r$recordedBy[r$newRecord & r$recordNumber == "12456"], "L. P. Queiroz")
})

test_that("the complete name that agrees the most with the record is chosen", {
  # Same date and municipality rather than just the same municipality
  r <- barRoso:::.match_collectors(c("E. A. Rodrigues", "E. H. A. Rodrigues", "E. M. A. Rodrigues"),
                                   rep("341", 3), rep("1995", 3), rep("9", 3), c("15", "15", "25"),
                                   rep("Jaboticabal", 3))
  expect_equal(as.character(r), c("E. H. A. Rodrigues", "E. H. A. Rodrigues", "E. M. A. Rodrigues"))
  expect_equal(attr(r, "ambiguous"), 0)
  # The same month counts more than the same year
  r <- barRoso:::.match_collectors(c("L. Queiroz", "L. P. Queiroz", "L. M. Queiroz"), rep("1", 3),
                                   rep(2001, 3), c(5, 5, 6), c(3, 4, 9), rep("Mucuge", 3))
  expect_equal(as.character(r)[1], "L. P. Queiroz")
})
