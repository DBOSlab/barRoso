dups <- data.frame(
  recordedBy = c("D. Cardoso", "D. Cardoso", "D. Cardoso", "D. Cardoso",
                 "L. P. Queiroz", "L. P. Queiroz", "L. P. Queiroz",
                 "H. C. Lima", "H. C. Lima",
                 "Unknown", "Unknown",
                 "A. Pott", "A. Pott", "A. Pott"),
  recordNumber = c("100", "100", "100", "100", "200", "200", "200", "300", "300",
                   "1", "1", NA, NA, NA),
  genus = c("Ormosia", "Ormosia", "Ormosia", "Swartzia", "Ormosia", "Ormosia", "Ormosia",
            "Ormosia", "Ormosia", "Ormosia", "Ormosia", "Ormosia", "Ormosia", "Ormosia"),
  specificEpithet = c("arborea", "arborea", NA, "apetala", "fastigiata", NA, NA,
                      "nitida", "nitida", "arborea", "arborea", "arborea", "arborea", "arborea"),
  collectionCode = c("MO", "JBB", "RB", "K", "HUEFS", "ALCB", "RB", "RB", "RB",
                     "RB", "K", "CGMS", "RB", "RB"),
  catalogNumber = c("2839102", "13548", "RB00123", NA, "1", "2", "3", "4", "5",
                    "6", "7", "8", "9", "10"),
  decimalLatitude = c(NA, "-12.9", NA, NA, NA, "-12.5", "-12.5", NA, NA, NA, NA, NA, NA, NA),
  decimalLongitude = c(NA, "-38.5", NA, NA, NA, "-38.1", "-38.1", NA, NA, NA, NA, NA, NA, NA),
  year = c(rep(NA, 11), "2001", "2001", "2002"),
  month = c(rep(NA, 11), "5", "5", "5"),
  day = c(rep(NA, 11), "3", "3", "3"))

test_that("blocks of duplicates and conflicting identifications are flagged", {
  r <- suppressMessages(barroso_flag_duplicates(dups))
  r <- r[order(r$recordedBy, r$recordNumber, r$catalogNumber), ]
  by_coll <- function(col, who) r[[col]][r$recordedBy == who]

  expect_true(all(by_coll("duplicate", "D. Cardoso")))
  expect_true(all(by_coll("identificationConflict", "D. Cardoso")))
  expect_equal(unique(by_coll("duplicateIdentifications", "D. Cardoso")),
               "Ormosia arborea | Ormosia | Swartzia apetala")
  expect_equal(unique(by_coll("duplicateCollectionCodes", "D. Cardoso")), "MO | JBB | RB | K")
  expect_equal(unique(by_coll("duplicateCatalogNumbers", "D. Cardoso")),
               "MO 2839102 | JBB 13548 | RB00123 | K")

  # Genus-only identifications do not conflict with a species of the same genus
  expect_false(any(by_coll("identificationConflict", "L. P. Queiroz")))
  expect_false(any(by_coll("identificationConflict", "H. C. Lima")))

  # Unknown collectors are not duplicates
  expect_false(any(by_coll("duplicate", "Unknown")))
  expect_true(all(is.na(by_coll("duplicateGroup", "Unknown"))))
  expect_setequal(by_coll("duplicateCollectionCodes", "Unknown"), c("K", "RB"))

  # Without number, duplicates share species, collector and date
  expect_equal(sum(by_coll("duplicate", "A. Pott")), 2)
})

test_that("rm_duplicates keeps the best record of each block", {
  r <- suppressMessages(barroso_flag_duplicates(dups, rm_duplicates = TRUE))
  kept <- function(who) r[r$recordedBy == who, ]

  # Majority identification (Ormosia arborea), then coordinates (JBB)
  expect_equal(kept("D. Cardoso")$collectionCode, "JBB")
  expect_equal(kept("D. Cardoso")$duplicateCatalogNumbers, "MO 2839102 | JBB 13548 | RB00123 | K")
  # Species name rather than genus only, even without coordinates
  expect_equal(kept("L. P. Queiroz")$collectionCode, "HUEFS")
  expect_equal(nrow(kept("H. C. Lima")), 1)
  expect_equal(nrow(kept("Unknown")), 2)
  expect_equal(nrow(kept("A. Pott")), 2)
})

test_that("the specific epithet is taken from a GBIF binomial in species", {
  gbif <- data.frame(recordedBy = "D. Cardoso", recordNumber = "1", genus = "Ormosia",
                     specificEpithet = NA, species = c("Ormosia arborea", "Ormosia sp.", "Ormosia nitida"))
  r <- suppressMessages(barroso_flag_duplicates(gbif))
  expect_true(all(r$identificationConflict))
  expect_equal(unique(r$duplicateIdentifications), "Ormosia arborea | Ormosia | Ormosia nitida")
})
