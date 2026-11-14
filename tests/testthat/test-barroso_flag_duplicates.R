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

# Specialists of Ormosia, e.g. spec("H. C. Lima")
spec <- function(specialist, taxon = "Ormosia") data.frame(specialist = specialist, taxon = taxon)

conflicts <- data.frame(
  recordedBy = c(rep("A. Silva", 4), rep("B. Lima", 3), rep("D. Cardoso", 2), "C. Souza", "C. Souza"),
  recordNumber = c("1", "1", "1", "1", "2", "2", "2", "3", "3", "4", "4"),
  genus = "Ormosia",
  specificEpithet = c("arborea", "arborea", "fastigiata", "fastigiata",
                      "arborea", "arborea", "nitida",
                      "arborea", "nitida", "arborea", "arborea"),
  identifiedBy = c("J. Pereira", NA, "H. C. Lima", NA,
                   "J. Pereira", "J. Pereira", "D. Cardoso & L. P. Queiroz",
                   "J. Pereira", NA, NA, NA),
  collectionCode = c("MO", "K", "RB", "NY", "MO", "K", "HUEFS", "RB", "HUEFS", "RB", "K"),
  decimalLatitude = NA, decimalLongitude = NA)

test_that("specialists resolve conflicting identifications, whatever their number", {
  r <- suppressMessages(barroso_flag_duplicates(conflicts, rm_duplicates = TRUE,
                                                specialists = spec(c("H. C. Lima", "D. B. O. S. Cardoso"))))
  sp <- function(coll) r$specificEpithet[r$recordedBy == coll]
  # A. Silva 1: two arborea and two fastigiata, one fastigiata identified by H. C. Lima
  expect_equal(sp("A. Silva"), "fastigiata")
  expect_equal(r$collectionCode[r$recordedBy == "A. Silva"], "RB")
  # B. Lima 2: two arborea, but nitida identified by D. Cardoso
  expect_equal(sp("B. Lima"), "nitida")
  # D. Cardoso 3: the duplicate without determiner was collected by the specialist
  expect_equal(sp("D. Cardoso"), "nitida")
  # C. Souza 4: no conflict
  expect_equal(sp("C. Souza"), "arborea")
  expect_true(all(r$identificationConflict[r$recordedBy != "C. Souza"]))
})

test_that("blocks with equally frequent names are kept, or removed with rm_tied_conflicts", {
  expect_message(r <- barroso_flag_duplicates(conflicts, rm_duplicates = TRUE),
                 "all 6 records of 2 blocks kept")
  # A. Silva 1 (2 against 2) and D. Cardoso 3 (1 against 1) keep all their records
  expect_equal(sum(r$recordedBy == "A. Silva"), 4)
  expect_equal(sum(r$recordedBy == "D. Cardoso"), 2)
  # B. Lima 2 has a majority name
  expect_equal(r$specificEpithet[r$recordedBy == "B. Lima"], "arborea")

  expect_message(r <- barroso_flag_duplicates(conflicts, rm_duplicates = TRUE, rm_tied_conflicts = TRUE),
                 "6 records of 2 blocks removed")
  expect_false(any(r$recordedBy %in% c("A. Silva", "D. Cardoso")))
  expect_equal(nrow(r), 2)

  # Without rm_duplicates, nothing is removed
  r <- suppressMessages(barroso_flag_duplicates(conflicts, rm_tied_conflicts = TRUE))
  expect_equal(nrow(r), nrow(conflicts))
  expect_error(barroso_flag_duplicates(conflicts, specialists = 1), "data frame with the columns")
  expect_error(barroso_flag_duplicates(conflicts, specialists = "H. C. Lima"), "data frame with the columns")
  expect_error(barroso_flag_duplicates(conflicts, specialists = data.frame(specialist = "H. C. Lima", taxon = NA)),
               "no specialist with a taxon")
})

test_that("barroso_std passes the specialists and rm_tied_conflicts", {
  x <- conflicts[1:4, ]
  x$family <- "Fabaceae"
  r <- suppressMessages(barroso_std(x, rm_duplicates = TRUE, index_herbariorum = FALSE,
                                    specialists = spec("H. C. Lima", "Leguminosae")))
  expect_equal(nrow(r), 1)
  expect_equal(r$specificEpithet, "fastigiata")
  r <- suppressMessages(barroso_std(x, rm_duplicates = TRUE, index_herbariorum = FALSE,
                                    rm_tied_conflicts = TRUE))
  expect_equal(nrow(r), 0)
})

test_that("identificationUnresolved marks the blocks whose name cannot be chosen", {
  r <- suppressMessages(barroso_flag_duplicates(conflicts))
  un <- function(coll) unique(r$identificationUnresolved[r$recordedBy == coll])
  expect_equal(names(r)[which(names(r) == "identificationConflict") + 1], "identificationUnresolved")
  expect_true(un("A. Silva"))
  expect_true(un("D. Cardoso"))
  expect_false(un("B. Lima"))
  expect_false(un("C. Souza"))
  # Specialists resolve the ties
  r <- suppressMessages(barroso_flag_duplicates(conflicts, specialists = spec(c("H. C. Lima", "D. Cardoso"))))
  expect_false(any(r$identificationUnresolved, na.rm = TRUE))
  # Kept with all their records, and easy to filter
  r <- suppressMessages(barroso_flag_duplicates(conflicts, rm_duplicates = TRUE))
  expect_equal(sum(r$identificationUnresolved), 6)
  expect_true(all(is.na(suppressMessages(barroso_flag_duplicates(dups))$identificationUnresolved[
    !suppressMessages(barroso_flag_duplicates(dups))$duplicate])))
})

test_that("specialists count only for the blocks of their taxa of expertise", {
  x <- conflicts
  x$family <- "Fabaceae"
  rm_dups <- function(s) suppressMessages(barroso_flag_duplicates(x, rm_duplicates = TRUE, specialists = s))
  # A specialist of Swartzia does not resolve conflicts in Ormosia
  r <- rm_dups(spec("H. C. Lima", "Swartzia"))
  expect_equal(sum(r$recordedBy == "A. Silva"), 4)
  # A specialist of the species, the genus or the family, also by its alternative name
  for (taxon in c("Ormosia fastigiata", "Ormosia", "Fabaceae", "Leguminosae", "LEGUMINOSAE")) {
    r <- rm_dups(spec("H. C. Lima", taxon))
    expect_equal(r$specificEpithet[r$recordedBy == "A. Silva"], "fastigiata")
  }
  # Several taxa of the same specialist
  r <- rm_dups(spec(c("H. C. Lima", "H. C. Lima"), c("Swartzia", "Ormosia")))
  expect_equal(r$specificEpithet[r$recordedBy == "A. Silva"], "fastigiata")
  expect_equal(barRoso:::.family_name(c("leguminosae", "compositae", "myristicaceae", NA)),
               c("fabaceae", "asteraceae", "myristicaceae", NA))
})

test_that("records without determiner collected by a specialist as additional collector count", {
  x <- conflicts[conflicts$recordedBy == "D. Cardoso", ]
  x$recordedBy <- "J. Souza"
  x$addCollector <- c(NA, "A. Alves, D. Cardoso & K. Paix\u00e3o")
  r <- suppressMessages(barroso_flag_duplicates(x, rm_duplicates = TRUE,
                                                specialists = spec("D. B. O. S. Cardoso")))
  expect_equal(r$specificEpithet, "nitida")
  # Not when the additional collectors are in another column
  r <- suppressMessages(barroso_flag_duplicates(x, rm_duplicates = TRUE, colname_addCollector = "other",
                                                specialists = spec("D. B. O. S. Cardoso")))
  expect_equal(nrow(r), 2)
})

