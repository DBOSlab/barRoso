# A database already standardized and with duplicates removed, as saved by the user
old <- data.frame(
  datasource = c("GBIF", "GBIF", "JABOT", "speciesLink"),
  gbifID = c("1001", "1002", NA, NA),
  occurrenceID = c(NA, NA, "urn:catalog:RB:1", NA),
  recordedBy = c("D. Cardoso", "L. P. Queiroz", "H. C. Lima", "E. Ule"),
  recordNumber = c("100", "200", "300", NA),
  family = "Fabaceae", genus = "Ormosia",
  specificEpithet = c("arborea", "fastigiata", "nitida", "arborea"),
  collectionCode = c("MO", "HUEFS", "RB", "US"),
  catalogNumber = c("MO2839102", "HUEFS1", "RB00001", NA),
  country = "Brazil")
old <- suppressMessages(barroso_std(old, rm_duplicates = TRUE, index_herbariorum = FALSE))
# Duplicates removed earlier, e.g. the sheet at K of D. Cardoso 100
old$duplicate[old$recordedBy == "D. Cardoso"] <- TRUE
old$duplicateCollectionCodes[old$recordedBy == "D. Cardoso"] <- "MO | K"
old$duplicateCatalogNumbers[old$recordedBy == "D. Cardoso"] <- "MO2839102 | K000123"
# A correction made by hand
old$locality[old$recordedBy == "L. P. Queiroz"] <- "Serra do Sincorá, corrected"

new <- data.frame(
  datasource = c("GBIF", "GBIF", "GBIF", "JABOT", "speciesLink", "GBIF", "GBIF", "GBIF"),
  gbifID = c("1001", "9001", "9002", NA, NA, "9003", "9004", "9005"),
  occurrenceID = c(NA, NA, NA, "urn:catalog:RB:1", NA, NA, NA, NA),
  recordedBy = c("Cardoso, D.", "Cardoso, D.", "Queiroz, L.P.", "Lima, H.C.", "Ule, E.", "Pott, A.", "Pott, A.", "Lima, H.C."),
  recordNumber = c("100", "100", "200", "300", NA, "500", "500", "300"),
  family = "Leguminosae", genus = "Ormosia",
  specificEpithet = c("arborea", "arborea", "arborea", "nitida", "arborea", "arborea", "arborea", "nitida"),
  collectionCode = c("MO", "K", "NY", "RB", "US", "CGMS", "RB", "SPF"),
  catalogNumber = c("MO2839102", "000123", "NY555", "RB00001", NA, "CGMS1", "RB2", "SPF7"),
  country = "Brasil")

test_that("new records are added to a database, without the records already in it", {
  path <- file.path(withr::local_tempdir(), "database.xlsx")
  openxlsx::write.xlsx(old, path)
  expect_message(r <- barroso_std(new, rm_duplicates = TRUE, index_herbariorum = FALSE, database = path),
                 paste("2 of 8 new records already in the database \\(same gbifID or occurrenceID of the",
                       "same datasource\\) removed; 1 new records without identifier are compared"))
  # gbifID 1001 and the JABOT record by their identifiers, the sheet at K by
  # duplicateCatalogNumbers, E. Ule s.n. (US) by its fields
  expect_false(any(r$gbifID %in% c("9001") & r$newRecord))
  expect_equal(sum(r$recordedBy == "E. Ule"), 1)
  # A. Pott 500 is new, with its duplicates removed
  expect_equal(sum(r$recordedBy == "A. Pott"), 1)
  expect_true(r$newRecord[r$recordedBy == "A. Pott"])
  expect_equal(r$duplicateCollectionCodes[r$recordedBy == "A. Pott"], "CGMS | RB")
  # Database records are kept, with their corrections
  expect_equal(sum(!r$newRecord), nrow(old))
  expect_equal(r$locality[r$recordedBy == "L. P. Queiroz"], "Serra do Sincorá, corrected")
  # Duplicates of database records are added to their duplicate columns and removed:
  # L. P. Queiroz 200 at NY, with another name, and H. C. Lima 300 at SPF
  q <- r[r$recordedBy == "L. P. Queiroz", ]
  expect_equal(nrow(q), 1)
  expect_false(q$newRecord)
  expect_equal(q$specificEpithet, "fastigiata")
  expect_true(q$identificationConflict)
  expect_equal(q$duplicateCollectionCodes, "HUEFS | NY")
  expect_equal(q$duplicateCatalogNumbers, "HUEFS1 | NY555")
  expect_equal(q$duplicateIdentifications, "Ormosia fastigiata | Ormosia arborea")
  l <- r[r$recordedBy == "H. C. Lima", ]
  expect_equal(c(nrow(l), l$duplicateCollectionCodes), c("1", "RB | SPF"))
  expect_false(l$identificationConflict)
  # Groups of new blocks do not repeat those of the database
  expect_false(anyDuplicated(stats::na.omit(r$duplicateGroup)) > 0)
  expect_equal(nrow(r), nrow(old) + 1)
})

test_that("without rm_duplicates, duplicates of database records are kept and flagged", {
  r <- suppressMessages(barroso_std(new, index_herbariorum = FALSE, database = old))
  q <- r[r$recordedBy == "L. P. Queiroz", ]
  expect_equal(nrow(q), 2)
  expect_equal(q$newRecord[order(q$newRecord)], c(FALSE, TRUE))
  expect_equal(length(unique(q$duplicateGroup)), 1)
  expect_true(all(q$duplicate & q$identificationConflict))
  expect_equal(sum(r$recordedBy == "A. Pott"), 2)
})

test_that("a database is read from .csv and .rds files, and nothing new is added twice", {
  dir <- withr::local_tempdir()
  utils::write.csv(old, file.path(dir, "db.csv"), row.names = FALSE)
  saveRDS(old, file.path(dir, "db.rds"))
  a <- suppressMessages(barroso_std(new, rm_duplicates = TRUE, index_herbariorum = FALSE,
                                    database = file.path(dir, "db.csv")))
  b <- suppressMessages(barroso_std(new, rm_duplicates = TRUE, index_herbariorum = FALSE,
                                    database = file.path(dir, "db.rds")))
  expect_equal(nrow(a), nrow(b))
  # Updating again with the same download adds nothing
  expect_message(r <- barroso_std(new, rm_duplicates = TRUE, index_herbariorum = FALSE, database = b),
                 "no new records to add")
  expect_equal(nrow(r), nrow(b))
  # All new records already in the database
  expect_message(r <- barroso_std(new[1, ], index_herbariorum = FALSE, database = old),
                 "no new records")
  expect_false(any(r$newRecord))
  expect_error(barroso_std(new, index_herbariorum = FALSE, database = file.path(dir, "db.txt")),
               "File not found")
  expect_error(barroso_std(new, index_herbariorum = FALSE, database = 1), "must be a data frame")
})

test_that("without identifiers, records are compared by their catalog numbers", {
  expect_message(r <- barroso_std(new[, !names(new) %in% c("gbifID", "occurrenceID")],
                                  rm_duplicates = TRUE, index_herbariorum = FALSE, database = old),
                 "no identifier columns")
  expect_equal(sum(r$newRecord), 1)
  expect_equal(barRoso:::.sheet_key(c("MO 2839102", "mo-2839102", "")), c("MO2839102", "MO2839102", NA))
  expect_equal(barRoso:::.id_text(c(4012345678, NA)), c("4012345678", NA))
})
