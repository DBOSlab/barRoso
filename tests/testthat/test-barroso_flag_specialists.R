records <- data.frame(
  recordedBy = c("A. Silva", "A. Silva", "D. Cardoso", "J. Souza", "B. Lima", "B. Lima", "C. Souza"),
  addCollector = c(NA, NA, NA, "K. Paixão & H. C. Lima", NA, NA, NA),
  recordNumber = c("1", "1", "2", "3", "4", "5", "6"),
  family = c("Fabaceae", "Fabaceae", "Fabaceae", "Leguminosae", "Fabaceae", "Myristicaceae", "Fabaceae"),
  genus = c("Ormosia", "Ormosia", "Ormosia", "Swartzia", "Ormosia", "Virola", "Ormosia"),
  specificEpithet = c("arborea", "arborea", "nitida", "apetala", "arborea", "sebifera", "nitida"),
  identifiedBy = c("H. C. Lima", NA, "D. B. O. S. Cardoso", NA, "J. Pereira", "D. Cardoso", "H. Lima & D. Cardoso"),
  collectionCode = c("RB", "MO", "HUEFS", "RB", "K", "INPA", "NY"))

specialists <- data.frame(specialist = c("D. B. O. S. Cardoso", "H. C. Lima"),
                          taxon = c("Ormosia", "Leguminosae"))

test_that("records identified by specialists of their taxa are flagged", {
  df <- suppressMessages(barroso_flag_duplicates(records))
  expect_message(r <- barroso_flag_specialists(df, specialists),
                 "5 of 7 records identified by the specialists \\(3 as determiners, 1 as collectors")
  r <- r[order(r$recordNumber, r$collectionCode), ]
  expect_equal(r$specialistIdentification, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE))
  # The duplicate at MO of the specimen identified by H. C. Lima at RB
  expect_equal(r$specialistEvidence, c("duplicate", "identifiedBy", "identifiedBy", "recordedBy",
                                       NA, NA, "identifiedBy"))
  expect_equal(r$specialistName, c("H. C. Lima", "H. C. Lima", "D. B. O. S. Cardoso", "H. C. Lima",
                                   NA, NA, "D. B. O. S. Cardoso | H. C. Lima"))
  # Virola is not among the taxa of D. Cardoso
  expect_false(r$specialistIdentification[r$genus == "Virola"])
  expect_equal(names(r)[which(names(r) == "identifiedBy") + 1:3],
               c("specialistIdentification", "specialistName", "specialistEvidence"))
})

test_that("filter keeps just the records identified by specialists", {
  r <- suppressMessages(barroso_flag_specialists(records, specialists, filter = TRUE))
  # Without duplicate columns, the duplicate at MO is not flagged
  expect_equal(nrow(r), 4)
  expect_true(all(r$specialistIdentification))
  # Run again on its output
  expect_equal(ncol(suppressMessages(barroso_flag_specialists(r, specialists))), ncol(r))
})

test_that("barroso_flag_specialists validates its input", {
  expect_error(barroso_flag_specialists(records), "Please give the `specialists`")
  expect_error(barroso_flag_specialists(records, "H. C. Lima"), "data frame with the columns")
  expect_error(barroso_flag_specialists(records, specialists, filter = NA), "TRUE or FALSE")
  expect_error(barroso_flag_specialists(records[, c("genus", "family")], specialists), "not found")
})

test_that("barroso_std flags the specialists before removing duplicates", {
  r <- suppressMessages(barroso_std(records, rm_duplicates = TRUE, flag_specialists = TRUE,
                                    specialists = specialists, index_herbariorum = FALSE))
  # A. Silva 1 is kept once, flagged by H. C. Lima
  expect_equal(sum(r$recordedBy == "A. Silva"), 1)
  expect_true(r$specialistIdentification[r$recordedBy == "A. Silva"])
  expect_error(suppressMessages(barroso_std(records, flag_specialists = TRUE, index_herbariorum = FALSE)),
               "Please give the `specialists`")
  # Without flag_duplicates, the duplicate columns are not added
  r <- suppressMessages(barroso_std(records, flag_duplicates = FALSE, flag_specialists = TRUE,
                                    specialists = specialists, index_herbariorum = FALSE))
  expect_false("duplicate" %in% names(r))
  expect_equal(sum(r$specialistIdentification), 5)
})
