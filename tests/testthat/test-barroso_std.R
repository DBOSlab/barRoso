herb <- data.frame(
  recordedBy = c("Cardoso, D.B.O.S.", "D. Cardoso", "Queiroz, L.P.", "", "Lima, H.C."),
  recordNumber = c("1350", "1350", "4021", "7", "88"),
  identifiedBy = c("Cardoso, D.", NA, "L.P. Queiroz 2005", NA, "Lima, H.C."),
  continent = NA,
  country = c("Brasil", "BRAZIL", "Brazil", "Brazil", "Brazil"),
  stateProvince = c("BA", "Bahia", "BA", "BA", "RJ"),
  county = NA,
  municipality = c("Salvador", "SALVADOR", NA, NA, NA),
  locality = c("Parque da Cidade", "Parque da Cidade", "Rio de Contas", NA, "Tijuca"),
  collectionCode = c("HUEFS", "RB", "HUEFS", "RB", "BCTW"),
  institutionCode = c("UEFS", "JBRJ", "UEFS", "JBRJ", "BCTW"),
  typeStatus = c("Holotype", "Isotype", NA, NA, NA),
  family = "Leguminosae", genus = "Ormosia",
  specificEpithet = c("arborea", "arborea", "fastigiata", "arborea", "nitida"),
  year = "2010", month = "3", day = "5")

test_that("barroso_std runs all standardizations and flags duplicates", {
  r <- suppressMessages(barroso_std(herb, index_herbariorum = FALSE))

  # The wood collection (BCTW) is removed by default
  expect_equal(nrow(r), 4)
  expect_false("BCTW" %in% r$collectionCode)
  expect_setequal(r$recordedBy, c("D. Cardoso", "L. P. Queiroz", "Unknown"))
  expect_setequal(stats::na.omit(r$identifiedBy), c("D. Cardoso", "L. P. Queiroz"))
  expect_true(all(r$stateProvince == "Bahia"))
  expect_true(all(r$family == "Fabaceae"))
  expect_equal(sum(r$duplicate), 2)
  expect_equal(unique(r$duplicateCollectionCodes[r$duplicate]), "HUEFS | RB")
  # Original columns are removed, except collector name and number
  expect_false("countryOriginal" %in% names(r))
  expect_true("recordedByOriginal" %in% names(r))
})

test_that("barroso_std options keep, remove and flag records", {
  r <- suppressMessages(barroso_std(herb, index_herbariorum = FALSE, unvouchered = FALSE, delunkcoll = TRUE,
                                    rm_duplicates = TRUE, rm_original_column = FALSE))
  expect_equal(nrow(r), 3)
  expect_true("BCTW" %in% r$collectionCode)
  expect_false("Unknown" %in% r$recordedBy)
  expect_true(all(c("countryOriginal", "identifiedByOriginal", "typeStatusOriginal") %in% names(r)))

  r <- suppressMessages(barroso_std(herb, index_herbariorum = FALSE, flag_duplicates = FALSE))
  expect_false("duplicate" %in% names(r))

  r <- suppressMessages(barroso_std(herb, index_herbariorum = FALSE, flag_duplicates = FALSE, rm_duplicates = TRUE))
  expect_equal(sum(r$recordNumber == "1350"), 1)
})

test_that("barroso_std splits large datasets into chunks", {
  big <- herb[rep(1:4, length.out = 10001), ]
  big$recordNumber <- as.character(seq_len(nrow(big)))
  expect_message(r <- barroso_std(big, index_herbariorum = FALSE, flag_duplicates = FALSE), "Chunking big: 11 of 11")
  expect_equal(nrow(r), nrow(big))
})

test_that("barroso_std treats the text \"NA\" as missing, except in code columns", {
  df <- herb[1:3, ]
  df$typeStatus <- c("NA", "Holotype", "")
  df$locality <- c("NA", "Parque da Cidade", "Rio de Contas")
  df$countryCode <- "NA"
  r <- suppressMessages(barroso_std(df, index_herbariorum = FALSE, flag_duplicates = FALSE))
  expect_equal(sum(is.na(r$typeStatus)), 2)
  expect_true("holotype" %in% r$typeStatus)
  expect_equal(sum(is.na(r$locality)), 1)
  # "NA" is the country code of Namibia
  expect_equal(unique(r$countryCode), "NA")
})

test_that("barroso_std sorts by species, collector and number, keeping duplicates together", {
  x <- data.frame(
    recordedBy = c("B. Silva", "A. Souza", "B. Silva", "B. Silva", "A. Souza", "C. Lima",
                   "B. Silva"),
    recordNumber = c("100", "20", "20", "100", "20", "5", "100"),
    family = "Fabaceae", genus = "Ormosia",
    specificEpithet = c("amazonica", "arborea", "amazonica", "ruddiana", "arborea",
                        "ruddiana", "amazonica"),
    collectionCode = c("MO", "RB", "NY", "K", "HUEFS", "INPA", "US"),
    institutionCode = NA, country = "Brazil", stateProvince = "Bahia",
    county = NA, municipality = "Salvador", locality = "Rio", continent = NA,
    typeStatus = NA)
  r <- suppressMessages(barroso_std(x, index_herbariorum = FALSE))
  # O. ruddiana of B. Silva 100 goes with its two duplicates of O. amazonica
  expect_equal(r$specificEpithet, c("amazonica", "amazonica", "amazonica", "ruddiana",
                                    "arborea", "arborea", "ruddiana"))
  expect_equal(r$recordNumber, c("20", "100", "100", "100", "20", "20", "5"))
  expect_true(all(r$identificationConflict[r$recordNumber == "100"]))

  # Numbers are sorted as numbers
  x$recordedBy <- "B. Silva"
  x$recordNumber <- c("100", "20", "3", "1000", "21", "5", "101")
  x$specificEpithet <- "arborea"
  r <- suppressMessages(barroso_std(x, index_herbariorum = FALSE))
  expect_equal(r$recordNumber, c("3", "5", "20", "21", "100", "101", "1000"))

  # The same order without duplicates, and when they are removed
  r <- suppressMessages(barroso_std(x, index_herbariorum = FALSE, flag_duplicates = FALSE))
  expect_equal(r$recordNumber, c("3", "5", "20", "21", "100", "101", "1000"))
  x$recordNumber <- "1"
  x$specificEpithet <- c("arborea", "amazonica", "amazonica", "amazonica", "arborea",
                         "arborea", "arborea")
  r <- suppressMessages(barroso_std(x, index_herbariorum = FALSE, rm_duplicates = TRUE))
  expect_equal(nrow(r), 1)
  expect_equal(r$specificEpithet, "arborea")
})
