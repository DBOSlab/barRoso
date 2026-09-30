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
  r <- suppressMessages(barroso_std(herb))

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
  r <- suppressMessages(barroso_std(herb, unvouchered = FALSE, delunkcoll = TRUE,
                                    rm_duplicates = TRUE, rm_original_column = FALSE))
  expect_equal(nrow(r), 3)
  expect_true("BCTW" %in% r$collectionCode)
  expect_false("Unknown" %in% r$recordedBy)
  expect_true(all(c("countryOriginal", "identifiedByOriginal", "typeStatusOriginal") %in% names(r)))

  r <- suppressMessages(barroso_std(herb, flag_duplicates = FALSE))
  expect_false("duplicate" %in% names(r))

  r <- suppressMessages(barroso_std(herb, flag_duplicates = FALSE, rm_duplicates = TRUE))
  expect_equal(sum(r$recordNumber == "1350"), 1)
})

test_that("barroso_std splits large datasets into chunks", {
  big <- herb[rep(1:4, length.out = 10001), ]
  big$recordNumber <- as.character(seq_len(nrow(big)))
  expect_message(r <- barroso_std(big, flag_duplicates = FALSE), "Chunking big: 11 of 11")
  expect_equal(nrow(r), nrow(big))
})
