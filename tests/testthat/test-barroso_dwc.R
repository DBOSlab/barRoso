# The three column layouts of speciesLink downloads
splink_old <- data.frame(
  collector = c("Queiroz, L. P.", "Cardoso, D."), collectornumber = c("1350", NA),
  fieldnumber = c(NA, "88"), collectioncode = "HUEFS", institutioncode = "UEFS",
  taxonclass = "Magnoliopsida", ordem = "Fabales", genus = "Ormosia",
  species = c("arborea", "cf. fastigiata"), subspecies = NA,
  scientificname = c("Ormosia arborea", "Ormosia fastigiata"),
  scientificnameauthor = c("(Vell.) Harms", "Tul."),
  yearidentified = c("2005", "2010"), monthidentified = c("3", NA), dayidentified = c("7", NA),
  yearcollected = "2001", latitude = "-12.97", longitude = "-38.5", notes = "Tree")

splink_lower_dwc <- data.frame(
  recordedby = "Queiroz, L. P.", recordnumber = "1350", collectioncode = "HUEFS",
  decimallatitude = "-12.97", decimallongitude = "-38.5", genus = "Ormosia",
  specificepithet = "arborea", scientificname = "Ormosia arborea",
  scientificnameauthorship = "(Vell.) Harms", yearidentified = "2005",
  monthidentified = "3", dayidentified = "7", typestatus = "Isotipo")

splink_new <- data.frame(
  id = "urn:catalog:JBRJ:RB:1356431", type = "PhysicalObject", collectionCode = "RB",
  recordedBy = "R.C. Forzza", recordNumber = "6733", eventDate = "23/11/2011",
  dateIdentified = "23/08/2017", genus = "Croton", specificEpithet = "abaitensis",
  spp = "Croton abaitensis", scientificName = "Croton abaitensis Baill.",
  scientificNameAuthorship = "Baill.")

test_that("the older speciesLink format is formatted into Darwin Core", {
  expect_message(r <- barroso_dwc(splink_old), "barroso_dwc: speciesLink format")
  expect_true(all(c("recordedBy", "recordNumber", "fieldNumber", "collectionCode",
                    "institutionCode", "class", "order", "specificEpithet",
                    "scientificNameAuthorship", "decimalLatitude", "decimalLongitude",
                    "year", "occurrenceRemarks", "dateIdentified", "species") %in% names(r)))
  expect_false(any(c("collector", "latitude", "yearidentified", "monthidentified") %in% names(r)))

  expect_equal(r$recordedBy, splink_old$collector)
  expect_equal(r$recordNumber, c("1350", "88"))
  expect_equal(r$specificEpithet, c("arborea", "fastigiata"))
  expect_equal(r$identificationQualifier, c(NA, "cf."))
  expect_equal(r$species, c("Ormosia arborea", "Ormosia fastigiata"))
  expect_equal(r$scientificName, c("Ormosia arborea (Vell.) Harms", "Ormosia fastigiata Tul."))
  expect_equal(r$dateIdentified, c("2005-03-07", "2010"))
})

test_that("speciesLink with lowercase Darwin Core names is formatted", {
  r <- barroso_dwc(splink_lower_dwc, verbose = FALSE)
  expect_equal(names(r)[1:6], c("recordedBy", "recordNumber", "collectionCode",
                                "decimalLatitude", "decimalLongitude", "genus"))
  expect_equal(r$decimalLatitude, "-12.97")
  expect_equal(r$decimalLongitude, "-38.5")
  expect_equal(r$dateIdentified, "2005-03-07")
  expect_equal(r$scientificName, "Ormosia arborea (Vell.) Harms")
  expect_equal(r$species, "Ormosia arborea")
})

test_that("the current speciesLink format keeps its names and gets ISO dates", {
  r <- barroso_dwc(splink_new, verbose = FALSE)
  expect_equal(r$species, "Croton abaitensis")
  expect_false("spp" %in% names(r))
  expect_equal(r$eventDate, "2011-11-23")
  expect_equal(r$dateIdentified, "2017-08-23")
  # The author is not added twice
  expect_equal(r$scientificName, "Croton abaitensis Baill.")
})

test_that("epithets with other words are split", {
  x <- c("\"sincorense\"", "viçosanum", "brasiliana.", "cf.andarahiensis", "af. acuminata",
         "stygium var. annulatum", "(Spinulosae) mollis", "x hybrida", "catharinae Mart.",
         "sp. nov.", "sp.1", "fabaceae sp1", "indet.")
  e <- barRoso:::.split_epithet(x)
  expect_equal(e$epithet, c("sincorense", "viçosanum", "brasiliana", "andarahiensis", "acuminata",
                            "stygium", "mollis", "hybrida", "catharinae", NA, NA, NA, NA))
  expect_equal(e$identificationQualifier, c(NA, NA, NA, "cf.", "aff.", rep(NA, 8)))
  expect_equal(e$infraspecificEpithet, c(rep(NA, 5), "annulatum", rep(NA, 7)))
})

test_that("the source is recognized, or can be given", {
  gbif <- data.frame(gbifID = "1", recordedBy = "A", genus = "Ormosia", species = "Ormosia arborea")
  expect_equal(barRoso:::.detect_source(splink_old), "speciesLink")
  expect_equal(barRoso:::.detect_source(splink_new), "speciesLink")
  expect_equal(barRoso:::.detect_source(gbif), "GBIF")
  expect_equal(barRoso:::.detect_source(data.frame(x = 1)), "unknown")
  expect_message(r <- barroso_dwc(gbif), "barroso_dwc: GBIF format\\s*$")
  expect_equal(r, gbif)

  # Lowercase Darwin Core names of any source are renamed
  r <- barroso_dwc(data.frame(recordedby = "A", decimallatitude = 1), source = "GBIF",
                   verbose = FALSE)
  expect_equal(names(r), c("recordedBy", "decimalLatitude"))
  # A name is never duplicated
  r <- barroso_dwc(data.frame(recordedBy = "A", recordedby = "B"), verbose = FALSE)
  expect_equal(names(r), c("recordedBy", "recordedby"))

  expect_error(barroso_dwc(1), "must be a data frame")
  expect_error(barroso_dwc(gbif, source = "JSTOR"), "should be one of")
})

test_that("outputs of different downloads can always be combined", {
  # As returned by the speciesLink API, with types depending on the values
  sp1 <- data.frame(recordedby = "A", coordinateprecision = NA_character_, yearcollected = 2001L)
  sp2 <- data.frame(recordedby = "B", coordinateprecision = 100L, yearcollected = "2002")
  expect_error(dplyr::bind_rows(sp1, sp2))
  r <- dplyr::bind_rows(lapply(list(sp1, sp2), barroso_dwc, verbose = FALSE))
  expect_equal(r$coordinatePrecision, c(NA, "100"))
  expect_equal(r$year, c("2001", "2002"))

  r <- barroso_dwc(sp2, as_character = FALSE, verbose = FALSE)
  expect_type(r$coordinatePrecision, "integer")
})
