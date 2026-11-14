gbif <- data.frame(recordedBy = c("A", "B", "C"), collectionCode = c("RB", "K", "NY"),
                   year = c(2001, 2002, 2003))
splink <- data.frame(collector = c("D", "E", "F"), collectornumber = c("1", "2", "3"),
                     collectioncode = c("RB", "HUEFS", "ALCB"), yearcollected = c("2004", "2005", NA),
                     genus = "Ormosia", species = c("arborea", "fastigiata", "arborea"))
jabot <- data.frame(recordedBy = c("G", "H"), collectionCode = c("HUEFS", "RB"))

test_that("sources are combined with Darwin Core names and a datasource column", {
  r <- suppressMessages(barroso_cat(list(GBIF = gbif, speciesLink = splink),
                                    keep_source = c("GBIF", "speciesLink"), index_herbariorum = FALSE))
  # RB of speciesLink is removed, as it is in GBIF
  expect_equal(nrow(r), 5)
  expect_true(all(c("recordedBy", "recordNumber", "collectionCode", "datasource") %in% names(r)))
  expect_equal(r$species[4:5], c("Ormosia fastigiata", "Ormosia arborea"))
  expect_type(r$year, "character")
})

test_that("each herbarium is kept from the first source that has it, in the order of keep_source", {
  sources <- list(GBIF = gbif, speciesLink = splink, JABOT = jabot)
  expect_message(r <- barroso_cat(sources, keep_source = c("GBIF", "speciesLink", "JABOT"),
                                  index_herbariorum = FALSE),
                 "removing 1 records of 1 herbaria from JABOT also present in speciesLink")
  expect_equal(r$datasource[r$collectionCode == "RB"], "GBIF")
  # HUEFS is in speciesLink and JABOT, but not in GBIF
  expect_equal(r$datasource[r$collectionCode == "HUEFS"], "speciesLink")
  expect_equal(nrow(r), 5)

  # JABOT first
  r <- suppressMessages(barroso_cat(sources, keep_source = c("JABOT", "speciesLink", "GBIF"),
                                    index_herbariorum = FALSE))
  expect_equal(r$datasource[r$collectionCode %in% c("RB", "HUEFS")], c("JABOT", "JABOT"))
  expect_equal(r$collectionCode[r$datasource == "speciesLink"], "ALCB")
  expect_equal(r$collectionCode[r$datasource == "GBIF"], c("K", "NY"))
})

test_that("barroso_cat validates its arguments", {
  expect_error(barroso_cat(list(gbif, splink), index_herbariorum = FALSE), "unique names")
  expect_error(barroso_cat(list(GBIF = gbif, SL = splink), index_herbariorum = FALSE),
               "order of priority of all sources in `keep_source`, e.g. c\\(\"GBIF\", \"SL\"\\)")
  for (k in list("X", "GBIF", c("GBIF", "GBIF"), c("GBIF", "SL", "X"), c("GBIF", NA))) {
    expect_error(barroso_cat(list(GBIF = gbif, SL = splink), keep_source = k), "names of all sources once")
  }
  expect_error(barroso_cat(list(GBIF = gbif), index_herbariorum = FALSE), "at least two")
})

reflora <- data.frame(
  occurrenceID = c("640303", "626347", "612006"),
  institutionCode = c("K", "RB", "RB"), collectionCode = c("K", "RB", "RB"),
  catalogNumber = c("K000892617", "RB00180784", "RB00181112"),
  genus = "Ormosia", species = c("Ormosia altimontana", "Ormosia arborea", NA),
  taxonName = c("Ormosia altimontana", "Ormosia arborea", "Ormosia"),
  recordedBy = c("H.C. Lima;J.E. Meireles", "A .Ducke", "A Ducke"),
  recordNumber = c("6464", "770", NA),
  eventDate = c("11/10/2006", "2/12/1940", "--/5/1880"),
  year = c("2006", "1940", "1880"),
  typeStatus = c(NA, "HOLOTYPUS", "ISOTYPUS"),
  identifiedBy = c("Administrador", "Cardoso, D.B.O.S.", "Administrador"),
  dateIdentified = c("22/4/2020", "9/10/1990 até 12/10/1990", NA),
  decimalLatitude = c(-22.3, NA, NA),
  bibliographicCitation = "Reflora Virtual Herbarium, available at: https://reflora.jbrj.gov.br")

jabot <- data.frame(
  occurrenceID = c("urn:catalog:CGMS:1053327", "urn:catalog:RB:180784"),
  institutionCode = c("UFMS", "JBRJ"), collectionCode = c("CGMS", "RB"),
  catalogNumber = c("CGMS1053327", "RB00180784"),
  kingdom = "Plantae", genus = "Ormosia", species = "Ormosia arborea", taxonName = "Ormosia arborea",
  recordedBy = c("A. Pott; V.J. Pott; L.C.P. Lima", "A .Ducke"), recordNumber = c("15200", "770"),
  eventDate = c("2008-9-26", "1940-12-02"), year = c("2008", "1940"),
  typeStatus = c(NA, "holotype"), identifiedBy = c(NA, "H.C. Lima"),
  dateIdentified = c("2010-3", "4/2020"),
  decimalLatitude = c(-23.1, NA))

test_that("REFLORA and JABOT values are standardized into Darwin Core formats", {
  expect_message(rf <- barroso_dwc(reflora), "REFLORA format, values standardized")
  jb <- suppressMessages(barroso_dwc(jabot))

  expect_equal(rf$identifiedBy, c(NA, "Cardoso, D.B.O.S.", NA))
  expect_equal(rf$eventDate, c("2006-10-11", "1940-12-02", "1880-05"))
  expect_equal(rf$dateIdentified, c("2020-04-22", "1990-10-09/1990-10-12", NA))
  expect_equal(rf$typeStatus, c(NA, "holotype", "isotype"))
  expect_equal(rf$institutionCode, c("K", "JBRJ", "JBRJ"))

  expect_equal(jb$eventDate, c("2008-09-26", "1940-12-02"))
  expect_equal(jb$dateIdentified, c("2010-03", "2020-04"))
  expect_equal(jb$typeStatus, c(NA, "holotype"))
})

test_that("REFLORA and JABOT are recognized by their content, not by their names", {
  r <- suppressMessages(barroso_cat(list(a = reflora, b = jabot, c = gbif), keep_source = c("a", "b", "c"),
                                    index_herbariorum = FALSE))
  # RB of b and RB and K of c are already in a
  expect_equal(r$eventDate[r$datasource == "b"], "2008-09-26")
  expect_equal(r$typeStatus[r$datasource == "a"], c(NA, "holotype", "isotype"))
  # GBIF data is not changed
  expect_equal(r$recordedBy[r$datasource == "c"], "C")

  expect_true(barRoso:::.is_reflora(reflora))
  expect_false(barRoso:::.is_jabot(reflora))
  expect_true(barRoso:::.is_jabot(jabot))
  expect_false(barRoso:::.is_reflora(gbif))
})

test_that("herbaria of REFLORA and JABOT already in GBIF are removed when GBIF comes first", {
  g <- data.frame(recordedBy = "A. Ducke", recordNumber = "770", collectionCode = "RB",
                  catalogNumber = "RB00180784")
  r <- suppressMessages(barroso_cat(list(GBIF = g, REFLORA = reflora, JABOT = jabot),
                                    keep_source = c("GBIF", "REFLORA", "JABOT"), index_herbariorum = FALSE))
  expect_equal(sort(unique(r$collectionCode)), c("CGMS", "K", "RB"))
  expect_equal(sum(r$collectionCode == "RB"), 1)
  expect_equal(r$datasource[r$collectionCode == "RB"], "GBIF")
})

test_that("dates are converted into ISO 8601", {
  expect_equal(barRoso:::.iso_date(c("2/12/1940", "--/5/1880", "--/--/1880", "2008-9-26",
                                     "1929-4", "1914", "4/2020", "sem data", NA)),
               c("1940-12-02", "1880-05", "1880", "2008-09-26", "1929-04", "1914", "2020-04",
                 "sem data", NA))
  expect_equal(barRoso:::.latin_type_status(c("HOLOTYPUS", "Lectotypus", "TYPUS", "isotype", NA)),
               c("holotype", "lectotype", "type", "isotype", NA))
})

test_that("herbarium acronyms are standardized before comparing the sources", {
  local_mocked_bindings(.ih_institutions = function() data.frame(
    code = c("MO", "INPA", "RB", "UNB"), organization = c("Missouri Botanical Garden",
    "Instituto Nacional de Pesquisas da Amazonia", "Jardim Botanico do Rio de Janeiro",
    "Universidade de Brasilia"), otherInstitutionNames = ""))
  g <- data.frame(recordedBy = c("A", "B", "C"), collectionCode = c("MO", "INPA", "UNB"))
  sl <- data.frame(collector = c("D", "E", "F", "G"), collectornumber = c("1", "2", "3", "4"),
                   collectioncode = c("MOBOT_BR", "Herbario INPA", "UnB", "INPA-Carpoteca"),
                   institutioncode = c("MOBOT", "INPA", "UnB", "INPA"))
  r <- suppressMessages(barroso_cat(list(GBIF = g, speciesLink = sl), keep_source = c("GBIF", "speciesLink")))
  # All herbaria of speciesLink are also in GBIF
  expect_equal(r$datasource, rep("GBIF", 3))
  expect_true(all(r$indexHerbariorum))

  r <- suppressMessages(barroso_cat(list(GBIF = data.frame(recordedBy = "H", collectionCode = "NY"),
                                         speciesLink = sl), keep_source = c("speciesLink", "GBIF")))
  expect_equal(r$collectionCode[2:5], c("MO", "INPA", "UNB", "INPA"))
  expect_equal(r$collectionCodeOriginal[2:5], sl$collectioncode)
  # The carpological collection is still recognized by barroso_std
  expect_equal(nrow(.delunvouchered(r, "collectionCode")), 4)
  # A source without institutionCode does not get one
  expect_true(is.na(r$institutionCode[1]))
})

test_that("barroso_cat works offline when Index Herbariorum cannot be reached", {
  local_mocked_bindings(.ih_institutions = function() {
    warning("Index Herbariorum could not be reached, so the codes were not checked", call. = FALSE)
    NULL
  })
  expect_warning(r <- suppressMessages(barroso_cat(list(GBIF = gbif, speciesLink = splink),
                                                    keep_source = c("GBIF", "speciesLink"))),
                 "could not be reached")
  expect_equal(nrow(r), 5)
  expect_false("indexHerbariorum" %in% names(r))
})

test_that("GBIF records citing REFLORA are recognized as GBIF", {
  g <- data.frame(gbifID = 1, recordedBy = "A", collectionCode = "RB", institutionCode = "RB",
                  bibliographicCitation = "Reflora - Virtual Herbarium")
  expect_message(barroso_dwc(g), "GBIF")
})

test_that("the identifiers of the sources are kept, with a warning for records without them", {
  g <- data.frame(gbifID = c(1234567890, 1234567891, NA), recordedBy = c("A", "B", "C"),
                  collectionCode = c("MO", "K", "NY"))
  expect_message(r <- (barRoso::barroso_cat(list(GBIF = g, REFLORA = reflora),
                                                             keep_source = c("GBIF", "REFLORA"),
                                                             index_herbariorum = FALSE)),
                 "1 of 3 records of GBIF have no unique identifier \\(gbifID\\); when updating a database")
  expect_equal(r$gbifID[1:2], c("1234567890", "1234567891"))
  # K of REFLORA is also in GBIF
  expect_equal(r$occurrenceID[r$datasource == "REFLORA"], reflora$occurrenceID[2:3])
  expect_message(barRoso::barroso_cat(list(GBIF = g[1:2, ], speciesLink = splink),
                                      keep_source = c("GBIF", "speciesLink"),
                                      index_herbariorum = FALSE),
                 "all records of speciesLink have no unique identifier \\(no column")
  # Repeated identifiers
  expect_message(barRoso:::.check_source_id(data.frame(occurrenceID = c("a", "a", "b")), "JABOT"),
                 "2 of 3 records of JABOT")
  expect_silent(barRoso:::.check_source_id(g[1:2, ], "GBIF"))
})

