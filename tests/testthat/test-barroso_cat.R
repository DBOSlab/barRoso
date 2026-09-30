gbif <- data.frame(recordedBy = c("A", "B", "C"), collectionCode = c("RB", "K", "NY"),
                   year = c(2001, 2002, 2003))
splink <- data.frame(collector = c("D", "E", "F"), collectornumber = c("1", "2", "3"),
                     collectioncode = c("RB", "HUEFS", "ALCB"), yearcollected = c("2004", "2005", NA),
                     genus = "Ormosia", species = c("arborea", "fastigiata", "arborea"))
jabot <- data.frame(recordedBy = c("G", "H"), collectionCode = c("HUEFS", "RB"))

test_that("sources are combined with Darwin Core names and a datasource column", {
  r <- suppressMessages(barroso_cat(list(GBIF = gbif, speciesLink = splink)))
  expect_equal(nrow(r), 6)
  expect_true(all(c("recordedBy", "recordNumber", "collectionCode", "datasource") %in% names(r)))
  expect_equal(r$species[4:6], c("Ormosia arborea", "Ormosia fastigiata", "Ormosia arborea"))
  expect_type(r$year, "character")
})

test_that("keep_source removes only herbaria also present in the preferred source", {
  r <- suppressMessages(barroso_cat(list(GBIF = gbif, speciesLink = splink, JABOT = jabot),
                                    keep_source = "GBIF"))
  expect_false(any(r$collectionCode == "RB" & r$datasource != "GBIF"))
  # HUEFS is shared by speciesLink and JABOT, but not by GBIF
  expect_equal(sum(r$collectionCode == "HUEFS"), 2)
})

test_that("barroso_cat validates its arguments", {
  expect_error(barroso_cat(list(gbif, splink)), "unique names")
  expect_error(barroso_cat(list(GBIF = gbif, SL = splink), keep_source = "X"), "must be one of")
  expect_error(barroso_cat(list(GBIF = gbif)), "at least two")
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
  expect_message(r <- barroso_cat(list(REFLORA = reflora, JABOT = jabot)),
                 "REFLORA values standardized")
  rf <- r[r$datasource == "REFLORA", ]
  jb <- r[r$datasource == "JABOT", ]

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
  r <- suppressMessages(barroso_cat(list(a = reflora, b = jabot, c = gbif)))
  expect_equal(r$eventDate[r$datasource == "b"], c("2008-09-26", "1940-12-02"))
  expect_equal(r$typeStatus[r$datasource == "a"], c(NA, "holotype", "isotype"))
  # GBIF data is not changed
  expect_equal(r$recordedBy[r$datasource == "c"], gbif$recordedBy)

  expect_true(barRoso:::.is_reflora(reflora))
  expect_false(barRoso:::.is_jabot(reflora))
  expect_true(barRoso:::.is_jabot(jabot))
  expect_false(barRoso:::.is_reflora(gbif))
})

test_that("herbaria of REFLORA and JABOT already in GBIF are removed with keep_source", {
  g <- data.frame(recordedBy = "A. Ducke", recordNumber = "770", collectionCode = "RB",
                  catalogNumber = "RB00180784")
  r <- suppressMessages(barroso_cat(list(GBIF = g, REFLORA = reflora, JABOT = jabot),
                                    keep_source = "GBIF"))
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
