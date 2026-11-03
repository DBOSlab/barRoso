specimens <- data.frame(
  genus = "Ouratea",
  specificEpithet = c("concinna", "concinna", "concinna", "crenulata", "crenulata", NA),
  scientificNameAuthorship = c(rep("A.Queiroz-Lima & D.B.O.S.Cardoso", 3), "Engl.", "Engl.", NA),
  recordedBy = c("D. B. O. S. Cardoso", "D. Cardoso", "A. Queiroz-Lima", "L. P. Queiroz",
                 "R. M. Harley", "J. Silva"),
  addCollector = c("et al.", NA, "D. B. O. S. Cardoso", NA, NA, NA),
  recordNumber = c("1350", "1350", "88", "4021", NA, "1"),
  country = c("Brazil", "Brazil", "Brazil", "Brazil", "Bolivia", "Brazil"),
  stateProvince = c("Bahia", "Bahia", "Minas Gerais", "Bahia", "Santa Cruz", "Bahia"),
  municipality = c("Salvador", "Salvador", "Diamantina", "Rio de Contas", NA, NA),
  locality = c("Parque da Cidade", "Parque da Cidade", "Serra do Espinhaço.",
               "Pico das Almas", "Chiquitos", NA),
  decimalLatitude = c(-12.9714, -12.9714, -18.24, -13.5, NA, NA),
  decimalLongitude = c(-38.5014, -38.5014, -43.6, -41.9, NA, NA),
  day = c(5, 5, NA, 12, NA, NA),
  month = c(3, 3, 7, 11, NA, NA),
  year = c(2010, 2010, 2015, 1988, 1995, NA),
  reproductiveCondition = c("fl", NA, NA, "fr", NA, NA),
  collectionCode = c("HUEFS", "RB", "DIAM", "K", "NY", "RB"),
  catalogNumber = c("HUEFS000123", "456", NA, NA, "NY0099", NA),
  typeStatus = c("holotype", "isotype", NA, NA, NA, NA))

write_specimens <- function(dir, journal = "Systematic Botany", ...) {
  barroso_write_specimens(df = specimens, journal = journal, dir = dir,
                          filename = "out", verbose = FALSE, ...)
}

docx_lines <- function(dir, name = "out") {
  f <- file.path(dir, paste0(name, ".docx"))
  expect_true(file.exists(f))
  txt <- officer::docx_summary(officer::read_docx(f))$text
  trimws(txt[nzchar(txt)])
}

test_that("specimens are grouped by species and place, merging duplicates", {
  dir <- withr::local_tempdir()
  res <- write_specimens(dir)

  # The record without species is skipped
  expect_equal(res$species_name, c("Ouratea concinna", "Ouratea crenulata"))
  concinna <- strsplit(res$specimens_text[1], "\n")[[1]]
  # Duplicates of Cardoso 1350 in HUEFS and RB are merged, without type status
  expect_equal(concinna, c(
    paste("Salvador, Parque da Cidade, 12°58′17″S, 38°30′05″W,",
          "5 Mar 2010 (fl), D.B.O.S. Cardoso et al. 1350 (HUEFS, RB)"),
    paste("Diamantina, Serra do Espinhaço, 18°14′24″S, 43°36′00″W,",
          "Jul 2015, A. Queiroz-Lima & D.B.O.S. Cardoso 88 (DIAM)")))
  crenulata <- strsplit(res$specimens_text[2], "\n")[[1]]
  # Bolivia before Brazil; missing values are not printed as "NA"
  # Collections without number are cited with the catalog number of the first herbarium
  expect_equal(crenulata[1], "Chiquitos, 1995, R.M. Harley s.n. (NY0099)")
  expect_match(crenulata[2], "13°30′S, 41°54′W, 12 Nov 1988 (fr)", fixed = TRUE)
  expect_false(any(grepl("NA", res$specimens_text)))

  lines <- docx_lines(dir)
  expect_true("Ouratea concinna A.Queiroz-Lima & D.B.O.S.Cardoso" %in% lines)
  expect_true(any(startsWith(lines, paste0("Representative Specimens Examined—Brazil. ",
                                           "—Bahia: Salvador"))))
  expect_true(any(grepl(". \u2014Minas Gerais: Diamantina", lines, fixed = TRUE)))
})

test_that("each journal has its own format", {
  dir <- withr::local_tempdir()
  first <- function(journal, ...) {
    strsplit(write_specimens(dir, journal, ...)$specimens_text[1], "\n")[[1]][1]
  }
  expect_match(first("Taxon"), "D.B.O.S. Cardoso & al. 1350", fixed = TRUE)
  expect_match(first("Kew"), "5 March 2010 (fl.), D. B. O. S. Cardoso et al. 1350", fixed = TRUE)
  expect_match(first("Kew"), "12°58'17\"S, 38°30'05\"W", fixed = TRUE)
  expect_match(first("Phytotaxa"), "Salvador: Parque da Cidade, ", fixed = TRUE)
  expect_match(first("Phytotaxa"), "5 March 2010", fixed = TRUE)
  expect_match(first("Brittonia"), "5 Mar 2010 [fl]", fixed = TRUE)
  expect_match(first("candollea"), "12°58'17\"S 38°30'05\"W, 5.III.2010, fl., Cardoso et al. 1350",
               fixed = TRUE)
  expect_match(first("Edimburgh"), "5 iii 2010 (fl), Cardoso et al. 1350", fixed = TRUE)
  expect_match(first("Systematics and Biodiversity"), "5 March 2010 (fl), D. B. O. S. Cardoso", fixed = TRUE)
  expect_match(first("Brazilian Journal of Botany"), "05 Mar 2010 (fl), Cardoso et al. 1350",
               fixed = TRUE)
  expect_match(first("Nordic"), "5 Mar. 2010, (fl.), Cardoso et al. 1350", fixed = TRUE)
  expect_match(first("Rodriguesia"), "12º58’17”S, 38º30’05”W, 5.III.2010, fl.,",
               fixed = TRUE)
  expect_match(first("Brittonia", catalog_numbers = TRUE),
               "(HUEFS no. HUEFS000123, RB no. 456)", fixed = TRUE)

  # Rodriguesia cites the catalog number of collections without number
  res <- write_specimens(dir, "Rodriguésia")
  expect_match(res$specimens_text[2], "R.M. Harley (NY0099)", fixed = TRUE)
  lines <- docx_lines(dir)
  expect_true(any(startsWith(lines, "Additional material examined: BOLIVIA. SANTA CRUZ: Chiquitos")))

  # Nordic Journal of Botany cites the species in the paragraph of its specimens
  write_specimens(dir, "Nordic Journal of Botany")
  lines <- docx_lines(dir)
  expect_equal(lines[1], "Additional specimen examined")
  expect_true(any(startsWith(lines, "Ouratea concinna – Brazil, Bahia, Salvador, Parque")))

  expect_error(write_specimens(dir, "Annals of Botany"), "not available")
})

test_that("species are filtered, files are read, and Portuguese is used", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "records.xlsx")
  openxlsx::write.xlsx(specimens, path)

  res <- barroso_write_specimens(path = path, species = "Ouratea crenulata Engl.",
                                 language = "pt", verbose = FALSE)
  expect_equal(res$species_name, "Ouratea crenulata")
  expect_match(res$specimens_text, "12 nov 1988", fixed = TRUE)
  lines <- docx_lines(dir, "records_Systematic_Botany")
  expect_true(any(startsWith(lines, "Material representativo examinado—Bolívia.")))

  csv <- file.path(dir, "records.csv")
  utils::write.csv(specimens, csv, row.names = FALSE, fileEncoding = "UTF-8")
  expect_message(barroso_write_specimens(path = csv, journal = "Taxon"), "2 species")

  expect_warning(res <- barroso_write_specimens(df = specimens, species = "Ouratea nonexistens",
                                                dir = dir, verbose = FALSE),
                 "No records of Ouratea nonexistens")
  expect_null(res)
  expect_error(barroso_write_specimens(), "Provide the records")
  expect_error(barroso_write_specimens(df = specimens, language = "fr"), "language")
})

test_that("coordinates, dates, collectors and herbaria are formatted", {
  expect_equal(barRoso:::.format_coordinates(-12.5, -38.25), "12°30′S, 38°15′W")
  expect_equal(barRoso:::.format_coordinates(1, 2), "1°N, 2°E")
  expect_equal(barRoso:::.format_coordinates(NA, 2), "")
  expect_equal(barRoso:::.format_coordinates(95, 2), "")
  expect_equal(barRoso:::.format_coordinates(0, 0), "")
  # 41.9 degrees are 41°54′, not 41°53′60″
  expect_equal(barRoso:::.format_coordinates(-13.5, -41.9), "13°30′S, 41°54′W")
  expect_equal(barRoso:::.format_date(1, 12, 2020), "1 Dec 2020")
  expect_equal(barRoso:::.format_date(NA, "Dec", 2020), "Dec 2020")
  expect_equal(barRoso:::.format_date(NA, NA, NA), "")
  expect_equal(barRoso:::.format_date(8, 5, 1994, format = "abbr_dot"), "8 May 1994")
  expect_equal(barRoso:::.format_date(8, 9, 1994, format = "kew"), "8 Sept. 1994")
  expect_equal(barRoso:::.translate_country("Mexico", "pt"), "México")
  expect_equal(barRoso:::.translate_country("Brazil", "es"), "Brasil")
  expect_equal(barRoso:::.translate_country("Narnia", "pt"), "Narnia")

  style <- barRoso:::.journal_style("Systematic Botany")
  expect_equal(barRoso:::.format_collector(c("D. B. O. S. Cardoso", "", "R. M. Harley"),
                                           c("Etal", NA, "A. M. Giulietti"), style),
               list(name = c("D.B.O.S. Cardoso", "Anonymous", "R.M. Harley & A.M. Giulietti"),
                    etal = c("et al.", "", "")))
  expect_equal(barRoso:::.format_elevation(c("650", "650", ""), c("700", "", ""), " m"),
               c("650–700 m", "650 m", ""))
})

test_that("Acta Botanica Brasilica marks the herbaria seen", {
  dir <- withr::local_tempdir()
  res <- write_specimens(dir, "Acta Botanica Brasilica")
  expect_match(res$specimens_text[1],
               paste("12°58’17” S, 38°30’05” W, 5 March 2010, (fl.),",
                     "D.B.O.S. Cardoso et al. 1350 (HUEFS!, RB!)"), fixed = TRUE)
  expect_true(any(startsWith(docx_lines(dir), "Additional specimens examined: BRAZIL. Bahia:")))
})

test_that("the header can be chosen or omitted", {
  dir <- withr::local_tempdir()
  write_specimens(dir, "Kew Bulletin")
  expect_true(any(startsWith(docx_lines(dir), "SPECIMENS EXAMINED. BRAZIL. Bahia:")))
  write_specimens(dir, "Kew Bulletin", header = "Paratypes")
  expect_true(any(startsWith(docx_lines(dir), "PARATYPES. BRAZIL. Bahia:")))
  write_specimens(dir, "Taxon", header = "Additional specimens examined (paratypes)")
  expect_true(any(startsWith(docx_lines(dir), "Additional specimens examined (paratypes). – BRAZIL.")))
  write_specimens(dir, "Taxon", header = "")
  expect_true(any(startsWith(docx_lines(dir), "BRAZIL. BAHIA: Salvador")))
  expect_error(write_specimens(dir, header = c("a", "b")), "single text")
})

test_that("Systematic Botany has countries in bold, states in small capitals and collectors in italics", {
  dir <- withr::local_tempdir()
  write_specimens(dir)
  xml <- officer::read_docx(file.path(dir, "out.docx"))$doc_obj$get()
  ns <- xml2::xml_ns(xml)
  runs <- xml2::xml_find_all(xml, "//w:r", ns)
  text <- xml2::xml_text(runs)
  has <- function(tag) !is.na(vapply(runs, function(r) {
    n <- xml2::xml_find_first(r, paste0(".//w:", tag), ns); if (inherits(n, "xml_missing")) NA_character_ else "x"
  }, character(1)))
  expect_true(all(has("b")[text == "Brazil."]))
  expect_true(all(has("smallCaps")[text %in% c("Bahia", "Minas Gerais")]))
  expect_true(all(has("i")[text == "D.B.O.S. Cardoso et al. 1350"]))
  expect_false(any(grepl("0A0B0C", as.character(xml))))
})

test_that("collector names, et al. and numbers are in the format of each journal", {
  dir <- withr::local_tempdir()
  italic <- function(journal, pattern) {
    write_specimens(dir, journal)
    xml <- officer::read_docx(file.path(dir, "out.docx"))$doc_obj$get()
    ns <- xml2::xml_ns(xml)
    run <- xml2::xml_find_all(xml, "//w:r", ns)
    run <- run[grepl(pattern, xml2::xml_text(run))][[1]]
    xml2::xml_attr(xml2::xml_find_first(run, ".//w:i", ns), "val")
  }
  expect_equal(italic("Taxon", "^L.P. Queiroz$"), "true")
  expect_equal(italic("Taxon", "^ 4021$"), "true")
  expect_equal(italic("Acta Botanica Brasilica", "^ 4021$"), "true")
  expect_equal(italic("Kew Bulletin", "^L. P. Queiroz$"), "true")
  expect_equal(italic("Kew Bulletin", "^ 4021$"), "false")
  expect_equal(italic("Edinburgh", "^ et al.$"), "false")
  expect_equal(italic("Nordic", "^Queiroz$"), "false")
})

test_that("Plant Ecology and Evolution separates specimens by bullets", {
  dir <- withr::local_tempdir()
  res <- write_specimens(dir, "Plant Ecology and Evolution")
  expect_match(res$specimens_text[1], "5 Mar. 2010 (fl); Cardoso et al. 1350; HUEFS, RB",
               fixed = TRUE)
  expect_true(any(startsWith(docx_lines(dir),
                             "Additional material examined. BRAZIL \u2022 Bahia, Salvador, Parque")))
})

test_that("PhytoKeys writes municipalities after Mun.", {
  dir <- withr::local_tempdir()
  res <- write_specimens(dir, "PhytoKeys")
  expect_match(res$specimens_text[1],
               "Mun. Salvador, Parque da Cidade, 12\u00b058'17\"S, 38\u00b030'05\"W, 5 Mar. 2010 (fl), D.B.O.S. Cardoso et al. 1350",
               fixed = TRUE)
  expect_true(any(startsWith(docx_lines(dir), "Additional specimens examined. Brazil. Bahia: Mun. Salvador")))
})

test_that("Willdenowia cites each municipality once, with a single header", {
  dir <- withr::local_tempdir()
  two <- rbind(specimens, specimens[4, ])
  two$recordNumber[7] <- "4022"
  two$locality[7] <- "Serra das Almas"
  barroso_write_specimens(df = two, journal = "Willdenowia", dir = dir, filename = "out",
                          verbose = FALSE)
  lines <- docx_lines(dir)
  expect_equal(lines[1], "Additional specimens examined.")
  expect_true(any(startsWith(lines, "Brazil: Bahia: Municipality Salvador, Parque da Cidade")))
  crenulata <- lines[grep("Ouratea crenulata", lines) + 1]
  expect_match(crenulata, paste("Bolivia: Santa Cruz: Chiquitos, 1995, R. M. Harley s.n. (NY0099).",
                                "Brazil: Bahia: Municipality Rio de Contas, Pico das Almas"), fixed = TRUE)
  expect_match(crenulata, "L. P. Queiroz 4021 (K); Serra das Almas, 13\u00b030'S", fixed = TRUE)
})

test_that("double spaces are removed", {
  dir <- withr::local_tempdir()
  df <- specimens
  df$locality[1:2] <- "Parque  da   Cidade "
  res <- barroso_write_specimens(df = df, header = "Paratypes ", dir = dir, filename = "out",
                                 verbose = FALSE)
  expect_false(any(grepl("\\s{2,}", res$specimens_text)))
  expect_false(any(grepl("\\s{2,}", docx_lines(dir))))
  expect_match(res$specimens_text[1], "Salvador, Parque da Cidade,", fixed = TRUE)
})

test_that("all journals in the argument have a style", {
  expect_setequal(eval(formals(barroso_write_specimens)$journal),
                  stringi::stri_trans_general(names(barRoso:::.journal_styles), "Latin-ASCII"))
})

test_that("words for municipality are removed", {
  dir <- withr::local_tempdir()
  df <- specimens
  df$municipality[1:2] <- "Salvador Mun."
  res <- write_specimens(dir, "PhytoKeys")
  expect_match(res$specimens_text[1], "^Mun. Salvador, Parque da Cidade")
})

test_that("Kew Bulletin writes the header and countries in bold Arial", {
  dir <- withr::local_tempdir()
  write_specimens(dir, "Kew Bulletin")
  xml <- officer::read_docx(file.path(dir, "out.docx"))$doc_obj$get()
  ns <- xml2::xml_ns(xml)
  runs <- xml2::xml_find_all(xml, "//w:r", ns)
  font <- function(text) {
    r <- runs[xml2::xml_text(runs) == text][[1]]
    c(xml2::xml_attr(xml2::xml_find_first(r, ".//w:rFonts", ns), "ascii"),
      xml2::xml_attr(xml2::xml_find_first(r, ".//w:b", ns), "val"))
  }
  expect_equal(font("SPECIMENS EXAMINED"), c("Arial", "true"))
  expect_equal(font("BRAZIL."), c("Arial", "true"))
  expect_equal(font("Bahia"), c("Times New Roman", "false"))
})

test_that("Botanical Journal of the Linnean Society has dates like 5.iii.2010", {
  dir <- withr::local_tempdir()
  res <- write_specimens(dir, "Botanical Journal of the Linnean Society")
  expect_match(res$specimens_text[1], "5.iii.2010 (fl), D.B.O.S. Cardoso et al. 1350", fixed = TRUE)
  expect_true(any(startsWith(docx_lines(dir), "Representative specimens examined: BRAZIL. Bahia: Salvador")))
  expect_equal(barRoso:::.format_date(8, 5, 1994, format = "roman_lower_dots"), "8.v.1994")
})

test_that("Annals of the Missouri Botanical Garden has states in bold", {
  dir <- withr::local_tempdir()
  res <- write_specimens(dir, "Annals of the Missouri Botanical Garden")
  expect_match(res$specimens_text[1], "5 Mar. 2010 (fl.), D. B. O. S. Cardoso et al. 1350 (HUEFS, RB)",
               fixed = TRUE)
  expect_true(any(startsWith(docx_lines(dir), "Additional specimens examined. BRAZIL. Bahia: Salvador")))
})

test_that("phenology comes from the column phenology, in the format of each journal", {
  dir <- withr::local_tempdir()
  df <- specimens
  df$phenology <- c("fl, fr", NA, "flower", "st", "fruit", NA)
  first <- function(journal, ...) {
    res <- barroso_write_specimens(df = df, journal = journal, dir = dir, filename = "out",
                                   verbose = FALSE, ...)
    strsplit(res$specimens_text, "\n")[[1]]
  }
  expect_match(first("Systematic Botany")[1], "5 Mar 2010 (fl, fr), ", fixed = TRUE)
  expect_match(first("Systematic Botany")[2], "Jul 2015 (fl), ", fixed = TRUE)
  expect_match(first("Taxon")[1], "(fl., fr.)", fixed = TRUE)
  expect_match(first("Annals of the Missouri")[1], "5 Mar. 2010 (fl./fr.), ", fixed = TRUE)
  expect_match(first("Brittonia")[1], "[fl, fr]", fixed = TRUE)
  expect_match(first("Brazilian Journal of Botany")[1], "(fl and fr)", fixed = TRUE)
  expect_match(first("Brazilian Journal of Botany", language = "pt")[1], "(fl e fr)", fixed = TRUE)
  expect_match(first("Rodriguesia")[1], "5.III.2010, fl. and fr., ", fixed = TRUE)
  expect_match(first("Nordic")[1], "(fl. fr.)", fixed = TRUE)
  expect_match(first("Acta Amazonica")[1], "Fl., fr. 5 March 2010, ", fixed = TRUE)
  expect_equal(barRoso:::.format_phenology(c("st", "Estéril", "fl, st", "fértil", NA),
                                           barRoso:::.journal_style("Hoehnea"), "pt"),
               c("est.", "est.", "fl.", "", ""))
  expect_equal(barRoso:::.format_phenology(c("vegetative", "flower, veg"),
                                           barRoso:::.journal_style("Darwiniana"), "es"),
               c("veg", "fl"))
})

test_that("the new journals have their own format", {
  dir <- withr::local_tempdir()
  first <- function(journal, ...) {
    strsplit(write_specimens(dir, journal, ...)$specimens_text[1], "\n")[[1]][1]
  }
  expect_match(first("Hoehnea"), "12°58’17”S, 38°30’05”W, 5-III-2010 (fl.), D.B.O.S. Cardoso et al. 1350",
               fixed = TRUE)
  expect_match(first("Plant Systematics and Evolution"),
               "Cardoso et al. 1350 (HUEFS!, RB!)", fixed = TRUE)
  expect_match(first("Plant Systematics and Evolution"), "12° 58′ 17″ S, 38° 30′ 05″ W, 5 Mar 2010 (fl.)",
               fixed = TRUE)
  expect_match(first("Acta Amazonica"), "Salvador, Parque da Cidade. 12º58’17”S, 38º30’05”W. Fl. 5 March 2010, D.B.O.S. Cardoso et al. 1350 (HUEFS, RB)",
               fixed = TRUE)
  expect_match(first("Darwiniana", language = "es"), "12º58’17” S, 38º30’05” O, 5-III-2010 (fl), ",
               fixed = TRUE)
  expect_match(first("Botany Letters"), "5 March 2010 (fl.), D.B.O.S. Cardoso et al. 1350", fixed = TRUE)
  expect_match(first("Feddes"), "5 Mar. 2010 (fl), Cardoso et al. 1350", fixed = TRUE)
  expect_match(first("Acta Botanica Mexicana"), "municipio Salvador, Parque da Cidade, 12°58'17''S, 38°30'05''W, 5.III.2010 (fl)",
               fixed = TRUE)
  lines <- docx_lines(dir)
  expect_true(any(grepl("Specimens examined: BRAZIL. Bahia, municipio Salvador", lines, fixed = TRUE)))

  write_specimens(dir, "Darwiniana", language = "es")
  expect_true(any(startsWith(docx_lines(dir), "Material examinado: BRASIL. Bahia, Salvador")))
  write_specimens(dir, "Hoehnea", language = "pt")
  expect_true(any(startsWith(docx_lines(dir), "Material examinado: BRASIL. Bahia: Salvador")))
  write_specimens(dir, "Botany Letters")
  expect_true(any(grepl("examined: BRAZIL. \u2014 BAHIA: Salvador", docx_lines(dir), fixed = TRUE)))
})

test_that("repeated places are written as l.c. or loc. cit.", {
  dir <- withr::local_tempdir()
  df <- specimens[c(1, 1, 1), ]
  df$recordNumber <- c("1", "2", "3")
  df$locality <- c("Parque da Cidade", "Parque da Cidade", "Dunas")
  df$decimalLatitude[3] <- NA
  res <- barroso_write_specimens(df = df, journal = "Acta Botanica Mexicana", dir = dir,
                                 filename = "out", verbose = FALSE)
  expect_true(any(grepl("(HUEFS); loc. cit., 5.III.2010 (fl), D. B. O. S. Cardoso et al. 2",
                        docx_lines(dir), fixed = TRUE)))
  barroso_write_specimens(df = df, journal = "Hoehnea", dir = dir, filename = "out", verbose = FALSE)
  expect_true(any(grepl("(HUEFS); l.c., Dunas, 5-III-2010 (fl.), D.B.O.S. Cardoso et al. 3",
                        docx_lines(dir), fixed = TRUE)))
})

test_that("one_per_municipality keeps the most complete specimen of each municipality", {
  dir <- withr::local_tempdir()
  df <- specimens[c(1, 1, 1, 3, 6), ]
  df$recordNumber <- c("1", "2", "3", "88", "1")
  df$locality[2] <- "Parque da Cidade, trilha principal"
  df$day[1] <- NA
  df$decimalLatitude[3] <- NA
  df$specificEpithet[5] <- "concinna"
  res <- barroso_write_specimens(df = df, dir = dir, filename = "out", verbose = FALSE,
                                 one_per_municipality = TRUE)
  lines <- strsplit(res$specimens_text, "\n")[[1]]
  # Number 2 has all fields, Diamantina has a single specimen, and the
  # specimen without municipality is excluded
  expect_length(lines, 2)
  expect_false(any(grepl("J. Silva", lines)))
  expect_true(any(grepl("trilha principal", lines)))
  expect_false(any(grepl("et al. 3 ", lines)))
  expect_true(any(grepl("Diamantina", lines)))
  all <- barroso_write_specimens(df = df, dir = dir, filename = "out", verbose = FALSE)
  expect_length(strsplit(all$specimens_text, "\n")[[1]], 5)
})
