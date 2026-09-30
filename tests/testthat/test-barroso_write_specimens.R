specimens <- data.frame(
  Genus = "Ouratea",
  Species = c("concinna", "concinna", "concinna", "crenulata", "crenulata"),
  Author = "A.Queiroz-Lima & D.B.O.S.Cardoso",
  recordedBy = c("D. Cardoso", "D. Cardoso", "A. Queiroz-Lima", "L. P. Queiroz", "R. M. Harley"),
  recordNumber = c("1350", "1350", "88", "4021", NA),
  country = c("Brazil", "Brazil", "Brazil", "Brazil", "Bolivia"),
  stateProvince = c("Bahia", "Bahia", "Minas Gerais", "Bahia", "Santa Cruz"),
  municipality = c("Salvador", "Salvador", "Diamantina", "Rio de Contas", NA),
  locality = c("Parque da Cidade", "Parque da Cidade", "Serra do Espinhaço", "Pico das Almas", "Chiquitos"),
  decimalLatitude = c(-12.9714, -12.9714, -18.24, -13.5, NA),
  decimalLongitude = c(-38.5014, -38.5014, -43.6, -41.9, NA),
  day = c(5, 5, NA, 12, NA),
  month = c(3, 3, 7, 11, NA),
  year = c(2010, 2010, 2015, 1988, 1995),
  collectionCode = c("HUEFS", "RB", "DIAM", "K", "NY"),
  typeStatus = c("holotype", "isotype", NA, NA, NA))

write_specimens_xlsx <- function(dir, df = specimens) {
  path <- file.path(dir, "specimens.xlsx")
  openxlsx::write.xlsx(df, path, overwrite = TRUE)
  path
}

docx_lines <- function(dir, name) {
  f <- file.path(dir, format(Sys.time(), "%d%b%Y"), paste0(name, ".docx"))
  expect_true(file.exists(f))
  txt <- officer::docx_summary(officer::read_docx(f))$text
  trimws(txt[nzchar(txt)])
}

test_that("specimens are grouped by species and place, merging duplicates", {
  dir <- withr::local_tempdir()
  res <- barroso_write_specimens(write_specimens_xlsx(dir), species_cols = 1:3,
                                 dir = dir, filename = "en", verbose = FALSE)

  expect_equal(res$species_name, c("Ouratea concinna", "Ouratea crenulata"))
  concinna <- strsplit(res$specimens_text[1], "\n")[[1]]
  # Duplicates of D. Cardoso 1350 in HUEFS and RB are merged, keeping each type status
  expect_equal(concinna[1], paste("Parque da Cidade, [holotype HUEFS, isotype RB],",
                                  "12°58′17.0″S, 38°30′5.0″W, 5 Mar 2010, D. Cardoso 1350 (HUEFS, RB)"))
  # Without day: month and year only
  expect_match(concinna[2], "Jul 2015, A. Queiroz-Lima 88 (DIAM)", fixed = TRUE)

  crenulata <- strsplit(res$specimens_text[2], "\n")[[1]]
  # 41.9 degrees are 41°54′, not 41°53′60″
  expect_match(crenulata[1], "13°30′S, 41°54′W, 12 Nov 1988", fixed = TRUE)
  # Missing coordinates, month and number are not printed as "NA"
  expect_equal(crenulata[2], "Chiquitos, 1995, R. M. Harley s.n. (NY)")
  expect_false(any(grepl("NA", res$specimens_text)))

  lines <- docx_lines(dir, "en")
  expect_true("Brazil. —BAHIA: Salvador" %in% lines)
  expect_true("Brazil. —MINAS GERAIS: Diamantina" %in% lines)
  expect_true("Bolivia. —SANTA CRUZ:" %in% lines)
})

test_that("Portuguese output translates months and countries", {
  dir <- withr::local_tempdir()
  res <- barroso_write_specimens(write_specimens_xlsx(dir), species_cols = 1:3, language = "pt",
                                 dir = dir, filename = "pt", verbose = FALSE)
  expect_match(res$specimens_text[1], "5 mar 2010", fixed = TRUE)
  expect_match(res$specimens_text[1], "jul 2015", fixed = TRUE)
  lines <- docx_lines(dir, "pt")
  expect_true("Brasil. —BAHIA: Salvador" %in% lines)
  expect_true("Bolívia. —SANTA CRUZ:" %in% lines)
})

test_that("journal style, header and formatting options are accepted", {
  dir <- withr::local_tempdir()
  path <- write_specimens_xlsx(dir)
  barroso_write_specimens(path, species_cols = 1:3, journal = "Taxon",
                          add_representative = FALSE, font_family = "Arial", font_size = 10,
                          species_bold = FALSE, species_italic = FALSE,
                          country_bold = FALSE, state_smallcaps = FALSE,
                          dir = dir, filename = "taxon", verbose = FALSE)
  with_header <- docx_lines(dir, "taxon")
  barroso_write_specimens(path, species_cols = 1:3, dir = dir, filename = "sb", verbose = FALSE)
  expect_true("Ouratea concinna" %in% with_header)
  expect_gt(length(docx_lines(dir, "sb")), length(with_header))
})

test_that("species can be filtered, and custom column names are used", {
  dir <- withr::local_tempdir()
  custom <- specimens
  names(custom)[names(custom) %in% c("recordedBy", "recordNumber", "country")] <-
    c("Collector", "Number", "Country")
  path <- write_specimens_xlsx(dir, custom)

  expect_message(
    res <- barroso_write_specimens(path, species_cols = 1:3,
                                   species_filter = "Ouratea crenulata",
                                   colname_recordedBy = "Collector",
                                   colname_recordNumber = "Number",
                                   colname_country = "Country",
                                   dir = dir, filename = "filtered"),
    "Filtered to 2 specimens")
  expect_equal(res$species_name, "Ouratea crenulata")
  expect_match(res$specimens_text, "L. P. Queiroz 4021 (K)", fixed = TRUE)

  expect_warning(
    res <- barroso_write_specimens(path, species_cols = 1:3, species_filter = "Ouratea nonexistens",
                                   colname_recordedBy = "Collector", colname_recordNumber = "Number",
                                   colname_country = "Country", dir = dir, verbose = FALSE),
    "No specimens after filtering")
  expect_null(res)
})

test_that("missing columns are reported and skipped", {
  dir <- withr::local_tempdir()
  minimal <- specimens[, c("Genus", "Species", "Author", "locality")]
  path <- write_specimens_xlsx(dir, minimal)
  res <- suppressWarnings(
    barroso_write_specimens(path, species_cols = 1:3, dir = dir, filename = "minimal", verbose = FALSE))
  expect_equal(nrow(res), 2)
  expect_match(res$specimens_text[1], "Parque da Cidade", fixed = TRUE)
  w <- capture_warnings(barroso_write_specimens(path, species_cols = 1:3, dir = dir, verbose = FALSE))
  expect_true(any(grepl("Column 'recordedBy' .* not found in data", w)))

  withr::local_dir(dir)
  suppressWarnings(barroso_write_specimens(path, species_cols = 1:3, verbose = FALSE))
  expect_true(dir.exists("specimens"))
})

test_that("coordinates and dates are formatted", {
  expect_equal(barRoso:::.format_coordinates(-12.5, -38.25), "12°30′S, 38°15′W")
  expect_equal(barRoso:::.format_coordinates(1, 2), "1°N, 2°E")
  expect_equal(barRoso:::.format_coordinates(NA, 2), "")
  expect_equal(barRoso:::.format_coordinates(95, 2), "")
  expect_equal(barRoso:::.format_date(1, 12, 2020), "1 Dec 2020")
  expect_equal(barRoso:::.format_date(NA, "Dec", 2020), "Dec 2020")
  expect_equal(barRoso:::.format_date(NA, NA, NA), "")
  expect_equal(barRoso:::.translate_country("Mexico"), "México")
  expect_equal(barRoso:::.translate_country("Japan"), "Japan")
})
