author <- "A.Queiroz-Lima & D.B.O.S.Cardoso"
mini <- data.frame(
  Genus = "Ouratea",
  Species = c("concinna", "concinna", "concinna", "crenulata"),
  Author = author,
  HABIT = c("Shrub", "Shrub", "Treelet", "Tree"),
  `HABIT height (m)` = c("1-2", "3", "2.5", "5"),
  `LEAF BLADE length (cm)` = c("10-12", "14", "11", "20"),
  `LEAF BLADE width (cm)` = c("3-4", "5", "4", "8"),
  `LEAF BLADE shape` = c("elliptic", "elliptic.", "oblong", "obovate"),
  `LEAF BLADE APEX` = c("acute", "acute", NA, "obtuse"),
  `FRUIT color` = c("black", NA, "black", "red"),
  `FLOWER STAMEN number` = c(10, 10, 10, 12),
  check.names = FALSE)

write_mini <- function(dir, df = mini) {
  path <- file.path(dir, "mini.xlsx")
  openxlsx::write.xlsx(df, path, overwrite = TRUE)
  path
}

docx_text <- function(dir) {
  f <- list.files(dir, pattern = "[.]docx$", recursive = TRUE, full.names = TRUE)
  expect_length(f, 1)
  officer::docx_summary(officer::read_docx(f))$text
}

test_that("duplicate specimens are merged into one description per species", {
  dir <- withr::local_tempdir()
  res <- barroso_write_descriptions(write_mini(dir), species_cols = 1:3, character_cols = 4:11,
                                   dir = dir, filename = "descr", verbose = FALSE)

  expect_equal(res$species_name, paste("Ouratea", c("concinna", "crenulata"), author))
  concinna <- res$description_plain[1]
  # Habit first, without organ name, with ranges merged across specimens
  expect_match(concinna, "\n\nShrub or Treelet 1–3 m tall.", fixed = TRUE)
  # Length and width merged into one measurement, final punctuation removed from cells
  expect_match(concinna, "Leaf blade elliptic or oblong, 10–14 × 3–5 cm, apex acute.", fixed = TRUE)
  # Values without variation get approx_char
  expect_match(res$description_plain[2], "Tree ca. 5 m tall", fixed = TRUE)
  expect_match(res$description_plain[2], "ca. 20 × 8 cm", fixed = TRUE)

  expect_true(file.exists(file.path(dir, format(Sys.time(), "%d%b%Y"), "descr.docx")))
  txt <- docx_text(dir)
  expect_equal(trimws(txt[1]), paste("Ouratea concinna", author))
  expect_match(txt[2], "^Shrub or Treelet")
})

test_that("trait values are never mistaken for formatting settings", {
  # Regression test: "black" and whole numbers from 8 to 16 were dropped
  dir <- withr::local_tempdir()
  res <- barroso_write_descriptions(write_mini(dir), species_cols = 1:3, character_cols = 4:11,
                                   dir = dir, verbose = FALSE)
  expect_match(res$description_plain[1], "Fruit black.", fixed = TRUE)
  expect_match(res$description_plain[2], "Fruit red.", fixed = TRUE)
  expect_match(res$description_plain[1], "stamen 10", fixed = TRUE)
  expect_match(res$description_plain[2], "stamen 12", fixed = TRUE)
})

test_that("species can be filtered and averages computed", {
  dir <- withr::local_tempdir()
  expect_message(
    res <- barroso_write_descriptions(write_mini(dir), species_cols = c("Genus", "Species", "Author"),
                                     character_cols = 4:11,
                                     species_filter = "Ouratea concinna",
                                     avg_cols = c("LEAF BLADE length (cm)", "LEAF BLADE width (cm)"),
                                     avg_min_n = 2, approx_char = "c.",
                                     dir = dir, filename = "filtered.docx"),
    "keeping 3 specimens of 1 species: Ouratea concinna")
  expect_equal(nrow(res), 1)
  # Mean of 10, 12, 14 and 11 is 11.75; of 3, 4, 5 and 4 is 4
  expect_match(res$description_plain, "10–14 × 3–5 cm, average 11.8 × 4 cm", fixed = TRUE)
  expect_true(file.exists(file.path(dir, format(Sys.time(), "%d%b%Y"), "filtered.docx")))

  # Too few values: no averages
  res <- barroso_write_descriptions(write_mini(dir), species_cols = 1:3, character_cols = 4:11,
                                   avg_cols = "LEAF BLADE length (cm)", avg_min_n = 10,
                                   dir = dir, verbose = FALSE)
  expect_no_match(res$description_plain[1], "average")

  expect_warning(
    barroso_write_descriptions(write_mini(dir), species_cols = 1:3, character_cols = 4:11,
                              species_filter = "Ouratea nonexistens", dir = dir, verbose = FALSE),
    "No species in the data match")
})

test_that("the output is named after the spreadsheet when no filename or dir is given", {
  dir <- withr::local_tempdir()
  path <- write_mini(dir)
  withr::local_dir(dir)
  res <- barroso_write_descriptions(path, species_cols = 1:3, character_cols = 4:11,
                                   font_family = "Arial", font_size = 11,
                                   species_bold = FALSE, species_italic = FALSE,
                                   group_bold = FALSE, group_italic = TRUE,
                                   description_bold = TRUE, description_italic = TRUE,
                                   verbose = FALSE)
  expect_true(file.exists(file.path("mini", format(Sys.time(), "%d%b%Y"), "mini_descriptions.docx")))
  expect_equal(nrow(res), 2)
})

test_that("the example dataset is fully described", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "chars_dataset.xlsx")
  openxlsx::write.xlsx(chars_dataset, path)
  char_cols <- which(names(chars_dataset) == "HABIT"):ncol(chars_dataset)

  res <- barroso_write_descriptions(path, species_cols = c("Genus", "Species", "Author"),
                                   character_cols = char_cols, dir = dir, verbose = FALSE,
                                   avg_cols = c("LEAF BLADE length (cm)", "LEAF BLADE width (cm)"))
  expect_equal(nrow(res), length(unique(paste(chars_dataset$Genus,
                                              chars_dataset$Species))))
  expect_true(all(grepl("^Ouratea ", res$species_name)))
  for (organ in c("Leaf", "Inflorescence", "Flower", "Fruit")) {
    expect_true(any(grepl(paste0("\\. ", organ, " "), res$description_plain)), info = organ)
  }
  # Ranges use en-dashes, never hyphens between numbers
  expect_false(any(grepl("[0-9]-[0-9]", res$description_plain)))
})

test_that("barroso_write_descriptions validates its input", {
  dir <- withr::local_tempdir()
  path <- write_mini(dir)
  expect_error(barroso_write_descriptions(c("a.xlsx", "b.xlsx"), species_cols = 1, character_cols = 2,
                                         dir = dir), "single text")
  expect_error(barroso_write_descriptions(path, species_cols = 1:3, character_cols = 4:99, dir = dir),
               "out of range")
  expect_error(barroso_write_descriptions(path, species_cols = 1:3, character_cols = 4:5,
                                         avg_cols = "LEAF BLADE length (cm)", dir = dir),
               "subset of character_cols|Missing columns for avg_cols")
  expect_error(barroso_write_descriptions(path, species_cols = 1:3, character_cols = "SEED shape",
                                         dir = dir), "Missing columns")
})

test_that("measurements are pooled across specimens, with rare values between parentheses", {
  f <- barRoso:::.format_measurements
  # Values of the same specimen separated by "|"
  expect_equal(f(c("4.5 | 7 | 11 | 14", "6")), "4.5\u201314")
  expect_equal(f(c("3", "3")), "3")
  expect_equal(f("0.10 | 0.25"), "0.1\u20130.25")
  expect_true(is.na(f("absent")))
  # Rare values marked in the spreadsheet are kept, with hyphens or en dashes
  expect_equal(f("(5-)7-38(-45)"), "(5\u2013)7\u201338(\u201345)")
  expect_equal(f(c("(2\u2013)6 | 9", "12")), "(2\u2013)6\u201312")
  # ... but not when they are within the range of the other values
  expect_equal(f(c("(7-)8", "5 | 9")), "5\u20139")
  # Automatic rare values from 10 values, with measured values as limits
  x <- as.character(c(2, 6, 7, 8, 9, 10, 12, 15, 20, 40, 55))
  expect_equal(f(x, extremes = c(0.1, 0.9)), "(2\u2013)6\u201340(\u201355)")
  expect_equal(f(x, extremes = NULL), "2\u201355")
  expect_equal(f(x[1:9], extremes = c(0.1, 0.9)), "2\u201320")
  expect_equal(f(x, extremes = c(0.1, 0.9), extremes_min_n = 20), "2\u201355")
  expect_equal(f(as.character(1:20), extremes = c(0.1, 0.9)), "(1\u2013)3\u201318(\u201320)")
})

test_that("barroso_write_descriptions writes rare values and checks extremes", {
  dir <- withr::local_tempdir()
  df <- data.frame(Genus = "Ouratea", Species = c("hostilis", "hostilis", "nitida"),
                   `LEAF length (cm)` = c("2 | 6 | 7 | 8 | 9 | 10", "12 | 15 | 20 | 40 | 55", "(1-)3 | 4"),
                   `LEAF width (cm)` = c("1", "2", "1 | 1.5"), check.names = FALSE)
  path <- file.path(dir, "leaves.xlsx")
  openxlsx::write.xlsx(df, path)
  r <- suppressMessages(barroso_write_descriptions(path, species_cols = 1:2, character_cols = 3:4,
                                                   dir = dir, verbose = FALSE))
  expect_match(r$description_plain[1], "(2\u2013)6\u201340(\u201355) \u00d7 1\u20132 cm", fixed = TRUE)
  expect_match(r$description_plain[2], "(1\u2013)3\u20134 \u00d7 1\u20131.5 cm", fixed = TRUE)
  r <- suppressMessages(barroso_write_descriptions(path, species_cols = 1:2, character_cols = 3:4,
                                                   extremes = NULL, dir = dir, verbose = FALSE))
  expect_match(r$description_plain[1], "2\u201355 \u00d7 1\u20132 cm", fixed = TRUE)
  expect_error(barroso_write_descriptions(path, species_cols = 1:2, character_cols = 3:4,
                                          extremes = c(0.9, 0.1), dir = dir), "two percentiles")
  expect_error(barroso_write_descriptions(path, species_cols = 1:2, character_cols = 3:4,
                                          extremes_min_n = 1, dir = dir), "at least 2")
})

test_that("counts become ranges followed by the last word of the column name", {
  df <- data.frame(a = c("6 | 12", "(4-)7", "8"), b = c("10", "8", NA), c = c("1 pair", "2", NA),
                   d = c("5", "abc", NA))
  expect_equal(barRoso:::.count_cols(df, c("a", "b", "c", "d")), c("a", "b"))
  dir <- withr::local_tempdir()
  df <- data.frame(Genus = "Ouratea", Species = c("hostilis", "hostilis", "nitida"),
                   `LEAF SECONDARY-VENATION pairs` = c("6 | 12", "7 | 9", "(3-)4"),
                   `LEAF SECONDARY-VENATION` = "curved distally towards the marginal vein",
                   `FLOWER STAMEN number` = c("10", "8", "10"),
                   `INFLORESCENCE flowers` = c("10 | 25", "15", NA), check.names = FALSE)
  path <- file.path(dir, "counts.xlsx")
  openxlsx::write.xlsx(df, path)
  r <- suppressMessages(barroso_write_descriptions(path, species_cols = 1:2, character_cols = 3:6,
                                                   dir = dir, verbose = FALSE))
  expect_match(r$description_plain[1], "6\u201312 pairs, curved distally towards the marginal vein", fixed = TRUE)
  expect_match(r$description_plain[1], "stamen 8\u201310", fixed = TRUE)
  expect_match(r$description_plain[1], "10\u201325 flowers", fixed = TRUE)
  expect_match(r$description_plain[2], "(3\u2013)4 pairs", fixed = TRUE)
})
