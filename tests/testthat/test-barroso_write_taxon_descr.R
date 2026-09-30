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
  res <- barroso_write_taxon_descr(write_mini(dir), species_cols = 1:3, character_cols = 4:11,
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
  res <- barroso_write_taxon_descr(write_mini(dir), species_cols = 1:3, character_cols = 4:11,
                                   dir = dir, verbose = FALSE)
  expect_match(res$description_plain[1], "Fruit black.", fixed = TRUE)
  expect_match(res$description_plain[2], "Fruit red.", fixed = TRUE)
  expect_match(res$description_plain[1], "stamen 10", fixed = TRUE)
  expect_match(res$description_plain[2], "stamen 12", fixed = TRUE)
})

test_that("species can be filtered and averages computed", {
  dir <- withr::local_tempdir()
  expect_message(
    res <- barroso_write_taxon_descr(write_mini(dir), species_cols = c("Genus", "Species", "Author"),
                                     character_cols = 4:11,
                                     species_filter = "Ouratea concinna",
                                     avg_cols = c("LEAF BLADE length (cm)", "LEAF BLADE width (cm)"),
                                     avg_min_n = 2, approx_char = "c.",
                                     dir = dir, filename = "filtered.docx"),
    "Filtering to 3 species")
  expect_equal(nrow(res), 1)
  # Mean of 10, 12, 14 and 11 is 11.75; of 3, 4, 5 and 4 is 4
  expect_match(res$description_plain, "10–14 × 3–5 cm, average 11.8 × 4 cm", fixed = TRUE)
  expect_true(file.exists(file.path(dir, format(Sys.time(), "%d%b%Y"), "filtered.docx")))

  # Too few values: no averages
  res <- barroso_write_taxon_descr(write_mini(dir), species_cols = 1:3, character_cols = 4:11,
                                   avg_cols = "LEAF BLADE length (cm)", avg_min_n = 10,
                                   dir = dir, verbose = FALSE)
  expect_no_match(res$description_plain[1], "average")

  expect_warning(
    barroso_write_taxon_descr(write_mini(dir), species_cols = 1:3, character_cols = 4:11,
                              species_filter = "Ouratea nonexistens", dir = dir, verbose = FALSE),
    "No species in the data match")
})

test_that("the output is named after the spreadsheet when no filename or dir is given", {
  dir <- withr::local_tempdir()
  path <- write_mini(dir)
  withr::local_dir(dir)
  res <- barroso_write_taxon_descr(path, species_cols = 1:3, character_cols = 4:11,
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
  path <- file.path(dir, "morphological_dataset.xlsx")
  openxlsx::write.xlsx(morphological_dataset, path)
  char_cols <- which(names(morphological_dataset) == "HABIT"):ncol(morphological_dataset)

  res <- barroso_write_taxon_descr(path, species_cols = c("Genus", "Species", "Author"),
                                   character_cols = char_cols, dir = dir, verbose = FALSE,
                                   avg_cols = c("LEAF BLADE length (cm)", "LEAF BLADE width (cm)"))
  expect_equal(nrow(res), length(unique(paste(morphological_dataset$Genus,
                                              morphological_dataset$Species))))
  expect_true(all(grepl("^Ouratea ", res$species_name)))
  for (organ in c("Leaf", "Inflorescence", "Flower", "Fruit")) {
    expect_true(any(grepl(paste0("\\. ", organ, " "), res$description_plain)), info = organ)
  }
  # Ranges use en-dashes, never hyphens between numbers
  expect_false(any(grepl("[0-9]-[0-9]", res$description_plain)))
})

test_that("barroso_write_taxon_descr validates its input", {
  dir <- withr::local_tempdir()
  path <- write_mini(dir)
  expect_error(barroso_write_taxon_descr(c("a.xlsx", "b.xlsx"), species_cols = 1, character_cols = 2,
                                         dir = dir), "non-empty character scalar")
  expect_error(barroso_write_taxon_descr(path, species_cols = 1:3, character_cols = 4:99, dir = dir),
               "out of range")
  expect_error(barroso_write_taxon_descr(path, species_cols = 1:3, character_cols = 4:5,
                                         avg_cols = "LEAF BLADE length (cm)", dir = dir),
               "subset of character_cols|Missing columns for avg_cols")
  expect_error(barroso_write_taxon_descr(path, species_cols = 1:3, character_cols = "SEED shape",
                                         dir = dir), "Missing columns")
})
