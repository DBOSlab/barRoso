species <- data.frame(Genus = c("Ouratea", "Ouratea", "Ouratea"),
                      Species = c("acicularis", "acuminata", "cassinefolia"),
                      Author = c("R.G.Chacon & K.Yamam.", "(DC.) Engl.", "(DC.) Engl."))

groups <- c("Leguminosae-Papilionoideae", "Leguminosae-Caesalpinioideae",
            "Leguminosae-Mimosoideae", "Asteraceae", "Ochnaceae", "Orchidaceae")

read_template <- function(path) as.data.frame(readxl::read_excel(path))

test_that("a generic template keeps the species and adds empty character columns", {
  dir <- withr::local_tempdir()
  path <- barroso_add_char_template(species, filename = "Ouratea", dir = dir, verbose = FALSE)

  expect_true(file.exists(path))
  expect_equal(basename(path), "Ouratea.xlsx")
  out <- read_template(path)
  expect_equal(out[, 1:3], species)
  expect_gt(ncol(out), 50)
  expect_true(all(is.na(out[, -(1:3)])))
  for (organ in c("HABIT", "STIPULE", "LEAF", "INFLORESCENCE", "FLOWER", "FRUIT", "SEED")) {
    expect_true(any(startsWith(names(out), organ)), info = organ)
  }
})

test_that("each plant group has its own template", {
  dir <- withr::local_tempdir()
  cols <- lapply(groups, function(g) {
    path <- barroso_add_char_template(species, plant_group = g, dir = dir, verbose = FALSE)
    expect_equal(basename(path), paste0(gsub("[^A-Za-z0-9._-]+", "_", g), "_template.xlsx"))
    names(read_template(path))
  })
  names(cols) <- groups
  generic <- names(read_template(
    barroso_add_char_template(species, dir = dir, verbose = FALSE, filename = "generic")))

  # Every group differs from the generic template and from each other
  for (g in groups) expect_false(identical(cols[[g]], generic), info = g)
  expect_equal(length(unique(lapply(cols, sort))), length(groups))

  expect_true(any(grepl("STANDARD|WING|KEEL", cols[["Leguminosae-Papilionoideae"]])))
  expect_true(any(grepl("CAPITULUM|PAPPUS|FLORET", cols[["Asteraceae"]])))
  expect_true(any(grepl("LABELLUM|COLUMN", cols[["Orchidaceae"]])))
})

test_that("templates can be read from Excel, with species columns and without formatting", {
  dir <- withr::local_tempdir()
  xlsx <- file.path(dir, "species.xlsx")
  openxlsx::write.xlsx(list(first = species[1, ], second = species), xlsx)

  expect_message(
    path <- barroso_add_char_template(xlsx, sheet = "second", species_cols = 1:2,
                                      format_excel = FALSE, dir = dir, filename = "fromxlsx"),
    "Reading Excel file")
  expect_equal(nrow(read_template(path)), 3)

  path <- suppressMessages(barroso_add_char_template(xlsx, sheet = 1, dir = dir, filename = "sheet1"))
  expect_equal(nrow(read_template(path)), 1)
})

test_that("user columns with the same name as template columns are kept", {
  dir <- withr::local_tempdir()
  generic <- names(read_template(
    barroso_add_char_template(species, dir = dir, verbose = FALSE, filename = "generic")))
  filled <- species
  filled[[generic[4]]] <- c("tree", "shrub", "tree")

  expect_message(path <- barroso_add_char_template(filled, dir = dir, filename = "filled"),
                 "Skipping template columns")
  out <- read_template(path)
  expect_equal(out[[generic[4]]], c("tree", "shrub", "tree"))
  expect_equal(sum(names(out) == generic[4]), 1)
})

test_that("a filled template can be described by barroso_write_taxon_descr()", {
  dir <- withr::local_tempdir()
  path <- barroso_add_char_template(species[1:2, ], dir = dir, filename = "tofill", verbose = FALSE)
  tmpl <- read_template(path)
  habit <- grep("^HABIT$", names(tmpl), value = TRUE)
  height <- grep("^HABIT.height", names(tmpl), value = TRUE)[1]
  expect_length(habit, 1)
  tmpl[[habit]] <- c("Tree", "Shrub")
  tmpl[[height]] <- c("2–5", "1")
  openxlsx::write.xlsx(tmpl, path, overwrite = TRUE)

  res <- barroso_write_taxon_descr(path, species_cols = 1:3, character_cols = 4:ncol(tmpl),
                                   dir = dir, filename = "descr", verbose = FALSE)
  expect_match(res$description_plain[1], "Tree 2–5 m tall")
  expect_match(res$description_plain[2], "Shrub ca. 1 m tall")
})

test_that("barroso_add_char_template validates its input", {
  dir <- withr::local_tempdir()
  expect_error(barroso_add_char_template(species[0, ], dir = dir), "no rows")
  expect_error(barroso_add_char_template(file.path(dir, "none.xlsx")), "File not found")
  csv <- file.path(dir, "species.csv")
  write.csv(species, csv)
  expect_error(barroso_add_char_template(csv), "must be an Excel file")
  expect_error(barroso_add_char_template(1), "data.frame or a character")
  expect_error(barroso_add_char_template(species, plant_group = "Poaceae", dir = dir),
               "Unknown plant_group")
  expect_error(barroso_add_char_template(species, plant_group = c("Asteraceae", "Ochnaceae"),
                                         dir = dir), "single character")
  expect_error(barroso_add_char_template(species, species_cols = "Family", dir = dir),
               "Missing columns")
  dup <- species
  names(dup)[2] <- "Genus"
  expect_error(barroso_add_char_template(dup, species_cols = 1, dir = dir, verbose = FALSE),
               "duplicate column names")

  barroso_add_char_template(species, dir = dir, filename = "once", verbose = FALSE)
  expect_error(barroso_add_char_template(species, dir = dir, filename = "once",
                                         overwrite = FALSE, verbose = FALSE),
               "overwrite = FALSE")
})
