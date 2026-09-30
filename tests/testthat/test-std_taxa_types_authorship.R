test_that("std_taxa standardizes family, genus and specific epithet", {
  df <- data.frame(family = c("Leguminosae", "FABACEAE", "Ochnaceae", NA, "Fabaceae"),
                   genus = c("ormosia", "Ormosia", "OURATEA", "Swartzia", "Indet"),
                   specificEpithet = c("arborea", "cf. fastigiata", "sp.", "aff. apetala", "sp. 1"))
  r <- suppressMessages(std_taxa(df, rm_original_column = FALSE))
  expect_equal(r$family, c("Fabaceae", "Fabaceae", "Ochnaceae", NA, "Fabaceae"))
  expect_equal(r$genus, c("Ormosia", "Ormosia", "Ouratea", "Swartzia", NA))
  expect_equal(r$specificEpithet, c("arborea", "fastigiata", NA, "apetala", NA))
  expect_equal(r$genusOriginal, df$genus)

  r <- suppressMessages(std_taxa(setNames(df, c("familia", "genero", "epiteto")),
                                 colname_family = "familia", colname_genus = "genero",
                                 colname_specificEpithet = "epiteto"))
  expect_equal(names(r), c("familia", "genero", "epiteto"))
  expect_equal(r$genero[3], "Ouratea")
})

test_that("std_types removes entries that are not types", {
  df <- data.frame(typeStatus = c("Holotype", "Fotografia do Tipo", "NOTATYPE", "sim - Isótipo",
                                  NA, "Epítipo"))
  r <- suppressMessages(std_types(df, rm_original_column = FALSE))
  expect_equal(r$typeStatus, c("Holotype", NA, NA, "Isótipo", NA, NA))
  expect_equal(r$typeStatusOriginal, df$typeStatus)

  r <- suppressMessages(std_types(setNames(df, "tipo"), colname_typeStatus = "tipo"))
  expect_equal(names(r), "tipo")
})

test_that("remove_authorship removes authors, also around infraspecific ranks", {
  x <- c("Ormosia arborea (Vell.) Harms", "Ouratea castaneifolia (DC.) Engl.",
         "Dalea purpurea Vent. var. purpurea",
         "Swartzia apetala Raddi var. glabra (Vogel) R.S.Cowan",
         "Ouratea concinna A.Queiroz-Lima & D.B.O.S.Cardoso", "Swartzia", "Quercus alba L.", NA)
  expect_equal(remove_authorship(x),
               c("Ormosia arborea", "Ouratea castaneifolia", "Dalea purpurea var. purpurea",
                 "Swartzia apetala var. glabra", "Ouratea concinna", "Swartzia", "Quercus alba", NA))
  expect_equal(remove_authorship(x[3:4], keep_infraspecific = FALSE),
               c("Dalea purpurea", "Swartzia apetala"))
  expect_error(remove_authorship(1), "must be a character vector")
})

test_that("auxiliary and defensive helpers work", {
  gbif <- data.frame(a = 1)
  expect_equal(names(barRoso:::.namedherbsource(gbif)), "gbif")

  df <- data.frame(collectionCode = c("RB", "BCTW", "Spirit Collection", "K"))
  expect_equal(barRoso:::.delunvouchered(df, "collectionCode")$collectionCode, c("RB", "K"))

  expect_equal(barRoso:::.upper_first_only(c("LEAF blade", "", NA, "a")),
               c("Leaf blade", "", NA, "A"))
  expect_equal(barRoso:::.upper_first_only(1), "1")

  df <- data.frame(Genus = 1, Species = 2)
  expect_equal(barRoso:::.resolve_cols(df, 2, "x"), "Species")
  expect_equal(barRoso:::.resolve_cols(df, "Genus", "x"), "Genus")
  expect_error(barRoso:::.resolve_cols(df, NULL, "x"), "must be non-empty")
  expect_error(barRoso:::.resolve_cols(df, 3, "x"), "out of range")
  expect_error(barRoso:::.resolve_cols(df, TRUE, "x"), "character vector of names")

  expect_equal(barRoso:::.arg_check_dir("results/"), "results")
  expect_error(barRoso:::.arg_check_dir(1), "should be a character")
  expect_error(barRoso:::.arg_check_xlsx_path(""), "non-empty character scalar")
})
