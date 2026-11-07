test_that(".check_df accepts data frames with records", {
  expect_silent(.check_df(data.frame(a = 1)))
  expect_silent(.check_df(tibble::tibble(a = 1)))
  expect_error(.check_df(NULL), "must be a data frame, not NULL")
  expect_error(.check_df(list(a = 1)), "must be a data frame, not a list")
  expect_error(.check_df(data.frame(a = numeric())), "has no records")
  expect_error(.check_df("x", "fieldbook"), "`fieldbook` must be a data frame")
})

test_that(".check_colnames and .check_cols name the argument to set", {
  expect_silent(.check_colnames(colname_genus = "genus"))
  expect_error(.check_colnames(colname_genus = c("a", "b")), "`colname_genus` must be the name of a column")
  expect_error(.check_colnames(colname_genus = NA), "e.g. \"genus\"")
  df <- data.frame(genus = "Ormosia")
  expect_silent(.check_cols(df, colname_genus = "genus"))
  expect_error(.check_cols(df, colname_genus = "genus", colname_recordedBy = "recordedBy"),
               "'recordedBy'. If they have other names, set `colname_recordedBy`")
})

test_that(".check_flag accepts a single TRUE or FALSE", {
  verbose <- TRUE
  rm_duplicates <- "yes"
  expect_silent(.check_flag(verbose))
  expect_error(.check_flag(verbose, rm_duplicates), "`rm_duplicates` must be TRUE or FALSE")
  na_flag <- NA
  expect_error(.check_flag(na_flag), "`na_flag` must be TRUE or FALSE")
})

test_that(".check_choice, .check_number and .check_string give clear errors", {
  language <- "fr"
  expect_error(.check_choice(language, c("en", "pt", "es")), "`language` must be one of: en, pt, es")
  expect_equal(.check_choice("pt", c("en", "pt")), "pt")
  max_distance <- 2
  expect_error(.check_number(max_distance, 0, 1), "`max_distance` must be a number between 0 and 1")
  digits <- -1
  expect_error(.check_number(digits, 0), "of at least 0")
  expect_error(.check_number("a", arg = "x"), "`x` must be a number.")
  expect_silent(.check_number(0.5, 0, 1))
  expect_silent(.check_string(NULL, null_ok = TRUE))
  font <- ""
  expect_error(.check_string(font), "`font` must be a single text.")
  expect_error(.check_string(1, null_ok = TRUE, arg = "filename"), "single text or NULL")
})

test_that(".check_file and .check_dir check paths", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines("a", f)
  expect_silent(.check_file(f, c("csv", "xlsx")))
  expect_error(.check_file(f, "xlsx"), "must be a .xlsx file")
  expect_error(.check_file(file.path(tempdir(), "none.xlsx")), "File not found")
  expect_equal(.check_dir("results/"), "results")
  expect_null(.check_dir(NULL, null_ok = TRUE))
  expect_error(.check_dir(NULL), "single text")
})

test_that(".check_package tells how to install missing packages", {
  expect_silent(.check_package("stats"))
  expect_error(.check_package("notapackage123"), "install.packages")
  expect_error(.check_package("notapackage123", repo = "DBOSlab/x"), "remotes::install_github\\(\"DBOSlab/x\"\\)")
  expect_error(.check_package("stats", version = "999.0"), "\\(>= 999.0\\)")
})

test_that("exported functions check their arguments", {
  df <- data.frame(recordedBy = "D. Cardoso", recordNumber = "1")
  expect_error(barroso_std(list(a = 1)), "must be a data frame")
  expect_error(suppressMessages(barroso_std(df, rm_duplicates = "yes")), "`rm_duplicates` must be TRUE or FALSE")
  expect_error(suppressMessages(barroso_std(data.frame(collector = "A", recordNumber = "1"))),
               "set `colname_recordedBy`")
  expect_error(std_place(NULL), "must be a data frame, not NULL")
  expect_error(std_taxa(df, colname_genus = 1), "`colname_genus` must be the name of a column")
  expect_error(std_types(df, rm_original_column = NA), "`rm_original_column` must be TRUE or FALSE")
  expect_error(std_collection(df, index_herbariorum = "no"), "`index_herbariorum` must be TRUE or FALSE")
  expect_error(std_recordedBy(data.frame(recordedBy = "A")), "set `colname_recordNumber`")
  expect_error(barroso_flag_duplicates(df[0, ]), "has no records")
  expect_error(barroso_write_specimens(df, language = "fr"), "`language` must be one of")
  expect_error(dms_to_decimal("10S", digits = -1), "`digits` must be a number of at least 0")
  expect_error(rm_authors("Ormosia arborea Harms", keep_infraspecific = "yes"),
               "`keep_infraspecific` must be TRUE or FALSE")
  expect_error(barroso_cat(list(a = df, b = df), index_herbariorum = 1), "TRUE or FALSE")
})
