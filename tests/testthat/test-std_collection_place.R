test_that("std_collection works with custom column names", {
  df <- data.frame(herbario = c("MOBOT_BR", "Herbarium", "NHM-LONDON-BOT"),
                   instituicao = c("MO", "RB", "NHMUK"),
                   genus = c("Ormosia", "Ormosia", "Swartzia"))
  r <- suppressMessages(std_collection(df, colname_collectionCode = "herbario",
                                       colname_institutionCode = "instituicao",
                                       rm_original_column = FALSE))
  expect_equal(names(r), c("herbarioOriginal", "herbario", "instituicaoOriginal",
                           "instituicao", "genus"))
  expect_equal(r$herbario, c("MO", "RB", "BM"))
  expect_equal(r$genus, df$genus)
})

test_that("std_place keeps each original column with its own name", {
  df <- data.frame(country = "Brasil", county = "SALVADOR", municipality = "SALVADOR",
                   locality = "Parque da Cidade")
  r <- suppressMessages(std_place(df, rm_original_column = FALSE))
  expect_true(all(c("countyOriginal", "municipalityOriginal", "localityOriginal") %in% names(r)))
  expect_false(any(grepl("[.][0-9]$", names(r))))

  r <- suppressMessages(std_place(setNames(df, c("pais", "condado", "municipio", "localidade")),
                                  colname_country = "pais", colname_county = "condado",
                                  colname_municipality = "municipio",
                                  colname_locality = "localidade", rm_original_column = FALSE))
  expect_true(all(c("municipioOriginal", "localidadeOriginal") %in% names(r)))
})
