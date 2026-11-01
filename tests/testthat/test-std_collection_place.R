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

test_that("std_collection extracts herbarium acronyms from dirty codes", {
  df <- data.frame(
    collectionCode = c("Tropicos", "Tropicos", "MO-Duplicates", "Botany", "Botany",
                       "F-Seedplants", "BHCB-Fanerogamas", "INPA-Carpoteca", "R-Tipos",
                       "UIS-H", "BSMvplantscoll", "NL-Botany", "CJBG-G-DC", "INPAw",
                       "Herbario INPA", "Herbario Nacional de Costa Rica (CR)",
                       "Herbarium Hamburgense HBG", "Observations", "Dados de Pesquisa SISBio",
                       "052041003001001", NA, NA, NA, "RECOLNAT_MNHN_P", "Plantae",
                       "ECO-TA-H", "PNGplants", "HT24989-25186"),
    institutionCode = c("NY", NA, "LPB", NA, "F", "F", "UFMG", "INPA", "MN",
                        "Universidad Industrial de Santander (UIS)", "M-VascularPlants",
                        "NBC", "CJBG", "INPA", "Instituto Nacional de Pesquisas da Amazonia (INPA)",
                        "Museo Nacional de Costa Rica (MNCR)", "HBG", "iNaturalist", "ICMBio",
                        "NEFI", "Instituto Florestal", "Empresas Publicas de Medellin E.S.P. (EPM)",
                        "Mined from GenBank, NCBI", "MNHN", "INBio", "ECOSUR", "BRI",
                        "Promigas S.A E.S.P (Promigas)"),
    catalogNumber = c(NA, NA, NA, "U.1317533", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                      NA, NA, NA, "340214363", NA, NA, NA, NA, "D. Cardoso 2118 (HUEFS)",
                      NA, NA, NA, NA, NA))
  r <- suppressMessages(std_collection(df))
  expect_equal(r$collectionCode,
               c("NY", "MO", "LPB", "U", "F", "F", "BHCB", "INPA", "R", "UIS", "M", "L",
                 "G-DC", "INPAw", "INPA", "CR", "HBG", NA, NA, "NEFI", "SPSF", NA, "HUEFS",
                 "P", "INB", "ECO-TA-H", "BRI", NA))
})

test_that("unvouchered collections and observations are removed", {
  df <- data.frame(collectionCode = c("RB", "BCTw", "INPA-Carpoteca", "BHZBcarpo", "K", "Seeds"),
                   basisOfRecord = c("PRESERVED_SPECIMEN", NA, NA, NA, "HUMAN_OBSERVATION", NA))
  expect_equal(barRoso:::.delunvouchered(df, "collectionCode")$collectionCode, "RB")
})
