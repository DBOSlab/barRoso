test_that("std_collection works with custom column names", {
  df <- data.frame(herbario = c("MOBOT_BR", "Herbarium", "NHM-LONDON-BOT"),
                   instituicao = c("MO", "RB", "NHMUK"),
                   genus = c("Ormosia", "Ormosia", "Swartzia"))
  r <- suppressMessages(std_collection(df, index_herbariorum = FALSE, colname_collectionCode = "herbario",
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
  r <- suppressMessages(std_collection(df, index_herbariorum = FALSE))
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

ih_fixture <- data.frame(
  code = c("INPA", "UNB", "P", "MNHN", "UPH", "UPH", "SPF", "ESA", "HUEFS"),
  organization = c("Instituto Nacional de Pesquisas da Amaz\u00f4nia", "Universidade de Bras\u00edlia",
                   "Mus\u00e9um National d'Histoire Naturelle", "Museo Nacional de Historia Natural",
                   "University of Prishtina", "University of Port Harcourt",
                   "Universidade de S\u00e3o Paulo", "Universidade de S\u00e3o Paulo",
                   "Universidade Estadual de Feira de Santana"),
  otherInstitutionNames = c("", "", "", "", "", "", "", "", "UEFS\nFeira de Santana State University"))

test_that("index_herbariorum checks the codes and always keeps the originals", {
  local_mocked_bindings(.ih_institutions = function() ih_fixture)
  df <- data.frame(
    collectionCode = c("Herbario INPA", "UnB", "P", "XYZ", "UPH", "Botany",
                       "Universidade Estadual de Feira de Santana", "Universidade de Sao Paulo"),
    institutionCode = c("Instituto Nacional de Pesquisas da Amazonia (INPA)", "UnB", "MNHN", "XYZ",
                        "UPH", "Feira de Santana State University", NA, NA))
  r <- suppressMessages(std_collection(df, index_herbariorum = TRUE))
  expect_equal(r$collectionCode[1:5], c("INPA", "UNB", "P", "XYZ", "UPH"))
  # The institution code is not changed
  expect_equal(r$institutionCode, df$institutionCode)
  expect_equal(r$indexHerbariorum, c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE))
  expect_equal(r$institutionName[c(1, 4, 5)],
               c("Instituto Nacional de Pesquisas da Amaz\u00f4nia", NA,
                 "University of Prishtina | University of Port Harcourt"))
  # From the names of institutions, when they have a single herbarium
  expect_equal(r$collectionCode[6:7], c("HUEFS", "HUEFS"))
  expect_false(r$collectionCode[8] %in% c("SPF", "ESA"))
  # The original collection code is kept even with rm_original_column = TRUE
  expect_equal(r$collectionCodeOriginal, df$collectionCode)
  expect_false("institutionCodeOriginal" %in% names(r))

  # And rerunning keeps them
  r2 <- suppressMessages(std_collection(r, index_herbariorum = TRUE))
  expect_equal(r2$collectionCodeOriginal, df$collectionCode)
  expect_equal(sum(names(r2) == "institutionName"), 1)

  # Checked by default, in std_collection and barroso_std
  r <- suppressMessages(std_collection(df))
  expect_true(all(c("indexHerbariorum", "collectionCodeOriginal") %in% names(r)))
  x <- cbind(df, recordedBy = "D. Cardoso", recordNumber = "1")
  expect_true("indexHerbariorum" %in% names(suppressMessages(barroso_std(x))))

  # FALSE does not need the API
  local_mocked_bindings(.ih_institutions = function() stop("API used"))
  r <- suppressMessages(std_collection(df, index_herbariorum = FALSE))
  expect_false(any(c("indexHerbariorum", "collectionCodeOriginal") %in% names(r)))
})

test_that("index_herbariorum warns and leaves the codes when the API is unavailable", {
  local_mocked_bindings(.ih_institutions = function() {
    warning("Index Herbariorum could not be reached, so the codes were not checked", call. = FALSE)
    NULL
  })
  df <- data.frame(collectionCode = "UnB", institutionCode = "UnB")
  expect_warning(r <- suppressMessages(std_collection(df, index_herbariorum = TRUE)), "could not be reached")
  expect_equal(r$collectionCode, "UnB")
  expect_false("indexHerbariorum" %in% names(r))
  expect_equal(r$collectionCodeOriginal, "UnB")
})

test_that("the Index Herbariorum API answers", {
  skip_on_cran()
  skip_if_offline("sweetgum.nybg.org")
  ih <- suppressWarnings(.ih_institutions())
  skip_if(is.null(ih))
  expect_true(all(c("INPA", "RB", "MO") %in% ih$code))
})

test_that("catalog numbers are written without spaces and punctuation, with the acronym", {
  x <- c("181351 ", "HATM 003653", "ANDES-H:6594", "FMB-71755", "M. A. Chocce 536",
         "barcode-00064442", "L.2049875", "MA-01-00738322", "CANB 490096.1", "JCT-S 1798.1",
         "Chocce, M.A. 536", "D. Cardoso 2118 (HUEFS)", NA, "", "ec29d5d8-b6bf-11e1-8c2f-acd112e444a7",
         "5718")
  code <- c("HUA", "HATM", "ANDES-H", "FMB", "USM", "A", "L", "MA", "CANB", "JCT", "USM", "HUEFS",
            "RB", "RB", "NY", NA)
  expect_equal(.std_catalog_number(x, code),
               c("HUA181351", "HATM003653", "ANDESH6594", "FMB71755", NA, "A00064442", "L2049875",
                 "MA0100738322", "CANB4900961", "JCTS17981", NA, NA, NA, NA,
                 "ec29d5d8-b6bf-11e1-8c2f-acd112e444a7", "5718"))

  df <- data.frame(collectionCode = c("Botany", "HUA"), institutionCode = NA,
                   catalogNumber = c("L.2049875", "181351 "))
  r <- suppressMessages(std_collection(df, index_herbariorum = FALSE, rm_original_column = FALSE))
  expect_equal(r$collectionCode, c("L", "HUA"))
  expect_equal(r$catalogNumber, c("L2049875", "HUA181351"))
  expect_equal(r$catalogNumberOriginal, df$catalogNumber)
  r <- suppressMessages(std_collection(df, index_herbariorum = FALSE))
  expect_false("catalogNumberOriginal" %in% names(r))
})
