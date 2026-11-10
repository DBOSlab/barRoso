std_coll <- function(x) {
  suppressMessages(std_recordedBy(data.frame(recordedBy = x, recordNumber = NA_character_)))
}

test_that("collector names are standardized into initials plus surname", {
  x <- c("Cardoso, D.B.O.S.", "Padgurschi, MCG", "Cardoso, D", "Estrada, Armando",
         "Schultes R.E.", "Croat TB", "Sergio M Faria", "David J.N. Hind",
         "Uribe Uribe AL", "Alexánder Francisco Rodríguez González")
  expect_equal(std_coll(x)$recordedBy,
               c("D. Cardoso", "M. C. G. Padgurschi", "D. Cardoso", "A. Estrada",
                 "R. E. Schultes", "T. B. Croat", "S. M. Faria", "D. J. N. Hind",
                 "A. L. Uribe-Uribe", "A. F. Rodríguez González"))
})

test_that("additional collectors go to addCollector", {
  r <- std_coll(c("Cardoso, D.|Santos, Q.", "Lima, H.C. & Cardoso, D.",
                  "J.A. Lombardi, H. Lorenzi, R. Tsuji",
                  "A. Gómez Pompa, A. J. Sharp & P. Hernández",
                  "Schultes R.E., Raffauf R.F. & Soejarto D.",
                  "Ordones, J. et al.", "Cardoso, N. e outros"))
  expect_equal(r$recordedBy, c("D. Cardoso", "H. C. Lima", "J. A. Lombardi",
                               "A. Gómez Pompa", "R. E. Schultes", "J. Ordones", "N. Cardoso"))
  expect_equal(r$addCollector, c("Q. Santos", "D. Cardoso", rep("et al.", 5)))
})

test_that("accents, particles and encodings are handled", {
  x <- c("Dusén", "Fróes", "Martius, CFP von", "Roosmalen, MGM van",
         "Co&#234;lho, L.F.", "M. De la Estrella", "Collector(s): Richard D. Worthington",
         "Daniela Inácio Junqueira - UEG", "#NOME?", "REFLORA provisional entry",
         "ling yung")
  expect_equal(std_coll(x)$recordedBy,
               c("Dusén", "R. L. Fróes", "C. F. P. von Martius", "M. G. M. van Roosmalen",
                 "L. F. Coêlho", "M. de la Estrella", "R. D. Worthington",
                 "D. Inácio Junqueira", NA, NA, "L. Yung"))
})

test_that("abbreviations do not depend on the other names in the data", {
  x <- "Davidson, Mary Elizabeth Spence"
  expect_equal(std_coll(c(x, "Maria Eduarda Silva Santos"))$recordedBy[1],
               std_coll(x)$recordedBy)
})

test_that("Chinese names in unicode escapes are converted into Latin names", {
  r <- std_coll(c("<U+674E><U+5149><U+7167>",
                  "<U+674E><U+5149><U+7167>,<U+5510><U+8D5B><U+6625>",
                  "<U+66FE><U+6000><U+5FB7>",
                  "<U+5DE6><U+666F><U+70C8><U+7B49>",
                  "T.C.Huang <U+9EC3><U+589E><U+6CC9>",
                  "780<U+690D><U+88AB><U+7EC4>"))
  expect_equal(r$recordedBy, c("G. Z. Li", "G. Z. Li", "H. D. Zeng", "J. L. Zuo",
                               "T. C. Huang", "Zhi Bei Zu"))
  expect_equal(r$addCollector, c(NA, "S. C. Tang", NA, "et al.", NA, NA))
  # As Chinese authors cite themselves, e.g. Ting-Shuang Yi and Rong Zhang
  expect_equal(std_coll(c("<U+6613><U+5A77><U+53CC>", "<U+5F20><U+8363>",
                          "<U+6B27><U+9633><U+4FEE>"))$recordedBy,
               c("T. S. Yi", "R. Zhang", "X. Ouyang"))
})

test_that("collector numbers are cleaned", {
  r <- suppressMessages(std_recordedBy(data.frame(
    recordedBy = c("Conceicao, A.A. 1161", "C Davis 812", "Cardoso, D."),
    recordNumber = c(NA, NA, "s.n."))))
  expect_equal(r$recordNumber, c("1161", "812", NA))
})

test_that("custom column names are kept", {
  r <- suppressMessages(std_recordedBy(data.frame(coletor = "Cardoso, D.", numero = "1"),
                                       colname_recordedBy = "coletor",
                                       colname_recordNumber = "numero"))
  expect_true(all(c("coletor", "coletorOriginal", "numero", "numeroOriginal",
                    "addCollector") %in% names(r)))
  expect_equal(r$coletor, "D. Cardoso")
})

test_that("numbers written with the collector name go to recordNumber", {
  r <- std_coll(c("Nakajima, J.N. 3101; Harley, R.M.", "Martius, C.F.P. von (no. Obs. 1935)",
                  "8470 G.H. Turner", "67-1240 N.C. Henderson", "C Davis D-14",
                  "Mark Hughes Sumatra 2011"))
  expect_equal(r$recordedBy, c("J. N. Nakajima", "C. F. P. von Martius", "G. H. Turner",
                               "N. C. Henderson", "C. Davis", "M. Hughes"))
  expect_equal(r$recordNumber, c("3101", "1935", "8470", "67-1240", "D-14", NA))
  expect_equal(r$addCollector[1], "R. M. Harley")
})

test_that("more collector formats and separators are standardized", {
  r <- std_coll(c("N. Marquete F. Silva", "C. -Ming Tan", "Rodríguez,D., Rodríguez,B. & Trejo,L.",
                  "J.A. Lombardi, H. Lorenzi, R. Tsuji, M. Sobral",
                  "Cervi, A. C., R. Spichiger, P.-A. Loizeau & E. Cottier",
                  "Silva, J.; Santos, M.", "Smith, J.|Jones, K.", "Harley, R.M. & AORibeiro",
                  "Queiroz, L.P.; Santos, A.; et al.", "Flora of Bahia Project",
                  "T.N.Liou<U+3001>P.C.Tsoong", "<U+738B><U+542F><U+65E0> <U+5218><U+745B>",
                  "<U+674E>"))
  expect_equal(r$recordedBy, c("N. Marquete", "C.-M. Tan", "D. Rodríguez", "J. A. Lombardi",
                               "A. C. Cervi", "J. Silva", "J. Smith", "R. M. Harley",
                               "L. P. Queiroz", "Flora of Bahia Project", "T. N. Liou",
                               "Q. W. Wang", "Li"))
  expect_equal(r$addCollector, c("F. Silva", NA, "et al.", "et al.", "et al.", "M. Santos", "K. Jones",
                                 "A. O. Ribeiro", "et al.", NA, "P. C. Tsoong", "Y. Liu", NA))
})

test_that("names in specific herbaria are standardized", {
  r <- suppressMessages(std_recordedBy(data.frame(
    recordedBy = c("Taciana Barbosa Cavalcanti", "Marcelo Fragomeni Simon",
                   "Pedro Pable Moreno, Walter Robleto"),
    recordNumber = c("1", "2", "3"),
    collectionCode = c("CEN", "CEN", "X"), institutionCode = c("CEN", "CEN", "UTEP"))))
  expect_equal(r$recordedBy, c("T. B. Cavalcanti", "M. F. Simon", "P. Pable Moreno"))
  expect_equal(r$addCollector, c(NA, NA, "W. Robleto"))
})

test_that("collector numbers are standardized", {
  r <- suppressMessages(std_recordedBy(data.frame(
    recordedBy = "Bullock, AA",
    recordNumber = c("Bullock, AA 712", "Heller, AA 61-35", "12/05/1990", "s.n.", "0045", "Harley22573"))))
  expect_equal(r$recordNumber, c("712", "61-35", NA, NA, "45", "22573"))
  r <- suppressMessages(std_recordedBy(data.frame(recordedBy = "D. Cardoso", recordNumber = "1"),
                                       rm_original_column = TRUE))
  expect_false(any(grepl("Original$", names(r))))
})

test_that("std_recordedBy converts names written in Chinese characters", {
  r <- std_coll(c("丁广奇、余莲", "周根生等",
                  "K.C.Yang 楊國禎", "Peng, Ching-I (彭鏡毅)",
                  "780植被组", "丘，李，农"))
  expect_equal(r$recordedBy, c("G. Q. Ding", "G. S. Zhou", "K. C. Yang", "C.-I. Peng",
                               "Zhi Bei Zu", "Qiu"))
  expect_equal(r$addCollector, c("L. Yu", "et al.", NA, NA, NA, "et al."))

  expect_equal(suppressMessages(barRoso:::.std_determiners(
    c("周以良 杨衔晋", "T.C.Huang 黃增泉"))),
    c("Y. L. Zhou & X. J. Yang", "T. C. Huang"))
})

test_that("std_recordedBy cleans identifiers, lowercase and team names", {
  r <- std_coll(c("E. Marcano (Wikidata Q5850623)", "katherine Bravo", "Wim.Jacobs",
                  "Belem Herbarium", "Equipe do Jardim Botanico de Brasilia"))
  expect_equal(r$recordedBy, c("E. Marcano", "K. Bravo", "W. Jacobs", "Belem Herbarium",
                               "Equipe do Jardim Bot\u00e2nico de Bras\u00edlia"))
})

test_that("placeholders, titles, positions and team numbers are cleaned", {
  x <- c("[missing; ex Herb. Acad. Petrop.]", "236-6\u961f", "[data not captured]",
         "Col. Richard Henry Beddome",
         "H.C. de Lima, C.A. Cid Ferreira, J. Guedes;F. Neves",
         "Warren Douglas Stevens, B. A. Krukoff Curator of Central American Botany",
         "-", "?", "[A. Beccari]", "Lima, H.C.", "Cid Ferreira, C.A.",
         "\u7ca473", "Hno. Daniel", "Dr. King's Collector [Malaysia]")
  r <- suppressMessages(std_recordedBy(data.frame(recordedBy = x, recordNumber = NA_character_)))
  expect_equal(r$recordedBy[c(1, 3, 7, 8)], rep(NA_character_, 4))
  expect_true(is.na(r$addCollector[1]))
  expect_equal(r$recordedBy[2], "Dui")
  expect_equal(r$recordNumber[2], "236-6")
  expect_equal(r$recordedBy[4], "R. H. Beddome")
  expect_equal(r$recordedBy[5], "H. C. Lima")
  expect_equal(r$addCollector[5], "et al.")
  expect_equal(r$recordedBy[6], "W. D. Stevens")
  # "B. A. Krukoff Curator" is the position of W. D. Stevens, not a collector
  expect_true(is.na(r$addCollector[6]))
  expect_equal(r$recordedBy[9], "A. Beccari")
  expect_equal(r$recordedBy[10], "H. C. Lima")
  expect_true(is.na(r$addCollector[10]))
  expect_equal(r$recordedBy[11], "C. A. Cid-Ferreira")
  expect_equal(c(r$recordedBy[12], r$recordNumber[12]), c("Yue", "73"))
  expect_equal(r$recordedBy[13], "Daniel")
  expect_equal(r$recordedByOriginal, x)

  # Several collectors without commas
  r <- suppressMessages(std_recordedBy(data.frame(
    recordedBy = c("M. J. E. Coode D. W. Kirkup et al.", "V. Ariati D.A. Roher J.E. Bianchin;M.M. Dutra"),
    recordNumber = NA_character_)))
  expect_equal(r$recordedBy, c("M. J. E. Coode", "V. Ariati"))
  expect_equal(r$addCollector, c("et al.", "et al."))

  # A particle is not a surname
  r <- suppressMessages(std_recordedBy(data.frame(recordedBy = "M. Dos S. Rodrigues", recordNumber = "1")))
  expect_equal(c(r$recordedBy, r$addCollector), c("M. D. S. Rodrigues", NA))

  # A team number does not replace the number of the collection, and is removed
  r <- suppressMessages(std_recordedBy(data.frame(recordedBy = "236-6\u961f", recordNumber = "5")))
  expect_equal(c(r$recordedBy, r$recordNumber), c("Dui", "5"))
})

test_that("numbers followed by Greek letters or \u00df keep the letter", {
  r <- suppressMessages(std_recordedBy(data.frame(
    recordedBy = c("Koorders SH", "A. Silva", "B. Lima", "C. Souza", "D. Cardoso"),
    recordNumber = c("Koorders, SH 9588 \u00df", "123 \u03b1", "45\u03b2", "7 \u0393", "1350"))))
  expect_equal(r$recordNumber, c("9588\u00df", "123\u03b1", "45\u03b2", "7\u0393", "1350"))
  expect_equal(r$recordedBy[1], "S. H. Koorders")

  # Also a single Latin letter, but not after other characters, e.g. "12380/139a"
  r <- suppressMessages(std_recordedBy(data.frame(
    recordedBy = "Koorders SH",
    recordNumber = c("Koorders, SH 5661 B", "Koorders, SH 5661 a", "Kerr, AFG 19008 A",
                     "Tang 61 A", "4021A", "12380/139a"))))
  expect_equal(r$recordNumber, c("5661B", "5661a", "19008A", "61A", "4021A", "12380/139a"))

  # Commas and dots between digits are removed, after the decimals of spreadsheets
  r <- suppressMessages(std_recordedBy(data.frame(
    recordedBy = "A. Silva", recordNumber = c("12,979b", "1.350", "159.0", "2773.00"))))
  expect_equal(r$recordNumber, c("12979b", "1350", "159", "2773"))
})
