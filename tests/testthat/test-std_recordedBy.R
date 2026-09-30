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
                 "D. Inácio Junqueira", "Unknown", "Unknown", "L. Yung"))
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
                               "T. C. Huang", "780 Zhi Bei Zu"))
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
  expect_equal(r$recordedBy, c("N. M. F. Silva", "C.-M. Tan", "D. Rodríguez", "J. A. Lombardi",
                               "A. C. Cervi", "J. Silva", "J. Smith", "R. M. Harley",
                               "L. P. Queiroz", "Flora of Bahia Project", "T. N. Liou",
                               "Q. W. Wang", "Li"))
  expect_equal(r$addCollector, c(NA, NA, "et al.", "et al.", "et al.", "M. Santos", "K. Jones",
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
