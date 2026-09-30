fieldbook <- data.frame(
  herbarium = c("HUEFS", "MSC", "HUEFS"),
  catalogNumber = c("12345", NA, "67890"),
  family = "Fabaceae",
  genus = c("Ormosia", "Dalea", "Ormosia"),
  species = c("arborea", "purpurea", NA),
  infraspecies = c(NA, "var. purpurea", NA),
  recordedBy = c("D. Cardoso", "D. Cardoso", "L. P. Queiroz"),
  recordNumber = c("1350", "2001", ""),
  addCollector = c("L. P. Queiroz", NA, NA),
  day = c("5", "12", NA), month = c("Mar", "Jun", NA), year = c("2010", "2019", NA),
  country = c("Brazil", "United States", "Brazil"),
  stateProvince = c("Bahia", "Michigan", "Bahia"),
  county = c("Salvador", "Ingham County", NA),
  locality = c("Parque da Cidade", "Lake Lansing Park-North", NA),
  vegetation = c("Atlantic forest", "Prairie", NA),
  plantDescription = c("Tree 10 m tall, seeds red and black", "Herb 0.5 m", NA),
  vernacularName = c("tento", NA, NA),
  decimalLatitude = c("-12.97", "42.76", "-12.5"),
  decimalLongitude = c("-38.50", "-84.40", "-41.0"),
  altitude = c("50 m", "260 m", NA))

make_labels <- function(fb, dir) {
  suppressWarnings(utils::capture.output(
    barroso_labels(fb, dir_create = dir, file_label = "labels.pdf")))
  list.files(dir, pattern = "[.]pdf$", recursive = TRUE, full.names = TRUE)
}

test_that("herbarium labels are written to PDF, six per page", {
  skip_if_not_installed("lcvplants")
  skip_if_not_installed("LCVP")
  dir <- file.path(withr::local_tempdir(), "labels")

  # Brazil, USA (with county map) and a specimen identified just to genus
  pdf <- make_labels(fieldbook, dir)
  expect_equal(basename(pdf), "labels_1.pdf")
  expect_equal(basename(dirname(pdf)), format(Sys.time(), "%d%b%Y"))
  expect_gt(file.size(pdf), 10000)

  # Seven labels need two pages
  pdf <- make_labels(fieldbook[c(1, 1, 1, 1, 1, 1, 3), ], file.path(dir, "seven"))
  expect_equal(sort(basename(pdf)), c("labels_1.pdf", "labels_2.pdf"))
})

test_that("barroso_labels requires the state of each specimen", {
  fb <- fieldbook
  fb$stateProvince[2] <- NA
  expect_error(barroso_labels(fb, dir_create = withr::local_tempdir()),
               "NA found in the stateProvince column")
})
