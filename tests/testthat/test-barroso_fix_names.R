wcvp_fixture <- data.frame(
  name = c("Ormosia arborea", "Ormosia fastigiata", "Ormosia panamensis", "Ormosia paraensis",
           "Ormosia heterophylla", "Ormosia cinerea", "Abarema cochliacarpos",
           "Pithecellobium cochliacarpum", "Ormosia nobilis", "Ormosia nobilis"),
  status = c("accepted", "accepted", "accepted", "accepted", "synonym", "accepted", "accepted",
             "synonym", "synonym", "synonym"),
  accepted = c("Ormosia arborea", "Ormosia fastigiata", "Ormosia panamensis", "Ormosia paraensis",
               "Ormosia paraensis", "Ormosia cinerea", "Abarema cochliacarpos",
               "Abarema cochliacarpos", "Ormosia bolivarensis", "Ormosia excelsa"),
  author = c("(Vell.) Harms", "Tul.", "Benth.", "Ducke", "Ducke", "Benth.", "(Gomes) Barneby & J.W.Grimes",
             "(Gomes) Barneby & J.W.Grimes", "x", "y"),
  family = "Fabaceae")
wcvp_fixture$genus <- sub("\\s.*", "", wcvp_fixture$name)

ffb_fixture <- function(names_u, max_distance) {
  out <- data.frame(name = names_u, status = "not found", accepted = NA_character_,
                    author = NA_character_, family = NA_character_)
  set <- function(n, s, a, au) {
    i <- out$name == n
    out$status[i] <<- s; out$accepted[i] <<- a; out$author[i] <<- au; out$family[i] <<- "Fabaceae"
  }
  set("Ormosia arborea", "accepted", "Ormosia arborea", "(Vell.) Harms")
  set("Ormosia cinerea", "synonym", "Ormosia coarctata", "Jacks.")
  set("Ormosia panamensis", "misspelled", "Ormosia paraensis", "Ducke")
  set("Ormosia excelsea", "misspelled", "Ormosia excelsa", "Benth.")
  out
}

records <- data.frame(
  family = c("Leguminosae", "Fabaceae", "Fabaceae", "Fabaceae", "Fabaceae", "Fabaceae",
             "Fabaceae", "Fabaceae", "Fabaceae", "Fabaceae", "Sphagnaceae", NA),
  genus = c("Ormosia", "Ormosia", "Ormosia", "Ormosia", "Ormosia", "Ormosia", "Ormosia",
            "Pithecellobium", "Ormosia", "Ormosiaa", "Sphagnum", "Ormosia"),
  specificEpithet = c("arborea", "fastigiatta", "heterophylla", "panamensis", "cinerea",
                      "excelsea", "nobilis", "cochliacarpum", NA, "arborea", "palustre", "sp."),
  scientificName = c("Ormosia arborea (Vell.) Harms", NA, NA, NA, NA, NA, NA,
                     "Pithecellobium cochliacarpum (Gomes) J.F.Macbr.", NA, NA, NA, NA),
  infraspecificEpithet = c(NA, NA, NA, NA, NA, NA, NA, "x", NA, NA, NA, NA))

test_that("names are harmonized with WCVP and FFB", {
  local_mocked_bindings(.check_package = function(...) invisible(TRUE),
                        .wcvp_names = function(families) wcvp_fixture,
                        .ffb_names = ffb_fixture)
  r <- suppressMessages(barroso_fix_names(records))
  expect_equal(r$taxonStatus,
               c("accepted", "misspelled", "synonym", "accepted", "synonym", "misspelled",
                 "ambiguous", "synonym", NA, "misspelled", "not found", NA))
  expect_equal(r$taxonSource,
               c("FFB", "WCVP", "WCVP", "WCVP", "FFB", "FFB", NA, "WCVP", NA, "WCVP", NA, NA))
  expect_equal(paste(r$genus, r$specificEpithet)[1:8],
               c("Ormosia arborea", "Ormosia fastigiata", "Ormosia paraensis",
                 "Ormosia panamensis", "Ormosia coarctata", "Ormosia excelsa",
                 "Ormosia nobilis", "Abarema cochliacarpos"))
  # A misspelling in FFB is not taken for a name found as it is in WCVP
  expect_equal(r$specificEpithet[4], "panamensis")
  # The family of accepted names is updated
  expect_equal(r$family[1], "Fabaceae")
  # Other columns of updated names
  expect_equal(r$scientificName[8], "Abarema cochliacarpos (Gomes) Barneby & J.W.Grimes")
  expect_true(is.na(r$infraspecificEpithet[8]))
  expect_equal(r$taxonOriginal[c(2, 9, 10)], c("Ormosia fastigiatta", "Ormosia", "Ormosiaa arborea"))
  expect_equal(names(r)[3:6], c("specificEpithet", "taxonOriginal", "taxonStatus", "taxonSource"))

  # Rerunning on harmonized data does not duplicate the columns
  r2 <- suppressMessages(barroso_fix_names(r))
  expect_equal(sum(names(r2) == "taxonStatus"), 1)
})

test_that("flora_brasil = FALSE uses just WCVP, and vascular_only removes other families", {
  local_mocked_bindings(.check_package = function(...) invisible(TRUE),
                        .wcvp_names = function(families) wcvp_fixture,
                        .ffb_names = function(...) stop("FFB used"))
  r <- suppressMessages(barroso_fix_names(records, flora_brasil = FALSE,
                                               vascular_only = TRUE))
  expect_equal(nrow(r), 10)
  expect_true(all(r$taxonSource %in% c("WCVP", NA)))
  expect_equal(r$specificEpithet[5], "cinerea")
})

test_that("barroso_fix_names validates its arguments", {
  expect_error(barroso_fix_names(list()), "data frame")
  expect_error(barroso_fix_names(data.frame(genus = "A")), "family")
  expect_error(barroso_fix_names(records, max_distance = 2), "between 0 and 1")
})

test_that("misspelled names are matched to the single closest name", {
  expect_equal(.closest("fastigiatta", c("fastigiata", "arborea"), 0.2), "fastigiata")
  expect_true(is.na(.closest("abc", c("abd", "abe"), 0.5)))
  expect_true(is.na(.closest("arborea", c("excelsa"), 0.2)))
})
