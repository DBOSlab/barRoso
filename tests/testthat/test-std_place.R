places <- data.frame(
  continent = c(NA, "AMERIQUE DU SUD", "EUROPE", NA, "Asia-Temperate", NA, NA, NA),
  country = c("Brasil", "BRAZIL", "France", "Guyane française", "China", "USA", "Bolivie", "Unknown"),
  stateProvince = c("BA", "Pa", "Bretagne", NA, "Yunnan (Prov.)", "TX", NA, "Unknown"),
  county = c("SALVADOR", NA, NA, NA, NA, "Travis County", NA, NA),
  municipality = c("SALVADOR", NA, NA, NA, NA, NA, NA, NA),
  locality = c("Parque da Cidade", "Municipio de Belém. Utinga", NA, "State of Cayenne. Roura",
               "Kunming", "Austin", "Depto. Santa Cruz, Chiquitos", NA))

test_that("countries, states, continents, counties and localities are standardized", {
  r <- suppressMessages(std_place(places))

  expect_equal(r$country, c("Brazil", "Brazil", "France", "French Guiana", "China",
                            "United States", "Bolivia", NA))
  expect_equal(r$continent, c("South America", "South America", "Europe", "South America",
                              "Asia", "North America", "South America", NA))
  # Acronyms, capital letters, provinces and states taken from the locality
  expect_equal(r$stateProvince, c("Bahia", "Pará", "Bretagne", "Cayenne", "Yunnan", "Texas",
                                  "Santa Cruz", NA))
  expect_equal(r$county[c(1, 6)], c("Salvador", "Travis"))
  expect_equal(r$municipality[1:2], c("Salvador", "Belém"))
  expect_equal(r$locality[c(2, 4, 7)], c("Utinga", "Roura", "Chiquitos"))
  expect_false(any(grepl("Original$", names(r))))
})

test_that("state acronyms are expanded only within their own country", {
  df <- data.frame(country = c("USA", "USA", "Brazil", "Brasil", "Brazil", "United States"),
                   stateProvince = c("AL", "PA", "AL", "Pa", "RR", "DC"),
                   locality = "x")
  r <- suppressMessages(std_place(df))
  expect_equal(r$stateProvince, c("Alabama", "Pennsylvania", "Alagoas", "Pará", "Roraima",
                                  "District of Columbia"))
})

test_that("std_place keeps the original columns with custom names", {
  df <- setNames(places[, c("continent", "country", "stateProvince", "locality")],
                 c("continente", "pais", "estado", "localidade"))
  r <- suppressMessages(std_place(df, colname_continent = "continente", colname_country = "pais",
                                  colname_stateProvince = "estado", colname_locality = "localidade",
                                  rm_original_column = FALSE))
  expect_true(all(c("continenteOriginal", "paisOriginal", "estadoOriginal") %in% names(r)))
  expect_equal(r$paisOriginal, places$country)
  expect_equal(r$pais[1], "Brazil")
  expect_equal(r$estado[2], "Pará")
})

test_that("Venezuela is spelled correctly and gets its continent", {
  r <- suppressMessages(std_place(data.frame(continent = NA, country = c("Venezula", "Vénézuela"))))
  expect_equal(r$country, c("Venezuela", "Venezuela"))
  expect_equal(r$continent, c("South America", "South America"))
})
