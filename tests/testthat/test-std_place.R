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

test_that("std_place removes words for municipality from county and municipality", {
  df <- data.frame(country = "Brazil",
                   county = c("Manaus Mun.", "Mun. de Castelo", "Novo Mundo", "MUN. ITACOATIARA"),
                   municipality = c("Município de Mucugê", "Arriaga Municipio",
                                    "Linhares (Município)", "Mpio. Ocosingo"),
                   locality = "Mata")
  r <- suppressMessages(std_place(df))
  expect_equal(r$county, c("Manaus", "Castelo", "Novo Mundo", "Itacoatiara"))
  expect_equal(r$municipality, c("Mucugê", "Arriaga", "Linhares", "Ocosingo"))
})

test_that("std_place writes particles in lowercase", {
  df <- data.frame(country = "Brazil", stateProvince = "Rio Grande Do Norte",
                   county = "Alvorada Do Norte", municipality = "Brejo Dos Santos",
                   locality = "Mata")
  r <- suppressMessages(std_place(df))
  expect_equal(c(r$stateProvince, r$county, r$municipality),
               c("Rio Grande do Norte", "Alvorada do Norte", "Brejo dos Santos"))
})

test_that("std_place converts country codes and finds missing countries", {
  df <- data.frame(
    country = c("BR", "GY", "BRA", NA, NA, NA, "Brazil", NA, NA, "Colombia", "French Guiana"),
    stateProvince = c("Bahia", NA, NA, "Par\u00e1", NA, "Amazonas", "Amazonas", "Amazonas", NA,
                      "Amazonas", NA),
    county = c(NA, NA, NA, NA, NA, "Manaus", "Manaus", NA, NA, "Leticia", NA),
    municipality = NA,
    locality = c(NA, NA, NA, NA, "Rio Negro, Brasil", "Reserva Ducke", NA, "Rio Negro",
                 "Puerto Arturo, Georgia", NA, NA),
    stringsAsFactors = FALSE)
  out <- suppressMessages(std_place(df))
  expect_equal(out$country, c("Brazil", "Guyana", "Brazil", "Brazil", "Brazil", "Brazil",
                              "Brazil", NA, NA, "Colombia", "French Guiana"))
})

test_that("std_place takes missing municipalities from the county", {
  df <- data.frame(country = "Brazil", stateProvince = "Bahia",
                   county = c("Mucug\u00ea", "Len\u00e7\u00f3is", "inc.sed.cty."),
                   municipality = c(NA, "Palmeiras", NA), locality = "x",
                   stringsAsFactors = FALSE)
  out <- suppressMessages(std_place(df))
  expect_equal(out$municipality, c("Mucug\u00ea", "Palmeiras", NA))
  expect_equal(out$county, c("Mucug\u00ea", "Len\u00e7\u00f3is", NA))
})

test_that("localities all in upper case get just the first letter of each word in upper case", {
  x <- c("ESTR.  UBAITUBA  /  MAR\u00c1U  AO  KM  54  .",
         "BR 40, ENTRE PARACATU E JO\u00c3O PINHEIRO.",
         "BR-316",
         "A 10 KM AL E DE CRUCERO COROZAL",
         "ALONG RIO CANUM\u00c3, W BANK ABOUT 8 KM S OF VILA DE CANUM\u00c3.",
         "2 KM DE LA VIA DEL TREN, HACIA EL CERRO",
         "ESTACION DE BIOLOGIA TROPICAL LOS TUXTLAS",
         "CA. 86KM N. OF XAVANTINA",
         "TVR UTM (395428N/9622570E).",
         "ESALQ",
         "PARATY, RJ.",
         "RJ",
         "JABOT_RJ",
         "15KM E OF ROCKSTONE",
         "\u00c1GUA FRIA",
         "Parque da Cidade, BR 101",
         NA)
  expect_equal(.fix_upper_locality(x),
               c("Estr. Ubaituba/Mar\u00e1u ao Km 54.",
                 "BR 40, entre Paracatu e Jo\u00e3o Pinheiro.",
                 "BR-316",
                 "A 10 Km al E de Crucero Corozal",
                 "Along Rio Canum\u00e3, W Bank about 8 Km S of Vila de Canum\u00e3.",
                 "2 Km de la Via del Tren, hacia el Cerro",
                 "Estacion de Biologia Tropical Los Tuxtlas",
                 "Ca. 86km N. of Xavantina",
                 "Tvr UTM (395428N/9622570E).",
                 "ESALQ",
                 "Paraty, RJ.",
                 "RJ",
                 "JABOT_RJ",
                 "15km E of Rockstone",
                 "\u00c1gua Fria",
                 "Parque da Cidade, BR 101",
                 NA))
})
