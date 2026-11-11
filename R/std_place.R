#' Standardize Place-Related Columns in Biodiversity Data
#'
#' @author Domingos Cardoso
#'
#' @description
#' Cleans and standardizes the geographic columns of a biodiversity collection
#' dataset. This includes unifying column names and harmonizing values for
#' `continent`, `country`, `stateProvince`, `county`, `municipality`, and `locality`.
#' The function handles translations, synonyms, upper-case anomalies, ISO country
#' codes, and common geographic aliases.
#'
#' @details
#' This function is used internally by the `barRoso` package to support record
#' reconciliation, duplicate detection, and label generation across different
#' biodiversity databases. It ensures consistency of location fields by correcting
#' common mistakes and variations. Country names are translated to English and
#' harmonized using `countrycode`, and country codes like `"BR"` or `"BRA"` are
#' converted into names. Missing countries are taken from the Brazilian state
#' (e.g. `"Bahia"`), from a country name in the locality (e.g. `"Manaus, Brasil"`),
#' or from other records with the same state and county. Brazilian and U.S.
#' state abbreviations are expanded to full names. Brazilian state names written
#' out in other ways are matched to the official names: with an `"Est. do"`,
#' `"Estado de"` or `"State of"` prefix (e.g. `"Est. do Bahia"`), without accents
#' or in another case (e.g. `"Paraiba"`, `"PARAIBA"`), and `"Brasilia"` for the
#' Federal District. Missing municipalities are
#' taken from the county. Words for municipality in `county` and `municipality`,
#' e.g. `"Manaus Mun."`, `"Mun. de Castelo"`, `"Município de Mucugê"` or
#' `"Arriaga Municipio"`, are removed, leaving just the name.
#' Localities all in upper case get just the first letter of each word in upper
#' case, e.g. `"ESTR. UBAITUBA / MARÁU AO KM 54 ."` into `"Estr. Ubaituba/Maráu ao
#' Km 54."`, keeping prepositions in lowercase and road codes (`"BR-316"`), acronyms
#' (`"ESALQ"`) and compass points (`"10 Km NE de"`) in upper case.
#'
#' @usage
#' std_place(df = NULL,
#'           colname_continent = "continent",
#'           colname_country = "country",
#'           colname_stateProvince = "stateProvince",
#'           colname_county = "county",
#'           colname_municipality = "municipality",
#'           colname_locality = "locality",
#'           rm_original_column = TRUE)
#'
#' @param df A data frame containing biodiversity records.
#' @param colname_continent Column name for continent (default: `"continent"`).
#' @param colname_country Column name for country (default: `"country"`).
#' @param colname_stateProvince Column name for state or province (default: `"stateProvince"`).
#' @param colname_county Column name for county (default: `"county"`).
#' @param colname_municipality Column name for municipality (default: `"municipality"`).
#' @param colname_locality Column name for locality (default: `"locality"`).
#' @param rm_original_column Logical; if `TRUE`, original columns are removed
#' after cleaning (default: `TRUE`).
#'
#' @return A data frame with standardized geographic information. If
#' `rm_original_column = FALSE`, the original columns are retained with `*Original`
#' suffixes.
#'
#' @examples
#' \dontrun{
#' df <- read.csv("herbarium_records.csv")
#' df_clean <- std_place(df,
#'                       colname_country = "pais",
#'                       colname_stateProvince = "estado",
#'                       rm_original_column = FALSE)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom tibble add_column
#' @importFrom dplyr filter mutate select
#' @importFrom stringi stri_trans_general
#' @importFrom countrycode countrycode countryname
#' @importFrom tools toTitleCase
#'
#' @export

std_place <- function(df = NULL,
                      colname_continent = "continent",
                      colname_country = "country",
                      colname_stateProvince = "stateProvince",
                      colname_county = "county",
                      colname_municipality = "municipality",
                      colname_locality = "locality",
                      rm_original_column = TRUE) {

  # Check the arguments ####
  .check_df(df)
  .check_flag(rm_original_column)
  .check_colnames(colname_continent = colname_continent, colname_country = colname_country,
                  colname_stateProvince = colname_stateProvince, colname_county = colname_county,
                  colname_municipality = colname_municipality, colname_locality = colname_locality)

  # Adjust colnames in the input dataset ####
  colnames_df <- names(df)
  if (colname_continent != "continent") {
    names(df)[colnames_df %in% colname_continent] <- "continent"
  }
  if (colname_country != "country") {
    names(df)[colnames_df %in% colname_country] <- "country"
  }
  if (colname_stateProvince != "stateProvince" ) {
    names(df)[colnames_df %in% colname_stateProvince] <- "stateProvince"
  }
  if (colname_county != "county") {
    names(df)[colnames_df %in% colname_county] <- "county"
  }
  if (colname_municipality != "municipality") {
    names(df)[colnames_df %in% colname_municipality] <- "municipality"
  }
  if (colname_locality != "locality") {
    names(df)[colnames_df %in% colname_locality] <- "locality"
  }

  #_____________________________________________________________________________
  # Standardizing COUNTRY ####
  if ("country" %in% names(df)) {

    message("std_place $country")

    df$country <- as.character(df$country)
    #df$country <- stringi::stri_trans_general(df$country, "Latin-ASCII")

    # Remove original country column ####
    if (rm_original_column == FALSE) {
      df <- df %>%
        tibble::add_column(countryOriginal = df$country,
                           .before = "country")
    } else {
      message(paste0("Original uncleaned '", colname_country, "' column removed"))
    }

    temp <- c("unknown or invalid", "Unknown", "Sem proced\u00eancia", "s.", "s.loc.",
              "s.l.",  "S.loc.", "s/localidade", "Indeterminada", "Indeterminado",
              "?")
    tf <- df$country %in% temp
    if (any(tf)) {
      df$country[tf] <- NA
    }

    df$country <- gsub("[[]|[]]", "", df$country)

    df$country <- .firstUp(df$country)

    tolowercountries <- grepl("[[:upper:]]{5,}", df$country)
    if (any(tolowercountries)) {
      df$country[tolowercountries] <- .firstUp(tolower(df$country[tolowercountries]))
    }

    # Fix country names such as put them in English ####
    df <- .fix_country_names(df)

    # Country codes into names, e.g. "BR", "GY" or "BRA" ####
    df$country <- .country_code_to_name(df$country)

    # Missing countries from the state or the locality, e.g. "Bahia" or
    # "Manaus, Brasil" ####
    df <- .country_from_place(df)


  }

  #_____________________________________________________________________________
  # Standard STATE ####

  if ("country" %in% names(df) &&
      "stateProvince" %in% names(df) &&
      "locality" %in% names(df)) {

    message("std_place $stateProvince")

    # Remove original stateProvince column ####
    if (rm_original_column == FALSE) {
      df <- df %>%
        tibble::add_column(stateProvinceOriginal = df$stateProvince,
                           .before = "stateProvince")
    } else {
      message(paste0("Original uncleaned '", colname_stateProvince, "' column removed"))
    }

    if (any(df$country %in% "Brazil")) {
      # Convert Brazilian states acronyms into their full name
      BrazilStates <- data.frame(states=c("Acre","Alagoas","Amap\u00e1","Amazonas","Bahia",
                                          "Cear\u00e1","Distrito Federal","Esp\u00edrito Santo",
                                          "Goi\u00e1s","Maranh\u00e3o","Mato Grosso","Mato Grosso do Sul",
                                          "Minas Gerais","Par\u00e1","Para\u00edba","Paran\u00e1",
                                          "Pernambuco","Piau\u00ed","Rio de Janeiro",
                                          "Rio Grande do Norte","Rio Grande do Sul",
                                          "Rond\u00f4nia","Roraima","Santa Catarina","S\u00e3o Paulo",
                                          "Sergipe","Tocantins"),
                                 acronyms1=c("AC","AL","AP","AM","BA","CE","DF","ES",
                                             "GO","MA","MT","MS","MG","PA","PB","PR",
                                             "PE","PI","RJ","RN","RS","RO","RR","SC",
                                             "SP","SE","TO"),
                                 acronyms2=c("Ac","Al","Ap","Am","Ba","Ce","Df","Es",
                                             "Go","Ma","Mt","Ms","Mg","Pa","Pb","Pr",
                                             "Pe","Pi","Rj","Rn","Rs","Ro","RR","Sc",
                                             "Sp","Se","To"))

      # Only for Brazilian records, as acronyms like "AL" or "PA" are also
      # used for states of other countries
      brazil <- df$country %in% "Brazil"
      for (acronyms in list(BrazilStates$acronyms1, BrazilStates$acronyms2)) {
        tf <- brazil & df$stateProvince %in% acronyms
        df$stateProvince[tf] <- BrazilStates$states[match(df$stateProvince[tf], acronyms)]
      }

      # Written-out variants of the state names: an "Est. do", "Estado de" or
      # "State of" prefix ("Est. do Bahia"), missing accents or another case
      # ("Paraiba", "PARAIBA", "Sao Paulo"), and "Brasilia" for the Federal
      # District
      state_key <- function(x) {
        gsub("\\s+", " ", tolower(stringi::stri_trans_general(trimws(x), "Latin-ASCII")))
      }
      prefix <- "^(est(ado)?\\.?|state)\\s+((do|da|de|of)\\s+)?"
      st_key <- sub(prefix, "", state_key(df$stateProvince))
      st_key[st_key %in% c("brasilia", "distrito federal (brasilia)")] <- "distrito federal"
      m <- match(st_key, state_key(BrazilStates$states))
      tf <- brazil & !is.na(m)
      df$stateProvince[tf] <- BrazilStates$states[m[tf]]

      # Further standardizing Brazilian states
      # More on ifelse expression: https://www.datamentor.io/r-programming/if-else-statement/
      df$stateProvince <- ifelse(df$stateProvince == "Amap&#225;",
                                 "Amap\u00e1", as.character(df$stateProvince))
      df$stateProvince <- ifelse(df$stateProvince %in% c("Para", "Par\u00c3\u00a1", "Par&#225;"),
                                 "Par\u00e1", as.character(df$stateProvince))
      df$stateProvince <- ifelse(df$stateProvince == "Rond&#244;nia",
                                 "Rond\u00f4nia", as.character(df$stateProvince))
    }

    usa <- df$country %in% c("United States", "United States of America", "USA")
    if (any(usa)) {
      # Converting USA states acronyms into complete name states
      usa_states <- c(stats::setNames(datasets::state.name, datasets::state.abb),
                      DC = "District of Columbia", AS = "American Samoa", GU = "Guam",
                      MP = "Northern Mariana Islands", PR = "Puerto Rico",
                      UM = "Minor Outlying Islands", VI = "U.S. Virgin Islands")
      tf <- usa & df$stateProvince %in% names(usa_states)
      df$stateProvince[tf] <- usa_states[df$stateProvince[tf]]
    }

    # Further standardizing the state/province
    df$stateProvince[which(df$stateProvince %in% c("unspecified",
                                                   "Unknown"))] <- NA
    df$stateProvince <- gsub("\\sdept[.].*", "", df$stateProvince)
    df$stateProvince <- gsub("\\sDept[.].*", "", df$stateProvince)
    df$stateProvince <- gsub("^Fco[.]\\s", "Francisco ", df$stateProvince)
    df$stateProvince <- gsub("^Franciso\\s", "Francisco ", df$stateProvince)
    df$stateProvince <- gsub("\\s[(]Prov[.][)].*", "", df$stateProvince)
    df$stateProvince <- gsub("Provincia\\sde\\s", "", df$stateProvince)
    df$stateProvince <- gsub("\\s[[].*", "", df$stateProvince)
    df$stateProvince <- gsub("\\s[(].*", "", df$stateProvince)

    tf <- grepl("[[:upper:]]{4,}", df$stateProvince)
    if(any(tf)){
      df$stateProvince[tf] <- toTitleCase(tolower(df$stateProvince[tf]))
    }
    tf <- grepl("[[:upper:]][[:lower:]]+\\s[[:lower:]]+", df$stateProvince)
    if (any(tf)) {
      df$stateProvince[tf] <- toTitleCase(df$stateProvince[tf])
    }
    df$stateProvince <- gsub("\\sDe\\s", " de ", df$stateProvince)
    df$stateProvince <- gsub("\\sLos\\s", " los ", df$stateProvince)
    df$stateProvince <- gsub("\\sLa\\s", " la ", df$stateProvince)
    # Portuguese and Spanish particles, e.g. "Rio Grande Do Norte", "Alvorada Do Norte"
    df$stateProvince <- gsub("\\s(Do|Da|Dos|Das|Del|E|Y)\\s", " \\L\\1 ", df$stateProvince, perl = TRUE)

    # Get state/province name whenever it is lacking but information is at the locality
    tf <- is.na(df$stateProvince)
    tftf <- grepl("^Depto[.]", df$locality[tf])
    if (any(tftf)) {
      df$locality[tf][tftf] <- gsub("^Depto[.]\\s", "",
                                    df$locality[tf][tftf])
      df$stateProvince[tf][tftf] <- gsub(",.+", "",
                                         df$locality[tf][tftf])
      # Remove all before the first comma and space
      df$locality[tf][tftf] <- sub(".*?,\\s", "",
                                   df$locality[tf][tftf])
    }

    tf <- is.na(df$stateProvince)
    tftf <- grepl("^State\\sof\\s", df$locality[tf])
    if (any(tftf)) {
      df$locality[tf][tftf] <- gsub("^State\\sof\\s", "",
                                    df$locality[tf][tftf])
      df$locality[tf][tftf] <- gsub("[.]", ",",
                                    df$locality[tf][tftf])
      df$stateProvince[tf][tftf] <- gsub(",.+", "",
                                         df$locality[tf][tftf])
      # Remove all before the first comma and space
      df$locality[tf][tftf] <- sub(".*?,\\s", "",
                                   df$locality[tf][tftf])
      df$locality[tf][tftf] <- sub("[,]$", ".",
                                   df$locality[tf][tftf])
    }

    tf <- is.na(df$stateProvince)
    tftf <- grepl("State\\sof\\s", df$locality[tf])
    if (any(tftf)) {
      df$locality[tf][tftf] <- sub(".*?State\\sof\\s", "",
                                   df$locality[tf][tftf])
      df$locality[tf][tftf] <- gsub("[.]", ",",
                                    df$locality[tf][tftf])
      df$stateProvince[tf][tftf] <- gsub(",.+", "",
                                         df$locality[tf][tftf])
      # Remove all before the first comma and space
      df$locality[tf][tftf] <- sub(".*?,\\s", "",
                                   df$locality[tf][tftf])
      df$locality[tf][tftf] <- sub("[,]$", ".",
                                   df$locality[tf][tftf])
    }

    # Deleting state/province name from the locality
    tf <- !is.na(df$stateProvince)
    tftf <- grepl("^Dept[.]", df$locality[tf])
    if (any(tftf)) {
      df$locality[tf][tftf] <- sub(".*?[:]\\s", "",
                                   df$locality[tf][tftf])
      df$locality[tf][tftf] <- gsub("^Dept[.]\\sof\\s", "",
                                    df$locality[tf][tftf])
      df$locality[tf][tftf] <- gsub("[.]$", "",
                                    df$locality[tf][tftf])
    }

    tf <- !is.na(df$stateProvince)
    tftf <- grepl("^State of", df$locality[tf])
    if (any(tftf)) {
      df$locality[tf][tftf] <- gsub("[.]", ",",
                                    df$locality[tf][tftf])
      df$locality[tf][tftf] <- gsub("[:]", ",",
                                    df$locality[tf][tftf])
      df$locality[tf][tftf] <- sub(".*?,", "",
                                   df$locality[tf][tftf])
      df$locality[tf][tftf] <- gsub("^\\s", "",
                                    df$locality[tf][tftf])
      df$locality[tf][tftf] <- gsub("[,]$", ".",
                                    df$locality[tf][tftf])
    }

    # Put original stateProvince name back ####
    if (colname_stateProvince != "stateProvince") {
      names(df)[names(df) %in% "stateProvince"] <- colname_stateProvince
      if (rm_original_column == FALSE) {
        names(df)[names(df) %in% "stateProvinceOriginal"] <- paste0(colname_stateProvince, "Original")
      }
    }

  }

  #_____________________________________________________________________________
  # Standardizing CONTINENT ####

  if ("country" %in% names(df) &&
      "continent" %in% names(df)) {

    message("std_place $continent")

    # Remove original continent column ####
    if (rm_original_column == FALSE) {
      df <- df %>%
        tibble::add_column(continentOriginal = df$continent,
                           .before = "continent")
    } else {
      message(paste0("Original uncleaned '", colname_continent, "' column removed"))
    }

    # Also when all continents are missing, so they are added from the country
    {
      # Cleaning the names in the column continent
      df$continent <- ifelse(df$continent %in% "Asia-Temperate", "Asia", as.character(df$continent))
      df$continent <- ifelse(df$continent %in% c("Am\u00e9rica", "AMERIQUE DU SUD",
                                                 "South America - Neotropics",
                                                 "Am\u00e9rica do Sul", "Southern America",
                                                 "Middle and South America", "SOUTH_AMERICA"),
                             "South America", as.character(df$continent))
      df$continent <- ifelse(df$continent %in% c("Northern America", "NORTH_AMERICA"),
                             "North America", as.character(df$continent))
      df$continent <- ifelse(df$continent %in% "ASIA", "Asia", as.character(df$continent))
      df$continent <- ifelse(df$continent %in% "AFRICA", "Africa", as.character(df$continent))
      df$continent <- ifelse(df$continent %in% "EUROPE", "Europe", as.character(df$continent))
      df$continent <- ifelse(df$continent %in% "OCEANIA", "Oceania", as.character(df$continent))

      # Adding continent names when lacking in the database
      Africa <- c('Algeria','Angola','Benin','Botswana','Burkina Faso','Burundi','Cabo Verde','Cameroon','Central African Republic','Chad','Comoros','Democratic Republic of the Congo','Republic of the Congo','Cote d\'Ivoire','Djibouti','Egypt','Equatorial Guinea','Eritrea','Ethiopia','Gabon','Gambia','Ghana','Guinea','Guinea Bissau','Kenya','Lesotho','Liberia','Libya','Madagascar','Malawi','Mali','Mauritania','Mauritius','Morocco','Mozambique','Namibia','Niger','Nigeria','Rwanda','Sao Tome and Principe','Senegal','Seychelles','Sierra Leone','Somalia','South Africa','South Sudan','Sudan','Swaziland','Tanzania','Togo','Tunisia','Uganda','Zambia','Zimbabwe')
      NorthAmerica<- c('Antigua and Barbuda','Puerto Rico','Trinidad & Tobago','Bahamas','Barbados','Belize','Canada','Costa Rica','Cuba','Dominica','Dominican Republic','El Salvador','Grenada','Guatemala','Haiti','Honduras','Jamaica','Mexico','Nicaragua','Panama','Saint Kitts and Nevis','Saint Lucia','Saint Vincent and the Grenadines','Trinidad and Tobago','United States of America','United States')
      SouthAmerica<- c('Argentina', 'Bolivia','Brazil','Chile','Colombia','Ecuador','French Guiana','Guyana','Paraguay','Peru','Suriname','Uruguay','Venezuela')
      Europe <- c('Albania','Andorra','Armenia','Austria','Azerbaijan','Belarus','Belgium','Bosnia and Herzegovina','Bulgaria','Croatia','Cyprus','Czech Republic','Denmark','Estonia','Finland','France','Georgia','Germany','Greece','Iceland','Ireland','Italy','Kazakhstan','Kosovo','Latvia','Liechtenstein','Lithuania','Luxembourg','Macedonia','Malta','Moldova','Monaco','Montenegro','Netherlands','Norway','Poland','Portugal','Romania','Russia','San Marino','Serbia','Slovakia','Slovenia','Spain','Sweden','Switzerland','Turkey','Ukraine','United Kingdom','Vatican City')
      Asia <- c('Armenia','Hong Kong','Azerbaijan','Bahrain','Bangladesh','Bhutan','Brunei', 'Cambodia','China','Cyprus','Georgia','India','Indonesia','Iran','Iraq','Israel', 'Japan','Jordan','Kazakhstan','Kuwait','Kyrgyzstan','Laos','Lebanon','Malaysia','Maldives','Mongolia','Myanmar','Nepal','North Korea','Oman','Pakistan','Palestine','Philippines','Qatar','Russia','Saudi Arabia','Singapore','South Korea','Sri Lanka','Syria','Taiwan','Tajikistan','Thailand','Timor Leste','Turkey','Turkmenistan','United Arab Emirates','Uzbekistan','Vietnam','Yemen')
      Oceania <- c("Australia","Federated States of Micronesia","Fiji","Kiribati","Marshall Islands","Nauru","New Zealand","Palau","Papua New Guinea","Samoa","Solomon Islands","Tonga","Tuvalu","Vanuatu")

      continents <- data.frame(country=Africa, continent="Africa")
      continents <- rbind(continents, data.frame(country=NorthAmerica, continent="North America"))
      continents <- rbind(continents, data.frame(country=SouthAmerica, continent="South America"))
      continents <- rbind(continents, data.frame(country=Europe, continent="Europe"))
      continents <- rbind(continents, data.frame(country=Asia, continent="Asia"))
      continents <- rbind(continents, data.frame(country=Oceania, continent="Oceania"))

      for (i in continents$country) {
        df$continent <- ifelse(df$country == i,
                               as.character(continents$continent[which(continents$country == i)]),
                               as.character(df$continent))
      }
    }

    # Put original continent name back ####
    if (colname_continent != "continent") {
      names(df)[names(df) %in% "continent"] <- colname_continent
      if (rm_original_column == FALSE) {
        names(df)[names(df) %in% "continentOriginal"] <- paste0(colname_continent, "Original")
      }
    }

  }

  #_____________________________________________________________________________
  # Standardizing COUNTY ####

  if ("county" %in% names(df)) {

    message("std_place $county")

    # Remove original county column ####
    if (rm_original_column == FALSE) {
      df <- df %>%
        tibble::add_column(countyOriginal = df$county,
                           .before = "county")
    } else {
      message(paste0("Original uncleaned '", colname_county, "' column removed"))
    }

    df$county <- .rm_municipality_word(df$county)
    df$county[which(df$county %in% c("NO DISPONIBLE",
                                     "Unknown",
                                     "Unknown-E",
                                     "Unplaced"))] <- NA
    # Unknown county, e.g. "inc.sed.cty." (incertae sedis county)
    df$county[grepl("^inc[.]?\\s*sed", df$county, ignore.case = TRUE)] <- NA

    df$county <- gsub("^[(]", "", df$county)
    df$county <- gsub("[)]$", "", df$county)
    df$county <- gsub("\\sCo[.].*", "", df$county)
    df$county <- gsub("\\sCounty.*", "", df$county)
    df$county <- gsub("\\sMunicipality.*", "", df$county)
    df$county <- gsub("\\sMunicipio.*", "", df$county)
    df$county <- gsub("\\s[(]Prov[.][)].*", "", df$county)
    df$county <- gsub("^Fco[.]\\s", "Francisco ", df$county)

    tf <- grepl("[[:upper:]]{4,}", df$county)
    if (any(tf)) {
      df$county[tf] <- toTitleCase(tolower(df$county[tf]))
    }
    df$county <- gsub("\\sDe\\s", " de ", df$county)
    df$county <- gsub("\\sLos\\s", " los ", df$county)
    df$county <- gsub("\\sLa\\s", " la ", df$county)
    # Portuguese and Spanish particles, e.g. "Rio Grande Do Norte", "Alvorada Do Norte"
    df$county <- gsub("\\s(Do|Da|Dos|Das|Del|E|Y)\\s", " \\L\\1 ", df$county, perl = TRUE)

    # Put original county name back ####
    if (colname_county != "county") {
      names(df)[names(df) %in% "county"] <- colname_county
      if (rm_original_column == FALSE) {
        names(df)[names(df) %in% "countyOriginal"] <- paste0(colname_county, "Original")
      }
    }

  }

  #_____________________________________________________________________________
  # Standardizing MUNICIPALTY and LOCALITY ####

  if ("municipality" %in% names(df) &&
      "locality" %in% names(df)) {

    message("std_place $municipality and $locality")

    # Remove original municipality and locality columns ####
    if (rm_original_column == FALSE) {
      df <- df %>%
        tibble::add_column(municipalityOriginal = df$municipality,
                           .before = "municipality") %>%
        tibble::add_column(localityOriginal = df$locality,
                           .before = "locality")
    } else {
      message(paste0("Original uncleaned '", colname_municipality, "' and ", colname_locality, "' columns removed"))
    }

    # Localities all in upper case, e.g. "ESTR. UBAITUBA / MAR\u00c1U AO KM 54 ."
    df$locality <- .fix_upper_locality(df$locality)

    df$municipality <- .rm_municipality_word(df$municipality)
    tf <- grepl("[[:upper:]]{4,}", df$municipality)
    if (any(tf)) {
      df$municipality[tf] <- toTitleCase(tolower(df$municipality[tf]))
    }
    df$municipality <- gsub("\\sDe\\s", " de ", df$municipality)
    df$municipality <- gsub("\\sLos\\s", " los ", df$municipality)
    df$municipality <- gsub("\\sLa\\s", " la ", df$municipality)
    # Portuguese and Spanish particles, e.g. "Rio Grande Do Norte", "Alvorada Do Norte"
    df$municipality <- gsub("\\s(Do|Da|Dos|Das|Del|E|Y)\\s", " \\L\\1 ", df$municipality, perl = TRUE)

    # Get county/municipality name whenever it is lacking but information is at the locality
    tf <- is.na(df$municipality)
    tftf <- grepl("^Municipio\\sde\\s", df$locality[tf])
    if (any(tftf)) {
      df$locality[tf][tftf] <- gsub("^Municipio\\sde\\s", "",
                                    df$locality[tf][tftf])
      df$locality[tf][tftf] <- gsub("[.]", ",",
                                    df$locality[tf][tftf])
      df$municipality[tf][tftf] <- gsub(",.+", "",
                                        df$locality[tf][tftf])
      # Remove all before the the first comma and space
      df$locality[tf][tftf] <- sub(".*?,\\s", "",
                                   df$locality[tf][tftf])
      df$locality[tf][tftf] <- sub("[,]$", ".",
                                   df$locality[tf][tftf])
    }

    tf <- is.na(df$municipality)
    tftf <- grepl("^Municipio\\s", df$locality[tf])
    if (any(tftf)) {
      df$locality[tf][tftf] <- gsub("^Municipio\\s", "",
                                    df$locality[tf][tftf])
      df$municipality[tf][tftf] <- gsub(",.+", "",
                                        df$locality[tf][tftf])
      # Remove all before the the first comma and space
      df$locality[tf][tftf] <- sub(".*?,\\s", "",
                                   df$locality[tf][tftf])
    }

    tf <- is.na(df$municipality)
    tftf <- grepl("^Mun[.]\\s", df$locality[tf])
    if (any(tftf)) {
      df$locality[tf][tftf] <- gsub("^Mun[.]\\s", "",
                                    df$locality[tf][tftf])
      df$municipality[tf][tftf] <- gsub(",.+", "",
                                        df$locality[tf][tftf])
      # Remove all before the the first comma and space
      df$locality[tf][tftf] <- sub(".*?,\\s", "",
                                   df$locality[tf][tftf])
    }

    tf <- is.na(df$municipality)
    tftf <- grepl("^Mpio[.]\\s", df$locality[tf])
    if (any(tftf)) {
      df$locality[tf][tftf] <- gsub("^Mpio[.]\\sde\\s", "",
                                    df$locality[tf][tftf])
      df$locality[tf][tftf] <- gsub("^Mpio[.]\\s", "",
                                    df$locality[tf][tftf])
      df$locality[tf][tftf] <- gsub("^Sta.\\s", "Santa ",
                                    df$locality[tf][tftf])
      df$locality[tf][tftf] <- gsub("[.]", ",",
                                    df$locality[tf][tftf])
      df$municipality[tf][tftf] <- gsub(",.+", "",
                                        df$locality[tf][tftf])
      # Remove all before the the first comma and space
      df$locality[tf][tftf] <- sub(".*?,\\s", "",
                                   df$locality[tf][tftf])
      df$locality[tf][tftf] <- sub("[,]$", ".",
                                   df$locality[tf][tftf])
    }

    # Municipality from the county, when missing, e.g. in GBIF records of
    # Brazil, where the municipality is often in the county ####
    if (colname_county %in% names(df)) {
      county <- df[[colname_county]]
      tf <- (is.na(df$municipality) | !nzchar(trimws(df$municipality))) &
        !is.na(county) & nzchar(trimws(county))
      df$municipality[tf] <- county[tf]
    }

    # Put original municipality name back ####
    if (colname_municipality != "municipality") {
      names(df)[names(df) %in% "municipality"] <- colname_municipality
      if (rm_original_column == FALSE) {
        names(df)[names(df) %in% "municipalityOriginal"] <- paste0(colname_municipality, "Original")
      }
    }

    # Put original locality name back ####
    if (colname_locality != "locality") {
      names(df)[names(df) %in% "locality"] <- colname_locality
      if (rm_original_column == FALSE) {
        names(df)[names(df) %in% "localityOriginal"] <- paste0(colname_locality, "Original")
      }
    }

  }

  # Put original country name back ####
  # Only at the end, as the steps for state and continent need the country
  if (colname_country != "country") {
    names(df)[names(df) %in% "country"] <- colname_country
    if (rm_original_column == FALSE) {
      names(df)[names(df) %in% "countryOriginal"] <- paste0(colname_country, "Original")
    }
  }

  return(df)
}


#_______________________________________________________________________________
# Standardize country names into English ####
.fix_country_names <- function(df) {

  # SOUTH AMERICA ####
  temp <- c("Bra$", "Braail", "Brail", "Brasi", "Brasiil", "BraSil", "Brasl",
            "Brasil", "Br\u00e9sil austral", "Br\u00e9sil central", "B\u00e9sil",
            "Br\u00e9sil m\u00e9ridional", "Br\u00e9sil septentrional", "Brasil.", "Brsail",
            "Brasilia tropica", "Brasil australs", "Brasilia tropicae", "Br\u00e9zil",
            "Br\u00e9sil", "BRazil", "Br\u00e9brsil", "Brasiliae", "Brasiliae tropicae",
            "Brasill", "Br\u00e9brsil", "Btasil", "Bresil", "Bresille", "Basil",
            "Barsil", "Br\u00e9sil")
  pattern <- paste0(temp, collapse = "|")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$country, pattern), FALSE)
  if (any(tf)) {
    df$country[tf] <- "Brazil"
  }

  temp <- c("Bol\u00edvia", "Bolivia", "Bolovie", "Bolivie", "Bol  via")
  pattern <- paste0(temp, collapse = "|")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$country, pattern), FALSE)
  if (any(tf)) {
    df$country[tf] <- "Bolivia"
  }

  temp <- c("ane anglaise$", "ane Anglaise$", "^Guyana$", "ane angloise$", "^Guian$",
            "Guyanna$", "Guyane[?]$", "^Guyanne$", "^Guyane$", "ane Guyane$",
            "Guayne$", "^Guiana$", "^British Guiana$", "Guiana Brit", "Guiana ang", "Guyane ang",
            "^Guiane$", "ana Inglesa$", "ane britannique$", "^Grayanae$",
            "British gu", "British Gu")
  pattern <- paste0(temp, collapse = "|")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$country, pattern), FALSE)
  if (any(tf)) {
    df$country[tf] <- "Guyana"
  }

  temp <- c("Guyana fra", "Guyane Fra", "Guyanne fra", "Guyane fra", "Guiane fra", "Guinana Fra",
            "Guiana Fra")
  pattern <- paste0(temp, collapse = "|")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$country, pattern), FALSE)
  if (any(tf)) {
    df$country[tf] <- "French Guiana"
  }

  temp <- c("Peru", "Perou", "P\u00e9rou", "Per\u00fa")
  pattern <- paste0(temp, collapse = "|")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$country, pattern), FALSE)
  if (any(tf)) {
    df$country[tf] <- "Peru"
  }

  temp <- c("Surinam", "ane hollandaise$", "ana Holandesa$", "Dutch Guyana",
            "Guyane hol", "Guiana Holandesa")
  pattern <- paste0(temp, collapse = "|")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$country, pattern), FALSE)
  if (any(tf)) {
    df$country[tf] <- "Surinam"
  }

  temp <- c("Venezu", "Venezula", "V\u00e9n\u00e9zuela", "V\u00e9n\u00e9zu\u00e9la", "Venezula")
  pattern <- paste0(temp, collapse = "|")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$country, pattern), FALSE)
  if (any(tf)) {
    df$country[tf] <- "Venezuela"
  }

  # NORTH AMERICA ####
  temp <- c("United State", "USA$", "E[.]U[.]A[.]", "U[.]S[.]A[.]", "^US$",
            "^\u00c9tats-Unis", "^Etats-Unis", "Etats Unis d",
            "^\u00c9tats-unis", "^Estados Un", "Estado Unidos da Am\u00e9rica$")
  pattern <- paste0(temp, collapse = "|")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$country, pattern), FALSE)
  if (any(tf)) {
    df$country[tf] <- "United States"
  }

  # EUROPE ####
  temp <- c("Alemaha", "Alemanga", "Alemanha")
  pattern <- paste0(temp, collapse = "|")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$country, pattern), FALSE)
  if (any(tf)) {
    df$country[tf] <- "Germany"
  }

  temp <- c("UK$", "Inglaterra")
  pattern <- paste0(temp, collapse = "|")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$country, pattern), FALSE)
  if (any(tf)) {
    df$country[tf] <- "England"
  }

  # AFRICA ####
  temp <- c("\u00c1frica do Sul", "\u00c1frica do sul", "South Africa", "Africa do Sul")
  pattern <- paste0(temp, collapse = "|")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$country, pattern), FALSE)
  if (any(tf)) {
    df$country[tf] <- "South Africa"
  }

  # sort(unique(df$country[tf]))
  # sort(unique(df$country))

  en <- countrycode::countryname(sourcevar = df$country,
                                 destination = "country.name.en",
                                 warn = FALSE)
  en[is.na(en)] <- df$country[is.na(en)]  # Keep original for unmatched
  df$country <- en

  # Convert country codes into country names

  # Problem with BR states acronyms
  # countrycodes <- grepl("[[:upper:]]{2,}", df$country)
  # if (any(countrycodes)) {
  #   df$country[countrycodes] <- countrycode::countrycode(df$country[countrycodes],
  #                                                        "iso2c", "country.name")
  # }

  return(df)
}

# Brazilian states ####
.brazil_states <- c("Acre", "Alagoas", "Amap\u00e1", "Amazonas", "Bahia", "Cear\u00e1",
                    "Distrito Federal", "Esp\u00edrito Santo", "Goi\u00e1s", "Maranh\u00e3o",
                    "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Par\u00e1",
                    "Para\u00edba", "Paran\u00e1", "Pernambuco", "Piau\u00ed", "Rio de Janeiro",
                    "Rio Grande do Norte", "Rio Grande do Sul", "Rond\u00f4nia", "Roraima",
                    "Santa Catarina", "S\u00e3o Paulo", "Sergipe", "Tocantins")

# Country codes into country names, e.g. "BR" or "BRA" into "Brazil" ####
.country_code_to_name <- function(x) {
  for (code in c("iso2c", "iso3c")) {
    tf <- grepl(if (code == "iso2c") "^[A-Z]{2}$" else "^[A-Z]{3}$", x)
    if (any(tf)) {
      en <- suppressWarnings(countrycode::countrycode(x[tf], code, "country.name.en",
                                                      warn = FALSE))
      x[tf][!is.na(en)] <- en[!is.na(en)]
    }
  }
  x
}

# Missing countries from other place columns ####
# 1. Brazilian states, except names also used in other countries, e.g.
# "Amazonas" (also in Colombia, Peru and Venezuela) or "Distrito Federal"
# 2. Country names at the start or end of the locality, e.g. "Brasil" or
# "Manaus, Brazil"
# 3. The country of other records with the same state and county, or the same
# state, when all of them are from a single country, e.g. "Amazonas, Manaus"
.country_from_place <- function(df) {
  miss <- function() is.na(df$country) | !nzchar(trimws(df$country))
  if (!any(miss())) return(df)
  norm <- function(x) tolower(trimws(stringi::stri_trans_general(x, "Latin-ASCII")))
  state <- if ("stateProvince" %in% names(df)) norm(df$stateProvince) else rep(NA, nrow(df))
  county <- if ("county" %in% names(df)) norm(df$county) else rep(NA, nrow(df))

  # 1. Brazilian states
  br <- norm(setdiff(.brazil_states, c("Amazonas", "Distrito Federal")))
  tf <- miss() & state %in% br
  df$country[tf] <- "Brazil"

  # 2. Country names in the locality
  if ("locality" %in% names(df) && any(miss())) {
    i <- which(miss() & !is.na(df$locality))
    parts <- strsplit(df$locality[i], "\\s*[],;:.()[]\\s*")
    ends <- lapply(parts, function(x) {
      x <- x[nzchar(x)]
      unique(c(x[1], x[length(x)]))
    })
    seg <- unique(unlist(ends))
    seg <- as.character(seg[!is.na(seg)])
    en <- if (length(seg) > 0) {
      suppressWarnings(countrycode::countryname(seg, "country.name.en", warn = FALSE))
    } else {
      character(0)
    }
    # Only whole names of countries, also in other languages, and not names
    # also used for people or places, e.g. "Georgia" or "Jordan"
    ok <- !is.na(en) & en %in% countrycode::codelist$country.name.en &
      !en %in% c("Unknown", "Georgia", "Jordan", "Chad", "Niger") &
      nchar(seg) >= 4 & !grepl("\\d", seg) &
      lengths(strsplit(seg, "\\s+")) <= lengths(strsplit(en, "\\s+")) + 1
    found <- vapply(ends, function(x) {
      m <- en[match(x, seg)][ok[match(x, seg)]]
      m <- unique(m[!is.na(m)])
      if (length(m) == 1) m else NA_character_
    }, character(1))
    df$country[i][!is.na(found)] <- found[!is.na(found)]
    # Brazilian states as the locality, e.g. "Par\u00e1"
    tf <- miss() & norm(df$locality) %in% br
    df$country[tf] <- "Brazil"
  }

  # 3. Country of other records with the same state (and county)
  for (key in list(paste(state, county), state)) {
    if (!any(miss())) break
    known <- !miss() & !is.na(state)
    if (!any(known)) break
    countries <- tapply(df$country[known], key[known], function(x) unique(x))
    single <- countries[lengths(countries) == 1]
    tf <- miss() & !is.na(state) & key %in% names(single)
    df$country[tf] <- unlist(single)[key[tf]]
  }
  df
}

# Localities all in upper case to just the first letter of each word in upper
# case, e.g. "ESTR. UBAITUBA / MARÁU AO KM 54 ." to "Estr. Ubaituba/Maráu ao
# Km 54.". Prepositions and conjunctions go to lower case, while road codes
# ("BR-316", "PA 150"), roman numerals, acronyms of institutions, compass points
# ("10 Km NE de") and a final state code ("Manaus, AM") are kept in upper case ####
.fix_upper_locality <- function(x) {
  # Not codes of datasets, e.g. "JABOT_RJ"
  tf <- !is.na(x) & stringi::stri_detect_regex(x, "\\p{Lu}{2}") &
    !stringi::stri_detect_regex(x, "\\p{Ll}|_")
  if (!any(tf)) return(x)

  y <- stringi::stri_trans_tolower(x[tf])

  # Spaces, e.g. "RIO  ORINOCO  ." and "UBAITUBA  /  MARÁU"
  y <- gsub("\\s+", " ", trimws(y))
  y <- gsub("\\s+([,.;:)])", "\\1", y)
  y <- gsub("\\(\\s+", "(", y)
  y <- gsub("\\s*/\\s*", "/", y)

  # First letter of each word
  y <- gsub("(?<![\\p{L}\\p{M}\\p{N}'\u2019])(\\p{L})", "\\U\\1", y, perl = TRUE)

  # Letters after a number, as in coordinates, but not in "86km"
  y <- gsub("(?<=\\d)(?!km\\b|m\\b)(\\p{L})", "\\U\\1", y, perl = TRUE)

  # Prepositions and conjunctions, except at the beginning or in abbreviations
  particles <- c("a", "ao", "aos", "as", "\u00e0", "\u00e0s", "o", "os", "com", "da",
                 "das", "de", "do", "dos", "e", "em", "entre", "na", "nas", "no",
                 "nos", "para", "pela", "pelo", "por", "que",
                 "al", "con", "del", "en", "hacia", "y",
                 "about", "and", "at", "by", "from", "in", "near", "of", "on",
                 "the", "to", "with")
  words <- paste0("(?i)(?<=\\s)(", paste(particles, collapse = "|"), ")(?=\\s)")
  y <- gsub(words, "\\L\\1", y, perl = TRUE)

  # Spanish articles after a preposition, but not in names like "Los Tuxtlas"
  y <- gsub("\\b(de|al?|en|hacia|por|para|con|entre)\\s(El|La|Las|Los)\\b", "\\1 \\L\\2",
            y, perl = TRUE)

  # Compass points after a distance, e.g. "10 Km E de", "al NO de"
  y <- gsub("(?i)(km[.]?\\s|\\bal\\s)(n|s|e|w|o|l|ne|no|nw|se|so|sw)\\b",
            "\\1\\U\\2\\E", y, perl = TRUE)

  # Road codes, e.g. "BR 060", "BR-316", "PA 150"
  y <- gsub("\\b(?!(?:Km|Ca|Rio|Rua|Rod|Est|Av|Mil|Ano)\\b)(\\p{Lu}\\p{Ll}{1,2})(?=[ -]?\\d)",
            "\\U\\1", y, perl = TRUE)

  # Roman numerals
  y <- gsub("\\b(Ii|Iii|Iv|Vi|Vii|Viii|Ix|Xi|Xii|Xiii|Xiv|Xv|Xvi|Xx)\\b", "\\U\\1", y, perl = TRUE)

  # Acronyms of institutions and places
  acronyms <- c("CEPEC", "CEPLAC", "CVRD", "EMBRAPA", "ESALQ", "ESAV", "FLONA", "FUNAI",
                "IBAMA", "IBGE", "ICMBIO", "INCRA", "INPA", "JABOT", "PARNA", "REBIO",
                "RPPN", "SUDAM", "UFAM", "UFBA", "UFMG", "UFPA", "UFRJ", "UHE", "USP",
                "UTM")
  y <- gsub(paste0("(?i)\\b(", paste(acronyms, collapse = "|"), ")\\b"), "\\U\\1",
            y, perl = TRUE)

  # A state code of Brazil alone or at the end, e.g. "Manaus, AM", "Paraty - RJ."
  states <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS",
              "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC",
              "SE", "SP", "TO")
  y <- gsub(paste0("(?i)(^|[,/-]\\s*)(", paste(states, collapse = "|"), ")([.]?)$"),
            "\\1\\U\\2\\E\\3", y, perl = TRUE)

  x[tf] <- y
  x
}



# Remove words for municipality from county and municipality names, e.g.
# "Manaus Mun.", "Mun. de Castelo", "Mpio. Ocosingo", "Munic\u00edpio de Mucug\u00ea",
# "Municipio Gran Sabana", "Arriaga Municipio", "Mecayapan Municipality" or
# "Linhares (Munic\u00edpio)", but not names like "Novo Mundo" ####
.rm_municipality_word <- function(x) {
  word <- "(mun|mpio|munic)[.]|munic[i\u00ed]pio|municipality|municipalidad"
  x <- gsub(paste0("(?i)^\\s*(", word, ")\\s*((de|do|da|dos|das|del|of)\\s+)?"), "",
            x, perl = TRUE)
  x <- gsub(paste0("(?i)[\\s,-]*[(]?\\b(", word, "|mun|mpio)[)]?\\s*$"), "", x, perl = TRUE)
  x <- trimws(x)
  x[x %in% ""] <- NA
  x
}
