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
#' harmonized using `countrycode` Brazilian and U.S. state abbreviations are
#' expanded to full names.
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

    temp <- c("unknown or invalid", "Unknown", "Sem procedência", "s.", "s.loc.",
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

    # Put original country name back ####
    if (colname_country != "country") {
      names(df)[names(df) %in% "country"] <- colname_country
      if (rm_original_column == FALSE) {
        names(df)[names(df) %in% "countryOriginal"] <- paste0(colname_country, "Original")
      }
    }

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
      BrazilStates <- data.frame(states=c("Acre","Alagoas","Amapá","Amazonas","Bahia",
                                          "Ceará","Distrito Federal","Espírito Santo",
                                          "Goiás","Maranhão","Mato Grosso","Mato Grosso do Sul",
                                          "Minas Gerais","Pará","Paraíba","Paraná",
                                          "Pernambuco","Piauí","Rio de Janeiro",
                                          "Rio Grande do Norte","Rio Grande do Sul",
                                          "Rondônia","Roraima","Santa Catarina","São Paulo",
                                          "Sergipe","Tocantins"),
                                 acronyms1=c("AC","AL","AP","AM","BA","CE","DF","ES",
                                             "GO","MA","MT","MS","MG","PA","PB","PR",
                                             "PE","PI","RJ","RN","RS","RO","RR","SC",
                                             "SP","SE","TO"),
                                 acronyms2=c("Ac","Al","Ap","Am","Ba","Ce","Df","Es",
                                             "Go","Ma","Mt","Ms","Mg","Pa","Pb","Pr",
                                             "Pe","Pi","Rj","Rn","Rs","Ro","RR","Sc",
                                             "Sp","Se","To"))

      for (i in BrazilStates$acronyms1) {
        df$stateProvince <- ifelse(df$stateProvince == i,
                                   as.character(BrazilStates$states[which(BrazilStates$acronyms1 == i)]),
                                   as.character(df$stateProvince))
      }

      for (i in BrazilStates$acronyms2) {
        df$stateProvince <- ifelse(df$stateProvince == i,
                                   as.character(BrazilStates$states[which(BrazilStates$acronyms2 == i)]),
                                   as.character(df$stateProvince))
      }

      # Further standardizing Brazilian states
      # More on ifelse expression: https://www.datamentor.io/r-programming/if-else-statement/
      df$stateProvince <- ifelse(df$stateProvince == "Amap&#225;",
                                 "Amapá", as.character(df$stateProvince))
      df$stateProvince <- ifelse(df$stateProvince %in% c("Para", "ParÃ¡", "Par&#225;"),
                                 "Pará", as.character(df$stateProvince))
      df$stateProvince <- ifelse(df$stateProvince == "Rond&#244;nia",
                                 "Rondônia", as.character(df$stateProvince))
    }

    if (any(df$country %in% "United States of America")) {
      # Converting USA states acronyms into complete name states
      USAStates <- data.frame(states=c("Alabama", "Alaska", "American Samoa", "Arizona",
                                       "Arkansas", "California", "Colorado", "Connecticut",
                                       "Delaware", "District of Columbia", "Florida",
                                       "Georgia", "Guam", "Hawaii", "Idaho", "Illinois",
                                       "Indiana", "Iowa", "Kansas", "Kentucky",
                                       "Louisiana", "Maine", "Maryland", "Massachusetts",
                                       "Michigan", "Minnesota", "Minor Outlying Islands",
                                       "Mississippi", "Missouri", "Montana", "Nebraska",
                                       "Nevada", "New Hampshire", "New Jersey", "New Mexico",
                                       "New York", "North Carolina", "North Dakota",
                                       "Northern Mariana Islands", "Ohio", "Oklahoma",
                                       "Oregon", "Pennsylvania", "Puerto Rico",
                                       "Rhode Island", "South Carolina", "South Dakota",
                                       "Tennessee", "Texas", "U.S. Virgin Islands",
                                       "Utah", "Vermont", "Virginia", "Washington",
                                       "West Virginia", "Wisconsin", "Wyoming"),
                              acronyms=c("AK", "AL", "AR", "AS", "AZ", "CA", "CO",
                                         "CT", "DC", "DE", "FL", "GA", "GU", "HI",
                                         "IA", "ID", "IL", "IN", "KS", "KY", "LA",
                                         "MA", "MD", "ME", "MI", "MN", "MO", "MP",
                                         "MS", "MT", "NC", "ND", "NE", "NH", "NJ",
                                         "NM", "NV", "NY", "OH", "OK", "OR", "PA",
                                         "PR", "RI", "SC", "SD", "TN", "TX", "UM",
                                         "UT", "VA", "VI", "VT", "WA", "WI", "WV",
                                         "WY"))

      for (i in USAStates$acronyms) {
        df$stateProvince <- ifelse(df$stateProvince == i,
                                   as.character(USAStates$states[which(USAStates$acronyms == i)]),
                                   as.character(df$stateProvince))
      }

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
    tft <- grepl("^Dept[.]", df$locality[tf])
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

    if (any(!is.na(df$continent))) {
      # Cleaning the names in the column continent
      df$continent <- ifelse(df$continent %in% "Asia-Temperate", "Asia", as.character(df$continent))
      df$continent <- ifelse(df$continent %in% c("América", "AMERIQUE DU SUD",
                                                 "South America - Neotropics",
                                                 "América do Sul", "Southern America",
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
      NorthAmerica<- c('Antigua and Barbuda','Puerto Rico','Trinidad & Tobago','Bahamas','Barbados','Belize','Canada','Costa Rica','Cuba','Dominica','Dominican Republic','El Salvador','Grenada','Guatemala','Haiti','Honduras','Jamaica','Mexico','Nicaragua','Panama','Saint Kitts and Nevis','Saint Lucia','Saint Vincent and the Grenadines','Trinidad and Tobago','United States of America')
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

    df$county[which(df$county %in% c("NO DISPONIBLE",
                                     "Unknown",
                                     "Unknown-E",
                                     "Unplaced"))] <- NA

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
        tibble::add_column(countyOriginal = df$municipality,
                           .before = "municipality") %>%
        tibble::add_column(countyOriginal = df$locality,
                           .before = "locality")
    } else {
      message(paste0("Original uncleaned '", colname_municipality, "' and ", colname_locality, "' columns removed"))
    }

    tf <- grepl("[[:upper:]]{4,}", df$municipality)
    if (any(tf)) {
      df$municipality[tf] <- toTitleCase(tolower(df$municipality[tf]))
    }
    df$municipality <- gsub("\\sDe\\s", " de ", df$municipality)
    df$municipality <- gsub("\\sLos\\s", " los ", df$municipality)
    df$municipality <- gsub("\\sLa\\s", " la ", df$municipality)

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
        names(df)[names(df) %in% "countyOriginal"] <- paste0(colname_locality, "Original")
      }
    }

  }

  return(df)
}


#_______________________________________________________________________________
# Standardize country names into English ####
.fix_country_names <- function(df) {

  # SOUTH AMERICA ####
  temp <- c("Bra$", "Braail", "Brail", "Brasi", "Brasiil", "BraSil", "Brasl",
            "Brasil", "Brésil austral", "Brésil central", "Bésil",
            "Brésil méridional", "Brésil septentrional", "Brasil.", "Brsail",
            "Brasilia tropica", "Brasil australs", "Brasilia tropicae", "Brézil",
            "Brésil", "BRazil", "Brébrsil", "Brasiliae", "Brasiliae tropicae",
            "Brasill", "Brébrsil", "Btasil", "Bresil", "Bresille", "Basil",
            "Barsil", "Brésil")
  pattern <- paste0(temp, collapse = "|")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$country, pattern), FALSE)
  if (any(tf)) {
    df$country[tf] <- "Brazil"
  }

  temp <- c("Bolívia", "Bolivia", "Bolovie", "Bolivie", "Bol  via")
  pattern <- paste0(temp, collapse = "|")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$country, pattern), FALSE)
  if (any(tf)) {
    df$country[tf] <- "Bolivia"
  }

  temp <- c("ane anglaise$", "ane Anglaise$", "^Guyana$", "ane angloise$", "^Guian$",
            "Guyanna$", "Guyane[?]$", "^Guyanne$", "^Guyane$", "ane Guyane$",
            "Guayne$", "Guiana$", "Guiana Brit", "Guiana ang", "Guyane ang",
            "^Guiane$", "ana Inglesa$", "ane britannique$", "^Grayanae$",
            "British gu", "British Gu")
  pattern <- paste0(temp, collapse = "|")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$country, pattern), FALSE)
  if (any(tf)) {
    df$country[tf] <- "Guyana"
  }

  temp <- c("Guyana fra", "Guyane Fra", "Guyanne fra", "Guyane fra", "Guinana Fra",
            "Guiana Fra")
  pattern <- paste0(temp, collapse = "|")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$country, pattern), FALSE)
  if (any(tf)) {
    df$country[tf] <- "French Guiana"
  }

  temp <- c("Peru", "Perou", "Pérou", "Perú")
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

  temp <- c("Venezu", "Venezula", "Vénézuela", "Vénézuéla", "Venezula")
  pattern <- paste0(temp, collapse = "|")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$country, pattern), FALSE)
  if (any(tf)) {
    df$country[tf] <- "Venezula"
  }

  # NORTH AMERICA ####
  temp <- c("United State", "USA$", "E[.]U[.]A[.]", "U[.]S[.]A[.]", "^US$",
            "^États-Unis", "^Etats-Unis", "Etats Unis d",
            "^États-unis", "^Estados Un", "Estado Unidos da América$")
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
  temp <- c("África do Sul", "África do sul", "South Africa", "Africa do Sul")
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

# Side function make first letter to upper case ####
.firstUp <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
