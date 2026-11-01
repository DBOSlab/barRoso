#' Standardize Herbarium Acronyms in Collection Records
#'
#' @author Domingos Cardoso
#'
#' @description
#' Cleans and standardizes herbarium acronyms in biodiversity datasets by harmonizing
#' values from the `collectionCode` and `institutionCode` fields. The function
#' corrects common issues in GBIF and other aggregated records, replacing ambiguous
#' or placeholder codes with the herbarium acronyms of
#' [Index Herbariorum](https://sweetgum.nybg.org/science/ih/).
#'
#' @details
#' This function is part of the `barRoso` package. Instead of a dictionary of
#' every code, it uses regular expressions that generalize across datasets, in
#' the following order:
#'
#' - Aggregated collections like `"Tropicos"` or `"MO-Duplicates"` are replaced
#'   with the herbarium of the specimen given in `institutionCode` (e.g. `"NY"`),
#'   or with `"MO"` when it is missing.
#' - Generic words like `"Botany"`, `"Herbarium"`, `"Vascular Plants"` or
#'   `"PNGplants"` are replaced with the acronym in `institutionCode` or, when
#'   missing, with the acronym at the beginning of `catalogNumber` (e.g.
#'   `"U.1317533"` into `"U"`) or in `occurrenceID` (e.g. `"urn:catalog:INB:..."`).
#' - Names of collections, like `"F-Seedplants"`, `"BHCB-Fanerogamas"`,
#'   `"INPA-Carpoteca"`, `"R-Tipos"`, `"UIS-H"` or `"BSMvplantscoll"`, are reduced
#'   to the herbarium acronym. Codes of wood collections, like `"INPAw"`, are
#'   kept, as they are Index Herbariorum codes.
#' - Names of herbaria and institutions, like `"Herbario INPA"`,
#'   `"Herbario Nacional de Costa Rica (CR)"` or `"Instituto de Botanica de Sao Paulo"`,
#'   are replaced with the acronym given in the name or with the known acronym.
#' - Codes that are not acronyms, like observation projects (`"Observations"`,
#'   `"Dados de Pesquisa SISBio"`), companies (`"Ecopetrol S.A"`) or numbers,
#'   are replaced with the acronym in `institutionCode` or `catalogNumber`, or
#'   with `NA`.
#'
#' The columns `catalogNumber` and `occurrenceID` are used when present.
#'
#' @usage
#' std_collection(df = NULL,
#'                colname_collectionCode = "collectionCode",
#'                colname_institutionCode = "institutionCode",
#'                rm_original_column = TRUE)
#'
#' @param df A data frame with biodiversity specimen records.
#' @param colname_collectionCode Name of the column containing collection codes (default: `"collectionCode"`).
#' @param colname_institutionCode Name of the column containing institution codes (default: `"institutionCode"`).
#' @param rm_original_column Logical; if `TRUE`, original columns are removed
#' after cleaning (default: `TRUE`).
#'
#' @return A data frame with standardized collection codes in the `collectionCode` column.
#' If `rm_original_column = FALSE`, the original values are saved with a `*Original` suffix.
#'
#' @examples
#' df <- data.frame(collectionCode = c("Tropicos", "Botany", "F-Seedplants",
#'                                     "Herbario INPA", "Observations"),
#'                  institutionCode = c("NY", NA, "F", "INPA", "iNaturalist"),
#'                  catalogNumber = c("100595392", "U.1317533", "V0059731F",
#'                                    "123", "340214363"))
#' std_collection(df)$collectionCode
#'
#' @importFrom tibble add_column
#' @importFrom magrittr %>%
#' @importFrom stringi stri_trans_general
#'
#' @export

std_collection <- function(df = NULL,
                           colname_collectionCode = "collectionCode",
                           colname_institutionCode = "institutionCode",
                           rm_original_column = TRUE) {

  colnames_df <- names(df)
  if (colname_collectionCode != "collectionCode") {
    names(df)[colnames_df %in% colname_collectionCode] <- "collectionCode"
  }
  if (colname_institutionCode != "institutionCode") {
    names(df)[colnames_df %in% colname_institutionCode] <- "institutionCode"
  }

  if ("collectionCode" %in% names(df) &&
      "institutionCode" %in% names(df)) {

    message("std_collection $collectionCode and $institutionCode")

    # Keep original columns ####
    if (rm_original_column == FALSE) {
      df <- df %>%
        tibble::add_column(collectionCodeOriginal = df$collectionCode,
                           .before = "collectionCode") %>%
        tibble::add_column(institutionCodeOriginal = df$institutionCode,
                           .before = "institutionCode")
    } else {
      message(paste0("Original uncleaned '", colname_collectionCode, "' and '", colname_institutionCode, "' columns removed"))
    }

    df$institutionCode <- .ascii_squish(df$institutionCode)
    df$collectionCode <- .std_herbarium_code(df$collectionCode,
                                             df$institutionCode,
                                             .col_or_na(df, "catalogNumber"),
                                             .col_or_na(df, "occurrenceID"))
  }

  # Put original collectionCode name back ####
  if (colname_collectionCode != "collectionCode") {
    names(df)[names(df) %in% "collectionCode"] <- colname_collectionCode
    if (rm_original_column == FALSE) {
      names(df)[names(df) %in% "collectionCodeOriginal"] <- paste0(colname_collectionCode, "Original")
    }
  }
  # Put original institutionCode name back ####
  if (colname_institutionCode != "institutionCode") {
    names(df)[names(df) %in% "institutionCode"] <- colname_institutionCode
    if (rm_original_column == FALSE) {
      names(df)[names(df) %in% "institutionCodeOriginal"] <- paste0(colname_institutionCode, "Original")
    }
  }

  return(df)
}


#_______________________________________________________________________________
# Auxiliary functions of std_collection ####

# Column as character, or NA when missing
.col_or_na <- function(df, col) {
  if (col %in% names(df)) as.character(df[[col]]) else rep(NA_character_, nrow(df))
}

# Without accents, extra spaces, empty cells and the text "NA" of spreadsheets
# exported from R (the code "NA" is not an herbarium)
.ascii_squish <- function(x) {
  x <- stringi::stri_trans_general(as.character(x), "Latin-ASCII")
  x <- trimws(gsub("\\s+", " ", x))
  x[x %in% c("", "NA", "na")] <- NA
  x
}

# Codes that look like Index Herbariorum acronyms, e.g. "RB", "HUEFS", "IAvH",
# "G-DC", "ECO-TA-H" or wood collections like "INPAw", but not words like
# "Herbarium", "Observations", "SinBiota", names with spaces or numbers
.is_acronym <- function(x) {
  !is.na(x) &
    grepl("^[A-Z][A-Za-z]{0,9}(-[A-Z][A-Za-z]{0,5}){0,2}w?$", x) &
    !grepl("[a-z]{3,}", x) &
    !x %in% .not_herbaria &
    !grepl("^(plantae?|plants|herb|botany|bot|flora|general|museum)$", x, ignore.case = TRUE)
}

# Collections and data providers that are not herbaria
.not_herbaria <- c("SinBiota", "SISBio", "ICMBio", "FAPESP", "DNA", "POLLENREFERENCE",
                   "CBG", "BOLD", "NCBI", "IDEAM", "EPM", "CAR", "CRQ", "CPR", "FPR", "AGAC")

# Institution codes, or names of herbaria and institutions, that are not the
# acronym of their herbarium in Index Herbariorum
.institution_acronyms <- c(
  "^MOBOT$" = "MO", "^MNHN$" = "P", "^NHMUK$" = "BM", "^NHM-LONDON-BOT$" = "BM",
  "^INBio$" = "INB", "^MPEG$" = "MG", "^JBRJ$" = "RB", "^MeiseBG$" = "BR",
  "^NBC$" = "L", "^NL$" = "L", "^NL-Botany$" = "L", "^M-VascularPlants$" = "M",
  "^LSU[(]NO[)]$" = "NO", "^NRM(-S)?$" = "S", "^CJBG$" = "G", "^Cenargen$" = "CEN",
  "^CAS(BOT|-BOT)?(-BC)?$|^BOT-BC$" = "CAS", "^FMNH$" = "F", "^UZH:Z$" = "Z",
  "^Unicamp$|Universidade Estadual de Campinas" = "UEC",
  "fcnym[.]unlp" = "LP", "^HERBMG$" = "MG", "^UNAPHerbarium$" = "AMAZ",
  "^INRA Antilles-Guyane$" = "INRA", "Herbier National du Gabon" = "LBV",
  "Herbario Tuxtlas" = "MEXU",
  "Instituto Nacional de Pesquisas da Amazonia|^Herbario INPA$" = "INPA",
  "Investigaciones Cientificas.*SINCHI|^SINCHI$" = "COAH",
  "Alexander von Humboldt|^IAvH$" = "FMB",
  "Museu?m? National d.Histoire Naturelle" = "P",
  "Rio de Janeiro Botanical Garden|Jardim Botanico do Rio de Janeiro" = "RB",
  "New York Botanical Garden" = "NY", "Royal Botanic Gardens?,? Kew|^Royal Botanic Gardens$" = "K",
  "Missouri Botanical Garden" = "MO", "Field Museum" = "F",
  "Herbarium Berolinense" = "B", "Herbarium Senckenbergianum" = "FR",
  "Herbarium Hamburgense" = "HBG", "Herbarium Haussknecht" = "JE",
  "Herbarium University Ulm" = "ULM", "Las Cruces Biological Station" = "HLDG",
  "South China Botanical Garden" = "IBSC", "Herbarium Bogoriense" = "BO",
  "Agraria La Molina" = "MOL", "Staatliches Museum fur Naturkunde Stuttgart" = "STU",
  "Federico Meden" = "FMB", "Herbier CNRST/INERA" = "HNBU",
  "Museu Botanico Municipal" = "MBM", "Universidade Federal de Roraima" = "UFRR",
  "Universidade Federal de Mato Grosso" = "UFMT", "Defensa de la Meseta de Bucaramanga" = "UIS",
  "Inst[a-z]*uto Florestal" = "SPSF", "Instituto de Botanica de Sao Paulo" = "SP",
  "Instituto Agronomico de Campinas|- IAC$" = "IAC", "ESALQ" = "ESA",
  "Instituto de Biociencias - Universidade de Sao Paulo|^USP-IB$" = "SPF",
  "IB/UNESP, Rio Claro" = "HRCB",
  "Instituto de Pesquisas Cientificas e Tecnologicas do Estado do Amapa" = "HAMAB")

# Replace codes and names that match the patterns of a named vector
.match_acronym <- function(x, patterns) {
  out <- rep(NA_character_, length(x))
  for (p in names(patterns)) {
    tf <- is.na(out) & !is.na(x) & grepl(p, x)
    out[tf] <- patterns[[p]]
  }
  out
}

# Acronym of an institution code, e.g. "MOBOT" into "MO", "Instituto Nacional
# de Pesquisas da Amazonia (INPA)" into "INPA", or NA when it is not an acronym
.inst_acronym <- function(inst) {
  out <- .match_acronym(inst, .institution_acronyms)
  # Acronym between parentheses, e.g. "Universidad de Antioquia (UdeA)"
  par <- sub(".*[(]([A-Za-z-]{1,10})[)]\\s*$", "\\1", inst)
  par[par == inst] <- NA
  tf <- is.na(out) & .is_acronym(inst)
  out[tf] <- inst[tf]
  tf <- is.na(out) & .is_acronym(par) & !.is_company(inst)
  out[tf] <- par[tf]
  out
}

# Companies, foundations and agencies that are not herbaria
.is_company <- function(x) {
  grepl(paste0("\\bS[.]\\s?A\\b|E[.]S[.]P|\\bS[.]A[.]S\\b|Corporaci|Fundaci|Empresas|",
               "Parques|Ecopetrol|Geopark|Isagen|Celsia|Ministerio|Conservation International|",
               "Hidrolog|Parex|PatriNat|DEAL |Area de Conservacion|GenBank|iNaturalist|Promigas|",
               "Consultor|Ingenieria"),
        x, ignore.case = TRUE)
}

# Acronym of the herbarium in the catalog number, at the end between parentheses
# in vouchers of GenBank sequences, e.g. "D. Cardoso 2118 (HUEFS)", or at the
# beginning, e.g. "U.1317533", "ESA:91746", "CM281029", "LSU00204747", but not
# a single letter followed by numbers, e.g. "M17116867", or in occurrenceID,
# e.g. "urn:catalog:INB:Atta:4359714"
.catalog_acronym <- function(catalog, occid) {
  par <- sub(".*[(]([A-Z][A-Za-z-]{0,9})[)]\\s*$", "\\1", catalog)
  par[par == catalog] <- NA
  pre <- sub("^([A-Z]{2,6}[ .:-]*|[A-Z][ .:-]+)[0-9].*$", "\\1", catalog)
  pre <- gsub("[ .:-]", "", pre)
  pre[pre == catalog] <- NA
  urn <- sub("^urn:catalog:([A-Za-z]{1,8}):.*$", "\\1", occid)
  urn[urn == occid] <- NA
  out <- ifelse(is.na(par), pre, par)
  out <- ifelse(is.na(out), urn, out)
  out[!.is_acronym(out)] <- NA
  out
}

# Standardize herbarium acronyms ####
.std_herbarium_code <- function(code, inst, catalog = NA, occid = NA) {

  code <- .ascii_squish(code)
  n <- length(code)
  inst <- rep_len(as.character(inst), n)
  catalog <- rep_len(as.character(catalog), n)
  occid <- rep_len(as.character(occid), n)

  # Acronym of the institution, or else of the catalog number, used when
  # collectionCode is missing or is not an herbarium acronym
  fallback <- .inst_acronym(inst)
  tf <- is.na(fallback) & (is.na(inst) | grepl("GenBank", inst))
  fallback[tf] <- .catalog_acronym(catalog, occid)[tf]

  # Missing codes
  out <- code
  out[is.na(code)] <- fallback[is.na(code)]
  done <- is.na(code)

  # Aggregated collections with specimens from many herbaria, e.g. "Tropicos"
  # and "MO-Duplicates" with the herbarium in institutionCode
  tf <- !done & grepl("^(Tropicos|MO-Duplicates)$", code, ignore.case = TRUE)
  out[tf] <- ifelse(.is_acronym(inst[tf]) & !inst[tf] %in% "MOBOT", inst[tf], "MO")
  done <- done | tf

  # Codes of the RECOLNAT network, e.g. "RECOLNAT_MNHN_P" into "P"
  tf <- !done & grepl("^RECOLNAT_", code)
  out[tf] <- sub(".*_", "", code[tf])
  done <- done | tf

  # Known names of collections, herbaria and institutions
  known <- .match_acronym(code, .institution_acronyms)
  tf <- !done & !is.na(known)
  out[tf] <- known[tf]
  done <- done | tf

  # Generic words, e.g. "Botany", "Herbarium", "Vascular Plants", "PNGplants",
  # "TUBvplantscoll", "FMNH-SEEDPLANTS", "F_BOTANY_BR", "Geneva Herbarium - General
  # Collection (G)", "FMNH-B-Types" (types of B photographed at F), codes with an
  # underline like "PELD_MAUA_JAU" or "FSN_FOREST_DYNAMICS", and numbers
  generic <- paste0("^(botany|bot|herb|herbarium|herbario|herbier|herbrecs|plants?|plantae|",
                    "plant specimens|flora|general|angiosperms?|vascular ?plants|",
                    "spermatophyta|seed ?plants|specimens?|collections?|observations)$|",
                    "^[A-Za-z]*(plants|vplantscoll)$|^FMNH-|^HU-Herb$|^NYBG$|^ZOBODAT$|^[^ ]+_[^ ]+$|",
                    "^Geneva Herbarium|^[0-9 .,|-]+$")
  tf <- !done & grepl(generic, code, ignore.case = TRUE)
  out[tf] <- fallback[tf]
  done <- done | tf

  # Names of collections with the acronym, e.g. "F-Seedplants", "BHCB-Fanerogamas",
  # "INPA-Carpoteca", "BHZBcarpo", "R-Tipos", "GOET-Typen", "LOJA-VASCULARES",
  # "UBCVascular", "MBML-Herbario", "BR-Meise", "HAS-PLANTAS-FUNGOS", "MFS-FRUTOS",
  # "ASU-PLANTS", "NRM-S-Plants" into "S", "CJBG-G-DC" into "G-DC"
  rest <- !done
  suffix <- paste0("[-_ ]?(Botany|BOT|Seedplants|Fanerogamas|Herbario|Herbarium|Carpoteca|",
                   "carpo|Vascular(es)?|Plants|Plantas(-Fungos)?|vplantscoll|Typen|Tipos|",
                   "Types|Meise|Frutos|Duplicates)$")
  x <- code[rest]
  x <- sub("^CJBG-", "", x)
  x <- sub(suffix, "", x, ignore.case = TRUE)
  # Final letter of some collections, e.g. "UIS-H", "ANDES-H", "RON-e", "MFS-E",
  # but not acronyms like "ECO-TA-H"
  x <- sub("^([A-Z]{2,})-[HEe]$", "\\1", x)
  known <- .match_acronym(x, .institution_acronyms)
  x <- ifelse(is.na(known), x, known)
  out[rest] <- ifelse(nchar(x) > 0, x, fallback[rest])

  # Names of herbaria with the acronym, e.g. "Herbario INPA", "Herbarium WU",
  # "Herbarium Hamburgense HBG", "Herbario Nacional de Costa Rica (CR)",
  # "Jardin Botanico de la Universidad del Valle (JBUV)"
  y <- out[rest]
  herb <- !.is_acronym(y) & grepl("Herb|Jard|Botan|Garden|Museu|Instit|Univers|Escola", y)
  acr <- ifelse(grepl("[(][A-Za-z-]{1,10}[)]\\s*$", y),
                sub(".*[(]([A-Za-z-]{1,10})[)]\\s*$", "\\1", y),
                sub(".*\\s([A-Z]{1,8})$", "\\1", y))
  tf <- herb & .is_acronym(acr) & acr != y
  y[tf] <- acr[tf]
  out[rest] <- y

  # Anything else that is not an acronym, e.g. "Dados de Pesquisa SISBio",
  # "UMS PatriNat", "Ecopetrol S.A" or "HT24989-25186"
  tf <- !.is_acronym(out)
  out[tf] <- fallback[tf]
  out[.is_company(out) | !.is_acronym(out)] <- NA

  # Herbaria known by other institution codes
  out[inst %in% "TAI"] <- "TAI"
  out[inst %in% "NHMUK"] <- "BM"

  out
}
