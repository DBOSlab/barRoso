#' Standardize Herbarium Acronyms in Collection Records
#'
#' @author Domingos Cardoso
#'
#' @description
#' Cleans and standardizes herbarium acronyms in biodiversity datasets by harmonizing
#' values from the `collectionCode` and `institutionCode` fields. The function
#' corrects common issues in GBIF and other aggregated records, replacing ambiguous
#' or placeholder codes with recognized herbarium acronyms. It also flags missing
#' values with fallback rules and optional original column retention.
#'
#' @details
#' This function is part of the `barRoso` package, and applies a large set of
#' conditional replacements based on known patterns and integrates fallback from
#' `institutionCode` when `collectionCode` is missing or ambiguous. Common aliases
#' like `"Herbarium"`, `"Botany"`, or `"Angiosperms"` are converted to valid acronyms
#' when possible.
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
#' \dontrun{
#' df <- read.csv("gbif_download.csv")
#' df_clean <- std_collection(df,
#'                            colname_collectionCode = "collection_code",
#'                            colname_institutionCode = "institution_code",
#'                            rm_original_column = FALSE)
#' }
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
    names(df)[colnames_df %in% colname_collectionCode] <- "institutionCode"
  }

  if ("collectionCode" %in% names(df) &&
      "institutionCode" %in% names(df)) {

    message("std_collection $collectionCode and $institutionCode")

    # Remove original typeStatus column ####
    if (rm_original_column == FALSE) {
      df <- df %>%
        tibble::add_column(collectionCodeOriginal = df$collectionCode,
                           .before = "collectionCode") %>%
        tibble::add_column(institutionCodeOriginal = df$institutionCode,
                           .before = "institutionCode")
    } else {
      message(paste0("Original uncleaned '", colname_collectionCode, "' and '", colname_institutionCode, "' columns removed"))
    }

    df$collectionCode <- as.character(df$collectionCode)
    df$institutionCode <- as.character(df$institutionCode)

    df$institutionCode <-
      stringi::stri_trans_general(df$institutionCode, "Latin-ASCII")
    df$collectionCode <-
      stringi::stri_trans_general(df$collectionCode, "Latin-ASCII")

      df$collectionCode <- ifelse(is.na(df$collectionCode), df$institutionCode, df$collectionCode)

    # General cleaning of herbarium acronyms
    df$collectionCode <- ifelse(df$collectionCode == "Botany", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "MBML-HERBARIO", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Herbarium W", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "TUBvplantscoll", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Angiosperms", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Herbrecs", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Herb", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "HERB", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "General", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "GOET-Typen", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "GOETvplantscoll", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "PLANT", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "HERBIER", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "INRA Antilles-Guyane", "INRA", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "HERBMG", "MG", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "FMNH-B-Types", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Herbarium", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Plant specimens", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "sgn_africanplants", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "VascularPlants", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Plantae", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "INBio", "INB", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Herbier National du Gabon", "LBV", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Plants", "ASU", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "ASU-PLANTS", "ASU", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "MNHN", "P", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "RECOLNAT_MNHN_P", "P", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "RECOLNAT_UM_MPU", "MPU", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Museum National d'Histoire Naturelle, Paris", "P", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "*Corporacion Autonoma Regional para la Defensa de la Meseta de Bucaramanga", "UIS", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "UIS-H", "UIS", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "UBCVascular", "UBC", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Museu Botanico Municipal Curritiba, Brasilia", "MBM", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Herbario Federico Meden Bogota (FMB)", "FMB", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Herbarium WU", "WU", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Herbario da Universidade Federal de Roraima", "UFRR", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Herbario Nacional de Costa Rica (CR)", "CR", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Staatliches Museum fur Naturkunde Stuttgart, Herbarium", "STU", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "HBGSpermatophyta - Herbarium Hamburgense", "HBG", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Universidade Federal de Mato Grosso", "UFMT", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "MFS-FRUTOS", "MFS", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "UNAPHerbarium", "AMAZ", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "R-TIPOS", "R", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Herbarium University Ulm", "ULM", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Herbier CNRST/INERA", "HNBU", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Herbarium Senckenbergianum", "FR", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Herbarium Berolinense", "B", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Herbarium LAGU", "LAGU", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Herbarium - Las Cruces Biological Station", "HLDG", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "HBGSpermatophyta - Herbarium Hamburgense", "HBG", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Colecao Botanica, Instituto Nacional de Pesquisas da Amazonia (INPA)", "INPA", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Geneva Herbarium - De Candolle's Prodromus (G-DC)", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Geneva Herbarium - General Collection (G)", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Geneva Herbarium ???? General Collection (G)", "G", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "FMNH-SEEDPLANTS", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "F_BOTANY_BR", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "MOBOT_BR", "MO", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "NYBG", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "NHM-LONDON-BOT", "BM", df$collectionCode)
    df$collectionCode <- ifelse(df$collectionCode == "Royal Botanic Gardens", "K", df$collectionCode)

    df$collectionCode <- ifelse(df$institutionCode == "TAI", df$institutionCode, df$collectionCode)
    df$collectionCode <- ifelse(df$institutionCode == "NHMUK", "BM", df$collectionCode)
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
    names(df)[names(df) %in% "genus"] <- colname_institutionCode
    if (rm_original_column == FALSE) {
      names(df)[names(df) %in% "institutionCodeOriginal"] <- paste0(colname_institutionCode, "Original")
    }
  }

  return(df)
}

