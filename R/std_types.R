#' Standardize and Fill Missing Type Status Information
#'
#' @author Domingos Cardoso
#'
#' @description
#' Cleans and standardizes the `typeStatus` column in biodiversity datasets,
#' addressing inconsistencies in type designations. It removes irrelevant entries,
#' harmonizes formatting, and optionally fills missing values if present in other
#' duplicate records (assumed to be handled outside this function).
#'
#' @details
#' This function is used internally in the `barRoso` package to prepare
#' type status data for reconciliation and label generation. It corrects known
#' placeholder or non-type entries (e.g. “Fotografia do Tipo”, “NOTATYPE”, “Epítipo”)
#' and simplifies terms like `"sim -"` to ensure clean type labels.
#'
#' @usage
#' std_types(df = NULL,
#'           colname_typeStatus = "typeStatus",
#'           rm_original_column = TRUE)
#'
#' @param df A data frame containing type designation records.
#' @param colname_typeStatus Name of the column holding type status information (default: `"typeStatus"`).
#' @param rm_original_column Logical; if `TRUE`, the original column is removed after cleaning (default: `TRUE`).
#'
#' @return A data frame with a standardized `typeStatus` column.
#' If `rm_original_column = FALSE`, the original values are preserved in a column named `typeStatusOriginal`.
#'
#' @examples
#' \dontrun{
#' df <- read.csv("specimens.csv")
#' df_clean <- std_types(df,
#'                       colname_typeStatus = "tipo",
#'                       rm_original_column = FALSE)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom tibble add_column
#'
#' @export

std_types <- function(df = NULL,
                      colname_typeStatus = "typeStatus",
                      rm_original_column = TRUE) {

  # Adjust colnames in the input dataset ####
  colnames_df <- names(df)
  if (colname_typeStatus != "typeStatus") {
    names(df)[colnames_df %in% colname_typeStatus] <- "typeStatus"
  }

  if ("typeStatus" %in% names(df)) {

    message("std_types $typeStatus")

    # Remove original typeStatus column ####
    if (rm_original_column == FALSE) {
      df <- df %>%
        tibble::add_column(typeStatusOriginal = df$typeStatus,
                           .before = "typeStatus")
    } else {
      message(paste0("Original uncleaned '", colname_typeStatus, "' column removed"))
    }

    x <- as.character(df$typeStatus)
    x[trimws(x) %in% c("", "NA", "n\u00e3o")] <- NA
    x <- gsub("^sim\\s-\\s", "", x)
    not_type <- paste("Rabo de macaco|Fotografia do Tipo|Ep\u00edtipo|^NOTATYPE|Ne\u00f3tipo|Cotipo",
                      "Merotypus|Possible type|EPITYPE", sep = "|")
    x[grepl(not_type, x, ignore.case = TRUE)] <- NA

    # Same term in English, Latin or Portuguese and in any case, e.g. "ISOTYPE",
    # "Isotypus" and "isótipo" into "isotype"
    df$typeStatus <- .std_type_terms(x)
  }

  # Put original typeStatus name back ####
  if (colname_typeStatus != "typeStatus") {
    names(df)[names(df) %in% "typeStatus"] <- colname_typeStatus
    if (rm_original_column == FALSE) {
      names(df)[names(df) %in% "typeStatusOriginal"] <- paste0(colname_typeStatus, "Original")
    }
  }

  return(df)
}


#_______________________________________________________________________________
# Type status terms into lowercase Darwin Core terms in English, keeping the
# name after "of", e.g. "ISOTYPUS of Ormosia amazonica Ducke" into
# "isotype of Ormosia amazonica Ducke" ####
.std_type_terms <- function(x) {
  has_name <- grepl("\\s+of\\s+", x)
  name <- ifelse(has_name, sub("^.*?\\s+of\\s+", " of ", x, perl = TRUE), "")
  term <- sub("\\s+of\\s+.*$", "", x)
  term <- tolower(stringi::stri_trans_general(trimws(term), "Latin-ASCII"))

  # Latin "-typus" and Portuguese "-tipo" into "-type"
  term <- sub("typus$", "type", term)
  term <- sub("tipo$", "type", term)
  # Portuguese "sintipo" into "syntype"
  term <- sub("^(iso)?sin(?=type$)", "\\1syn", term, perl = TRUE)
  term <- sub("^originalmaterial$", "original material", term)

  ifelse(is.na(x), NA_character_, paste0(term, name))
}
