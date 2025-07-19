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

    df$typeStatus <- gsub("^não$", NA, df$typeStatus)
    df$typeStatus <- gsub("^sim\\s-\\s", "", df$typeStatus)
    df$typeStatus <- gsub("Rabo de macaco|Fotografia do Tipo|Epítipo|^NOTATYPE|Neótipo|Cotipo|Merotypus|Possible type|EPITYPE", NA, df$typeStatus)
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
