#' Flag or Remove Duplicate Specimens
#'
#' @author Domingos Cardoso
#'
#' @description
#' Identifies and optionally removes duplicate herbarium specimen records based on
#' collector name and number, or—when these are missing—by species name and date.
#' Adds a logical `duplicate` column to indicate flagged duplicates.
#'
#' @details
#' This function is part of the internal workflow of the `barRoso` package, supporting
#' record reconciliation and dataset cleaning. It uses combinations of collector names
#' (`recordedBy`), collection numbers (`recordNumber`), and collection dates (`year`,
#' `month`, `day`) to identify duplicate entries. When `rm_duplicates = TRUE`, one
#' record from each duplicated group is retained, and all others are removed.
#' Specimens missing collector numbers are handled in a separate logic pass using
#' additional fields (`species`, `recordedBy`, `year`, `month`, `day`) to detect
#' duplicates.
#'
#' @usage
#' barroso_flag_duplicates(df,
#'                         rm_duplicates = FALSE)
#'
#' @param df A data frame with biodiversity specimen records.
#' @param rm_duplicates Logical; if `TRUE`, removes duplicates and retains one record
#' per duplicated group (default: `FALSE`).
#'
#' @return A data frame with an added `duplicate` column. If `rm_duplicates = TRUE`,
#' duplicated entries are removed based on standardized logic.
#'
#' @examples
#' \dontrun{
#' df <- read.csv("herbarium_data.csv")
#' df_flagged <- barroso_flag_duplicates(df)
#' df_clean <- barroso_flag_duplicates(df, rm_duplicates = TRUE)
#' }
#'
#' @importFrom tibble add_column
#' @importFrom dplyr arrange
#'
#' @export

barroso_flag_duplicates <- function(df,
                                    rm_duplicates = FALSE,
                                    colname_recordedBy = "recordedBy",
                                    colname_recordNumber = "recordNumber",
                                    colname_genus = "genus",
                                    colname_specificEpithet = "specificEpithet") {

  tf <- is.na(df[[colname_recordNumber]])
  df_na <- df[tf, ]
  df <- df[!tf, ]

  duplicates <- c(duplicated(df[, c(colname_recordedBy, colname_recordNumber)],
                             fromLast = TRUE) |
                    duplicated(df[, c(colname_recordedBy, colname_recordNumber)]))

  df <- tibble::add_column(df, scientificName_temp = paste(df[[colname_genus]], df[[colname_specificEpithet]]), .after = colname_recordNumber)
  df_na <- tibble::add_column(df_na, scientificName_temp = paste(df_na[[colname_genus]], df_na[[colname_specificEpithet]]), .after = colname_recordNumber)

  df <- tibble::add_column(df, duplicate = duplicates, .after = colname_recordNumber)

  # Identify all duplicates when specimens do not have collector number
  duplicates <- c(duplicated(df_na[, c("scientificName_temp", colname_recordedBy, colname_recordNumber)], fromLast = TRUE) |
                    duplicated(df_na[, c("scientificName_temp", colname_recordedBy, colname_recordNumber)]))
  df_na <- tibble::add_column(df_na, duplicate = duplicates, .after = colname_recordNumber)

  duplicates <- c(duplicated(df_na[, c("scientificName_temp", colname_recordedBy,
                                       "year", "month", "day")], fromLast = TRUE) |
                    duplicated(df_na[, c("scientificName_temp", colname_recordedBy,
                                         "year", "month", "day")]))

  df_na$duplicate[duplicates] <- TRUE
  df_na$duplicate[!duplicates] <- FALSE

  df <- rbind(df, df_na)

  df <- df %>% arrange(recordedBy, recordNumber)


  # Remove duplicates
  if (rm_duplicates) {

    df$duplicate[is.na(df[,c(colname_recordNumber)]) &
                   grepl("TRUE", df[,c("duplicate")]) == T] <- NA

    # Deleting duplicated specimens based on collector + collector number
    df <- df[duplicated(df[,c(colname_recordedBy,
                              colname_recordNumber,
                              "duplicate")])==F, ]

    # Deleting duplicated in cases when the collector number is missing
    df_na <- df_na[duplicated(df_na[,c("scientificName_temp",
                                       colname_recordedBy,
                                       colname_recordNumber,
                                       "duplicate",
                                       "year", "month", "day")])==F, ]

    df <- rbind(df, df_na)
  }

  df <- df[ , !(names(df) %in% "scientificName_temp")]

  return(df)
}
