#' Combine and Harmonize Multiple Herbarium Data Sources
#'
#' @author Domingos Cardoso
#'
#' @description
#' Merges herbarium records from two or more biodiversity data sources into a single
#' harmonized data frame. Optionally prioritizes specific sources when duplicates
#' are detected across herbaria, retaining records based on a flexible exclusion
#' strategy. The function keeps non-Brazilian herbaria records by default, assuming
#' higher completeness from global repositories.
#'
#' @details
#' This function aligns column structures, removes redundant records from overlapping
#' herbaria, and merges all sources into a single output. Duplicate filtering is
#' based on matching `collectionCode` across sources. Users can specify a preferred
#' source (`keep_source`) when duplicates exist.
#'
#' @usage
#' barroso_cat(list_sources = list(source1, source2, ...),
#'             keep_source = NULL)
#'
#' @param list_sources A named list of data frames. Each element represents a
#' herbarium data source. The names of the list are used to track the source origin
#' for internal filtering.
#' @param keep_source Optional character string specifying the preferred data
#' source (e.g., "GBIF") for resolving duplicate `collectionCode` conflicts. If
#' NULL, all records are retained.
#'
#' @return A harmonized data frame combining all provided herbarium sources, with
#' columns aligned and optionally filtered to resolve duplicate collections.
#'
#' @examples
#'
#' combined_df <- barroso_cat(list_sources = list(GBIF = gbif_data,
#'                                                speciesLink = splink_data,
#'                                                JABOT = jabot_data),
#'                                 keep_source = "GBIF")
#'
#' @importFrom dplyr bind_rows filter
#' @export

barroso_cat <- function(list_sources = list(), keep_source = NULL) {
  if (!is.list(list_sources) || length(list_sources) < 2) {
    stop("Please provide a named list of at least two data sources.")
  }

  # Extract unique collection codes by source
  herb_by_source <- lapply(list_sources, function(df) unique(df$collectionCode))

  # Flatten into a mapping for overlap comparison
  overlap_map <- table(unlist(herb_by_source))
  duplicated_herbaria <- names(overlap_map[overlap_map > 1])

  # Initialize filtered list
  filtered_list <- list()

  for (source_name in names(list_sources)) {
    df <- list_sources[[source_name]]

    if (!"collectionCode" %in% names(df)) {
      warning(paste("Skipping", source_name, ": missing 'collectionCode' column."))
      next
    }

    if (!is.null(keep_source)) {
      if (source_name == keep_source) {
        df <- df
      } else {
        df <- dplyr::filter(df, !collectionCode %in% duplicated_herbaria)
      }
    }

    filtered_list[[source_name]] <- df
  }

  # Harmonize columns by adding missing ones as NA
  all_columns <- unique(unlist(lapply(filtered_list, names)))
  for (i in seq_along(filtered_list)) {
    missing_cols <- setdiff(all_columns, names(filtered_list[[i]]))
    filtered_list[[i]][missing_cols] <- NA
  }

  combined_df <- dplyr::bind_rows(filtered_list)

  return(combined_df)
}
