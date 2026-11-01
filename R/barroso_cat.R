#' Combine and Harmonize Multiple Herbarium Data Sources
#'
#' @author Domingos Cardoso
#'
#' @description
#' Merges herbarium records from two or more biodiversity data sources into a single
#' harmonized data frame. Optionally prioritizes specific sources when duplicates
#' are detected across herbaria, retaining records based on a flexible exclusion
#' strategy.
#'
#' @details
#' This function aligns column structures, removes redundant records from overlapping
#' herbaria, and merges all sources into a single output. Duplicate filtering is
#' based on matching `collectionCode` across sources: when `keep_source` is given,
#' the records of any herbarium that is also present in `keep_source` are removed
#' from the other sources. Herbaria shared only among the other sources are kept.
#'
#' Each source is first formatted into Darwin Core column names and values with
#' [barroso_dwc()], e.g. the columns of speciesLink downloads (`collector`,
#' `collectornumber`, `recordedby`) are renamed into `recordedBy` and
#' `recordNumber`, and the dates and type status of REFLORA and JABOT records are
#' standardized. The sources are recognized by their content, so their names in
#' `list_sources` can be any name.
#'
#' Columns with different types across sources (e.g. numeric `year` in one source
#' and character in another) are combined as character. A column `datasource`
#' records the source of each record.
#'
#' @usage
#' barroso_cat(list_sources = list(),
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
#' columns aligned, a `datasource` column, and optionally filtered to resolve
#' duplicate collections.
#'
#' @examples
#' \dontrun{
#' reflora <- refloraR::reflora_records(herbarium = "RB", taxon = "Ormosia", save = FALSE)
#' jabot <- jabotR::jabot_records(herbarium = "MBM", taxon = "Ormosia", save = FALSE)
#'
#' combined_df <- barroso_cat(list_sources = list(GBIF = gbif_data,
#'                                                speciesLink = splink_data,
#'                                                REFLORA = reflora,
#'                                                JABOT = jabot),
#'                            keep_source = "GBIF")
#' }
#'
#' @importFrom dplyr bind_rows
#' @export

barroso_cat <- function(list_sources = list(), keep_source = NULL) {

  if (!is.list(list_sources) || is.data.frame(list_sources) || length(list_sources) < 2) {
    stop("Please provide a named list of at least two data sources.", call. = FALSE)
  }
  src_names <- names(list_sources)
  if (is.null(src_names) || any(is.na(src_names) | src_names == "") ||
      anyDuplicated(src_names)) {
    stop("All data sources in 'list_sources' must have unique names, e.g. ",
         "list(GBIF = gbif, speciesLink = splink).", call. = FALSE)
  }
  if (!is.null(keep_source) && !keep_source %in% src_names) {
    stop(paste0("'keep_source' must be one of: ", paste(src_names, collapse = ", "), "."),
         call. = FALSE)
  }

  # Format each source into Darwin Core column names and values
  list_sources <- lapply(list_sources, barroso_dwc)

  missing_code <- !vapply(list_sources, function(df) "collectionCode" %in% names(df), logical(1))
  if (any(missing_code)) {
    warning(paste0("No 'collectionCode' column in: ", paste(src_names[missing_code], collapse = ", "),
                   ". Overlapping herbaria cannot be detected for these sources."),
            call. = FALSE)
  }

  # Remove from the other sources the herbaria also present in keep_source
  if (!is.null(keep_source) && !missing_code[[keep_source]]) {
    kept_herbaria <- unique(stats::na.omit(list_sources[[keep_source]]$collectionCode))
    for (s in setdiff(src_names[!missing_code], keep_source)) {
      df <- list_sources[[s]]
      tf <- df$collectionCode %in% kept_herbaria
      if (any(tf)) {
        message(paste0("barroso_cat: removing ", sum(tf), " records of ",
                       length(unique(df$collectionCode[tf])), " herbaria from ", s,
                       " also present in ", keep_source))
        list_sources[[s]] <- df[!tf, , drop = FALSE]
      }
    }
  }

  # Columns with different types across sources are combined as character
  types <- lapply(list_sources, function(df) vapply(df, function(x) class(x)[1], character(1)))
  all_types <- split(unlist(types, use.names = FALSE),
                     unlist(lapply(types, names), use.names = FALSE))
  mixed <- names(all_types)[vapply(all_types, function(x) length(unique(x)) > 1, logical(1))]
  list_sources <- lapply(list_sources, function(df) {
    cols <- intersect(mixed, names(df))
    df[cols] <- lapply(df[cols], as.character)
    df
  })

  combined_df <- dplyr::bind_rows(list_sources, .id = "datasource")
  combined_df <- combined_df[, c(setdiff(names(combined_df), "datasource"), "datasource")]

  return(combined_df)
}
