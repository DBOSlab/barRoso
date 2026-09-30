#' Flag or Remove Duplicate Specimens
#'
#' @author Domingos Cardoso
#'
#' @description
#' Identifies and optionally removes duplicate herbarium specimen records based on
#' collector name and number, or—when the number is missing—by species name, collector
#' and date. Duplicates are grouped into blocks, and blocks with conflicting
#' identifications are highlighted. The herbaria and catalog numbers of all
#' duplicates are stored together, so no information is lost when duplicates are
#' removed.
#'
#' @details
#' This function is part of the internal workflow of the `barRoso` package, supporting
#' record reconciliation and dataset cleaning. Duplicates are records with the same
#' collector (`recordedBy`) and collector number (`recordNumber`). For records
#' without collector number, duplicates must share the species name, the collector
#' and the full collection date (`year`, `month`, `day`). Records with unknown
#' collector (`NA` or `"Unknown"`) are never treated as duplicates.
#'
#' The following columns are added after the collector number:
#' - `duplicate`: `TRUE` for records belonging to a block of duplicates.
#' - `duplicateGroup`: an identifier of each block of duplicates.
#' - `identificationConflict`: `TRUE` when the duplicates of a block are identified
#'   as different species (e.g. `"Ormosia arborea"` and `"Ormosia fastigiata"`) or
#'   genera. A duplicate identified just to genus (e.g. `"Ormosia"`) does not conflict
#'   with a complete name of the same genus. Names are compared exactly, so spelling
#'   variants (e.g. `"Ormosia trifoliata"` and `"Ormosia trifoliolata"`) are also
#'   flagged as conflicts, which helps finding misspelled names.
#' - `duplicateIdentifications`: all names found in the block, e.g.
#'   `"Ormosia arborea | Ormosia fastigiata"`.
#' - `duplicateCollectionCodes`: all herbaria of the block, e.g. `"MO | JBB"`.
#' - `duplicateCatalogNumbers`: all catalog numbers of the block with their herbarium,
#'   e.g. `"MO 2839102 | JBB 13548"`.
#'
#' The specific epithet is taken from `specificEpithet` or, when missing, from a
#' binomial in the column `species` (as in GBIF downloads).
#'
#' When `rm_duplicates = TRUE`, one record per block is kept, chosen as follows:
#' 1. Records identified to species are preferred over those identified just to genus.
#' 2. Among them, records with the most frequent identification in the block are
#'    preferred (e.g. the name given by two duplicates rather than by one). When
#'    names are equally frequent, all of them go to the next rule.
#' 3. Among them, records with latitude and longitude, and then records with more
#'    filled columns, are preferred. If records are still tied, the first one in
#'    the data is kept.
#'
#' The kept record still has `identificationConflict` and all the names, herbaria and
#' catalog numbers of its block, so conflicting identifications remain visible
#' after removing the duplicates.
#'
#' @usage
#' barroso_flag_duplicates(df,
#'                         rm_duplicates = FALSE,
#'                         colname_recordedBy = "recordedBy",
#'                         colname_recordNumber = "recordNumber",
#'                         colname_genus = "genus",
#'                         colname_specificEpithet = "specificEpithet",
#'                         colname_collectionCode = "collectionCode",
#'                         colname_catalogNumber = "catalogNumber",
#'                         colname_decimalLatitude = "decimalLatitude",
#'                         colname_decimalLongitude = "decimalLongitude")
#'
#' @param df A data frame with biodiversity specimen records.
#' @param rm_duplicates Logical; if `TRUE`, keeps just one record per block of
#' duplicates (default: `FALSE`).
#' @param colname_recordedBy Column name for the collector (default: `"recordedBy"`).
#' @param colname_recordNumber Column name for the collector number (default: `"recordNumber"`).
#' @param colname_genus Column name for the genus (default: `"genus"`).
#' @param colname_specificEpithet Column name for the specific epithet (default: `"specificEpithet"`).
#' @param colname_collectionCode Column name for the herbarium acronym (default: `"collectionCode"`).
#' @param colname_catalogNumber Column name for the catalog number (default: `"catalogNumber"`).
#' @param colname_decimalLatitude Column name for latitude (default: `"decimalLatitude"`).
#' @param colname_decimalLongitude Column name for longitude (default: `"decimalLongitude"`).
#'
#' @return A data frame with the duplicate columns described above. If
#' `rm_duplicates = TRUE`, only one record per block of duplicates is kept.
#'
#' @examples
#' \dontrun{
#' df <- read.csv("herbarium_data.csv")
#' df_flagged <- barroso_flag_duplicates(df)
#'
#' # Blocks of duplicates with conflicting identifications
#' df_flagged[df_flagged$identificationConflict %in% TRUE, ]
#'
#' df_clean <- barroso_flag_duplicates(df, rm_duplicates = TRUE)
#' }
#'
#' @importFrom tibble add_column
#'
#' @export

barroso_flag_duplicates <- function(df,
                                    rm_duplicates = FALSE,
                                    colname_recordedBy = "recordedBy",
                                    colname_recordNumber = "recordNumber",
                                    colname_genus = "genus",
                                    colname_specificEpithet = "specificEpithet",
                                    colname_collectionCode = "collectionCode",
                                    colname_catalogNumber = "catalogNumber",
                                    colname_decimalLatitude = "decimalLatitude",
                                    colname_decimalLongitude = "decimalLongitude") {

  missing <- setdiff(c(colname_recordedBy, colname_recordNumber), names(df))
  if (length(missing) > 0) {
    stop(paste0("Column(s) not found in the data frame: ", paste(missing, collapse = ", ")),
         call. = FALSE)
  }

  # Remove the columns of a previous run
  new_cols <- c("duplicate", "duplicateGroup", "identificationConflict",
                "duplicateIdentifications", "duplicateCollectionCodes",
                "duplicateCatalogNumbers")
  df <- df[, !names(df) %in% new_cols, drop = FALSE]

  n <- nrow(df)
  coll <- .col_or_na(df, colname_recordedBy)
  num <- .col_or_na(df, colname_recordNumber)
  genus <- .col_or_na(df, colname_genus)
  epithet <- .epithet(df, genus, colname_specificEpithet)
  species <- ifelse(is.na(genus), NA, ifelse(is.na(epithet), genus, paste(genus, epithet)))

  #_____________________________________________________________________________
  # Blocks of duplicates ####
  # Same collector and number, or same species, collector and date when the
  # number is missing
  known_coll <- !is.na(coll) & !coll %in% "Unknown"
  key <- rep(NA_character_, n)
  tf <- known_coll & !is.na(num)
  key[tf] <- paste("number", coll[tf], num[tf], sep = "\r")
  if (all(c("year", "month", "day") %in% names(df))) {
    y <- .col_or_na(df, "year")
    m <- .col_or_na(df, "month")
    d <- .col_or_na(df, "day")
    tf <- known_coll & is.na(num) & !is.na(species) & !is.na(y) & !is.na(m) & !is.na(d)
    key[tf] <- paste("date", species[tf], coll[tf], y[tf], m[tf], d[tf], sep = "\r")
  }

  size <- as.integer(table(key)[key])
  duplicate <- !is.na(size) & size > 1
  group <- rep(NA_integer_, n)
  group[duplicate] <- as.integer(factor(key[duplicate], levels = unique(key[duplicate])))

  #_____________________________________________________________________________
  # Identifications, herbaria and catalog numbers of each block ####
  # Singletons are their own block
  block <- ifelse(duplicate, paste0("g", group), paste0("r", seq_len(n)))
  rows <- split(seq_len(n), factor(block, levels = unique(block)))

  code <- .col_or_na(df, colname_collectionCode)
  catalog <- .col_or_na(df, colname_catalogNumber)
  code_catalog <- ifelse(is.na(catalog), code,
                         ifelse(is.na(code) | startsWith(catalog, code), catalog,
                                paste(code, catalog)))

  collapse <- function(x) {
    x <- unique(x[!is.na(x)])
    if (length(x) == 0) NA_character_ else paste(x, collapse = " | ")
  }
  info <- lapply(rows, function(i) {
    complete <- unique(species[i][!is.na(epithet[i])])
    conflict <- length(complete) > 1 || length(unique(stats::na.omit(genus[i]))) > 1
    c(conflict, collapse(species[i]), collapse(code[i]), collapse(code_catalog[i]))
  })
  info <- do.call(rbind, info)[match(block, names(rows)), , drop = FALSE]

  conflict <- ifelse(duplicate, info[, 1] == "TRUE", NA)
  identifications <- ifelse(duplicate, info[, 2], NA_character_)

  df <- tibble::add_column(df,
                           duplicate = duplicate,
                           duplicateGroup = group,
                           identificationConflict = conflict,
                           duplicateIdentifications = identifications,
                           duplicateCollectionCodes = info[, 3],
                           duplicateCatalogNumbers = info[, 4],
                           .after = colname_recordNumber)

  message(paste0("barroso_flag_duplicates: ", sum(duplicate), " records in ",
                 length(unique(stats::na.omit(group))), " blocks of duplicates, ",
                 length(unique(group[conflict %in% TRUE])),
                 " blocks with conflicting identifications"))

  #_____________________________________________________________________________
  # Keep one record per block of duplicates ####
  if (rm_duplicates) {
    has_coords <- !is.na(.col_or_na(df, colname_decimalLatitude)) &
      !is.na(.col_or_na(df, colname_decimalLongitude))
    filled <- rowSums(!is.na(df) & as.matrix(df) != "", na.rm = TRUE)

    keep <- vapply(rows[startsWith(names(rows), "g")], function(i) {
      # 1. Identified to species rather than just to genus
      if (any(!is.na(epithet[i]))) i <- i[!is.na(epithet[i])]
      # 2. The most frequent identification in the block
      if (any(!is.na(species[i]))) {
        freq <- table(species[i])
        i <- i[species[i] %in% names(freq)[freq == max(freq)]]
      }
      # 3. With coordinates, then with more filled columns
      i[order(-has_coords[i], -filled[i])][1]
    }, integer(1))

    tf <- !duplicate | seq_len(n) %in% keep
    message(paste0("barroso_flag_duplicates: ", sum(!tf), " duplicates removed"))
    df <- df[tf, , drop = FALSE]
  }

  df <- df[order(df[[colname_recordedBy]], df[[colname_recordNumber]]), , drop = FALSE]

  return(df)
}


#_______________________________________________________________________________
# A column as character with "" as NA, or all NA when the column is missing ####
.col_or_na <- function(df, col) {
  if (!col %in% names(df)) return(rep(NA_character_, nrow(df)))
  x <- trimws(as.character(df[[col]]))
  x[x %in% c("", "NA")] <- NA
  x
}

# Specific epithet from specificEpithet or, when missing, from a binomial in
# the column species, as in GBIF downloads ####
.epithet <- function(df, genus, colname_specificEpithet) {
  epithet <- .col_or_na(df, colname_specificEpithet)
  species <- .col_or_na(df, "species")
  tf <- is.na(epithet) & !is.na(genus) & !is.na(species) &
    startsWith(species, paste0(genus, " "))
  from_species <- sub("^\\S+\\s+(\\S+).*", "\\1", species[tf])
  # Not placeholders like "sp.", "cf.", "aff."
  from_species[!grepl("^[[:lower:]-]+$", from_species) |
                 from_species %in% c("sp", "spp", "cf", "aff", "indet")] <- NA
  epithet[tf] <- from_species
  epithet
}
