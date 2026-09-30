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
#' Raw speciesLink downloads, which use lowercase column names such as
#' `collector`, `collectornumber` and `collectioncode`, are automatically renamed
#' into Darwin Core terms (`recordedBy`, `recordNumber`, `collectionCode`, etc.).
#' Records of the REFLORA Virtual Herbarium and of the JABOT collections, as
#' returned by the packages [refloraR](https://dboslab.github.io/refloraR-website/)
#' and [jabotR](https://dboslab.github.io/jabotR-website/), already have Darwin
#' Core column names, but some of their values are standardized to match the
#' other sources:
#' - REFLORA: the determiner `"Administrador"` (the name of the system user, given
#'   to most records) is set to `NA`; dates like `"11/10/2006"` or `"--/5/1880"`
#'   become `"2006-10-11"` and `"1880-05"`; type status in Latin like `"ISOTYPUS"`
#'   becomes `"isotype"`; and the institution of RB becomes `"JBRJ"`, as in GBIF
#'   and JABOT.
#' - JABOT: dates like `"2008-9-26"` or `"22/4/2020"` become `"2008-09-26"` and
#'   `"2020-04-22"`.
#'
#' The sources are recognized by their content, so their names in `list_sources`
#' can be any name.
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

  # Harmonize each source with Darwin Core: rename the columns of raw speciesLink
  # downloads, and standardize the values of REFLORA and JABOT records
  list_sources <- lapply(list_sources, function(df) {
    .jabot_to_dwc(.reflora_to_dwc(.splink_to_dwc(df)))
  })

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


#_______________________________________________________________________________
# Rename columns of raw speciesLink downloads into Darwin Core terms ####
.splink_to_dwc <- function(df) {

  # Only for the raw speciesLink format, with all lowercase column names
  if (!all(c("collector", "collectioncode") %in% names(df))) return(df)

  dwc <- c(institutioncode = "institutionCode",
           collectioncode = "collectionCode",
           catalognumber = "catalogNumber",
           scientificname = "scientificName",
           basisofrecord = "basisOfRecord",
           ordem = "order",
           species = "specificEpithet",
           subspecies = "infraspecificEpithet",
           scientificnameauthor = "scientificNameAuthorship",
           identifiedby = "identifiedBy",
           yearidentified = "yearIdentified",
           monthidentified = "monthIdentified",
           dayidentified = "dayIdentified",
           typestatus = "typeStatus",
           collectornumber = "recordNumber",
           fieldnumber = "fieldNumber",
           collector = "recordedBy",
           yearcollected = "year",
           monthcollected = "month",
           daycollected = "day",
           continentocean = "continent",
           stateprovince = "stateProvince",
           longitude = "decimalLongitude",
           latitude = "decimalLatitude",
           coordinateprecision = "coordinatePrecision",
           minimumelevation = "minimumElevationInMeters",
           maximumelevation = "maximumElevationInMeters",
           individualcount = "individualCount",
           previouscatalognumber = "previousCatalogNumber",
           notes = "occurrenceRemarks")

  tf <- names(df) %in% names(dwc)
  names(df)[tf] <- dwc[names(df)[tf]]

  # In speciesLink, the column "species" is just the specific epithet, while in
  # GBIF it is the binomial; so we create the binomial for combining them
  if (all(c("genus", "specificEpithet") %in% names(df)) && !"species" %in% names(df)) {
    df$species <- ifelse(is.na(df$specificEpithet) | df$specificEpithet == "",
                         NA, paste(df$genus, df$specificEpithet))
  }

  message("barroso_cat: speciesLink columns renamed into Darwin Core terms")

  return(df)
}


#_______________________________________________________________________________
# Standardize values of REFLORA records, as returned by refloraR ####
# The columns are already Darwin Core terms, but some values are not: the
# determiner of most records is the name of the system user ("Administrador"),
# dates are written as "11/10/2006", and type status is in Latin ("ISOTYPUS")
.reflora_to_dwc <- function(df) {

  if (!.is_reflora(df)) return(df)

  if ("identifiedBy" %in% names(df)) {
    df$identifiedBy[df$identifiedBy %in% "Administrador"] <- NA
  }
  for (col in intersect(c("eventDate", "dateIdentified"), names(df))) {
    df[[col]] <- .iso_date(df[[col]])
  }
  if ("typeStatus" %in% names(df)) {
    df$typeStatus <- .latin_type_status(df$typeStatus)
  }
  # RB, as in GBIF and JABOT
  if (all(c("collectionCode", "institutionCode") %in% names(df))) {
    df$institutionCode[df$collectionCode %in% "RB" & df$institutionCode %in% "RB"] <- "JBRJ"
  }

  message("barroso_cat: REFLORA values standardized into Darwin Core formats")

  return(df)
}

#_______________________________________________________________________________
# Standardize values of JABOT records, as returned by jabotR ####
# The columns are already Darwin Core terms, but dates are not padded with
# zeros ("2008-9-26") and some dates of identification are written as "22/4/2020"
.jabot_to_dwc <- function(df) {

  if (!.is_jabot(df)) return(df)

  for (col in intersect(c("eventDate", "dateIdentified"), names(df))) {
    df[[col]] <- .iso_date(df[[col]])
  }

  message("barroso_cat: JABOT values standardized into Darwin Core formats")

  return(df)
}

# REFLORA records cite the REFLORA Virtual Herbarium, and their images
.is_reflora <- function(df) {
  "bibliographicCitation" %in% names(df) &&
    any(grepl("Reflora", df$bibliographicCitation, ignore.case = TRUE))
}

# JABOT records have occurrenceID as "urn:catalog:RB:123", and taxonName
.is_jabot <- function(df) {
  all(c("occurrenceID", "taxonName") %in% names(df)) &&
    any(grepl("^urn:catalog:", df$occurrenceID)) && !.is_reflora(df)
}

# Dates into ISO 8601, e.g. "2/12/1940" into "1940-12-02", "--/5/1880" into
# "1880-05", "4/2020" into "2020-04", "2008-9-26" into "2008-09-26", and
# intervals "9/10/1990 at\u00e9 12/10/1990" into "1990-10-09/1990-10-12"
.iso_date <- function(x) {
  x <- trimws(as.character(x))
  out <- x

  interval <- grepl("\\s+at\u00e9\\s+", x)
  if (any(interval)) {
    p <- strsplit(x[interval], "\\s+at\u00e9\\s+")
    out[interval] <- vapply(p, function(v) paste(.iso_date(v), collapse = "/"), character(1))
  }

  # Month/year
  my <- grepl("^[0-9]{1,2}/[0-9]{4}$", x)
  if (any(my)) {
    p <- do.call(rbind, strsplit(x[my], "/"))
    out[my] <- paste(p[, 2], sprintf("%02d", as.integer(p[, 1])), sep = "-")
  }
  pad <- function(v) ifelse(is.na(v), NA, sprintf("%02d", suppressWarnings(as.integer(v))))
  build <- function(y, m, d) {
    ifelse(is.na(m), y, ifelse(is.na(d), paste(y, m, sep = "-"), paste(y, m, d, sep = "-")))
  }

  # Day/month/year, with "--" for unknown parts
  dmy <- grepl("^(--|[0-9]{1,2})/(--|[0-9]{1,2})/[0-9]{4}$", x)
  if (any(dmy)) {
    p <- do.call(rbind, strsplit(x[dmy], "/"))
    p[p == "--"] <- NA
    out[dmy] <- build(p[, 3], pad(p[, 2]), pad(p[, 1]))
  }

  # Year-month-day, not padded with zeros
  ymd <- grepl("^[0-9]{4}(-[0-9]{1,2}){0,2}$", x)
  if (any(ymd)) {
    p <- strsplit(x[ymd], "-")
    out[ymd] <- vapply(p, function(v) build(v[1], pad(v[2]), pad(v[3])), character(1))
  }

  out
}

# Type status in Latin into Darwin Core terms, e.g. "HOLOTYPUS" into "holotype"
.latin_type_status <- function(x) {
  tf <- grepl("TYPUS$", x, ignore.case = TRUE)
  x[tf] <- sub("typus$", "type", tolower(x[tf]))
  x
}
