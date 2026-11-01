#' Format Biodiversity Datasets into Darwin Core
#'
#' @author Domingos Cardoso
#'
#' @description
#' Formats a dataset of herbarium records into [Darwin Core](https://dwc.tdwg.org/terms/)
#' column names and value formats, so that records downloaded from different
#' sources, such as GBIF, speciesLink, REFLORA and JABOT, can be combined and
#' standardized together.
#'
#' @details
#' The source of the dataset is recognized from its columns and values, or can be
#' given in `source`. Then:
#'
#' - Columns named as Darwin Core terms in other cases, e.g. `recordedby` or
#'   `decimallatitude`, are renamed into the terms, e.g. `recordedBy` and
#'   `decimalLatitude`, for any source.
#' - speciesLink: the columns of the older speciesLink format are renamed into
#'   Darwin Core terms, e.g. `collector` into `recordedBy`, `collectornumber` into
#'   `recordNumber`, `latitude` into `decimalLatitude`, and `species` (just the
#'   epithet) into `specificEpithet`; `spp` (the binomial) becomes `species`, as in
#'   GBIF. The year, month and day of identification become `dateIdentified`; the
#'   author is added to `scientificName`; dates like `"23/11/2011"` become
#'   `"2011-11-23"`; and a missing `recordNumber` is taken from `fieldNumber`.
#' - REFLORA (as returned by refloraR): the determiner `"Administrador"` (the
#'   system user, given to most records) becomes `NA`; dates like `"11/10/2006"`
#'   become `"2006-10-11"`; type status in Latin like `"ISOTYPUS"` becomes
#'   `"isotype"`; and the institution of RB becomes `"JBRJ"`, as in GBIF and JABOT.
#' - JABOT (as returned by jabotR): dates like `"2008-9-26"` or `"22/4/2020"`
#'   become `"2008-09-26"` and `"2020-04-22"`.
#' - GBIF: downloads are already in Darwin Core, so only the column names are
#'   checked.
#'
#' In all sources, a binomial `species` column is created from `genus` and
#' `specificEpithet` when missing, as in GBIF downloads. Columns that are not
#' Darwin Core terms are kept unchanged.
#'
#' By default, all columns are returned as character. Downloads of the same source
#' may otherwise have different column types, e.g. `coordinatePrecision` as integer
#' in a species with values and as character in a species without them, as in
#' the speciesLink API used by `plantR::rspeciesLink()`, and then they cannot be
#' combined with `dplyr::bind_rows()`.
#'
#' [barroso_cat()] runs this function on each source before combining them.
#'
#' @usage
#' barroso_dwc(df,
#'             source = c("auto", "speciesLink", "REFLORA", "JABOT", "GBIF"),
#'             as_character = TRUE,
#'             verbose = TRUE)
#'
#' @param df A data frame of biodiversity records.
#' @param source The source of the records: `"auto"` (default) to recognize it from
#' the data, or one of `"speciesLink"`, `"REFLORA"`, `"JABOT"` or `"GBIF"`.
#' @param as_character Logical; if `TRUE` (default), all columns are returned as
#' character, so the outputs of different downloads can always be combined.
#' @param verbose Logical; if `TRUE`, reports the source and what was changed.
#'
#' @return The data frame with Darwin Core column names and value formats.
#'
#' @examples
#' splink <- data.frame(collector = "Queiroz, L. P.", collectornumber = "1350",
#'                      collectioncode = "HUEFS", genus = "Ormosia", species = "arborea",
#'                      scientificname = "Ormosia arborea",
#'                      scientificnameauthor = "(Vell.) Harms",
#'                      yearidentified = "2005", monthidentified = "3",
#'                      dayidentified = NA, latitude = "-12.97", longitude = "-38.5")
#' barroso_dwc(splink)
#'
#' @export

barroso_dwc <- function(df,
                        source = c("auto", "speciesLink", "REFLORA", "JABOT", "GBIF"),
                        as_character = TRUE,
                        verbose = TRUE) {

  if (!is.data.frame(df)) {
    stop("df must be a data frame.", call. = FALSE)
  }
  source <- match.arg(source)
  if (source == "auto") source <- .detect_source(df)

  changes <- character(0)

  # Source-specific columns, then Darwin Core terms written in other cases
  if (source == "speciesLink") {
    res <- .rename_cols(df, .splink_aliases(df))
    df <- res$df
    changes <- c(changes, res$changes)
  }
  res <- .rename_cols(df, .dwc_case_aliases(df))
  df <- res$df
  changes <- c(changes, res$changes)

  # Source-specific values
  if (source == "speciesLink") {
    res <- .splink_values(df)
    df <- res$df
    changes <- c(changes, res$changes)
  }
  if (source == "REFLORA") df <- .reflora_to_dwc(df)
  if (source == "JABOT") df <- .jabot_to_dwc(df)

  # Binomial species name, as in GBIF
  if (all(c("genus", "specificEpithet") %in% names(df)) && !"species" %in% names(df)) {
    df$species <- ifelse(is.na(df$genus) | is.na(df$specificEpithet) | df$specificEpithet == "",
                         NA, paste(df$genus, df$specificEpithet))
    changes <- c(changes, "created species from genus and specificEpithet")
  }

  # The same type for all columns, so different downloads can be combined
  if (as_character) {
    df[] <- lapply(df, function(x) if (is.atomic(x)) as.character(x) else x)
  }

  if (verbose) {
    message("barroso_dwc: ", source, " format",
            if (source == "REFLORA") ", values standardized into Darwin Core formats",
            if (source == "JABOT") ", values standardized into Darwin Core formats",
            if (length(changes) > 0) paste0("; ", paste(changes, collapse = "; ")))
  }

  return(df)
}


#_______________________________________________________________________________
# Recognize the source of a dataset from its columns and values ####
.detect_source <- function(df) {
  nms <- names(df)
  if (.is_reflora(df)) return("REFLORA")
  if (.is_jabot(df)) return("JABOT")
  splink_cols <- c("collector", "collectornumber", "yearcollected", "spp",
                   "scientificnameauthor", "yearidentified", "collectioncode")
  if (any(splink_cols %in% nms)) return("speciesLink")
  if (any(c("gbifID", "datasetKey") %in% nms)) return("GBIF")
  "unknown"
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


#_______________________________________________________________________________
# Column names ####

# Darwin Core terms of occurrences, plus "species" as in GBIF downloads
.dwc_terms <- c(
  "type", "modified", "language", "license", "rightsHolder", "references",
  "bibliographicCitation", "institutionID", "collectionID", "datasetID",
  "institutionCode", "collectionCode", "datasetName", "basisOfRecord",
  "informationWithheld", "dataGeneralizations", "dynamicProperties",
  "occurrenceID", "catalogNumber", "recordNumber", "recordedBy", "recordedByID",
  "individualCount", "sex", "lifeStage", "reproductiveCondition", "establishmentMeans",
  "preparations", "associatedMedia", "associatedReferences", "associatedSequences",
  "otherCatalogNumbers", "occurrenceRemarks", "fieldNumber", "eventDate", "eventTime",
  "year", "month", "day", "verbatimEventDate", "habitat", "samplingProtocol",
  "fieldNotes", "eventRemarks", "higherGeography", "continent", "waterBody",
  "islandGroup", "island", "country", "countryCode", "stateProvince", "county",
  "municipality", "locality", "verbatimLocality", "minimumElevationInMeters",
  "maximumElevationInMeters", "verbatimElevation", "minimumDepthInMeters",
  "maximumDepthInMeters", "locationRemarks", "decimalLatitude", "decimalLongitude",
  "geodeticDatum", "coordinateUncertaintyInMeters", "coordinatePrecision",
  "verbatimCoordinates", "verbatimLatitude", "verbatimLongitude",
  "verbatimCoordinateSystem", "georeferencedBy", "georeferencedDate",
  "georeferenceRemarks", "identifiedBy", "identifiedByID", "dateIdentified",
  "identificationQualifier", "identificationRemarks", "previousIdentifications",
  "typeStatus", "scientificName", "acceptedNameUsage", "higherClassification",
  "kingdom", "phylum", "class", "order", "family", "genus", "subgenus",
  "specificEpithet", "infraspecificEpithet", "taxonRank", "verbatimTaxonRank",
  "scientificNameAuthorship", "vernacularName", "taxonRemarks", "species")

# Columns named as Darwin Core terms in other cases, e.g. "decimallatitude"
.dwc_case_aliases <- function(df) {
  nms <- names(df)
  term <- .dwc_terms[match(tolower(nms), tolower(.dwc_terms))]
  tf <- !is.na(term) & nms != term
  stats::setNames(term[tf], nms[tf])
}

# Columns of the older speciesLink format
.splink_aliases <- function(df) {
  aliases <- c(collector = "recordedBy",
               collectornumber = "recordNumber",
               yearcollected = "year",
               monthcollected = "month",
               daycollected = "day",
               continentocean = "continent",
               latitude = "decimalLatitude",
               longitude = "decimalLongitude",
               ordem = "order",
               taxonclass = "class",
               subspecies = "infraspecificEpithet",
               scientificnameauthor = "scientificNameAuthorship",
               minimumelevation = "minimumElevationInMeters",
               maximumelevation = "maximumElevationInMeters",
               minimumdepth = "minimumDepthInMeters",
               maximumdepth = "maximumDepthInMeters",
               preparationtype = "preparations",
               datelastmodified = "modified",
               notes = "occurrenceRemarks",
               spp = "species")
  # In the older format, "species" is just the specific epithet
  if ("species" %in% names(df) && !any(tolower(names(df)) == "specificepithet")) {
    aliases <- c(aliases, species = "specificEpithet")
  }
  aliases[names(aliases) %in% names(df)]
}

# Rename columns, but never into a name already in the data
.rename_cols <- function(df, aliases) {
  aliases <- aliases[!aliases %in% names(df) & !duplicated(aliases)]
  if (length(aliases) == 0) return(list(df = df, changes = character(0)))
  idx <- match(names(aliases), names(df))
  names(df)[idx] <- aliases
  changes <- paste0("renamed ", length(aliases), " column", if (length(aliases) > 1) "s",
                    " (", paste(utils::head(paste(names(aliases), "->", aliases), 4),
                                collapse = ", "),
                    if (length(aliases) > 4) ", ...", ")")
  list(df = df, changes = changes)
}


#_______________________________________________________________________________
# Values of speciesLink records ####
.splink_values <- function(df) {
  changes <- character(0)

  # Specific epithet of the older format may come with other words, e.g.
  # "cf. platychila", "stygium var. annulatum", "sp. 2", "paniculata willd. ex vog."
  if ("specificEpithet" %in% names(df)) {
    tf <- !is.na(df$specificEpithet)
    ep <- .split_epithet(df$specificEpithet[tf])
    changed <- !mapply(identical, ep$epithet, trimws(df$specificEpithet[tf]))
    if (any(changed)) {
      df$specificEpithet[tf] <- ep$epithet
      for (col in c("identificationQualifier", "infraspecificEpithet")) {
        new <- ep[[col]]
        if (any(!is.na(new))) {
          if (!col %in% names(df)) df[[col]] <- NA_character_
          fill <- is.na(df[[col]][tf]) & !is.na(new)
          df[[col]][tf][fill] <- new[fill]
        }
      }
      changes <- c(changes, paste("cleaned", sum(changed), "specificEpithet (e.g. \"cf. platychila\", \"sp. 2\")"))
    }
  }

  # Date of identification from its year, month and day
  parts <- c("yearidentified", "monthidentified", "dayidentified")
  if (parts[1] %in% tolower(names(df))) {
    get <- function(p) {
      col <- names(df)[tolower(names(df)) == p]
      if (length(col) == 0) return(rep(NA_integer_, nrow(df)))
      suppressWarnings(as.integer(df[[col[1]]]))
    }
    y <- get(parts[1]); m <- get(parts[2]); d <- get(parts[3])
    ok_m <- !is.na(m) & m >= 1 & m <= 12
    ok_d <- ok_m & !is.na(d) & d >= 1 & d <= 31
    date <- ifelse(is.na(y), NA_character_,
                   ifelse(ok_d, sprintf("%04d-%02d-%02d", y, m, d),
                          ifelse(ok_m, sprintf("%04d-%02d", y, m), sprintf("%04d", y))))
    if (!"dateIdentified" %in% names(df)) {
      df$dateIdentified <- date
    } else {
      df$dateIdentified <- ifelse(is.na(df$dateIdentified), date, df$dateIdentified)
    }
    df <- df[, !tolower(names(df)) %in% parts, drop = FALSE]
    changes <- c(changes, "created dateIdentified from year, month and day of identification")
  }

  # Dates like "23/11/2011" into ISO 8601
  for (col in intersect(c("eventDate", "dateIdentified"), names(df))) {
    df[[col]] <- .iso_date(df[[col]])
  }

  # Author in the scientific name, as in Darwin Core
  if (all(c("scientificName", "scientificNameAuthorship") %in% names(df))) {
    author <- trimws(df$scientificNameAuthorship)
    tf <- !is.na(df$scientificName) & !is.na(author) & nzchar(author) &
      !mapply(function(n, a) grepl(a, n, fixed = TRUE), df$scientificName, author)
    if (any(tf)) {
      df$scientificName[tf] <- paste(df$scientificName[tf], author[tf])
      changes <- c(changes, paste("added the author to", sum(tf), "scientificName"))
    }
  }

  # Collector number recorded as field number
  if (all(c("recordNumber", "fieldNumber") %in% names(df))) {
    tf <- (is.na(df$recordNumber) | df$recordNumber %in% "") &
      !is.na(df$fieldNumber) & !df$fieldNumber %in% ""
    if (any(tf)) {
      df$recordNumber[tf] <- as.character(df$fieldNumber[tf])
      changes <- c(changes, paste("took", sum(tf), "missing recordNumber from fieldNumber"))
    }
  }

  list(df = df, changes = changes)
}


# Split epithets with other words into the epithet, the identification
# qualifier and the infraspecific epithet, e.g. "cf. platychila" into
# "platychila" and "cf.", "stygium var. annulatum" into "stygium" and
# "annulatum", and "sp. 2" or "fabaceae sp1" (morphospecies) into NA
.split_epithet <- function(x) {
  x <- trimws(gsub("[\"]", "", x))                 # "\"sincorense\""
  x <- sub("^\\([^)]*\\)\\s*", "", x)          # "(Spinulosae) mollis"
  x <- sub("^[x\u00d7]\\s+", "", x)                # hybrids, "x hybrida"
  # Qualifiers, also without space or misspelled, e.g. "cf.andarahiensis", "af. acuminata"
  qual <- "^(cf|aff?|gr)([.]\\s*|\\s+)(?=\\p{Ll})"
  qualifier <- ifelse(grepl(qual, x, perl = TRUE),
                      sub("^af$", "aff", sub("^(cf|aff?|gr).*", "\\1", x)), NA)
  qualifier[!is.na(qualifier)] <- paste0(qualifier[!is.na(qualifier)], ".")
  x <- sub(qual, "", x, perl = TRUE)
  infra <- ifelse(grepl("^\\S+\\s+(var|subsp|ssp|f)[.]\\s+\\S+", x),
                  sub("^\\S+\\s+(var|subsp|ssp|f)[.]\\s+(\\S+).*", "\\2", x), NA)
  epithet <- sub("[.]$", "", sub("\\s.*", "", x))  # "brasiliana."
  # Morphospecies and families are not epithets
  epithet[!grepl("^\\p{Ll}[\\p{Ll}-]+$", epithet, perl = TRUE) | grepl("aceae$", epithet) |
            epithet %in% c("sp", "spp", "indet")] <- NA
  list(epithet = epithet, identificationQualifier = qualifier, infraspecificEpithet = infra)
}


#_______________________________________________________________________________
# Values of REFLORA records, as returned by refloraR ####
# The columns are already Darwin Core terms, but some values are not: the
# determiner of most records is the name of the system user ("Administrador"),
# dates are written as "11/10/2006", and type status is in Latin ("ISOTYPUS")
.reflora_to_dwc <- function(df) {
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
  df
}

#_______________________________________________________________________________
# Values of JABOT records, as returned by jabotR ####
# The columns are already Darwin Core terms, but dates are not padded with
# zeros ("2008-9-26") and some dates of identification are written as "22/4/2020"
.jabot_to_dwc <- function(df) {
  for (col in intersect(c("eventDate", "dateIdentified"), names(df))) {
    df[[col]] <- .iso_date(df[[col]])
  }
  df
}


#_______________________________________________________________________________
# Dates into ISO 8601, e.g. "2/12/1940" into "1940-12-02", "--/5/1880" into
# "1880-05", "4/2020" into "2020-04", "2008-9-26" into "2008-09-26", and
# intervals "9/10/1990 at\u00e9 12/10/1990" into "1990-10-09/1990-10-12" ####
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
