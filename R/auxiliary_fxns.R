# Auxiliary functions
# Author: Domingos Cardoso


#_______________________________________________________________________________
# Create a named list of the input database ####
#' @keywords internal
#' @noRd
.namedherbsource <- function(...) {
  nms <- sapply(as.list(substitute(list(...))), deparse)[-1]
  stats::setNames(list(...), nms)
}


#_______________________________________________________________________________
# Delete all records of wood, seed, fruit, pollen or spirit collections, and ####
# observations without a specimen
#' @keywords internal
#' @noRd
.delunvouchered <- function(df,
                            colname_collectionCode) {

  # Wood collections like "BCTw", "INPAw", "MADW", carpological collections like
  # "INPA-Carpoteca", "BHZBcarpo", "MFS-FRUTOS", and other unvouchered collections
  # Also the original codes, e.g. "INPA-Carpoteca" standardized into "INPA" by
  # barroso_cat()
  code <- trimws(as.character(df[[colname_collectionCode]]))
  original <- paste0(colname_collectionCode, "Original")
  if (original %in% names(df)) {
    code <- paste(code, trimws(as.character(df[[original]])))
  }
  temp <- paste0("(^|\\s)(BCT|INPA|SPF|BOTU|HDJF|HTSA|HVASF|JOI|MEXU|FCAB|MAD|F)w(\\s|$)|",
                 "carpo|-FRUTOS(\\s|$)|(^|\\s)Seeds?(\\s|$)|(^|\\s)Economic Botany|",
                 "(^|\\s)Spirit|POLLEN")
  tf <- grepl(temp, code, ignore.case = TRUE)

  # Records of observations, e.g. "HUMAN_OBSERVATION", "MachineObservation"
  if ("basisOfRecord" %in% names(df)) {
    tf <- tf | grepl("observation", df$basisOfRecord, ignore.case = TRUE)
  }

  if (any(tf)) {
    df <- df[!tf, , drop = FALSE]
  }

  return(df)
}

#' @keywords internal
#' @noRd
.upper_first_only <- function(x) {
  if (!is.character(x)) {
    x <- as.character(x)
  }

  sapply(x, function(str) {
    if (is.na(str) || !nzchar(str)) {
      return(str)
    }

    # Convert first character to uppercase, rest to lowercase
    first_char <- substr(str, 1, 1)
    rest <- substr(str, 2, nchar(str))

    paste0(toupper(first_char), tolower(rest))
  }, USE.NAMES = FALSE)
}

#' @keywords internal
#' @noRd
.resolve_cols <- function(df, cols, arg) {
  if (is.null(cols) || length(cols) == 0L) stop(arg, " must be non-empty.", call. = FALSE)
  nms <- names(df)
  if (is.numeric(cols)) {
    idx <- as.integer(cols)
    if (anyNA(idx) || any(idx < 1L) || any(idx > length(nms))) {
      stop(arg, " indices out of range 1..", length(nms), ".", call. = FALSE)
    }
    return(nms[idx])
  }
  if (is.character(cols)) {
    missing <- setdiff(cols, nms)
    if (length(missing) > 0L) {
      stop("Missing columns for ", arg, ": ", paste(missing, collapse = ", "),
           call. = FALSE)
    }
    return(cols)
  }
  stop(arg, " must be a character vector of names or numeric indices.", call. = FALSE)
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

# Without accents, extra spaces, empty cells and the text "NA" of spreadsheets
# exported from R (the code "NA" is not an herbarium)
.ascii_squish <- function(x) {
  x <- stringi::stri_trans_general(as.character(x), "Latin-ASCII")
  x <- trimws(gsub("\\s+", " ", x))
  x[x %in% c("", "NA", "na")] <- NA
  x
}

# Side function make first letter to upper case ####
.firstUp <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
