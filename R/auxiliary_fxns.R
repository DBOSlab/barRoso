# Auxiliary functions
# Author: Domingos Cardoso


#_______________________________________________________________________________
# Create a named list of the input database ####
#' @keywords internal
#' @noRd
.namedherbsource <- function(...) {
  nms <- sapply(as.list(substitute(list(...))), deparse)[-1]
  setNames(list(...), nms)
}


#_______________________________________________________________________________
# Delete all records associated with just wood, seed or spirit collections  ####
#' @keywords internal
#' @noRd
.delunvouchered <- function(df,
                            colname_collectionCode) {

  temp <- c("BCTW",
            "INPAW",
            "SPFW",
            "BOTUW",
            "BCTW",
            "HDJFW",
            "HTSAW",
            "HVASFW",
            "JOIW",
            "HTSA-CARPOTECA",
            "UFP-CARPOTECA",
            "INPA-CARPOTECA",
            "Seeds",
            "Economic Botany Collection",
            "Spirit Collection")

  if (any(df[[colname_collectionCode]] %in% temp)) {
    tf <- which(df[[colname_collectionCode]] %in% temp)
    df <- df[-tf,]
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
