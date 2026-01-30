# Auxiliary functions
# Author: Domingos Cardoso


#_______________________________________________________________________________
# Create a named list of the input database ####
.namedherbsource <- function(...) {
  nms <- sapply(as.list(substitute(list(...))), deparse)[-1]
  setNames(list(...), nms)
}


#_______________________________________________________________________________
# Delete all records associated with just wood, seed or spirit collections  ####
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


