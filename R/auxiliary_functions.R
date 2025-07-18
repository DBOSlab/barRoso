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
