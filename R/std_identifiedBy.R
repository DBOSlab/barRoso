#' Standardize Determiner Names in Biodiversity Records
#'
#' @author Domingos Cardoso
#'
#' @description
#' Cleans and standardizes the `identifiedBy` field in biodiversity collection
#' data, using the same name-cleaning rules of [std_recordedBy()]. Each
#' determiner name is formatted with abbreviated initials before the surname
#' (e.g. `"Cardoso, D.B.O.S."` into `"D. B. O. S. Cardoso"`).
#'
#' @details
#' Unlike [std_recordedBy()], no additional column is created when there are
#' several determiners. All names are kept in the same cell, each one
#' standardized and joined as `"D. Cardoso & L. P. Queiroz"` or
#' `"A. Pott, V. J. Pott & D. Cardoso"`.
#'
#' Before standardizing the names, the function removes information that often
#' comes along with the determiner in the `identifiedBy` field, such as the
#' prefix `"det."`, herbarium acronyms in parentheses (e.g. `"(MO)"`), and
#' determination dates (e.g. `"/VI-2011"`, `", 2006"`, `"18 Jun 1996"`).
#' Missing or anonymous determiners are returned as `NA`.
#'
#' @usage
#' std_identifiedBy(df = NULL,
#'                  colname_identifiedBy = "identifiedBy",
#'                  rm_original_column = FALSE)
#'
#' @param df A data frame containing biodiversity records.
#' @param colname_identifiedBy Column name for the determiner (default: `"identifiedBy"`).
#' @param rm_original_column Logical; if `TRUE`, the original column is removed after
#' cleaning. If `FALSE`, it is retained with the `*Original` suffix (default: `FALSE`).
#'
#' @return A data frame with the standardized determiner names.
#'
#' @examples
#' \dontrun{
#' df <- read.csv("herbarium_records.csv")
#' df_clean <- std_identifiedBy(df,
#'                              colname_identifiedBy = "identifiedBy",
#'                              rm_original_column = FALSE)
#' }
#'
#' @importFrom tibble add_column
#'
#' @export

std_identifiedBy <- function(df = NULL,
                             colname_identifiedBy = "identifiedBy",
                             rm_original_column = FALSE) {

  if (!colname_identifiedBy %in% names(df)) {
    stop(paste0("Column '", colname_identifiedBy, "' not found in the data frame."),
         call. = FALSE)
  }

  message("std_identifiedBy $", colname_identifiedBy)

  x <- as.character(df[[colname_identifiedBy]])

  if (!rm_original_column) {
    df <- tibble::add_column(df, !!!stats::setNames(list(x), paste0(colname_identifiedBy, "Original")),
                             .before = colname_identifiedBy)
  }

  df[[colname_identifiedBy]] <- .std_determiners(x)

  return(df)
}


#_______________________________________________________________________________
# Standardize a vector of determiners, keeping all names in the same element ####
.std_determiners <- function(x) {

  x <- .predetclean(x)

  # Split each element into single names
  names_list <- strsplit(x, "\\s*(;|&|[|]|/|\\sand\\s|\\sy\\s|\\set\\s(?!al))\\s*", perl = TRUE)
  names_list <- lapply(names_list, .predetname)

  # Standardize all unique names at once with the rules of std_recordedBy
  all_names <- unique(unlist(names_list))
  all_names <- all_names[!is.na(all_names) & !grepl("^et al[.]$", all_names)]
  if (length(all_names) == 0) return(rep(NA_character_, length(x)))
  std <- suppressMessages(
    std_recordedBy(data.frame(recordedBy = all_names, recordNumber = NA_character_),
                   rm_original_column = TRUE))
  std_names <- ifelse(is.na(std$addCollector) | std$addCollector == "et al.",
                      std$recordedBy,
                      paste(std$recordedBy, std$addCollector, sep = "; "))
  names(std_names) <- all_names

  # Put the standardized names back together
  # Keep acronyms of determiners as they are, e.g. "JAGG", "MRG"
  acronym <- grepl("^[[:upper:]]{2,4}$", all_names)
  std_names[acronym] <- all_names[acronym]

  vapply(names_list, function(n) {
    etal <- any(n %in% "et al.")
    n <- n[!is.na(n) & !n %in% "et al."]
    n <- unique(unlist(strsplit(std_names[n], "; ")))
    n <- n[!n %in% "Unknown"]
    if (length(n) == 0) return(NA_character_)
    if (length(n) > 1) {
      n <- paste(paste(n[-length(n)], collapse = ", "), n[length(n)], sep = " & ")
    }
    if (etal) n <- paste(n, "et al.")
    n
  }, character(1), USE.NAMES = FALSE)
}

# Remove prefixes, herbarium acronyms and dates of determination ####
.predetclean <- function(x) {

  x <- .decode_html(x)

  .gsub_all(x, c(
    # Dates after a slash, e.g. "D.Cardoso/4-2009", "H.C. de Lima/VI-2011"
    "/[^/;&|]*[0-9][^/;&|]*" = "",
    # Herbarium acronyms, e.g. "R. Barneby (NY)", "Strong, Mark T., (US), NMNH"
    "\\s*[(][^)]*[)]" = "",
    ",\\s*,.*$" = "",
    # Prefixes like "det. J.Andre", "Det: D. Cardoso", "fide Cardoso"
    "(^|\\s)[Dd]et[.:]+\\s*" = "\\1",
    "^[Ff]ide\\s+" = "",
    # Placeholders like "McDonal Nulo, Nulo"
    "\\s*\\bNulo\\b" = ""))
}

# Clean a vector of single determiner names ####
.predetname <- function(n) {

  n <- trimws(n)
  latin <- !grepl("<U[+]", n)

  # Dates of determination, e.g. "R. Barneby 1987", "M. Véliz, 3 Nov",
  # "J.Ickert-Bond 18 Jun 1996", "David Keil   Oct 1968"
  n[latin] <- sub(",?\\s*[0-9].*$", "", n[latin])
  months <- paste0("(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|June?|July?|Aug(ust)?|",
                   "Sep(t|tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?)[.]?$")
  n[latin] <- sub(paste0("([^,])\\s+", months), "\\1", n[latin])
  n <- sub(",?\\s*$", "", n)
  n[grepl("^et al", n)] <- "et al."

  # Anonymous or missing determiners
  unknown <- c("^$", "^[[:alpha:]][.]?$", "non pr[\u00e9e]cis[\u00e9e]", "[Aa]n[\u00f3\u00f4o]nimo",
               "^Anon", "[Ii]nc[\u00f3o]gnito", "[Hh]erbarium", "[Hh]erbario", "[Uu]nknown",
               "[Ii]ndet", "NO DISPONIBLE", "^Administrador$", "[Ss]yn[oe]n[oy]my", "ID Flag")
  n[grepl(paste(unknown, collapse = "|"), n)] <- NA

  n
}
