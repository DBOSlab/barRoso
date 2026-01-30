#' Remove author names from scientific names
#'
#' This function removes author citations from scientific names, leaving only
#' the genus and species epithet (and optionally infraspecific ranks).
#'
#' @param scientific_names Character vector of scientific names with authors.
#' @param keep_infraspecific Logical, if TRUE, keeps infraspecific epithets
#'   (subspecies, varieties, forms). Default is TRUE.
#' @return Character vector of scientific names without authors.
#'
#' @examples
#' remove_authors("Quercus alba L.")
#' # Returns: "Quercus alba"
#'
#' remove_authors("Quercus rubra var. borealis (F.Michx.) Farw.")
#' # Returns: "Quercus rubra var. borealis" (if keep_infraspecific = TRUE)
#'
#' remove_authors(c("Acer saccharum Marshall", "Pinus strobus L."))
#' # Returns: c("Acer saccharum", "Pinus strobus")
#'
#' @export

remove_authorship <- function(scientific_names, keep_infraspecific = TRUE) {
  if (!is.character(scientific_names)) {
    stop("scientific_names must be a character vector")
  }

  # Pattern to match author citations
  # This matches authors that typically start with:
  # - Capital letter (single author like "L.")
  # - Parentheses with authors inside "(L.)"
  # - Multiple authors separated by "&", "et", "ex", etc.
  # - Authors with special characters like "×", "-", "'"

  # Common author abbreviations and patterns
  author_patterns <- c(
    # Single abbreviated authors (L., Mill., Lam., etc.)
    "\\s+[A-Z][a-z]*?\\.(?:\\s+[A-Z][a-z]*?\\.)*",
    # Authors in parentheses (L.) or (Linnaeus)
    "\\s+\\([^)]+\\)",
    # Multiple authors with "&", "et", "ex"
    "\\s+[A-Z].*?(?:\\s+(?:&|et|ex)\\s+[A-Z].*?)+",
    # Authors starting with capital letter, possibly with apostrophes, hyphens
    "\\s+[A-ZÀ-ÿ][A-Za-zÀ-ÿ'\\-]*?(?:\\s+[A-ZÀ-ÿ][A-Za-zÀ-ÿ'\\-]*?)*"
  )

  # Combine patterns
  full_pattern <- paste0("(", paste(author_patterns, collapse = "|"), ")$")

  # Remove authors
  clean_names <- gsub(full_pattern, "", scientific_names)

  # Trim whitespace
  clean_names <- trimws(clean_names)

  # If keep_infraspecific is FALSE, also remove infraspecific epithets
  if (!keep_infraspecific) {
    # Remove everything after species epithet
    # This pattern keeps genus and species, removes subsp., var., f., etc.
    clean_names <- gsub("\\s+(subsp\\.|var\\.|f\\.|ssp\\.|nothosubsp\\.).*$", "", clean_names)
  }

  return(clean_names)
}

