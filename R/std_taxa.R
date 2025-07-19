#' Standardize Taxonomic Columns in Biodiversity Records
#'
#' @author Domingos Cardoso
#'
#' @description
#' Cleans and standardizes taxonomic fields in a biodiversity collection dataset.
#' Specifically targets and harmonizes the `family`, `genus`, and `specificEpithet` columns,
#' correcting legacy naming (e.g. Leguminosae → Fabaceae), removing ambiguous entries,
#' and formatting genus/species names for consistency.
#'
#' @details
#' This function is part of the `barRoso` package and is designed to improve the
#' quality of taxon names for reconciliation, querying, and label generation.
#' It removes common taxonomic noise such as uncertain identifiers (e.g. “cf.”,
#' “aff.”, “indet.”), numeric placeholders, and genus-only labels mistakenly
#' stored in the species field. Genus names are capitalized, and legacy family
#' names (like `Leguminosae`) are standardized to their accepted equivalents
#' (e.g. `Fabaceae`).
#'
#' @usage
#' std_taxa(df = NULL,
#'          colname_family = "family",
#'          colname_genus = "genus",
#'          colname_specificEpithet = "specificEpithet",
#'          rm_original_column = TRUE)
#'
#' @param df A data frame with biodiversity collection records.
#' @param colname_family Name of the column containing plant family names (default: `"family"`).
#' @param colname_genus Name of the column containing genus names (default: `"genus"`).
#' @param colname_specificEpithet Name of the column containing specific epithet of the species names (default: `"specificEpithet"`).
#' @param rm_original_column Logical; if `TRUE`, original columns are removed after
#' cleaning (default: `TRUE`).
#'
#' @return A data frame with cleaned and standardized `family`, `genus`, and `specificEpithet` columns.
#' If `rm_original_column = FALSE`, original values are retained with a `*Original` suffix.
#'
#' @examples
#' \dontrun{
#' df <- read.csv("taxa.csv")
#' df_clean <- std_taxa(df,
#'                      colname_family = "familia",
#'                      colname_genus = "genero",
#'                      colname_specificEpithet = "especie",
#'                      rm_original_column = FALSE)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom tibble add_column
#'
#' @export

std_taxa <- function(df = NULL,
                     colname_family = "family",
                     colname_genus = "genus",
                     colname_specificEpithet = "specificEpithet",
                     rm_original_column = TRUE) {

  # Adjust colnames in the input dataset ####
  colnames_df <- names(df)
  if (colname_family != "family") {
    names(df)[colnames_df %in% colname_family] <- "family"
  }
  if (colname_genus != "genus") {
    names(df)[colnames_df %in% colname_genus] <- "genus"
  }
  if (colname_specificEpithet != "specificEpithet" ) {
    names(df)[colnames_df %in% colname_specificEpithet] <- "specificEpithet"
  }

  if ("family" %in% names(df) &&
      "genus" %in% names(df) &&
      "specificEpithet" %in% names(df)) {

    message("std_taxa $family $genus $specificEpithet")

    # Remove original typeStatus column ####
    if (rm_original_column == FALSE) {
      df <- df %>%
        tibble::add_column(familyOriginal = df$family,
                           .before = "family") %>%
        tibble::add_column(genusOriginal = df$genus,
                           .before = "genus") %>%
        tibble::add_column(specificEpithetOriginal = df$specificEpithet,
                           .before = "specificEpithet")
    } else {
      message(paste0("Original uncleaned '", colname_family, "', ", colname_genus, "' and '", colname_specificEpithet, "' columns removed"))
    }

    del <- c("Leguminosae|leguminosae|LEGUMINOSAE|fabaceae|FABACEAE")
    tf <- grepl(del, df$family)
    if (any(tf)) {
      df$family[tf] <- "Fabaceae"
    }

    # Clean $genus
    del <- c("[0-9]|^Sp$|^Sp[.]|indet[.]|Leguminosae|^Genre$|^Indet$|^INDET|^Sp[.]$|^Sp$")
    tf <- grepl(del, df$genus)
    if (any(tf)) {
      df$genus[tf] <- NA
    }

    # General cleaning
    tf <- grepl("^[[:lower:]]", df$genus)
    if (any(tf)) {
      df$genus[tf] <- .firstUp(df$genus[tf])
    }

    # Clean $specificEpithet
    tf <- grepl("[0-9]", df$specificEpithet)
    if (any(tf)) {
      df$specificEpithet[tf] <- NA
    }

    tf <- grepl("[&]", df$specificEpithet)
    if (any(tf)) {
      df$specificEpithet[tf] <- sub("[&].*|\\s[(].*", "", df$specificEpithet[tf])
    }

    # Clean $specificEpithet when there is only genus
    del <- c("\\ssp$|\\ssp[.]|\\sSp[.]|\\sindet[.]|^indet$")
    tf <- grepl(del, df$specificEpithet)
    if (any(tf)) {
      df$specificEpithet[tf] <- NA
    }

    tf <- !grepl("\\s", df$specificEpithet)
    if (any(tf)) {
      df$specificEpithet[tf] <- NA
    }

    del <- c("\\scf$|\\scf[.]|\\scf\\s|\\saff\\s|\\saff$|\\saff[.]|\\sCf[.]|\\sAff[.]|\\sCf$|\\sAff$")
    tf <- grepl(del, df$specificEpithet)
    if (any(tf)) {
      df$specificEpithet[tf] <- gsub(del, " ", df$specificEpithet[tf])
      df$specificEpithet[tf] <- gsub("\\s\\s", " ", df$specificEpithet[tf])
    }

  }

  # Put original family name back ####
  if (colname_family != "family") {
    names(df)[names(df) %in% "family"] <- colname_family
    if (rm_original_column == FALSE) {
      names(df)[names(df) %in% "familyOriginal"] <- paste0(colname_family, "Original")
    }
  }
  # Put original genus name back ####
  if (colname_genus != "genus") {
    names(df)[names(df) %in% "genus"] <- colname_genus
    if (rm_original_column == FALSE) {
      names(df)[names(df) %in% "genusOriginal"] <- paste0(colname_genus, "Original")
    }
  }
  # Put original specificEpithet name back ####
  if (colname_specificEpithet != "specificEpithet") {
    names(df)[names(df) %in% "specificEpithet"] <- colname_specificEpithet
    if (rm_original_column == FALSE) {
      names(df)[names(df) %in% "specificEpithetOriginal"] <- paste0(colname_specificEpithet, "Original")
    }
  }

  return(df)
}


#_______________________________________________________________________________
# Side function make first letter to upper case ####
.firstUp <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

