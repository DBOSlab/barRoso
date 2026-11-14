#' Flag Records Identified by Specialists
#'
#' @author Domingos Cardoso
#'
#' @description
#' Flags the herbarium records whose identifications were given by specialists of
#' their taxa, and so are probably accurate, or keeps just these records, e.g. to
#' build a dataset of reliable identifications for distribution maps, niche models
#' or checklists. The specialists are given in a data frame with their taxa of
#' expertise, as in [barroso_flag_duplicates()].
#'
#' @details
#' A record is flagged when the name of a specialist is in `identifiedBy` (also
#' among several determiners, e.g. `"D. Cardoso & L. P. Queiroz"`), and the family,
#' genus or species of the record is among the taxa of expertise of the specialist.
#' Names of people are compared by surname and first initial, so
#' `"D. B. O. S. Cardoso"` matches `"D. Cardoso"`, and families also by their
#' alternative names, e.g. Leguminosae and Fabaceae.
#'
#' Two more kinds of records are flagged, as told in `specialistEvidence`:
#' - `"identifiedBy"`: identified by the specialist.
#' - `"recordedBy"`: without determiner, but collected by the specialist, as main
#'   (`recordedBy`) or additional collector (`addCollector`), as specialists usually
#'   identify their own collections.
#' - `"duplicate"`: a duplicate of the same specimen (same `duplicateGroup` of
#'   [barroso_flag_duplicates()]) with the same name was identified or collected by
#'   the specialist, e.g. the duplicate at MO of a specimen identified by the
#'   specialist at RB. This needs the columns of [barroso_flag_duplicates()], so
#'   flag the duplicates before removing them, as done by [barroso_std()] with
#'   `flag_specialists = TRUE`.
#'
#' The best results come from a database standardized with [barroso_std()], where the
#' names of collectors and determiners, and the families, are already standardized.
#'
#' @usage
#' barroso_flag_specialists(df,
#'                          specialists,
#'                          filter = FALSE,
#'                          colname_identifiedBy = "identifiedBy",
#'                          colname_recordedBy = "recordedBy",
#'                          colname_addCollector = "addCollector",
#'                          colname_family = "family",
#'                          colname_genus = "genus",
#'                          colname_specificEpithet = "specificEpithet")
#'
#' @param df A data frame with herbarium records, preferably standardized with
#' [barroso_std()].
#' @param specialists A data frame with the columns `specialist` and `taxon`, one row
#' for each specialist and taxon of expertise, which can be a family (e.g.
#' `"Myristicaceae"`), a genus (e.g. `"Ormosia"`) or a species (e.g.
#' `"Ormosia arborea"`).
#' @param filter Logical; if `TRUE`, only the records identified by the specialists
#' are returned (default: `FALSE`).
#' @param colname_identifiedBy Column name for the determiner (default: `"identifiedBy"`).
#' @param colname_recordedBy Column name for the collector (default: `"recordedBy"`).
#' @param colname_addCollector Column name for the additional collectors (default:
#' `"addCollector"`).
#' @param colname_family Column name for the family (default: `"family"`).
#' @param colname_genus Column name for the genus (default: `"genus"`).
#' @param colname_specificEpithet Column name for the specific epithet (default:
#' `"specificEpithet"`).
#'
#' @return The data frame with three new columns: `specialistIdentification`
#' (`TRUE` for the records identified by the specialists), `specialistName` (the
#' specialist, as written in `specialists`, or several separated by `" | "`) and
#' `specialistEvidence` (`"identifiedBy"`, `"recordedBy"` or `"duplicate"`). With
#' `filter = TRUE`, only the records with `specialistIdentification = TRUE`.
#'
#' @examples
#' \dontrun{
#' specialists <- data.frame(
#'   specialist = c("D. B. O. S. Cardoso", "D. B. O. S. Cardoso", "H. C. Lima"),
#'   taxon = c("Ormosia", "Swartzia", "Leguminosae"))
#'
#' # Flag the records identified by the specialists
#' df <- barroso_flag_specialists(df, specialists)
#' table(df$specialistEvidence)
#'
#' # Keep just these records
#' reliable <- barroso_flag_specialists(df, specialists, filter = TRUE)
#' }
#'
#' @importFrom tibble add_column
#'
#' @export

barroso_flag_specialists <- function(df,
                                     specialists,
                                     filter = FALSE,
                                     colname_identifiedBy = "identifiedBy",
                                     colname_recordedBy = "recordedBy",
                                     colname_addCollector = "addCollector",
                                     colname_family = "family",
                                     colname_genus = "genus",
                                     colname_specificEpithet = "specificEpithet") {

  # Check the arguments ####
  .check_df(df)
  .check_flag(filter)
  if (missing(specialists) || is.null(specialists)) {
    stop(paste("Please give the `specialists`, a data frame with the columns specialist and taxon,",
               "e.g. data.frame(specialist = \"H. C. Lima\", taxon = \"Leguminosae\")."),
         call. = FALSE)
  }
  specialists <- .check_specialists(specialists)
  .check_colnames(colname_identifiedBy = colname_identifiedBy,
                  colname_recordedBy = colname_recordedBy,
                  colname_addCollector = colname_addCollector,
                  colname_family = colname_family,
                  colname_genus = colname_genus,
                  colname_specificEpithet = colname_specificEpithet)
  if (!any(c(colname_identifiedBy, colname_recordedBy) %in% names(df))) {
    stop(paste0("Column(s) not found in `df`: '", colname_identifiedBy, "', '",
                colname_recordedBy, "'. If they have other names, set `colname_identifiedBy` ",
                "and `colname_recordedBy`."), call. = FALSE)
  }

  # Remove the columns of a previous run
  new_cols <- c("specialistIdentification", "specialistName", "specialistEvidence")
  df <- df[, !names(df) %in% new_cols, drop = FALSE]

  n <- nrow(df)
  coll <- .col_or_na(df, colname_recordedBy)
  genus <- .col_or_na(df, colname_genus)
  epithet <- .epithet(df, genus, colname_specificEpithet)
  species <- ifelse(is.na(genus), NA, ifelse(is.na(epithet), genus, paste(genus, epithet)))

  # Identified or collected by each specialist, within their taxa ####
  spec <- .specialist_matrices(df, specialists, coll, genus, species,
                               colname_identifiedBy, colname_addCollector, colname_family)
  identified <- spec$identified & spec$in_taxa
  collected <- spec$collected & spec$in_taxa
  people <- colnames(identified)
  who <- function(m) apply(m, 1, function(r) paste(people[r], collapse = " | "))

  evidence <- ifelse(rowSums(identified) > 0, "identifiedBy",
                     ifelse(rowSums(collected) > 0, "recordedBy", NA_character_))
  name <- ifelse(evidence %in% "identifiedBy", who(identified),
                 ifelse(evidence %in% "recordedBy", who(collected), NA_character_))

  # Duplicates with the same name of a record identified by a specialist ####
  if ("duplicateGroup" %in% names(df)) {
    group <- df$duplicateGroup
    key <- ifelse(is.na(group) | is.na(species), NA, paste(group, species, sep = "\r"))
    flagged <- which(!is.na(evidence) & !is.na(key))
    same <- which(is.na(evidence) & key %in% key[flagged])
    evidence[same] <- "duplicate"
    name[same] <- name[flagged][match(key[same], key[flagged])]
  }

  # Write the columns after the determiner, or after the collector ####
  after <- if (colname_identifiedBy %in% names(df)) colname_identifiedBy else colname_recordedBy
  df <- tibble::add_column(df,
                           specialistIdentification = !is.na(evidence),
                           specialistName = name,
                           specialistEvidence = evidence,
                           .after = after)

  counts <- table(factor(evidence, levels = c("identifiedBy", "recordedBy", "duplicate")))
  message(paste0("barroso_flag_specialists: ", sum(!is.na(evidence)), " of ", n,
                 " records identified by the specialists (", counts[["identifiedBy"]],
                 " as determiners, ", counts[["recordedBy"]], " as collectors without ",
                 "determiner, ", counts[["duplicate"]], " by a duplicate)"))

  if (filter) df <- df[!is.na(evidence), , drop = FALSE]

  return(df)
}
