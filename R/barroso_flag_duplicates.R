#' Flag or Remove Duplicate Specimens
#'
#' @author Domingos Cardoso
#'
#' @description
#' Identifies and optionally removes duplicate herbarium specimen records based on
#' collector name and number, or—when the number is missing—by species name, collector
#' and date. Duplicates are grouped into blocks, and blocks with conflicting
#' identifications are highlighted. The herbaria and catalog numbers of all
#' duplicates are stored together, so no information is lost when duplicates are
#' removed.
#'
#' @details
#' This function is part of the internal workflow of the `barRoso` package, supporting
#' record reconciliation and dataset cleaning. Duplicates are records with the same
#' collector (`recordedBy`) and collector number (`recordNumber`). For records
#' without collector number, duplicates must share the species name, the collector
#' and the full collection date (`year`, `month`, `day`). Records with unknown
#' collector (`NA` or `"Unknown"`) are never treated as duplicates.
#'
#' The following columns are added after the collector number:
#' - `duplicate`: `TRUE` for records belonging to a block of duplicates.
#' - `duplicateGroup`: an identifier of each block of duplicates.
#' - `identificationConflict`: `TRUE` when the duplicates of a block are identified
#'   as different species (e.g. `"Ormosia arborea"` and `"Ormosia fastigiata"`) or
#'   genera. A duplicate identified just to genus (e.g. `"Ormosia"`) does not conflict
#'   with a complete name of the same genus. Names are compared exactly, so spelling
#'   variants (e.g. `"Ormosia trifoliata"` and `"Ormosia trifoliolata"`) are also
#'   flagged as conflicts, which helps finding misspelled names.
#' - `identificationUnresolved`: `TRUE` for the records of blocks with conflicting
#'   identifications whose name cannot be chosen by the rules below, as their names
#'   are equally frequent (e.g. two duplicates as `"Ormosia arborea"` and two as
#'   `"Ormosia fastigiata"`), even after `specialists`. These blocks need checking:
#'   with `rm_duplicates = TRUE` and `rm_tied_conflicts = FALSE`, all their records
#'   are kept. `FALSE` for the other duplicates, and `NA` for records that are not
#'   duplicates.
#' - `duplicateIdentifications`: all names found in the block, e.g.
#'   `"Ormosia arborea | Ormosia fastigiata"`.
#' - `duplicateCollectionCodes`: all herbaria of the block, e.g. `"MO | JBB"`.
#' - `duplicateCatalogNumbers`: all catalog numbers of the block with their herbarium,
#'   e.g. `"MO 2839102 | JBB 13548"`.
#'
#' The specific epithet is taken from `specificEpithet` or, when missing, from a
#' binomial in the column `species` (as in GBIF downloads).
#'
#' When `rm_duplicates = TRUE`, one record per block is kept, chosen as follows:
#' 1. Records identified to species are preferred over those identified just to genus.
#' 2. In blocks with conflicting identifications, the records identified by one of
#'    the `specialists` are preferred, whatever the number of duplicates with other
#'    names, when the taxon of expertise of the specialist includes one of the names
#'    of the block. A record without determiner (`identifiedBy`) that was collected
#'    by a specialist, as main (`recordedBy`) or additional collector
#'    (`addCollector`), counts as identified by the specialist.
#' 3. Among them, records with the most frequent identification in the block are
#'    preferred (e.g. the name given by two duplicates rather than by one).
#' 4. When two or more names are still equally frequent (e.g. two duplicates as
#'    `"Ormosia arborea"` and two as `"Ormosia fastigiata"`), it is not possible to
#'    know which name is correct, so the block is not reduced: all its records are
#'    kept, to be checked, as needed in taxonomic revisions and checklists. With
#'    `rm_tied_conflicts = TRUE`, as may suit macroecological analyses, the whole
#'    block is removed instead, and a message tells how many records and blocks were
#'    removed and why.
#' 5. Among the records with the chosen name, records with latitude and longitude,
#'    and then records with more filled columns, are preferred. If records are still
#'    tied, the first one in the data is kept.
#'
#' The kept record still has `identificationConflict` and all the names, herbaria and
#' catalog numbers of its block, so conflicting identifications remain visible
#' after removing the duplicates.
#'
#' @usage
#' barroso_flag_duplicates(df,
#'                         rm_duplicates = FALSE,
#'                         specialists = NULL,
#'                         rm_tied_conflicts = FALSE,
#'                         colname_recordedBy = "recordedBy",
#'                         colname_recordNumber = "recordNumber",
#'                         colname_genus = "genus",
#'                         colname_specificEpithet = "specificEpithet",
#'                         colname_identifiedBy = "identifiedBy",
#'                         colname_addCollector = "addCollector",
#'                         colname_family = "family",
#'                         colname_collectionCode = "collectionCode",
#'                         colname_catalogNumber = "catalogNumber",
#'                         colname_decimalLatitude = "decimalLatitude",
#'                         colname_decimalLongitude = "decimalLongitude")
#'
#' @param df A data frame with biodiversity specimen records.
#' @param rm_duplicates Logical; if `TRUE`, keeps just one record per block of
#' duplicates (default: `FALSE`).
#' @param specialists Optional data frame with the columns `specialist` and `taxon`,
#' one row for each specialist and taxon of expertise, which can be a family (e.g.
#' `"Myristicaceae"`, all genera and species of the family), a genus (e.g.
#' `"Ormosia"`) or a species (e.g. `"Ormosia arborea"`), e.g.
#' `data.frame(specialist = c("D. B. O. S. Cardoso", "D. B. O. S. Cardoso", "H. C. Lima"),
#' taxon = c("Ormosia", "Swartzia", "Leguminosae"))`. With `rm_duplicates = TRUE`, in
#' blocks with conflicting identifications that include a name of their taxa, the
#' records identified by them (or collected by them, also as additional collectors,
#' when without determiner) are preferred. Names are compared by surname and first
#' initial, so `"D. B. O. S. Cardoso"` matches `"D. Cardoso"`, and families by their
#' alternative names too, e.g. `"Leguminosae"` and `"Fabaceae"` (default: `NULL`).
#' @param rm_tied_conflicts Logical; with `rm_duplicates = TRUE`, if `TRUE`, blocks
#' whose conflicting names are equally frequent, even after `specialists`, are removed
#' entirely. If `FALSE`, all their records are kept (default: `FALSE`).
#' @param colname_recordedBy Column name for the collector (default: `"recordedBy"`).
#' @param colname_recordNumber Column name for the collector number (default: `"recordNumber"`).
#' @param colname_genus Column name for the genus (default: `"genus"`).
#' @param colname_specificEpithet Column name for the specific epithet (default: `"specificEpithet"`).
#' @param colname_identifiedBy Column name for the determiner (default: `"identifiedBy"`).
#' @param colname_addCollector Column name for the additional collectors (default:
#' `"addCollector"`).
#' @param colname_family Column name for the family, compared with the families in
#' `specialists` (default: `"family"`).
#' @param colname_collectionCode Column name for the herbarium acronym (default: `"collectionCode"`).
#' @param colname_catalogNumber Column name for the catalog number (default: `"catalogNumber"`).
#' @param colname_decimalLatitude Column name for latitude (default: `"decimalLatitude"`).
#' @param colname_decimalLongitude Column name for longitude (default: `"decimalLongitude"`).
#'
#' @return A data frame with the duplicate columns described above. If
#' `rm_duplicates = TRUE`, only one record per block of duplicates is kept.
#'
#' @examples
#' \dontrun{
#' df <- read.csv("herbarium_data.csv")
#' df_flagged <- barroso_flag_duplicates(df)
#'
#' # Blocks of duplicates with conflicting identifications
#' df_flagged[df_flagged$identificationConflict %in% TRUE, ]
#'
#' df_clean <- barroso_flag_duplicates(df, rm_duplicates = TRUE)
#'
#' # Identifications of specialists first, and tied conflicts removed
#' df_clean <- barroso_flag_duplicates(df, rm_duplicates = TRUE,
#'                                     specialists = data.frame(
#'                                       specialist = c("D. B. O. S. Cardoso", "H. C. Lima"),
#'                                       taxon = c("Ormosia", "Leguminosae")),
#'                                     rm_tied_conflicts = TRUE)
#' }
#'
#' @importFrom tibble add_column
#'
#' @export

barroso_flag_duplicates <- function(df,
                                    rm_duplicates = FALSE,
                                    specialists = NULL,
                                    rm_tied_conflicts = FALSE,
                                    colname_recordedBy = "recordedBy",
                                    colname_recordNumber = "recordNumber",
                                    colname_genus = "genus",
                                    colname_specificEpithet = "specificEpithet",
                                    colname_identifiedBy = "identifiedBy",
                                    colname_addCollector = "addCollector",
                                    colname_family = "family",
                                    colname_collectionCode = "collectionCode",
                                    colname_catalogNumber = "catalogNumber",
                                    colname_decimalLatitude = "decimalLatitude",
                                    colname_decimalLongitude = "decimalLongitude") {

  # Check the arguments ####
  .check_df(df)
  .check_flag(rm_duplicates, rm_tied_conflicts)
  specialists <- .check_specialists(specialists)
  .check_colnames(colname_recordedBy = colname_recordedBy,
                  colname_recordNumber = colname_recordNumber,
                  colname_genus = colname_genus,
                  colname_specificEpithet = colname_specificEpithet,
                  colname_identifiedBy = colname_identifiedBy,
                  colname_addCollector = colname_addCollector,
                  colname_family = colname_family,
                  colname_collectionCode = colname_collectionCode,
                  colname_catalogNumber = colname_catalogNumber,
                  colname_decimalLatitude = colname_decimalLatitude,
                  colname_decimalLongitude = colname_decimalLongitude)
  .check_cols(df, colname_recordedBy = colname_recordedBy,
              colname_recordNumber = colname_recordNumber)

  # Remove the columns of a previous run
  new_cols <- c("duplicate", "duplicateGroup", "identificationConflict",
                "identificationUnresolved", "duplicateIdentifications", "duplicateCollectionCodes",
                "duplicateCatalogNumbers")
  df <- df[, !names(df) %in% new_cols, drop = FALSE]

  n <- nrow(df)
  coll <- .col_or_na(df, colname_recordedBy)
  num <- .col_or_na(df, colname_recordNumber)
  genus <- .col_or_na(df, colname_genus)
  epithet <- .epithet(df, genus, colname_specificEpithet)
  species <- ifelse(is.na(genus), NA, ifelse(is.na(epithet), genus, paste(genus, epithet)))

  #_____________________________________________________________________________
  # Blocks of duplicates ####
  # Same collector and number, or same species, collector and date when the
  # number is missing
  known_coll <- !is.na(coll) & !coll %in% "Unknown"
  key <- rep(NA_character_, n)
  tf <- known_coll & !is.na(num)
  key[tf] <- paste("number", coll[tf], num[tf], sep = "\r")
  if (all(c("year", "month", "day") %in% names(df))) {
    y <- .col_or_na(df, "year")
    m <- .col_or_na(df, "month")
    d <- .col_or_na(df, "day")
    tf <- known_coll & is.na(num) & !is.na(species) & !is.na(y) & !is.na(m) & !is.na(d)
    key[tf] <- paste("date", species[tf], coll[tf], y[tf], m[tf], d[tf], sep = "\r")
  }

  size <- as.integer(table(key)[key])
  duplicate <- !is.na(size) & size > 1
  group <- rep(NA_integer_, n)
  group[duplicate] <- as.integer(factor(key[duplicate], levels = unique(key[duplicate])))

  #_____________________________________________________________________________
  # Identifications, herbaria and catalog numbers of each block ####
  # Singletons are their own block
  block <- ifelse(duplicate, paste0("g", group), paste0("r", seq_len(n)))
  rows <- split(seq_len(n), factor(block, levels = unique(block)))

  code <- .col_or_na(df, colname_collectionCode)
  catalog <- .col_or_na(df, colname_catalogNumber)
  code_catalog <- ifelse(is.na(catalog), code,
                         ifelse(is.na(code) | startsWith(catalog, code) |
                                  startsWith(catalog, gsub("[^[:alnum:]]", "", code)), catalog,
                                paste(code, catalog)))

  collapse <- function(x) {
    x <- unique(x[!is.na(x)])
    if (length(x) == 0) NA_character_ else paste(x, collapse = " | ")
  }
  info <- lapply(rows, function(i) {
    complete <- unique(species[i][!is.na(epithet[i])])
    conflict <- length(complete) > 1 || length(unique(stats::na.omit(genus[i]))) > 1
    c(conflict, collapse(species[i]), collapse(code[i]), collapse(code_catalog[i]))
  })
  info <- do.call(rbind, info)[match(block, names(rows)), , drop = FALSE]

  conflict <- ifelse(duplicate, info[, 1] == "TRUE", NA)
  identifications <- ifelse(duplicate, info[, 2], NA_character_)

  #_____________________________________________________________________________
  # The record kept in each block of duplicates, by the rules below, and the
  # blocks whose name cannot be chosen ####
  has_coords <- !is.na(.col_or_na(df, colname_decimalLatitude)) &
    !is.na(.col_or_na(df, colname_decimalLongitude))
  filled <- rowSums(!is.na(df) & as.matrix(df) != "", na.rm = TRUE)

  # Records identified by each specialist, or collected by them (also as additional
  # collectors) when without determiner, and records of their taxa of expertise
  by_person <- in_taxa <- matrix(FALSE, n, 0)
  if (!is.null(specialists)) {
    det <- .col_or_na(df, colname_identifiedBy)
    add <- .col_or_na(df, colname_addCollector)
    people <- unique(specialists$specialist)
    by_person <- vapply(people, function(p) {
      .is_specialist(det, p) |
        (is.na(det) & (.is_specialist(coll, p) | .is_specialist(add, p)))
    }, logical(n))
    in_taxa <- vapply(people, function(p) {
      .in_taxa(specialists$taxon[specialists$specialist == p],
               .col_or_na(df, colname_family), genus, species)
    }, logical(n))
    dim(by_person) <- dim(in_taxa) <- c(n, length(people))
  }

  blocks <- rows[startsWith(names(rows), "g")]
  choice <- lapply(blocks, function(i) {
    all_i <- i
    spec <- FALSE
    # 1. Identified to species rather than just to genus
    if (any(!is.na(epithet[i]))) i <- i[!is.na(epithet[i])]
    # 2. Identified by a specialist of one of the taxa of the block, in blocks with
    # conflicting identifications
    if (length(unique(stats::na.omit(species[i]))) > 1 && ncol(by_person) > 0) {
      experts <- colSums(in_taxa[all_i, , drop = FALSE]) > 0
      by_specialist <- rowSums(by_person[i, experts, drop = FALSE]) > 0
      if (any(by_specialist)) {
        i <- i[by_specialist]
        spec <- TRUE
      }
    }
    # 3. The most frequent identification in the block
    if (any(!is.na(species[i]))) {
      freq <- table(species[i])
      best <- names(freq)[freq == max(freq)]
      # 4. Names equally frequent: the name cannot be chosen
      if (length(best) > 1) return(list(keep = all_i, tied = TRUE, spec = spec))
      i <- i[species[i] %in% best]
    }
    # 5. With coordinates, then with more filled columns
    list(keep = i[order(-has_coords[i], -filled[i])][1], tied = FALSE, spec = spec)
  })
  tied <- names(blocks)[vapply(choice, `[[`, logical(1), "tied")]
  n_tied <- sum(lengths(blocks[tied]))
  unresolved <- ifelse(duplicate, block %in% tied, NA)

  df <- tibble::add_column(df,
                           duplicate = duplicate,
                           duplicateGroup = group,
                           identificationConflict = conflict,
                           identificationUnresolved = unresolved,
                           duplicateIdentifications = identifications,
                           duplicateCollectionCodes = info[, 3],
                           duplicateCatalogNumbers = info[, 4],
                           .after = colname_recordNumber)

  message(paste0("barroso_flag_duplicates: ", sum(duplicate), " records in ",
                 length(unique(stats::na.omit(group))), " blocks of duplicates, ",
                 length(unique(group[conflict %in% TRUE])),
                 " blocks with conflicting identifications, ", length(tied),
                 " of them unresolved (identificationUnresolved == TRUE)"))

  #_____________________________________________________________________________
  # Keep one record per block of duplicates ####
  if (rm_duplicates) {
    by_spec <- sum(vapply(choice, `[[`, logical(1), "spec") &
                     !vapply(choice, `[[`, logical(1), "tied"))
    keep <- unlist(lapply(names(blocks), function(b) {
      if (b %in% tied && rm_tied_conflicts) integer(0) else choice[[b]]$keep
    }))
    tf <- !duplicate | seq_len(n) %in% keep
    message(paste0("barroso_flag_duplicates: ", sum(!tf) - if (rm_tied_conflicts) n_tied else 0,
                   " duplicates removed",
                   if (by_spec > 0) paste0("; ", by_spec, " conflicting blocks resolved by the specialists")))
    if (length(tied) > 0) {
      if (rm_tied_conflicts) {
        message(paste0("barroso_flag_duplicates: ", n_tied, " records of ", length(tied),
                       " blocks removed, as their duplicates are identified as different ",
                       "species, equally frequent, so the correct name cannot be chosen ",
                       "(rm_tied_conflicts = TRUE)"))
      } else {
        message(paste0("barroso_flag_duplicates: all ", n_tied, " records of ", length(tied),
                       " blocks kept, as their duplicates are identified as different ",
                       "species, equally frequent, so the correct name cannot be chosen; ",
                       "check them with identificationUnresolved == TRUE"))
      }
    }
    df <- df[tf, , drop = FALSE]
  }

  df <- df[order(df[[colname_recordedBy]], df[[colname_recordNumber]]), , drop = FALSE]

  return(df)
}


#_______________________________________________________________________________
# Names given by specialists, compared by surname and first initial, e.g.
# "D. B. O. S. Cardoso" and "D. Cardoso", also among several names in the same
# cell, e.g. "D. Cardoso & L. P. Queiroz" ####
.is_specialist <- function(x, specialists) {
  key <- function(v) {
    v <- tolower(.ascii_squish(v))
    surname <- sub(".*\\s", "", v)
    paste(substr(v, 1, 1), surname)
  }
  spec <- key(specialists)
  vapply(x, function(v) {
    if (is.na(v)) return(FALSE)
    people <- trimws(strsplit(v, "\\s*(&|;|,|\\|)\\s*")[[1]])
    any(key(people[nzchar(people)]) %in% spec)
  }, logical(1), USE.NAMES = FALSE)
}

# Specialists as a data frame with the columns specialist and taxon ####
.check_specialists <- function(specialists) {
  if (is.null(specialists)) return(NULL)
  example <- paste0("e.g. data.frame(specialist = c(\"D. B. O. S. Cardoso\", \"H. C. Lima\"), ",
                    "taxon = c(\"Ormosia\", \"Leguminosae\")).")
  if (!is.data.frame(specialists) || !all(c("specialist", "taxon") %in% names(specialists))) {
    stop(paste("`specialists` must be a data frame with the columns specialist and taxon,",
               example), call. = FALSE)
  }
  specialists <- data.frame(specialist = trimws(as.character(specialists$specialist)),
                            taxon = trimws(as.character(specialists$taxon)))
  specialists <- specialists[!is.na(specialists$specialist) & nzchar(specialists$specialist) &
                               !is.na(specialists$taxon) & nzchar(specialists$taxon), ]
  if (nrow(specialists) == 0) {
    stop(paste("`specialists` has no specialist with a taxon,", example), call. = FALSE)
  }
  specialists
}

# Records of the taxa of expertise of a specialist: families, with their
# alternative names, genera or species ####
.in_taxa <- function(taxa, family, genus, species) {
  key <- function(v) .family_name(tolower(.ascii_squish(v)))
  taxa <- key(taxa)
  key(family) %in% taxa | key(genus) %in% taxa | key(species) %in% taxa
}

# Alternative family names into a single name, e.g. "leguminosae" into "fabaceae" ####
.family_name <- function(x) {
  alt <- c(leguminosae = "fabaceae", compositae = "asteraceae", gramineae = "poaceae",
           palmae = "arecaceae", cruciferae = "brassicaceae", guttiferae = "clusiaceae",
           labiatae = "lamiaceae", umbelliferae = "apiaceae")
  i <- !is.na(x) & x %in% names(alt)
  x[i] <- alt[x[i]]
  x
}

