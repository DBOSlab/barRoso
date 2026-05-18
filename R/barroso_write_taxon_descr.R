#' Build species morphological descriptions from an Excel spreadsheet and export to Word (.docx).
#'
#' @author Domingos Cardoso
#'
#' @description This function reads morphological data from an Excel spreadsheet,
#'   generates standardized species descriptions in plain text and Word document formats,
#'   and exports them to a Word (.docx) file. It handles complex morphological data
#'   structures including measurements, subterms and grouping of related characters.
#'   The function automatically detects measurement ranges and formats them with
#'   en dashes (e.g., "4.3–14.5 cm"). Optionally, it can compute and print averages
#'   for user-selected measurement columns when a species has more than a threshold
#'   number of values (see \code{avg_cols} and \code{avg_min_n}).
#'
#' @details \strong{Column naming conventions}
#'
#' The function relies on a structured column naming system to parse and organize
#' morphological traits. Columns must follow these conventions:
#'
#' \describe{
#'   \item{\strong{Main character groups}}{Use ALL CAPS for the primary category:
#'     \code{HABIT}, \code{STIPULE}, \code{LEAF}, \code{INFLORESCENCE},
#'     \code{FLOWER}, \code{FRUIT}, \code{SEED}. These will become section headers.}
#'
#'   \item{\strong{Simple characters}}{Lowercase descriptive terms after the main group.
#'     Examples: \code{LEAF shape}, \code{LEAF margin}, \code{FLOWER symmetry}.
#'     These will be appended directly after the lowercase group name.}
#'
#'   \item{\strong{Hierarchical characters}}{Use ALL CAPS for subgroup terms.
#'     Examples: \code{LEAF PETIOLE length (cm)}, \code{FLOWER SEPALS shape}.
#'     The capitalized term (\code{PETIOLE}, \code{SEPALS}) is extracted once
#'     and used as a subheading.}
#'
#'   \item{\strong{Measurements}}{Include units in parentheses for automatic detection:
#'     \code{LEAF length (cm)}, \code{STIPULE width (mm)}, \code{HABIT height (m)}.
#'     Multiple measurements (e.g., length + width) are automatically combined
#'     with "×" and the unit is placed at the end.}
#'
#'   \item{\strong{Column name flexibility}}{Spaces, dots, or underscores can be used
#'     as separators. All of the following are valid:
#'     \itemize{
#'       \item \code{STIPULE length (mm)}
#'       \item \code{STIPULE.length.(mm)}
#'       \item \code{STIPULE_length_mm}
#'       \item \code{LEAF PETIOLE shape}
#'       \item \code{LEAF.PETIOLE.shape}
#'     }}
#' }
#'
#' \strong{Example dataset}
#'
#' The package includes an example dataset \code{morphological_dataset} containing
#' morphological data for 15 species of \emph{Ouratea} (Ochnaceae) compiled from
#' herbarium specimens and field observations for a taxonomic revision. This dataset
#' demonstrates proper column naming conventions and can be used to test the function:
#'
#' \preformatted{
#' data(morphological_dataset)
#' head(names(morphological_dataset))
#' }
#'
#' \strong{Creating a template}
#'
#' To create a properly formatted Excel template with all required columns for your
#' plant group, use the companion function \code{\link{barroso_add_char_template}}.
#' This function generates a ready-to-use template based on your species list and
#' selected plant group (e.g., "Ochnaceae", "Asteraceae", "Orchidaceae",
#' "Leguminosae-Papilionoideae"), saving hours of manual column creation.
#'
#' \preformatted{
#' # Create a template for Ochnaceae
#' barroso_add_char_template(
#'   species_df = my_species_list,
#'   plant_group = "Ochnaceae",
#'   filename = "Ochnaceae_template"
#' )
#' }
#'
#' @param xlsx_path Character. Path to the .xlsx file containing morphological data
#'   with properly structured column names (see Details for naming conventions).
#' @param species_cols Character vector or integer indices. Column names or indices
#'   used to build the species header (e.g., c("Genus", "Species", "Author") or 1:3).
#'   These can be combined flexibly (e.g., a single "Scientific_name" column or
#'   separate Genus, Species, and Author columns).
#' @param character_cols Character vector or integer indices. Column names or indices
#'   of morphological character columns (e.g., c("HABIT", "LEAF shape",
#'   "LEAF length (cm)") or 4:20). These must follow the naming conventions described
#'   in Details.
#' @param sheet Character string or integer. Sheet name or index passed to
#'   \code{readxl::read_excel()}. Default is 1.
#' @param species_filter Optional character vector of species names to filter.
#'   Only species matching these names will have descriptions generated.
#'   The names should match the species name as it appears when concatenating
#'   the \code{species_cols} columns (without authorship).
#' @param avg_cols Optional character vector or integer indices of columns for
#'   which an average should be reported, displayed immediately after the existing
#'   range (e.g. "3–8 × 1–2 cm, average 5.5 × 1.5 cm"). Must be a subset of
#'   \code{character_cols}. Default \code{NULL} (no averages reported).
#' @param avg_min_n Integer. Minimum number of non-empty observations per species
#'   required for an average to be reported. Averages are emitted only when the
#'   number of observed values is strictly greater than \code{avg_min_n}.
#'   Default \code{3} (i.e., need at least 4 values for averaging).
#' @param approx_char Character string to use for approximate measurements when
#'   no variation is present (e.g., "ca." or "c."). Default is "ca.".
#'   Set to \code{NULL} to disable adding approximation. Examples:
#'   \code{"ca. 0.1 × 0.1 cm"} (no variation) vs \code{"1.3–2.5 × 0.6–1.1 cm"}
#'   (variation present, no "ca.").
#' @param font_family Character string. Font family for the Word document.
#'   Default is "Times New Roman". Common options include "Arial", "Calibri",
#'   "Cambria".
#' @param font_size Numeric. Font size in points. Default is 12.
#' @param species_bold Logical. Whether to make species names bold.
#'   Default is TRUE.
#' @param species_italic Logical. Whether to make species names italic.
#'   Default is TRUE (following botanical conventions).
#' @param group_bold Logical. Whether to make group names (e.g., "Habit",
#'   "Leaf") bold. Default is TRUE.
#' @param group_italic Logical. Whether to make group names italic.
#'   Default is FALSE.
#' @param description_bold Logical. Whether to make description text bold.
#'   Default is FALSE.
#' @param description_italic Logical. Whether to make description text italic.
#'   Default is FALSE.
#' @param verbose Logical. Print progress messages. Default TRUE.
#' @param dir Output directory. Default NULL (uses Excel filename as directory name).
#' @param filename Output file base name (without extension). Default NULL
#'   (uses Excel filename with "_descriptions" suffix).
#'
#' @return Invisibly returns a data.frame with two columns:
#'   \itemize{
#'     \item{\code{species_name}: Full species name with authorship}
#'     \item{\code{description_plain}: Plain text description}
#'   }
#'   The main output is a Word document (.docx) saved to the specified directory.
#'
#' @examples
#' \dontrun{
#' # Basic usage with example dataset
#' data(morphological_dataset)
#'
#' # First, save the example dataset to Excel
#' library(openxlsx)
#' write.xlsx(morphological_dataset, "morphological_dataset.xlsx")
#'
#' # Generate descriptions using proper column selection
#' barroso_write_taxon_descr(
#'   xlsx_path = "morphological_dataset.xlsx",
#'   species_cols = c("Genus", "Species", "Author"),  # Taxonomic identification
#'   character_cols = 19:131,  # Morphological character columns
#'   approx_char = "ca."  # Add "ca." to measurements without variation
#' )
#'
#' # Generate descriptions with average calculation
#' barroso_write_taxon_descr(
#'   xlsx_path = "morphological_data.xlsx",
#'   species_cols = c("Genus", "Species", "Author"),
#'   character_cols = 4:20,
#'   avg_cols = c("LEAF length (cm)", "FRUIT length (cm)"),
#'   avg_min_n = 3,  # Calculate averages when n > 3 observations
#'   approx_char = "ca."
#' )
#'
#' # Filter to specific species only
#' barroso_write_taxon_descr(
#'   xlsx_path = "morphological_data.xlsx",
#'   species_cols = c("Genus", "Species"),
#'   character_cols = 3:15,
#'   species_filter = c("Ouratea concinna", "Ouratea coarctata")
#' )
#'
#' # Customize text formatting in Word output
#' barroso_write_taxon_descr(
#'   xlsx_path = "morphological_data.xlsx",
#'   species_cols = 1:3,
#'   character_cols = 4:25,
#'   font_family = "Arial",
#'   font_size = 11,
#'   species_bold = TRUE,
#'   species_italic = TRUE,
#'   group_bold = FALSE,
#'   group_italic = TRUE,
#'   description_bold = FALSE,
#'   description_italic = FALSE,
#'   filename = "MySpeciesDescriptions",
#'   dir = "Output"
#' )
#' }
#'
#' @seealso
#' \code{\link{barroso_add_char_template}} for creating properly formatted Excel templates,
#' \code{\link{morphological_dataset}} for an example dataset showing correct column naming
#'
#' @importFrom readxl read_excel
#' @importFrom stringr str_split_fixed str_split str_replace_all str_trim str_squish str_detect str_to_lower str_match
#' @importFrom officer read_docx body_add_fpar fpar fp_text ftext body_add_par
#' @importFrom dplyr select all_of group_by summarise across first
#' @importFrom tidyr unite
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom stats na.omit
#'
#' @export

barroso_write_taxon_descr <- function(xlsx_path,
                                      species_cols,
                                      character_cols,
                                      sheet = 1,
                                      species_filter = NULL,
                                      avg_cols = NULL,
                                      avg_min_n = 3,
                                      approx_char = "ca.",
                                      font_family = "Times New Roman",
                                      font_size = 12,
                                      species_bold = TRUE,
                                      species_italic = TRUE,
                                      group_bold = TRUE,
                                      group_italic = FALSE,
                                      description_bold = FALSE,
                                      description_italic = FALSE,
                                      verbose = TRUE,
                                      filename = NULL,
                                      dir = NULL) {

  # Creating the directory to save the file based on the current date
  if (!is.null(dir)) {
    dir <- .arg_check_dir(dir)
    foldername <- paste0(dir, "/", format(Sys.time(), "%d%b%Y"))
    if (!dir.exists(dir)) dir.create(dir)
    if (!dir.exists(foldername)) dir.create(foldername)
  } else {
    dir <- tools::file_path_sans_ext(basename(xlsx_path))
    foldername <- paste0(dir, "/", format(Sys.time(), "%d%b%Y"))
    if (!dir.exists(dir)) dir.create(dir)
    if (!dir.exists(foldername)) dir.create(foldername)
  }

  # xlsx_path check
  .arg_check_xlsx_path(xlsx_path)

  # Read Excel structured spreadsheet
  df <- readxl::read_excel(path = xlsx_path, sheet = sheet)
  names(df) <- gsub("[.]", " ", names(df))
  names(df) <- trimws(gsub("(\\s){2,}", " ", names(df)))

  # Allow user to pass indices or names
  species_cols <- .resolve_cols(df, species_cols,   arg = "species_cols")
  character_cols <- .resolve_cols(df, character_cols, arg = "character_cols")


  # Remove signals at the end of the string
  df[character_cols] <- data.frame(
    lapply(df[character_cols], function(x) {
      if (is.character(x)) {
        x <- trimws(gsub("[.,;:]+$", "", x))
      }
      x
    }),
    stringsAsFactors = FALSE
  )

  # Select columns
  selected_cols <- unique(c(species_cols, character_cols))
  missing_cols <- setdiff(selected_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("The following columns are not found in the data: ",
         paste(missing_cols, collapse = ", "))
  }
  df <- df %>% dplyr::select(dplyr::all_of(selected_cols))

  # Update character_cols to match exactly what's in the filtered dataframe
  character_cols <- intersect(character_cols, names(df))

  # Resolve avg_cols (must be a subset of character_cols)
  if (!is.null(avg_cols)) {
    avg_cols <- .resolve_cols(df, avg_cols, arg = "avg_cols")
    not_in_chars <- setdiff(avg_cols, character_cols)
    if (length(not_in_chars) > 0) {
      stop("avg_cols must be a subset of character_cols. ",
           "These columns are not in character_cols: ",
           paste(not_in_chars, collapse = ", "), call. = FALSE)
    }
  }

  # Filter by species if species_filter is provided
  if (!is.null(species_filter)) {
    species_names_vector <- df %>%
      tidyr::unite("species_name", dplyr::all_of(species_cols),
                   sep = " ", remove = FALSE, na.rm = TRUE) %>%
      .$species_name
    matches <- barRoso::remove_authorship(species_names_vector) %in% species_filter
    if (sum(matches) == 0) {
      warning("No species in the data match the species_filter: ",
              paste(species_filter, collapse = ", "))
    }
    df <- df[matches, , drop = FALSE]
    if (verbose) {
      message("Filtering to ", nrow(df), " species: ",
              paste(species_names_vector[matches], collapse = ", "))
    }
  }

  # ---- IMPORTANT: compute averages from the ORIGINAL (pre-merge) data ----
  # Done BEFORE en-dash conversion and BEFORE merging duplicates so each row
  # contributes its own observed value(s) to the per-species pool.
  species_averages <- .compute_species_averages(
    df, species_cols, avg_cols,
    avg_min_n = avg_min_n
  )

  # Normalize ranges to en-dashes
  df[] <- lapply(df, function(col) {
    gsub("(\\d+(?:\\.\\d+)?)\\s*\\-\\s*(\\d+(?:\\.\\d+)?)",
         "\\1–\\2", col, perl = TRUE)
  })

  # Merge duplicate species rows
  df <- .merge_duplicate_species(df, species_cols, character_cols)

  # Parse character columns
  char_meta <- .parse_character_columns(character_cols)

  # Output filename
  if (is.null(filename)) {
    base <- tools::file_path_sans_ext(basename(xlsx_path))
    filename <- paste0(foldername, "/", base, "_descriptions.docx")
  } else if (tools::file_ext(filename) == "") {
    filename <- paste0(foldername, "/", filename, ".docx")
  }

  out_tbl <- data.frame(
    species_name = character(0),
    description_plain = character(0),
    stringsAsFactors = FALSE
  )

  doc <- officer::read_docx()

  for (i in seq_len(nrow(df))) {
    row <- df[i, , drop = FALSE]
    species_name_parts <- .build_species_name_parts(row, species_cols)

    # Look up averages for this species
    row_averages <- .get_row_averages(row, species_cols, species_averages)

    desc_plain <- .build_species_description_plain(
      row, species_name_parts, char_meta,
      row_averages = row_averages,
      approx_char = approx_char
    )

    paragraphs <- .build_species_paragraph_runs(
      row, species_name_parts, char_meta,
      font_family = font_family,
      font_size = font_size,
      species_bold = species_bold,
      species_italic = species_italic,
      group_bold = group_bold,
      group_italic = group_italic,
      description_bold = description_bold,
      description_italic = description_italic,
      row_averages = row_averages,
      approx_char = approx_char
    )

    out_tbl <- rbind(
      out_tbl,
      data.frame(species_name = species_name_parts$full_name,
                 description_plain = desc_plain,
                 stringsAsFactors = FALSE)
    )

    # Add blank line between species
    if (i > 1) doc <- officer::body_add_par(doc, "")

    # Add species name as its own paragraph
    doc <- officer::body_add_fpar(doc, do.call(officer::fpar, paragraphs$name))

    # Add description as a separate paragraph (this ensures it starts on a new line)
    if (length(paragraphs$description) > 0) {
      doc <- officer::body_add_fpar(doc, do.call(officer::fpar, paragraphs$description))
    }
  }

  print(doc, target = filename)
  invisible(out_tbl)
}


# ---------------------------------------------------------------------------
# Number extraction helpers
# ---------------------------------------------------------------------------

#' Tokenise every numeric run in a string. Hyphens, en-dashes, commas, "or",
#' "x" and "×" are all treated as separators. CRITICAL: this does NOT honour
#' a leading "-" as a negative sign, because in morphological data the only
#' real-world meaning of "-" between digits is a range separator. With the
#' old regex \code{"-?\\d+\\.?\\d*"}, "12-21" extracted as 12 and -21
#' (producing absurd means and ranges); we want 12 and 21.
#' @keywords internal
#' @noRd
.tokenize_numbers <- function(s) {
  if (is.null(s) || length(s) == 0L) return(numeric(0))
  if (is.na(s) || !nzchar(s)) return(numeric(0))
  m <- regmatches(s, gregexpr("\\d+\\.?\\d*", s))[[1]]
  out <- suppressWarnings(as.numeric(m))
  out[!is.na(out)]
}

#' Extract every numeric token across a vector of cell values.
#' @keywords internal
#' @noRd
.extract_all_numerics <- function(vals) {
  out <- numeric(0)
  for (v in vals) {
    if (is.null(v)) next
    if (length(v) == 0L) next
    if (is.na(v)) next
    if (is.numeric(v)) {
      out <- c(out, v)
    } else {
      out <- c(out, .tokenize_numbers(as.character(v)))
    }
  }
  out
}


# ---------------------------------------------------------------------------
# Average computation
# ---------------------------------------------------------------------------

#' For each species, compute the mean of all numeric values in each avg_col.
#' Only emit an average if the number of non-empty observations is strictly
#' greater than \code{avg_min_n}.
#' @keywords internal
#' @noRd
.compute_species_averages <- function(df, species_cols, avg_cols, avg_min_n = 3) {
  if (is.null(avg_cols) || length(avg_cols) == 0L) return(list())

  species_id <- df %>%
    tidyr::unite("species_id", dplyr::all_of(species_cols),
                 sep = " ", remove = FALSE, na.rm = TRUE) %>%
    .$species_id
  species_id <- barRoso::remove_authorship(species_id)

  result <- list()
  for (sp in unique(species_id)) {
    idx <- which(species_id == sp)
    avgs <- list()
    for (col in avg_cols) {
      raw_vals <- df[[col]][idx]
      n_obs <- sum(!is.na(raw_vals) & nzchar(as.character(raw_vals)))
      if (n_obs <= avg_min_n) next
      nums <- .extract_all_numerics(raw_vals)
      if (length(nums) == 0L) next
      avgs[[col]] <- mean(nums, na.rm = TRUE)
    }
    if (length(avgs) > 0L) result[[sp]] <- avgs
  }
  result
}

#' Look up averages for the species in a given merged row.
#' @keywords internal
#' @noRd
.get_row_averages <- function(row, species_cols, species_averages) {
  if (is.null(species_averages) || length(species_averages) == 0L) return(list())
  parts <- vapply(species_cols, function(cn) .cell_as_text(row[[cn]][[1]]),
                  character(1))
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) return(list())
  sp_id <- paste(parts, collapse = " ")
  sp_id <- barRoso::remove_authorship(sp_id)
  if (sp_id %in% names(species_averages)) return(species_averages[[sp_id]])
  list()
}

#' Format an average number for printing.
#' @keywords internal
#' @noRd
.format_avg <- function(x, digits = 1) {
  if (is.null(x) || length(x) == 0L) return("")
  if (is.na(x) || !is.finite(x)) return("")
  if (isTRUE(all.equal(x, round(x)))) return(as.character(as.integer(round(x))))
  formatC(x, format = "f", digits = digits)
}


# ---------------------------------------------------------------------------
# Column resolution / parsing
# ---------------------------------------------------------------------------

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

#' @keywords internal
#' @noRd
.parse_character_columns <- function(character_cols) {
  main_terms <- vapply(character_cols, .extract_main_term, character(1))
  group_order <- unique(main_terms)
  groups <- lapply(group_order, function(main) {
    idx <- which(main_terms == main)
    cols <- character_cols[idx]
    parsed_cols <- lapply(cols, function(col) {
      list(
        name = col,
        hierarchy = .extract_hierarchy(col),
        is_length = .is_length_col(col),
        is_width = .is_width_col(col),
        is_height = .is_height_col(col),
        unit = .extract_unit_from_colname(col, fallback = "")
      )
    })
    subterm <- vapply(parsed_cols, function(p) {
      if (length(p$hierarchy) == 0) "" else p$hierarchy[length(p$hierarchy)]
    }, character(1))
    list(
      main = main,
      cols = cols,
      parsed_cols = parsed_cols,
      subterms = subterm,
      is_length = vapply(parsed_cols, function(p) p$is_length, logical(1)),
      is_width = vapply(parsed_cols, function(p) p$is_width, logical(1)),
      is_height = vapply(parsed_cols, function(p) p$is_height, logical(1)),
      units = vapply(parsed_cols, function(p) p$unit, character(1))
    )
  })
  list(group_order = group_order, groups = groups)
}

#' @keywords internal
#' @noRd
.extract_hierarchy <- function(col_name) {
  col_clean <- gsub("\\([^)]*\\)", "", col_name)
  col_clean <- gsub("\\.", " ", col_clean)
  tokens <- stringr::str_split(stringr::str_squish(col_clean), "\\s+")[[1]]
  if (length(tokens) <= 1L) return(character(0))
  hierarchy <- character(0)
  for (i in 2:length(tokens)) {
    token <- tokens[i]
    if (grepl("^[A-Z][A-Z\\-]*$", token) &&
        !tolower(token) %in% c("length", "width", "height", "long", "wide", "tall")) {
      hierarchy <- c(hierarchy, token)
    } else {
      break
    }
  }
  hierarchy
}

#' @keywords internal
#' @noRd
.extract_main_term <- function(col_name) {
  pre_dot <- stringr::str_split_fixed(col_name, "\\.", 2)[, 1]
  tokens <- stringr::str_split(stringr::str_trim(pre_dot), "\\s+")[[1]]
  tokens <- tokens[tokens != ""]
  if (length(tokens) == 0) col_name else tokens[[1]]
}

#' @keywords internal
#' @noRd
.extract_subterm <- function(col_name) {
  stripped <- stringr::str_replace_all(col_name, "[()]", " ")
  tokens <- stringr::str_split(stringr::str_squish(stripped), "\\s+")[[1]]
  if (length(tokens) <= 1L) return("")
  rest <- tokens[-1]
  hit <- rest[stringr::str_detect(rest, "^[\\p{Lu}]")]
  if (length(hit) > 0L) hit[[1]] else ""
}

#' @keywords internal
#' @noRd
.is_height_col <- function(col_name) {
  stripped <- stringr::str_replace_all(col_name, "[()]", " ")
  tokens <- stringr::str_split(stringr::str_to_lower(stringr::str_squish(stripped)),
                               "\\s+")[[1]]
  any(tokens == "height")
}

#' @keywords internal
#' @noRd
.is_length_col <- function(col_name) {
  stripped <- stringr::str_replace_all(col_name, "[()]", " ")
  tokens <- stringr::str_split(stringr::str_to_lower(stringr::str_squish(stripped)),
                               "\\s+")[[1]]
  any(tokens == "length")
}

#' @keywords internal
#' @noRd
.is_width_col <- function(col_name) {
  stripped <- stringr::str_replace_all(col_name, "[()]", " ")
  tokens <- stringr::str_split(stringr::str_to_lower(stringr::str_squish(stripped)),
                               "\\s+")[[1]]
  any(tokens == "width")
}

#' @keywords internal
#' @noRd
.build_species_name_parts <- function(row, species_cols) {
  parts <- vapply(species_cols, function(cn) .cell_as_text(row[[cn]][[1]]),
                  character(1))
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) {
    return(list(genus_species = "Unknown species", author = "",
                full_name = "Unknown species"))
  }
  genus_species <- parts[1]
  author <- ""
  if (length(parts) > 1) {
    last_part <- parts[length(parts)]
    if (grepl("[()]", last_part) || grepl("^[A-Z]", last_part)) {
      author <- last_part
      genus_species <- paste(parts[-length(parts)], collapse = " ")
    } else {
      genus_species <- paste(parts, collapse = " ")
    }
  }
  list(
    genus_species = genus_species,
    author = author,
    full_name = if (nzchar(author)) paste(genus_species, author) else genus_species
  )
}


# ---------------------------------------------------------------------------
# Description / runs builders
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.build_species_description_plain <- function(row, species_name_parts, char_meta,
                                             row_averages = list(),
                                             approx_char = NULL) {
  sentences <- character(0)
  for (l in seq_along(char_meta$groups)) {
    group <- char_meta$groups[[l]]
    items <- .build_group_items(row, group$cols, group$parsed_cols,
                                group$subterms, group$is_length,
                                group$is_width, group$is_height,
                                group$units,
                                row_averages = row_averages,
                                approx_char = approx_char)
    if (length(items) == 0L) next
    formatted_items <- .format_group_items(items)
    if (l == 1) {
      # First group (Habit) is special: the very first trait is the habit
      # value itself (e.g. "Tree", "Shrub") and the separator BETWEEN that
      # trait and the next one is a single space — NOT a semicolon.
      items_list <- strsplit(formatted_items, "; ")[[1]]
      if (length(items_list) >= 2) {
        rebuilt <- paste0(items_list[1], " ", items_list[2])
        if (length(items_list) > 2) {
          rebuilt <- paste0(rebuilt, "; ",
                            paste(items_list[3:length(items_list)],
                                  collapse = "; "))
        }
        formatted_items <- rebuilt
      }
      sentences <- c(sentences,
                     paste0(toupper(substr(formatted_items, 1, 1)),
                            substr(formatted_items, 2, nchar(formatted_items)),
                            "."))
    } else {
      sentences <- c(sentences,
                     paste0(.upper_first_only(group$main), " ", formatted_items, "."))
    }
  }
  if (length(sentences) == 0L) return(paste0(species_name_parts$full_name))
  paste0(species_name_parts$full_name, "\n\n", paste(sentences, collapse = " "))
}

#' @keywords internal
#' @noRd
.format_group_items <- function(items) {
  if (length(items) == 0) "" else paste(items, collapse = "; ")
}

#' @keywords internal
#' @noRd
.build_species_paragraph_runs <- function(row, species_name_parts, char_meta,
                                          font_family = "Arial",
                                          font_size = 11,
                                          species_bold = TRUE,
                                          species_italic = TRUE,
                                          group_bold = TRUE,
                                          group_italic = FALSE,
                                          description_bold = FALSE,
                                          description_italic = FALSE,
                                          row_averages = list(),
                                          approx_char = NULL) {
  species_prop <- officer::fp_text(font.family = font_family, font.size = font_size,
                                   bold = species_bold, italic = species_italic)
  author_prop <- officer::fp_text(font.family = font_family, font.size = font_size,
                                  bold = FALSE, italic = FALSE)
  group_prop <- officer::fp_text(font.family = font_family, font.size = font_size,
                                 bold = group_bold, italic = group_italic)
  description_prop <- officer::fp_text(font.family = font_family, font.size = font_size,
                                       bold = description_bold,
                                       italic = description_italic)
  normal_prop <- officer::fp_text(font.family = font_family, font.size = font_size,
                                  bold = FALSE, italic = FALSE)

  # ---- Paragraph 1: species name + authorship ----
  name_runs <- list(officer::ftext(species_name_parts$genus_species,
                                   prop = species_prop))
  if (nzchar(species_name_parts$author)) {
    name_runs <- c(name_runs,
                   list(officer::ftext(paste0(" ", species_name_parts$author),
                                       prop = author_prop)))
  }
  # Add a line break after the species name
  name_runs <- c(name_runs, list(officer::ftext("\n", prop = normal_prop)))

  # ---- Paragraph 2: morphological description ----
  desc_runs <- list()

  for (group_idx in seq_along(char_meta$groups)) {
    group <- char_meta$groups[[group_idx]]
    items <- .build_group_items(row, group$cols, group$parsed_cols,
                                group$subterms, group$is_length,
                                group$is_width, group$is_height,
                                group$units,
                                row_averages = row_averages,
                                approx_char = approx_char)
    if (length(items) == 0L) next

    formatted_items <- .format_group_items(items)

    if (group_idx == 1) {
      items_list <- strsplit(formatted_items, "; ")[[1]]
      if (length(items_list) > 0) {
        first_item <- items_list[1]
        first_item <- paste0(toupper(substr(first_item, 1, 1)),
                             substr(first_item, 2, nchar(first_item)))
        desc_runs <- c(desc_runs,
                       list(officer::ftext(first_item, prop = group_prop)))
        if (length(items_list) >= 2) {
          remainder <- paste0(" ", items_list[2])
          if (length(items_list) > 2) {
            remainder <- paste0(remainder, "; ",
                                paste(items_list[3:length(items_list)],
                                      collapse = "; "))
          }
          desc_runs <- c(desc_runs,
                         list(officer::ftext(remainder, prop = description_prop)))
        }
      }
    } else {
      if (length(desc_runs) > 0) {
        desc_runs <- c(desc_runs, list(officer::ftext(" ", prop = normal_prop)))
      }
      desc_runs <- c(desc_runs,
                     list(officer::ftext(.upper_first_only(group$main),
                                         prop = group_prop)))
      desc_runs <- c(desc_runs, list(officer::ftext(" ",
                                                    prop = normal_prop)))
      desc_runs <- c(desc_runs, list(officer::ftext(formatted_items,
                                                    prop = description_prop)))
    }
    desc_runs <- c(desc_runs, list(officer::ftext(".", prop = normal_prop)))
  }

  list(name = name_runs, description = desc_runs)
}

#' @keywords internal
#' @noRd
.build_group_items <- function(row, cols, parsed_cols, subterms,
                               is_length, is_width, is_height, units,
                               row_averages = list(),
                               approx_char = NULL) {
  items <- character(0)
  n <- length(cols)
  processed <- logical(n)

  i <- 1
  while (i <= n) {
    if (processed[i]) { i <- i + 1; next }

    cn <- cols[i]
    if (!cn %in% names(row)) { processed[i] <- TRUE; i <- i + 1; next }

    val <- .cell_as_text(row[[cn]][[1]])
    if (!nzchar(val)) { processed[i] <- TRUE; i <- i + 1; next }

    info <- parsed_cols[[i]]
    hierarchy <- info$hierarchy

    if (length(hierarchy) > 0) {
      first_level <- hierarchy[1]
      group_indices <- i
      if (i < n) {
        for (j in (i + 1):n) {
          if (processed[j]) next
          other_info <- parsed_cols[[j]]
          other_hi <- other_info$hierarchy
          if (length(other_hi) > 0 && other_hi[1] == first_level) {
            group_indices <- c(group_indices, j)
          }
        }
      }

      main_values <- character(0)
      subgroup_values <- list()
      group_measurements <- list()

      for (idx in group_indices) {
        if (processed[idx]) next
        col_cn <- cols[idx]
        if (!col_cn %in% names(row)) { processed[idx] <- TRUE; next }

        col_val <- .cell_as_text(row[[col_cn]][[1]])
        if (!nzchar(col_val)) { processed[idx] <- TRUE; next }

        col_info <- parsed_cols[[idx]]
        col_hi <- col_info$hierarchy

        if (col_info$is_length || col_info$is_width || col_info$is_height) {
          group_measurements <- c(group_measurements, list(list(
            value = col_val,
            is_length = col_info$is_length,
            is_width = col_info$is_width,
            is_height = col_info$is_height,
            unit = col_info$unit,
            sub_hierarchy = if (length(col_hi) > 1) col_hi[-1] else character(0),
            col_name = col_cn,
            avg = row_averages[[col_cn]]
          )))
        } else {
          if (length(col_hi) == 1) {
            main_values <- c(main_values, col_val)
          } else {
            sub <- tolower(col_hi[length(col_hi)])
            if (is.null(subgroup_values[[sub]])) subgroup_values[[sub]] <- character(0)
            subgroup_values[[sub]] <- c(subgroup_values[[sub]], col_val)
          }
        }
        processed[idx] <- TRUE
      }

      main_measurements <- list()
      sub_measurements <- list()
      for (meas in group_measurements) {
        if (length(meas$sub_hierarchy) == 0) {
          main_measurements <- c(main_measurements, list(meas))
        } else {
          k <- tolower(paste(meas$sub_hierarchy, collapse = "."))
          if (is.null(sub_measurements[[k]])) sub_measurements[[k]] <- list()
          sub_measurements[[k]] <- c(sub_measurements[[k]], list(meas))
        }
      }

      group_parts <- character(0)
      if (length(main_values) > 0) group_parts <- c(group_parts, main_values)

      if (length(main_measurements) > 0) {
        dims <- .collect_dims(main_measurements)
        measurement_text <- .format_measurement_with_unit(
          dims$len_val, dims$wid_val, dims$ht_val,
          dims$len_unit, dims$wid_unit, dims$ht_unit,
          len_avg = dims$len_avg, wid_avg = dims$wid_avg, ht_avg = dims$ht_avg,
          approx_char = approx_char
        )
        if (nzchar(measurement_text)) group_parts <- c(group_parts, measurement_text)
      }

      if (length(subgroup_values) > 0) {
        for (subname in sort(names(subgroup_values))) {
          sv <- subgroup_values[[subname]]
          if (length(sv) > 0) group_parts <- c(group_parts, paste(subname, sv))
        }
      }

      if (length(sub_measurements) > 0) {
        for (k in names(sub_measurements)) {
          dims <- .collect_dims(sub_measurements[[k]])
          measurement_text <- .format_measurement_with_unit(
            dims$len_val, dims$wid_val, dims$ht_val,
            dims$len_unit, dims$wid_unit, dims$ht_unit,
            len_avg = dims$len_avg, wid_avg = dims$wid_avg, ht_avg = dims$ht_avg,
            approx_char = approx_char
          )
          if (nzchar(measurement_text)) {
            group_parts <- c(group_parts, paste(k, measurement_text))
          }
        }
      }

      if (length(group_parts) > 0) {
        items <- c(items, paste(tolower(first_level),
                                paste(group_parts, collapse = ", ")))
      }
      i <- i + 1

    } else {
      # No hierarchy - could be measurement or simple column
      if (info$is_length || info$is_width || info$is_height) {
        base_name <- gsub("\\s*\\([^)]+\\)", "", cn)
        base_name <- gsub("\\s*(length|width|height|long|wide|tall).*", "",
                          base_name, ignore.case = TRUE)
        base_name <- stringr::str_squish(base_name)

        related_indices <- integer(0)
        for (j in seq_along(cols)) {
          if (j == i || processed[j]) next
          other_cn <- cols[j]
          other_base <- gsub("\\s*\\([^)]+\\)", "", other_cn)
          other_base <- gsub("\\s*(length|width|height|long|wide|tall).*", "",
                             other_base, ignore.case = TRUE)
          other_base <- stringr::str_squish(other_base)
          if (other_base == base_name &&
              (parsed_cols[[j]]$is_length || parsed_cols[[j]]$is_width ||
               parsed_cols[[j]]$is_height) &&
              length(parsed_cols[[j]]$hierarchy) == 0) {
            related_indices <- c(related_indices, j)
          }
        }

        len_val <- wid_val <- ht_val <- ""
        len_unit <- wid_unit <- ht_unit <- ""
        len_avg <- wid_avg <- ht_avg <- NA_real_

        if (info$is_length) { len_val <- val; len_unit <- info$unit
        len_avg <- row_averages[[cn]] %||% NA_real_ }
        else if (info$is_width) { wid_val <- val; wid_unit <- info$unit
        wid_avg <- row_averages[[cn]] %||% NA_real_ }
        else if (info$is_height) { ht_val <- val; ht_unit <- info$unit
        ht_avg <- row_averages[[cn]] %||% NA_real_ }
        processed[i] <- TRUE

        for (j in related_indices) {
          if (processed[j]) next
          ri <- parsed_cols[[j]]
          rv <- .cell_as_text(row[[cols[j]]][[1]])
          if (nzchar(rv)) {
            if (ri$is_length) { len_val <- rv; len_unit <- ri$unit
            len_avg <- row_averages[[cols[j]]] %||% NA_real_ }
            else if (ri$is_width) { wid_val <- rv; wid_unit <- ri$unit
            wid_avg <- row_averages[[cols[j]]] %||% NA_real_ }
            else if (ri$is_height) { ht_val <- rv; ht_unit <- ri$unit
            ht_avg <- row_averages[[cols[j]]] %||% NA_real_ }
          }
          processed[j] <- TRUE
        }

        measurement <- .format_measurement_with_unit(
          len_val, wid_val, ht_val, len_unit, wid_unit, ht_unit,
          len_avg = len_avg, wid_avg = wid_avg, ht_avg = ht_avg,
          approx_char = approx_char
        )
        if (nzchar(measurement)) items <- c(items, measurement)

      } else {
        items <- c(items, val)
        processed[i] <- TRUE
      }
      i <- i + 1
    }
  }
  items[nzchar(items)]
}

#' Null-coalescing helper used above
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

#' Helper function to detect if a measurement has variation
#' @keywords internal
#' @noRd
.has_variation <- function(value_str) {
  if (is.null(value_str) || !nzchar(value_str)) return(FALSE)
  # Check for range indicators: en-dash, hyphen, "or", "to", comma with numbers
  grepl("–|-|\\sor\\s|\\sto\\s|\\d+\\s*,\\s*\\d+", value_str)
}

#' Collect length/width/height values, units, and averages from a list of
#' measurement structs.
#' @keywords internal
#' @noRd
.collect_dims <- function(meas_list) {
  len_val <- wid_val <- ht_val <- ""
  len_unit <- wid_unit <- ht_unit <- ""
  len_avg <- wid_avg <- ht_avg <- NA_real_
  for (m in meas_list) {
    if (isTRUE(m$is_length)) {
      len_val <- m$value
      len_unit <- m$unit
      if (!is.null(m$avg) && length(m$avg) > 0L) len_avg <- m$avg
    } else if (isTRUE(m$is_width)) {
      wid_val <- m$value
      wid_unit <- m$unit
      if (!is.null(m$avg) && length(m$avg) > 0L) wid_avg <- m$avg
    } else if (isTRUE(m$is_height)) {
      ht_val <- m$value
      ht_unit <- m$unit
      if (!is.null(m$avg) && length(m$avg) > 0L) ht_avg <- m$avg
    }
  }
  list(len_val = len_val, wid_val = wid_val, ht_val = ht_val,
       len_unit = len_unit, wid_unit = wid_unit, ht_unit = ht_unit,
       len_avg = len_avg, wid_avg = wid_avg, ht_avg = ht_avg)
}

#' @keywords internal
#' @noRd
.extract_unit_from_colname <- function(col_name, fallback = "") {
  m <- stringr::str_match(col_name, "\\(([^)]+)\\)")
  if (!is.na(m[, 2]) && nzchar(m[, 2])) stringr::str_trim(m[, 2]) else fallback
}

#' Format a dimension expression from raw value strings (e.g. "3–8", "1–2")
#' @keywords internal
#' @noRd
.format_dim_text <- function(len_val, wid_val, ht_val,
                             len_unit, wid_unit, ht_unit,
                             approx_char = NULL) {
  len_val <- stringr::str_squish(len_val)
  wid_val <- stringr::str_squish(wid_val)
  ht_val <- stringr::str_squish(ht_val)

  unit <- ""
  if (nzchar(len_unit))      unit <- len_unit
  else if (nzchar(wid_unit)) unit <- wid_unit
  else if (nzchar(ht_unit))  unit <- ht_unit

  # Determine if we need to add approximation
  needs_approx <- FALSE
  if (!is.null(approx_char) && nzchar(approx_char)) {
    # Check each dimension for variation
    dims <- c(len_val, wid_val, ht_val)
    dims <- dims[nzchar(dims)]
    if (length(dims) > 0) {
      # If any dimension has variation, don't add "ca."
      has_var <- any(sapply(dims, .has_variation))
      if (!has_var) {
        needs_approx <- TRUE
      }
    }
  }

  # Build the prefix with approximation if needed
  prefix <- if (needs_approx) paste0(approx_char, " ") else ""

  if (nzchar(len_val) && nzchar(wid_val) && nzchar(ht_val)) {
    return(paste0(prefix, len_val, " × ", wid_val, " × ", ht_val,
                  if (nzchar(unit)) paste0(" ", unit) else ""))
  } else if (nzchar(len_val) && nzchar(wid_val)) {
    return(paste0(prefix, len_val, " × ", wid_val,
                  if (nzchar(unit)) paste0(" ", unit) else ""))
  } else if (nzchar(len_val) && nzchar(ht_val)) {
    return(paste0(prefix, len_val, " × ", ht_val,
                  if (nzchar(unit)) paste0(" ", unit) else ""))
  } else if (nzchar(wid_val) && nzchar(ht_val)) {
    return(paste0(prefix, wid_val, " × ", ht_val,
                  if (nzchar(unit)) paste0(" ", unit) else ""))
  } else if (nzchar(len_val)) {
    return(paste0(prefix, len_val,
                  if (nzchar(unit)) paste0(" ", unit, " long") else " long"))
  } else if (nzchar(wid_val)) {
    return(paste0(prefix, wid_val,
                  if (nzchar(unit)) paste0(" ", unit, " wide") else " wide"))
  } else if (nzchar(ht_val)) {
    return(paste0(prefix, ht_val,
                  if (nzchar(unit)) paste0(" ", unit, " tall") else " tall"))
  } else {
    return("")
  }
}

#' Format a measurement with units, optionally appending an "average ..." clause.
#' @keywords internal
#' @noRd
.format_measurement_with_unit <- function(len_val, wid_val, ht_val,
                                          len_unit, wid_unit, ht_unit,
                                          len_avg = NA_real_,
                                          wid_avg = NA_real_,
                                          ht_avg = NA_real_,
                                          approx_char = NULL) {
  base <- .format_dim_text(len_val, wid_val, ht_val,
                           len_unit, wid_unit, ht_unit,
                           approx_char = approx_char)
  if (!nzchar(base)) return("")

  has_avg <- (!is.na(len_avg) && is.finite(len_avg)) ||
    (!is.na(wid_avg) && is.finite(wid_avg)) ||
    (!is.na(ht_avg)  && is.finite(ht_avg))
  if (!has_avg) return(base)

  la <- if (nzchar(len_val) && !is.na(len_avg) && is.finite(len_avg))
    .format_avg(len_avg, digits = 1) else ""
  wa <- if (nzchar(wid_val) && !is.na(wid_avg) && is.finite(wid_avg))
    .format_avg(wid_avg, digits = 1) else ""
  ha <- if (nzchar(ht_val) && !is.na(ht_avg) && is.finite(ht_avg))
    .format_avg(ht_avg,  digits = 1) else ""

  avg_text <- .format_dim_text(la, wa, ha, len_unit, wid_unit, ht_unit,
                               approx_char = NULL)  # Don't add "ca." to averages
  if (!nzchar(avg_text)) return(base)
  paste0(base, ", average ", avg_text)
}

#' @keywords internal
#' @noRd
.cell_as_text <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  if (length(x) > 1L) x <- x[!is.na(x)]
  if (length(x) == 0) return("")
  if (is.na(x[[1]])) return("")
  if (is.logical(x[[1]])) return("")
  if (inherits(x[[1]], "POSIXt")) return(format(x[[1]], "%Y-%m-%d"))
  if (is.numeric(x[[1]])) {
    if (length(x) == 1L && isTRUE(all.equal(x[[1]], round(x[[1]]))) &&
        x[[1]] %in% 8:16) return("")
    if (isTRUE(all.equal(x[[1]], round(x[[1]])))) {
      return(as.character(as.integer(round(x[[1]]))))
    }
    return(format(x[[1]], trim = TRUE, scientific = FALSE))
  }
  txt <- stringr::str_squish(as.character(x[[1]]))
  if (!nzchar(txt)) return("")
  if (txt %in% c("TRUE", "FALSE", "black", "Arial", "baseline", "transparent")) return("")
  txt
}


# ---------------------------------------------------------------------------
# Duplicate-species merging (FIXED measurement extraction)
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.merge_duplicate_species <- function(df, species_cols, character_cols) {
  original_cols <- names(df)
  df_temp <- df %>%
    tidyr::unite("_species_id", dplyr::all_of(species_cols),
                 sep = " ", remove = FALSE, na.rm = TRUE)
  df_temp[[1]] <- barRoso::remove_authorship(df_temp[[1]])
  char_cols <- intersect(character_cols, names(df))

  df_temp <- df_temp %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(char_cols), as.character))

  is_measurement_col <- function(col_name) {
    col_lower <- tolower(col_name)
    any(stringr::str_detect(col_lower,
                            c("length", "width", "height",
                              "\\(cm\\)", "\\(mm\\)", "\\(m\\)")))
  }
  measurement_cols <- char_cols[sapply(char_cols, is_measurement_col)]
  non_measurement_cols <- setdiff(char_cols, measurement_cols)

  merge_non_measurement <- function(x) {
    x_clean <- x[!is.na(x) & nzchar(as.character(x))]
    if (length(x_clean) == 0) return(as.character(NA))
    x_clean <- as.character(x_clean)
    unique_vals <- unique(x_clean)
    if (length(unique_vals) == 1) return(unique_vals[1])
    unique_vals <- sort(unique_vals)
    if (length(unique_vals) == 2) return(paste(unique_vals, collapse = " or "))
    if (length(unique_vals) >= 3) {
      all_but_last <- unique_vals[1:(length(unique_vals) - 1)]
      last_item <- unique_vals[length(unique_vals)]
      return(paste(paste(all_but_last, collapse = ", "), "or", last_item))
    }
  }

  # FIXED merger.
  #
  # Bug being fixed: the old implementation did
  #   nums <- as.numeric(gsub("[^0-9.-]", "", x_clean))
  # which, for a cell containing "12–21" (en-dash), STRIPPED the en-dash and
  # parsed "1221", and for "12-21" (ASCII hyphen) failed to parse at all.
  # Combined with a cell of "10" elsewhere in the same column it produced
  # absurd merged ranges like "10–1221 × 9–718 cm".
  #
  # New behaviour: every numeric run in every cell is tokenised. "12-21" and
  # "12–21" each yield {12, 21}; "10" yields {10}; "1.5" yields {1.5}. The
  # merged range is then min..max over the pooled tokens, so {10, 12, 21}
  # becomes "10–21" as expected.
  merge_measurement <- function(x) {
    x_clean <- x[!is.na(x) & nzchar(as.character(x))]
    if (length(x_clean) == 0) return(as.character(NA))
    x_clean <- as.character(x_clean)

    nums <- numeric(0)
    for (val in x_clean) {
      nums <- c(nums, .tokenize_numbers(val))
    }

    if (length(nums) >= 2) {
      min_val <- min(nums); max_val <- max(nums)
      if (min_val == max_val) return(as.character(min_val))
      return(paste0(min_val, "–", max_val))
    } else if (length(nums) == 1) {
      return(as.character(nums))
    } else if (length(x_clean) == 1) {
      return(x_clean[1])
    } else {
      unique_vals <- sort(unique(x_clean))
      if (length(unique_vals) == 2) return(paste(unique_vals, collapse = " or "))
      if (length(unique_vals) >= 3) {
        all_but_last <- unique_vals[1:(length(unique_vals) - 1)]
        last_item <- unique_vals[length(unique_vals)]
        return(paste(paste(all_but_last, collapse = ", "), "or", last_item))
      }
    }
  }

  df_merged <- df_temp %>%
    dplyr::group_by(`_species_id`) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(species_cols),
                    ~ as.character(dplyr::first(na.omit(.)))),
      dplyr::across(dplyr::all_of(non_measurement_cols),
                    ~ merge_non_measurement(.)),
      dplyr::across(dplyr::all_of(measurement_cols),
                    ~ merge_measurement(.)),
      .groups = "drop"
    ) %>%
    dplyr::select(-`_species_id`)

  for (i in seq_along(measurement_cols)) {
    tf <- grepl(",|or", df_merged[[measurement_cols[i]]])
    if (any(tf)) {
      df_merged[[measurement_cols[i]]] <-
        .simplify_measurement_range(df_merged[[measurement_cols[i]]])
    }
  }

  df_merged <- df_merged[, original_cols]
  return(df_merged)
}

#' Reduce a string containing several numeric tokens (separated by commas,
#' "or", hyphens, en-dashes, …) to "min–max". Uses the same tokeniser as the
#' merger so range separators are never mistaken for negative signs.
#' @keywords internal
#' @noRd
.simplify_measurement_range <- function(x, sep = "–", na_string = NA_character_) {
  sapply(x, function(str) {
    if (is.na(str) || !nzchar(trimws(str))) return(na_string)
    nums <- .tokenize_numbers(str)
    if (length(nums) == 0) return(na_string)
    min_val <- min(nums); max_val <- max(nums)
    if (min_val == max_val) return(as.character(min_val))
    paste0(min_val, sep, max_val)
  }, USE.NAMES = FALSE)
}
