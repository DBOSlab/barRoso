#' Build species morphological descriptions from an Excel spreadsheet and export to Word (.docx).
#'
#' @author Domingos Cardoso
#'
#' @description This function reads morphological data from an Excel spreadsheet,
#'   generates standardized species descriptions in plain text and Word document formats,
#'   and exports them to a Word (.docx) file. It handles complex morphological data
#'   structures including measurements, subterms, and grouping of related characters.
#'   The function automatically detects measurement ranges and formats them with
#'   en dashes (e.g., "4.3–14.5 cm").
#'
#' @details The function expects an Excel file with specific column naming conventions.
#'   Morphological characters should be grouped with consistent naming patterns:
#'
#'   - Simple characters: "Habit", "LEAF shape", "LEAF margin"
#'   - Measurements: "LEAF length (cm)", "LEAF width (mm)"
#'   - Hierarchical structures: "FLOWER SEPALS length (mm)", "FLOWER SEPALS width (mm)"
#'
#'   The package includes an example dataset \code{morphological_dataset} containing
#'   morphological data for 15 species of \emph{Ouratea} (Ochnaceae) that demonstrates
#'   these naming conventions. This dataset can be used to test the function
#'   and understand the required data structure.
#'
#'   The function automatically:
#'   \itemize{
#'     \item{Detects and groups related morphological characters}
#'     \item{Handles measurement ranges (e.g., "4.3-14.5" becomes "4.3–14.5")}
#'     \item{Merges duplicate species entries}
#'     \item{Formats descriptions with customizable text formatting}
#'     \item{Exports to Word with proper scientific typography}
#'   }
#'
#'   Text formatting can be controlled via the \code{species_bold}, \code{group_bold},
#'   and related parameters. Species names are italicized by default following
#'   botanical conventions.
#'
#' @usage barroso_write_taxon_descr(
#'   xlsx_path,
#'   species_cols,
#'   character_cols,
#'   sheet = 1,
#'   species_filter = NULL,
#'   font_family = "Times New Roman",
#'   font_size = 12,
#'   species_bold = TRUE,
#'   species_italic = TRUE,
#'   group_bold = TRUE,
#'   group_italic = FALSE,
#'   description_bold = FALSE,
#'   description_italic = FALSE,
#'   verbose = TRUE,
#'   dir = NULL,
#'   filename = NULL
#' )
#'
#' @param xlsx_path Character string. Path to the .xlsx file containing
#'   morphological data.
#' @param species_cols Character vector or integer indices. Column names or
#'   indices used to build the species header (e.g., c("Genus", "Species",
#'   "Author") or 1:3).
#' @param character_cols Character vector or integer indices. Column names or
#'   indices of morphological character columns (e.g., c("Habit", "Leaf.length",
#'   "Leaf.width") or 4:20).
#' @param sheet Character string or integer. Sheet name or index passed to
#'   \code{readxl::read_excel()}. Default is 1.
#' @param species_filter Optional character vector of species names to filter.
#'   Only species matching these names will have descriptions generated.
#'   The names should match the species name as it appears when concatenating
#'   the \code{species_cols} columns (without authorship).
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
#' @param verbose Logical. If \code{TRUE} (default), progress messages are
#'   printed to the console. Set to \code{FALSE} for silent operation.
#' @param filename Character string. Basename for output files (without
#'   extension). If \code{NULL}, uses the Excel filename with "_descriptions"
#'   suffix.
#' @param dir Character string. Directory path where output will be saved.
#'   A date-stamped subfolder will be created inside this directory. If
#'   \code{NULL}, uses the Excel filename as directory name.
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
#' # Basic usage - generate descriptions for all species
#' barroso_write_taxon_descr(
#'   xlsx_path = "morphological_data.xlsx",
#'   species_cols = c("Genus", "Species", "Author"),
#'   character_cols = 4:20
#' )
#'
#' # Generate descriptions only for specific species
#' barroso_write_taxon_descr(
#'   xlsx_path = "morphological_data.xlsx",
#'   species_cols = c("Genus", "Species"),
#'   character_cols = 3:15,
#'   species_filter = c("Ouratea concinna", "Ouratea coarctata")
#' )
#'
#' # Customize formatting
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
#'
#' # Using column indices instead of names
#' barroso_write_taxon_descr(
#'   xlsx_path = "data.xlsx",
#'   species_cols = 1:2,          # First two columns: Genus and Species
#'   character_cols = 3:18        # Columns 3-18: morphological characters
#' )
#' }
#'
#' @examples
#' # Using the included morphological_dataset
#' \dontrun{
#' # First save the dataset to Excel format=
#' library(openxlsx)
#' openxlsx::write.xlsx(morphological_dataset, "morphological_dataset.xlsx")
#'
#' # Generate descriptions using the example dataset
#' barroso_write_taxon_descr(
#'   xlsx_path = "morphological_dataset.xlsx",
#'   species_cols = c("Genus", "Species", "Author"),
#'   character_cols = 19:131  # Morphological character columns
#' )
#' }
#'
#' @section Column naming conventions and text construction:
#' The function comprehensively parses column names to construct coherent descriptions.
#' Capitalized terms in column names serve as organizational elements that are
#' pulled out and used to structure the description text. Follow these conventions:
#'
#' \describe{
#'   \item{Species columns}{One or more columns containing taxonomic information.
#'     Can be flexible combinations:
#'     \itemize{
#'       \item{Single column: "Species_name" (e.g., "Ouratea concinna")}
#'       \item{Two columns: "Genus" and "Species" (e.g., "Ouratea" and "concinna")}
#'       \item{Three columns: "Genus", "Species", and "Author" (e.g., "Ouratea",
#'             "concinna", and "(Mart.) Engl.")}
#'       \item{Any other combination needed for your data structure}
#'     }
#'     These are concatenated to form the species header.}
#'   \item{Main character groups}{Use ALL CAPS: "HABIT", "LEAF",
#'     "Flower". These become section headers in the description.}
#'   \item{Hierarchical structures}{Use ALL CAPS for sub-levels: "LEAF shape",
#'     "LEAF margin", "FLOWER PETALS color". The capitalized terms ("LEAF",
#'     "PETALS") are extracted once and the descriptive text from the cell is
#'     appended. For example, a column named "LEAF shape" with cell value
#'     "elliptic to ovate" produces: "leaf elliptic to ovate".}
#'   \item{Measurements}{Include units in parentheses: "LEAF length (cm)",
#'        "PETAL width (mm)". Measurements with multiple values (e.g., "4.3-14.5")
#'        are automatically formatted with en dashes: "4.3–14.5 cm".}
#'   \item{Related measurements}{Columns like "LEAF length (cm)" and
#'        "LEAF width (cm)" are automatically combined: "4.3–14.5 × 2.1–3.8 cm".}
#'   \item{Higher hierarchies}{Complex structures like "FLOWER SEPALS BASE shape"
#'     are parsed as: "flower sepals base" + cell value. This allows grouping
#'     related characters under common headings.}
#' }
#'
#' @section Examples of species column flexibility:
#' \preformatted{
#' # Example 1: Single species name column
#' species_cols = "Species"  # Column contains "Ouratea concinna (Mart.) Engl."
#'
#' # Example 2: Separate genus and species
#' species_cols = c("Genus", "Species")  # "Ouratea" + "concinna"
#'
#' # Example 3: With separate authorship
#' species_cols = c("Genus", "Species", "Author")  # "Ouratea" + "concinna" + "(Mart.) Engl."
#'
#' # Example 4: With infraspecific rank
#' species_cols = c("Genus", "Species", "Infrasp_rank", "Infrasp", "Author")
#' # Produces: "Ouratea concinna subsp. coarctata (Mart.) Engl."
#' }
#'
#' @section How descriptions are constructed:
#' For each species, the function:
#' 1. **Parses column names** to extract hierarchical structure
#' 2. **Groups related characters** based on capitalized terms
#' 3. **Extracts capitalized terms once** to avoid repetition
#' 4. **Appends cell values** to create natural language descriptions
#' 5. **Formats measurements** with proper units and en dashes
#' 6. **Organizes text** with appropriate punctuation
#'
#' Example transformation:
#' \preformatted{
#' Column name: "LEAF shape" with cell value: "elliptic to ovate"
#' Result in description: "leaf elliptic to ovate"
#'
#' Column names: "LEAF length (cm)" (value: "4.3-14.5") and
#'               "LEAF width (cm)" (value: "2.1-3.8")
#' Result in description: "leaf 4.3–14.5 × 2.1–3.8 cm"
#'
#' Column names: "FLOWER PETALS color" (value: "white") and
#'               "FLOWER PETALS number" (value: "5")
#' Result in description: "flower petals white, 5"
#' }
#'
#' @section Output structure:
#' The generated Word document contains:
#' \itemize{
#'   \item{Species name with authorship (italicized by default)}
#'   \item{Morphological description organized by character groups}
#'   \item{Measurements formatted with proper units and en dashes for ranges}
#'   \item{Hierarchical characters grouped logically}
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom stringr str_split_fixed str_split str_replace_all str_trim str_squish str_detect str_to_lower str_match
#' @importFrom officer read_docx body_add_fpar fpar fp_text ftext body_add_par
#' @importFrom dplyr select all_of group_by summarise across
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
                                      font_family = "Times New Roman",
                                      font_size = 12,
                                      species_bold = TRUE,
                                      species_italic = TRUE,
                                      group_bold = TRUE,
                                      group_italic = FALSE,
                                      description_bold = FALSE,
                                      description_italic = FALSE,
                                      verbose = TRUE,
                                      dir = NULL,
                                      filename = NULL) {

  # Creating the directory to save the file based on the current date
  if (!is.null(dir)) {
    # dir check
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

  # Allow user to pass indices or names
  species_cols <- .resolve_cols(df, species_cols, arg = "species_cols")
  character_cols <- .resolve_cols(df, character_cols, arg = "character_cols")

  # Create a vector of all columns to select
  selected_cols <- unique(c(species_cols, character_cols))

  # Validate all columns exist
  missing_cols <- setdiff(selected_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("The following columns are not found in the data: ",
         paste(missing_cols, collapse = ", "))
  }

  # Select columns
  df <- df %>%
    dplyr::select(dplyr::all_of(selected_cols))

  # Update character_cols to match exactly what's in the filtered dataframe
  character_cols <- intersect(character_cols, names(df))

  # Filter by species if species_filter is provided
  if (!is.null(species_filter)) {
    species_names_vector <- df %>%
      tidyr::unite("species_name", dplyr::all_of(species_cols),
                   sep = " ",
                   remove = FALSE,
                   na.rm = TRUE) %>%
      .$species_name

    # Check if species_filter matches any species
    matches <- barRoso::remove_authorship(species_names_vector) %in% species_filter

    if (sum(matches) == 0) {
      warning("No species in the data match the species_filter: ",
              paste(species_filter, collapse = ", "))
    }

    # Filter the dataframe
    df <- df[matches, , drop = FALSE]

    if (verbose) {
      message("Filtering to ", nrow(df), " species: ",
              paste(species_names_vector[matches], collapse = ", "))
    }
  }

  df[] <- lapply(df, function(col) {
    gsub("(\\d+(?:\\.\\d+)?)\\s*\\-\\s*(\\d+(?:\\.\\d+)?)",
         "\\1–\\2",
         col,
         perl = TRUE)
  })

  df <- .merge_duplicate_species(df, species_cols, character_cols)

  # Parse character columns
  char_meta <- .parse_character_columns(character_cols)

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

    # Build descriptions for this specific row
    desc_plain <- .build_species_description_plain(row, species_name_parts, char_meta)

    runs <- .build_species_paragraph_runs(row, species_name_parts, char_meta,
                                          font_family = font_family,
                                          font_size = font_size,
                                          species_bold = species_bold,
                                          species_italic = species_italic,
                                          group_bold = group_bold,
                                          group_italic = group_italic,
                                          description_bold = description_bold,
                                          description_italic = description_italic)

    out_tbl <- rbind(
      out_tbl,
      data.frame(species_name = species_name_parts$full_name,
                 description_plain = desc_plain,
                 stringsAsFactors = FALSE)
    )

    # Add line break between species if not the first one
    if (i > 1) {
      doc <- officer::body_add_par(doc, "")
    }

    # Use do.call to pass the list of runs as arguments to fpar
    doc <- officer::body_add_fpar(doc, do.call(officer::fpar, runs))

  }

  print(doc, target = filename)

  invisible(out_tbl)
}

.resolve_cols <- function(df, cols, arg) {
  if (is.null(cols) || length(cols) == 0L) {
    stop(arg, " must be non-empty.", call. = FALSE)
  }

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
      stop("Missing columns for ", arg, ": ", paste(missing, collapse = ", "), call. = FALSE)
    }
    return(cols)
  }

  stop(arg, " must be a character vector of names or numeric indices.", call. = FALSE)
}

.parse_character_columns <- function(character_cols) {
  main_terms <- vapply(character_cols, .extract_main_term, character(1))
  group_order <- unique(main_terms)

  groups <- lapply(group_order, function(main) {
    idx <- which(main_terms == main)
    cols <- character_cols[idx]

    # Parse each column to extract its hierarchical structure
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

    # Extract subterm (deepest level) for backward compatibility
    subterm <- vapply(parsed_cols, function(p) {
      if (length(p$hierarchy) == 0) "" else p$hierarchy[length(p$hierarchy)]
    }, character(1))

    list(
      main = main,
      cols = cols,
      parsed_cols = parsed_cols,  # Store full parsed structure
      subterms = subterm,  # For backward compatibility
      is_length = vapply(parsed_cols, function(p) p$is_length, logical(1)),
      is_width = vapply(parsed_cols, function(p) p$is_width, logical(1)),
      is_height = vapply(parsed_cols, function(p) p$is_height, logical(1)),
      units = vapply(parsed_cols, function(p) p$unit, character(1))
    )
  })

  list(group_order = group_order, groups = groups)
}

.extract_hierarchy <- function(col_name) {
  # Clean the column name
  # Remove anything in parentheses (units)
  col_clean <- gsub("\\([^)]*\\)", "", col_name)
  # Replace dots with spaces
  col_clean <- gsub("\\.", " ", col_clean)
  # Split by spaces
  tokens <- stringr::str_split(stringr::str_squish(col_clean), "\\s+")[[1]]

  if (length(tokens) <= 1L) return(character(0))

  # The main term is the first token
  # Collect all subsequent tokens that are ALL CAPS
  hierarchy <- character(0)

  # Skip the first token (main group term)
  for (i in 2:length(tokens)) {
    token <- tokens[i]

    # Check if token is ALL CAPS (allowing for hyphens)
    # Exclude common measurement words that might be in ALL CAPS
    if (grepl("^[A-Z][A-Z\\-]*$", token) &&
        !tolower(token) %in% c("length", "width", "height", "long", "wide", "tall")) {
      hierarchy <- c(hierarchy, token)
    } else {
      # If we hit a lowercase word, stop collecting hierarchy
      break
    }
  }

  hierarchy
}

.extract_main_term <- function(col_name) {
  pre_dot <- stringr::str_split_fixed(col_name, "\\.", 2)[, 1]
  tokens <- stringr::str_split(stringr::str_trim(pre_dot), "\\s+")[[1]]
  tokens <- tokens[tokens != ""]
  if (length(tokens) == 0) col_name else tokens[[1]]
}

.extract_subterm <- function(col_name) {
  stripped <- stringr::str_replace_all(col_name, "[()]", " ")
  tokens <- stringr::str_split(stringr::str_squish(stripped), "\\s+")[[1]]
  if (length(tokens) <= 1L) return("")
  rest <- tokens[-1]
  hit <- rest[stringr::str_detect(rest, "^[\\p{Lu}]")]
  if (length(hit) > 0L) hit[[1]] else ""
}

.is_height_col <- function(col_name) {
  stripped <- stringr::str_replace_all(col_name, "[()]", " ")
  tokens <- stringr::str_split(stringr::str_to_lower(stringr::str_squish(stripped)), "\\s+")[[1]]
  any(tokens == "height")
}

.is_length_col <- function(col_name) {
  stripped <- stringr::str_replace_all(col_name, "[()]", " ")
  tokens <- stringr::str_split(stringr::str_to_lower(stringr::str_squish(stripped)), "\\s+")[[1]]
  any(tokens == "length")
}

.is_width_col <- function(col_name) {
  stripped <- stringr::str_replace_all(col_name, "[()]", " ")
  tokens <- stringr::str_split(stringr::str_to_lower(stringr::str_squish(stripped)), "\\s+")[[1]]
  any(tokens == "width")
}

.build_species_name_parts <- function(row, species_cols) {
  parts <- vapply(species_cols, function(cn) .cell_as_text(row[[cn]][[1]]), character(1))
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) {
    return(list(
      genus_species = "Unknown species",
      author = "",
      full_name = "Unknown species"
    ))
  }

  # Try to identify genus+species and author parts
  # Typically, the author is the last part if it contains parentheses or uppercase
  genus_species <- parts[1]
  author <- ""

  if (length(parts) > 1) {
    # Check if the last part looks like an author (contains parentheses or uppercase letters)
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

.build_species_description_plain <- function(row, species_name_parts, char_meta) {
  sentences <- character(0)
  for (l in seq_along(char_meta$groups)) {

    group = char_meta$groups[[l]]

    items <- .build_group_items(row, group$cols, group$parsed_cols,
                                group$subterms, group$is_length,
                                group$is_width, group$is_height,
                                group$units)
    if (length(items) == 0L) next

    # Format the group with semicolons between major items
    formatted_items <- .format_group_items(items)

    # For Habit group, capitalize first letter
    if (l == 1) {
      sentences <- c(sentences, paste0(toupper(substr(formatted_items, 1, 1)),
                                       substr(formatted_items, 2, nchar(formatted_items)), "."))
    } else {
      sentences <- c(sentences, paste0(.upper_first_only(group$main), " ", formatted_items, "."))
    }
  }

  if (length(sentences) == 0L) return(paste0(species_name_parts$full_name))
  paste0(species_name_parts$full_name, "\n\n", paste(sentences, collapse = " "))
}

.format_group_items <- function(items) {
  if (length(items) == 0) {
    return("")
  } else {
    return(paste(items, collapse = "; "))
  }

}

.build_species_paragraph_runs <- function(row, species_name_parts, char_meta,
                                          font_family = "Arial",
                                          font_size = 11,
                                          species_bold = TRUE,
                                          species_italic = TRUE,
                                          group_bold = TRUE,
                                          group_italic = FALSE,
                                          description_bold = FALSE,
                                          description_italic = FALSE) {

  # Create font properties with custom settings
  species_prop <- officer::fp_text(
    font.family = font_family,
    font.size = font_size,
    bold = species_bold,
    italic = species_italic
  )

  author_prop <- officer::fp_text(
    font.family = font_family,
    font.size = font_size,
    bold = FALSE,  # Author usually not bold
    italic = FALSE  # Author usually not italic
  )

  group_prop <- officer::fp_text(
    font.family = font_family,
    font.size = font_size,
    bold = group_bold,
    italic = group_italic
  )

  description_prop <- officer::fp_text(
    font.family = font_family,
    font.size = font_size,
    bold = description_bold,
    italic = description_italic
  )

  normal_prop <- officer::fp_text(
    font.family = font_family,
    font.size = font_size,
    bold = FALSE,
    italic = FALSE
  )

  bold_prop <- officer::fp_text(
    font.family = font_family,
    font.size = font_size,
    bold = TRUE,
    italic = FALSE
  )

  runs <- list()

  # Add genus+species with custom formatting
  runs <- c(runs, list(officer::ftext(species_name_parts$genus_species, prop = species_prop)))

  # Add author with custom formatting
  if (nzchar(species_name_parts$author)) {
    runs <- c(runs, list(officer::ftext(paste0(" ", species_name_parts$author), prop = author_prop)))
  }

  # Add newline (two line breaks for paragraph)
  runs <- c(runs, list(officer::ftext("\n\n", prop = normal_prop)))

  # Build description runs
  for (group_idx in seq_along(char_meta$groups)) {

    group <- char_meta$groups[[group_idx]]

    items <- .build_group_items(row, group$cols, group$parsed_cols,
                                group$subterms, group$is_length,
                                group$is_width, group$is_height,
                                group$units)

    if (length(items) == 0L) next

    # Format the group with semicolons between major items
    formatted_items <- .format_group_items(items)

    # For Habit group (first group), make first term bold and uppercase first letter
    if (group_idx == 1) {
      # Split items and capitalize first term
      items_list <- strsplit(formatted_items, "; ")[[1]]
      if (length(items_list) > 0) {
        # Capitalize first letter of first term
        first_item <- items_list[1]
        first_item <- paste0(toupper(substr(first_item, 1, 1)),
                             substr(first_item, 2, nchar(first_item)))

        # Add first term in bold
        runs <- c(runs, list(officer::ftext(first_item, prop = group_prop)))

        # Add rest of items
        if (length(items_list) > 1) {
          runs <- c(runs, list(officer::ftext(paste0("; ", paste(items_list[-1], collapse = "; ")), prop = description_prop)))
        }
      }
    } else {
      # Add group name in bold for other groups
      if (group_idx > 1) {
      runs <- c(runs, list(officer::ftext(.upper_first_only(group$main), prop = group_prop)))
      }
      runs <- c(runs, list(officer::ftext(" ", prop = normal_prop)))
      runs <- c(runs, list(officer::ftext(formatted_items, prop = description_prop)))
    }

    runs <- c(runs, list(officer::ftext(". ", prop = normal_prop)))
  }

  runs
}

.build_group_items <- function(row, cols, parsed_cols, subterms, is_length, is_width, is_height, units) {
  items <- character(0)

  # Track processed columns
  n <- length(cols)
  processed <- logical(n)

  # Process columns in original order
  i <- 1
  while (i <= n) {
    if (processed[i]) {
      i <- i + 1
      next
    }

    cn <- cols[i]
    if (!cn %in% names(row)) {
      processed[i] <- TRUE
      i <- i + 1
      next
    }

    val <- .cell_as_text(row[[cn]][[1]])
    if (!nzchar(val)) {
      processed[i] <- TRUE
      i <- i + 1
      next
    }

    info <- parsed_cols[[i]]
    hierarchy <- info$hierarchy

    # If column has hierarchy, group ALL columns with same first-level hierarchy
    if (length(hierarchy) > 0) {
      # Get the first level of hierarchy (e.g., "SEPALS" from "SEPALS BASE")
      first_level <- hierarchy[1]

      # Find ALL columns with the same first level (including measurements!)
      group_indices <- i

      # Look ahead for columns with same first level
      if (i < n) {
        for (j in (i+1):n) {
          if (processed[j]) next

          other_info <- parsed_cols[[j]]
          other_hierarchy <- other_info$hierarchy

          if (length(other_hierarchy) > 0 && other_hierarchy[1] == first_level) {
            group_indices <- c(group_indices, j)
          }
        }
      }

      # Now process ALL columns in this group together
      # We need to handle:
      # 1. Main values (hierarchy length = 1, e.g., "SEPALS free")
      # 2. Subgroup values (hierarchy length > 1, e.g., "SEPALS BASE truncate")
      # 3. Measurements within this group (e.g., "SEPALS length (mm)")

      main_values <- character(0)
      subgroup_values <- list()
      group_measurements <- list()  # Measurements within this hierarchy group

      for (idx in group_indices) {
        if (processed[idx]) next

        col_cn <- cols[idx]
        if (!col_cn %in% names(row)) {
          processed[idx] <- TRUE
          next
        }

        col_val <- .cell_as_text(row[[col_cn]][[1]])
        if (!nzchar(col_val)) {
          processed[idx] <- TRUE
          next
        }

        col_info <- parsed_cols[[idx]]
        col_hierarchy <- col_info$hierarchy

        # Check if it's a measurement column
        if (col_info$is_length || col_info$is_width || col_info$is_height) {
          # Add to group measurements
          group_measurements <- c(group_measurements, list(
            list(
              value = col_val,
              is_length = col_info$is_length,
              is_width = col_info$is_width,
              is_height = col_info$is_height,
              unit = col_info$unit,
              sub_hierarchy = if (length(col_hierarchy) > 1) col_hierarchy[-1] else character(0)
            )
          ))
        } else {
          # Non-measurement column
          if (length(col_hierarchy) == 1) {
            # Just the main term (e.g., "SEPALS")
            main_values <- c(main_values, col_val)
          } else {
            # Has sub-term (e.g., "SEPALS BASE")
            subterm <- tolower(col_hierarchy[length(col_hierarchy)])  # "base"

            # Initialize if needed
            if (is.null(subgroup_values[[subterm]])) {
              subgroup_values[[subterm]] <- character(0)
            }

            subgroup_values[[subterm]] <- c(subgroup_values[[subterm]], col_val)
          }
        }

        processed[idx] <- TRUE
      }

      # Now we need to handle measurements that might have their own sub-hierarchy
      # For example: "SEPALS length (mm)" vs "STYLE length (mm)" vs "GYNOECIUM length (mm)"
      # Within the SEPALS group, we only have "SEPALS length (mm)" which has no sub-hierarchy

      # First, let's combine measurements that share the same context
      # Measurements without sub-hierarchy go with main group
      # Measurements with sub-hierarchy (like "STYLE length" within "GYNOECIUM") need special handling

      main_measurements <- list()
      sub_measurements <- list()  # Measurements with sub-hierarchy, e.g., "STYLE" within "GYNOECIUM"

      for (meas in group_measurements) {
        if (length(meas$sub_hierarchy) == 0) {
          # No sub-hierarchy - goes with main group
          main_measurements <- c(main_measurements, list(meas))
        } else {
          # Has sub-hierarchy (e.g., "STYLE" in "GYNOECIUM STYLE length")
          sub_key <- tolower(paste(meas$sub_hierarchy, collapse = "."))

          if (is.null(sub_measurements[[sub_key]])) {
            sub_measurements[[sub_key]] <- list()
          }
          sub_measurements[[sub_key]] <- c(sub_measurements[[sub_key]], list(meas))
        }
      }

      # Build the description for this group
      group_parts <- character(0)

      # 1. Add main non-measurement values
      if (length(main_values) > 0) {
        group_parts <- c(group_parts, main_values)
      }

      # 2. Add main measurements (combined)
      if (length(main_measurements) > 0) {
        # Combine all measurements without sub-hierarchy
        len_val <- ""
        wid_val <- ""
        ht_val <- ""
        len_unit <- ""
        wid_unit <- ""
        ht_unit <- ""

        for (meas in main_measurements) {
          if (meas$is_length) {
            len_val <- meas$value
            len_unit <- meas$unit
          } else if (meas$is_width) {
            wid_val <- meas$value
            wid_unit <- meas$unit
          } else if (meas$is_height) {
            ht_val <- meas$value
            ht_unit <- meas$unit
          }
        }

        measurement_text <- .format_measurement_with_unit(len_val, wid_val, ht_val,
                                                          len_unit, wid_unit, ht_unit)
        if (nzchar(measurement_text)) {
          group_parts <- c(group_parts, measurement_text)
        }
      }

      # 3. Add subgroup non-measurement values
      if (length(subgroup_values) > 0) {
        # Get subgroup names in alphabetical order for consistency
        subgroup_names <- sort(names(subgroup_values))

        for (subname in subgroup_names) {
          sub_vals <- subgroup_values[[subname]]
          if (length(sub_vals) > 0) {
            group_parts <- c(group_parts, paste(subname, sub_vals))
          }
        }
      }

      # 4. Add measurements with sub-hierarchy (e.g., "style 4 mm long")
      if (length(sub_measurements) > 0) {
        for (sub_key in names(sub_measurements)) {
          sub_ms <- sub_measurements[[sub_key]]

          # Combine measurements for this sub-hierarchy
          len_val <- ""
          wid_val <- ""
          ht_val <- ""
          len_unit <- ""
          wid_unit <- ""
          ht_unit <- ""

          for (meas in sub_ms) {
            if (meas$is_length) {
              len_val <- meas$value
              len_unit <- meas$unit
            } else if (meas$is_width) {
              wid_val <- meas$value
              wid_unit <- meas$unit
            } else if (meas$is_height) {
              ht_val <- meas$value
              ht_unit <- meas$unit
            }
          }

          measurement_text <- .format_measurement_with_unit(len_val, wid_val, ht_val,
                                                            len_unit, wid_unit, ht_unit)
          if (nzchar(measurement_text)) {
            # Add the sub-hierarchy as prefix
            group_parts <- c(group_parts, paste(sub_key, measurement_text))
          }
        }
      }

      if (length(group_parts) > 0) {
        items <- c(items, paste(tolower(first_level), paste(group_parts, collapse = ", ")))
      }

      i <- i + 1

    } else {
      # No hierarchy - could be measurement or simple column
      if (info$is_length || info$is_width || info$is_height) {
        # Single measurement without hierarchy
        # Try to find related measurements using old logic
        base_name <- gsub("\\s*\\([^)]+\\)", "", cn)
        base_name <- gsub("\\s*(length|width|height|long|wide|tall).*", "", base_name, ignore.case = TRUE)
        base_name <- stringr::str_squish(base_name)

        related_indices <- integer(0)

        for (j in seq_along(cols)) {
          if (j == i || processed[j]) next

          other_cn <- cols[j]
          other_base <- gsub("\\s*\\([^)]+\\)", "", other_cn)
          other_base <- gsub("\\s*(length|width|height|long|wide|tall).*", "", other_base, ignore.case = TRUE)
          other_base <- stringr::str_squish(other_base)

          if (other_base == base_name &&
              (parsed_cols[[j]]$is_length || parsed_cols[[j]]$is_width || parsed_cols[[j]]$is_height) &&
              length(parsed_cols[[j]]$hierarchy) == 0) {
            related_indices <- c(related_indices, j)
          }
        }

        # Collect measurements
        len_val <- ""
        wid_val <- ""
        ht_val <- ""
        len_unit <- ""
        wid_unit <- ""
        ht_unit <- ""

        if (info$is_length) {
          len_val <- val
          len_unit <- info$unit
        } else if (info$is_width) {
          wid_val <- val
          wid_unit <- info$unit
        } else if (info$is_height) {
          ht_val <- val
          ht_unit <- info$unit
        }

        processed[i] <- TRUE

        for (j in related_indices) {
          if (!processed[j]) {
            related_info <- parsed_cols[[j]]
            related_val <- .cell_as_text(row[[cols[j]]][[1]])
            if (nzchar(related_val)) {
              if (related_info$is_length) {
                len_val <- related_val
                len_unit <- related_info$unit
              } else if (related_info$is_width) {
                wid_val <- related_val
                wid_unit <- related_info$unit
              } else if (related_info$is_height) {
                ht_val <- related_val
                ht_unit <- related_info$unit
              }
            }
            processed[j] <- TRUE
          }
        }

        measurement <- .format_measurement_with_unit(len_val, wid_val, ht_val, len_unit, wid_unit, ht_unit)
        if (nzchar(measurement)) {
          items <- c(items, measurement)
        }

      } else {
        # Simple non-measurement column without hierarchy
        items <- c(items, val)
        processed[i] <- TRUE
      }

      i <- i + 1
    }
  }

  items[nzchar(items)]
}

.extract_unit_from_colname <- function(col_name, fallback = "") {
  m <- stringr::str_match(col_name, "\\(([^)]+)\\)")
  if (!is.na(m[, 2]) && nzchar(m[, 2])) stringr::str_trim(m[, 2]) else fallback
}

.format_measurement_with_unit <- function(len_val, wid_val, ht_val, len_unit, wid_unit, ht_unit) {
  len_val <- stringr::str_squish(len_val)
  wid_val <- stringr::str_squish(wid_val)
  ht_val <- stringr::str_squish(ht_val)

  # Determine the common unit - prioritize length, then width, then height
  unit <- ""
  if (nzchar(len_unit)) unit <- len_unit
  else if (nzchar(wid_unit)) unit <- wid_unit
  else if (nzchar(ht_unit)) unit <- ht_unit

  if (nzchar(len_val) && nzchar(wid_val) && nzchar(ht_val)) {
    # All three dimensions
    return(paste0(len_val, " × ", wid_val, " × ", ht_val, if (nzchar(unit)) paste0(" ", unit) else ""))
  } else if (nzchar(len_val) && nzchar(wid_val)) {
    # Length and width
    return(paste0(len_val, " × ", wid_val, if (nzchar(unit)) paste0(" ", unit) else ""))
  } else if (nzchar(len_val) && nzchar(ht_val)) {
    # Length and height
    return(paste0(len_val, " × ", ht_val, if (nzchar(unit)) paste0(" ", unit) else ""))
  } else if (nzchar(wid_val) && nzchar(ht_val)) {
    # Width and height
    return(paste0(wid_val, " × ", ht_val, if (nzchar(unit)) paste0(" ", unit) else ""))
  } else if (nzchar(len_val)) {
    # Only length
    return(paste0(len_val, if (nzchar(unit)) paste0(" ", unit, " long") else " long"))
  } else if (nzchar(wid_val)) {
    # Only width
    return(paste0(wid_val, if (nzchar(unit)) paste0(" ", unit, " wide") else " wide"))
  } else if (nzchar(ht_val)) {
    # Only height
    return(paste0(ht_val, if (nzchar(unit)) paste0(" ", unit, " tall") else " tall"))
  } else {
    return("")
  }
}

.cell_as_text <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  if (length(x) > 1L) x <- x[!is.na(x)]
  if (length(x) == 0) return("")
  if (is.na(x[[1]])) return("")
  if (is.logical(x[[1]])) return("")
  if (inherits(x[[1]], "POSIXt")) return(format(x[[1]], "%Y-%m-%d"))

  if (is.numeric(x[[1]])) {
    if (length(x) == 1L && isTRUE(all.equal(x[[1]], round(x[[1]]))) && x[[1]] %in% 8:16) return("")
    if (isTRUE(all.equal(x[[1]], round(x[[1]])))) return(as.character(as.integer(round(x[[1]]))))
    return(format(x[[1]], trim = TRUE, scientific = FALSE))
  }

  txt <- stringr::str_squish(as.character(x[[1]]))
  if (!nzchar(txt)) return("")
  if (txt %in% c("TRUE", "FALSE", "black", "Arial", "baseline", "transparent")) return("")
  txt
}

#' Merge duplicate species rows with special handling for measurements
#'
#' This version handles measurement columns (those with "length", "width", "height")
#' differently by keeping ranges if they exist.
.merge_duplicate_species <- function(df, species_cols, character_cols) {

  # Store original column order
  original_cols <- names(df)

  # Create a temporary species identifier column
  df_temp <- df %>%
    tidyr::unite("_species_id", dplyr::all_of(species_cols), sep = " ", remove = FALSE, na.rm = TRUE)
  df_temp[[1]] <- barRoso::remove_authorship(df_temp[[1]])

  # Identify character columns
  char_cols <- intersect(character_cols, names(df))

  # Convert all character columns to character type to ensure consistency
  df_temp <- df_temp %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(char_cols), as.character))

  # Identify measurement columns
  is_measurement_col <- function(col_name) {
    col_lower <- tolower(col_name)
    any(stringr::str_detect(col_lower, c("length", "width", "height", "\\(cm\\)", "\\(mm\\)", "\\(m\\)")))
  }

  measurement_cols <- char_cols[sapply(char_cols, is_measurement_col)]
  non_measurement_cols <- setdiff(char_cols, measurement_cols)

  # Function to merge non-measurement values with proper punctuation
  merge_non_measurement <- function(x) {
    x_clean <- x[!is.na(x) & nzchar(as.character(x))]

    if (length(x_clean) == 0) {
      return(as.character(NA))
    }

    # Convert all to character
    x_clean <- as.character(x_clean)
    unique_vals <- unique(x_clean)

    if (length(unique_vals) == 1) {
      return(unique_vals[1])
    } else {
      # Sort values
      unique_vals <- sort(unique_vals)

      # For 2 items: use "or"
      if (length(unique_vals) == 2) {
        return(paste(unique_vals, collapse = " or "))
      }
      # For 3 or more items: use commas and "or" before last item
      else if (length(unique_vals) >= 3) {
        all_but_last <- unique_vals[1:(length(unique_vals)-1)]
        last_item <- unique_vals[length(unique_vals)]
        return(paste(paste(all_but_last, collapse = ", "), "or", last_item))
      }
    }
  }

  # Function to merge measurement values with proper punctuation
  merge_measurement <- function(x) {
    x_clean <- x[!is.na(x) & nzchar(as.character(x))]

    if (length(x_clean) == 0) {
      return(as.character(NA))
    }

    # Convert to character
    x_clean <- as.character(x_clean)

    # Try to extract numeric values for ranges
    nums <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", x_clean)))
    nums <- nums[!is.na(nums)]

    if (length(nums) >= 2) {
      # Create range
      min_val <- min(nums)
      max_val <- max(nums)

      if (min_val == max_val) {
        return(as.character(min_val))
      } else {
        return(paste0(min_val, "–", max_val))
      }
    } else if (length(x_clean) == 1) {
      return(x_clean[1])
    } else {
      # Multiple non-numeric measurements
      unique_vals <- unique(x_clean)
      unique_vals <- sort(unique_vals)

      if (length(unique_vals) == 2) {
        return(paste(unique_vals, collapse = " or "))
      } else if (length(unique_vals) >= 3) {
        all_but_last <- unique_vals[1:(length(unique_vals)-1)]
        last_item <- unique_vals[length(unique_vals)]
        return(paste(paste(all_but_last, collapse = ", "), "or", last_item))
      }
    }
  }

  # Group and merge
  df_merged <- df_temp %>%
    dplyr::group_by(`_species_id`) %>%
    dplyr::summarise(
      # Species columns (take first, ensure character type)
      dplyr::across(dplyr::all_of(species_cols), ~as.character(dplyr::first(na.omit(.)))),
      # Non-measurement columns
      dplyr::across(dplyr::all_of(non_measurement_cols),
             ~merge_non_measurement(.)),
      # Measurement columns
      dplyr::across(dplyr::all_of(measurement_cols),
             ~merge_measurement(.)),
      .groups = "drop"
    ) %>%
    dplyr::select(-`_species_id`)

  for (i in seq_along(measurement_cols)) {
    tf <- grepl(",|or", df_merged[[measurement_cols[i]]])
    if (any(tf)) {
      df_merged[[measurement_cols[i]]] <- .simplify_measurement_range(df_merged[[measurement_cols[i]]])
    }
  }

  # Ensure final order matches original
  df_merged <- df_merged[, original_cols]

  return(df_merged)
}


.simplify_measurement_range <- function(x, sep = "–", na_string = NA_character_) {

  # Vectorized function
  sapply(x, function(str) {

    # Handle NA and empty strings
    if (is.na(str) || !nzchar(trimws(str))) {
      return(na_string)
    }

    # Extract all numbers from the string (handles decimals)
    # This regex captures numbers including: 1.4, 5.6, 3, 7, etc.
    numbers <- regmatches(str, gregexpr("-?\\d+\\.?\\d*", str))[[1]]

    # Convert to numeric
    nums <- as.numeric(numbers)

    # Remove any NA values that might have been created
    nums <- nums[!is.na(nums)]

    # If no numbers found, return the original string or NA
    if (length(nums) == 0) {
      return(na_string)
    }

    # Calculate min and max
    min_val <- min(nums)
    max_val <- max(nums)

    # If min and max are the same, just return the value
    if (min_val == max_val) {
      return(as.character(min_val))
    }

    # Return as "min–max"
    paste0(min_val, sep, max_val)

  }, USE.NAMES = FALSE)
}
