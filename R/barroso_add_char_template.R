#' Append a blank morphological-character template to a species table.
#'
#' @author Domingos Cardoso
#'
#' @description Given a \code{data.frame} of species (one row per species, or
#'   per specimen) OR a path to an Excel file containing such data, this
#'   function writes an \code{.xlsx} file whose columns begin with whatever
#'   identification columns are in \code{species_df} — Genus/Species/Author,
#'   Family/Taxon, etc. — and continue with a standard morphological-character
#'   template (HABIT, STIPULE, LEAF, INFLORESCENCE, FLOWER, FRUIT, SEED) left
#'   empty for the user to fill in.
#'
#'   When \code{plant_group} is specified, INFLORESCENCE/FLOWER/FRUIT blocks
#'   are replaced with the morphology that group actually has — e.g.
#'   Leguminosae-Papilionoideae swaps generic petals for STANDARD / WING /
#'   KEEL and adds stamen arrangement and fruit dehiscence; Asteraceae
#'   replaces the inflorescence with capitulum + involucre and the flower
#'   block with RAY / DISC florets + pappus; Ochnaceae uses the full
#'   carpophore/mericarp fruit and seed-apical-hook block; Orchidaceae uses
#'   dorsal/lateral sepals, labellum and column. Supported groups:
#'   \itemize{
#'     \item "Leguminosae-Papilionoideae"
#'     \item "Leguminosae-Caesalpinioideae"
#'     \item "Leguminosae-Mimosoideae"
#'     \item "Asteraceae"
#'     \item "Ochnaceae"
#'     \item "Orchidaceae"
#'   }
#'
#'   The resulting file is the input format expected by
#'   \code{barroso_write_taxon_descr()}: column-name conventions match
#'   (capitalised group prefixes, lowercase subterms, "(cm)" / "(mm)" /
#'   "(m)" units, "length" / "width" / "height" keywords).
#'
#' @param species_df Either a \code{data.frame} containing the species-identification
#'   columns you want preserved (e.g. Genus, Species, Author), OR a character
#'   string path to an Excel file (.xlsx) containing such data. Every column
#'   in \code{species_df} is copied into the output as-is; template columns
#'   are appended after them and written empty. Any template column whose
#'   name already exists in \code{species_df} is skipped, so user data is
#'   never overwritten.
#' @param sheet Character string or integer. Sheet name or index to read when
#'   \code{species_df} is a path to an Excel file. Default is 1.
#' @param plant_group Optional character. One of the supported profiles
#'   listed above. \code{NULL} (default) uses the generic flowering-plant
#'   profile.
#' @param species_cols Character vector or integer indices. Column names or indices
#'   used to build the species header (e.g., c("Genus", "Species", "Author") or 1:3).
#'   These can be combined flexibly (e.g., a single "Scientific_name" column or
#'   separate Genus, Species, and Author columns).
#' @param overwrite Logical. If TRUE (default), an existing file at the
#'   target path is overwritten; if FALSE, the function errors out.
#' @param format_excel Logical. If TRUE (default), the function will use more
#'   user-friendly format for the spreadsheet.
#' @param sheet_name Character. Name of the worksheet. Default "described_specimens".
#' @param base_font_size Numeric. Base font size. Default 10.
#' @param base_font_name Character. Base font name. Default "Calibri Light".
#' @param verbose Logical. Print progress messages. Default TRUE.
#' @param filename Output file base name (with or without ".xlsx"). If NULL,
#'   uses "taxon_template" or "<plant_group>_template" when a group is given.
#' @param dir Output directory. If NULL, a directory named after
#'   \code{filename} (without extension) is created in the working
#'   directory. A date-stamped subfolder is created inside.
#'
#' @return Invisibly returns the absolute path to the written .xlsx file.
#'
#' @examples
#' \dontrun{
#' # Using a data frame
#' species_df <- data.frame(
#'   Genus   = c("Ouratea", "Ouratea", "Ouratea"),
#'   Species = c("acicularis", "acuminata", "cassinefolia"),
#'   Author  = c("R.G.Chacon & K.Yamam.", "(DC.) Engl.", "(DC.) Engl.")
#' )
#'
#' barroso_add_char_template(
#'   species_df = species_df,
#'   filename   = "Ouratea_template"
#' )
#'
#' # Using an Excel file path
#' barroso_add_char_template(
#'   species_df = "my_species_list.xlsx",
#'   sheet = 1,
#'   plant_group = "Ochnaceae",
#'   filename = "Ouratea_Ochnaceae_template"
#' )
#' }
#'
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_excel
#' @importFrom tools file_path_sans_ext file_ext
#'
#' @export
barroso_add_char_template <- function(species_df,
                                      sheet = 1,
                                      plant_group = NULL,
                                      species_cols = NULL,
                                      overwrite = TRUE,
                                      format_excel = TRUE,
                                      sheet_name = "described_specimens",
                                      base_font_size = 10,
                                      base_font_name = "Calibri Light",
                                      verbose = TRUE,
                                      filename = NULL,
                                      dir = NULL) {

  # ---- Input validation ------------------------------------------------------
  # Check if species_df is a data frame or a file path
  if (is.data.frame(species_df)) {
    if (nrow(species_df) == 0L) {
      stop("species_df has no rows.", call. = FALSE)
    }
    df <- species_df
    if (verbose) {
      message("Using provided data frame with ", nrow(df), " rows.")
    }
  } else if (is.character(species_df) && length(species_df) == 1) {
    # Check if it's a file path
    if (!file.exists(species_df)) {
      stop("File not found: ", species_df, call. = FALSE)
    }

    # Check file extension
    ext <- tolower(tools::file_ext(species_df))
    if (!ext %in% c("xlsx", "xls")) {
      stop("species_df file must be an Excel file (.xlsx or .xls). Got: ", ext, call. = FALSE)
    }

    # Read the Excel file
    if (verbose) {
      message("Reading Excel file: ", species_df)
      if (is.numeric(sheet)) {
        message("  Sheet index: ", sheet)
      } else {
        message("  Sheet name: ", sheet)
      }
    }

    df <- readxl::read_excel(path = species_df, sheet = sheet)

    if (nrow(df) == 0L) {
      stop("The Excel file has no rows.", call. = FALSE)
    }

    if (verbose) {
      message("Successfully read ", nrow(df), " rows and ", ncol(df), " columns.")
    }
  } else {
    stop("species_df must be either a data.frame or a character string path to an Excel file.",
         call. = FALSE)
  }

  input_cols <- names(df)

  # Allow user to pass indices or names
  if (!is.null(species_cols) && length(species_cols) > 0) {
    species_cols <- .resolve_cols(df, species_cols, arg = "species_cols")
  } else {
    # If no species_cols provided, use all input columns (or throw warning)
    species_cols <- input_cols
    if (verbose) {
      message("No species_cols specified, using all input columns as species identifiers.")
    }
  }
  # Check for duplicate column names
  if (any(duplicated(input_cols))) {
    dupes <- paste(unique(input_cols[duplicated(input_cols)]), collapse = ", ")
    stop("species_df contains duplicate column names: ", dupes, call. = FALSE)
  }

  # ---- Pick the column set for the chosen profile ----------------------------
  template_cols <- .taxon_template_columns(plant_group)

  # Never overwrite a user column: drop template names already in species_df.
  collisions <- intersect(input_cols, template_cols)
  if (length(collisions) > 0L) {
    template_cols <- setdiff(template_cols, collisions)
    if (isTRUE(verbose)) {
      message("Skipping template columns already present in species_df: ",
              paste(collisions, collapse = ", "))
    }
  }

  # ---- Assemble the output data.frame ----------------------------------------
  out <- df
  for (cn in template_cols) {
    out[[cn]] <- NA_character_
  }

  # ---- Extract trait groups for coloring ------------------------------------
  trait_groups <- .extract_trait_groups(template_cols, plant_group)

  # ---- Resolve output path ---------------------------------------------------
  if (is.null(filename)) {
    base <- if (is.null(plant_group)) "taxon_template" else
      paste0(gsub("[^A-Za-z0-9._-]+", "_", plant_group), "_template")
  } else {
    base <- tools::file_path_sans_ext(basename(filename))
  }

  if (is.null(dir)) dir <- base
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  foldername <- file.path(dir, format(Sys.time(), "%d%b%Y"))
  if (!dir.exists(foldername)) dir.create(foldername, recursive = TRUE)

  out_path <- file.path(foldername, paste0(base, ".xlsx"))

  if (file.exists(out_path) && !overwrite) {
    stop("File already exists and overwrite = FALSE: ", out_path, call. = FALSE)
  }

  # ---- Write to Excel with formatting ----------------------------------------
  if (format_excel) {
    # Use custom formatted writer
    .write_spreadsheet(
      data = out,
      species_cols = species_cols,
      trait_groups = trait_groups,
      sheet_name = sheet_name,
      filename = out_path,
      overwrite = overwrite,
      base_font_size = base_font_size,
      base_font_name = base_font_name
    )
  } else {
    # Use simple write.xlsx
    openxlsx::write.xlsx(out, file = out_path, overwrite = overwrite)
  }

  if (isTRUE(verbose)) {
    message(sprintf(
      "Wrote template: %s\n  rows: %d   input columns: %d   character columns: %d   profile: %s",
      out_path, nrow(out), length(input_cols), length(template_cols),
      if (is.null(plant_group)) "generic" else plant_group
    ))
  }

  invisible(normalizePath(out_path, winslash = "/", mustWork = FALSE))
}


# ==============================================================================
# Profile definitions
# ==============================================================================
#
# Profiles are built by composing reusable "blocks". Naming conventions
# (UPPERCASE group prefixes, lowercase trait words, "length" / "width" /
# "height" + "(unit)" for measurements) match exactly what
# barroso_write_taxon_descr() expects.
# ==============================================================================

#' Creates taxon-specific template
#' @keywords internal
#' @noRd
.taxon_template_columns <- function(plant_group = NULL) {

  supported <- c("Leguminosae-Papilionoideae",
                 "Leguminosae-Caesalpinioideae",
                 "Leguminosae-Mimosoideae",
                 "Asteraceae",
                 "Ochnaceae",
                 "Orchidaceae")

  if (!is.null(plant_group)) {
    if (length(plant_group) != 1L || !is.character(plant_group)) {
      stop("plant_group must be a single character string or NULL.", call. = FALSE)
    }
    if (!plant_group %in% supported) {
      stop("Unknown plant_group: '", plant_group, "'. Supported: ",
           paste(supported, collapse = ", "), ".", call. = FALSE)
    }
  }

  # ----------------------------------------------------------------------------
  # Generic shared blocks
  # ----------------------------------------------------------------------------
  habit_block <- c(
    "HABIT",
    "HABIT.height.(m)",
    "HABIT.branch.growth",
    "HABIT.branching.pattern",
    "HABIT.STEM.bark",
    "HABIT.STEM.color",
    "HABIT.STEM.indument"
  )

  stipule_block <- c(
    "STIPULE.number",
    "STIPULE.position",
    "STIPULE.persistence",
    "STIPULE.shape",
    "STIPULE.length.(mm)",
    "STIPULE.width.(mm)",
    "STIPULE.BASE",
    "STIPULE.MARGIN",
    "STIPULE.APEX",
    "STIPULE.indument"
  )

  leaf_block <- c(
    "LEAF.phyllotaxis",
    "LEAF.arrangement.on.branch",
    "LEAF.PETIOLE.shape",
    "LEAF.PETIOLE.indument",
    "LEAF.PETIOLE.length.(cm)",
    "LEAF.PETIOLE.width.(cm)",
    "LEAF.BLADE.brightness",
    "LEAF.BLADE.texture",
    "LEAF.BLADE.shape",
    "LEAF.BLADE.length.(cm)",
    "LEAF.BLADE.width.(cm)",
    "LEAF.BLADE.BASE",
    "LEAF.BLADE.APEX",
    "LEAF.MARGINS",
    "LEAF.venation",
    "LEAF.MIDVEIN",
    "LEAF.indument"
  )

  inflorescence_block <- c(
    "INFLORESCENCE.type",
    "INFLORESCENCE.position",
    "INFLORESCENCE.length.(cm)",
    "INFLORESCENCE.width.(cm)",
    "INFLORESCENCE.flower.number",
    "INFLORESCENCE.AXES.indument",
    "INFLORESCENCE.PEDUNCLE.length.(cm)",
    "INFLORESCENCE.PEDUNCLE.width.(cm)",
    "INFLORESCENCE.PEDUNCLE.indument",
    "INFLORESCENCE.BRACT",
    "INFLORESCENCE.BRACT.shape",
    "INFLORESCENCE.BRACT.length.(mm)",
    "INFLORESCENCE.BRACT.width.(mm)"
  )

  flower_block <- c(
    "FLOWER.symmetry",
    "FLOWER.PEDICEL.indument",
    "FLOWER.PEDICEL.length.(mm)",
    "FLOWER.PEDICEL.width.(mm)",
    "FLOWER.SEPALS.number",
    "FLOWER.SEPALS.arrangement",
    "FLOWER.SEPALS.color",
    "FLOWER.SEPALS.indument",
    "FLOWER.SEPALS.shape",
    "FLOWER.SEPALS.length.(mm)",
    "FLOWER.SEPALS.width.(mm)",
    "FLOWER.SEPALS.BASE",
    "FLOWER.SEPALS.APEX",
    "FLOWER.SEPALS.persistence",
    "FLOWER.PETALS.number",
    "FLOWER.PETALS.color",
    "FLOWER.PETALS.indument",
    "FLOWER.PETALS.shape",
    "FLOWER.PETALS.length.(mm)",
    "FLOWER.PETALS.width.(mm)",
    "FLOWER.PETALS.BASE",
    "FLOWER.PETALS.APEX",
    "FLOWER.STAMEN.number",
    "FLOWER.STAMEN.filament",
    "FLOWER.STAMEN.ANTHERS.color",
    "FLOWER.STAMEN.ANTHERS.indument",
    "FLOWER.STAMEN.ANTHERS.dehiscence",
    "FLOWER.STAMEN.ANTHERS.length.(mm)",
    "FLOWER.STAMEN.ANTHERS.width.(mm)",
    "FLOWER.GYNOECIUM.position",
    "FLOWER.GYNOECIUM.length.(mm)",
    "FLOWER.GYNOECIUM.indument",
    "FLOWER.GYNOECIUM.CARPELS",
    "FLOWER.GYNOECIUM.STYLE.length.(mm)",
    "FLOWER.GYNOECIUM.STIGMA"
  )

  fruit_block <- c(
    "FRUIT.type",
    "FRUIT.color",
    "FRUIT.shape",
    "FRUIT.indument",
    "FRUIT.length.(cm)",
    "FRUIT.width.(cm)"
  )

  seed_block <- c(
    "SEED.number",
    "SEED.shape",
    "SEED.color",
    "SEED.length.(mm)",
    "SEED.width.(mm)"
  )

  # ----------------------------------------------------------------------------
  # Leguminosae flower / legume-pod fruit blocks
  # ----------------------------------------------------------------------------
  papilionoid_flower_block <- c(
    "FLOWER.symmetry",
    "FLOWER.PEDICEL.length.(mm)",
    "FLOWER.PEDICEL.indument",
    "FLOWER.CALYX.tube.length.(mm)",
    "FLOWER.CALYX.indument",
    "FLOWER.SEPALS.number",
    "FLOWER.SEPALS.shape",
    "FLOWER.SEPALS.length.(mm)",
    "FLOWER.SEPALS.width.(mm)",
    "FLOWER.STANDARD.color",
    "FLOWER.STANDARD.shape",
    "FLOWER.STANDARD.indument",
    "FLOWER.STANDARD.length.(mm)",
    "FLOWER.STANDARD.width.(mm)",
    "FLOWER.STANDARD.BASE",
    "FLOWER.STANDARD.APEX",
    "FLOWER.WING.color",
    "FLOWER.WING.shape",
    "FLOWER.WING.indument",
    "FLOWER.WING.length.(mm)",
    "FLOWER.WING.width.(mm)",
    "FLOWER.KEEL.color",
    "FLOWER.KEEL.shape",
    "FLOWER.KEEL.indument",
    "FLOWER.KEEL.length.(mm)",
    "FLOWER.KEEL.width.(mm)",
    "FLOWER.STAMEN.number",
    "FLOWER.STAMEN.arrangement",
    "FLOWER.STAMEN.filament.length.(mm)",
    "FLOWER.STAMEN.ANTHERS.color",
    "FLOWER.STAMEN.ANTHERS.dehiscence",
    "FLOWER.STAMEN.ANTHERS.length.(mm)",
    "FLOWER.GYNOECIUM.position",
    "FLOWER.GYNOECIUM.OVARY.length.(mm)",
    "FLOWER.GYNOECIUM.OVARY.indument",
    "FLOWER.GYNOECIUM.STYLE.length.(mm)",
    "FLOWER.GYNOECIUM.STYLE.curvature",
    "FLOWER.GYNOECIUM.STIGMA"
  )

  caesalpinioid_flower_block <- c(
    "FLOWER.symmetry",
    "FLOWER.PEDICEL.indument",
    "FLOWER.PEDICEL.length.(mm)",
    "FLOWER.HYPANTHIUM.length.(mm)",
    "FLOWER.HYPANTHIUM.indument",
    "FLOWER.SEPALS.number",
    "FLOWER.SEPALS.arrangement",
    "FLOWER.SEPALS.color",
    "FLOWER.SEPALS.shape",
    "FLOWER.SEPALS.length.(mm)",
    "FLOWER.SEPALS.width.(mm)",
    "FLOWER.SEPALS.indument",
    "FLOWER.PETALS.number",
    "FLOWER.PETALS.color",
    "FLOWER.PETALS.shape",
    "FLOWER.PETALS.length.(mm)",
    "FLOWER.PETALS.width.(mm)",
    "FLOWER.PETALS.indument",
    "FLOWER.STAMEN.number",
    "FLOWER.STAMEN.arrangement",
    "FLOWER.STAMEN.filament.length.(mm)",
    "FLOWER.STAMEN.ANTHERS.color",
    "FLOWER.STAMEN.ANTHERS.dehiscence",
    "FLOWER.STAMEN.ANTHERS.length.(mm)",
    "FLOWER.STAMINODES.number",
    "FLOWER.GYNOECIUM.position",
    "FLOWER.GYNOECIUM.OVARY.length.(mm)",
    "FLOWER.GYNOECIUM.OVARY.indument",
    "FLOWER.GYNOECIUM.STYLE.length.(mm)",
    "FLOWER.GYNOECIUM.STIGMA"
  )

  mimosoid_flower_block <- c(
    "FLOWER.symmetry",
    "FLOWER.PEDICEL.length.(mm)",
    "FLOWER.sessile",
    "FLOWER.CALYX.length.(mm)",
    "FLOWER.CALYX.indument",
    "FLOWER.SEPALS.number",
    "FLOWER.SEPALS.fusion",
    "FLOWER.COROLLA.length.(mm)",
    "FLOWER.COROLLA.color",
    "FLOWER.PETALS.number",
    "FLOWER.PETALS.fusion",
    "FLOWER.PETALS.indument",
    "FLOWER.STAMEN.number",
    "FLOWER.STAMEN.color",
    "FLOWER.STAMEN.filament.length.(mm)",
    "FLOWER.STAMEN.filament.fusion",
    "FLOWER.STAMEN.exsertion",
    "FLOWER.STAMEN.ANTHERS.gland",
    "FLOWER.STAMEN.ANTHERS.length.(mm)",
    "FLOWER.GYNOECIUM.position",
    "FLOWER.GYNOECIUM.OVARY.length.(mm)",
    "FLOWER.GYNOECIUM.OVARY.indument",
    "FLOWER.GYNOECIUM.STYLE.length.(mm)",
    "FLOWER.GYNOECIUM.STIGMA"
  )

  legume_fruit_block <- c(
    "FRUIT.type",
    "FRUIT.dehiscence",
    "FRUIT.color",
    "FRUIT.shape",
    "FRUIT.indument",
    "FRUIT.length.(cm)",
    "FRUIT.width.(cm)",
    "FRUIT.constrictions",
    "FRUIT.valves"
  )

  # ----------------------------------------------------------------------------
  # Asteraceae blocks (capitulum / florets / pappus)
  # ----------------------------------------------------------------------------
  asteraceae_inflorescence_block <- c(
    "INFLORESCENCE.CAPITULUM.arrangement",
    "INFLORESCENCE.CAPITULUM.shape",
    "INFLORESCENCE.CAPITULUM.diameter.(mm)",
    "INFLORESCENCE.PEDUNCLE.length.(cm)",
    "INFLORESCENCE.PEDUNCLE.indument",
    "INFLORESCENCE.INVOLUCRE.shape",
    "INFLORESCENCE.INVOLUCRE.height.(mm)",
    "INFLORESCENCE.INVOLUCRE.diameter.(mm)",
    "INFLORESCENCE.PHYLLARIES.series",
    "INFLORESCENCE.PHYLLARIES.shape",
    "INFLORESCENCE.PHYLLARIES.length.(mm)",
    "INFLORESCENCE.PHYLLARIES.indument",
    "INFLORESCENCE.RECEPTACLE.shape",
    "INFLORESCENCE.RECEPTACLE.paleae"
  )

  asteraceae_floret_block <- c(
    "FLORET.RAY.presence",
    "FLORET.RAY.number",
    "FLORET.RAY.color",
    "FLORET.RAY.shape",
    "FLORET.RAY.length.(mm)",
    "FLORET.RAY.width.(mm)",
    "FLORET.RAY.indument",
    "FLORET.DISC.number",
    "FLORET.DISC.color",
    "FLORET.DISC.COROLLA.shape",
    "FLORET.DISC.COROLLA.length.(mm)",
    "FLORET.DISC.COROLLA.lobes",
    "FLORET.DISC.STAMEN.color",
    "FLORET.DISC.STYLE.branches",
    "FLORET.PAPPUS.type",
    "FLORET.PAPPUS.length.(mm)",
    "FLORET.PAPPUS.color",
    "FLORET.PAPPUS.persistence"
  )

  asteraceae_fruit_block <- c(
    "FRUIT.type",
    "FRUIT.shape",
    "FRUIT.color",
    "FRUIT.indument",
    "FRUIT.length.(mm)",
    "FRUIT.width.(mm)",
    "FRUIT.ribs"
  )

  # ----------------------------------------------------------------------------
  # Ochnaceae block (full Ouratea/Ochnaceae 115-column set, cols 27-141 of
  # the user's reference spreadsheet). Includes carpophore + mericarp fruit
  # block, seed apical hook, BUDS, BRACT longitudinal venation, secondary/
  # tertiary venation, etc.
  # ----------------------------------------------------------------------------
  ochnaceae_block <- c(
    # HABIT
    "HABIT",
    "HABIT.height.(m)",
    "HABIT.branch.growth",
    "HABIT.branching.pattern",
    "HABIT.STEM.Bark",
    "HABIT.STEM.color",
    "HABIT.STEM.indument",
    # STIPULE
    "STIPULE.number",
    "STIPULE.position",
    "STIPULE.persistence",
    "STIPULE.length.(mm)",
    "STIPULE.width.(mm)",
    "STIPULE.shape",
    "STIPULE.BASE",
    "STIPULE.MARGIN",
    "STIPULE.APEX",
    "STIPULE.indument",
    # LEAF
    "LEAF.phyllotaxis",
    "LEAF.arrangement.on.branch",
    "LEAF.PETIOLE.shape",
    "LEAF.PETIOLE.Striations",
    "LEAF.PETIOLE.indument",
    "LEAF.PETIOLE.length.(cm)",
    "LEAF.PETIOLE.width.(cm)",
    "LEAF.BLADE.style",
    "LEAF.BLADE.brightness",
    "LEAF.BLADE.texture",
    "LEAF.BLADE.shape",
    "LEAF.BLADE.length.(cm)",
    "LEAF.BLADE.width.(cm)",
    "LEAF.BLADE.BASE",
    "LEAF.BLADE.APEX",
    "LEAF.MARGINS",
    "LEAF.MARGINS.percentual.serrate",
    "LEAF.MARGINS.edges",
    "LEAF.venation",
    "LEAF.SECONDARY-VENATION",
    "LEAF.SECONDARY-VENATION.visibility",
    "LEAF.TERTIARY-VENATION",
    "LEAF.TERTIARY-VENATION.visibility",
    "LEAF.MIDVEIN",
    "LEAF.indument",
    # INFLORESCENCE
    "INFLORESCENCE.type",
    "INFLORESCENCE.size",
    "INFLORESCENCE.position",
    "INFLORESCENCE.further.notes",
    "INFLORESCENCE.length.(cm)",
    "INFLORESCENCE.width.(cm)",
    "INFLORESCENCE.flower.number",
    "INFLORESCENCE.AXES.indument",
    "INFLORESCENCE.PEDUNCLE",
    "INFLORESCENCE.PEDUNCLE.length.(cm)",
    "INFLORESCENCE.PEDUNCLE.width.(cm)",
    "INFLORESCENCE.PEDUNCLE.indument",
    "INFLORESCENCE.BRACT",
    "INFLORESCENCE.BRACT.distribution",
    "INFLORESCENCE.BRACT.shape",
    "INFLORESCENCE.BRACT.length.(mm)",
    "INFLORESCENCE.BRACT.width.(mm)",
    "INFLORESCENCE.BRACT.LONGITUDINAL-VENATION",
    "INFLORESCENCE.BRACT.MARGINS",
    # FLOWER
    "FLOWER.BUDS.length.(mm)",
    "FLOWER.symmetry",
    "FLOWER.PEDICEL.indument",
    "FLOWER.PEDICEL.length.(mm)",
    "FLOWER.PEDICEL.width.(mm)",
    "FLOWER.SEPALS.arrangement",
    "FLOWER.SEPALS.color",
    "FLOWER.SEPALS.indument",
    "FLOWER.SEPALS.shape",
    "FLOWER.SEPALS.length.(mm)",
    "FLOWER.SEPALS.width.(mm)",
    "FLOWER.SEPALS.BASE",
    "FLOWER.SEPALS.APEX",
    "FLOWER.SEPALS.margin",
    "FLOWER.SEPALS.persistence",
    "FLOWER.PETALS.number",
    "FLOWER.PETALS.arrangement",
    "FLOWER.PETALS.color",
    "FLOWER.PETALS.texture",
    "FLOWER.PETALS.indument",
    "FLOWER.PETALS.shape",
    "FLOWER.PETALS.length.(mm)",
    "FLOWER.PETALS.width.(mm)",
    "FLOWER.PETALS.BASE",
    "FLOWER.PETALS.APEX",
    "FLOWER.STAMEN.number",
    "FLOWER.STAMEN.filament",
    "FLOWER.STAMEN.ANTHERS.color",
    "FLOWER.STAMEN.ANTHERS.texture",
    "FLOWER.STAMEN.ANTHERS.indument",
    "FLOWER.STAMEN.ANTHERS.compartments",
    "FLOWER.STAMEN.ANTHERS.dehiscence",
    "FLOWER.STAMEN.ANTHERS.length.(mm)",
    "FLOWER.STAMEN.ANTHERS.width.(mm)",
    "FLOWER.GYNOECIUM.position",
    "FLOWER.GYNOECIUM.length.(mm)",
    "FLOWER.GYNOECIUM.indument",
    "FLOWER.GYNOECIUM.CARPELS",
    "FLOWER.GYNOECIUM.STYLE.length.(mm)",
    "FLOWER.GYNOECIUM.STYLE.color",
    "FLOWER.GYNOECIUM.STIGMA",
    # FRUIT
    "FRUIT.known",
    "FRUIT.length.(cm)",
    "FRUIT.CARPOPHORE.form",
    "FRUIT.CARPOPHORE.color",
    "FRUIT.CARPOPHORE.length.(cm)",
    "FRUIT.CARPOPHORE.width.(cm)",
    "FRUIT.MERICARP.number",
    "FRUIT.MERICARP.length.(mm)",
    "FRUIT.MERICARP.width.(mm)",
    "FRUIT.MERICARP.form",
    # SEED
    "SEED.length.(mm)",
    "SEED.width.(mm)",
    "SEED.apical.hook"
  )

  # ----------------------------------------------------------------------------
  # Orchidaceae blocks (zygomorphic with labellum + column).
  # Dorsal/lateral sepals nested *under* SEPALS so the parser treats them
  # as sub-groups (length+width combine within each, output reads
  # "sepals dorsal 3 × 1 mm, lateral 4 × 1.2 mm"). Avoid underscore in
  # token names — .extract_hierarchy() accepts A-Z and hyphen only, so
  # "DORSAL_SEPAL" would fall out of its group.
  # ----------------------------------------------------------------------------
  orchidaceae_flower_block <- c(
    "FLOWER.symmetry",
    "FLOWER.PEDICEL.length.(mm)",
    "FLOWER.OVARY.length.(mm)",
    "FLOWER.OVARY.indument",
    "FLOWER.SEPALS.color",
    "FLOWER.SEPALS.indument",
    "FLOWER.SEPALS.DORSAL.shape",
    "FLOWER.SEPALS.DORSAL.length.(mm)",
    "FLOWER.SEPALS.DORSAL.width.(mm)",
    "FLOWER.SEPALS.LATERAL.shape",
    "FLOWER.SEPALS.LATERAL.length.(mm)",
    "FLOWER.SEPALS.LATERAL.width.(mm)",
    "FLOWER.PETALS.color",
    "FLOWER.PETALS.shape",
    "FLOWER.PETALS.length.(mm)",
    "FLOWER.PETALS.width.(mm)",
    "FLOWER.LABELLUM.color",
    "FLOWER.LABELLUM.shape",
    "FLOWER.LABELLUM.length.(mm)",
    "FLOWER.LABELLUM.width.(mm)",
    "FLOWER.LABELLUM.lobes",
    "FLOWER.LABELLUM.spur",
    "FLOWER.LABELLUM.callus",
    "FLOWER.COLUMN.color",
    "FLOWER.COLUMN.shape",
    "FLOWER.COLUMN.length.(mm)",
    "FLOWER.COLUMN.foot",
    "FLOWER.COLUMN.POLLINIA.number",
    "FLOWER.COLUMN.STIGMA.shape"
  )

  # ----------------------------------------------------------------------------
  # Profile composition
  # ----------------------------------------------------------------------------
  if (is.null(plant_group)) {
    return(c(
      habit_block,
      stipule_block,
      leaf_block,
      inflorescence_block,
      flower_block,
      fruit_block,
      seed_block
    ))
  }

  switch(plant_group,
    "Leguminosae-Papilionoideae" = c(
      habit_block,
      stipule_block,
      leaf_block,
      inflorescence_block,
      papilionoid_flower_block,
      legume_fruit_block,
      seed_block
    ),
    "Leguminosae-Caesalpinioideae" = c(
      habit_block,
      stipule_block,
      leaf_block,
      inflorescence_block,
      caesalpinioid_flower_block,
      legume_fruit_block,
      seed_block
    ),
    "Leguminosae-Mimosoideae" = c(
      habit_block,
      stipule_block,
      leaf_block,
      inflorescence_block,
      mimosoid_flower_block,
      legume_fruit_block,
      seed_block
    ),
    "Asteraceae" = c(
      habit_block,
      leaf_block,
      asteraceae_inflorescence_block,
      asteraceae_floret_block,
      asteraceae_fruit_block
    ),
    "Ochnaceae" = ochnaceae_block,
    "Orchidaceae" = c(
      habit_block,
      leaf_block,
      inflorescence_block,
      orchidaceae_flower_block,
      fruit_block,
      seed_block
    )
  )
}


#' Extract trait groups from template columns for coloring
#' @keywords internal
#' @noRd
.extract_trait_groups <- function(template_cols, plant_group = NULL) {
  # Define group patterns based on column prefixes
  trait_groups <- list()

  # List of main trait blocks and their patterns
  block_definitions <- list(
    "HABIT" = "^HABIT",
    "STIPULE" = "^STIPULE",
    "LEAF" = "^LEAF",
    "INFLORESCENCE" = "^INFLORESCENCE",
    "FLOWER" = "^FLOWER",
    "FRUIT" = "^FRUIT",
    "SEED" = "^SEED"
  )

  # Add Asteraceae-specific blocks
  if (!is.null(plant_group) && plant_group == "Asteraceae") {
    block_definitions$INFLORESCENCE <- "^(INFLORESCENCE|CAPITULUM|INVOLUCRE|PHYLLARIES|RECEPTACLE)"
    block_definitions$FLORET <- "^(FLORET|RAY|DISC|PAPPUS)"
  }

  # Add Ochnaceae-specific blocks (carpophore and mericarp under fruit)
  if (!is.null(plant_group) && plant_group == "Ochnaceae") {
    block_definitions$FRUIT <- "^(FRUIT|CARPOPHORE|MERICARP)"
  }

  # Add Orchidaceae-specific blocks
  if (!is.null(plant_group) && plant_group == "Orchidaceae") {
    block_definitions$FLOWER <- "^(FLOWER|SEPALS|PETALS|LABELLUM|COLUMN)"
  }

  # For Leguminosae groups, add the specialized flower parts
  if (!is.null(plant_group) && grepl("Leguminosae", plant_group)) {
    block_definitions$FLOWER <- "^(FLOWER|STANDARD|WING|KEEL|CALYX|COROLLA)"
  }

  # Build trait groups by matching column names to patterns
  for (block_name in names(block_definitions)) {
    pattern <- block_definitions[[block_name]]
    matched_cols <- grep(pattern, template_cols, value = TRUE, perl = TRUE)
    if (length(matched_cols) > 0) {
      trait_groups[[block_name]] <- matched_cols
    }
  }

  # Also capture any columns that start with UPPERCASE words as potential blocks
  # This catches any custom blocks that might have been added via extra_cols
  all_cols <- template_cols
  potential_blocks <- unique(gsub("^([A-Z]+).*", "\\1", all_cols))
  for (pb in potential_blocks) {
    if (!pb %in% names(trait_groups) && nchar(pb) > 0) {
      matched <- grep(paste0("^", pb), all_cols, value = TRUE)
      if (length(matched) > 0) {
        trait_groups[[pb]] <- matched
      }
    }
  }

  return(trait_groups)
}

#' Write a formatted Excel spreadsheet with colored morphological trait blocks
#'
#' @description Creates a professionally formatted Excel file with color-coded
#'   blocks for each morphological trait group (HABIT, STIPULE, LEAF, etc.),
#'   with frozen header rows, auto-adjusted column widths based on header text,
#'   filter dropdowns, and clean formatting.
#'
#' @param data Data frame to write to Excel.
#' @param species_cols Character vector. Names of columns that identify the species
#'   (these will NOT be colored as trait blocks and will be frozen).
#' @param trait_groups Named list. Each element contains the column names for a
#'   specific trait group (e.g., HABIT, LEAF, FLOWER). Created by .extract_trait_groups().
#' @param sheet_name Character. Name of the worksheet. Default "described_specimens".
#' @param filename Character. Path where the Excel file will be saved.
#' @param overwrite Logical. Overwrite existing file? Default TRUE.
#' @param base_font_size Numeric. Base font size. Default 10.
#' @param base_font_name Character. Base font name. Default "Calibri Light".
#'
#' @return Invisibly returns the file path.
#'
#' @importFrom openxlsx createWorkbook addWorksheet modifyBaseFont writeData
#'   freezePane setColWidths saveWorkbook addStyle createStyle addFilter
#'
#' @keywords internal
#' @noRd
.write_spreadsheet <- function(data,
                               species_cols,
                               trait_groups,
                               sheet_name = "described_specimens",
                               filename,
                               overwrite = TRUE,
                               base_font_size = 10,
                               base_font_name = "Calibri Light") {

  # Default block colors for trait groups
  block_colors <- c(
    "HABIT" = "#E6F3FF",           # Light blue
    "STIPULE" = "#FFF0E6",         # Light orange
    "LEAF" = "#E6FFE6",            # Light green
    "INFLORESCENCE" = "#FFE6F0",   # Light pink
    "FLOWER" = "#FFE6CC",          # Light peach
    "FRUIT" = "#F0E6FF",           # Light purple
    "SEED" = "#FFFACD",            # Light yellow
    "FLORET" = "#FFDAB9"           # Peach puff for Asteraceae florets
  )

  # Create workbook
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetName = sheet_name)

  # Modify base font
  openxlsx::modifyBaseFont(wb, fontSize = base_font_size,
                           fontName = base_font_name,
                           fontColour = "black")

  # Create a mapping of column to its trait group
  col_to_group <- list()
  for (group_name in names(trait_groups)) {
    for (col in trait_groups[[group_name]]) {
      col_to_group[[col]] <- group_name
    }
  }

  col_names <- names(data)

  # Write ALL data in one go (much faster!)
  openxlsx::writeData(wb, sheet = sheet_name, x = data,
                      startCol = 1, startRow = 2, colNames = FALSE)

  # Write headers
  for (j in seq_along(col_names)) {
    openxlsx::writeData(wb, sheet = sheet_name, x = col_names[j],
                        startCol = j, startRow = 1)
  }

  # Create and apply styles by column group (apply to entire columns at once)
  # This is much faster than cell-by-cell styling

  # First, apply styles to header row
  for (j in seq_along(col_names)) {
    col_name <- col_names[j]

    if (col_name %in% species_cols) {
      style <- openxlsx::createStyle(
        fontSize = base_font_size + 1,
        fontColour = "#000000",
        fontName = base_font_name,
        fgFill = "#F0F0F0",
        halign = "left",
        valign = "center",
        textDecoration = "bold",
        border = "TopBottomLeftRight",
        borderColour = "#CCCCCC"
      )
    } else if (col_name %in% names(col_to_group)) {
      group_name <- col_to_group[[col_name]]
      color <- if (group_name %in% names(block_colors)) block_colors[group_name] else "#F5F5F5"
      style <- openxlsx::createStyle(
        fontSize = base_font_size + 1,
        fontColour = "#000000",
        fontName = base_font_name,
        fgFill = color,
        halign = "left",
        valign = "center",
        textDecoration = "bold",
        border = "TopBottomLeftRight",
        borderColour = "#CCCCCC"
      )
    } else {
      style <- openxlsx::createStyle(
        fontSize = base_font_size + 1,
        fontColour = "#000000",
        fontName = base_font_name,
        fgFill = "#FFFFFF",
        halign = "left",
        valign = "center",
        textDecoration = "bold",
        border = "TopBottomLeftRight",
        borderColour = "#CCCCCC"
      )
    }
    openxlsx::addStyle(wb, sheet = sheet_name, style = style,
                       rows = 1, cols = j, gridExpand = TRUE, stack = TRUE)
  }

  # Apply styles to data rows (apply to entire columns at once)
  if (nrow(data) > 0) {
    for (j in seq_along(col_names)) {
      col_name <- col_names[j]

      if (col_name %in% species_cols) {
        style <- openxlsx::createStyle(
          fontSize = base_font_size,
          fontColour = "#000000",
          fontName = base_font_name,
          fgFill = "#FFFFFF",
          halign = "left",
          valign = "center",
          wrapText = FALSE,
          border = "TopBottomLeftRight",
          borderColour = "#CCCCCC"
        )
      } else if (col_name %in% names(col_to_group)) {
        group_name <- col_to_group[[col_name]]
        color <- if (group_name %in% names(block_colors)) block_colors[group_name] else "#F5F5F5"
        style <- openxlsx::createStyle(
          fontSize = base_font_size,
          fontColour = "#000000",
          fontName = base_font_name,
          fgFill = color,
          halign = "left",
          valign = "center",
          wrapText = FALSE,
          border = "TopBottomLeftRight",
          borderColour = "#CCCCCC"
        )
      } else {
        style <- openxlsx::createStyle(
          fontSize = base_font_size,
          fontColour = "#000000",
          fontName = base_font_name,
          fgFill = "#FFFFFF",
          halign = "left",
          valign = "center",
          wrapText = FALSE,
          border = "TopBottomLeftRight",
          borderColour = "#CCCCCC"
        )
      }
      # Apply style to entire data column at once (rows 2 to n+1)
      openxlsx::addStyle(wb, sheet = sheet_name, style = style,
                         rows = 2:(nrow(data) + 1), cols = j,
                         gridExpand = TRUE, stack = TRUE)
    }
  }

  # Add filters to the header row
  if (nrow(data) > 0) {
    openxlsx::addFilter(wb, sheet = sheet_name, rows = 1, cols = 1:ncol(data))
  }

  # Freeze panes
  species_col_indices <- which(col_names %in% species_cols)

  if (length(species_col_indices) > 0) {
    last_species_col <- max(species_col_indices)
    if (last_species_col == 1) {
      openxlsx::freezePane(wb, sheet = sheet_name, firstRow = TRUE, firstCol = TRUE)
    } else {
      openxlsx::freezePane(wb, sheet = sheet_name,
                           firstActiveRow = 2,
                           firstActiveCol = last_species_col + 1)
    }
  } else {
    openxlsx::freezePane(wb, sheet = sheet_name, firstRow = TRUE, firstCol = FALSE)
  }

  # Auto-adjust column widths based on header text
  for (j in seq_len(ncol(data))) {
    header_text <- as.character(col_names[j])
    header_width <- nchar(header_text) + 4
    width <- min(max(header_width, 8), 50)
    openxlsx::setColWidths(wb, sheet = sheet_name, cols = j, widths = width)
  }

  # Save the workbook
  openxlsx::saveWorkbook(wb, filename, overwrite = overwrite)

  invisible(filename)
}

