#' Write lists of examined specimens in the format of a journal
#'
#' @author Domingos Cardoso
#'
#' @description Writes the lists of examined specimens of one, several or all
#'   species of a database of herbarium records in Darwin Core format, as
#'   standardized by [barroso_std()], into a Word (.docx) file, in the format of
#'   a botanical journal, e.g. Systematic Botany, Taxon, Kew Bulletin or Phytotaxa.
#'
#' @param df A data frame of herbarium records in Darwin Core format, e.g. the
#'   output of [barroso_std()] or [barroso_dwc()]. Not needed when `path` is given.
#' @param path Path to an .xlsx, .xls or .csv file with the herbarium records,
#'   used when `df` is `NULL`.
#' @param sheet Sheet name or index of the .xlsx file. Default 1.
#' @param species Character vector of species names, with or without authors
#'   (e.g. `"Ormosia arborea"`), to write the specimens of just these species.
#'   If `NULL` (default), the specimens of all species are written.
#' @param journal Name of the journal whose format is followed, as in recent
#'   papers of the journal: `"Systematic Botany"` (default),
#'   `"Acta Botanica Brasilica"`, `"Annals of the Missouri Botanical Garden"`,
#'   `"Botanical Journal of the Linnean Society"`,
#'   `"Brazilian Journal of Botany"`, `"Brittonia"`,
#'   `"Candollea"`, `"Edinburgh Journal of Botany"`, `"Kew Bulletin"`,
#'   `"Nordic Journal of Botany"`, `"Phytotaxa"`, `"PhytoKeys"`,
#'   `"Plant Ecology and Evolution"`,
#'   `"Rodriguesia"`, `"Systematics and Biodiversity"`, `"Taxon"` or `"Willdenowia"`. The name can
#'   be written in any case, with or without accents, and abbreviated
#'   (e.g. `"systematic botany"`, `"Kew"`, `"Edinburgh"`, `"Rodriguésia"`).
#' @param header Text of the header before the specimens, e.g.
#'   `"Additional specimens examined"`, `"Paratypes"` or `"Selected specimens"`,
#'   written with the format and punctuation of the journal. If `NULL` (default),
#'   the header of the journal, e.g. `"Representative Specimens Examined"` in
#'   Systematic Botany. Use `""` to write no header.
#' @param language Language of the section header, months and countries:
#'   `"en"` (default) or `"pt"`.
#' @param catalog_numbers Logical. If `TRUE`, the catalog number is written after
#'   each herbarium, e.g. `"(IAC no. 14754, NY no. 1162464)"` in Brittonia. Default `FALSE`.
#' @param dir Folder where the .docx file is saved. By default, the folder of
#'   `path`, or `"results_specimens"` when the records are given in `df`.
#' @param filename Name of the .docx file. By default, the name of `path` (or
#'   `"specimens"`) followed by the journal, e.g.
#'   `"Ormosia_herbarium_records_Systematic_Botany.docx"`.
#' @param font_family Font of the document. Default `"Times New Roman"`.
#' @param font_size Font size in points. Default 12.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @details
#' The records are formatted with [barroso_dwc()], so the columns may also come
#' from other sources, e.g. speciesLink. The columns used are `species`,
#' `country`, `stateProvince`, `municipality`, `locality`, `decimalLatitude`,
#' `decimalLongitude`, `minimumElevationInMeters`, `maximumElevationInMeters`,
#' `day`, `month`, `year`, `reproductiveCondition`, `recordedBy`, `addCollector`,
#' `recordNumber`, `collectionCode` and `catalogNumber`; and, when
#' present, the columns of [barroso_flag_duplicates()] with all herbaria of each
#' specimen (`duplicateCollectionCodes`, `duplicateCatalogNumbers`). The author
#' of each species comes from `scientificNameAuthorship`, `acceptedScientificName`
#' or `scientificName`.
#'
#' Duplicates of the same collection (same collector and number, or same
#' collector, date and locality when there is no number) are written once, with
#' all their herbaria. The specimens are ordered by country, state, municipality
#' and date.
#'
#' Examples of the format of each journal:
#'
#' \preformatted{
#' Systematic Botany: Representative Specimens Examined—Brazil. —Bahia: Abaíra,
#'   Catolés, 13°23′S, 41°48′W, 1200 m, 8 May 1994 (fl), D.B.O.S. Cardoso et al.
#'   2066 (HUEFS, RB). (header and states in small capitals, countries in bold,
#'   collectors and numbers in italics, as also in Taxon and Acta Botanica Brasilica)
#' Taxon: Additional specimens examined. – BRAZIL. BAHIA: Abaíra, Catolés,
#'   13°23′S, 41°48′W, 1200 m a.s.l., 8 May 1994 (fl), D.B.O.S. Cardoso & al. 2066
#'   (HUEFS, RB).
#' Kew Bulletin: SPECIMENS EXAMINED. BRAZIL. Bahia: Abaíra, Catolés, 13°23'S,
#'   41°48'W, 1200 m, 8 May 1994 (fl), D. B. O. S. Cardoso et al. 2066 (HUEFS, RB).
#' Phytotaxa: Additional specimens examined:―BRAZIL. Bahia: Abaíra: Catolés, ...,
#'   8 May 1994 (fl), D.B.O.S. Cardoso et al. 2066 (HUEFS, RB).
#' Brittonia: Additional specimens examined.—BRAZIL. Bahia: ..., 8 May 1994 [fl],
#'   D. B. O. S. Cardoso et al. 2066 (HUEFS, RB).
#' Candollea: Additional specimens examined. – Brazil. Bahia: ..., 13°23'S 41°48'W,
#'   1200 m, 8.V.1994, Cardoso et al. 2066 (HUEFS, RB).
#' Edinburgh Journal of Botany: Additional specimens examined. BRAZIL. Bahia: ...,
#'   8 v 1994 (fl), Cardoso et al. 2066 (HUEFS, RB).
#' Systematics and Biodiversity: Additional specimens examined. BRAZIL. Bahia: ...,
#'   8 May 1994, D. B. O. S. Cardoso et al. 2066 (HUEFS, RB).
#' Acta Botanica Brasilica: Additional specimens examined: BRAZIL. Bahia: ...,
#'   13°23’ S, 41°48’ W, 8 May 1994, (fl), D.B.O.S. Cardoso et al. 2066 (HUEFS!, RB!).
#' Plant Ecology and Evolution: Additional material examined. BRAZIL • Bahia, Abaíra,
#'   Catolés; 13°23′S, 41°48′W; 1200 m; 8 May 1994 (fl); Cardoso et al. 2066; HUEFS, RB.
#' PhytoKeys: Additional specimens examined. Brazil. Bahia: Mun. Abaíra, Catolés,
#'   13°23'S, 41°48'W, 1200 m alt., 8 May 1994, D.B.O.S. Cardoso et al. 2066 (HUEFS, RB).
#' Willdenowia: Brazil: Bahia: Municipality Abaíra, Catolés, 13°23'S, 41°48'W, 1200 m,
#'   8 May 1994, D. B. O. S. Cardoso & al. 2066 (HUEFS, RB); Serra do Barbado, ...
#'   Municipality Mucugê, ... . — Minas Gerais: ... (header once, before all species)
#' Annals of the Missouri Botanical Garden: Additional specimens examined. BRAZIL.
#'   Bahia: Abaíra, Catolés, 13°23′S, 41°48′W, 1200 m, 8 May 1994 (fl), D. B. O. S.
#'   Cardoso et al. 2066 (HUEFS, RB).
#' Botanical Journal of the Linnean Society: Representative specimens examined: BRAZIL.
#'   Bahia: Abaíra, Catolés, 13°23′S, 41°48′W, 1200 m, 8.v.1994, D.B.O.S. Cardoso et al.
#'   2066 (HUEFS, RB); Serra do Barbado, ...
#' Brazilian Journal of Botany: Selected additional specimens—BRAZIL. Bahia: ...,
#'   08 May 1994 (fl), Cardoso et al. 2066 (HUEFS, RB).
#' Nordic Journal of Botany: Ormosia arborea – Brazil, Bahia, Abaíra, Catolés, ...,
#'   8 May 1994, (fl), Cardoso et al. 2066 (HUEFS, RB). Mucugê, ...
#' Rodriguésia: Additional material examined: BRAZIL. BAHIA: Abaíra, Catolés, ...,
#'   8.V.1994, fl, D.B.O.S. Cardoso et al. 2066 (HUEFS, RB). Mucugê, ...,
#'   A. Fernandes (EAC 11356).
#' }
#'
#' @return Invisibly, a data frame with the species (`species_name`) and their
#'   examined specimens (`specimens_text`, one specimen per line). The .docx file
#'   is written as a side effect.
#'
#' @examples
#' \dontrun{
#' # All species of a database standardized by barRoso
#' barroso_write_specimens(path = "results/Ormosia_herbarium_records.xlsx",
#'                         journal = "Systematic Botany")
#'
#' # Two species, in the format of Kew Bulletin
#' barroso_write_specimens(df = data_all_clean,
#'                         species = c("Ormosia arborea", "Ormosia fastigiata"),
#'                         journal = "Kew Bulletin",
#'                         dir = "results")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom officer read_docx body_add_fpar fpar fp_text ftext
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom utils read.csv
#' @importFrom stringi stri_trans_general
#' @importFrom xml2 xml_find_all xml_set_name xml_attr<-
#'
#' @export
barroso_write_specimens <- function(df = NULL,
                                    path = NULL,
                                    sheet = 1,
                                    species = NULL,
                                    journal = c("Systematic Botany",
                                                "Acta Botanica Brasilica",
                                                "Annals of the Missouri Botanical Garden",
                                                "Botanical Journal of the Linnean Society",
                                                "Brazilian Journal of Botany",
                                                "Brittonia", "Candollea",
                                                "Edinburgh Journal of Botany",
                                                "Kew Bulletin",
                                                "Nordic Journal of Botany",
                                                "Phytotaxa",
                                                "Plant Ecology and Evolution",
                                                "PhytoKeys",
                                                "Willdenowia",
                                                "Rodriguesia",
                                                "Systematics and Biodiversity",
                                                "Taxon"),
                                    header = NULL,
                                    language = "en",
                                    catalog_numbers = FALSE,
                                    dir = NULL,
                                    filename = NULL,
                                    font_family = "Times New Roman",
                                    font_size = 12,
                                    verbose = TRUE) {

  # Records from a data frame or a file ####
  if (is.null(df)) {
    if (is.null(path)) {
      stop("Provide the records in `df` or the path to a file in `path`.", call. = FALSE)
    }
    df <- .read_records(path, sheet)
  }
  if (!is.data.frame(df)) stop("`df` must be a data frame.", call. = FALSE)
  if (!language %in% c("en", "pt")) stop("`language` must be \"en\" or \"pt\".", call. = FALSE)

  # The first journal by default, as in match.arg()
  if (length(journal) > 1) journal <- journal[1]
  style <- .journal_style(journal)

  # Darwin Core columns, as character, with the binomial in "species"
  df <- barroso_dwc(as.data.frame(df), verbose = FALSE)
  if (!"species" %in% names(df)) {
    stop("The records need a column `species` or the columns `genus` and `specificEpithet`.",
         call. = FALSE)
  }
  df$species <- trimws(remove_authorship(df$species))

  # Species to write ####
  no_sp <- is.na(df$species) | !nzchar(df$species)
  if (any(no_sp) && verbose) {
    message("Skipping ", sum(no_sp), " records not identified to species")
  }
  df <- df[!no_sp, , drop = FALSE]
  if (!is.null(species)) {
    species <- trimws(remove_authorship(species))
    missing_sp <- setdiff(species, df$species)
    if (length(missing_sp) > 0) {
      warning("No records of ", paste(missing_sp, collapse = ", "), call. = FALSE)
    }
    df <- df[df$species %in% species, , drop = FALSE]
    if (nrow(df) == 0) return(invisible(NULL))
  }
  if (nrow(df) == 0) {
    warning("No specimens to write", call. = FALSE)
    return(invisible(NULL))
  }

  # One entry per specimen, with all herbaria of its duplicates ####
  specimens <- .specimen_entries(df, style, language, catalog_numbers)

  # Document ####
  # Formats like "bold", "italic", "smallcaps", "upper" (uppercase letters),
  # "sans" (Arial, as in Kew Bulletin) or combined, e.g. "bold+italic"
  run <- function(text, format = "plain") {
    has <- function(x) grepl(x, format, fixed = TRUE)
    if (has("upper")) text <- toupper(text)
    officer::ftext(text, officer::fp_text(
      font.family = if (has("sans")) "Arial" else font_family, font.size = font_size,
      bold = has("bold"), italic = has("italic"),
      # Word small capitals, marked by a color replaced by <w:smallCaps/>
      color = if (has("smallcaps")) .smallcaps_mark else "black"))
  }
  # Text with a final punctuation in the same format, and spaces in plain
  # text, e.g. "Brazil." in bold followed by " "
  run_end <- function(text, end, format) {
    punct <- sub("\\s+$", "", end)
    list(run(paste0(text, punct), format), run(substring(end, nchar(punct) + 1)))
  }

  doc <- officer::read_docx()
  out <- data.frame(species_name = character(0), specimens_text = character(0))
  if (is.null(header)) {
    header <- if (language == "pt") style$header_pt else style$header
  }
  if (!is.character(header) || length(header) != 1) {
    stop("`header` must be a single text, e.g. \"Paratypes\".", call. = FALSE)
  }
  header_runs <- if (nzchar(header)) {
    list(run(header, style$header_format),
         run(style$header_punct, style$header_punct_format),
         run(style$header_end))
  }

  # Journals that cite the species in the same paragraph of its specimens, e.g.
  # "Dimerostemma arnottii \u2013 Brazil, ...", under a single header
  if ((style$species_inline || style$header_once) && nzchar(header)) {
    doc <- officer::body_add_fpar(doc, do.call(officer::fpar, header_runs))
  }

  for (sp in sort(unique(specimens$species))) {
    sp_df <- specimens[specimens$species == sp, , drop = FALSE]

    if (style$species_inline) {
      runs <- list(run(sp, "italic"), run(" \u2013 "))
    } else {
      # Species name in italics followed by its author, then the header
      author <- .species_author(df[df$species == sp, , drop = FALSE], sp)
      doc <- officer::body_add_fpar(doc, officer::fpar(
        run(sp, "bold+italic"), run(if (nzchar(author)) paste0(" ", author) else "", "bold")))
      runs <- if (style$header_once) list() else header_runs
    }

    # Countries, states and specimens in a single paragraph
    countries <- unique(sp_df$country_label)
    for (cn in countries) {
      cn_df <- sp_df[sp_df$country_label == cn, , drop = FALSE]
      if (nzchar(cn)) runs <- c(runs, run_end(cn, style$country_end, style$country_format))
      states <- unique(cn_df$state_label)
      for (k in seq_along(states)) {
        st_df <- cn_df[cn_df$state_label == states[k], , drop = FALSE]
        if (nzchar(states[k])) {
          # e.g. "\u2014Guaviare:" after the country, then "\u2013Barima-Waini:" in
          # Systematic Botany
          prefix <- if (k == 1) style$state_prefix else style$state_prefix_next
          runs <- c(runs, list(run(prefix)),
                    if (style$state_colon_format) {
                      run_end(states[k], style$state_end, style$state_format)
                    } else {
                      list(run(states[k], style$state_format), run(style$state_end))
                    })
        }
        end <- if (k < length(states)) style$state_sep else ". "
        for (j in seq_len(nrow(st_df))) {
          # Specimens of other municipalities after ". " in journals like Willdenowia
          sep <- if (j == nrow(st_df)) end else
            if (st_df$muni[j + 1] != st_df$muni[j]) style$municipality_sep else style$specimen_sep
          same <- style$municipality_once && j > 1 && nzchar(st_df$muni[j]) &&
            st_df$muni[j] == st_df$muni[j - 1]
          before <- if (same) st_df$before_same[j] else st_df$before[j]
          if (nzchar(before)) runs <- c(runs, list(run(paste0(before, style$field_sep))))
          # Collector, "et al." and number, each in the format of the journal
          # The herbaria start with their own separator, e.g. " (" or "; "
          parts <- list(c(st_df$coll_name[j], style$name_format, ""),
                        c(st_df$coll_etal[j], style$etal_format, " "),
                        c(st_df$coll_number[j], style$number_format, " "),
                        c(st_df$herb_text[j], "plain", ""))
          parts <- parts[vapply(parts, function(x) nzchar(x[1]), logical(1))]
          for (m in seq_along(parts)) {
            txt <- parts[[m]][1]
            # Without a final period before separators like ". ", e.g. after "s.n."
            if (m == length(parts) && startsWith(sep, ".")) txt <- sub("[.]$", "", txt)
            runs <- c(runs, list(run(paste0(if (m > 1) parts[[m]][3], txt), parts[[m]][2])))
          }
          runs <- c(runs, list(run(sep)))
        }
      }
    }
    doc <- officer::body_add_fpar(doc, do.call(officer::fpar, .squish_runs(runs)))
    doc <- officer::body_add_fpar(doc, officer::fpar(run("")))

    out <- rbind(out, data.frame(species_name = sp,
                                 specimens_text = paste(sp_df$text, collapse = "\n")))
  }

  # Save the document ####
  if (is.null(dir)) {
    dir <- if (!is.null(path)) dirname(path) else "results_specimens"
  }
  dir <- .arg_check_dir(dir)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  if (is.null(filename)) {
    base <- if (!is.null(path)) tools::file_path_sans_ext(basename(path)) else "specimens"
    filename <- paste0(base, "_", gsub("\\s+", "_", style$name))
  }
  if (tools::file_ext(filename) == "") filename <- paste0(filename, ".docx")
  file <- file.path(dir, filename)
  doc <- .set_smallcaps(doc)
  print(doc, target = file)

  if (verbose) {
    message("Examined specimens of ", nrow(out), " species (", nrow(specimens),
            " specimens) written in the format of ", style$name, " to: ", file)
  }

  invisible(out)
}


#_______________________________________________________________________________
# Auxiliary functions of barroso_write_specimens ####

# Word small capitals: officer has no small capitals, so the runs are written
# with this color, then replaced by <w:smallCaps/> in the document
.smallcaps_mark <- "#0A0B0C"
.set_smallcaps <- function(doc) {
  body <- doc$doc_obj$get()
  nodes <- xml2::xml_find_all(
    body, "//*[local-name()='color'][@*[local-name()='val']='0A0B0C']")
  for (n in nodes) {
    xml2::xml_set_name(n, "smallCaps")
    xml2::xml_attr(n, "w:val") <- "true"
  }
  doc
}

# Without double spaces, also between runs of text in different formats, e.g.
# "Brazil. " followed by " Bahia"
.squish_runs <- function(runs) {
  runs <- runs[!vapply(runs, is.null, logical(1))]
  for (k in seq_along(runs)) {
    runs[[k]]$value <- gsub("\\s{2,}", " ", runs[[k]]$value)
    if (k > 1 && grepl("\\s$", runs[[k - 1]]$value)) {
      runs[[k]]$value <- sub("^\\s+", "", runs[[k]]$value)
    }
  }
  runs <- runs[nzchar(vapply(runs, `[[`, "", "value"))]
  # Without a space at the end of the paragraph
  n <- length(runs)
  if (n > 0) runs[[n]]$value <- sub("\\s+$", "", runs[[n]]$value)
  runs[nzchar(vapply(runs, `[[`, "", "value"))]
}

# Read records from an .xlsx, .xls or .csv file, as character
.read_records <- function(path, sheet = 1) {
  .arg_check_xlsx_path(path)
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  if (tolower(tools::file_ext(path)) == "csv") {
    utils::read.csv(path, colClasses = "character", check.names = FALSE, encoding = "UTF-8",
                    na.strings = c("", "NA"))
  } else {
    as.data.frame(readxl::read_excel(path, sheet = sheet, col_types = "text"))
  }
}

# Formats of the lists of examined specimens in each journal ####
# Checked against recent papers of each journal (bold, italics and small
# capitals as printed).
# Formats of text: "plain", "bold", "italic", "smallcaps", "upper" (uppercase),
# "sans" (Arial), or combined, e.g. "bold+italic"
# header_punct: punctuation after the header, in header_punct_format, followed
# by header_end in plain text, e.g. "Specimens examined" + "." + " "
# state_colon_format: whether the colon after the state has its format
# collector: "compact" (D.B.O.S. Cardoso), "spaced" (D. B. O. S. Cardoso) or
# "surname" (Cardoso), with name_format, etal_format and number_format
# date: "abbr" (8 May 1994), "abbr_dot" (8 Nov. 1994), "kew" (8 Jan. 1994),
# "full" (8 January 1994), "roman_dots" (8.V.1994), "roman_lower_dots" (8.v.1994)
# or "roman_lower" (8 v 1994)
# phenology: brackets around reproductiveCondition, or NULL to omit it
# sn: text for collections without number, which are also cited with the
# catalog number of the first herbarium, e.g. "A. Fernandes (EAC 11356)"
.style_defaults <- list(
  header = "Additional specimens examined", header_pt = "Material examinado",
  header_format = "italic", header_punct = ".", header_punct_format = "italic",
  header_end = " ", species_inline = FALSE,
  country_case = "upper", country_format = "plain", country_end = ". ",
  state_format = "plain", state_colon_format = FALSE, state_prefix = "",
  state_prefix_next = "", state_end = ": ", state_sep = ". ", municipality_end = ", ",
  specimen_sep = "; ", coord_sep = ", ", degree = "\u00b0",
  primes = c("\u2032", "\u2033"), hemisphere_sep = "", elevation = " m",
  date = "abbr", day_zero = FALSE, phenology = c("(", ")"), phenology_sep = " ",
  collector = "compact", name_format = "italic", etal_format = "italic",
  number_format = "italic", etal = "et al.", sn = "s.n.", catalog = "%s %s",
  herbarium_mark = "", field_sep = ", ", herbaria_open = " (", herbaria_close = ")",
  municipality_prefix = "", municipality_sep = NULL, municipality_once = FALSE,
  header_once = FALSE)

.journal_styles <- list(
  "Acta Botanica Brasilica" = list(
    header_format = "bold", header_punct = ":", header_punct_format = "bold",
    country_format = "bold", state_format = "bold", state_colon_format = TRUE,
    primes = c("\u2019", "\u201d"), hemisphere_sep = " ", date = "full",
    phenology_sep = ", ", herbarium_mark = "!", catalog = "%s barcode %s",
    municipality_sep = ". ", municipality_once = TRUE),
  "Systematic Botany" = list(
    header = "Representative Specimens Examined", header_format = "bold+italic",
    header_punct = "", header_end = "\u2014", country_case = "title",
    country_format = "bold", state_format = "smallcaps", state_prefix = "\u2014",
    state_prefix_next = "\u2014"),
  "Taxon" = list(
    header_end = " \u2013 ", state_format = "upper", state_sep = "; ",
    elevation = " m a.s.l.", etal = "& al.", catalog = "%s barcode %s"),
  "Kew Bulletin" = list(
    header = "Specimens examined", header_format = "bold+upper+sans",
    header_punct_format = "bold+sans", country_format = "bold+sans",
    primes = c("'", "\""), date = "kew", collector = "spaced", number_format = "plain"),
  "Phytotaxa" = list(
    header_format = "bold", header_punct = ":\u2015", header_punct_format = "bold",
    header_end = "", municipality_end = ": ", primes = c("\u2019", "\u201d"),
    date = "full"),
  "Brittonia" = list(
    header_format = "bold+italic", header_end = "\u2014", country_format = "bold",
    state_format = "bold", state_colon_format = TRUE, phenology = c("[", "]"),
    collector = "spaced", etal_format = "plain", catalog = "%s no. %s"),
  "Candollea" = list(
    header_end = " \u2013 ", country_case = "title", country_format = "bold+smallcaps",
    state_format = "bold", state_colon_format = TRUE, coord_sep = " ",
    primes = c("'", "\""), date = "roman_dots", phenology = NULL, collector = "surname"),
  "Edinburgh Journal of Botany" = list(
    country_format = "bold", state_format = "bold", date = "roman_lower",
    collector = "surname", etal_format = "plain", number_format = "plain",
    catalog = "%s [%s]", municipality_sep = ". "),
  "Systematics and Biodiversity" = list(
    header = "Additional specimens analysed", header_format = "bold",
    header_punct_format = "bold", primes = c("'", "\""), date = "full",
    phenology = NULL, collector = "spaced"),
  "Annals of the Missouri Botanical Garden" = list(
    state_format = "bold", state_colon_format = TRUE, date = "abbr_dot",
    collector = "spaced"),
  "Botanical Journal of the Linnean Society" = list(
    header = "Representative specimens examined", header_punct = ":", header_end = " ",
    state_format = "bold", date = "roman_lower_dots", phenology = NULL,
    municipality_once = TRUE),
  "Brazilian Journal of Botany" = list(
    header = "Selected additional specimens", header_punct = "", header_end = "\u2014",
    day_zero = TRUE, collector = "surname", sn = "sn"),
  "Nordic Journal of Botany" = list(
    header = "Additional specimen examined", header_format = "bold", header_punct = "",
    header_end = "", species_inline = TRUE, country_case = "title", country_end = ", ",
    state_end = ", ", specimen_sep = ". ", date = "abbr_dot", phenology_sep = ", ",
    collector = "surname", name_format = "plain", etal_format = "plain",
    number_format = "plain", catalog = "%s%s"),
  "Plant Ecology and Evolution" = list(
    header = "Additional material examined", header_format = "bold",
    header_punct_format = "bold", country_end = " \u2022 ", state_end = ", ",
    state_sep = " \u2022 ", specimen_sep = " \u2022 ", field_sep = "; ",
    primes = c("\u2019", "\u201d"), date = "abbr_dot", collector = "surname",
    herbaria_open = "; ", herbaria_close = ""),
  "PhytoKeys" = list(
    header_format = "bold", header_punct_format = "bold", country_case = "title",
    country_format = "bold+smallcaps", state_format = "bold", state_colon_format = TRUE,
    municipality_prefix = "Mun. ", primes = c("'", "\""), elevation = " m alt.",
    date = "abbr_dot", phenology = NULL, catalog = "%s [%s]"),
  "Willdenowia" = list(
    header_format = "bold", header_punct_format = "bold", header_once = TRUE,
    country_case = "title", country_format = "smallcaps", country_end = ": ",
    state_format = "smallcaps", state_prefix_next = "\u2014 ", municipality_prefix = "Municipality ",
    municipality_sep = ". ", municipality_once = TRUE, primes = c("'", "\""),
    phenology = NULL, collector = "spaced", etal = "& al."),
  "Rodrigu\u00e9sia" = list(
    header = "Additional material examined", header_format = "bold",
    header_punct = ":", header_punct_format = "bold", state_format = "upper",
    specimen_sep = ". ", degree = "\u00ba", primes = c("\u2019", "\u201d"),
    date = "roman_dots", phenology = c("", ""), phenology_sep = ", ", sn = ""))

# Find the style of a journal from its name, in any case or abbreviated,
# e.g. "systematic botany", "SystematicBotany", "Kew", "Edimburgh"
.journal_style <- function(journal) {
  if (!is.character(journal) || length(journal) != 1) {
    stop("`journal` must be the name of a journal.", call. = FALSE)
  }
  norm <- function(x) gsub("[^a-z]", "", tolower(x))
  keys <- norm(stringi::stri_trans_general(names(.journal_styles), "Latin-ASCII"))
  j <- sub("^edimburgh", "edinburgh", norm(stringi::stri_trans_general(journal, "Latin-ASCII")))
  i <- which(keys == j)
  if (length(i) == 0) i <- which(startsWith(keys, j))
  if (length(i) != 1) {
    stop("Journal \"", journal, "\" not available. Use one of: ",
         paste(names(.journal_styles), collapse = ", "), call. = FALSE)
  }
  style <- utils::modifyList(.style_defaults, .journal_styles[[i]], keep.null = TRUE)
  style$name <- names(.journal_styles)[i]
  if (is.null(style$municipality_sep)) style$municipality_sep <- style$specimen_sep
  style
}

# Column as character, or empty strings when missing
.col_or_empty <- function(df, col) {
  if (col %in% names(df)) .na_to_empty(trimws(df[[col]])) else rep("", nrow(df))
}

# Author of a species, from the most frequent author of its records
.species_author <- function(df, sp) {
  if ("scientificNameAuthorship" %in% names(df)) {
    a <- .col_or_empty(df, "scientificNameAuthorship")
  } else {
    a <- character(0)
    for (col in intersect(c("acceptedScientificName", "scientificName"), names(df))) {
      x <- .col_or_empty(df, col)
      x <- x[startsWith(x, paste0(sp, " "))]
      a <- c(a, trimws(substring(x, nchar(sp) + 2)))
    }
  }
  # Without infraspecific names, e.g. "Benth. var. ..."
  a <- a[nzchar(a) & !grepl("\\b(var|subsp|f)[.]\\s", a)]
  if (length(a) == 0) return("")
  names(sort(table(a), decreasing = TRUE))[1]
}

# Build the text of each specimen, merging duplicates ####
.specimen_entries <- function(df, style, language, catalog_numbers) {

  # Values without double spaces, e.g. "Serra  do Orob\u00f3"
  col <- function(x) gsub("\\s{2,}", " ", .col_or_empty(df, x))
  coll <- col("recordedBy")
  num <- col("recordNumber")
  num[num %in% c("s.n.", "s/n", "sn")] <- ""
  locality <- sub("[.;,[:space:]]+$", "", col("locality"))

  # Duplicates have the same species, collector surname and number, e.g.
  # "G. T. Prance 11558" and "C. Prance 11558", or the same collector, date and
  # locality when there is no number
  surname <- tolower(stringi::stri_trans_general(sub(".*\\s", "", coll), "Latin-ASCII"))
  key <- ifelse(nzchar(num),
                paste(df$species, surname, num, sep = "|"),
                paste(df$species, coll, col("year"), col("month"), col("day"), locality,
                      sep = "|"))

  # Herbaria of each record with their catalog numbers, including those of its
  # duplicates from barroso_flag_duplicates, e.g. "LD 2052774 | P03101230"
  herb <- col("collectionCode")
  cat_nr <- col("catalogNumber")
  dup_herb <- col("duplicateCollectionCodes")
  dup_cat <- col("duplicateCatalogNumbers")
  # Herbarium acronyms of databases not standardized with std_collection,
  # e.g. "F-Seedplants" or "Herbario INPA"
  clean_code <- function(code) {
    bad <- !.is_acronym(code) | code %in% "Tropicos"
    if (any(bad)) code[bad] <- .std_herbarium_code(code[bad], NA)
    code
  }
  split_bar <- function(x) strsplit(x, "\\s*[|]\\s*")[[1]]
  herb_pairs <- lapply(seq_len(nrow(df)), function(i) {
    entries <- if (nzchar(dup_cat[i])) split_bar(dup_cat[i]) else trimws(paste(herb[i], cat_nr[i]))
    code <- ifelse(grepl("\\s", entries), sub("\\s.*", "", entries),
                   sub("^([A-Z]+).*$", "\\1", entries))
    # Barcodes starting with the acronym, e.g. "P03101230"
    nr <- ifelse(grepl("\\s", entries), sub("^\\S+\\s*", "", entries),
                 ifelse(code == entries, "", entries))
    if (nzchar(dup_herb[i])) {
      extra <- setdiff(split_bar(dup_herb[i]), code)
      code <- c(code, extra)
      nr <- c(nr, rep("", length(extra)))
    }
    data.frame(code = clean_code(code), nr = nr)
  })
  herb <- clean_code(herb)

  # Fields of each specimen ####
  coords <- mapply(.format_coordinates,
                   suppressWarnings(as.numeric(col("decimalLatitude"))),
                   suppressWarnings(as.numeric(col("decimalLongitude"))),
                   MoreArgs = list(primes = style$primes, sep = style$coord_sep,
                                   degree = style$degree, hemisphere_sep = style$hemisphere_sep),
                   USE.NAMES = FALSE)
  elev <- .format_elevation(col("minimumElevationInMeters"), col("maximumElevationInMeters"),
                            style$elevation)
  date <- mapply(.format_date, col("day"), col("month"), col("year"),
                 MoreArgs = list(language = language, format = style$date,
                                 day_zero = style$day_zero), USE.NAMES = FALSE)
  phen <- col("reproductiveCondition")
  if (!is.null(style$phenology)) {
    phen <- paste0(style$phenology[1], phen, style$phenology[2])
    date <- ifelse(nzchar(col("reproductiveCondition")),
                   ifelse(nzchar(date), paste0(date, style$phenology_sep, phen), phen), date)
  }
  collector <- .format_collector(coll, col("addCollector"), style)
  number <- ifelse(nzchar(num), num, style$sn)

  country <- col("country")
  if (language == "pt") country <- vapply(country, .translate_country, "", USE.NAMES = FALSE)
  country_label <- if (style$country_case == "upper") toupper(country) else country
  state <- col("stateProvince")
  state_label <- if (grepl("upper", style$state_format)) toupper(state) else state
  # Municipalities are often in county, e.g. in GBIF records of Brazil
  # without words like "Mun." or "Munic\u00edpio de", as in std_place
  muni <- .na_to_empty(.rm_municipality_word(col("municipality")))
  county <- .na_to_empty(.rm_municipality_word(col("county")))
  muni[!nzchar(muni)] <- county[!nzchar(muni)]

  # One entry per specimen, taking the first filled value of its duplicates
  idx <- split(seq_len(nrow(df)), factor(key, levels = unique(key)))
  first <- function(x, i) {
    v <- x[i][nzchar(x[i])]
    if (length(v) > 0) v[1] else ""
  }
  rows <- lapply(idx, function(i) {
    hp <- do.call(rbind, herb_pairs[i])
    hp <- hp[!is.na(hp$code) & nzchar(hp$code), , drop = FALSE]
    # One entry per herbarium, with its first catalog number, in alphabetical order
    hp <- hp[order(toupper(hp$code), !nzchar(hp$nr)), , drop = FALSE]
    hp <- hp[!duplicated(hp$code), , drop = FALSE]
    # Catalog numbers are written when asked, and, for collections without
    # number, the catalog number of the first herbarium
    with_cat <- if (catalog_numbers) rep(TRUE, nrow(hp)) else {
      !nzchar(num[i[1]]) & seq_len(nrow(hp)) == which(nzchar(hp$nr))[1]
    }
    with_cat[is.na(with_cat)] <- FALSE
    h <- ifelse(with_cat & nzchar(hp$nr),
                ifelse(startsWith(hp$nr, hp$code) & style$catalog %in% c("%s %s", "%s%s"),
                       hp$nr, sprintf(style$catalog, hp$code, hp$nr)),
                hp$code)
    h <- paste0(h, if (nrow(hp)) style$herbarium_mark)
    data.frame(species = df$species[i[1]],
               country = first(country, i), country_label = first(country_label, i),
               state = first(state, i), state_label = first(state_label, i),
               muni = first(muni, i), locality = first(locality, i),
               coords = first(coords, i), elev = first(elev, i), date = first(date, i),
               year = first(col("year"), i), month = first(col("month"), i),
               day = first(col("day"), i), coll_name = collector$name[i[1]],
               coll_etal = collector$etal[i[1]], coll_number = number[i[1]],
               herbaria = paste(h, collapse = ", "))
  })
  sp <- do.call(rbind, rows)

  # Text of each specimen
  sp$muni <- ifelse(nzchar(sp$muni), paste0(style$municipality_prefix, sp$muni), "")
  place <- ifelse(nzchar(sp$muni) & nzchar(sp$locality),
                  paste0(sp$muni, style$municipality_end, sp$locality),
                  paste0(sp$muni, sp$locality))
  # Text before the collector, the collector with number, and the herbaria
  join <- function(...) apply(cbind(...), 1, function(x) {
    paste(trimws(x[nzchar(trimws(x))]), collapse = style$field_sep)
  })
  sp$before <- join(place, sp$coords, sp$elev, sp$date)
  # Without the municipality, for the next specimens of the same municipality
  # in journals like Willdenowia
  sp$before_same <- join(sp$locality, sp$coords, sp$elev, sp$date)
  sp$collector <- trimws(gsub("\\s+", " ", paste(sp$coll_name, sp$coll_etal, sp$coll_number)))
  # Herbaria between parentheses, e.g. "(HUEFS, RB)", or after a semicolon in
  # Plant Ecology and Evolution
  sp$herb_text <- ifelse(nzchar(sp$herbaria),
                         paste0(style$herbaria_open, sp$herbaria, style$herbaria_close), "")
  sp$text <- paste0(ifelse(nzchar(sp$before), paste0(sp$before, style$field_sep), ""),
                    sp$collector, sp$herb_text)
  sp$text <- gsub("\\s{2,}", " ", sp$text)
  # Without a final period, e.g. after "s.n.", before separators like ". "
  if (startsWith(style$specimen_sep, ".")) sp$text <- sub("[.]$", "", sp$text)

  # Order by country, state, municipality and date, with records without
  # country at the end
  num_or <- function(x) suppressWarnings(as.numeric(x))
  o <- order(!nzchar(sp$country), sp$country, !nzchar(sp$state), sp$state, sp$muni,
             num_or(sp$year), num_or(sp$month), num_or(sp$day), sp$collector)
  sp[o, , drop = FALSE]
}

# Collector names as cited in each journal, from names standardized by
# std_recordedBy like "D. B. O. S. Cardoso", with the additional collector
.format_collector <- function(coll, add, style) {
  fmt <- function(x) {
    switch(style$collector,
           compact = gsub("(\\p{Lu}[.])\\s+(?=\\p{Lu}[.])", "\\1", x, perl = TRUE),
           surname = sub("^((\\p{Lu}[.]|-)\\s*)+(?=\\p{Lu}\\p{Ll})", "", x, perl = TRUE),
           x)
  }
  add <- .na_to_empty(add)
  name <- fmt(.na_to_empty(coll))
  name[!nzchar(name)] <- "Anonymous"
  etal <- grepl("^(et\\.?\\s*al\\.?|&\\s*al\\.?)$", add, ignore.case = TRUE)
  # The second collector, e.g. "R.M. Harley & A.M. Giulietti", or "et al."
  two <- nzchar(add) & !etal
  name[two] <- paste(name[two], "&", fmt(add[two]))
  list(name = name, etal = ifelse(etal, style$etal, ""))
}

# Elevation, e.g. "650 m" or "650–700 m a.s.l."
.format_elevation <- function(min, max, unit) {
  min <- sub("[.]0+$", "", min)
  max <- sub("[.]0+$", "", max)
  elev <- ifelse(nzchar(min) & nzchar(max) & min != max, paste0(min, "\u2013", max),
                 ifelse(nzchar(min), min, max))
  ifelse(nzchar(elev), paste0(elev, unit), "")
}

#' Format coordinates for display
#' @keywords internal
#' @noRd
.format_coordinates <- function(lat, lon, primes = c("\u2032", "\u2033"), sep = ", ",
                               degree = "\u00b0", hemisphere_sep = "") {
  if (is.na(lat) || is.na(lon)) return("")
  if (abs(lat) > 90 || abs(lon) > 180 || (lat == 0 && lon == 0)) return("")

  # Degrees, minutes and seconds from the rounded total of seconds, so that
  # 59.6 seconds become the next minute instead of "60 seconds"
  dms <- function(x) {
    total <- round(abs(x) * 3600)
    deg <- total %/% 3600
    min <- (total - deg * 3600) %/% 60
    c(deg, min, total - deg * 3600 - min * 60)
  }
  la <- dms(lat)
  lo <- dms(lon)
  lat_dir <- if (lat >= 0) "N" else "S"
  lon_dir <- if (lon >= 0) "E" else "W"
  fmt <- function(d, h, sec, min) {
    paste0(d[1], degree,
           # Minutes and seconds with two digits, e.g. 13\u00b000\u203217\u2033S
           if (min) paste0(sprintf("%02d", d[2]), primes[1]) else "",
           if (sec) paste0(sprintf("%02d", d[3]), primes[2]) else "", hemisphere_sep, h)
  }
  sec <- la[3] > 0 || lo[3] > 0
  min <- sec || la[2] > 0 || lo[2] > 0
  paste0(fmt(la, lat_dir, sec, min), sep, fmt(lo, lon_dir, sec, min))
}

#' Format date for display
#' @keywords internal
#' @noRd
.format_date <- function(day, month, year, language = "en", format = "abbr",
                         day_zero = FALSE) {
  year <- .na_to_empty(year)
  if (!nzchar(year)) return("")
  day <- sub("^0+", "", .na_to_empty(day))
  if (day_zero && nzchar(day)) day <- sprintf("%02d", as.integer(day))
  m <- suppressWarnings(as.numeric(month))
  # Months written as text, e.g. "Dec"
  if (is.na(m)) m <- match(tolower(substr(.na_to_empty(month), 1, 3)), tolower(month.abb))
  if (is.na(m) || m < 1 || m > 12) return(year)

  months <- if (language == "pt") {
    c("janeiro", "fevereiro", "mar\u00e7o", "abril", "maio", "junho", "julho",
      "agosto", "setembro", "outubro", "novembro", "dezembro")
  } else {
    month.name
  }
  roman <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII")
  kew <- c("Jan.", "Feb.", "March", "April", "May", "June", "July", "Aug.", "Sept.",
           "Oct.", "Nov.", "Dec.")
  mon <- switch(format,
                full = months[m],
                kew = if (language == "pt") substr(months[m], 1, 3) else kew[m],
                roman_dots = roman[m],
                roman_lower = tolower(roman[m]),
                abbr_dot = if (nchar(months[m]) > 4) paste0(substr(months[m], 1, 3), ".") else months[m],
                substr(months[m], 1, 3))
  if (format == "roman_lower_dots") {
    return(paste(c(if (nzchar(day)) day, tolower(roman[m]), year), collapse = "."))
  }
  if (format == "roman_dots") {
    return(paste(c(if (nzchar(day)) day, mon, year), collapse = "."))
  }
  paste(c(if (nzchar(day)) day, mon, year), collapse = " ")
}

#' Missing values as empty strings
#' @keywords internal
#' @noRd
.na_to_empty <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

#' Translate country names to Portuguese
#' @keywords internal
#' @noRd
.translate_country <- function(country) {
  translations <- c(
    "Brazil" = "Brasil", "United States" = "Estados Unidos", "USA" = "EUA",
    "Colombia" = "Col\u00f4mbia", "Ecuador" = "Equador", "Bolivia" = "Bol\u00edvia",
    "Paraguay" = "Paraguai", "Uruguay" = "Uruguai", "Guyana" = "Guiana",
    "French Guiana" = "Guiana Francesa", "France" = "Fran\u00e7a", "Germany" = "Alemanha",
    "Italy" = "It\u00e1lia", "Spain" = "Espanha", "United Kingdom" = "Reino Unido",
    "Mexico" = "M\u00e9xico", "Canada" = "Canad\u00e1", "Panama" = "Panam\u00e1",
    "Costa Rica" = "Costa Rica", "Nicaragua" = "Nicar\u00e1gua", "Belize" = "Belize",
    "Honduras" = "Honduras", "Cuba" = "Cuba", "China" = "China", "Japan" = "Jap\u00e3o",
    "Vietnam" = "Vietn\u00e3", "Thailand" = "Tail\u00e2ndia", "Indonesia" = "Indon\u00e9sia",
    "Malaysia" = "Mal\u00e1sia", "Philippines" = "Filipinas", "India" = "\u00cdndia",
    "Australia" = "Austr\u00e1lia")
  if (country %in% names(translations)) translations[[country]] else country
}
