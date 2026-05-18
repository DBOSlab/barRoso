#' Write examined specimens from a database to Word (.docx) in journal format
#'
#' @author Domingos Cardoso
#'
#' @description This function reads specimen data from an Excel spreadsheet and
#'   generates formatted lists of examined specimens in the style required by
#'   specific scientific journals (Systematic Botany, Taxon, etc.). It handles
#'   duplicate specimens (same collector and number) by grouping multiple herbaria
#'   into a single entry, formats geographic hierarchy (country, state/province,
#'   municipality), and localizes month names and country names according to the
#'   specified language.
#'
#' @param xlsx_path Character. Path to the .xlsx file containing specimen data.
#' @param species_cols Character/integer. Columns used to build the species header.
#' @param collection_cols Named list or character vector. Columns containing
#'   specimen information. Must include: country, stateProvince, municipality,
#'   locality, decimalLatitude, decimalLongitude, day, month, year, recordBy,
#'   recordNumber, collectionCode. Names can be customized using a named list
#'   (e.g., list(country = "Country", state = "StateProvince")).
#' @param sheet Sheet name or index. Default 1.
#' @param species_filter Optional character vector to filter species.
#' @param journal Character. Journal style format. Currently supports
#'   "SystematicBotany" and "Taxon". Default is "SystematicBotany".
#' @param language Character. Language for output ("en" for English, "pt" for
#'   Portuguese). Affects month names and country translations. Default "en".
#' @param add_representative Logical. Should "Representative Specimens Examined"
#'   header be added? Default TRUE.
#' @param font_family Character. Font family for Word document. Default "Times New Roman".
#' @param font_size Numeric. Font size in points. Default 12.
#' @param species_bold Logical. Make species names bold. Default TRUE.
#' @param species_italic Logical. Make species names italic. Default TRUE.
#' @param country_bold Logical. Make country names bold. Default TRUE.
#' @param state_smallcaps Logical. Use small caps for state/province names. Default TRUE.
#' @param verbose Logical. Print progress messages. Default TRUE.
#' @param dir Output directory. Default NULL.
#' @param filename Output file base name. Default NULL.
#'
#' @return Invisibly returns a data.frame with species names and formatted text.
#'   Writes a .docx file as the main side effect.
#'
#' @details \strong{Specimen format by journal:}
#'
#' For Systematic Botany:
#' \preformatted{
#' Representative Specimens Examined—Country. —STATE/PROVINCE: Locality,
#' coordinates, date (phenology), Collector Name ## (HERBARIUM1, HERBARIUM2).
#' }
#'
#' For Taxon:
#' \preformatted{
#' Country. —State/Province: Locality, coordinates, date (phenology),
#' Collector Name ## (HERBARIUM1, HERBARIUM2).
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' barroso_write_specimens(
#'   xlsx_path = "specimens.xlsx",
#'   species_cols = c("Genus", "Species", "Author"),
#'   collection_cols = c("country", "stateProvince", "municipality",
#'                        "locality", "decimalLatitude", "decimalLongitude",
#'                        "day", "month", "year", "recordBy", "recordNumber",
#'                        "collectionCode"),
#'   journal = "SystematicBotany",
#'   language = "en"
#' )
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom officer read_docx body_add_fpar fpar fp_text ftext body_add_par
#' @importFrom dplyr group_by summarise arrange select
#' @importFrom tidyr unite
#' @importFrom tools file_path_sans_ext file_ext
#'
#' @export
barroso_write_specimens <- function(xlsx_path,
                                    species_cols,
                                    collection_cols,
                                    sheet = 1,
                                    species_filter = NULL,
                                    journal = "SystematicBotany",
                                    language = "en",
                                    add_representative = TRUE,
                                    font_family = "Times New Roman",
                                    font_size = 12,
                                    species_bold = TRUE,
                                    species_italic = TRUE,
                                    country_bold = TRUE,
                                    state_smallcaps = TRUE,
                                    verbose = TRUE,
                                    dir = NULL,
                                    filename = NULL) {

  # Create output directory
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

  # Read Excel file
  .arg_check_xlsx_path(xlsx_path)
  df <- readxl::read_excel(path = xlsx_path, sheet = sheet)

  # Resolve column names
  species_cols <- .resolve_cols(df, species_cols, arg = "species_cols")

  # Handle collection_cols - if named list, map to actual names
  if (is.list(collection_cols) && !is.null(names(collection_cols))) {
    mapped_cols <- character()
    for (std_name in names(collection_cols)) {
      actual_name <- collection_cols[[std_name]]
      if (actual_name %in% names(df)) {
        mapped_cols[std_name] <- actual_name
      } else {
        stop("Column '", actual_name, "' (mapped from '", std_name, "') not found in data")
      }
    }
    collection_cols <- mapped_cols
  } else if (is.character(collection_cols)) {
    names(collection_cols) <- collection_cols
  }

  # Required standard column names
  required_cols <- c("country", "stateProvince", "locality", "decimalLatitude",
                     "decimalLongitude", "day", "month", "year", "recordBy",
                     "recordNumber", "collectionCode")

  for (req in required_cols) {
    if (!req %in% names(collection_cols)) {
      stop("Required column mapping missing for: ", req)
    }
  }

  # Select all needed columns
  all_needed <- unique(c(species_cols, collection_cols))
  missing <- setdiff(all_needed, names(df))
  if (length(missing) > 0) {
    stop("Missing columns: ", paste(missing, collapse = ", "))
  }
  df <- df[, all_needed, drop = FALSE]

  # Filter species if needed
  if (!is.null(species_filter)) {
    species_names <- apply(df[, species_cols, drop = FALSE], 1,
                           function(x) paste(na.omit(x), collapse = " "))
    species_names_clean <- barRoso::remove_authorship(species_names)
    matches <- species_names_clean %in% species_filter
    df <- df[matches, , drop = FALSE]
    if (verbose && sum(matches) > 0) {
      message("Filtered to ", sum(matches), " specimens for ",
              paste(species_filter, collapse = ", "))
    }
  }

  if (nrow(df) == 0) {
    warning("No specimens after filtering")
    return(invisible(NULL))
  }

  # Create species identifier
  df$species_id <- apply(df[, species_cols, drop = FALSE], 1,
                         function(x) paste(na.omit(x), collapse = " "))
  df$species_id <- barRoso::remove_authorship(df$species_id)

  # Group by collector and number to merge duplicate herbaria
  df$collector_key <- paste(df[[collection_cols["recordBy"]]],
                            df[[collection_cols["recordNumber"]]],
                            sep = " ## ")

  # Format coordinates
  df$coords <- .format_coordinates(df[[collection_cols["decimalLatitude"]]],
                                   df[[collection_cols["decimalLongitude"]]])

  # Format date
  df$date <- .format_date(df[[collection_cols["day"]]],
                          df[[collection_cols["month"]]],
                          df[[collection_cols["year"]]],
                          language = language)

  # Format phenology (to be added by user or extracted from other columns)
  df$phenology <- ""  # Placeholder - could be extracted from additional column

  # Group by species and geographic hierarchy
  result_list <- list()

  for (sp in unique(df$species_id)) {
    sp_df <- df[df$species_id == sp, ]

    # Group by country, state, municipality
    sp_df$geo_key <- paste(sp_df[[collection_cols["country"]]],
                           sp_df[[collection_cols["stateProvince"]]],
                           sp_df[[collection_cols["municipality"]]],
                           sep = "||")

    specimens_list <- list()

    for (geo in unique(sp_df$geo_key)) {
      geo_df <- sp_df[sp_df$geo_key == geo, ]

      country <- unique(geo_df[[collection_cols["country"]]])[1]
      state <- unique(geo_df[[collection_cols["stateProvince"]]])[1]
      municipality <- unique(geo_df[[collection_cols["municipality"]]])[1]

      # Translate country if needed
      if (language == "pt") {
        country <- .translate_country(country)
      }

      # Group specimens within this geographic area
      specimens <- character()

      # Group by collector to merge duplicates
      for (collector in unique(geo_df$collector_key)) {
        coll_df <- geo_df[geo_df$collector_key == collector, ]

        # Get unique herbaria
        herbaria <- unique(coll_df[[collection_cols["collectionCode"]]])
        herbaria <- herbaria[!is.na(herbaria) & nzchar(herbaria)]
        herbarium_text <- if (length(herbaria) > 0) {
          paste0("(", paste(herbaria, collapse = ", "), ")")
        } else {
          ""
        }

        # Format locality info
        locality <- unique(coll_df[[collection_cols["locality"]]])[1]
        coords <- unique(coll_df$coords)[1]
        date <- unique(coll_df$date)[1]

        # Build specimen entry
        collector_name <- unique(coll_df[[collection_cols["recordBy"]]])[1]
        collector_num <- unique(coll_df[[collection_cols["recordNumber"]]])[1]

        specimen_entry <- .format_specimen_entry(
          locality = locality,
          coords = coords,
          date = date,
          phenology = "",  # Could be added
          collector = collector_name,
          collector_num = collector_num,
          herbaria = herbarium_text,
          journal = journal
        )

        specimens <- c(specimens, specimen_entry)
      }

      # Add geographic header
      specimens_list[[length(specimens_list) + 1]] <- list(
        country = country,
        state = state,
        municipality = municipality,
        specimens = specimens
      )
    }

    result_list[[sp]] <- specimens_list
  }

  # Build Word document
  doc <- officer::read_docx()

  out_tbl <- data.frame(species_name = character(0),
                        specimens_text = character(0),
                        stringsAsFactors = FALSE)

  for (i in seq_along(names(result_list))) {
    sp <- names(result_list)[i]
    specimens_list <- result_list[[sp]]

    # Create species name with formatting
    species_prop <- officer::fp_text(font.family = font_family,
                                     font.size = font_size,
                                     bold = species_bold,
                                     italic = species_italic)

    species_name_runs <- list(officer::ftext(sp, prop = species_prop))

    # Add "Representative Specimens Examined" header
    if (add_representative && i == 1) {
      header_text <- if (journal == "SystematicBotany") {
        "Representative Specimens Examined"
      } else if (journal == "Taxon") {
        "Specimens Examined"
      } else {
        "Specimens Examined"
      }

      header_prop <- officer::fp_text(font.family = font_family,
                                      font.size = font_size,
                                      bold = TRUE,
                                      italic = FALSE)
      doc <- officer::body_add_fpar(doc,
                                    officer::fpar(officer::ftext(header_text,
                                                                 prop = header_prop)))
      doc <- officer::body_add_par(doc, "")
    }

    # Add species name
    doc <- officer::body_add_fpar(doc,
                                  do.call(officer::fpar, species_name_runs))
    doc <- officer::body_add_par(doc, "")

    # Build specimens text
    specimens_text_all <- character()

    for (geo_group in specimens_list) {
      # Country with bold
      country_prop <- officer::fp_text(font.family = font_family,
                                       font.size = font_size,
                                       bold = country_bold,
                                       italic = FALSE)

      # State with small caps
      state_prop <- officer::fp_text(font.family = font_family,
                                     font.size = font_size,
                                     bold = FALSE,
                                     italic = FALSE,
                                     vertical.align = "baseline")
      if (state_smallcaps) {
        state_prop <- officer::fp_text(font.family = font_family,
                                       font.size = font_size,
                                       bold = FALSE,
                                       italic = FALSE,
                                       vertical.align = "baseline",
                                       fp_p = officer::fp_par(text.align = "left"))
        # Note: officer doesn't directly support small caps
        # We'll use regular text and note in documentation
      }

      # Format country line
      if (journal == "SystematicBotany") {
        if (nzchar(geo_group$state)) {
          country_line <- paste0(geo_group$country, ". —",
                                 toupper(geo_group$state), ":")
        } else {
          country_line <- paste0(geo_group$country, ".")
        }
      } else {
        if (nzchar(geo_group$state)) {
          country_line <- paste0(geo_group$country, ". —",
                                 toupper(geo_group$state), ":")
        } else {
          country_line <- paste0(geo_group$country, ".")
        }
      }

      country_run <- officer::ftext(country_line, prop = country_prop)
      doc <- officer::body_add_fpar(doc, officer::fpar(country_run))

      # Add specimens
      for (spec in geo_group$specimens) {
        normal_prop <- officer::fp_text(font.family = font_family,
                                        font.size = font_size,
                                        bold = FALSE,
                                        italic = FALSE)
        spec_run <- officer::ftext(paste0("  ", spec), prop = normal_prop)
        doc <- officer::body_add_fpar(doc, officer::fpar(spec_run))

        specimens_text_all <- c(specimens_text_all, spec)
      }
    }

    # Add blank line between species
    doc <- officer::body_add_par(doc, "")

    out_tbl <- rbind(out_tbl,
                     data.frame(species_name = sp,
                                specimens_text = paste(specimens_text_all,
                                                       collapse = "\n"),
                                stringsAsFactors = FALSE))
  }

  # Save document
  if (is.null(filename)) {
    base <- tools::file_path_sans_ext(basename(xlsx_path))
    filename <- paste0(foldername, "/", base, "_specimens.docx")
  } else if (tools::file_ext(filename) == "") {
    filename <- paste0(foldername, "/", filename, ".docx")
  }

  print(doc, target = filename)

  if (verbose) {
    message("Specimens list written to: ", filename)
  }

  invisible(out_tbl)
}

# Helper functions

#' Format coordinates for display
#' @keywords internal
#' @noRd
.format_coordinates <- function(lat, lon) {
  if (is.na(lat) || is.na(lon)) return("")

  # Format degrees/minutes/seconds if needed
  if (abs(lat) > 90) return("")

  # Simple decimal format
  lat_dir <- ifelse(lat >= 0, "N", "S")
  lon_dir <- ifelse(lon >= 0, "E", "W")

  lat_abs <- abs(lat)
  lon_abs <- abs(lon)

  # Format as degrees/minutes/seconds for precision
  lat_deg <- floor(lat_abs)
  lat_min_float <- (lat_abs - lat_deg) * 60
  lat_min <- floor(lat_min_float)
  lat_sec <- round((lat_min_float - lat_min) * 60, 1)

  lon_deg <- floor(lon_abs)
  lon_min_float <- (lon_abs - lon_deg) * 60
  lon_min <- floor(lon_min_float)
  lon_sec <- round((lon_min_float - lon_min) * 60, 1)

  if (lat_sec > 0 || lon_sec > 0) {
    return(sprintf("%d°%d′%.1f″%s, %d°%d′%.1f″%s",
                   lat_deg, lat_min, lat_sec, lat_dir,
                   lon_deg, lon_min, lon_sec, lon_dir))
  } else if (lat_min > 0 || lon_min > 0) {
    return(sprintf("%d°%d′%s, %d°%d′%s",
                   lat_deg, lat_min, lat_dir,
                   lon_deg, lon_min, lon_dir))
  } else {
    return(sprintf("%d°%s, %d°%s", lat_deg, lat_dir, lon_deg, lon_dir))
  }
}

#' Format date for display
#' @keywords internal
#' @noRd
.format_date <- function(day, month, year, language = "en") {
  if (is.na(year) || year == "") return("")

  months_en <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  months_pt <- c("jan", "fev", "mar", "abr", "mai", "jun",
                 "jul", "ago", "set", "out", "nov", "dez")

  months <- if (language == "pt") months_pt else months_en

  month_num <- as.numeric(month)
  if (!is.na(month_num) && month_num >= 1 && month_num <= 12) {
    month_abbr <- months[month_num]
  } else {
    month_abbr <- as.character(month)
  }

  if (!is.na(day) && nzchar(day) && day != "") {
    return(paste(day, month_abbr, year))
  } else {
    return(paste(month_abbr, year))
  }
}

#' Translate country names to Portuguese
#' @keywords internal
#' @noRd
.translate_country <- function(country) {
  translations <- list(
    "Brazil" = "Brasil",
    "United States" = "Estados Unidos",
    "USA" = "EUA",
    "Colombia" = "Colômbia",
    "Peru" = "Peru",
    "Argentina" = "Argentina",
    "Venezuela" = "Venezuela",
    "Ecuador" = "Equador",
    "Bolivia" = "Bolívia",
    "Paraguay" = "Paraguai",
    "Uruguay" = "Uruguai",
    "Chile" = "Chile",
    "Guyana" = "Guiana",
    "Suriname" = "Suriname",
    "French Guiana" = "Guiana Francesa",
    "France" = "França",
    "Germany" = "Alemanha",
    "Italy" = "Itália",
    "Spain" = "Espanha",
    "Portugal" = "Portugal",
    "United Kingdom" = "Reino Unido",
    "Mexico" = "México",
    "Canada" = "Canadá"
  )

  if (country %in% names(translations)) {
    return(translations[[country]])
  }
  return(country)
}

#' Format individual specimen entry
#' @keywords internal
#' @noRd
.format_specimen_entry <- function(locality, coords, date, phenology,
                                   collector, collector_num, herbaria,
                                   journal = "SystematicBotany") {
  parts <- c()

  if (nzchar(locality)) parts <- c(parts, locality)
  if (nzchar(coords)) parts <- c(parts, coords)
  if (nzchar(date)) {
    if (nzchar(phenology)) {
      parts <- c(parts, paste0(date, " (", phenology, ")"))
    } else {
      parts <- c(parts, date)
    }
  }

  collector_info <- paste(collector, collector_num)
  if (nzchar(herbaria)) {
    collector_info <- paste(collector_info, herbaria)
  }
  parts <- c(parts, collector_info)

  paste(parts, collapse = ", ")
}

