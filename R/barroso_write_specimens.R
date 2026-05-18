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
#' @param sheet Sheet name or index. Default 1.
#' @param species_filter Optional character vector to filter species.
#' @param journal Character. Journal style format. Currently supports
#'   "SystematicBotany" and "Taxon". Default is "SystematicBotany".
#' @param language Character. Language for output ("en" for English, "pt" for
#'   Portuguese). Affects month names and country translations. Default "en".
#' @param add_representative Logical. Should "Representative Specimens Examined"
#'   header be added? Default TRUE.
#' @param colname_recordedBy Character. Column name for collector name. Default "recordedBy".
#' @param colname_recordNumber Character. Column name for collector number. Default "recordNumber".
#' @param colname_continent Character. Column name for continent. Default "continent".
#' @param colname_country Character. Column name for country. Default "country".
#' @param colname_stateProvince Character. Column name for state/province. Default "stateProvince".
#' @param colname_county Character. Column name for county. Default "county".
#' @param colname_municipality Character. Column name for municipality. Default "municipality".
#' @param colname_locality Character. Column name for locality description. Default "locality".
#' @param colname_decimalLatitude Character. Column name for latitude. Default "decimalLatitude".
#' @param colname_decimalLongitude Character. Column name for longitude. Default "decimalLongitude".
#' @param colname_day Character. Column name for collection day. Default "day".
#' @param colname_month Character. Column name for collection month. Default "month".
#' @param colname_year Character. Column name for collection year. Default "year".
#' @param colname_collectionCode Character. Column name for herbarium code. Default "collectionCode".
#' @param colname_institutionCode Character. Column name for institution code. Default "institutionCode".
#' @param colname_typeStatus Character. Column name for type status. Default "typeStatus".
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
#' # Basic usage with default column names
#' barroso_write_specimens(
#'   xlsx_path = "specimens.xlsx",
#'   species_cols = c("Genus", "Species", "Author"),
#'   journal = "SystematicBotany",
#'   language = "en"
#' )
#'
#' # Custom column names
#' barroso_write_specimens(
#'   xlsx_path = "specimens.xlsx",
#'   species_cols = c("Genus", "Species", "Author"),
#'   colname_country = "Country",
#'   colname_stateProvince = "State",
#'   colname_locality = "Location",
#'   colname_recordedBy = "Collector",
#'   colname_recordNumber = "Number"
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
                                    sheet = 1,
                                    species_filter = NULL,
                                    journal = "SystematicBotany",
                                    language = "en",
                                    add_representative = TRUE,
                                    colname_recordedBy = "recordedBy",
                                    colname_recordNumber = "recordNumber",
                                    colname_continent = "continent",
                                    colname_country = "country",
                                    colname_stateProvince = "stateProvince",
                                    colname_county = "county",
                                    colname_municipality = "municipality",
                                    colname_locality = "locality",
                                    colname_decimalLatitude = "decimalLatitude",
                                    colname_decimalLongitude = "decimalLongitude",
                                    colname_day = "day",
                                    colname_month = "month",
                                    colname_year = "year",
                                    colname_collectionCode = "collectionCode",
                                    colname_institutionCode = "institutionCode",
                                    colname_typeStatus = "typeStatus",
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

  # Clean column names (replace dots with spaces)
  names(df) <- gsub("[.]", " ", names(df))
  names(df) <- trimws(gsub("(\\s){2,}", " ", names(df)))

  # Resolve species columns
  species_cols <- .resolve_cols(df, species_cols, arg = "species_cols")

  # Create mapping of standard names to actual column names
  col_mapping <- list(
    recordedBy = colname_recordedBy,
    recordNumber = colname_recordNumber,
    continent = colname_continent,
    country = colname_country,
    stateProvince = colname_stateProvince,
    county = colname_county,
    municipality = colname_municipality,
    locality = colname_locality,
    decimalLatitude = colname_decimalLatitude,
    decimalLongitude = colname_decimalLongitude,
    day = colname_day,
    month = colname_month,
    year = colname_year,
    collectionCode = colname_collectionCode,
    institutionCode = colname_institutionCode,
    typeStatus = colname_typeStatus
  )

  # Check that all required columns exist
  required_cols <- c("country", "stateProvince", "locality", "decimalLatitude",
                     "decimalLongitude", "day", "month", "year", "recordedBy",
                     "recordNumber", "collectionCode")

  for (req in required_cols) {
    col_name <- col_mapping[[req]]
    if (!col_name %in% names(df)) {
      warning("Column '", col_name, "' (mapped from '", req, "') not found in data. ",
              "This may affect output quality.")
    }
  }

  # Select all needed columns
  all_needed <- unique(c(species_cols, unlist(col_mapping)))
  existing_cols <- intersect(all_needed, names(df))
  missing <- setdiff(all_needed, existing_cols)
  if (length(missing) > 0 && verbose) {
    message("Note: The following columns are missing and will be skipped: ",
            paste(missing, collapse = ", "))
  }

  df <- df[, existing_cols, drop = FALSE]

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
  if (col_mapping$recordedBy %in% names(df) && col_mapping$recordNumber %in% names(df)) {
    df$collector_key <- paste(df[[col_mapping$recordedBy]],
                              df[[col_mapping$recordNumber]],
                              sep = " ## ")
  } else {
    df$collector_key <- paste(seq_len(nrow(df)), "temp", sep = "_")
  }

  # Format coordinates if columns exist
  if (col_mapping$decimalLatitude %in% names(df) && col_mapping$decimalLongitude %in% names(df)) {
    df$coords <- .format_coordinates(df[[col_mapping$decimalLatitude]],
                                     df[[col_mapping$decimalLongitude]])
  } else {
    df$coords <- ""
  }

  # Format date if columns exist
  if (col_mapping$year %in% names(df)) {
    df$date <- .format_date(
      if (col_mapping$day %in% names(df)) df[[col_mapping$day]] else NA,
      if (col_mapping$month %in% names(df)) df[[col_mapping$month]] else NA,
      df[[col_mapping$year]],
      language = language
    )
  } else {
    df$date <- ""
  }

  # Format type status if column exists
  if (col_mapping$typeStatus %in% names(df)) {
    df$type_status <- df[[col_mapping$typeStatus]]
  } else {
    df$type_status <- ""
  }

  # Group by species and geographic hierarchy
  result_list <- list()

  for (sp in unique(df$species_id)) {
    sp_df <- df[df$species_id == sp, ]

    # Create geographic key if country exists
    if (col_mapping$country %in% names(df)) {
      sp_df$geo_key <- paste(
        if (col_mapping$country %in% names(df)) sp_df[[col_mapping$country]] else "",
        if (col_mapping$stateProvince %in% names(df)) sp_df[[col_mapping$stateProvince]] else "",
        if (col_mapping$municipality %in% names(df)) sp_df[[col_mapping$municipality]] else "",
        sep = "||"
      )
    } else {
      sp_df$geo_key <- "No geography||"
    }

    specimens_list <- list()

    for (geo in unique(sp_df$geo_key)) {
      geo_df <- sp_df[sp_df$geo_key == geo, ]

      country <- if (col_mapping$country %in% names(df)) {
        unique(geo_df[[col_mapping$country]])[1]
      } else { "" }

      state <- if (col_mapping$stateProvince %in% names(df)) {
        unique(geo_df[[col_mapping$stateProvince]])[1]
      } else { "" }

      municipality <- if (col_mapping$municipality %in% names(df)) {
        unique(geo_df[[col_mapping$municipality]])[1]
      } else { "" }

      # Translate country if needed
      if (language == "pt" && nzchar(country)) {
        country <- .translate_country(country)
      }

      # Group specimens within this geographic area
      specimens <- character()

      # Group by collector to merge duplicates
      for (collector in unique(geo_df$collector_key)) {
        coll_df <- geo_df[geo_df$collector_key == collector, ]

        # Get unique herbaria
        herbaria <- if (col_mapping$collectionCode %in% names(df)) {
          unique(coll_df[[col_mapping$collectionCode]])
        } else { character(0) }

        herbaria <- herbaria[!is.na(herbaria) & nzchar(herbaria)]
        herbarium_text <- if (length(herbaria) > 0) {
          paste0("(", paste(herbaria, collapse = ", "), ")")
        } else {
          ""
        }

        # Format locality info
        locality <- if (col_mapping$locality %in% names(df)) {
          unique(coll_df[[col_mapping$locality]])[1]
        } else { "" }

        coords <- unique(coll_df$coords)[1]
        date <- unique(coll_df$date)[1]
        type_status <- unique(coll_df$type_status)[1]

        # Add type status if present
        if (nzchar(type_status)) {
          type_text <- paste0("[", type_status, "] ")
        } else {
          type_text <- ""
        }

        # Build specimen entry
        collector_name <- if (col_mapping$recordedBy %in% names(df)) {
          unique(coll_df[[col_mapping$recordedBy]])[1]
        } else { "Anonymous" }

        collector_num <- if (col_mapping$recordNumber %in% names(df)) {
          unique(coll_df[[col_mapping$recordNumber]])[1]
        } else { "" }

        specimen_entry <- .format_specimen_entry(
          locality = locality,
          coords = coords,
          date = date,
          type_status = type_text,
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
      # Skip if no specimens
      if (length(geo_group$specimens) == 0) next

      # Skip if no country (can't format properly)
      if (!nzchar(geo_group$country) && !nzchar(geo_group$state)) {
        # Just add specimens without header
        for (spec in geo_group$specimens) {
          normal_prop <- officer::fp_text(font.family = font_family,
                                          font.size = font_size,
                                          bold = FALSE,
                                          italic = FALSE)
          spec_run <- officer::ftext(paste0("  ", spec), prop = normal_prop)
          doc <- officer::body_add_fpar(doc, officer::fpar(spec_run))
          specimens_text_all <- c(specimens_text_all, spec)
        }
        next
      }

      # Country with bold
      country_prop <- officer::fp_text(font.family = font_family,
                                       font.size = font_size,
                                       bold = country_bold,
                                       italic = FALSE)

      # Format country line based on journal style
      if (journal == "SystematicBotany") {
        if (nzchar(geo_group$state)) {
          country_line <- paste0(geo_group$country, ". —",
                                 toupper(geo_group$state))
          if (nzchar(geo_group$municipality)) {
            country_line <- paste0(country_line, ": ", geo_group$municipality)
          } else {
            country_line <- paste0(country_line, ":")
          }
        } else if (nzchar(geo_group$country)) {
          country_line <- paste0(geo_group$country, ".")
        } else {
          country_line <- ""
        }
      } else {
        # Taxon style
        if (nzchar(geo_group$state)) {
          country_line <- paste0(geo_group$country, ". —",
                                 toupper(geo_group$state))
          if (nzchar(geo_group$municipality)) {
            country_line <- paste0(country_line, ": ", geo_group$municipality)
          } else {
            country_line <- paste0(country_line, ":")
          }
        } else if (nzchar(geo_group$country)) {
          country_line <- paste0(geo_group$country, ".")
        } else {
          country_line <- ""
        }
      }

      if (nzchar(country_line)) {
        country_run <- officer::ftext(country_line, prop = country_prop)
        doc <- officer::body_add_fpar(doc, officer::fpar(country_run))
      }

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

# Helper functions (same as before, included for completeness)

#' Format coordinates for display
#' @keywords internal
#' @noRd
.format_coordinates <- function(lat, lon) {
  if (is.na(lat) || is.na(lon)) return("")
  if (abs(lat) > 90) return("")

  lat_dir <- ifelse(lat >= 0, "N", "S")
  lon_dir <- ifelse(lon >= 0, "E", "W")

  lat_abs <- abs(lat)
  lon_abs <- abs(lon)

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

  month_num <- suppressWarnings(as.numeric(month))
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
.format_specimen_entry <- function(locality, coords, date, type_status,
                                   collector, collector_num, herbaria,
                                   journal = "SystematicBotany") {
  parts <- c()

  if (nzchar(locality)) parts <- c(parts, locality)
  if (nzchar(type_status)) parts <- c(parts, type_status)
  if (nzchar(coords)) parts <- c(parts, coords)
  if (nzchar(date)) parts <- c(parts, date)

  collector_info <- paste(collector, collector_num)
  if (nzchar(herbaria)) {
    collector_info <- paste(collector_info, herbaria)
  }
  parts <- c(parts, collector_info)

  paste(parts, collapse = ", ")
}
