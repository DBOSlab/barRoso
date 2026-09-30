#' Fix and Flag Geographic Coordinates in Biodiversity Records
#'
#' @author Domingos Cardoso
#'
#' @description
#' Checks the geographic coordinates of biodiversity records following the
#' workflow of the \href{https://brunobrr.github.io/bdc/}{bdc} package. Coordinates
#' with latitude and longitude transposed or with inverted signs are fixed, while
#' all other problems are flagged, so the user knows what was fixed and which
#' records need attention.
#'
#' @details
#' The function applies, in this order:
#' 1. Parsing of coordinates stored as text, e.g. with decimal commas (`"-12,5"`).
#'    Values that cannot be read as numbers (e.g. `"Bloqueada"` in speciesLink)
#'    are set to `NA` and flagged.
#' 2. `bdc::bdc_coordinates_empty()` and `bdc::bdc_coordinates_outOfRange()`.
#' 3. `bdc::bdc_country_standardized()`, used internally to get the country codes
#'    (the original country column is not changed).
#' 4. `bdc::bdc_coordinates_transposed()`, which FIXES coordinates with latitude
#'    and longitude swapped and/or with inverted signs, based on the country of
#'    each record.
#' 5. `bdc::bdc_coordinates_country_inconsistent()`, for coordinates falling
#'    outside the country informed in the record.
#' 6. `bdc::bdc_coordinates_precision()`, for coordinates with few decimals.
#' 7. `CoordinateCleaner::clean_coordinates()`, with the tests used in the space
#'    module of bdc that do not need downloads: country capitals, country and
#'    province centroids, biodiversity institutions, zero and equal coordinates,
#'    and the GBIF headquarters.
#'
#' The flag columns follow the bdc convention, where `TRUE` means the record passed
#' the test and `FALSE` means it was flagged: `.coordinates_empty`,
#' `.coordinates_outOfRange`, `.coordinates_country_inconsistent`, `.rou`, `.equ`,
#' `.zer`, `.cap`, `.cen`, `.gbf`, `.inst`. As in bdc, the column
#' `coordinates_transposed` has no leading dot because the problem was fixed
#' (`FALSE` means the coordinates were corrected). So, the output can be used
#' directly with `bdc::bdc_summary_col()` and `bdc::bdc_filter_out_flags()`.
#'
#' A column `coordinateIssues` describes in words what was fixed or flagged in
#' each record, e.g. `"transposed, fixed (latitude and longitude swapped); low precision"`.
#'
#' @usage
#' std_coordinates(df = NULL,
#'                 colname_decimalLatitude = "decimalLatitude",
#'                 colname_decimalLongitude = "decimalLongitude",
#'                 colname_country = "country",
#'                 colname_scientificName = "scientificName",
#'                 fix_transposed = TRUE,
#'                 check_country = TRUE,
#'                 check_space = TRUE,
#'                 min_decimals = 2,
#'                 rm_original_column = FALSE)
#'
#' @param df A data frame containing biodiversity records.
#' @param colname_decimalLatitude Column name for latitude in decimal degrees
#' (default: `"decimalLatitude"`).
#' @param colname_decimalLongitude Column name for longitude in decimal degrees
#' (default: `"decimalLongitude"`).
#' @param colname_country Column name for country (default: `"country"`). If the
#' column is not found, the country-based tests are skipped.
#' @param colname_scientificName Column name for the taxon name, used to report
#' the transposed coordinates (default: `"scientificName"`).
#' @param fix_transposed Logical; if `TRUE`, fix transposed coordinates (default: `TRUE`).
#' @param check_country Logical; if `TRUE`, flag coordinates outside the informed
#' country (default: `TRUE`).
#' @param check_space Logical; if `TRUE`, flag coordinates at capitals, centroids,
#' institutions, zero or equal coordinates and GBIF headquarters (default: `TRUE`).
#' @param min_decimals Minimum number of decimals of both latitude and longitude,
#' below which coordinates are flagged as low precision (default: `2`).
#' @param rm_original_column Logical; if `TRUE`, the original coordinate columns are
#' removed. If `FALSE`, they are retained with the `*Original` suffix (default: `FALSE`).
#'
#' @return The input data frame with numeric coordinates, with transposed coordinates
#' fixed, plus the bdc-like flag columns and the column `coordinateIssues`.
#'
#' @examples
#' \dontrun{
#' df <- read.csv("gbif_download.csv")
#' df_coords <- std_coordinates(df)
#'
#' # Summarize the flags and remove flagged records with bdc
#' df_coords <- bdc::bdc_summary_col(df_coords)
#' df_clean <- bdc::bdc_filter_out_flags(df_coords, col_to_remove = "all")
#' }
#'
#' @export

std_coordinates <- function(df = NULL,
                            colname_decimalLatitude = "decimalLatitude",
                            colname_decimalLongitude = "decimalLongitude",
                            colname_country = "country",
                            colname_scientificName = "scientificName",
                            fix_transposed = TRUE,
                            check_country = TRUE,
                            check_space = TRUE,
                            min_decimals = 2,
                            rm_original_column = FALSE) {

  .check_suggests(c("bdc", "CoordinateCleaner", "rnaturalearth",
                    "rnaturalearthdata", "rnaturalearthhires", "sf"))

  lat_col <- colname_decimalLatitude
  lon_col <- colname_decimalLongitude
  missing <- setdiff(c(lat_col, lon_col), names(df))
  if (length(missing) > 0) {
    stop(paste0("Column(s) not found in the data frame: ", paste(missing, collapse = ", ")),
         call. = FALSE)
  }
  has_country <- colname_country %in% names(df)
  if (!has_country && (fix_transposed || check_country)) {
    warning(paste0("Column '", colname_country, "' not found. ",
                   "Transposed and country-inconsistent coordinates will not be checked."),
            call. = FALSE)
  }

  message("std_coordinates $", lat_col, " and $", lon_col)

  n <- nrow(df)
  issues <- rep("", n)
  add_issue <- function(issues, tf, text) {
    tf <- !is.na(tf) & tf
    issues[tf] <- ifelse(issues[tf] == "", text, paste(issues[tf], text, sep = "; "))
    issues
  }

  # Keep the original coordinates ####
  if (!rm_original_column) {
    df <- tibble::add_column(df, !!!stats::setNames(list(df[[lat_col]]), paste0(lat_col, "Original")),
                             .before = lat_col)
    df <- tibble::add_column(df, !!!stats::setNames(list(df[[lon_col]]), paste0(lon_col, "Original")),
                             .before = lon_col)
  }

  # 1. Coordinates stored as text ####
  lat <- .parse_coord(df[[lat_col]])
  lon <- .parse_coord(df[[lon_col]])
  issues <- add_issue(issues, attr(lat, "not_numeric") | attr(lon, "not_numeric"),
                      "not numeric, set to NA")
  lat <- as.numeric(lat)
  lon <- as.numeric(lon)

  # Temporary data with the bdc column names
  tmp <- data.frame(database_id = seq_len(n),
                    scientificName = if (colname_scientificName %in% names(df)) {
                      as.character(df[[colname_scientificName]])
                    } else {
                      NA_character_
                    },
                    decimalLatitude = lat,
                    decimalLongitude = lon,
                    country = if (has_country) as.character(df[[colname_country]]) else NA_character_)

  # 2. Empty and out of range coordinates ####
  tmp <- suppressMessages(bdc::bdc_coordinates_empty(tmp))
  tmp <- suppressMessages(bdc::bdc_coordinates_outOfRange(tmp))
  issues <- add_issue(issues, !tmp$.coordinates_empty, "empty")
  issues <- add_issue(issues, !tmp$.coordinates_outOfRange, "out of range")
  valid <- tmp$.coordinates_empty & tmp$.coordinates_outOfRange

  # 3. Standardized country names and codes, just for the next tests ####
  transposed <- rep(TRUE, n)
  country_inconsistent <- rep(TRUE, n)
  if (has_country && (fix_transposed || check_country) && any(valid & !is.na(tmp$country))) {
    cntr <- suppressMessages(suppressWarnings(
      bdc::bdc_country_standardized(tmp[!is.na(tmp$country), c("database_id", "country")])))
    code <- cntr$countryCode[match(tmp$database_id, cntr$database_id)]
    code[!valid] <- NA

    old_s2 <- sf::sf_use_s2()
    suppressMessages(sf::sf_use_s2(FALSE))
    on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
    world <- .world_map()
    polys <- new.env()

    # 4. Fix transposed coordinates ####
    # As in bdc::bdc_coordinates_transposed, coordinates outside their country
    # (with a buffer of 0.2 degrees) are tested with latitude and longitude
    # swapped and/or with inverted signs, and the first combination falling
    # inside the country is kept
    if (fix_transposed) {
      outside <- .in_country(tmp$decimalLatitude, tmp$decimalLongitude, code, world, polys, 0.2) %in% FALSE
      la <- tmp$decimalLatitude
      lo <- tmp$decimalLongitude
      trans <- list("sign of latitude inverted" = list(-la, lo),
                    "sign of longitude inverted" = list(la, -lo),
                    "signs of latitude and longitude inverted" = list(-la, -lo),
                    "latitude and longitude swapped" = list(lo, la),
                    "latitude and longitude swapped, and sign of latitude inverted" = list(-lo, la),
                    "latitude and longitude swapped, and sign of longitude inverted" = list(lo, -la),
                    "latitude and longitude swapped, and signs inverted" = list(-lo, -la))
      for (how in names(trans)) {
        if (!any(outside)) break
        new_la <- trans[[how]][[1]]
        new_lo <- trans[[how]][[2]]
        tf <- outside & abs(new_la) <= 90 & abs(new_lo) <= 180
        tf[tf] <- .in_country(new_la[tf], new_lo[tf], code[tf], world, polys, 0) %in% TRUE
        if (any(tf)) {
          tmp$decimalLatitude[tf] <- new_la[tf]
          tmp$decimalLongitude[tf] <- new_lo[tf]
          transposed[tf] <- FALSE
          outside[tf] <- FALSE
          issues <- add_issue(issues, tf, paste0("transposed, fixed (", how, ")"))
        }
      }
    }

    # 5. Coordinates outside the informed country ####
    # As in bdc::bdc_coordinates_country_inconsistent, with a buffer of 0.1 degrees
    if (check_country) {
      inside <- .in_country(tmp$decimalLatitude, tmp$decimalLongitude, code, world, polys, 0.1)
      country_inconsistent <- !(inside %in% FALSE)
      issues <- add_issue(issues, !country_inconsistent, "outside the informed country")
    }
  }

  # 6. Low precision ####
  rou <- rep(TRUE, n)
  if (any(valid)) {
    pr <- suppressMessages(bdc::bdc_coordinates_precision(tmp[valid, ], ndec = 0:min_decimals))
    rou[valid] <- pr$.rou
    issues <- add_issue(issues, !rou, paste0("low precision (< ", min_decimals, " decimals)"))
  }

  # 7. Capitals, centroids, institutions, zeros, equal coordinates and GBIF ####
  space <- NULL
  if (check_space && any(valid)) {
    tests <- c(".equ" = "equal latitude and longitude",
               ".zer" = "zero coordinates",
               ".cap" = "country capital",
               ".cen" = "country or province centroid",
               ".gbf" = "GBIF headquarters",
               ".inst" = "biodiversity institution")
    cc <- suppressMessages(suppressWarnings(
      CoordinateCleaner::clean_coordinates(
        data.frame(species = ifelse(is.na(tmp$scientificName[valid]), "sp",
                                    tmp$scientificName[valid]),
                   decimalLongitude = tmp$decimalLongitude[valid],
                   decimalLatitude = tmp$decimalLatitude[valid]),
        tests = c("equal", "zeros", "capitals", "centroids", "gbif", "institutions"),
        value = "spatialvalid", verbose = FALSE)))
    space <- lapply(names(tests), function(t) {
      v <- rep(TRUE, n)
      v[valid] <- cc[[t]]
      v
    })
    names(space) <- names(tests)
    for (t in names(tests)) issues <- add_issue(issues, !space[[t]], tests[[t]])
  }

  # Put the cleaned coordinates and the flags back ####
  df[[lat_col]] <- tmp$decimalLatitude
  df[[lon_col]] <- tmp$decimalLongitude
  df$.coordinates_empty <- tmp$.coordinates_empty
  df$.coordinates_outOfRange <- tmp$.coordinates_outOfRange
  if (has_country && fix_transposed) df$coordinates_transposed <- transposed
  if (has_country && check_country) df$.coordinates_country_inconsistent <- country_inconsistent
  df$.rou <- rou
  for (t in names(space)) df[[t]] <- space[[t]]
  df$coordinateIssues <- ifelse(issues == "", NA_character_, issues)

  message(paste0("std_coordinates: ", sum(!transposed), " transposed coordinates fixed; ",
                 sum(issues != "" & tmp$.coordinates_empty), " records with coordinates flagged; ",
                 sum(!tmp$.coordinates_empty), " records without coordinates."))

  return(df)
}


#_______________________________________________________________________________
# Read coordinates stored as text, e.g. "-12,5" or " -12.5 " ####
.parse_coord <- function(x) {
  if (is.numeric(x)) {
    attr(x, "not_numeric") <- rep(FALSE, length(x))
    return(x)
  }
  x <- trimws(as.character(x))
  x[x %in% c("", "NA")] <- NA
  x <- sub("^([+-]?[0-9]+),([0-9]+)$", "\\1.\\2", x)
  num <- suppressWarnings(as.numeric(x))
  attr(num, "not_numeric") <- !is.na(x) & is.na(num)
  num
}

# Countries of the Natural Earth map used by bdc, with ISO2 codes ####
.world_map <- function() {
  w <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
  w$iso2c <- ifelse(is.na(w$iso_a2) | w$iso_a2 == "-99", w$iso_a2_eh, w$iso_a2)
  w[!is.na(w$iso2c) & w$iso2c != "-99", "iso2c"]
}

# Test whether coordinates fall inside the country of each record, given by
# ISO2 codes; NA when the test is not possible. Buffered country polygons are
# kept in the environment `polys` to be reused
.in_country <- function(lat, lon, code, world, polys, buffer = 0) {
  out <- rep(NA, length(lat))
  ok <- !is.na(lat) & !is.na(lon) & !is.na(code)
  for (cc in unique(code[ok])) {
    key <- paste(cc, buffer)
    if (is.null(polys[[key]])) {
      poly <- suppressMessages(sf::st_union(world[world$iso2c == cc, ]))
      if (buffer > 0 && length(poly) > 0) {
        poly <- suppressMessages(suppressWarnings(sf::st_buffer(poly, buffer)))
      }
      polys[[key]] <- poly
    }
    poly <- polys[[key]]
    if (length(poly) == 0) next
    i <- which(ok & code == cc)
    pts <- sf::st_as_sf(data.frame(x = lon[i], y = lat[i]), coords = c("x", "y"),
                        crs = sf::st_crs(world))
    out[i] <- suppressMessages(suppressWarnings(lengths(sf::st_intersects(pts, poly)) > 0))
  }
  out
}

# Check that suggested packages are installed ####
.check_suggests <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(paste0("Please install the package(s): ", paste(missing, collapse = ", "),
                ", e.g. install.packages(c(\"", paste(missing, collapse = "\", \""), "\"))"),
         call. = FALSE)
  }
}
