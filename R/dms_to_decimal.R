#' Convert Coordinates in Degrees, Minutes and Seconds into Decimal Degrees
#'
#' @author Domingos Cardoso
#'
#' @description
#' Converts geographic coordinates written in degrees, minutes and seconds
#' (e.g. `"22º22´55´´S"`, `"53°45'W"`, `"12 30 15.5 S"`) into decimal degrees
#' (e.g. `-22.381944`), as they are often recorded in herbarium labels and in the
#' verbatim coordinates of databases like REFLORA, JABOT and speciesLink.
#'
#' @details
#' The function recognizes:
#' - degree symbols `º`, `°`, `˚` or `d`; minute symbols `'`, `´`, `′`, `’` or `m`;
#'   second symbols `"`, `''`, `´´`, `″`, `”` or `s`, or just spaces between the
#'   numbers;
#' - decimal minutes or seconds, with dot or comma (e.g. `"12°30,5'S"`);
#' - the hemisphere as `N`, `S`, `E`, `W`, or `O` (oeste) and `L` (leste) as in
#'   Portuguese and Spanish, before or after the numbers, or a minus sign;
#' - coordinates already in decimal degrees (e.g. `"-12.5"`), which are kept.
#'
#' Values that cannot be converted, or with minutes or seconds of 60 or more, or
#' out of the range of latitude (±90) or longitude (±180), are returned as `NA`,
#' with a warning.
#'
#' When the hemisphere is not written (e.g. `"22º22´55´´"`), the result is positive,
#' unless it is given in `hemisphere`, e.g. `hemisphere = "S"` for latitudes of
#' Brazilian specimens.
#'
#' @usage
#' dms_to_decimal(x,
#'                type = c("auto", "latitude", "longitude"),
#'                hemisphere = NULL,
#'                digits = 6)
#'
#' @param x Character vector of coordinates.
#' @param type Either `"latitude"`, `"longitude"` or `"auto"` (default). It is used
#' to check the range of the coordinates; with `"auto"`, the range depends on the
#' hemisphere (±90 for N and S, ±180 otherwise).
#' @param hemisphere Optional hemisphere (`"N"`, `"S"`, `"E"` or `"W"`) for the
#' coordinates written without hemisphere or sign.
#' @param digits Number of decimal places of the result (default: `6`, about 10 cm).
#'
#' @return A numeric vector of coordinates in decimal degrees.
#'
#' @examples
#' dms_to_decimal(c("22º22´55´´S", "53°45'W", "12 30 15.5 S", "S 8°30.5'", "-55.602222"))
#'
#' # Latitudes without hemisphere
#' dms_to_decimal(c("22º22´55´´", "1º42´12´´"), type = "latitude", hemisphere = "S")
#'
#' # Longitude in Portuguese (O = oeste = west)
#' dms_to_decimal("43º40'49''O")
#'
#' @export

dms_to_decimal <- function(x,
                           type = c("auto", "latitude", "longitude"),
                           hemisphere = NULL,
                           digits = 6) {

  type <- match.arg(type)
  if (!is.null(hemisphere) &&
      (length(hemisphere) != 1 || !toupper(hemisphere) %in% c("N", "S", "E", "W"))) {
    stop("hemisphere must be one of 'N', 'S', 'E' or 'W'.", call. = FALSE)
  }

  txt <- trimws(as.character(x))
  txt[txt %in% c("", "NA")] <- NA
  txt <- toupper(txt)

  # Letters used as symbols between the numbers, e.g. "12D30M15SS" (S = seconds,
  # then S = south), become spaces
  txt <- gsub("(?<=[0-9])[DMS](?=\\s*[0-9]|[NSEWOL]$)", " ", txt, perl = TRUE)

  # Hemisphere letter, before or after the numbers
  hem_all <- rep(NA_character_, length(txt))
  at_end <- grepl("(?<![A-Z])[NSEWOL]$", txt, perl = TRUE)
  at_start <- grepl("^[NSEWOL](?![A-Z])", txt, perl = TRUE)
  hem_all[at_end] <- substring(txt[at_end], nchar(txt[at_end]))
  hem_all[at_start] <- substr(txt[at_start], 1, 1)
  hem_all <- chartr("OL", "WE", hem_all)
  negative <- grepl("^\\s*-", txt)

  # Numbers: degrees, minutes and seconds, with dot or comma as decimal mark
  numbers <- regmatches(txt, gregexpr("[0-9]+([.,][0-9]+)?", txt))
  numbers <- lapply(numbers, function(v) as.numeric(sub(",", ".", v)))

  # Anything else than numbers, symbols, spaces, sign and the hemisphere letter
  # at the start or end is not valid
  core <- txt
  core[at_end] <- sub("[NSEWOL]$", "", core[at_end])
  core[at_start] <- sub("^[NSEWOL]", "", core[at_start])
  rest <- gsub("[-0-9.,[:space:]\u00ba\u00b0\u02da'\"\u00b4\u2032\u2033\u2019\u201d]", "", core)
  invalid_text <- !is.na(txt) & nzchar(rest)

  out <- vapply(seq_along(txt), function(i) {
    v <- numbers[[i]]
    if (is.na(txt[i]) || invalid_text[i] || length(v) == 0 || length(v) > 3) return(NA_real_)
    v <- c(v, 0, 0)[1:3]
    if (v[2] >= 60 || v[3] >= 60) return(NA_real_)
    v[1] + v[2] / 60 + v[3] / 3600
  }, numeric(1))

  # Sign from the hemisphere, the minus sign or the default hemisphere
  hem_used <- hem_all
  if (!is.null(hemisphere)) {
    hem_used[is.na(hem_used) & !negative] <- toupper(hemisphere)
  }
  out[negative | hem_used %in% c("S", "W")] <- -out[negative | hem_used %in% c("S", "W")]

  # Range of latitude or longitude
  max_value <- switch(type,
                      latitude = rep(90, length(out)),
                      longitude = rep(180, length(out)),
                      auto = ifelse(hem_all %in% c("N", "S"), 90, 180))
  out[!is.na(out) & abs(out) > max_value] <- NA

  failed <- !is.na(txt) & is.na(out)
  if (any(failed)) {
    warning(sum(failed), " coordinate(s) could not be converted and were set to NA, e.g. '",
            paste(utils::head(unique(x[failed]), 3), collapse = "', '"), "'.", call. = FALSE)
  }

  round(out, digits)
}
