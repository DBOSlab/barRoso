#' Standardize the Day, Month and Year of Collection
#'
#' @author Domingos Cardoso
#'
#' @description
#' Cleans the `day`, `month` and `year` columns of herbarium records into whole
#' numbers, fixing the most common errors of these columns, and flags the dates
#' that could not be fixed.
#'
#' @details
#' The following changes are made:
#' - Numbers written with zeros or decimals become whole numbers, e.g. `"06"` and
#'   `"6.0"` into `6`, and zeros like `"0"` or `"00"`, used for unknown days and
#'   months, become `NA`.
#' - Months written as names or Roman numerals in English, Portuguese, Spanish or
#'   Latin become numbers, e.g. `"Jun"`, `"junho"`, `"junio"` and `"VI"` into `6`.
#' - Days and months in the wrong columns are swapped, which can only be known when
#'   the month is greater than 12 and the day is not, e.g. month `23` and day `7`
#'   into day `23` and month `7`.
#' - Missing days, months and years are taken from `eventDate`, e.g. `"1995-07-23"`
#'   or `"23/7/1995"`, when present.
#' - Values that are not possible dates become `NA`, e.g. month `14` with day `15`,
#'   day `31` in April, day `29` in February of a year that is not a leap year, or
#'   years before 1700 or after the current year.
#'
#' The column `dateIssues` tells what was changed in each record, e.g.
#' `"day and month swapped"`, `"invalid month"`, `"invalid day"`, `"invalid year"`
#' or `"date from eventDate"`, and is `NA` for records without changes beyond the
#' number format.
#'
#' @usage
#' std_dates(df = NULL,
#'           colname_day = "day",
#'           colname_month = "month",
#'           colname_year = "year",
#'           colname_eventDate = "eventDate",
#'           rm_original_column = TRUE)
#'
#' @param df A data frame with herbarium records.
#' @param colname_day Column name for the day of collection (default: `"day"`).
#' @param colname_month Column name for the month of collection (default: `"month"`).
#' @param colname_year Column name for the year of collection (default: `"year"`).
#' @param colname_eventDate Column name for the date of collection, used to fill in
#' missing days, months and years. Ignored when not in `df` (default: `"eventDate"`).
#' @param rm_original_column Logical; if `FALSE`, the original columns are kept
#' with the suffix `Original`, e.g. `monthOriginal` (default: `TRUE`).
#'
#' @return The data frame with the day, month and year as whole numbers, and the
#' column `dateIssues` after them.
#'
#' @examples
#' df <- data.frame(day = c("07", "23", "00", "31", NA),
#'                  month = c("06", "7", "Jun", "IV", NA),
#'                  year = c("1995", "2001", "1987.0", "2010", NA),
#'                  eventDate = c(NA, NA, NA, NA, "1995-07-23"))
#' std_dates(df)
#'
#' @importFrom tibble add_column
#'
#' @export

std_dates <- function(df = NULL,
                      colname_day = "day",
                      colname_month = "month",
                      colname_year = "year",
                      colname_eventDate = "eventDate",
                      rm_original_column = TRUE) {

  # Check the arguments ####
  .check_df(df)
  .check_flag(rm_original_column)
  .check_colnames(colname_day = colname_day, colname_month = colname_month,
                  colname_year = colname_year, colname_eventDate = colname_eventDate)
  cols <- c(colname_day, colname_month, colname_year)
  if (!any(c(cols, colname_eventDate) %in% names(df))) {
    stop(paste0("No date columns in `df`: '", paste(c(cols, colname_eventDate), collapse = "', '"),
                "'. If they have other names, set `colname_day`, `colname_month`, ",
                "`colname_year` or `colname_eventDate`."), call. = FALSE)
  }

  # Missing columns are created, to be filled in from eventDate
  for (col in cols[!cols %in% names(df)]) df[[col]] <- NA

  day <- .date_number(df[[colname_day]])
  month <- .month_number(df[[colname_month]])
  year <- .date_number(df[[colname_year]])
  issues <- rep(NA_character_, nrow(df))
  add_issue <- function(tf, text) {
    tf <- tf %in% TRUE
    issues[tf] <<- ifelse(is.na(issues[tf]), text, paste(issues[tf], text, sep = "; "))
  }

  # Missing values from eventDate, e.g. "1995-07-23" or "23/7/1995" ####
  if (colname_eventDate %in% names(df)) {
    # Without times and the end of intervals, e.g. "1995-07-23T00:00:00", "1990-10-09/1990-10-12"
    ev <- sub("T.*$", "", trimws(as.character(df[[colname_eventDate]])))
    iso <- .iso_date(sub("/.*$", "", .iso_date(ev)))
    iso[!grepl("^[0-9]{4}(-[0-9]{2}){0,2}$", iso)] <- NA
    parts <- strsplit(ifelse(is.na(iso), "", iso), "-")
    get <- function(k) suppressWarnings(as.integer(vapply(parts, `[`, character(1), k)))
    fill <- function(x, v) {
      tf <- is.na(x) & !is.na(v) & v != 0
      x[tf] <- v[tf]
      list(x = x, tf = tf)
    }
    # Days and months only for records of the same year, or without year
    same_year <- is.na(year) | year %in% get(1)
    y <- fill(year, get(1)); year <- y$x
    m <- fill(month, ifelse(same_year, get(2), NA)); month <- m$x
    d <- fill(day, ifelse(same_year & !is.na(month) & month %in% get(2), get(3), NA)); day <- d$x
    add_issue(y$tf | m$tf | d$tf, "date from eventDate")
  }

  # Days and months in the wrong columns, e.g. month 23 and day 7 ####
  swap <- !is.na(month) & month > 12 & month <= 31 & !is.na(day) & day >= 1 & day <= 12
  if (any(swap)) {
    tmp <- day[swap]
    day[swap] <- month[swap]
    month[swap] <- tmp
    add_issue(swap, "day and month swapped")
  }

  # Values that are not possible dates ####
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  bad_year <- !is.na(year) & (year < 1700 | year > this_year)
  year[bad_year] <- NA
  add_issue(bad_year, "invalid year")

  bad_month <- !is.na(month) & (month < 1 | month > 12)
  month[bad_month] <- NA
  add_issue(bad_month, "invalid month")

  bad_day <- !is.na(day) & (day < 1 | day > .days_in_month(month, year))
  day[bad_day] <- NA
  add_issue(bad_day, "invalid day")

  # Write the columns ####
  if (!rm_original_column) {
    for (col in cols) {
      df <- tibble::add_column(df, !!!stats::setNames(list(df[[col]]), paste0(col, "Original")),
                               .before = col)
    }
  }
  df[[colname_day]] <- day
  df[[colname_month]] <- month
  df[[colname_year]] <- year
  df <- df[, names(df) != "dateIssues", drop = FALSE]
  after <- names(df)[max(match(cols, names(df)))]
  df <- tibble::add_column(df, dateIssues = issues, .after = after)

  n <- sum(!is.na(issues))
  if (n > 0) {
    message(paste0("std_dates: ", n, " dates fixed or flagged (dateIssues): ",
                   paste(names(table(unlist(strsplit(issues[!is.na(issues)], "; ")))),
                         table(unlist(strsplit(issues[!is.na(issues)], "; "))),
                         sep = " ", collapse = ", ")))
  }

  return(df)
}


#_______________________________________________________________________________
# Whole numbers, e.g. "06" and "6.0" into 6, and zeros into NA ####
.date_number <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("^([0-9]+)[.,]0+$", "\\1", x)
  out <- suppressWarnings(as.integer(ifelse(grepl("^[0-9]+$", x), x, NA)))
  out[out %in% 0L] <- NA
  out
}

# Months as numbers, also from names and Roman numerals, e.g. "Jun", "junho",
# "junio", "Iunius", "VI" into 6 ####
.month_number <- function(x) {
  out <- .date_number(x)
  txt <- tolower(.ascii_squish(as.character(x)))
  txt <- sub("[.]$", "", txt)
  roman <- c("i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix", "x", "xi", "xii")
  names <- list(c("jan", "janeiro", "enero", "ene", "ianuarius", "januarius"),
                c("feb", "fev", "fevereiro", "febrero", "februarius"),
                c("mar", "marco", "marzo", "martius"),
                c("apr", "abr", "abril", "aprilis"),
                c("may", "mai", "maio", "mayo", "maius"),
                c("jun", "junho", "junio", "iunius", "junius"),
                c("jul", "julho", "julio", "iulius", "julius"),
                c("aug", "ago", "agosto", "augustus"),
                c("sep", "sept", "set", "setembro", "septiembre", "setiembre", "september"),
                c("oct", "out", "outubro", "octubre", "october"),
                c("nov", "novembro", "noviembre", "november"),
                c("dec", "dez", "dezembro", "diciembre", "dic", "december"))
  english <- tolower(month.name)
  for (m in 1:12) {
    words <- c(names[[m]], english[m], roman[m])
    tf <- is.na(out) & txt %in% words
    out[tf] <- m
  }
  out
}

# Number of days of each month, with 29 days in February of unknown or leap years ####
.days_in_month <- function(month, year) {
  days <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[month]
  days[is.na(month)] <- 31
  leap <- is.na(year) | (year %% 4 == 0 & (year %% 100 != 0 | year %% 400 == 0))
  days[month %in% 2 & !leap] <- 28
  days
}
