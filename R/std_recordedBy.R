#' Standardize Collector Names in Biodiversity Records
#'
#' @author Domingos Cardoso
#'
#' @description
#' Cleans and standardizes the `recordedBy` and `recordNumber` fields in biodiversity
#' collection data, consolidating collector names and removing inconsistencies across
#' herbarium records. The function identifies and formats collector initials, extracts
#' main collector names, and handles multilingual and complex name structures including
#' multiple collectors, Asian unicode names, and Brazilian surname conventions.
#'
#' @details
#' This function is part of the `barRoso` package. It supports reconciliation of
#' biodiversity records, especially for resolving collector name discrepancies
#' across duplicate specimens. A new column `addCollector` is created when multiple
#' collectors are detected, storing secondary collectors as `"et al."`. Original
#' columns can be preserved or overwritten.
#'
#' Specifically, this function performs extensive string cleaning including:
#' - Converting Chinese names written as unicode escapes (e.g. `"<U+674E><U+5149><U+7167>"`)
#'   into abbreviated Latin names, as Chinese authors cite themselves (e.g. `"G. Z. Li"`)
#' - Parsing and normalizing collector names split by `&`, `and`, `e`, `y`, `;`, `|`, etc.
#' - Handling cases of one, two, or more collectors
#' - Cleaning spacing, punctuation, and known collector aliases
#' - Adding standardized initials or removing redundant suffixes (e.g., "et al.")
#'
#' @usage
#' std_recordedBy(df = NULL,
#'                colname_recordedBy = "recordedBy",
#'                colname_recordNumber = "recordNumber",
#'                rm_original_column = FALSE)
#'
#' @param df A data frame containing biodiversity records.
#' @param colname_recordedBy Column name for the main collector (default: "recordedBy").
#' @param colname_recordNumber Column name for the collector number (default: "recordNumber").
#' @param rm_original_column Logical; if `TRUE`, original columns are removed after
#' cleaning. If `FALSE`, they are retained with `*Original` suffixes (default: `FALSE`).
#'
#' @return A data frame with cleaned and harmonized collector name fields. A new column
#' `addCollector` is added where additional collectors are identified.
#'
#' @examples
#' \dontrun{
#' df <- read.csv("herbarium_records.csv")
#' df_clean <- std_recordedBy(df,
#'                            colname_recordedBy = "coletor",
#'                            colname_recordNumber = "num_coleta",
#'                            rm_original_column = FALSE)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom tibble add_column
#' @importFrom stringr str_extract
#' @importFrom stringi stri_replace_first_regex stri_trans_general stri_unescape_unicode
#'
#' @export

std_recordedBy <- function(df = NULL,
                           colname_recordedBy = "recordedBy",
                           colname_recordNumber = "recordNumber",
                           rm_original_column = FALSE) {

  # Adjust colnames in the input dataset ####
  colnames_df <- names(df)
  if (colname_recordedBy != "recordedBy") {
    names(df)[colnames_df %in% colname_recordedBy] <- "recordedBy"
  }
  if (colname_recordNumber != "recordNumber") {
    names(df)[colnames_df %in% colname_recordNumber] <- "recordNumber"
  }

  # Adding new columns in the main database
  df <- df %>%
    tibble::add_column(recordedByOriginal = df$recordedBy,
                       .before = "recordedBy") %>%
    tibble::add_column(recordNumberOriginal = df$recordNumber,
                       .before = "recordedBy") %>%
    tibble::add_column(addCollector = NA,
                       .after = "recordedBy")

  # Make the column recordedBy as character before using grepl and gsub
  df$recordedBy <- as.character(df$recordedBy)

  # Decode HTML entities like "Co&#234;lho" and drop prefixes like "Collector(s): "
  df$recordedBy <- .decode_html(df$recordedBy)
  df$recordedBy <- gsub("^Collector[(]s[)]:\\s*(unknown,\\s*)?", "", df$recordedBy)

  # Links to useful codes and regular expressions for filtering char patterns
  # by using regular expressions [regex]
  # https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf
  # https://stringr.tidyverse.org/articles/regular-expressions.html

  #_____________________________________________________________________________
  # Asian-like names written as unicode escapes, e.g. "<U+674E><U+5149><U+7167>" ####
  # Split multiple collectors and convert them into Latin names before any other
  # cleaning, as the steps below would otherwise break the escape codes
  # The converted names like "L. G. Zhao" are kept aside and put back after
  # cleaning, so the steps for Latin names do not change them
  tf <- grepl("<U[+][0-9A-Fa-f]{4}>", df$recordedBy)
  han <- han_add <- rep(FALSE, nrow(df))
  if (any(tf)) {
    df <- .preunicodeclean(df, tf)
    han <- .is_han(df$recordedBy)
    han_add <- .is_han(df$addCollector)
    df <- .unicodeclean(df)
    han_names <- df$recordedBy[han]
    han_addnames <- df$addCollector[han_add]
  }

  #_____________________________________________________________________________
  # When there are MORE THAN TWO COLLECTORS
  # Keep just the main collector and remove all other additional collectors
  # but paste ET AL. in the the newly created $addCollector
  df <- .deletal(df)

  #_____________________________________________________________________________
  # Pre-cleaning $recordedBy and $recordNumber
  df <- .prenbrclean(df)

  #_____________________________________________________________________________
  # Pre-cleaning $recordedBy
  df <- .precollclean(df)

  #_____________________________________________________________________________
  # When there are ONLY TWO COLLECTORS
  # Keep just the main collector but put the additional one at $addCollector
  df <- .split_two_collectors(df)

  #_____________________________________________________________________________
  # Delete particles like de, da, do, dos
  df$recordedBy <- .rm_particles(df$recordedBy)

  #_____________________________________________________________________________
  # Cleaning some unusual formats of collector names at specific collections ####
  if (any(df$collectionCode %in% "CEN") |
      any(df$institutionCode %in% "UTEP")) {
    df <- .precollherb(df)
  }

  #_____________________________________________________________________________
  # Standardize the collector name into the format "D. B. O. S. Cardoso" ####
  df$recordedBy <- .std_initials(df$recordedBy)

  #_____________________________________________________________________________
  # Putting back original collectors when written as "Expeditions", etc
  tf <- grepl("\\sExpedition|^Flora\\sof\\s|\\sProject", df$recordedByOriginal)
  if (any(tf)) {
    df$recordedBy[tf] <- df$recordedByOriginal[tf]
    df$recordNumber[tf] <- df$recordNumberOriginal[tf]
  }

  #_____________________________________________________________________________
  # Particles before surnames, based on the original collector column ####
  df$recordedBy <- .std_particles(df$recordedBy, df$recordedByOriginal)

  #_____________________________________________________________________________
  # Final cleaning of the collector names
  df$recordedBy <- .final_collclean(df$recordedBy)
  if (any(han)) df$recordedBy[han] <- han_names

  #_____________________________________________________________________________
  # Cleaning specific collector names ####
  df <- .std_specific_coll(df)

  #_____________________________________________________________________________
  # Clean the newly created column of additional collectors
  df <- .addcollclean(df)
  if (any(han_add)) df$addCollector[han_add] <- han_addnames

  #_____________________________________________________________________________
  # Furthern cleaning numbers at $recordNumber
  df <- .std_recordNumber(df)

  #_____________________________________________________________________________
  # Put original names back ####
  if (colname_recordedBy != "recordedBy") {
    names(df)[names(df) %in% "recordedBy"] <- colname_recordedBy
    names(df)[names(df) %in% "recordedByOriginal"] <- paste0(colname_recordedBy, "Original")
  }
  if (colname_recordNumber != "recordNumber") {
    names(df)[names(df) %in% "recordNumber"] <- colname_recordNumber
    names(df)[names(df) %in% "recordNumberOriginal"] <- paste0(colname_recordNumber, "Original")
  }

  # Remove original recordedBy and recordNumber columns ####
  if (rm_original_column) {
    message(paste0("Original uncleaned '", colname_recordedBy, "' ", "and '", colname_recordNumber, "' columns removed"))

    df <- df[ , !grepl("Original$", names(df))]
  }

  return(df)
}


#_______________________________________________________________________________
# Small helpers for recurrent string operations ####

# Extract the first match of a pattern, with "" when there is no match
.extract <- function(x, pattern) {
  out <- stringr::str_extract(x, pattern)
  out[is.na(out)] <- ""
  out
}

# Replace a pattern only in the elements where `detect` is found. When several
# patterns are given, they are applied in sequence to the same elements
.gsub_if <- function(x, detect, pattern = detect, replacement = "") {
  tf <- grepl(detect, x)
  if (any(tf)) {
    replacement <- rep_len(replacement, length(pattern))
    for (i in seq_along(pattern)) {
      x[tf] <- gsub(pattern[i], replacement[i], x[tf])
    }
  }
  x
}

# Apply a sequence of gsub, given as c("pattern" = "replacement", ...)
.gsub_all <- function(x, rules) {
  for (i in seq_along(rules)) {
    x <- gsub(names(rules)[i], rules[[i]], x)
  }
  x
}

# Put a part of a name (e.g. the initials after a comma in "Cardoso, D.")
# in front of the remaining name (e.g. "D. Cardoso")
#   idx     logical or numeric positions of the names to rearrange
#   pattern the part to be moved to the front
#   fun     function to format the moved part
#   remove  pattern to delete the moved part from the remaining name
.move_to_front <- function(x, idx, pattern, fun = identity, sep = " ",
                           remove = pattern, msg = NULL) {
  if (is.logical(idx)) idx <- which(idx)
  if (length(idx) == 0) return(x)
  if (!is.null(msg)) message("std_recordedBy $recordedBy ", msg)
  front <- fun(.extract(x[idx], pattern))
  x[idx] <- paste(front, sub(remove, "", x[idx]), sep = sep)
  x
}

# Join initials after a comma, e.g. ", M C G" into "MCG", but keeping
# particles apart, e.g. ", CFP von" into "CFP von"
.join_initials <- function(i) {
  trimws(gsub("([[:upper:]])\\s+(?=[[:upper:]])", "\\1", gsub(",", "", i), perl = TRUE))
}

# Abbreviate names, e.g. "Domingos" into "D." or "Domingos Benicio" into "DB"
# strict = TRUE keeps each abbreviation independent from the other names in
# the vector, otherwise abbreviate() lengthens duplicates, e.g. "ME" into "MarE"
.abbrev <- function(x, dot = TRUE) {
  abbreviate(x, minlength = 1, strict = TRUE, dot = dot, use.classes = FALSE)
}

# Abbreviate the leading name(s) and put them back in front of the surname
.abbrev_to_front <- function(x, idx, pattern = "^(\\S*\\s+)", dot = TRUE,
                             sep = " ", remove = pattern, msg = NULL) {
  .move_to_front(x, idx, pattern, sep = sep, remove = remove, msg = msg,
                 fun = function(i) .abbrev(gsub("^ ", "", i), dot = dot))
}

# Separate initials with full period and space, e.g. "DBOS" into "D. B. O. S."
.spell_initials <- function(x) {
  x <- trimws(gsub("([[:alpha:]])", " \\1", x))
  x <- gsub(" ", ".", paste0(x, " "))
  trimws(gsub("([[:punct:]])", "\\1 ", x))
}

# Make names in just the first letter capitalized, e.g. "CARDOSO" into "Cardoso"
# (*UCP) makes accented letters part of words, avoiding e.g. "DuséN", "FróEs"
.title_case <- function(x) {
  gsub("(*UCP)\\b(\\p{Ll})", "\\U\\1", tolower(x), perl = TRUE)
}

# Decode numeric HTML entities, e.g. "Co&#234;lho" into "Coêlho"
.decode_html <- function(x) {
  tf <- grepl("&#[0-9]+;", x)
  if (any(tf)) {
    m <- gregexpr("&#[0-9]+;", x[tf])
    regmatches(x[tf], m) <- lapply(regmatches(x[tf], m), function(e) {
      vapply(as.integer(gsub("\\D", "", e)), intToUtf8, character(1))
    })
  }
  x
}

# Remove particles like de, da, do, dos from collector names
.rm_particles <- function(x, squish = TRUE) {
  tf <- grepl(" de", x) & !grepl("van den|van der", x)
  x[tf] <- gsub(" de", " ", x[tf])

  # detect, pattern, replacement
  rules <- list(c("[.]de", "de", " "),
                c(" De ", " De ", " "),
                c(" DE ", " DE ", " "),
                c("^De ", "^De ", ""),
                c("^de ", "^de ", ""),
                c(" De$", " De$", ""),
                c("[.]\\sDE$", " DE$", ""),
                c("[[:space:]]da$", " da", ""),
                c(" da ", " da ", " "),
                c(" da;", " da;", " "),
                c("[.]da", "da", " "),
                c("[.]\\sDA\\s", "DA\\s", ""),
                # I might need to exclude the next step for names like "WALMOR DA FONSECA"
                c("[[:upper:]]+\\sDA\\s[[:upper:]]+", "DA\\s", ""),
                c(" dos", " dos", " "),
                c(" do", " do", " "),
                c("[.]dos", "dos", " "),
                c("[.]do", "do", " "))
  for (r in rules) {
    x <- .gsub_if(x, r[1], r[2], r[3])
  }

  if (squish) x <- gsub("[[:space:]]{2}", " ", x)
  x
}


#_______________________________________________________________________________
# Extract secondary collectors and keep only the principal ####
.deepcollclean <- function(df,
                           pos,
                           extract_pattern) {

  if (is.logical(pos)) pos <- which(pos)
  if (length(pos) == 0) return(df)

  collector <- .extract(df$recordedBy[pos], extract_pattern)

  if (grepl("[$]$", extract_pattern)) {
    extract_pattern <- gsub("[$]|[.][+]", "", extract_pattern)
    df$recordedBy[pos] <- gsub(paste0(extract_pattern, ".*"), "\\1", df$recordedBy[pos])
    df$addCollector[pos] <- collector
  } else {
    df$recordedBy[pos] <- gsub(extract_pattern, "", df$recordedBy[pos])
    extract_pattern <- gsub("[$]|[.][+]", "", extract_pattern)
    df$addCollector[pos] <- gsub(extract_pattern, "", collector)
  }

  df$recordedBy[pos] <- gsub("^\\s|\\s$", "", df$recordedBy[pos])
  df$addCollector[pos] <- gsub("^\\s|\\s$", "", df$addCollector[pos])

  return(df)
}


#_______________________________________________________________________________
# Cleaning $recordedBy with ONLY TWO COLLECTORS ####
.split_two_collectors <- function(df) {

  x <- df$recordedBy

  # Clean e.g. "Cardoso, D.|Santos, Q."
  df <- .deepcollclean(df, grepl("[|]", x), "[|].+")

  # Clean e.g. "Olga Kotchetkoff Henriques e Andrei Furlan"
  df <- .deepcollclean(df, grepl("\\s[[:upper:]][[:lower:]]+\\s[e]\\s[[:upper:]][[:lower:]]+\\s",
                                 df$recordedBy), "\\s[e]\\s.+")

  # Clean two collectors separated by "y"
  df <- .deepcollclean(df, grepl("\\sy\\s", df$recordedBy), "\\sy\\s.+")

  # Examples of collectors not separated by symbols (,  ;  &)
  # Extracting just examples like "Teraoka, W. Baker, R."
  tf <- grepl(".*,.*,", df$recordedBy)
  tfa <- !grepl(";", df$recordedBy[tf])
  tfb <- !grepl("&", df$recordedBy[tf][tfa])
  df <- .deepcollclean(df, which(tf)[tfa][tfb], "(\\S*\\s+\\S+)$")

  # Collectors separated by ; but without any comma , separating initials and surnames
  tf <- grepl(".*;", df$recordedBy)
  tfa <- !grepl(".*,", df$recordedBy[tf])
  df <- .deepcollclean(df, which(tf)[tfa], ";.+")

  # Clean remaining examples of two collectors separated by semicolon
  tf <- grepl(".*;", df$recordedBy)
  if (any(tf)) {
    df <- .deepcollclean(df, tf, ";.+")
    # Before next grepl we need to delete the remaining semicolon ";"
    df$recordedBy <- gsub("[;]", "", df$recordedBy)
  }

  # Lets first edit examples like this "J. Campbell-Snelling, M. Chambers"
  df$recordedBy <- .gsub_if(df$recordedBy,
                            "[[:lower:]]+[-][[:upper:]][[:lower:]]+,\\s[[:upper:]][.]\\s[[:upper:]]",
                            ",", " &")

  # Clean two collectors separated by "&"
  df <- .deepcollclean(df, grepl("&", df$recordedBy), "&.+")

  # NEED TO WORK MORE HERE; I might get some problems as mentioned below
  # Finding two collectors like
  # "C. H. Dodson, P. M. Dodson" or "P.P. Wan, K.S. Chow"
  # "Robert F. Thorne, Geoff Tracey"
  # I need to work on this so as make it more general and grab the following names
  # "Robert Thorne, Geoff Tracey"
  # "C.Farney, G.Byer"
  df <- .deepcollclean(df, grepl("([[:upper:]][.]){1,}\\s*[[:upper:]][[:lower:]]+[,]\\s",
                                 df$recordedBy), ",.+")

  return(df)
}


#_______________________________________________________________________________
# Standardize the collector name into the format "D. B. O. S. Cardoso" ####
# When there is only ONE COLLECTOR at $recordedBy
.std_initials <- function(x) {

  #_____________________________________________________________________________
  # General cleaning of collector initials
  # Clean e.g. "Arbo M .M."
  x <- .gsub_if(x, "\\s[.][[:upper:]][.]", c("\\s[.]", "[.][.]"), ".")

  # Clean e.g. "Meira, Neto J A": replace first space with a comma
  tf <- grepl("^[[:upper:]][[:lower:]]+[-](Filho|Sobrinho|Neto)\\s([[:upper:]]{1,}|[[:upper:]]\\s)", x)
  x[tf] <- sub("\\s+", ", ", x[tf])

  # Clean e.g. "Roberto Paulo Orlandi.", "Adonias Araujo."
  x <- .gsub_if(x, "([[:upper:]][[:lower:]]+\\s){1,}[[:upper:]]([[:lower:]]){2,}[.]", "[.]$", "")

  # Clean e.g. "Elton. M. C. Leme", "Franco. I.M.": remove only the first dot
  for (p in c("[[:upper:]][[:lower:]]+[.]\\s[[:upper:]][.]", "^[[:upper:]]+{2,}[.]\\s[[:upper:]]")) {
    tf <- grepl(p, x)
    x[tf] <- stringi::stri_replace_first_regex(x[tf], "[.]{1}", "")
  }

  #_____________________________________________________________________________
  # Extracting collector initials that are separated by COMMA
  # Grabbing and inserting back first names with at least one abbreviation
  # so, initials like..." C.F.P. von ", " J.E.L.S.", " R.", " Terence D."
  tf <- grepl(",", x)
  tfa <- grepl("([[:upper:]][.]){1}", x[tf])
  x <- .move_to_front(x, which(tf)[tfa], ",.+", msg = "initials #1",
                      fun = function(i) gsub("^\\s", "", gsub(",", "", i)))

  # Grabbing and inserting back first names with at least two abbreviation and no dot
  # so, initials like..."Padgurschi, MCG", "Oliveira, AA"
  tf <- grepl(",", x)
  idx <- which(tf)[grepl("([[:upper:]]){2}", x[tf])]
  x[idx] <- gsub("^\\s", "", x[idx])
  x <- .move_to_front(x, idx, ",.+", msg = "initials #2", fun = .join_initials)

  # Grabbing and inserting back first names with just one initial and no dot
  # so, initials like..."Cardoso, D", "Oliveira, A"
  tf <- grepl(",", x)
  tfa <- !grepl("[[:upper:]][[:lower:]]+[,]\\s*[[:upper:]][[:lower:]]+", x[tf])
  x <- .move_to_front(x, which(tf)[tfa], ",.+", msg = "initials #3", fun = .join_initials)

  # This step will be enough to grab and insert back the non abbreviated names
  # like "Estrada, Armando", "Sellow, Friedrich", "Pierre, Jean Baptiste Louis"
  tf <- grepl(",", x)
  x[tf] <- gsub("[.]$", "", gsub(",$", "", x[tf]))
  x <- .move_to_front(x, tf, ",.+", msg = "initials #4",
                      fun = function(i) gsub("^\\s|\\s$", "", gsub(",", "", i)))

  # Deleting blank space at the beginning and end of the cell
  x <- .gsub_all(x, c("^[[:space:]]" = "", "[[:space:]]{2}" = " ", "[[:space:]]$" = ""))

  #_____________________________________________________________________________
  # Now cleaning collector names with initials NOT separated by comma ####
  # or semicolon like "Callejas R.", "Schultes R.E.", "Krukoff BA" "Sergio M Faria"
  # We do in a series of steps otherwise we will erase others based on the patterns

  # Names with just surnames or with first word in capital letters
  tf <- !grepl("[[:space:]]|[.]", x)
  x[tf] <- .title_case(x[tf])
  tf <- grepl("[[:upper:]]+{2,}\\s[[:upper:]][.]", x)
  x[tf] <- .title_case(x[tf])

  # "Zwaan CJ van der", "Martius CFP von"
  tf <- grepl("[[:lower:]]+\\s+([[:upper:]]{1,})+\\s", x)
  tfa <- grepl("van den|van der| von| van| bin", x[tf])
  x <- .move_to_front(x, which(tf)[tfa], " .+", msg = "initials #5",
                      fun = function(i) sub("^\\s", "", i))

  # Names like "Sergio M Faria", "Domingos S Cardoso", "Marcelo T Nascimento"
  x <- .abbrev_to_front(x, grepl("[[:lower:]]+\\s+([[:upper:]]{1,})+\\s", x),
                        dot = FALSE, sep = "", msg = "initials #6")

  # Names in capital letter like "JORGE  C.A. LIMA"
  tf <- grepl("[[:upper:]]+\\s+([[:upper:]]+[.]{1,})+\\s+[[:upper:]]{3}", x)
  x[tf] <- .title_case(x[tf])

  # "Roberto P.Orlandi", "Jorge C.A.Lima": open spaces between initials
  x <- .gsub_if(x, "[[:lower:]]+\\s+([[:upper:]]+[.]){1,}[[:upper:]][[:lower:]]+",
                "[.]", ". ")

  # "David J.N. Hind", "Jorge C. A. Lima", "Grady L. Webster", "Charles M. Ek"
  tf <- grepl("[[:upper:]][[:lower:]]+\\s+(.*[[:upper:]][.]){1,}\\s+[[:alpha:]]{2,}", x)
  x[tf] <- gsub(",$", "", gsub("^\\s", "", x[tf]))
  x <- .abbrev_to_front(x, tf, msg = "initials #7")

  # "Schultes R.E.", "Soejarto D.", "Maas P.J.M."
  tf <- grepl("^\\S[[:lower:]]+\\s([[:upper:]]+[.]){1,}", x)
  tfa <- !grepl("\\s+[[:upper:]][[:lower:]]+", x[tf])
  x <- .move_to_front(x, which(tf)[tfa], " .+", msg = "initials #8")

  # "Croat TB", "Kostermans AJGH": more than two initials without full period
  x <- .move_to_front(x, grepl("^\\S[[:lower:]]+\\s([[:upper:]]{2,})", x), " .+",
                      msg = "initials #9")

  # Getting rid of the last abbreviated initial in Spanish-like names
  # "Percy Núñez V.", "P. Nuñez V.", "Mario Sousa S.", "G. Ibarra M."
  tf <- grepl("[[:lower:]]+\\s+([[:upper:]]+[.]{1})", x)
  idx <- which(tf)[grepl("(\\s+[[:upper:]]+[[:lower:]]+\\s+([[:upper:]]+[.]$))", x[tf])]
  x[idx] <- gsub("\\s[^ ]+$", "", x[idx])

  # "N. Castaño-A." "W. Trujillo-C."
  x <- .gsub_if(x, "[[:upper:]][[:lower:]]+[-]+([[:upper:]]+[.]{1})", "-[^-]+$", "")

  # "Uribe Uribe AL", "Cid Ferreira CA": separate the last names by an hyphen
  tf <- grepl("[[:lower:]]+[[:space:]]([[:upper:]]{2,})", x)
  if (any(tf)) {
    message("std_recordedBy $recordedBy initials #10")
    initials <- .extract(x[tf], " [^ ]+$")
    x[tf] <- paste(initials, gsub(" ", "-", sub("\\s[^ ]+$", "", x[tf])))
  }

  # "Cardenas D", "Ferreira L": only one surname and one initial
  tf <- grepl("[[:upper:]][[:lower:]]+\\s[[:upper:]]", x)
  tfa <- !grepl("[.]", x[tf])
  tfb <- !grepl("[[:upper:]][[:lower:]]+\\s[[:upper:]][[:lower:]]+", x[tf][tfa])
  x <- .move_to_front(x, which(tf)[tfa][tfb], " .+", msg = "initials #11",
                      fun = function(i) gsub("^\\s", "", paste0(i, ".")))

  # Adding full period in names like "D Cardoso", "DD Cardoso", "DDD Cardoso"
  x <- gsub("^ ", "", x)
  tf <- grepl("[[[:upper:]]{2}", x)
  tfa <- !grepl("[.]", x[tf])
  tfb <- !grepl("[[:upper:]]{5,}", x[tf][tfa])
  x <- .move_to_front(x, which(tf)[tfa][tfb], "^(\\S*\\s+)", remove = "^\\S*.",
                      msg = "initials #12",
                      fun = function(i) .spell_initials(gsub(" $", "", i)))

  # Abbreviating and adding points to names like "Dionisio Constantino"
  tf <- !grepl("[.]", x)
  tfa <- !grepl("[[:upper:]][[:lower:]]+\\s[[:upper:]][[:lower:]]+\\s", x[tf])
  tfb <- grepl("\\s", x[tf][tfa])
  tfc <- !grepl("-", x[tf][tfa][tfb])
  x <- .abbrev_to_front(x, which(tf)[tfa][tfb][tfc], remove = "^\\S*.",
                        msg = "initials #13")

  #_____________________________________________________________________________
  # "T S SANTOS", "A Ducke", or errors like "A .Ducke" "G .T. Prance" "L W. Williams"
  x <- .gsub_if(x, "^[[:upper:]]\\s", " [.]", ". ")

  # Correcting errors like "L W. Williams"
  tf <- grepl("^[[:upper:]]\\s", x)
  idx <- which(tf)[grepl("[.]", x[tf])]
  x[idx] <- gsub(" ", ". ", gsub("[.]", "", x[idx]))

  # Now searching just "T S SANTOS", "A Ducke"
  tf <- grepl("^[[:upper:]]\\s", x)
  if (any(tf)) {
    x[tf] <- .gsub_all(x[tf], c(" " = ". ", "bin." = "bin", "van." = "van",
                                "(^|\\s)(von|den|der|ter|la)[.]" = "\\1\\2"))
    idx <- which(tf)[grepl("[[:upper:]][[:lower:]]+[.]\\s[[:upper:]][[:lower:]]+", x[tf])]
    # Add point after the first word
    x[idx] <- sub("^(\\w)", "\\1.", gsub("[.] ", " ", x[idx]))
  }

  # "Monod Froideville C": remove last words after the last space OR an hyphen
  tf <- grepl("[[:upper:]]$", x)
  idx <- which(tf)[!grepl("[.]", x[tf])]
  x[idx] <- gsub("(-|\\s)[A-Z]+$", "", x[idx])

  # Collectors with all uppercase letters
  tf <- grepl("[[:upper:]]{3,}", x)
  x[tf] <- .title_case(x[tf])

  # Collectors with more than four names: abbreviate first two names
  # "Alexánder Francisco Rodríguez González"
  tf <- !grepl("[.]", x)
  tfa <- grepl(".*\\s.*\\s.*\\s", x[tf])
  x <- .move_to_front(x, which(tf)[tfa], "^(\\S*\\s\\S*\\s+)", msg = "initials #14",
                      fun = function(i) {
                        .spell_initials(.abbrev(gsub(" $", "", i), dot = FALSE))
                      })

  #_____________________________________________________________________________
  # Collectors with initials not separated by comma
  # "H.C. Lima", "D.B.O.S. Cardoso" and "F.C.How"
  x <- gsub("[[:space:]]{2}", " ", x)
  tf <- grepl("([[:upper:]][.]){2,}", x)
  idx <- which(tf)[!grepl("[[:upper:]][[:lower:]]+\\s[[:upper:]][.]", x[tf])]
  if (length(idx) > 0) {
    message("std_recordedBy $recordedBy initials #15")
    # Initials before the last dot, with just one space after each dot
    initials <- trimws(gsub("([[:punct:]])", "\\1 ", sub(" ", "", sub("[^.]+$", "", x[idx]))))
    # Surname after the last dot
    x[idx] <- paste(initials, gsub("^ ", "", gsub(".*\\.", "", x[idx])))
  }

  # "N. Marquete F. Silva": abbreviate the second name
  tf <- grepl("([[:upper:]][.])\\s[[:upper:]][[:lower:]]+\\s([[:upper:]][.])", x)
  if (any(tf)) {
    message("std_recordedBy $recordedBy initials #16")
    initials <- sub(" $", "", sub("(\\S*\\s+\\S+)$", "", x[tf]))
    initials <- .spell_initials(.abbrev(initials, dot = FALSE))
    x[tf] <- paste(initials, gsub("^ ", "", gsub("^(\\S*\\s+\\S+)", "", x[tf])))
  }

  # "J. A.S. Santos", "M.Oliveira" into "J. A. S. Santos", "M. Oliveira"
  x <- .gsub_if(x, "[[:upper:]][.][[:upper:]][[:lower:]]+", "[.]", ". ")

  # "G.D Colletta", "E. M.B Prata", "C.E Zartman"
  x <- .gsub_if(x, "[[:upper:]][.][[:upper:]]\\s",
                c("\\s", "[.]", " [.] ", "\\s\\s"), c(". ", ". ", "", " "))

  # ALL remaining collectors without full period
  tf <- !grepl("[.]", x)
  idx <- which(tf)[grepl("\\s", x[tf])]
  if (length(idx) > 0) {
    message("std_recordedBy $recordedBy initials #17")
    # Take out the accents before using the function abbreviate
    first <- gsub("([A-Za-z]+).*", "\\1", stringi::stri_trans_general(x[idx], "Latin-ASCII"))
    x[idx] <- paste(.abbrev(first), gsub("^ ", "", gsub("^(\\w+)", "", x[idx])))
  }

  # "C. -Ming Tan" into "C.-M. Tan"
  tf <- grepl("[[:upper:]][.]\\s[-][[:upper:]][[:lower:]]+", x)
  if (any(tf)) {
    message("std_recordedBy $recordedBy initials #18")
    initials <- abbreviate(gsub("(\\w+)$", "", x[tf]),
                           minlength = 4, strict = TRUE, dot = TRUE, use.classes = FALSE)
    x[tf] <- paste(initials, gsub("^(\\S*\\s\\S*\\s+)", "", x[tf]))
  }

  return(x)
}


#_______________________________________________________________________________
# Particles before surnames, based on the original collector column ####
.std_particles <- function(x, original) {

  # Remove any additional collector from the original names
  original <- gsub("(;.+)|([|].+)", "", original)

  # Collectors that has " de la " like " de la Cruz", " De la Estrella"
  tf <- grepl(" [Dd]e la ", original)
  if (any(tf)) {
    message("std_recordedBy $recordedBy particles before surname #1")
    x[tf] <- gsub(" la ", " de la ", x[tf])
  }

  # Abbreviating second names if there exists just one kind of particle like
  # "de" do" "dos" in the original collector column
  particles <- c(" de ", " DE ", " De ", " do ", " DO ", " Do ", " dos ", " DOS ", " da ", " DA ")
  n_particles <- Reduce(`+`, lapply(particles, grepl, x = original, fixed = TRUE))
  tf <- grepl("[.]\\s[[:upper:]][[:lower:]]+\\s[[:upper:]][[:lower:]]+", x) & n_particles == 1
  if (any(tf)) {
    message("std_recordedBy $recordedBy particles before surname #2")
    first <- gsub("[^.]+$", "", x[tf])
    names <- gsub(".*\\.", "", x[tf])
    second <- .abbrev(gsub("(\\S*\\S+)$", "", names))
    last <- gsub("^(\\S*\\s+\\S+)", "", names)
    x[tf] <- paste(first, second, last, sep = " ")
  }

  return(x)
}


#_______________________________________________________________________________
# Final cleaning of $recordedBy ####
.final_collclean <- function(x) {

  # Finding possible examples like these "B. T. P. M Góes", "M. P Dias"
  x <- .gsub_if(x, "\\s[[:upper:]]\\s[[:upper:]][[:lower:]]+",
                c("\\s", "[.][.]"), c(". ", "."))

  # Removing dots and comma at the end of surnames like:
  # "V. O. Amorim."	"J. H. C. Ribeiro," from RB collections or "A...S... Flores"
  x <- .gsub_all(trimws(x), c("[.]$" = "",
                      ",$" = "",
                      "\\s[.]\\s" = " ",
                      "[.][.][.]" = ". ",
                      "[[:space:]]{2}" = " "))

  # Removing de, da etc when they are not separated from the surnames
  for (p in c(" de", " da", " das", " do", " dos")) {
    x <- .gsub_if(x, paste0("\\s", trimws(p), "[[:upper:]][[:lower:]]+"), p, " ")
  }

  # Last abbreviated initial in Spanish-like names, e.g. "M. Sousa S."
  x <- gsub("^((\\p{Lu}[.] )+\\p{Lu}\\p{Ll}+)\\s\\p{Lu}$", "\\1", x, perl = TRUE)

  # Adding Unknown collector to empty cells
  x <- gsub("^$", "Unknown", trimws(x))

  # Further cleaning
  x <- .gsub_all(x, c("[?]$" = "", "\\s$" = "", "^[.]\\s" = ""))
  x <- .gsub_if(x, "^[\177][[:upper:]][.]", "^[\177]", "")
  x <- .gsub_if(x, "[.]\\s,\\s[[:upper:]]", "\\s,", "")

  return(x)
}


#_______________________________________________________________________________
# Cleaning $recordedBy with more than two collectors ####
# The function automatically adds "et al." at $addCollector

.deletal <- function(df) {

  # Adding et al. in the column "addCollector" when the column "recordedBy"
  # has more than two collectors
  df <- .deepdeletal(df, "& et al[.]", c(" &.+", "[,].+"), n_message = "#1")

  # Finding examples with multiples "--"; we have to grepl from the original because
  # these hyphens were deleted previously in the main column
  df <- .deepdeletal(df, pattern = "[,].+", n_message = "#2",
                     tf = grepl("(.*[-]{2}.*[-]{2}){1,}", df$recordedByOriginal))

  # Lima H.S., Neto J.P.; Marimon B.S.
  df <- .deepdeletal(df, "[[:lower:]]+\\s([[:upper:]][.]){1,}[,]\\s[[:upper:]][[:lower:]]+\\s([[:upper:]][.]){1,}[;]\\s[[:upper:]][[:lower:]]+\\s[[:upper:]]",
                     ",.+", n_message = "#3")
  df <- .deepdeletal(df, "; et al[.]; et al[.]", ";.+", n_message = "#4")
  df <- .deepdeletal(df, "[|]et al[.]|[|] et al[.]", "[|].+", n_message = "#5")
  df <- .deepdeletal(df, "(.*[|].*[|]){1,}", "\\|.+", n_message = "#6")
  df <- .deepdeletal(df, "[|] Otros| Partícipes| Participantes",
                     "[|] Otros.+| Partícipes.+| Participantes.+", n_message = "#7")
  df <- .deepdeletal(df, "(.*\\sy\\s.*\\sy\\s){1,}", "\\sy.+", n_message = "#8")
  df <- .deepdeletal(df, "(.*&.*\\sy\\s){1,}", "&.+|\\s&.+", n_message = "#9")
  df <- .deepdeletal(df, "(.*[|].*\\sy\\s){1,}", "[|].+", n_message = "#10")
  df <- .deepdeletal(df, "(.*[;].*\\sy\\s){1,}", "[;].+", n_message = "#11")
  df <- .deepdeletal(df, "(.*[,].*\\sy\\s){1,}", "[,].+|[;].+", n_message = "#12")
  df <- .deepdeletal(df, "(.*\\sy\\s.*[,].*[,]){1,}", "\\sy.+", n_message = "#13")
  df <- .deepdeletal(df, "; Etc", ";.+", n_message = "#14")
  df <- .deepdeletal(df, "[:]", ":.+", n_message = "#15")
  df <- .deepdeletal(df, "(.*;.*;){1,}", "[;].+", n_message = "#16")
  df <- .deepdeletal(df, ".*;.*&", ";.+", n_message = "#17")
  df <- .deepdeletal(df, ".*&.*,.*,", "\\s&.+", n_message = "#18")
  df <- .deepdeletal(df, ".*;.*[[:space:]]-[[:space:]]", ";.+", n_message = "#19")
  df <- .deepdeletal(df, ".*;.*[[:alpha:]]+[[:space:]]+[e]+[[:space:]]+[[:alpha:]]", ";.+",
                     n_message = "#20")
  # "et al.; Redden, K.M."
  df <- .deepdeletal(df, "et al[.][;]\\s[[:upper:]]", "et al.; ", n_message = "#21")
  df <- .deepdeletal(df, " ET AL|et[.]al", ";.+", n_message = "#22")
  df <- .deepdeletal(df, " et[.] al", c("et[.] al.+", ";.+"), n_message = "#23")
  df <- .deepdeletal(df, "([^;]+;+[[:space:]]+[[:upper:]]+[.][^,]+),", ";.+", n_message = "#24")
  df <- .deepdeletal(df, ".*,.*,.*et Al", c("^(\\S*\\s+\\S+).*", ",$"), x = c("\\1", ""),
                     n_message = "#25")
  df <- .deepdeletal(df, "\\set\\sAl[.]", "\\set\\sAl[.]", n_message = "#26")

  #_____________________________________________________________________________
  tf <- grepl(".*,.*,.*&", df$recordedBy)
  if (any(tf)) {
    message(".deletal $recordedBy and $addCollector #27")

    df$addCollector[tf] <- "et al."

    # "Rodríguez,D., Rodríguez,B. & Trejo,L." "Rodríguez,D., Galán,P. & Valle,J.V."
    tfa <- grepl("[[:upper:]][[:lower:]]+[,][[:upper:]][.]", df$recordedBy[tf])

    # "Cervi, A. C., R. Spichiger, P.-A. Loizeau & E. Cottier"
    # this part "(.*[[:upper:]][.].*[[:upper:]][.]){1,}" means
    # at least one alternating uppercase letter with dots so...
    # the entire regex grabs "Cervi, A. C.,"
    tfb <- grepl("[[:upper:]][[:lower:]]+[,]\\s(.*[[:upper:]][.].*[[:upper:]][.]){1,}[,]",
                 df$recordedBy[tf])

    # Remove all after second comma
    # https://stackoverflow.com/questions/33062016/how-to-delete-everything-after-nth-delimiter-in-r
    idx <- which(tf)[tfa | tfb]
    df$recordedBy[idx] <- gsub("^([^,]+,[^,]+).*", "\\1", df$recordedBy[idx])
    idx <- which(tf)[tfb]
    idx <- idx[grepl("[[:upper:]][.]\\s[[:upper:]][[:lower:]]+[,]\\s", df$recordedBy[idx])]
    df$recordedBy[idx] <- gsub("^([^,]+).*", "\\1", df$recordedBy[idx])

    # Then do last search again
    df$recordedBy <- .gsub_if(df$recordedBy, ".*,.*,.*&", "[.],.+", ".")
    df$recordedBy <- .gsub_if(df$recordedBy, ".*,.*,.*&", ",.+", "")
  }
  #_____________________________________________________________________________

  # Three collectors with full names like "A. Gómez Pompa, A. J. Sharp & P. Hernández"
  # or "Schultes R.E., Raffauf R.F. & Soejarto D."
  df <- .deepdeletal(df, pattern = ",.+", n_message = "#42",
                     tf = .three_colls(df$recordedBy))

  df <- .deepdeletal(df, ".*&.*&", "&.+", n_message = "#28")
  df <- .deepdeletal(df, " Et al", ";.+", n_message = "#29")
  df <- .deepdeletal(df, "& et al", "&.+", n_message = "#30")
  df <- .deepdeletal(df, "; et al[.]|; et al", ";.+", n_message = "#31")

  # "J.A. Lombardi, H. Lorenzi, R. Tsuji et al."
  tf <- grepl(" et al", df$recordedBy)
  df <- .deepdeletal(df, pattern = "[,].+", n_message = "#32",
                     tf = tf & grepl(".*,.*,", df$recordedBy))

  df <- .deepdeletal(df, " et al[.]| et al", "\\s*et al[.]?", n_message = "#33")
  # "M.G.Bovini; A.Quinet et L.E.Barros"
  df <- .deepdeletal(df, ".*;.* et ", ";.+", n_message = "#34")
  df <- .deepdeletal(df, ".*;.*;", "[;].+", n_message = "#35")
  df <- .deepdeletal(df, ".*;.*,.*,", "[;].+", n_message = "#36")

  tf <- grepl(".*,.*,.*,", df$recordedBy)
  if (any(tf)) {
    message(".deletal $recordedBy and $addCollector #37")
    initial_comma <- grepl("[[:upper:]][.][,]", df$recordedBy)
    df <- .deepdeletal(df, pattern = c("[,].+", "[;].+"), n_message = NULL,
                       tf = tf & !initial_comma)
    df <- .deepdeletal(df, pattern = "(^[^,]+,[^,]+).*$", x = "\\1", n_message = NULL,
                       tf = tf & initial_comma)
  }

  df <- .deepdeletal(df, "& Al[.]|& col[.]|& al[.]|&\\sal$", "\\s&.+", n_message = "#38")
  df <- .deepdeletal(df, "e auxiliares| e outros", " e auxiliares.*| e outros.*",
                     n_message = "#39")

  # The following step was messing examples like;
  # ""Gardner, Martin F. & Knees, Sabina G.", "Ludlow, F. & Sherriff, G."
  # tf <- grepl(".*,.*&", df$recordedBy)
  # tfa <- !grepl(".*,.*&.*,", df$recordedBy[tf])
  # if (any(tfa)){
  #   df$addCollector[tf][tfa] <- paste("et al.")
  #   df$recordedBy[tf][tfa] <- gsub("[,].+", "", df$recordedBy[tf][tfa])
  # }

  # Cleaning remaining examples of more than two collectors separated by just commas
  # and no symbols like &, semicolon or et al.
  # "J.A. Lombardi, H. Lorenzi, R. Tsuji"
  df <- .deepdeletal(df, pattern = "[,].+", n_message = "#40",
                     tf = .many_commas(df$recordedBy, "[[:upper:]][.]\\s[[:upper:]][[:lower:]]+,\\s"))

  # "Radford A.E., J. Bozeman & Ramseur, George S."
  df <- .deepdeletal(df, pattern = c("[,].+", "\\s"), x = c("", ", "), n_message = "#41",
                     tf = .many_commas(df$recordedBy, "[[:upper:]][.]\\s[[:upper:]][[:lower:]]+\\s&"))

  return(df)
}

# Extract secondary collectors, keep only the principal and add "et al."
.deepdeletal <- function(df, detect = NULL, pattern = NULL, x = "", n_message,
                         tf = grepl(detect, df$recordedBy)) {
  if (!any(tf)) return(df)
  if (!is.null(n_message)) {
    message(paste(".deletal $recordedBy and $addCollector", n_message))
  }
  df$addCollector[tf] <- "et al."
  x <- rep_len(x, length(pattern))
  for (i in seq_along(pattern)) {
    df$recordedBy[tf] <- gsub(pattern[i], x[i], df$recordedBy[tf])
  }

  return(df)
}

# Find three collectors as "Name1, Name2 & Name3", where the first two names
# include initials, so avoiding two collectors like "Gardner, M. & Knees, S."
.three_colls <- function(x) {
  is_name <- function(s) {
    grepl("[[:upper:]][.]", s) & grepl("[[:alpha:]]{2,}", s) & grepl("\\S\\s+\\S", trimws(s))
  }
  grepl("^[^,&;|]+,[^,&;|]+&", x) &
    is_name(sub(",.*", "", x)) &
    is_name(sub("^[^,]+,([^&]*)&.*", "\\1", x))
}

# Find names with at least two commas, no semicolon, at least five spaces,
# and matching a given pattern
.many_commas <- function(x, pattern) {
  grepl("(.*,.*,){1,}", x) & !grepl(";", x) &
    grepl("(.*[[:space:]]){5,}", x) & grepl(pattern, x)
}


#_______________________________________________________________________________
# Pre-cleaning $recordedBy before standardizing collector names ####
.precollclean <- function(df){

  x <- df$recordedBy

  x <- .gsub_if(x, "(^|[.])[[:upper:]][[:lower:]]+,\\s(Filho|Sobrinho|Neto)",
                ", (Filho|Sobrinho|Neto)", "-\\1")

  # Removing titles, notes and symbols
  x <- .gsub_all(x, c("\"" = "",
                      # Institution after the name, e.g. "Lutero Lerner - IFN"
                      "\\s-\\s[[:upper:]]{2,}$" = "",
                      "[[][?][]]" = "",
                      "\\s[(]J[.][?][)]" = "",
                      "[[]" = "",
                      "[]]" = "",
                      "[(]Lady[)]" = "",
                      "[(]Miss[)]" = "",
                      "[(]Capt[.][)]" = "",
                      "[(]Prof[.][)]" = "",
                      "[(]Rev[.][)]" = "",
                      "[(]Mrs[)][.]" = "",
                      "[(]Mrs[)]" = "",
                      "\\s[(]Mr\\s[&]\\sMrs[)]" = "",
                      "\\s[(]Countess\\sof.+" = "",
                      "[(]Major[)]" = "",
                      "[(]photo[)]" = "",
                      "[(]Photo[)]" = "",
                      "[(]Pere[)]" = "",
                      "\\s[(]Karl[)]" = "",
                      "[(]Frère[)]" = "",
                      "[(]Dr[.][)]" = "",
                      "\\s[(]Dr[)]" = "",
                      ",\\sDr\\s" = ", ",
                      "\\s[(]Dr[.][/]Sir[)]" = "",
                      "\\s[(]Col[.][)]$" = "",
                      "^[(]" = "",
                      "[)]$" = "",
                      "[?]$|^[?]" = "",
                      "- Botanist" = "",
                      "\\sAfrica$" = "",
                      ",\\sunknown" = "",
                      "unknown collector" = "",
                      "[\'][s]\\sCollector" = "",
                      "MrandMrs" = "",
                      "MrMrs" = "",
                      "CETA[:][|]" = "",
                      "\\s[(]SEMO[)]" = "",
                      "Collector[(]s[)][:]\\sunknown," = "",
                      "[(]Coll.+" = "",
                      "\\s[-]\\sPau\\sBrasil$" = "",
                      "Collector[(]s[)][:]\\s" = "",
                      ",\\s[*]$|[*]$| [{]2[º] SERIE[}]|^[?];et al[.]," = "",
                      "^[-][.]\\s|[-][-]|^[<]" = ""))

  # Unknown collectors
  temp <- c("^$", "^[@]$", "[@];,S[.]N[.]", "Collector illegible", "Native Collector",
            "Collector unspecified", "NO DISPONIBLE", "#NOME[?]",
            "Illegible collector name", "s[.]coll[.]", "no data",
            "Collector unknown", "[(]unknown[)]", "[(]n[/]a[)]",
            "sem coletor", "s[.]coll[.]", "s[.]col[.]", "s[.]col", "collector",
            "[[]data\\snot\\scaptured[]]", "s.c.", "Anonymous", "coletor$",
            "Anon.", "[?]", "Unclear", "unclear", "C. F. C. R", "Cfcr",
            "^_V$", "Sem coletor$", "^([0-9]){1,}$|^([0-9]){1,}.*([0-9]){1,}$",
            "^#NOME", "^NA$", "^n/a$", "^N/A$", "^sc$", "^SC$", "not a person",
            "provisional entry")
  x[is.na(x) | grepl(paste0(temp, collapse = "|"), x)] <- "Unknown"

  # Names in lowercase like "stahel", "ling yung", "johnson, I."
  x <- .lower_to_title(x)

  x <- .gsub_all(x, c("[?][.] " = "",
                      " - " = "-",
                      "- " = "-",
                      "[*] " = "",
                      "-[.]" = "",
                      ",[.]" = ".",
                      ", III" = "",
                      ", --" = "",
                      "--" = " ",
                      ", not a person" = "",
                      " Mrs Captain" = "",
                      ", 1" = "",
                      ", Jr[.]" = "",
                      " Jr[.]" = "",
                      " Jr " = " ",
                      " Jr, " = ", ",
                      "-Júnior" = "",
                      " Junior" = "",
                      " d'" = "",
                      " Neto" = "-Neto",
                      " NETO" = "-NETO",
                      " Filho" = "-Filho",
                      " FILHO" = "-FILHO",
                      "Leitão Fo[.]," = "Leitão-Filho,",
                      " Sobrinho" = "-Sobrinho",
                      " [(]IAN[)]" = "",
                      " and " = " & ",
                      "ex herb[.] " = "",
                      "^Prof[.] " = "",
                      "Mrs[.] " = "",
                      "Dr[.] " = "",
                      "Dr [?]" = "",
                      "[(]|[)]" = "",
                      # "Fred Melgert / Carla Hoegen"
                      "\\s\\/\\s" = " & ",
                      "\\swith\\s" = " & ",
                      " HERB[.] AMAZ[.]" = "",
                      "\\s[()]Brother[)]" = ""))

  # detect, pattern, replacement
  x <- .gsub_if(x, "\\sCollectors[:]\\s", ".*?[:]\\s", "")
  x <- .gsub_if(x, "[[:lower:]]+,\\sSir\\s[[:upper:]]", "Sir\\s", "")
  x <- .gsub_if(x, "\\sDr[.]$", c("[.]\\sDr[.]", ",\\sDr[.]"), "")
  x <- .gsub_if(x, "^Dr\\s[[:upper:]]", "Dr\\s", "")
  x <- .gsub_if(x, "; Herb[.] Amaz[.]", ";.+", "")
  # "Lewis, Mr John "
  x <- .gsub_if(x, "[[:upper:]][[:lower:]]+[,]\\sMr\\s[[:upper:]][[:lower:]]+\\s", ", Mr ", ", ")
  x <- .gsub_if(x, "\\s[[:upper:]][[:lower:]]+\\sF[.]L[.]S[.]", "\\sF[.]L[.]S[.]", "")
  # "DrHapeman, H.", "MrsYoung, H.S.", "BroArsene, G.; BroBenedict, A.", "Steve Stephens II"
  x <- .gsub_if(x, "^Dr[[:upper:]][[:lower:]]+[,]\\s[[:upper:]]", "Dr", "")
  x <- .gsub_if(x, "^Mrs[[:upper:]][[:lower:]]+[,]\\s[[:upper:]]", "Mrs", "")
  x <- .gsub_if(x, "^Mrs\\s[[:upper:]]", "Mrs\\s", "")
  x <- .gsub_if(x, "^Mr[[:upper:]][[:lower:]]+[,]\\s[[:upper:]]", "Mr", "")
  x <- .gsub_if(x, "^Mr\\s[[:upper:]][[:lower:]]+", "Mr\\s", "")
  x <- .gsub_if(x, "Mr[.]$", " Mr[.]", "")
  x <- .gsub_if(x, "^Bro[[:upper:]][[:lower:]]+[,]\\s[[:upper:]]", "Bro", "")
  x <- .gsub_if(x, "\\s[[:upper:]][[:lower:]]+\\s(I){2,}", c("\\sIII", "\\sII"), "")
  x <- .gsub_if(x, "\\s[[:upper:]][[:lower:]]+\\s(IV)", "\\sIV", "")
  x <- .gsub_if(x, "^-[.]\\s[[:upper:]][[:lower:]]+,\\s", "-[.]\\s", "")
  x <- .gsub_if(x, "[[:upper:]][[:lower:]]+,\\s[-]$", ",\\s[-]", "")
  x <- .gsub_if(x, "[[:upper:]][.]\\set\\s[[:upper:]][.]", "et", "and")

  df$recordedBy <- x

  return(df)
}


#_______________________________________________________________________________
# Capitalize names written just in lowercase letters
.lower_to_title <- function(x) {
  tf <- !grepl("[[:upper:]]", x) & grepl("[[:lower:]]{2,}", x) & !grepl("^et al", x)
  x[tf] <- .title_case(x[tf])
  gsub("^([[:lower:]]{2,},)", "\\U\\1", x, perl = TRUE)
}


#_______________________________________________________________________________
# Pre-cleaning collector names at specific collections ####
.precollherb <- function(df){

  message(".precollherb $recordedBy in specific collections")

  #_____________________________________________________________________________
  # CEN collections
  cen <- which(df$collectionCode %in% "CEN")
  x <- df$recordedBy[cen]
  x <- .gsub_if(x, "[[:lower:]]+[-]([[:upper:]]){2,}", "-.+", "")

  # In the CEN collections all names are not abbreviated like:
  # "Taciana Barbosa Cavalcanti", "Carolyn Elinore Barnes Proença", "Marcelo Fragomeni Simon"
  # still need to correct left examples like: R. L. RM.  Machado Leite
  tf <- grepl("([[:upper:]][[:lower:]]+\\s){1,}", x)
  if (any(tf)) {
    colls <- x[tf]

    # Abbreviate first words
    first <- gsub("[[:lower:]]", "", .abbrev(gsub("\\s.+", "", colls)))
    colls <- paste(first, sub(".*?\\s", "", colls))

    # Abbreviate the second name
    tfa <- grepl("\\S*\\s\\S*\\s+", colls)
    full <- colls[tfa]
    names <- gsub(".*\\.", "", full)
    colls[tfa] <- paste(gsub("[^.]+$", "", full),
                        .abbrev(gsub("(\\S*\\S+)$", "", names)),
                        gsub("^(\\S*\\s+\\S+)", "", names),
                        sep = " ")
    x[tf] <- colls

    tfb <- grepl("[[:upper:]]{2}[.]\\s", colls[tfa])
    colls_b <- sub("[[:upper:]][.](\\s){2}", " ", colls[tfa][tfb])
    # Keeping all before second space
    initials <- sub("\\s$", ".", sub("(\\S*\\s+\\S+)$", "", colls_b))
    x[tf][tfa][tfb] <- paste(initials, sub("\\S*\\s\\S*\\s", "", colls_b))
  }
  df$recordedBy[cen] <- x

  #_____________________________________________________________________________
  # UTEP collections/ but we grepl with institution code in this case

  # Extracting TWO COLLECTORS that are separated by COMMA
  # We need to place in this order, not before, othwerwise it will mess the cleaning
  # Two authors no abbreviation and just one comma
  # so, initials like... "Pedro Pable Moreno, Walter Robleto"
  utep <- which(df$institutionCode %in% "UTEP")
  utep <- utep[grepl("(\\s[[:upper:]][[:lower:]]+{2,})[,](\\s[[:upper:]][[:lower:]]+{2,})",
                     df$recordedBy[utep])]
  if (length(utep) > 0) {
    df$addCollector[utep] <- gsub(", ", "", .extract(df$recordedBy[utep], ",.+"))
    df$recordedBy[utep] <- gsub(",.+", "", df$recordedBy[utep])
  }

  return(df)
}


#_______________________________________________________________________________
# Correcting specific collector names ####

# Replace names that are exactly any of `from`
.fix_exact <- function(x, from, to) {
  x[x %in% from] <- to
  x
}

# Replace names that match the regular expression `pattern`
.fix_regex <- function(x, pattern, to) {
  x[grepl(pattern, x)] <- to
  x
}

.std_specific_coll <- function(df){

  x <- df$recordedBy

  #_____________________________________________________________________________
  # Cleaning Brazilian collectors

  # A
  x <- .fix_regex(x, "^A.*M.* Amorim", "A. M. A. Amorim")
  x <- .fix_exact(x, "W. Anderson", "W. R. Anderson")
  x <- .fix_exact(x, c("Andrade-Lima", "A. D. Andrade-Lima"), "D. Andrade-Lima")
  x <- .fix_exact(x, "J. B. Christophore Fusée Aublet", "J. B. C. F. Aublet")

  # B
  x <- .fix_exact(x, "G. Maciel Barroso", "G. M. Barroso")
  x <- .fix_exact(x, "R. Henry Beddome", "R. H. Beddome")
  x <- .fix_exact(x, "M. M. Brandão", "M. Brandão")
  x <- .fix_regex(x, "R. P. Bel|^Belem$", "R. P. Belém")
  x <- .fix_exact(x, "Burchell", "W. J. Burchell")
  x <- .fix_exact(x, c("B. Marx", "R. Burle Marx"), "R. Burle-Marx")

  # C
  x <- .fix_exact(x, "D. B. O. S. Cardoso", "D. Cardoso")
  x <- .fix_exact(x, "P. Cavalcante", "P. B. Cavalcante")
  x <- .fix_exact(x, "T. Barbosa Cavalcanti", "T. B. Cavalcanti")
  x <- .fix_exact(x, c("C. A. C. Ferreira", "C. A. Cid Ferreira", "C. A. Cid",
                       "C. A .Cid", "C. A. F."), "C. A. Cid-Ferreira")
  x <- .fix_regex(x, "^C. A. Cid|^C. A. C. Ferreira", "C. A. Cid-Ferreira")
  x <- .fix_exact(x, "A. S. Conceiçaõ", "A. S. Conceição")
  x <- .fix_exact(x, "T. Alfred Coward", "T. A. Coward")
  x <- .fix_exact(x, "R. C. Monteiro Costa", "R. C. M. Costa")

  # D
  x <- .fix_exact(x, c("Daly", "D. Daly"), "D. C. Daly")
  x <- .fix_exact(x, c("M. E. Spence Davidson", "M. E. Davidson"), "M. E. S. Davidson")
  x <- .fix_regex(x, "Ducke", "A. Ducke")
  x <- .fix_exact(x, c("W. Adolpho Ducke", "W. A. Ducke", "A. Duck"), "A. Ducke")

  # F
  x <- .fix_exact(x, "M. Clara Ferreira", "M. C. Ferreira")
  x <- gsub("^Forzza|^R. Forzza|^R. C. Forzzan", "R. C. Forzza", x)
  x <- .fix_regex(x, "F. Franca|F. FrançA", "F. França")
  x <- .fix_exact(x, c("R. Froes", "R. Lemos Froes-Cpatu", "Froes", "Fróes", "R. L. Froes",
                       "R. L. FrÃ³es"), "R. L. Fróes")
  x <- .fix_regex(x, "R. L. Fr", "R. L. Fróes")
  x <- .fix_exact(x, "H. Ogg Forbes", "H. O. Forbes")

  # G
  x <- .fix_exact(x, c("A. F. Marie Glaziou", "A. Glaziou", "Glaziou"), "A. F. M. Glaziou")
  x <- gsub("^M. L. S. Guedes$|^M. Guedes$|^M. L. Silva Guedes|ML. Silva Guedes|M. Lenise Guedes",
            "M. L. Guedes", x)
  x <- .fix_exact(x, "A. Gentry", "A. H. Gentry")

  # H
  x <- .fix_exact(x, c("R. Harley", "Harley", "R. H. Harley", "R. M. Harkey"), "R. M. Harley")
  x <- .fix_regex(x, "Hatschbach", "G. G. Hatschbach")
  x <- .fix_exact(x, "M. J. G. Hopkins", "M. Hopkins")
  x <- .fix_exact(x, c("F. W. R. Hostmann", "Hostmann", "F. W. Hostmann"), "W. R. Hostmann")

  # I
  x <- gsub("J. R. Vieira Iganci|J. R. Iganci|^Iganci$|J. Iganci", "J. R. V. Iganci", x)
  x <- .fix_exact(x, c("H. Irwin", "Irwin"), "H. S. Irwin")

  # K
  x <- gsub("^A. C. Krapovickas", "A. Krapovickas", x)
  x <- .fix_exact(x, c("B. Alexander Krukoff", "B. Krukoff", "Krukoff"), "B. A. Krukoff")

  # L
  x <- .fix_exact(x, "E. Junqueira Leite", "E. J. Leite")
  x <- gsub("^H..C. Lima|^H.c Lima|^H. Cavalcante Lima|^H. C Lima|^H. C. D. E. Lima|^H. C. dde Lima$|^H. C. De Lima|^H. C. DeLima|^H. c. Lima",
            "H. C. Lima", x)
  x <- gsub("^G. P. çLewis$|G. Peter Lewis|G.PLewi", "G. P. Lewis", x)
  x <- .fix_exact(x, "Little", "E. L. Little")
  x <- .fix_exact(x, "Lombardi", "J. A. Lombardi")
  x <- .fix_exact(x, "Lorenzi", "H. Lorenzi")
  x <- .fix_exact(x, "Luetzelburg", "P. von Luetzelburg")
  x <- .fix_exact(x, "Ee.Nic Lughadha", "E. Nic Lughadha")

  # M
  x <- .fix_exact(x, c("Martius", "C. F. vanMartius", "C. F. P. Martius",
                       "C. F. Philipp von Martius", "K. F. von. Martius"), "C. F. P. von Martius")
  x <- .fix_exact(x, "Maas", "P. J. M. Maas")
  x <- gsub("^S.* Miotto", "S. T. S. Miotto", x)
  x <- .fix_exact(x, "Martinelli", "G. Martinelli")
  x <- .fix_exact(x, c("L. A. Mattos Silva", "L. A. Matts Silva"), "L. A. Mattos-Silva")
  x <- .fix_exact(x, c("H. L. Mello Barreto", "H. L. M. Barreto"), "H. L. Mello-Barreto")
  x <- .fix_exact(x, c("R. Mello Silva", "R. mello-silva", "R. Mello"), "R. Mello-Silva")
  x <- .fix_exact(x, c("Mori", "S. Mori"), "S. A. Mori")
  x <- .fix_exact(x, "P. Watson Moonlight", "P. W. Moonlight")

  # O
  x <- .fix_exact(x, "R. Paulo Orlandi", "R. P. Orlandi")

  # P
  x <- gsub("^J. Paula Souza$|^J. Paula-Sousa$|^J. Paulo-Souza$|^J. Paula$|^J. P. Souza$|J. P. Sousa$",
            "J. Paula-Souza", x)
  x <- .fix_exact(x, "R. Toby Pennington", "R. T. Pennington")
  x <- gsub("^G. P. Silva$", "G. Pereira-Silva", x)
  x <- .fix_exact(x, "G. C. Pereira Pinto", "G. C. P. Pinto")
  x <- .fix_exact(x, c("J. James Pipoly", "J. Pipoly", "I. I. I. JJ-Pipoly"), "J. J. Pipoly")
  x <- .fix_exact(x, c("J. Pirani", "Pirani", "J. Rubens Pirani"), "J. R. Pirani")
  x <- .fix_regex(x, "^J.*M.*Pires|^J. Murça Pires$", "J. M. Pires")
  x <- .fix_exact(x, "G. T. Prace", "G. T. Prance")
  x <- .fix_regex(x, "^C. E. B. Proen|^C.* Proenc|^C. E. Barnes Proença$", "C. E. B. Proença")

  # Q
  x <- .fix_exact(x, "L. P. Queiróz", "L. P. Queiroz")
  x <- gsub("^L..P. Queiroz|^L. P. Queiroz.$|^L. P. De Queiroz", "L. P. Queiroz", x)

  # R
  x <- gsub("^-R. -- Reitz|Pe. Raulino Reitz|^Reitz$|^P. R. Reitz$", "R. Reitz", x)
  x <- .fix_exact(x, c("C. Tolledo Rizzini", "Rizzini"), "C. T. Rizzini")

  # S
  x <- .fix_exact(x, "Sellow", "F. Sellow")
  x <- .fix_exact(x, "Spruce", "R. Spruce")
  x <- gsub("^M. Fragomenir Simon$|^M. Simon|^Simon$|^M. Fragomeni Simon$", "M. F. Simon", x)
  x <- gsub("^V. C. Sousa|^V. Castro Souza$ ", "V. C. Souza", x)
  x <- .fix_regex(x, "^R. Sch.*Rodrigues", "R. Schütz-Rodrigues")
  x <- .fix_exact(x, "Schwacke", "C. A. W. Schwacke")
  x <- .fix_exact(x, c("Schultes", "R. Evans Schultes"), "R. E. Schultes")

  # T
  x <- .fix_exact(x, c("W. Wayt Thomas", "W. Thomas"), "W. W. Thomas")

  # V
  x <- .fix_regex(x, "^J.*Valls$", "J. F. M. Valls")

  # Z
  x <- .fix_exact(x, c("D. C. Zapii", "D. Zappi"), "D. C. Zappi")
  x <- .fix_exact(x, "J. Zarucchi", "J. L. Zarucchi")

  #_____________________________________________________________________________
  # Cleaning general collectors
  x[grepl("Academia Brasileira de C", df$recordedByOriginal)] <- "Academia Brasileira de Ciências"
  x[grepl("Equipe do Jardim Bot", df$recordedByOriginal)] <- "Equipe do Jardim Botânico de Brasília"

  #_____________________________________________________________________________
  # Cleaning Spanish-like collectors
  x <- .fix_exact(x, "N. A. Zamora Villalobos", "N. Zamora")
  x <- .fix_exact(x, "G. Ibarra Manriquez", "G. Ibarra")
  x <- .fix_exact(x, "H. Mendoza Cifuentes", "H. Mendoza")
  x <- .fix_exact(x, "J. Rafael Garcia", "J. R. García")
  x <- .fix_exact(x, "M. Sousa Sánchez", "M. Sousa")
  x <- .fix_exact(x, c("A. Reyes", "A. Reyes Garcia"), "A. Reyes-García")
  x <- .fix_exact(x, c("R. Vasquez Martinez", "Rod. Vasquez", "Rod. Vásquez", "R. Vasquez"), "R. Vásquez")
  x <- .fix_exact(x, c("J. Schunke Vigo", "J. Schunke", "J. V. Schunke", "J. M. Schunke"),
                  "J. Schunke-Vigo")
  x <- .fix_exact(x, c("M. M. arbo", "M. N. Arbo"), "M. M. Arbo")

  df$recordedBy <- x

  return(df)
}


#_______________________________________________________________________________
# Cleaning Asian-like names when they are in unicode characters from "UTF-8" encoding.

# Pre-cleaning before standardizing the collector names  ####
.preunicodeclean <- function(df, tf){

  message(".preunicodeclean $recordedBy Asian names #1")

  # "<U+3001>" is the Chinese comma separating collectors, and "<U+7B49>" means "etc."
  x <- df$recordedBy[tf]
  etc <- grepl("\\s*<U[+]7B49>$", x)
  df$addCollector[tf][etc] <- "et al."
  x <- gsub("\\s*<U[+]7B49>$", "", x)
  df$recordedBy[tf] <- gsub("\\s+", " ", gsub("\\s*<U[+]3001>\\s*", ",", x))

  tfa <- grepl("[,]|[;]", df$recordedBy[tf])
  tfb <- grepl("(.*,.*,){1,}|(.*;.*;){1,}", df$recordedBy[tf][tfa])
  idx <- which(tf)[tfa]

  # Chinese names with more than two collectors
  if (any(tfb)) {
    df$addCollector[idx][tfb] <- "et al."
    df$recordedBy[idx][tfb] <- gsub(",.+", "", df$recordedBy[idx][tfb])
  }

  # Chinese names with just two collectors
  if (any(!tfb)) {
    message(".preunicodeclean $recordedBy Asian names #2")
    df$addCollector[idx][!tfb] <- gsub(",", "", .extract(df$recordedBy[idx][!tfb], "[,|;].+"))
    df$recordedBy[idx][!tfb] <- gsub(",.+", "", df$recordedBy[idx][!tfb])
  }

  # detect, pattern to remove the additional collectors, message number
  # Chinese names with more than two collectors separated by space
  # "<U+738B><U+542F><U+65E0> <U+5218><U+745B>"
  # Chinese names with more than two collectors that are not separated
  # "T.N.Liou<U+3001>P.C.Tsoong"
  rules <- list(c("(.*[>]\\s[<][[:upper:]]){2,}", "\\s.+", "#3"),
                c("[[:upper:]][[:lower:]]+[<][[:upper:]]", "[<].+", "#4"))
  for (r in rules) {
    tf <- grepl(r[1], df$recordedBy)
    if (any(tf)) {
      message(".preunicodeclean $recordedBy Asian names ", r[3])
      df$addCollector[tf] <- "et al."
      df$recordedBy[tf] <- gsub(r[2], "", df$recordedBy[tf])
    }
  }

  # Chinese names with mixed unicode and Latin-like characters
  # Extracting names also seperated by a space
  # "T.N.Liou <U+3001>P.C.Tsoong"
  tf <- grepl("[[:upper:]][[:lower:]]+\\s[<][[:upper:]]", df$recordedBy)
  idx <- which(tf)[grepl("([[:lower:]]+)$", df$recordedBy[tf])]
  if (length(idx) > 0) {
    message(".preunicodeclean $recordedBy Asian names #5")
    df$addCollector[idx] <- "et al."
    df$recordedBy[idx] <- gsub("\\s.+", "", df$recordedBy[idx])
  }

  # Chinese names with two collectors separated by space
  # "<U+738B><U+542F><U+65E0> <U+5218><U+745B>"
  tf <- grepl("(.*[>]\\s[<][[:upper:]]){1}", df$recordedBy)
  if (any(tf)) {
    message(".preunicodeclean $recordedBy Asian names #6")
    df$addCollector[tf] <- ifelse(is.na(df$addCollector[tf]),
                                  gsub(".*?\\s", "", df$recordedBy[tf]),
                                  df$addCollector[tf])
    df$recordedBy[tf] <- gsub("\\s.+", "", df$recordedBy[tf])
  }

  # The same collector in Latin followed by Chinese characters, keep just the
  # Latin name, e.g. "T.C.Huang <U+9EC3><U+589E><U+6CC9>"
  tf <- grepl("[[:upper:]][[:lower:]]+\\s[<][[:upper:]]", df$recordedBy)
  if (any(tf)) {
    message(".preunicodeclean $recordedBy Asian names #7")
    df$recordedBy[tf] <- gsub("\\s.+", "", df$recordedBy[tf])
  }

  return(df)
}


#_______________________________________________________________________________
# Converting Asian-like names in unicode characters into Latin names ####
.unicodeclean <- function(df) {

  message(".unicodeclean $recordedBy Asian-like names in unicode")

  df$recordedBy <- .han_to_latin(df$recordedBy)
  df$addCollector <- .han_to_latin(df$addCollector)

  return(df)
}

# Names written just with unicode escapes, e.g. "<U+674E><U+5149><U+7167>"
# or groups like "780<U+690D><U+88AB><U+7EC4>"
.is_han <- function(x) {
  grepl("<U[+]", x) & grepl("^(<U[+][0-9A-Fa-f]{4}>|[0-9 -])+$", x)
}

# Convert Chinese names written as unicode escapes into Latin names, as Chinese
# authors cite themselves in publications: initials of the given name followed
# by the family name, e.g. "<U+674E><U+5149><U+7167>" (Li Guang-Zhao) into "G. Z. Li"
.han_to_latin <- function(x) {
  tf <- grepl("<U[+][0-9A-Fa-f]{4}>", x)
  if (!any(tf)) return(x)

  # From unicode escape codes into Chinese characters, e.g. "\u674e\u5149\u7167"
  han <- stringi::stri_unescape_unicode(gsub("<U\\+([0-9A-Fa-f]{4})>", "\\\\u\\1", x[tf]))

  # Keep names of teams and groups in full, e.g. "780<U+690D><U+88AB><U+7EC4>"
  # (vegetation group 780) into "780 Zhi Bei Zu"
  # (dui = team, zu = group, yuan = institute, suo = office, guan = museum)
  group <- grepl("[0-9]|\u961f|\u7ec4|\u9662|\u6240|\u9986", han)
  latin <- .han_translit(han[group])
  x[tf][group] <- .title_case(gsub("([0-9])([[:alpha:]])", "\\1 \\2", trimws(latin)))

  x[tf][!group] <- vapply(han[!group], .han_name, character(1), USE.NAMES = FALSE)

  x
}

# From Chinese characters into Latin syllables, e.g. "li guang zhao"
.han_translit <- function(x) {
  stringi::stri_trans_general(x, "Han-Latin; Latin-ASCII")
}

# A single Chinese name, e.g. "\u674e\u5149\u7167" into "G. Z. Li"
.han_name <- function(h) {
  chars <- strsplit(h, "")[[1]]
  chars <- chars[grepl("\\p{Han}", chars, perl = TRUE)]
  n <- length(chars)
  if (n == 0) return(NA_character_)

  # Family names of two characters, e.g. Ouyang, Sima, Zhuge
  compound <- c("\u6b27\u9633", "\u6b50\u967d", "\u53f8\u9a6c", "\u53f8\u99ac",
                "\u8bf8\u845b", "\u8af8\u845b", "\u4e0a\u5b98", "\u7687\u752b",
                "\u53f8\u5f92", "\u4e1c\u65b9", "\u6771\u65b9", "\u590f\u4faf",
                "\u6155\u5bb9", "\u5c09\u8fdf", "\u4ee4\u72d0", "\u516c\u5b59",
                "\u957f\u5b59", "\u5b87\u6587", "\u7aef\u6728")
  k <- if (n > 2 && paste(chars[1:2], collapse = "") %in% compound) 2 else 1

  # Family names read differently from the common reading of the character,
  # e.g. Zeng instead of Ceng
  reading <- c("\u66fe" = "Zeng", "\u8983" = "Qin", "\u5355" = "Shan", "\u55ae" = "Shan",
               "\u89e3" = "Xie", "\u4ec7" = "Qiu", "\u67e5" = "Zha", "\u7fdf" = "Zhai",
               "\u6734" = "Piao", "\u7f2a" = "Miao", "\u533a" = "Ou", "\u5340" = "Ou")
  family <- paste(chars[1:k], collapse = "")
  family <- if (family %in% names(reading)) {
    reading[[family]]
  } else {
    .title_case(gsub("\\s", "", .han_translit(family)))
  }
  if (n == k) return(family)

  given <- .han_translit(chars[(k + 1):n])
  paste(c(paste0(toupper(substr(given, 1, 1)), "."), family), collapse = " ")
}


#_______________________________________________________________________________
# Standardize additional collectors in the newly created column addCollector ####
.addcollclean <- function(df) {

  message(".addcollclean $addCollector")

  y <- df$addCollector

  # Inserting NAs in and empty cell
  y <- gsub("^$|^\\s$", NA, y)
  y <- .lower_to_title(y)

  # Deleting any numbers and blank space at the beginning and end of the cell
  y <- .gsub_all(y, c("[0-9]" = "",
                      "^[[:space:]]" = "",
                      "[[:space:]]{2}" = " ",
                      "[[:space:]]$" = ""))

  # Adding NA for cells with "s.c."
  y[y %in% "s.c."] <- NA

  # Adding et al. for groups and more than two collectors
  for (p in c("Grupo|Alunos|Plantas Vasculares|British Guiana Forestry|de estudios",
              "[[:lower:]]+\\s+[[:upper:]][[:lower:]]+[,]+\\s+[[:upper:]][[:lower:]]+\\s+[[:upper:]][[:lower:]]+",
              "(.*[-].*[-]){1,}",
              # Examples like "L Cardin-A C Borges"
              "\\s+[[:upper:]][[:lower:]]+[-]+[[:upper:]]+\\s",
              "(.*[[:space:]].*[[:space:]]){3,}")) {
    y[grepl(p, y)] <- "et al."
  }

  y <- .gsub_if(y, "[;]+[[:upper:]][[:lower:]]+[,]", ";", "")
  y <- .gsub_if(y, "([[:upper:]][.]){1,}[[:upper:]][[:lower:]]", "[.]", ". ")
  # Use sub as it matches only the first occurrence of pattern
  tf <- grepl("([[:upper:]][.]){2,}\\s+[[:upper:]][[:lower:]]", y)
  y[tf] <- sub("[.]", ". ", y[tf])
  # Deleting last name after last space
  y <- .gsub_if(y, "([[:upper:]][.]){1,}\\s+[[:upper:]][[:lower:]]+\\s+[[:upper:]][.]",
                "\\s[^ ]+$", "")

  # Cleaning collector names like "Acero, E."
  y <- .move_to_front(y, grepl(",", y), ",.+",
                      fun = function(i) gsub("^\\s", "", gsub(",", "", i)))

  # Make the names in just the first letter capitalized
  tf <- grepl("([[:upper:]]){4,}", y)
  y[tf] <- .title_case(y[tf])

  # Cleaning examples like "Egler WA", "Trotz N"
  tf <- grepl("[[:lower:]]+\\s([[:upper:]]){1,}$", y)
  y <- .move_to_front(y, which(tf)[!grepl("[.]", y[tf])], "\\s.+",
                      fun = function(i) gsub("^\\s", "", i))

  # Cleaning examples like "F. Chigo S", "Patricia Gómez A."
  y <- .gsub_if(y, "[[:lower:]]+\\s([[:upper:]]){1,}$", "\\s[^ ]+$", "")
  y <- .gsub_if(y, "\\s[[:upper:]][[:lower:]]+\\s([[:upper:]][.]){1}$", "\\s[^ ]+$", "")

  # Cleaning examples like "Sueroque F.", "Jaramillo R."
  y <- .move_to_front(y, grepl("[[:lower:]]+\\s([[:upper:]][.]){1}$", y), "\\s.+",
                      fun = function(i) gsub("^\\s", "", i))

  # Cleaning examples like ".", " . " and "TA Naves,"
  y[grepl("^[.]$", y)] <- NA
  y <- .gsub_all(y, c("\\s[.]\\s" = ". ", "," = ""))

  # Cleaning examples like "Steege H ter", "Paie I bin", "Wilde-Duyfjes BEE de"
  for (p in c("[[:lower:]]+\\s[[:upper:]]\\s[[:lower:]]+$",
              "[[:upper:]][[:lower:]]+\\s([[:upper:]]){2,}")) {
    y <- .move_to_front(y, grepl(p, y), "\\s.+", fun = function(i) gsub("^\\s", "", i))
  }

  # Cleaning particles like de, da, dos, do
  y <- .rm_particles(y, squish = FALSE)

  # Abbreviate first names like "Sergio M Faria", "Domingos S Cardoso"
  y <- .abbrev_to_front(y, grepl("[[:lower:]]+\\s+([[:upper:]]{1,})+\\s", y),
                        dot = FALSE, sep = "")

  # Abbreviate names like "David J.N. Hind", "Jorge C. A. Lima", "Grady L. Webster"
  y <- .abbrev_to_front(y, grepl("[[:upper:]][[:lower:]]+\\s+(.*[[:upper:]][.]){1,}\\s+[[:alpha:]]{3}", y))

  # Abbreviate first name like "Sergio Faria"
  y <- .abbrev_to_front(y, grepl("^[[:upper:]][[:lower:]]+\\s+[[:upper:]][[:lower:]]+", y))

  # Adding full period in names like D Cardoso, DD Cardoso, DDD Cardoso
  y <- .move_to_front(y, grepl("^([[[:upper:]]){1,}\\s[[:upper:]][[:lower:]]", y),
                      "^(\\S*\\s+)", remove = "^\\S*.",
                      fun = function(i) .spell_initials(gsub(" $", "", i)))

  y <- .gsub_if(y, "([[[:upper:]][.]){2,}|[[[:upper:]][.]{1,}[[:upper:]][[:lower:]]+",
                c("[.]", "[[:space:]]{2}", "[.]$", "[[:space:]]$"), c(". ", " ", "", ""))

  # Cleaning examples like "H ter Steege", "I bin Paie", "PP-H But"
  tf <- grepl("([[[:upper:]]){1,}\\s", y)
  y <- .move_to_front(y, tf, "^(\\S*\\s+)", remove = "^\\S*.",
                      fun = function(i) .spell_initials(gsub(" $", "", i)))
  # Correcting the examples like "P. P- . H." by first removing the first duplicated initial
  idx <- which(tf)[grepl("[-]\\s[.]", y[tf])]
  y[idx] <- gsub("[-]\\s[.]\\s", ".-", gsub("^(\\S*\\s+)", "", y[idx]))

  # Cleaning examples like "J. R. M Ferreira"
  tf <- grepl("([[[:upper:]]){1,}\\s", y)
  idx <- which(tf)[grepl("([[[:upper:]][.])", y[tf])]
  y[idx] <- gsub("[.][.]\\s", "", gsub("\\s", ". ", y[idx]))

  # Cleaning examples like "C. H. R.  Paula.", "G. Calero Ch."
  tf <- grepl("\\s[[[:upper:]][[:lower:]]+[.]", y)
  y[tf] <- gsub("[.]$", "", y[tf])
  idx <- which(tf)[grepl("\\s[[[:upper:]][[:lower:]]+\\s", y[tf])]
  y[idx] <- gsub("\\s[^ ]+$", "", y[idx])

  # Further cleaning examples like "A. .R. Lopes" and spaces
  y <- .gsub_all(y, c("[.]\\s[.]" = ". ", "^[[:space:]]" = "", "[[:space:]]{2}" = " "))

  # Cleaning examples like "AORibeiro"
  tf <- grepl("^[[:upper:]]{2,}[[:lower:]]+", y)
  if (any(tf)) {
    # Separate the initials from the surname first
    y[tf] <- gsub("([[:upper:]])([[:upper:]][[:lower:]])", "\\1 \\2", y[tf])
    initials <- gsub(" $", "", .extract(y[tf], "^(\\S*\\s+)"))
    initials <- gsub("([[:upper:]])([[:upper:]])", "\\1 \\2", initials)
    initials <- gsub("\\s$", "", gsub(" ", ". ", paste0(initials, " ")))
    y[tf] <- paste(initials, sub("^\\S*.", "", y[tf]))
  }

  df$addCollector <- y

  return(df)
}


#_______________________________________________________________________________
# Pre-cleaning collector numbers before standardizing collector names ####
.prenbrclean <- function(df){

  # Clean e.g. "Nakajima, J.N. 3101;...", "Harley, R.M. 20580;..."
  # Finding examples like "Martius, C.F.P. von (no. Obs. 1935)"
  # "Martius, C.F.P. von (no. [Obs. 1383])", "Luetzelburg, P. von (no. 142)"
  # detect, pattern to remove from the name, message number
  rules <- list(c("[[:digit:]];", "\\d", "#1"),
                c("[(]no[.]\\sObs[.]|[(]no[.]\\s[[]Obs[.]|\\s[(]no[.]\\s", "\\s[(].+", "#2"))
  for (r in rules) {
    tf <- grepl(r[1], df$recordedBy)
    if (any(tf)) {
      message(".prenbrclean $recordedBy numbers ", r[3])
      # Extracting only numbers
      #https://stackoverflow.com/questions/14543627/extracting-numbers-from-vectors-of-strings
      df$recordNumber[tf] <- ifelse(is.na(df$recordNumber[tf]),
                                    as.character(as.numeric(gsub("\\D", "", df$recordedBy[tf]))),
                                    as.character(df$recordNumber[tf]))
      df$recordedBy[tf] <- gsub(r[2], "", df$recordedBy[tf])
    }
  }

  # Clean e.g. "8470 G.H. Turner", "67-1240 N.C. Henderson", "1042 J. Campbell-Snelling, M. Chambers"
  tf <- grepl("^[0-9][0-9-]*\\s([[:upper:]][.]|[[:upper:]]\\s)", df$recordedBy)
  if (any(tf)) {
    message(".prenbrclean $recordedBy numbers #3")
    df$recordNumber[tf] <- gsub("\\s.+", "", df$recordedBy[tf])
    # Remove all before the the first comma and space
    df$recordedBy[tf] <- sub(".*?\\s", "", df$recordedBy[tf])
  }

  # Clean e.g. "Mark Hughes Sumatra 2011"
  if (any(grepl("\\sSumatra\\s", df$recordedBy))) {
    message(".prenbrclean $recordedBy numbers #6")
    df$recordedBy <- gsub("\\sSumatra.+", "", df$recordedBy)
  }

  # Clean e.g. "C Davis 812"
  tf <- grepl("[[:upper:]][[:lower:]]+\\s[0-9]", df$recordedBy)
  if (any(tf)) {
    message(".prenbrclean $recordedBy numbers #4")
    df$recordedBy[tf] <- gsub(";\\sBiology.*", "", df$recordedBy[tf])
    df <- .rm_before_after(df, tf, ".*\\s", "\\s+[^ ]+$")
    df$recordNumber[tf] <- gsub("[[:alpha:]]", NA, df$recordNumber[tf])
  }

  # Clean e.g. "C Davis D-14"
  tf <- grepl("[[:upper:]][[:lower:]]+\\s[[:alpha:]][-][0-9]", df$recordedBy)
  if (any(tf)) {
    message(".prenbrclean $recordedBy numbers #5")
    df <- .rm_before_after(df, tf, ".*\\s", "\\s+[^ ]+$")
  }

  # detect, pattern before the number, pattern after the number, message number
  # "Jesus, M.L.B. de 132"
  # "Brade, A.C. 17713; Altamiro, B. & Mello Filho, L.E."
  # "Conceicao, A.A. 1161"
  # "L.M.NASCIMENTO481"
  rules <- list(c("[[:upper:]][.]\\s[[:lower:]]+\\s[0-9]+$", ".*\\s", "\\s+[^ ]+$", "#7"),
                c("([[:upper:]][.]){1,}\\s[0-9]+;", ";.*", "[0-9]+", "#8"),
                c("([[:upper:]][.]){1,}\\s[0-9]+$", ".*\\s", "\\s+[^ ]+$", "#9"),
                c("[[:upper:]][.][[:upper:]]+[0-9]+$", ".*[[:upper:]]", "[0-9]+[^ ]+$", "#10"))
  for (r in rules) {
    tf <- grepl(r[1], df$recordedBy)
    if (any(tf)) {
      message(".prenbrclean $recordedBy numbers ", r[4])
      df <- .rm_before_after(df, tf, r[2], r[3])
    }
  }

  # Still working on these patterns
  # Clean e.g.
  # A. M. Girardi-Deiro et al, 1815
  # Girardi-Deiro, 1174
  # L. A. Z. Machado et al. 1816
  # L. A. Z. Machado. 1807
  # A.M.Girardi-Deiro et al .1810
  # A. M. Girardi-Deiro et V.A. Marin,1818
  # Melo, E. 1596 et al.
  # Berg, C.C. P 19770; Bisby, F.A. & Monteiro, O.P.
  # Queiroz, L.P.; Moradillo-Mello, R.C.B. & Pinto, N.R. 1194
  # H.M. Dias 108, D. Medina
  # Queiroz, L.P.de 7159
  # L. Scur nº174
  # R. Wasum 1335 a
  # Santos, A.K.A 371
  # R. Záchia, 1914
  # douglass18 or julia_santos1998
  # A. Carvalho (1)
  # J.S. Silva (1); A.L.B. Sartori & F.M. Alves
  # "Acevedo-Rodríguez 16730", "Hatschbach Sobrinho 23446",
  # "Adalardo de Oliveira 2775", "Fernandes s.n. (EAC 11333)"

  return(df)
}

# Side function to move the number from the name into $recordNumber
.rm_before_after <- function(df, tf, pattern_before, pattern_after){
  # Remove everything BEFORE the number
  df$recordNumber[tf] <- sub(pattern_before, "", df$recordedBy[tf])
  # Remove everything AFTER the name
  df$recordedBy[tf] <- sub(pattern_after, "", df$recordedBy[tf])

  return(df)
}


#_______________________________________________________________________________
# Auxiliary function for cleaning numbers at $recordNumber ####
.std_recordNumber <- function(df) {

  message(".std_recordNumber $recordNumber")

  x <- as.character(df$recordNumber)
  x_original <- df$recordNumberOriginal

  # Adding NAs in unumbered collections
  x[x %in% c("s/n", "s.n.", "s. n.", "PCDs/n", "S.n.", "S.N.", "S. N.", "s.n",
             "s,n,", "sn", "SN", "N", "N.", "n", "n.", "nd", "possibly")] <- NA
  x[grepl("s[.]n[.]", x)] <- NA

  # Adding NAs to empty cells and "s/nº"
  # https://en.wikipedia.org/wiki/ISO/IEC_8859-1
  x <- gsub("^$", NA, trimws(x))
  x <- gsub("s/n\\xba", NA, x)

  # Remove all after last space
  x <- .gsub_if(x, "[[:upper:]][[:lower:]]+\\s([0-9]){1,}\\s[[:print:]]", "\\s+[^ ]+$", "")

  # Remove spaces at beginning and end
  x <- .gsub_all(x, c("^\\s" = "", "\\s$" = "", "[[:space:]]{2}" = " "))

  # Remove names, i.e. all before the last space
  x <- .gsub_if(x, "[A-Za-z]", ".+? ", "")

  # General cleaning
  x <- .gsub_all(x, c("&nf;" = "", "^-" = "", "-$" = "", "CFCR-" = "CFCR"))

  # Deleting leading zeros
  # https://stackoverflow.com/questions/23538576/removing-leading-zeros-from-alphanumeric-characters-in-r
  tf <- !grepl("/|-", x)
  x[tf] <- gsub("(?<![0-9])0+", "", x[tf], perl = TRUE)

  # Finding examples like "Harley22573": keep just numbers
  x <- .gsub_if(x, "^[[:upper:]][[:lower:]]+([0-9]){1,}$", "[^0-9.]", "")
  x <- gsub("-Duplicate$", "", x)

  tf <- grepl("[[:upper:]][[:lower:]]+\\s([0-9]){1,}\\s[A-Z]", x_original)
  x[tf] <- gsub("^(\\S*\\s\\S*\\s+)", "", x_original[tf])
  tfa <- grepl("[0-9]\\s[A-Za-z]", x_original) & !tf
  x[tfa] <- gsub("\\s", "", x_original[tfa])

  x[grepl("^[-]$", x)] <- NA
  x <- gsub("[#][?][#]", "", x)

  # Collection numbers as date
  x[grepl("([0-9]){1,}[/]([0-9]){1,}[/]([0-9]){1,}", x)] <- NA

  x <- .gsub_if(x, "[[]|[]]|[(]", c("\\s.+", "[[]", "[]]", "[(]", "[)]"), "")

  x[grepl("s[/]n|s[.]n[.]", x)] <- NA

  # Fixing examples "Bullock, AA  712" "Heller, AA  6135"
  tf <- grepl("[[:lower:]]+[,]\\s([[:upper:]]){1,}\\s[0-9]", x_original)
  if (any(tf)) {
    tfa <- tf & grepl("[/]", x_original)
    x[tfa] <- gsub("[^0-9.-/]", "", x_original[tfa])
    tfa <- tf & grepl("[-]", x_original)
    x[tfa] <- gsub("^[-]", "", gsub("[^0-9.-]", "", x_original[tfa]))
  }

  x <- .gsub_all(x, c("[.]\\s|[?]|[*]|p[.]p[.]|[#]" = "", "--" = "-"))
  x <- .gsub_if(x, "& ", "&.*", "")
  x <- gsub(",.*", "", x)

  # Cleaning collection numbers that appear as dates
  x[!is.na(as.Date(x, format = "%d/%m/%Y")) |
      !is.na(as.Date(x, format = "%m/%d/%Y"))] <- NA

  x[x %in% "s"] <- NA
  x <- gsub("[.]", "", x)

  df$recordNumber <- x

  return(df)
}
