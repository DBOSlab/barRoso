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
#' - Converting unicode (e.g., Chinese) to Latin names
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
#' @importFrom stringr str_detect str_extract_all str_replace
#' @importFrom stringi stri_replace_first_regex
#' @importFrom tmcn toPinyin strextract
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

  # Get number of specimens in the dataset
  l_rows <- nrow(df)

  # Links to useful codes and regular expressions for filtering char patterns
  # by using regular expressions [regex]
  # https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf
  # https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html
  # https://stringr.tidyverse.org/articles/regular-expressions.html
  # http://www.endmemo.com/program/R/grepl.php
  # https://rstudio-pubs-static.s3.amazonaws.com/74603_76cd14d5983f47408fdf0b323550b846.html

  #_____________________________________________________________________________
  # Preliminary cleaning of Asian-like names when they are in unicode characters ####
  # from "UTF-8" encoding. Then  at the end of this script we translate the Chinese
  # characters into Latin alphabet
  tf <- grepl("[+]", df$recordedBy)
  if (any(tf)) {
    df <- .preunicodeclean(df, tf)
  }

  #_____________________________________________________________________________
  # When there are MORE THAN TWO COLLECTORS
  # Keep just the main collector and remove all other additional collectors
  # but paste ET AL. in the the newly created $addCollector
  # Adding et al. at $addCollector when $recordedBy has for more than two collectors
  df <- .deletal(df)

  #_____________________________________________________________________________
  # Pre-cleaning $recordedBy and $recordNumber
  df <- .prenbrclean(df)

  #_____________________________________________________________________________
  # Pre-cleaning $recordedBy
  df <- .precollclean(df)

  #_____________________________________________________________________________
  # When there are ONLY TWO COLLECTORS
  # Keep just the main collector
  # but put the additional collector in newly created at $addCollector

  # Clean e.g. "Cardoso, D.|Santos, Q."
  pos <- which(grepl("[|]", df$recordedBy))
  if (length(pos) > 0) {
    extract_pattern = "[|].+"
    df <- .deepcollclean(df, pos, extract_pattern, l_rows)
  }

  # Clean e.g. "Olga Kotchetkoff Henriques e Andrei Furlan"
  pos <- which(grepl("\\s[[:upper:]][[:lower:]]+\\s[e]\\s[[:upper:]][[:lower:]]+\\s",
                     df$recordedBy))
  if (length(pos) > 0) {
    extract_pattern = "\\s[e]\\s.+"
    df <- .deepcollclean(df, pos, extract_pattern, l_rows)
  }

  # Clean two collectors separated by "y"
  pos <- which(grepl("\\sy\\s", df$recordedBy))
  if (length(pos) > 0) {
    extract_pattern = "\\sy\\s.+"
    df <- .deepcollclean(df, pos, extract_pattern, l_rows)
  }

  # Examples of collectors not separated by symbols (,  ;  &)
  # Extracting just examples like "Teraoka, W. Baker, R."
  tf <- grepl(".*,.*,", df$recordedBy)
  tfa <- !grepl(";", df$recordedBy[tf])
  tfb <- !grepl("&", df$recordedBy[tf][tfa])
  pos <- which(tf)[tfa][tfb]
  # Extracting just examples like "Teraoka, W. Baker, R."
  if (length(pos) > 0) {
    extract_pattern = "(\\S*\\s+\\S+)$"
    df <- .deepcollclean(df, pos, extract_pattern, l_rows)
  }

  #_____________________________________________________________________________
  # Extracting the collector names when there are only two collectors
  # Collectors separated by ; but without any comma , separating initials and surnames
  tf <- grepl(".*;", df$recordedBy)
  tfa <- !grepl(".*,", df$recordedBy[tf])
  pos <- which(tf)[tfa]
  if (length(pos) > 0) {
    extract_pattern = ";.+"
    df <- .deepcollclean(df, pos, extract_pattern, l_rows)
  }

  # Clean remaining examples of two collectors separated by semicolon
  pos <- which(grepl(".*;", df$recordedBy))
  if (length(pos) > 0) {
    extract_pattern = ";.+"
    df <- .deepcollclean(df, pos, extract_pattern, l_rows)

    # Before next grepl we need to delete the remaining semicolon ";"
    df$recordedBy <- gsub("[;]", "", df$recordedBy)
  }

  # Clean two collectors separated by "&"
  # Lets first edit examples like this "J. Campbell-Snelling, M. Chambers"
  tf <- grepl("[[:lower:]]+[-][[:upper:]][[:lower:]]+,\\s[[:upper:]][.]\\s[[:upper:]]",
              df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <-  gsub(",", " &", df$recordedBy[tf])
  }

  # Clean two collectors separated by "&"
  pos <- which(grepl("&", df$recordedBy))
  if (length(pos) > 0) {
    extract_pattern = "&.+"
    df <- .deepcollclean(df, pos, extract_pattern, l_rows)
  }

  #_____________________________________________________________________________
  # NEED TO WORK MORE HERE; I might get some problems as mentioned below
  # Finding two collectors like
  # "C. H. Dodson, P. M. Dodson" or "P.P. Wan, K.S. Chow"
  # "Robert F. Thorne, Geoff Tracey"
  # I need to work on this so as make it more general and grab the following names
  # "Robert Thorne, Geoff Tracey"
  # "C.Farney, G.Byer"

  pos <- which(grepl("([[:upper:]][.]){1,}\\s*[[:upper:]][[:lower:]]+[,]\\s",
                     df$recordedBy))
  #"P.P. Wan, K.S. Chow" %in% df$recordedBy[tf]
  #"C. H. Dodson, P. M. Dodson" %in% df$recordedBy[tf]
  #"C.Farney" %in% df$recordedBy[tf]
  #"Robert F. Thorne, Geoff Tracey" %in% df$recordedBy[tf]
  if (length(pos) > 0) {
    extract_pattern = ",.+"
    df <- .deepcollclean(df, pos, extract_pattern, l_rows)
  }

  #_____________________________________________________________________________
  # Before next grepl we need to delete particles like de, da, do, dos
  tf <- grepl(" de", df$recordedBy)
  tfa <- grepl(paste(c("van den", "van der"), collapse = "|"), df$recordedBy)
  df$recordedBy[which(tf - tfa == T)] <-
    gsub(" de", " ", df$recordedBy[which(tf - tfa == T)])
  tf <- grepl("[.]de", df$recordedBy)
  df$recordedBy[tf] <- gsub("de", " ", df$recordedBy[tf])
  tf <- grepl(" De ", df$recordedBy)
  df$recordedBy[tf] <- gsub(" De ", " ", df$recordedBy[tf])
  tf <- grepl(" DE ", df$recordedBy)
  df$recordedBy[tf] <- gsub(" DE ", " ", df$recordedBy[tf])
  tf <- grepl("^De ", df$recordedBy)
  df$recordedBy[tf] <- gsub("^De ", "", df$recordedBy[tf])
  tf <- grepl("^de ", df$recordedBy)
  df$recordedBy[tf] <- gsub("^de ", "", df$recordedBy[tf])
  tf <- grepl(" De$", df$recordedBy)
  df$recordedBy[tf] <- gsub(" De$", "", df$recordedBy[tf])
  tf <- grepl("[.]\\sDE$", df$recordedBy)
  df$recordedBy[tf] <- gsub(" DE$", "", df$recordedBy[tf])

  tf <- grepl("[[:space:]]da$", df$recordedBy)
  df$recordedBy[tf] <- gsub(" da", "", df$recordedBy[tf])
  tf <- grepl(" da ", df$recordedBy)
  df$recordedBy[tf] <- gsub(" da ", " ", df$recordedBy[tf])
  tf <- grepl(" da;", df$recordedBy)
  df$recordedBy[tf] <- gsub(" da;", " ", df$recordedBy[tf])
  tf <- grepl("[.]da", df$recordedBy)
  df$recordedBy[tf] <- gsub("da", " ", df$recordedBy[tf])
  tf <- grepl("[.]\\sDA\\s", df$recordedBy)
  df$recordedBy[tf] <- gsub("DA\\s", "", df$recordedBy[tf])
  # I might need to exclude the next step for names like "WALMOR DA FONSECA"
  tf <- grepl("[[:upper:]]+\\sDA\\s[[:upper:]]+", df$recordedBy)
  df$recordedBy[tf] <- gsub("DA\\s", "", df$recordedBy[tf])

  tf <- grepl(" dos", df$recordedBy)
  df$recordedBy[tf] <- gsub(" dos", " ", df$recordedBy[tf])
  tf <- grepl(" do", df$recordedBy)
  df$recordedBy[tf] <- gsub(" do", " ", df$recordedBy[tf])
  tf <- grepl("[.]dos", df$recordedBy)
  df$recordedBy[tf] <- gsub("dos", " ", df$recordedBy[tf])
  tf <- grepl("[.]do", df$recordedBy)
  df$recordedBy[tf] <- gsub("do", " ", df$recordedBy[tf])

  df$recordedBy <- gsub("[[:space:]]{2}", " ", df$recordedBy)

  #_____________________________________________________________________________
  # Cleaning some unusual formats of collector names at specific collections ####
  if (any(df$collectionCode %in% "CEN")|
      any(df$institutionCode %in% "UTEP")) {
    message(".precollherb $recordedBy in specific herbaria")
    df <- .precollherb(df)
  }

  #_____________________________________________________________________________
  # Cleaning when there is only ONE COLLECTOR at $recordedBy

  #_____________________________________________________________________________
  # General cleaning of collector initials
  # Clean e.g. "Arbo M .M."
  tf <- grepl("\\s[.][[:upper:]][.]", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("\\s[.]", ".", df$recordedBy[tf])
    df$recordedBy[tf] <- gsub("[.][.]", ".", df$recordedBy[tf])
  }

  # Clean e.g. "Meira, Neto J A"
  tf <- grepl("^[[:upper:]][[:lower:]]+[-](Filho|Sobrinho|Neto)\\s([[:upper:]]{1,}|[[:upper:]]\\s)", df$recordedBy)
  if (any(tf)) {
    # Replace first space with a comma into the string
    df$recordedBy[tf] <- sub("\\s+", ", ", df$recordedBy[tf])
  }

  # Clean e.g. "Roberto Paulo Orlandi.", "Adonias Araujo."
  tf <- grepl("([[:upper:]][[:lower:]]+\\s){1,}[[:upper:]]([[:lower:]]){2,}[.]", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("[.]$", "", df$recordedBy[tf])
  }

  # Clean e.g. "Elton. M. C. Leme", "Franco. I.M."
  tf <- grepl("[[:upper:]][[:lower:]]+[.]\\s[[:upper:]][.]", df$recordedBy)
  if (any(tf)) {
    # Replace only first match of a word
    df$recordedBy[tf] <-
      stringi::stri_replace_first_regex(df$recordedBy[tf], "[.]{1}", "")
    #stringi::stri_replace_first_fixed("21st-August-2017", "st", "xx")

  }

  tf <- grepl("^[[:upper:]]+{2,}[.]\\s[[:upper:]]", df$recordedBy)
  #df$recordedBy[tf]
  if (any(tf)) {
    # replace only first match of a word
    df$recordedBy[tf] <-
      stringi::stri_replace_first_regex(df$recordedBy[tf], "[.]{1}", "")
  }

  #_____________________________________________________________________________
  # Extracting collector initials that are separated by COMMA
  tf <- grepl(",", df$recordedBy)
  # Grabbing and inserting back first names with at least one abbreviation
  # so, initials like..." C.F.P. von ", " J.E.L.S.", " R.", " Terence D."
  tfa <- grepl("([[:upper:]][.]){1}", df$recordedBy[tf])
  if (any(tfa)) {
    message("std_recordedBy $recordedBy initials #1")
    temp_df <- data.frame(initials=stringr::str_extract_all(df$recordedBy, ",.+",
                                                            simplify = TRUE))
    if (length(temp_df) == 0) {
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)
    temp_df$initials[tf][tfa] <-
      gsub(",", "", temp_df$initials[tf][tfa])
    temp_df$initials[tf][tfa] <-
      gsub("^\\s", "", temp_df$initials[tf][tfa])
    # Then delete all initials from the main column recordedBy
    df$recordedBy[tf][tfa] <-
      gsub(",.+", "", df$recordedBy[tf][tfa])
    df$recordedBy[tf][tfa] <-
      paste(as.character(temp_df$initials[tf][tfa]),
            as.character(df$recordedBy[tf][tfa]), sep=" ")
  }

  #_____________________________________________________________________________
  # Extracting collector initials that are separated by COMMA
  tf <- grepl(",", df$recordedBy)
  # Grabbing and inserting back first names with at least two abbreviation and no dot
  # so, initials like..."Padgurschi, MCG", "Oliveira, AA"
  tfa <- grepl("([[:upper:]]){2}", df$recordedBy[tf])

  if (any(tfa)) {
    message("std_recordedBy $recordedBy initials #2")
    df$recordedBy[tf][tfa] <-
      gsub("^\\s", "", df$recordedBy[tf][tfa])
    temp_df <- data.frame(initials=stringr::str_extract_all(df$recordedBy, ",.+",
                                                            simplify = TRUE))
    if (length(temp_df) == 0) {
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)

    temp_df$initials[tf][tfa] <-
      gsub(",", "", temp_df$initials[tf][tfa])
    temp_df$initials[tf][tfa] <-
      gsub(" ", "", temp_df$initials[tf][tfa])

    # Then delete all initials from the main column recordedBy
    df$recordedBy[tf][tfa] <-
      gsub(",.+", "", df$recordedBy[tf][tfa])

    df$recordedBy[tf][tfa] <-
      paste(as.character(temp_df$initials[tf][tfa]),
            as.character(df$recordedBy[tf][tfa]), sep=" ")
  }

  #_____________________________________________________________________________
  # Extracting collector initials that are separated by COMMA ####

  tf <- grepl(",", df$recordedBy)
  # Grabbing and inserting back first names with at least two abbreviation and no dot
  # so, initials like..."Cardoso, D", "Oliveira, A"
  tfa <- !grepl("[[:upper:]][[:lower:]]+[,]\\s*[[:upper:]][[:lower:]]+",
                df$recordedBy[tf])
  if (any(tfa)) {
    message("std_recordedBy $recordedBy initials #3")
    temp_df <- data.frame(initials=stringr::str_extract_all(df$recordedBy, ",.+", simplify = TRUE))
    if (length(temp_df) == 0) {
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)
    temp_df$initials[tf][tfa] <-
      gsub(",", "", temp_df$initials[tf][tfa])
    temp_df$initials[tf][tfa] <-
      gsub(" ", "", temp_df$initials[tf][tfa])
    df$recordedBy[tf][tfa] <-
      gsub(",.+", "", df$recordedBy[tf][tfa])
    df$recordedBy[tf][tfa] <-
      paste(as.character(temp_df$initials[tf][tfa]),
            as.character(df$recordedBy[tf][tfa]), sep=" ")
  }

  tf <- grepl(",", df$recordedBy)
  #This step will be enough to grab and insert back the non abbreaviated names
  # like "Estrada, Armando", "Sellow, Friedrich", "Pierre, Jean Baptiste Louis"
  # Find positions of names inside the main database
  if (any(tf)) {
    message("std_recordedBy $recordedBy initials #4")
    df$recordedBy[tf] <- gsub(",$", "", df$recordedBy[tf])
    df$recordedBy[tf] <- gsub("[.]$", "", df$recordedBy[tf])

    temp_df <- data.frame(initials=stringr::str_extract_all(df$recordedBy, ",.+",
                                                            simplify = TRUE))
    if (length(temp_df) == 0) {
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)
    temp_df$initials[tf] <- gsub(",", "", temp_df$initials[tf])
    temp_df$initials[tf] <- gsub("^\\s", "", temp_df$initials[tf])
    temp_df$initials[tf] <- gsub("\\s$", "", temp_df$initials[tf])
    # Then delete all initials from the main column recordedBy
    df$recordedBy[tf] <- gsub(",.+", "", df$recordedBy[tf])
    df$recordedBy[tf] <- paste(as.character(temp_df$initials[tf]),
                               as.character(df$recordedBy[tf]), sep=" ")
  }

  #_____________________________________________________________________________
  # BEFORE next grepl...
  # deleting blank space at the begining and end of the cell
  df$recordedBy <- gsub("^[[:space:]]", "", df$recordedBy)
  df$recordedBy <- gsub("[[:space:]]{2}", " ", df$recordedBy)
  df$recordedBy <- gsub("[[:space:]]$", "", df$recordedBy)
  #pos.non.punct <- grepl("[!:punct:]", herb.database$collector)
  #herb.database$collector[pos.non.punct] <- gsub(" ", ".", herb.database$collector[pos.non.punct])

  #_____________________________________________________________________________
  # Now cleaning collector names with initials NOT separated by comma ####
  # or semicolon like "Callejas R.", "Schultes R.E.", "Krukoff BA" "Sergio M Faria"

  # Marking names like "Cardoso D", "Maas PJM", "Maas P.J.M.", "Cardoso D."
  # "Zwaan CJ van der", "Martius CFP von"
  # these names are not separated by comma or full period in the initials

  # We do in a series of steps otherwise we will erase others based on the patterns

  # Mark the word with just surnames
  tf <- !grepl("[[:space:]]|[.]", df$recordedBy)
  if (any(tf)) {
    # Make them in just the first letter capitalized
    df$recordedBy[tf] <- gsub("\\b([a-z])", "\\U\\1",
                              tolower(df$recordedBy[tf]), perl = TRUE)
  }

  #_____________________________________________________________________________
  # Mark the first word in capital letters
  tf <- grepl("[[:upper:]]+{2,}\\s[[:upper:]][.]", df$recordedBy)
  if (any(tf)) {
    # Make them in just the first letter capitalized
    df$recordedBy[tf] <- gsub("\\b([a-z])", "\\U\\1",
                              tolower(df$recordedBy[tf]), perl = TRUE)
  }

  #_____________________________________________________________________________
  # "Zwaan CJ van der", "Martius CFP von"
  # these names are not separated by comma or full period in the initials
  tf <- grepl("[[:lower:]]+\\s+([[:upper:]]{1,})+\\s", df$recordedBy)
  if (any(tf)) {
    # Marking names with "van den", etc
    # like "Zwaan CJ van der", "Martius CFP von"
    tfa <- grepl(paste(c("van den", "van der", " von", " van", " bin"), collapse = "|"),
                 df$recordedBy[tf])
    if (any(tfa)) {
      message("std_recordedBy $recordedBy initials #5")
      tfb <- df$recordedBy[tf][tfa]
      tfc <- grepl(paste(tfb, collapse = "|"), df$recordedBy)
      # Extracting collector initials
      temp_df <- data.frame(initials=stringr::str_extract_all(df$recordedBy, " .+",
                                                              simplify = TRUE))
      if (length(temp_df) == 0) {
        temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
      }
      temp_df$initials <- as.character(temp_df$initials)
      temp_df$initials[!tfc] <- NA
      df$recordedBy[tfc] <- gsub(" .+", "", df$recordedBy[tfc])
      df$recordedBy <- ifelse(!is.na(temp_df$initials == TRUE),
                              paste(as.character(temp_df$initials),
                                    as.character(df$recordedBy), sep=" "),
                              as.character(df$recordedBy))
      df$recordedBy[tfc] <- gsub("^[[:space:]]", "", df$recordedBy[tfc])
    }
  }

  #_____________________________________________________________________________
  # Abbreviate first name
  # Mark the names like "Sergio M Faria", "Domingos S Cardoso", "Marcelo T Nascimento"
  # these names are not separated by comma or full period in the initials
  tf <- grepl("[[:lower:]]+\\s+([[:upper:]]{1,})+\\s", df$recordedBy)
  if (any(tf)) {
    message("std_recordedBy $recordedBy initials #6")
    # Extracting collector initials
    temp_df <- data.frame(initials=stringr::str_extract_all(df$recordedBy, "^(\\S*\\s+)",
                                                            simplify = TRUE))
    if (length(temp_df) == 0) {
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }

    temp_df$initials <- as.character(temp_df$initials)

    temp_df$initials[!tf] <- NA
    temp_df$initials[tf] <- gsub("^ ", "", temp_df$initials[tf])
    temp_df$initials[tf] <- abbreviate(temp_df$initials[tf],
                                       minlength = 1, strict = T, dot = F, use.classes = F)
    # Deleting first name, which is before first space
    df$recordedBy[tf] <- gsub("^(\\S*\\s+)", "", df$recordedBy[tf])
    df$recordedBy <- ifelse(!is.na(temp_df$initials == TRUE),
                            paste(as.character(temp_df$initials),
                                  as.character(df$recordedBy), sep=""),
                            as.character(df$recordedBy))
  }

  #_____________________________________________________________________________
  # Mark the names like "Domingos S. Cardoso", "Domingos S.M. Cardoso", "Domingos S.M.G. Cardoso"
  # Let's first find names in capital letter like "JORGE  C.A. LIMA"
  # and then abbreviate the first name
  tf <- grepl("[[:upper:]]+\\s+([[:upper:]]+[.]{1,})+\\s+[[:upper:]]{3}", df$recordedBy)
  # Make them in just the first letter capitalized
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("\\b([a-z])", "\\U\\1",
                              tolower(df$recordedBy[tf]),
                              perl = TRUE)
  }

  #_____________________________________________________________________________
  # finding examples like "Roberto P.Orlandi", "Jorge C.A.Lima"
  # and open spaces between abreaviated initials
  tf <- grepl("[[:lower:]]+\\s+([[:upper:]]+[.]){1,}[[:upper:]][[:lower:]]+",
              df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("[.]", ". ", df$recordedBy[tf])
  }

  #_____________________________________________________________________________
  # "David J.N. Hind", "Jorge C. A. Lima", "Grady L. Webster", "Charles M. Ek"
  tf <- grepl("[[:upper:]][[:lower:]]+\\s+(.*[[:upper:]][.]){1,}\\s+[[:alpha:]]{2,}",
              df$recordedBy)
  if (any(tf)) {
    message("std_recordedBy $recordedBy initials #7")
    # Cleaning possible presence of space in the begining of the name
    df$recordedBy[tf] <- gsub("^\\s", "", df$recordedBy[tf])
    # Cleaning  possible presence of comma at the end of the name
    df$recordedBy[tf] <- gsub(",$", "", df$recordedBy[tf])

    # Extracting collector initials
    temp_df <- data.frame(initials=stringr::str_extract_all(df$recordedBy, "^(\\S*\\s+)",
                                                            simplify = TRUE))
    if (length(temp_df) == 0) {
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }

    temp_df$initials <- as.character(temp_df$initials)
    temp_df$initials[!tf] <- NA
    temp_df$initials[tf] <- gsub("^ ", "", temp_df$initials[tf])
    temp_df$initials[tf] <- abbreviate(temp_df$initials[tf],
                                       minlength = 1, strict = T, dot = T, use.classes = F)
    # Deleting first name, which is before first space
    df$recordedBy[tf] <- gsub("^(\\S*\\s+)", "", df$recordedBy[tf])
    df$recordedBy <- ifelse(!is.na(temp_df$initials == TRUE),
                            paste(as.character(temp_df$initials),
                                  as.character(df$recordedBy), sep=" "),
                            as.character(df$recordedBy))
  }

  #_____________________________________________________________________________
  # Finding collector names like "Schultes R.E.", "Soejarto D.", "Maas P.J.M."
  tf <- grepl("^\\S[[:lower:]]+\\s([[:upper:]]+[.]){1,}", df$recordedBy)
  tfa <- !grepl("\\s+[[:upper:]][[:lower:]]+", df$recordedBy[tf])
  if (any(tfa)) {
    message("std_recordedBy $recordedBy initials #8")
    temp_df <- data.frame(initials=stringr::str_extract_all(df$recordedBy[tf][tfa], " .+",
                                                            simplify = TRUE))
    if (length(temp_df) == 0) {
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)

    df$recordedBy[tf][tfa] <- gsub(" .+", "", df$recordedBy[tf][tfa])

    df$recordedBy[tf][tfa] <- paste(as.character(temp_df$initials),
                                    as.character(df$recordedBy[tf][tfa]), sep=" ")
  }

  #_____________________________________________________________________________
  # Finding collector names like "Croat TB", "Kostermans AJGH"
  # No comma separating and more than two initials without full period
  tf <- grepl("^\\S[[:lower:]]+\\s([[:upper:]]{2,})", df$recordedBy)
  if (any(tf)) {
    message("std_recordedBy $recordedBy initials #9")
    temp_df <- data.frame(initials=stringr::str_extract_all(df$recordedBy, " .+",
                                                            simplify = TRUE))
    if (length(temp_df) == 0) {
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)
    df$recordedBy[tf] <- gsub(" .+", "", df$recordedBy[tf])
    df$recordedBy[tf] <- paste(as.character(temp_df$initials[tf]),
                               as.character(df$recordedBy[tf]), sep=" ")
  }

  #_____________________________________________________________________________
  # Getting rid of the last abbreviated initial in Spanish-like names
  # "Percy Núñez V.", "P. Nuñez V.", "Mario Sousa S.", "G. Ibarra M."

  tf <- grepl("[[:lower:]]+\\s+([[:upper:]]+[.]{1})", df$recordedBy)
  tfa <- grepl("(\\s+[[:upper:]]+[[:lower:]]+\\s+([[:upper:]]+[.]$))",
               df$recordedBy[tf])
  if (any(tfa)) {
    # Remove everything after last space
    df$recordedBy[tf][tfa] <-
      gsub("\\s[^ ]+$", "", df$recordedBy[tf][tfa])
  }

  #_____________________________________________________________________________
  # Getting rid of the last abbreviated initial in Spanish-like names
  # "N. Castaño-A." "W. Trujillo-C."
  tf <- grepl("[[:upper:]][[:lower:]]+[-]+([[:upper:]]+[.]{1})", df$recordedBy)
  if (any(tf)) {
    # Remove everything after the hyphen at the end
    df$recordedBy[tf] <- gsub("-[^-]+$", "", df$recordedBy[tf])
  }

  #_____________________________________________________________________________
  # Finding collector names like "Uribe Uribe AL", ""Cid Ferreira CA"
  # Then separate the last names by an hiphen
  tf <- grepl("[[:lower:]]+[[:space:]]([[:upper:]]{2,})", df$recordedBy)
  if (any(tf)) {
    message("std_recordedBy $recordedBy initials #10")
    # extract all AFTER second space
    #str_extract_all("Uribe Uribe AL", " [^ ]+$", simplify = TRUE)
    # Extracting initials
    temp_df <- data.frame(initials=stringr::str_extract_all(df$recordedBy, " [^ ]+$",
                                                            simplify = TRUE))
    if (length(temp_df) == 0) {
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }

    temp_df$initials <- as.character(temp_df$initials)

    # Removing all after last space
    df$recordedBy[tf] <- gsub("\\s[^ ]+$", "", df$recordedBy[tf])
    df$recordedBy[tf] <- gsub(" ", "-", df$recordedBy[tf])
    df$recordedBy[tf] <- paste(as.character(temp_df$initials[tf]),
                               as.character(df$recordedBy[tf]), sep=" ")
  }

  #_____________________________________________________________________________
  # Finding collector names like "Cardenas D", "Ferreira L"
  # Only one surname and one initial
  tf <- grepl("[[:upper:]][[:lower:]]+\\s[[:upper:]]", df$recordedBy)

  tfa <- !grepl("[.]", df$recordedBy[tf])

  tfb <- !grepl("[[:upper:]][[:lower:]]+\\s[[:upper:]][[:lower:]]+",
                df$recordedBy[tf][tfa])
  if (any(tfb)) {
    message("std_recordedBy $recordedBy initials #11")
    # Extracting initials
    temp_df <- data.frame(initials=stringr::str_extract_all(df$recordedBy, " .+",
                                                            simplify = TRUE))
    if (length(temp_df) == 0) {
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }

    temp_df$initials <- as.character(temp_df$initials)
    # adding a full period at the end of the initials
    temp_df$initials[tf][tfa][tfb] <-
      gsub("$", ".", temp_df$initials[tf][tfa][tfb])
    temp_df$initials[tf][tfa][tfb] <-
      gsub("^\\s", "", temp_df$initials[tf][tfa][tfb])

    df$recordedBy[tf][tfa][tfb] <-
      gsub(" .+", "", df$recordedBy[tf][tfa][tfb])

    df$recordedBy[tf][tfa][tfb] <-
      paste(as.character(temp_df$initials[tf][tfa][tfb]),
            as.character(df$recordedBy[tf][tfa][tfb]), sep=" ")
  }

  #_____________________________________________________________________________
  # Adding full period in names like D Cardoso, DD Cardoso, DDD Cardoso
  # This step could have been done by searching first with
  # grepl("^[[:upper:]]{2}", df$recordedBy)
  df$recordedBy <- gsub("^ ", "", df$recordedBy)

  tf <- grepl("[[[:upper:]]{2}", df$recordedBy)
  tfa <- !grepl("[.]", df$recordedBy[tf])
  tfb <- !grepl("[[:upper:]]{5,}", df$recordedBy[tf][tfa])

  if (any(tfb)) {
    message("std_recordedBy $recordedBy initials #12")
    # Extracting initials
    temp_df <- data.frame(initials=stringr::str_extract_all(df$recordedBy, "^(\\S*\\s+)",
                                                            simplify = TRUE))
    if (length(temp_df) == 0) {
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)

    temp_df$initials[tf][tfa][tfb] <-
      gsub(" $", "", temp_df$initials[tf][tfa][tfb])
    # adding a space between all initials
    temp_df$initials[tf][tfa][tfb] <-
      lapply(temp_df$initials[tf][tfa][tfb],
             function(x) trimws(gsub("([[:alpha:]])", " \\1", x)))
    # adding a space at the end initials
    temp_df$initials[tf][tfa][tfb] <-
      gsub("$", " ", temp_df$initials[tf][tfa][tfb])
    # replace space by full periods
    temp_df$initials[tf][tfa][tfb] <-
      gsub(" ", ".", temp_df$initials[tf][tfa][tfb])
    # adding space between each initials now with full period
    # put the space after the "\\1 "
    temp_df$initials[tf][tfa][tfb] <-
      lapply(temp_df$initials[tf][tfa][tfb],
             function(x) trimws(gsub("([[:punct:]])", "\\1 ", x)))
    temp_df <- data.frame(initials=unlist(temp_df$initials))
    if (length(temp_df) == 0) {
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)
    # Remove first words before first space
    df$recordedBy[tf][tfa][tfb] <-
      gsub("^(\\S*)", "", df$recordedBy[tf][tfa][tfb])
    df$recordedBy[tf][tfa][tfb] <-
      gsub("^.", "", df$recordedBy[tf][tfa][tfb])

    df$recordedBy[tf][tfa][tfb] <-
      paste(as.character(temp_df$initials[tf][tfa][tfb]),
            as.character(df$recordedBy[tf][tfa][tfb]), sep=" ")
  }

  #_____________________________________________________________________________
  # Abbreviating and adding points to names like
  # "Dionisio Constantino"
  tf <- !grepl("[.]", df$recordedBy)
  tfa <- !grepl("[[:upper:]][[:lower:]]+\\s[[:upper:]][[:lower:]]+\\s",
                df$recordedBy[tf])
  tfb <- grepl("\\s", df$recordedBy[tf][tfa])
  tfc <- !grepl("-", df$recordedBy[tf][tfa][tfb])

  # From these we grab the first word and then abbreviate
  if (any(tfc)) {
    message("std_recordedBy $recordedBy initials #13")
    # Extracting the first word before first space
    temp_df <- data.frame(initials=stringr::str_extract_all(df$recordedBy, "^(\\S*\\s+)",
                                                            simplify = TRUE))
    if (length(temp_df) == 0) {
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)

    # Remove space at the end
    temp_df$initials[tf][tfa][tfb][tfc] <-
      gsub(" $", "", temp_df$initials[tf][tfa][tfb][tfc])
    temp_df$initials[tf][tfa][tfb][tfc] <-
      abbreviate(temp_df$initials[tf][tfa][tfb][tfc], minlength = 1, strict = T, dot = T, use.classes = F)

    # Remove first words before first space
    df$recordedBy[tf][tfa][tfb][tfc] <-
      gsub("^(\\S*)", "", df$recordedBy[tf][tfa][tfb][tfc])
    df$recordedBy[tf][tfa][tfb][tfc] <-
      gsub("^.", "", df$recordedBy[tf][tfa][tfb][tfc])

    # Combining initials with surname
    df$recordedBy[tf][tfa][tfb][tfc] <-
      paste(as.character(temp_df$initials[tf][tfa][tfb][tfc]),
            as.character(df$recordedBy[tf][tfa][tfb][tfc]), sep=" ")
  }

  #_____________________________________________________________________________
  # "T S SANTOS", "A Ducke", or errors like "A .Ducke" "G .T. Prance" "L W. Williams"

  tf <- grepl("^[[:upper:]]\\s", df$recordedBy)
  if (any(tf)) {
    # Correct errors like "A .Ducke" "G .T. Prance"
    df$recordedBy[tf] <-
      gsub(" [.]", ". ", df$recordedBy[tf])
  }

  # Correcting errors like "L W. Williams"
  tf <- grepl("^[[:upper:]]\\s", df$recordedBy)
  tfa <- grepl("[.]", df$recordedBy[tf])
  if (any(tfa)) {
    df$recordedBy[tf][tfa] <-
      gsub("[.]", "", df$recordedBy[tf][tfa])
    df$recordedBy[tf][tfa] <-
      gsub(" ", ". ", df$recordedBy[tf][tfa])
  }

  # Now searching just # "T S SANTOS", "A Ducke",
  tf <- grepl("^[[:upper:]]\\s", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <-
      gsub(" ", ". ", df$recordedBy[tf])
    df$recordedBy[tf] <-
      gsub("bin.", "bin", df$recordedBy[tf])
    df$recordedBy[tf] <-
      gsub("van.", "van", df$recordedBy[tf])

    tfa <- grepl("[[:upper:]][[:lower:]]+[.]\\s[[:upper:]][[:lower:]]+",
                 df$recordedBy[tf])
    if (any(tfa)) {

      df$recordedBy[tf][tfa] <-
        gsub("[.] ", " ", df$recordedBy[tf][tfa])
      # Add point after the first word
      df$recordedBy[tf][tfa] <-
        gsub("^(\\w)", "\\1.", df$recordedBy[tf][tfa])
    }

  }

  #_____________________________________________________________________________
  # Finding collector names like "Monod Froideville C"
  # How to find names with just one upper letter at the end "[[:upper:]]$"
  # And delete last letter
  tf <- grepl("[[:upper:]]$", df$recordedBy)
  tfa <- !grepl("[.]", df$recordedBy[tf])
  if (any(tfa)) {
    # Remove last word after the last space
    #df$recordedBy[tf][tfa] <-
    #gsub("\\s\\w$", "", df$recordedBy[tf][tfa])
    # "(-|\\s)[A-Z]+$" will remove last words after the last space OR an hiphen
    df$recordedBy[tf][tfa] <-
      gsub("(-|\\s)[A-Z]+$", "", df$recordedBy[tf][tfa])
  }

  #_____________________________________________________________________________
  # Finding collectors with all uppercase letters
  alluppercase <- grepl("[[:upper:]]{3,}", df$recordedBy)
  #df$recordedBy[which(alluppercase == T)]

  # Make them in just the first letter capitalized
  df$recordedBy[alluppercase] <- gsub("\\b([a-z])", "\\U\\1",
                                      tolower(df$recordedBy[alluppercase]),
                                      perl = TRUE)

  #_____________________________________________________________________________
  # Finding collectors with more than four names and abbreviate first two names
  # "Alexánder Francisco Rodríguez González"
  tf <- !grepl("[.]", df$recordedBy)
  tfa <- grepl(".*\\s.*\\s.*\\s", df$recordedBy[tf])
  if (any(tfa)) {
    message("std_recordedBy $recordedBy initials #14")
    # Extracting the first two names to abbreviate
    temp_df <- data.frame(initials=stringr::str_extract_all(df$recordedBy,
                                                            "^(\\S*\\s\\S*\\s+)",
                                                            simplify = TRUE))
    if (length(temp_df) == 0) {
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)
    temp_df$initials[tf][tfa] <-
      gsub(" $", "", temp_df$initials[tf][tfa])
    # First abbreviate without dots then put space between the initials
    temp_df$initials[tf][tfa] <-
      abbreviate(temp_df$initials[tf][tfa],
                 minlength = 1, strict = F, dot = F, use.classes = F)
    # adding a space between all initials
    temp_df$initials[tf][tfa] <-
      lapply(temp_df$initials[tf][tfa],
             function(x) trimws(gsub("([[:alpha:]])", " \\1", x)))
    # adding a space at the end initials
    temp_df$initials[tf][tfa] <-
      gsub("$", " ", temp_df$initials[tf][tfa])
    # replace space by full periods
    temp_df$initials[tf][tfa] <-
      gsub(" ", ".", temp_df$initials[tf][tfa])
    # adding space between each inicials now with full period
    # put the space after the "\\1 "
    temp_df$initials[tf][tfa] <-
      lapply(temp_df$initials[tf][tfa],
             function(x) trimws(gsub("([[:punct:]])", "\\1 ", x)))
    temp_df <- data.frame(initials=unlist(temp_df$initials))
    if (length(temp_df) == 0) {
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)
    # Deleting first two names, i.e. those before second space
    df$recordedBy[tf][tfa] <-
      gsub("^(\\S*\\s\\S*\\s+)", "", df$recordedBy[tf][tfa])

    # Combining initials with surname
    df$recordedBy[tf][tfa] <-
      paste(as.character(temp_df$initials[tf][tfa]),
            as.character(df$recordedBy[tf][tfa]), sep=" ")
  }

  #_____________________________________________________________________________
  # Finding collectors with initials not separated by comma
  # "H.C. Lima", "D.B.O.S. Cardoso" and "F.C.How"
  # Removing double spaces
  df$recordedBy <- gsub("[[:space:]]{2}", " ", df$recordedBy)

  tf <- grepl("([[:upper:]][.]){2,}", df$recordedBy)
  # Until here the vector ex_fullnames contains names like
  # "J.A. Lombardi", "Jorge C.A. Lima"
  # But we need to exclude the "Jorge C.A. Lima"
  # I HAVE ALREADY EXCLUDED "Jorge C.A. Lima" BEFORE, SO SOME STEPS HERE RE REDUNDANT
  # The grepl tfa is redundant
  tfa <- grepl("[[:upper:]][[:lower:]]+\\s[[:upper:]][.]", df$recordedBy[tf])
  #"Jorge C.A. Lima" %in% df$recordedBy[tf][tfa]
  tfb <- !grepl("[[:upper:]][[:lower:]]+\\s[[:upper:]][.]", df$recordedBy[tf])
  #"Jorge C.A. Lima" %in% df$recordedBy[tf][tfb]
  #"Jorge C.A. Lima" %in% df$recordedBy[tf][which(tfb - tfa == T)]
  #"J.A. Lombardi" %in% df$recordedBy[tf][which(tfb - tfa == T)]
  #"F.C.How" %in% df$recordedBy[tf][which(tfb - tfa == T)]

  if (any(tfb)) {
    message("std_recordedBy $recordedBy initials #15")
    # Extract all before first space
    #str_extract_all(c("P.E.E. Sintenis", "H.C. Lima", "D.B.O.S. Cardoso"), "^([\\S*]+)", simplify = TRUE)
    # Remove all after last dot
    #sub("[^.]+$", "", c("P.E. E. Sintenis", "H.C.Lima", "D.B. O.S. Cardoso"))
    # Extracting the first two names after the last [.]
    # We can do this withoud the function str_extract_all
    temp_df <- data.frame(initials=df$recordedBy)
    if (length(temp_df) == 0) {
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)

    temp_df$initials[tf][which(tfb - tfa == T)] <-
      sub("[^.]+$", "", temp_df$initials[tf][which(tfb - tfa == T)])
    # After the previous step we may have initials like "J. L.G."
    # So, lets remove spaces and then onpe just one spaces using lapply function
    temp_df$initials[tf][which(tfb - tfa == T)] <-
      sub(" ", "", temp_df$initials[tf][which(tfb - tfa == T)])
    # Adding space between each inicials now with full period
    # Put the space after the "\\1 "
    temp_df$initials[tf][which(tfb - tfa == T)] <-
      lapply(temp_df$initials[tf][which(tfb - tfa == T)],
             function(x) trimws(gsub("([[:punct:]])", "\\1 ", x)))
    temp_df <- data.frame(initials=unlist(temp_df$initials))
    if (length(temp_df) == 0) {
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)
    # Removing all initials/names BEFORE last dot
    # Then remove first space
    df$recordedBy[tf][which(tfb - tfa == T)] <-
      gsub(".*\\.", "", df$recordedBy[tf][which(tfb - tfa == T)])
    df$recordedBy[tf][which(tfb - tfa == T)] <-
      gsub("^ ", "", df$recordedBy[tf][which(tfb - tfa == T)])
    # Combining initials with surname
    df$recordedBy[tf][which(tfb - tfa == T)] <-
      paste(as.character(temp_df$initials[tf][which(tfb - tfa == T)]),
            as.character(df$recordedBy[tf][which(tfb - tfa == T)]), sep=" ")
  }

  #_____________________________________________________________________________
  # Finding collectors like "N. Marquete F. Silva"
  # and abbreviate the second name
  tf <- grepl("([[:upper:]][.])\\s[[:upper:]][[:lower:]]+\\s([[:upper:]][.])", df$recordedBy)
  if (any(tf)) {
    message("std_recordedBy $recordedBy initials #16")
    # extract all before before first space
    #str_extract_all(c("P.E.E. Sintenis", "H.C. Lima", "D.B.O.S. Cardoso"), "^([\\S*]+)", simplify = TRUE)
    # remove all after last dot
    #sub("[^.]+$", "", c("P.E. E. Sintenis", "H.C.Lima", "D.B. O.S. Cardoso"))
    # Extracting the first two names after the second space
    # We can do this withoud the function str_extract_all
    temp_df <- data.frame(initials=df$recordedBy)
    if (length(temp_df) == 0) {
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)
    temp_df$initials[!tf] <- NA
    temp_df$initials[tf] <-
      sub("(\\S*\\s+\\S+)$", "", temp_df$initials[tf])
    temp_df$initials[tf] <-
      sub(" $", "", temp_df$initials[tf])
    temp_df$initials[tf] <-
      abbreviate(temp_df$initials[tf],
                 minlength = 1, strict = F, dot = F, use.classes = F)
    # adding a space between all initials
    temp_df$initials[tf] <-
      lapply(temp_df$initials[tf],
             function(x) trimws(gsub("([[:alpha:]])", " \\1", x)))
    # adding a space at the end initials
    temp_df$initials[tf] <-
      gsub("$", " ", temp_df$initials[tf])
    # replace space by full periods
    temp_df$initials[tf] <-
      gsub(" ", ".", temp_df$initials[tf])
    # adding space between each inicials now with full period
    # put the space after the "\\1 "
    temp_df$initials[tf] <-
      lapply(temp_df$initials[tf],
             function(x) trimws(gsub("([[:punct:]])", "\\1 ", x)))
    temp_df <- data.frame(initials=unlist(temp_df$initials))
    if (length(temp_df) == 0) {
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)
    # Removing all initials/names BEFORE second space
    # Then remove first space
    df$recordedBy[tf] <- gsub("^(\\S*\\s+\\S+)", "", df$recordedBy[tf])
    df$recordedBy[tf] <- gsub("^ ", "", df$recordedBy[tf])

    # Combining initials with surname
    df$recordedBy[tf] <- paste(as.character(temp_df$initials[tf]),
                               as.character(df$recordedBy[tf]), sep=" ")
  }

  #_____________________________________________________________________________
  # Finding collectors like "J. A.S. Santos", M.Oliveira
  # separate abbreviations like "J. A. S. Santos", M. Oliveira

  tf <- grepl("[[:upper:]][.][[:upper:]][[:lower:]]+", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("[.]", ". ", df$recordedBy[tf])
  }

  #_____________________________________________________________________________
  # Finding collectors like "G.D Colletta", "E. M.B Prata", "C.E Zartman"

  tf <- grepl("[[:upper:]][.][[:upper:]]\\s", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("\\s", ". ", df$recordedBy[tf])
    df$recordedBy[tf] <- gsub("[.]", ". ", df$recordedBy[tf])
    df$recordedBy[tf] <- gsub(" [.] ", "", df$recordedBy[tf])
    df$recordedBy[tf] <- gsub("\\s\\s", " ", df$recordedBy[tf])
  }

  #_____________________________________________________________________________
  # Finding ALL remaining collectors without full period
  tf <- !grepl("[.]", df$recordedBy)
  tfa <- grepl("\\s", df$recordedBy[tf])
  if (any(tfa)) {
    message("std_recordedBy $recordedBy initials #17")
    # Codes to find first words
    temp_df <- data.frame(fullnames=df$recordedBy)
    if (length(temp_df) == 0) {
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$fullnames <- as.character(temp_df$fullnames)
    # Perhaps we need to take out the accents before using the function abbreviate
    # See this link where I found the function stri_trans_general
    # https://stackoverflow.com/questions/39148759/remove-accents-from-a-dataframe-column-in-r
    # And here an interesting way on the use of the symbol := to parse data from a data table structure
    # https://stackoverflow.com/questions/45651394/what-does-symbol-mean-in-r
    temp_df$fullnames <-
      stri_trans_general(temp_df$fullnames, "Latin-ASCII")

    # Grab the first word into a new column
    temp_df$coll.abbrev <- gsub("([A-Za-z]+).*", "\\1",
                                temp_df$fullnames)

    # I had some troube when using abbreviate function after doing a gsub like the one bellow
    # To get first names that include accent, like "Sérgio Miana Faria)
    #temp_df$coll.abbrev <- gsub("([A-Za-zéáí]+).*", "\\1", temp_df$fullnames)

    temp_df$coll.abbrev[tf][tfa] <-
      abbreviate(temp_df$coll.abbrev[tf][tfa],
                 minlength = 1, strict = T, dot = T, use.classes = F)
    # Removing all initials/names BEFORE second space
    # gsub("^(\\S*\\s+)", "", df$recordedBy[tf][tfa]) # Remove words before first space
    # gsub("^(\\w+)", "", df$recordedBy[tf][tfa]) # Remove first word
    # Then remove first space
    df$recordedBy[tf][tfa] <-
      gsub("^(\\w+)", "", df$recordedBy[tf][tfa])
    df$recordedBy[tf][tfa] <-
      gsub("^ ", "", df$recordedBy[tf][tfa])
    # Combining initials with surname
    df$recordedBy[tf][tfa] <-
      paste(as.character(temp_df$coll.abbrev[tf][tfa]),
            as.character(df$recordedBy[tf][tfa]), sep=" ")
  }

  #_____________________________________________________________________________
  # Finding collectors like "C. -Ming Tan"
  tf <- grepl("[[:upper:]][.]\\s[-][[:upper:]][[:lower:]]+", df$recordedBy)
  if (any(tf)) {
    message("std_recordedBy $recordedBy initials #18")
    # Codes to find first words
    temp_df <- data.frame(fullnames=df$recordedBy)
    if (length(temp_df) == 0) {
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$fullnames <- as.character(temp_df$fullnames)
    # Deleting last name after second space
    temp_df$coll.abbrev <- gsub("(\\w+)$", "", temp_df$fullnames)
    # to abbreviate words like this "C. -Ming" into C.-M.
    # use args minlength = 4, strict = T, dot = T
    temp_df$coll.abbrev[tf] <-
      abbreviate(temp_df$coll.abbrev[tf],
                 minlength = 4, strict = T, dot = T, use.classes = F)
    temp_df$coll.abbrev[!tf] <- NA
    # Removing all initials/names BEFORE second space
    # gsub("^(\\S*\\s\\S*\\s+)", "", df$recordedBy[abbrev.pos]) # Remove words before first space
    # gsub("^(\\w+)", "", df$recordedBy[abbrev.pos]) # Remove first word
    # Then remove first space
    df$recordedBy[tf] <- gsub("^(\\S*\\s\\S*\\s+)", "", df$recordedBy[tf])
    # Combining initials with surname
    df$recordedBy[tf] <-
      paste(as.character(temp_df$coll.abbrev[tf]),
            as.character(df$recordedBy[tf]), sep=" ")
  }

  #_____________________________________________________________________________
  # Putting back original collectors when written as "Expeditions", etc

  tf <- grepl("\\sExpedition", df$recordedByOriginal)
  if (any(tf)) {
    df$recordedBy[tf] <- df$recordedByOriginal[tf]
    df$recordNumber[tf] <- df$recordNumberOriginal[tf]
  }

  tf <- grepl("^Flora\\sof\\s", df$recordedByOriginal)
  if (any(tf)) {
    df$recordedBy[tf] <- df$recordedByOriginal[tf]
    df$recordNumber[tf] <- df$recordNumberOriginal[tf]
  }

  tf <- grepl("\\sProject", df$recordedByOriginal)
  if (any(tf)) {
    df$recordedBy[tf] <- df$recordedByOriginal[tf]
    df$recordNumber[tf] <- df$recordNumberOriginal[tf]
  }

  #_____________________________________________________________________________
  # Finding collectors that has " de la " like " de la Cruz"

  dela <- grepl(" de la ", df$recordedByOriginal)
  #df$recordedBy[dela]
  #df$recordedByOriginal[dela]

  # before grepl with the original, lets clean the second authors
  dela.original <- df$recordedByOriginal[dela]
  dela.original.cleaned <- gsub("(;.+)|([|].+)", "", dela.original)

  dela.1 <- grepl(" de la ", dela.original.cleaned)
  dela.names <- df$recordedByOriginal[dela][dela.1]

  pos.dela.names <- grepl(paste(paste("^", dela.names, "$", sep = ""),
                                collapse = "|"), df$recordedByOriginal)

  if (any(pos.dela.names)) {
    message("std_recordedBy $recordedBy particles before surname #1")
    df$recordedBy[pos.dela.names] <- gsub(" la ", " de la ", df$recordedBy[pos.dela.names])
  }

  #_____________________________________________________________________________
  # Abbreviating second names if there exists particles like
  # "de" do" "dos" in the original collector column

  de <- grepl("[.]\\s[[:upper:]][[:lower:]]+\\s[[:upper:]][[:lower:]]+", df$recordedBy)

  # Before grepl with the original, lets clean the second authors
  de.original <- df$recordedByOriginal[de]
  de.original.cleaned <- gsub("(;.+)|([|].+)", "", de.original)

  de.1 <- grepl(" de ", de.original.cleaned)
  de.2 <- grepl(" DE ", de.original.cleaned)
  de.3 <- grepl(" De ", de.original.cleaned)
  do.1 <- grepl(" do ", de.original.cleaned)
  do.2 <- grepl(" DO ", de.original.cleaned)
  do.3 <- grepl(" Do ", de.original.cleaned)
  dos.1 <- grepl(" dos ", de.original.cleaned)
  dos.2 <- grepl(" DOS ", de.original.cleaned)
  da.1 <- grepl(" da ", de.original.cleaned)
  da.2 <- grepl(" DA ", de.original.cleaned)

  if (any(de.1) |
      any(de.2) |
      any(de.3) |
      any(do.1) |
      any(do.2) |
      any(do.3) |
      any(dos.1) |
      any(dos.2) |
      any(da.1) |
      any(da.2)) {
    message("std_recordedBy $recordedBy particles before surname #2")
    de.names <- df$recordedByOriginal[de][which(de.1 + de.2 + de.3 +
                                                  do.1 + do.2 + do.3 +
                                                  dos.1 + dos.2 +
                                                  da.1 + da.2 == T)]
    de.names <- gsub("[(]", "[(]", de.names)
    de.names <- gsub("[)]", "[)]", de.names)
    pos.de.1.names <- grepl(paste(paste("^", de.names, "$", sep = ""),
                                  collapse = "|"), df$recordedByOriginal)

    #df$recordedBy[pos.de.1.names]

    # Codes to find first words
    temp_df <- data.frame(df$recordedBy)
    colnames(temp_df) <- "fullnames"
    temp_df$fullnames <- as.character(temp_df$fullnames)

    # extracting each collum to abbreviate just the second
    temp_df$coll.abbrev1 <- gsub("[^.]+$", "", temp_df$fullnames)
    temp_df$coll.abbrevtemp <- gsub(".*\\.", "", temp_df$fullnames)
    temp_df$coll.abbrev2 <- gsub("(\\S*\\S+)$", "", temp_df$coll.abbrevtemp)
    temp_df$coll.abbrevlast <- gsub("^(\\S*\\s+\\S+)", "", temp_df$coll.abbrevtemp)
    temp_df$coll.abbrev2[pos.de.1.names] <-
      abbreviate(temp_df$coll.abbrev2[pos.de.1.names],
                 minlength = 1, strict = T, dot = T, use.classes = F)

    temp_df$coll.abbrev1[!pos.de.1.names] <- NA
    temp_df$coll.abbrev2[!pos.de.1.names] <- NA
    temp_df$coll.abbrevlast[!pos.de.1.names] <- NA

    df$recordedBy <- ifelse(!is.na(temp_df$coll.abbrev2 == TRUE),
                            paste(as.character(temp_df$coll.abbrev1),
                                  as.character(temp_df$coll.abbrev2),
                                  as.character(temp_df$coll.abbrevlast),
                                  sep=" "),
                            as.character(df$recordedBy))
  }

  # Finding possible examples like these "B. T. P. M Góes", "M. P Dias"
  tf <- grepl("\\s[[:upper:]]\\s[[:upper:]][[:lower:]]+", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("\\s", ". ", df$recordedBy[tf])
    df$recordedBy[tf] <- gsub("[.][.]", ".", df$recordedBy[tf])
  }

  # Removing dots and comma at the end of surnames like:
  # "V. O. Amorim."	"J. H. C. Ribeiro," from RB collections or "A...S... Flores"
  df$recordedBy <- gsub("[.]$", "", as.character(df$recordedBy))
  df$recordedBy <- gsub(",$", "", df$recordedBy)
  df$recordedBy <- gsub("\\s[.]\\s", " ", df$recordedBy)
  df$recordedBy <- gsub("[.][.][.]", ". ", df$recordedBy)

  # Removing final double spaces
  df$recordedBy <- gsub("[[:space:]]{2}", " ", df$recordedBy)

  # Removing de, da etc when they are not separated from the surnames
  tf <- grepl("\\sde[[:upper:]][[:lower:]]+", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub(" de", " ", df$recordedBy[tf])
  }

  tf <- grepl("\\sda[[:upper:]][[:lower:]]+", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub(" da", " ", df$recordedBy[tf])
  }

  tf <- grepl("\\sdas[[:upper:]][[:lower:]]+", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub(" das", " ", df$recordedBy[tf])
  }

  tf <- grepl("\\sdo[[:upper:]][[:lower:]]+", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub(" do", " ", df$recordedBy[tf])
  }

  tf <- grepl("\\sdos[[:upper:]][[:lower:]]+", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub(" dos", " ", df$recordedBy[tf])
  }

  #_____________________________________________________________________________
  # Correcting tilde accents; this might be a problem with tolower function when
  # using with words that have tilde accents.
  # "G. O. RomãO"
  # get the codes for tilde or any characters here
  # https://en.wikipedia.org/wiki/ISO/IEC_8859-1
  tf <- grepl("\\xe3O|\\xe3OS|\\xe3E|\\xe3ES|\\xf5ES", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("\\xe3O", "\\xe3o", df$recordedBy[tf])
    df$recordedBy[tf] <- gsub("\\xe3OS", "\\xe3os", df$recordedBy[tf])
    df$recordedBy[tf] <- gsub("\\xe3E", "\\xe3e", df$recordedBy[tf])
    df$recordedBy[tf] <- gsub("\\xe3ES", "\\xe3es", df$recordedBy[tf])
    df$recordedBy[tf] <- gsub("\\xf5ES", "\\xf5es", df$recordedBy[tf])
  }

  #_____________________________________________________________________________
  # Further cleaning Asian-like names when they are in unicode characters ####
  # from "UTF-8" encoding
  #tf <- grepl("[+][0-9]", "<U+8983><U+704F><U+5BCC>")
  tf <- grepl("[+][0-9]", df$recordedBy)
  if (any(tf)) {
    df <- .unicodeclean(df, tf)
  }

  #_____________________________________________________________________________
  # Adding Unknown collector to empty cells
  df$recordedBy <- gsub("^$", "Unknown", trimws(df$recordedBy))

  #_____________________________________________________________________________
  # Further cleaning
  tf <- grepl("[?]$", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("[?]$", "", df$recordedBy[tf])
  }

  df$recordedBy <- gsub("\\s$", "", df$recordedBy)
  df$recordedBy <- gsub("^[.]\\s", "", df$recordedBy)

  tf <- grepl("^[\177][[:upper:]][.]", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("^[\177]", "", df$recordedBy[tf])
  }

  tf <- grepl("[.]\\s,\\s[[:upper:]]", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("\\s,", "", df$recordedBy[tf])
  }


  #_____________________________________________________________________________
  # Cleaning specific collector names ####
  df <- .std_specific_coll(df)


  #_____________________________________________________________________________
  # Clean the newly created column of additional collectors
  df <- .addcollclean(df)


  #_____________________________________________________________________________
  # Furthern cleaning numbers at $recordNumber
  df <- .std_recordNumber(df = df,
                          colnames_df = colnames_df,
                          colname_recordNumber = colname_recordNumber)


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
# Extract secondary collectors and keep only the principal ####
.deepcollclean <- function(df,
                           pos,
                           extract_pattern,
                           l_rows) {

  temp_df <- data.frame(collector=stringr::str_extract_all(df$recordedBy,
                                                           extract_pattern,
                                                           simplify = TRUE))

  if (length(temp_df) == 0) {
    temp_df <- data.frame(collector=rep(NA, l_rows))
  }
  temp_df$collector <- as.character(temp_df$collector)

  temp_df$collector[-pos] <- NA

  tf <- grepl("[$]$", extract_pattern)
  if (tf) {
    extract_pattern <- gsub("[$]|[.][+]", "", extract_pattern)
    df$recordedBy[pos] <- gsub(paste0(extract_pattern, ".*"), "\\1", df$recordedBy[pos])
    df$addCollector[pos] <- temp_df$collector[pos]
  } else {
    df$recordedBy[pos] <- gsub(extract_pattern, "", df$recordedBy[pos])
    extract_pattern <- gsub("[$]|[.][+]", "", extract_pattern)
    df$addCollector[pos] <- gsub(extract_pattern, "", temp_df$collector[pos])
  }

  df$recordedBy[pos] <- gsub("^\\s|\\s$", "", df$recordedBy[pos])
  df$addCollector[pos] <- gsub("^\\s|\\s$", "", df$addCollector[pos])

  return(df)
}


#_______________________________________________________________________________
# Cleaning $recordedBy with more than two collectors ####
# The function automatically adds "et al." at $addCollector

.deletal <- function(df) {

  # Adding et al. in the collumn "addCollector" when the collumn "recordedBy" has for more than two collectors
  tf <- grepl("& et al[.]", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern=" &.+", n_message="#1")
    df$recordedBy[tf] <- gsub("[,].+", "", df$recordedBy[tf])
  }

  # Finding examples with multiples "--"; we have to grepl from the original because
  # these hyphens were deleted previously in the main column
  tf <- grepl("(.*[-]{2}.*[-]{2}){1,}", df$recordedByOriginal)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern="[,].+", n_message="#2")
  }

  # Lima H.S., Neto J.P.; Marimon B.S.
  tf <- grepl("[[:lower:]]+\\s([[:upper:]][.]){1,}[,]\\s[[:upper:]][[:lower:]]+\\s([[:upper:]][.]){1,}[;]\\s[[:upper:]][[:lower:]]+\\s[[:upper:]]", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern=",.+", n_message="#3")
  }

  tf <- grepl("; et al[.]; et al[.]", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern=";.+", n_message="#4")
  }

  tf <- grepl("[|]et al[.]|[|] et al[.]", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern="[|].+", n_message="#5")
  }

  tf <- grepl("(.*[|].*[|]){1,}", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern="\\|.+", n_message="#6")
  }

  tf <- grepl("[|] Otros| Partícipes| Participantes", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern="[|] Otros.+| Partícipes.+| Participantes.+", n_message="#7")
  }

  tf <- grepl("(.*\\sy\\s.*\\sy\\s){1,}", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern="\\sy.+", n_message="#8")
  }

  tf <- grepl("(.*&.*\\sy\\s){1,}", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern="&.+|\\s&.+", n_message="#9")
  }

  tf <- grepl("(.*[|].*\\sy\\s){1,}", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern="[|].+", n_message="#10")
  }

  tf <- grepl("(.*[;].*\\sy\\s){1,}", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern="[;].+", n_message="#11")
  }

  tf <- grepl("(.*[,].*\\sy\\s){1,}", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern="[,].+|[;].+", n_message="#12")
  }

  tf <- grepl("(.*\\sy\\s.*[,].*[,]){1,}", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern="\\sy.+", n_message="#13")
  }

  tf <- grepl("; Etc", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern=";.+", n_message="#14")
  }

  tf <- grepl("[:]", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern=":.+", n_message="#15")
  }

  # Adding et al. in the collumn "addCollector" when the collumn "recordedBy"
  # has for more than two collectors
  tf <- grepl("(.*;.*;){1,}", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern="[;].+", n_message="#16")
  }

  tf <- grepl(".*;.*&", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern=";.+", n_message="#17")
  }

  tf <- grepl(".*&.*,.*,", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern="\\s&.+", n_message="#18")
  }

  tf <- grepl(".*;.*[[:space:]]-[[:space:]]", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern=";.+", n_message="#19")
  }

  tf <- grepl(".*;.*[[:alpha:]]+[[:space:]]+[e]+[[:space:]]+[[:alpha:]]", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern=";.+", n_message="#20")
  }

  tf <- grepl("et al[.][;]\\s[[:upper:]]", df$recordedBy)
  # "et al.; Redden, K.M."
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern="et al.; ", n_message="#21")
  }

  tf <- grepl(" ET AL|et[.]al", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern=";.+", n_message="#22")
  }

  tf <- grepl(" et[.] al", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern="et[.] al.+", n_message="#23")
    df$recordedBy[tf] <- gsub(";.+", "", df$recordedBy[tf])
  }

  tf <- grepl("([^;]+;+[[:space:]]+[[:upper:]]+[.][^,]+),", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern=";.+", n_message="#24")
  }

  tf <- grepl(".*,.*,.*et Al", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern="^(\\S*\\s+\\S+).*", x="\\1", n_message="#25")
    df$recordedBy[tf] <- gsub(",$", "", df$recordedBy[tf])
  }

  tf <- grepl("\\set\\sAl[.]", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern="\\set\\sAl[.]", n_message="#26")
  }

  #_____________________________________________________________________________
  tf <- grepl(".*,.*,.*&", df$recordedBy)
  if (any(tf)) {
    message(".deletal $recordedBy and $addCollector #27")

    df$addCollector <- ifelse(tf, "et al.", as.character(df$addCollector))

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
    df$recordedBy[tf][which(tfa + tfb == T)] <-
      gsub("^([^,]+,[^,]+).*", "\\1",
           df$recordedBy[tf][which(tfa + tfb == T)])
    tfc <- grepl("[[:upper:]][.]\\s[[:upper:]][[:lower:]]+[,]\\s",
                 df$recordedBy[tf][tfb])
    df$recordedBy[tf][tfb][tfc] <-
      gsub("^([^,]+).*", "\\1", df$recordedBy[tf][tfb][tfc])

    # Then do last search again
    tf <- grepl(".*,.*,.*&", df$recordedBy)
    df$recordedBy[tf] <- gsub("[.],.+", ".", df$recordedBy[tf])
    tf <- grepl(".*,.*,.*&", df$recordedBy)
    df$recordedBy[tf] <- gsub(",.+", "", df$recordedBy[tf])
  }
  #_____________________________________________________________________________

  tf <- grepl(".*&.*&", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern="&.+", n_message="#28")
  }

  tf <- grepl(" Et al", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern=";.+", n_message="#29")
  }

  tf <- grepl("& et al", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern="&.+", n_message="#30")
  }

  tf <- grepl("; et al[.]|; et al", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern=";.+", n_message="#31")
  }

  tf <- grepl(" et al", df$recordedBy)
  # "J.A. Lombardi, H. Lorenzi, R. Tsuji et al."
  tfa <- grepl(".*,.*,", df$recordedBy[tf])
  if (any(tfa)) {
    message(".deletal $recordedBy and $addCollector #32")

    # This next commented code can be much more simple
    #df$addCollector[tf] <- ifelse(tfa, "et al.", as.character(df$addCollector[tf]))
    df$addCollector[tf][tfa] <- "et al."
    df$recordedBy[tf][tfa] <- gsub("[,].+", "", df$recordedBy[tf][tfa])
  }

  tf <- grepl(" et al[.]| et al", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern=" et al+", n_message="#33")
  }

  tf <- grepl(".*;.* et ", df$recordedBy)
  # "M.G.Bovini; A.Quinet et L.E.Barros"
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern=";.+", n_message="#34")
  }

  tf <- grepl(".*;.*;", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern="[;].+", n_message="#35")
  }

  tf <- grepl(".*;.*,.*,", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern="[;].+", n_message="#36")
  }

  tf <- grepl(".*,.*,.*,", df$recordedBy)
  if (any(tf)) {
    message(".deletal $recordedBy and $addCollector #37")

    tfa <- !grepl("[[:upper:]][.][,]", df$recordedBy[tf])
    df$addCollector[tf][tfa] <- "et al."
    df$recordedBy[tf][tfa] <- gsub("[,].+", "", df$recordedBy[tf][tfa])
    df$recordedBy[tf][tfa] <- gsub("[;].+", "", df$recordedBy[tf][tfa])

    tfb <- grepl("[[:upper:]][.][,]", df$recordedBy[tf])
    df$addCollector[tf][tfb] <- "et al."
    df$recordedBy[tf][tfb] <- gsub("(^[^,]+,[^,]+).*$", "\\1", df$recordedBy[tf][tfb])
  }

  tf <- grepl("& Al[.]|& col[.]|& al[.]|&\\sal$", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern="\\s&.+", n_message="#38")
  }

  tf <- grepl("e auxiliares| e outros", df$recordedBy)
  if (any(tf)) {
    df <- .deepdeletal(df, tf, pattern=" e auxiliares.+| e outros.+", n_message="#39")
  }

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
  tf <- grepl("(.*,.*,){1,}", df$recordedBy)
  tfa <- !grepl(";", df$recordedBy[tf])
  tfb <- grepl("(.*[[:space:]]){5,}", df$recordedBy[tf][tfa])
  tfc <- grepl("[[:upper:]][.]\\s[[:upper:]][[:lower:]]+,\\s", df$recordedBy[tf][tfa][tfb])
  #extracting e.g. "J.A. Lombardi, H. Lorenzi, R. Tsuji"

  #ex_tfb <- df$recordedBy[tf][tfa][tfb]
  # When grepl comes with zero names, search the names always between "^$"
  # otherwise the answer will be always TRUE
  #xxxxx <- c("", "")
  #lllll <- c("CC", "DD")
  #grepl("^xxxxx$", lllll)
  #paste("^", lllll, "$", sep = "")
  #tf3 <- grepl(paste(paste("^", ex_tfb, "$", sep = ""), collapse = "|"), df$recordedBy)
  if (any(tfc)) {
    message(".deletal $recordedBy and $addCollector #40")
    df$addCollector[tf][tfa][tfb][tfc] <- "et al."
    df$recordedBy[tf][tfa][tfb][tfc] <- gsub("[,].+", "", df$recordedBy[tf][tfa][tfb][tfc])
  }

  # Cleaning remaining examples of more than two collectors separated by just commas
  # and no symbols like &, semicolon or et al.
  tf <- grepl("(.*,.*,){1,}", df$recordedBy)
  tfa <- !grepl(";", df$recordedBy[tf])
  tfb <- grepl("(.*[[:space:]]){5,}", df$recordedBy[tf][tfa])
  tfc <- grepl("[[:upper:]][.]\\s[[:upper:]][[:lower:]]+\\s&", df$recordedBy[tf][tfa][tfb])
  #extracting just examples like "Radford A.E., J. Bozeman & Ramseur, George S."

  if (any(tfc)) {
    message(".deletal $recordedBy and $addCollector #41")
    df$addCollector[tf][tfa][tfb][tfc] <- "et al."
    df$recordedBy[tf][tfa][tfb][tfc] <- gsub("[,].+", "", df$recordedBy[tf][tfa][tfb][tfc])
    df$recordedBy[tf][tfa][tfb][tfc] <- gsub("\\s", ", ", df$recordedBy[tf][tfa][tfb][tfc])
  }


  return(df)
}

# Extract secondary collectors and keep only the principal
.deepdeletal <- function(df, tf, pattern = NULL, x = "", n_message) {
  message(paste(".deletal $recordedBy and $addCollector", n_message))
  df$addCollector <- ifelse(tf, "et al.", as.character(df$addCollector))
  df$recordedBy[tf] <- gsub(pattern, x, df$recordedBy[tf])

  return(df)
}


#_______________________________________________________________________________
# Pre-cleaning $recordedBy before standardizing collector names ####
.precollclean <- function(df){

  tf <- grepl("(^|[.])[[:upper:]][[:lower:]]+,\\s(Filho|Sobrinho|Neto)", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub(", Filho", "-Filho", df$recordedBy[tf])
    df$recordedBy[tf] <- gsub(", Sobrinho", "-Sobrinho", df$recordedBy[tf])
    df$recordedBy[tf] <- gsub(", Neto", "-Neto", df$recordedBy[tf])
  }

  df$recordedBy <- gsub("\"", "", df$recordedBy)
  df$recordedBy <- gsub("[[][?][]]", "", df$recordedBy)
  df$recordedBy <- gsub("\\s[(]J[.][?][)]", "", df$recordedBy)

  df$recordedBy <- gsub("[[]", "", df$recordedBy)
  df$recordedBy <- gsub("[]]", "", df$recordedBy)
  df$recordedBy <- gsub("[(]Lady[)]", "", df$recordedBy)
  df$recordedBy <- gsub("[(]Miss[)]", "", df$recordedBy)
  df$recordedBy <- gsub("[(]Capt[.][)]", "", df$recordedBy)
  df$recordedBy <- gsub("[(]Prof[.][)]", "", df$recordedBy)
  df$recordedBy <- gsub("[(]Rev[.][)]", "", df$recordedBy)
  df$recordedBy <- gsub("[(]Mrs[)][.]", "", df$recordedBy)
  df$recordedBy <- gsub("[(]Mrs[)]", "", df$recordedBy)
  df$recordedBy <- gsub("\\s[(]Mr\\s[&]\\sMrs[)]", "", df$recordedBy)
  df$recordedBy <- gsub("\\s[(]Countess\\sof.+", "", df$recordedBy)
  df$recordedBy <- gsub("[(]Major[)]", "", df$recordedBy)
  df$recordedBy <- gsub("[(]photo[)]", "", df$recordedBy)
  df$recordedBy <- gsub("[(]Photo[)]", "", df$recordedBy)
  df$recordedBy <- gsub("[(]Pere[)]", "", df$recordedBy)
  df$recordedBy <- gsub("\\s[(]Karl[)]", "", df$recordedBy)
  df$recordedBy <- gsub("[(]Frère[)]", "", df$recordedBy)
  df$recordedBy <- gsub("[(]Dr[.][)]", "", df$recordedBy)
  df$recordedBy <- gsub("\\s[(]Dr[)]", "", df$recordedBy)
  df$recordedBy <- gsub(",\\sDr\\s", ", ", df$recordedBy)
  df$recordedBy <- gsub("\\s[(]Dr[.][/]Sir[)]", "", df$recordedBy)
  df$recordedBy <- gsub("\\s[(]Col[.][)]$", "", df$recordedBy)
  df$recordedBy <- gsub("^[(]", "", df$recordedBy)
  df$recordedBy <- gsub("[)]$", "", df$recordedBy)
  df$recordedBy <- gsub("[?]$|^[?]", "", df$recordedBy)
  df$recordedBy <- gsub("- Botanist", "", df$recordedBy)
  df$recordedBy <- gsub("\\sAfrica$", "", df$recordedBy)
  df$recordedBy <- gsub(",\\sunknown", "", df$recordedBy)
  df$recordedBy <- gsub("unknown collector", "", df$recordedBy)
  df$recordedBy <- gsub("[\'][s]\\sCollector", "", df$recordedBy)
  df$recordedBy <- gsub("MrandMrs", "", df$recordedBy)
  df$recordedBy <- gsub("MrMrs", "", df$recordedBy)
  df$recordedBy <- gsub("CETA[:][|]", "", df$recordedBy)
  df$recordedBy <- gsub("\\s[(]SEMO[)]", "", df$recordedBy)
  df$recordedBy <- gsub("Collector[(]s[)][:]\\sunknown,", "", df$recordedBy)
  df$recordedBy <- gsub("[(]Coll.+", "", df$recordedBy)
  df$recordedBy <- gsub("\\s[-]\\sPau\\sBrasil$", "", df$recordedBy)
  df$recordedBy <- gsub("Collector[(]s[)][:]\\s", "", df$recordedBy)
  df$recordedBy <- gsub(",\\s[*]$|[*]$| [{]2[º] SERIE[}]|^[?];et al[.],", "", df$recordedBy)
  df$recordedBy <- gsub("^[-][.]\\s|[-][-]|^[<]", "", df$recordedBy)

  temp <- c("^$", "^[@]$", "[@];,S[.]N[.]", "Collector illegible", "Native Collector",
            "Collector unspecified", "NO DISPONIBLE", "#NOME[?]",
            "Illegible collector name", "s[.]coll[.]", "no data",
            "Collector unknown", "[(]unknown[)]", "[(]n[/]a[)]",
            "sem coletor", "s[.]coll[.]", "s[.]col[.]", "s[.]col", "collector",
            "[[]data\\snot\\scaptured[]]", "s.c.", "Anonymous", "coletor$",
            "Anon.", "[?]", "Unclear", "unclear", "C. F. C. R", "Cfcr",
            "^_V$", "Sem coletor$", "^([0-9]){1,}$|^([0-9]){1,}.*([0-9]){1,}$")

  tf <- is.na(df$recordedBy) | grepl(paste0(temp, collapse = "|"), df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- "Unknown"
  }

  df$recordedBy <- gsub("[?][.] ", "", df$recordedBy)
  df$recordedBy <- gsub(" - ", "-", df$recordedBy)
  df$recordedBy <- gsub("- ", "-", df$recordedBy)
  df$recordedBy <- gsub("[*] ", "", df$recordedBy)
  df$recordedBy <- gsub("-[.]", "", df$recordedBy)
  df$recordedBy <- gsub(",[.]", ".", df$recordedBy)
  df$recordedBy <- gsub(", III", "", df$recordedBy)
  df$recordedBy <- gsub(", --", "", df$recordedBy)
  df$recordedBy <- gsub("--", " ", df$recordedBy)
  df$recordedBy <- gsub(", not a person", "", df$recordedBy)
  df$recordedBy <- gsub(" Mrs Captain", "", df$recordedBy)
  df$recordedBy <- gsub(", 1", "", df$recordedBy)
  df$recordedBy <- gsub(", Jr[.]", "", df$recordedBy)
  df$recordedBy <- gsub(" Jr[.]", "", df$recordedBy)
  df$recordedBy <- gsub(" Jr ", " ", df$recordedBy)
  df$recordedBy <- gsub(" Jr, ", ", ", df$recordedBy)
  df$recordedBy <- gsub("-Júnior", "", df$recordedBy)
  df$recordedBy <- gsub(" Junior", "", df$recordedBy)
  df$recordedBy <- gsub(" d'", "", df$recordedBy)
  df$recordedBy <- gsub(" Neto", "-Neto", df$recordedBy)
  df$recordedBy <- gsub(" NETO", "-NETO", df$recordedBy)
  df$recordedBy <- gsub(" Filho", "-Filho", df$recordedBy)
  df$recordedBy <- gsub(" FILHO", "-FILHO", df$recordedBy)
  df$recordedBy <- gsub("Leitão Fo[.],", "Leitão-Filho,", df$recordedBy)
  df$recordedBy <- gsub(" Sobrinho", "-Sobrinho", df$recordedBy)
  df$recordedBy <- gsub(" [(]IAN[)]", "", df$recordedBy)
  df$recordedBy <- gsub(" and ", " & ", df$recordedBy)
  df$recordedBy <- gsub("ex herb[.] ", "", df$recordedBy)
  df$recordedBy <- gsub("^Prof[.] ", "", df$recordedBy)
  df$recordedBy <- gsub("Mrs[.] ", "", df$recordedBy)
  df$recordedBy <- gsub("Dr[.] ", "", df$recordedBy)
  df$recordedBy <- gsub("Dr [?]", "", df$recordedBy)
  df$recordedBy <- gsub("[(]|[)]", "", df$recordedBy)

  # Finding examples like "Fred Melgert / Carla Hoegen"
  tf <- grepl("\\s\\/\\s", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("\\s\\/\\s", " & ", df$recordedBy[tf])
  }

  tf <- grepl("\\sCollectors[:]\\s", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub(".*?[:]\\s", "", df$recordedBy[tf])
  }

  tf <- grepl("\\swith\\s", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("\\swith\\s", " & ", df$recordedBy[tf])
  }

  tf <- grepl("[[:lower:]]+,\\sSir\\s[[:upper:]]", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("Sir\\s", "", df$recordedBy[tf])
  }

  tf <- grepl("\\sDr[.]$", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("[.]\\sDr[.]", "", df$recordedBy[tf])
    df$recordedBy[tf] <- gsub(",\\sDr[.]", "", df$recordedBy[tf])
  }

  tf <- grepl("^Dr\\s[[:upper:]]", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("Dr\\s", "", df$recordedBy[tf])
  }

  tf <- grepl("; Herb[.] Amaz[.]", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub(";.+", "", df$recordedBy[tf])
  }

  tf <- grepl(" HERB[.] AMAZ[.]", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub(" HERB[.] AMAZ[.]", "", df$recordedBy[tf])
  }

  #Clean example like this "Lewis, Mr John "
  tf <- grepl("[[:upper:]][[:lower:]]+[,]\\sMr\\s[[:upper:]][[:lower:]]+\\s", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub(", Mr ", ", ", df$recordedBy[tf])
  }

  tf <- grepl("\\s[[:upper:]][[:lower:]]+\\sF[.]L[.]S[.]", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("\\sF[.]L[.]S[.]", "", df$recordedBy[tf])
  }

  # Clean examples like this "DrHapeman, H.", "MrsYoung, H.S.",
  # "BroArsene, G.; BroBenedict, A.", "Steve Stephens II"
  tf <- grepl("^Dr[[:upper:]][[:lower:]]+[,]\\s[[:upper:]]", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("Dr", "", df$recordedBy[tf])
  }

  tf <- grepl("^Mrs[[:upper:]][[:lower:]]+[,]\\s[[:upper:]]", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("Mrs", "", df$recordedBy[tf])
  }

  tf <- grepl("^Mrs\\s[[:upper:]]", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("Mrs\\s", "", df$recordedBy[tf])
  }

  tf <- grepl("^Mr[[:upper:]][[:lower:]]+[,]\\s[[:upper:]]", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("Mr", "", df$recordedBy[tf])
  }

  tf <- grepl("^Mr\\s[[:upper:]][[:lower:]]+", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("Mr\\s", "", df$recordedBy[tf])
  }

  tf <- grepl("Mr[.]$", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub(" Mr[.]", "", df$recordedBy[tf])
  }

  tf <- grepl("\\s[()]Brother[)]", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("\\s[()]Brother[)]", "", df$recordedBy[tf])
  }

  tf <- grepl("^Bro[[:upper:]][[:lower:]]+[,]\\s[[:upper:]]", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("Bro", "", df$recordedBy[tf])
  }

  tf <- grepl("\\s[[:upper:]][[:lower:]]+\\s(I){2,}", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("\\sIII", "", df$recordedBy[tf])
    df$recordedBy[tf] <- gsub("\\sII", "", df$recordedBy[tf])
  }

  tf <- grepl("\\s[[:upper:]][[:lower:]]+\\s(IV)", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("\\sIV", "", df$recordedBy[tf])
  }

  tf <- grepl("^-[.]\\s[[:upper:]][[:lower:]]+,\\s", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("-[.]\\s", "", df$recordedBy[tf])
  }

  tf <- grepl("^[(]\\s[[:upper:]][[:lower:]]+,\\s", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("-[.]\\s", "", df$recordedBy[tf])
  }

  tf <- grepl("[[:upper:]][[:lower:]]+,\\s[-]$", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub(",\\s[-]", "", df$recordedBy[tf])
  }

  tf <- grepl("[[:upper:]][.]\\set\\s[[:upper:]][.]", df$recordedBy)
  if (any(tf)) {
    df$recordedBy[tf] <- gsub("et", "and", df$recordedBy[tf])
  }

  return(df)
}


#_______________________________________________________________________________
# Pre-cleaning collector names at specific collections ####
.precollherb <- function(df){

  message(".precollherb $recordedBy in specific collections")

  #_____________________________________________________________________________
  # CEN collections
  tf <- grepl("[[:lower:]]+[-]([[:upper:]]){2,}", df$recordedBy[which(df$collectionCode %in% "CEN")])
  if (any(tf)){
    df$recordedBy[which(df$collectionCode %in% "CEN")][tf] <-
      gsub("-.+", "", df$recordedBy[which(df$collectionCode %in% "CEN")][tf])
  }

  #_____________________________________________________________________________
  # Further cleaning collector names within some specific herbarium collections

  # In the CEN collections all names are not abbreviated like:
  # "Taciana Barbosa Cavalcanti", "Carolyn Elinore Barnes Proença", "Marcelo Fragomeni Simon"
  # still need to correct left examples like: R. L. RM.  Machado Leite
  tf <- grepl("([[:upper:]][[:lower:]]+\\s){1,}",
              df$recordedBy[which(df$collectionCode %in% "CEN")])
  get_cen_colls <- df$recordedBy[which(df$collectionCode %in% "CEN")][tf]

  if (any(tf)){

    # Abbreviate first words
    fstname <- gsub("\\s.+", "", get_cen_colls)
    fstname <- abbreviate(fstname, minlength = 1, strict = F, dot = T, use.classes = F)
    fstname <- gsub("[[:lower:]]", "", fstname)

    get_cen_colls <- sub(".*?\\s", "", get_cen_colls)
    get_cen_colls <- paste(fstname, get_cen_colls)
    df$recordedBy[which(df$collectionCode %in% "CEN")][tf] <- get_cen_colls

    tfa <- grepl("\\S*\\s\\S*\\s+", get_cen_colls)
    get_cen_colls[tfa]

    # Codes to find first words
    initials_fullnames <- data.frame(get_cen_colls[tfa])
    colnames(initials_fullnames) <- "fullnames"
    initials_fullnames$fullnames <- as.character(initials_fullnames$fullnames)
    # Deleting last name after last space
    #initials_fullnames$coll.abrevtemp <- gsub("\\s[^ ]+$", "", initials_fullnames$fullnames
    initials_fullnames$coll.abrev1 <- gsub("[^.]+$", "", initials_fullnames$fullnames)
    initials_fullnames$coll.abrevtemp <- gsub(".*\\.", "", initials_fullnames$fullnames)
    initials_fullnames$coll.abrev2 <- gsub("(\\S*\\S+)$", "", initials_fullnames$coll.abrevtemp)
    initials_fullnames$coll.abrevlast <- gsub("^(\\S*\\s+\\S+)", "", initials_fullnames$coll.abrevtemp)
    initials_fullnames$coll.abrev2 <-
      abbreviate(initials_fullnames$coll.abrev2,
                 minlength = 1, strict = T, dot = T, use.classes = F)

    # Combining initials with surname
    get_cen_colls[tfa] <-
      paste(as.character(initials_fullnames$coll.abrev1),
            as.character(initials_fullnames$coll.abrev2),
            as.character(initials_fullnames$coll.abrevlast),
            sep=" ")
    df$recordedBy[which(df$collectionCode %in% "CEN")][tf][tfa] <-
      paste(as.character(initials_fullnames$coll.abrev1),
            as.character(initials_fullnames$coll.abrev2),
            as.character(initials_fullnames$coll.abrevlast),
            sep=" ")

    tfb <- grepl("[[:upper:]]{2}[.]\\s", get_cen_colls[tfa])
    get_cen_colls[tfa][tfb] <- sub("[[:upper:]][.](\\s){2}", " ",
                                   get_cen_colls[tfa][tfb])
    # Keeping all before second space
    initials <- sub("(\\S*\\s+\\S+)$", "", get_cen_colls[tfa][tfb])
    initials <- sub("\\s$", ".", initials)
    get_cen_colls[tfa][tfb] <-
      sub("\\S*\\s\\S*\\s", "", get_cen_colls[tfa][tfb])
    df$recordedBy[which(df$collectionCode %in% "CEN")][tf][tfa][tfb] <-
      paste(initials, get_cen_colls[tfa][tfb])
  }

  #_____________________________________________________________________________
  # UTEP collections/ but we grepl with institution code in this case

  # Extracting TWO COLLECTORS that are separated by COMMA
  # We need to place in this order, not before, othwerwise it will mess the cleaning
  # Two authors no abbreviation and just one comma
  # so, initials like... "Pedro Pable Moreno, Walter Robleto"
  tf <- grepl("(\\s[[:upper:]][[:lower:]]+{2,})[,](\\s[[:upper:]][[:lower:]]+{2,})",
              df$recordedBy[which(df$institutionCode %in% "UTEP")])
  if (any(tf)){
    temp_df <- data.frame(collector=stringr::str_extract_all(df$recordedBy, ",.+",
                                                             simplify = TRUE))
    if(length(temp_df) == 0){
      temp_df <- data.frame(collector=rep(NA, length(row.names(df))))
    }
    temp_df$collector <- as.character(temp_df$collector)
    df$addCollector[which(df$institutionCode %in% "UTEP")][tf] <-
      temp_df$collector[which(df$institutionCode %in% "UTEP")][tf]
    df$addCollector[which(df$institutionCode %in% "UTEP")][tf] <-
      gsub(", ", "", df$addCollector[which(df$institutionCode %in% "UTEP")][tf])
    df$recordedBy[which(df$institutionCode %in% "UTEP")][tf] <-
      gsub(",.+", "", df$recordedBy[which(df$institutionCode %in% "UTEP")][tf])
  }

  return(df)
}


#_______________________________________________________________________________
# Correcting specific collector names ####
.std_specific_coll <- function(df){

  #_____________________________________________________________________________
  # Cleaning Brazilian collectors

  # A
  tf <- grepl("^A.*M.* Amorim", df$recordedBy)
  df$recordedBy[tf] <- "A. M. A. Amorim"
  df$recordedBy[df$recordedBy %in% c("W. Anderson")] <- "W. R. Anderson"
  df$recordedBy[df$recordedBy %in% c("Andrade-Lima",
                                     "A. D. Andrade-Lima")] <- "D. Andrade-Lima"
  df$recordedBy[df$recordedBy %in% c("J. B. Christophore Fusée Aublet")] <- "J. B. C. F. Aublet"

  # B
  df$recordedBy[df$recordedBy %in% c("G. Maciel Barroso")] <- "G. M. Barroso"
  df$recordedBy[df$recordedBy %in% c("R. Henry Beddome")] <- "R. H. Beddome"
  df$recordedBy <- gsub("^M. M. Brandão$",
                        "M. Brandão", df$recordedBy)
  tf <- grepl("R. P. Bel|^Belem$", df$recordedBy)
  df$recordedBy[tf] <- "R. P. Belém"
  df$recordedBy[df$recordedBy %in% c("Burchell")] <- "W. J. Burchell"
  df$recordedBy[df$recordedBy %in% c("B. Marx",
                                     "R. Burle Marx")] <- "R. Burle-Marx"

  # C
  df$recordedBy[df$recordedBy %in% c("D. B. O. S. Cardoso")] <- "D. Cardoso"
  df$recordedBy[df$recordedBy %in% c("P. Cavalcante")] <- "P. B. Cavalcante"
  df$recordedBy[df$recordedBy %in% c("T. Barbosa Cavalcanti")] <- "T. B. Cavalcanti"
  df$recordedBy[df$recordedBy %in% c("C. A. C. Ferreira",
                                     "C. A. Cid Ferreira",
                                     "C. A. Cid",
                                     "C. A .Cid",
                                     "C. A. F.")] <- "C. A. Cid-Ferreira"
  tf <- grepl("^C. A. Cid|^C. A. C. Ferreira", df$recordedBy)
  df$recordedBy[tf] <- "C. A. Cid-Ferreira"
  df$recordedBy[df$recordedBy %in% c("A. S. Conceiçaõ")] <- "A. S. Conceição"
  df$recordedBy[df$recordedBy %in% c("T. Alfred Coward")] <- "T. A. Coward"
  df$recordedBy[df$recordedBy %in% c("R. C. Monteiro Costa")] <- "R. C. M. Costa"

  # D
  df$recordedBy[df$recordedBy %in% c("Daly",
                                     "D. Daly")] <- "D. C. Daly"
  df$recordedBy[df$recordedBy %in% c("M. E. Spence Davidson",
                                     "M. E. Davidson")] <- "M. E. S. Davidson"
  df$recordedBy[df$recordedBy %in% c("W. Adolpho Ducke",
                                     "W. A. Ducke",
                                     "A. Duck",
                                     "Ducke")] <- "A. Ducke"
  tf <- grepl("Ducke", df$recordedBy)
  df$recordedBy[tf] <- "A. Ducke"

  # F
  df$recordedBy[df$recordedBy %in% c("M. Clara Ferreira")] <- "M. C. Ferreira"
  df$recordedBy <- gsub("^Forzza|^R. Forzza|^R. C. Forzzan",
                        "R. C. Forzza", df$recordedBy)
  tf <- grepl("F. Franca|F. FrançA", df$recordedBy)
  df$recordedBy[tf] <- "F. França"
  df$recordedBy[df$recordedBy %in% c("R. Froes",
                                     "R. Lemos Froes-Cpatu",
                                     "Froes",
                                     "R. L. Froes",
                                     "R. Froes",
                                     "R. L. FrÃ³es",
                                     "R. L. Froes")] <- "R. L. Fróes"
  tf <- grepl("R. L. Fr", df$recordedBy)
  df$recordedBy[tf] <- "R. L. Fróes"
  df$recordedBy[df$recordedBy %in% c("H. Ogg Forbes")] <- "H. O. Forbes"

  # G
  df$recordedBy[df$recordedBy %in% c("A. F. Marie Glaziou",
                                     "A. Glaziou",
                                     "Glaziou")] <- "A. F. M. Glaziou"
  df$recordedBy <- gsub("^M. L. S. Guedes$|^M. Guedes$|^M. L. Silva Guedes|ML. Silva Guedes|M. Lenise Guedes",
                        "M. L. Guedes", df$recordedBy)
  df$recordedBy[df$recordedBy %in% c("A. Gentry")] <- "A. H. Gentry"

  # H
  df$recordedBy[df$recordedBy %in% c("R. Harley",
                                     "Harley",
                                     "R. H. Harley",
                                     "R. M. Harkey")] <- "R. M. Harley"
  tf <- grepl("Hatschbach", df$recordedBy)
  df$recordedBy[tf] <- "G. G. Hatschbach"
  df$recordedBy[df$recordedBy %in% c("M. J. G. Hopkins")] <- "M. Hopkins"
  df$recordedBy[df$recordedBy %in% c("F. W. R. Hostmann",
                                     "Hostmann",
                                     "F. W. Hostmann")] <- "W. R. Hostmann"
  # I
  df$recordedBy <- gsub("J. R. Vieira Iganci|J. R. Iganci|^Iganci$|J. Iganci",
                        "J. R. V. Iganci", df$recordedBy)
  df$recordedBy[df$recordedBy %in% c("H. Irwin",
                                     "Irwin")] <- "H. S. Irwin"
  # K
  df$recordedBy <- gsub("^A. C. Krapovickas",
                        "A. Krapovickas", df$recordedBy)
  df$recordedBy[df$recordedBy %in% c("B. Alexander Krukoff",
                                     "B. Krukoff",
                                     "Krukoff")] <- "B. A. Krukoff"
  # L
  df$recordedBy[df$recordedBy %in% c("E. Junqueira Leite")] <- "E. J. Leite"
  df$recordedBy <- gsub("^H..C. Lima|^H.c Lima|^H. Cavalcante Lima|^H. C Lima|^H. C. D. E. Lima|^H. C. dde Lima$|^H. C. De Lima|^H. C. DeLima|^H. c. Lima",
                        "H. C. Lima", df$recordedBy)
  df$recordedBy <- gsub("^G. P. çLewis$|G. Peter Lewis|G.PLewi",
                        "G. P. Lewis", df$recordedBy)
  df$recordedBy[df$recordedBy %in% c("Little")] <- "E. L. Little"
  df$recordedBy[df$recordedBy %in% c("Lombardi")] <- "J. A. Lombardi"
  df$recordedBy[df$recordedBy %in% c("Lorenzi")] <- "H. Lorenzi"
  df$recordedBy[df$recordedBy %in% c("Luetzelburg")] <- "P. von Luetzelburg"
  df$recordedBy[df$recordedBy %in% c("Ee.Nic Lughadha")] <- "E. Nic Lughadha"

  # M
  df$recordedBy[df$recordedBy %in% c("Martius",
                                     "C. F. vanMartius",
                                     "C. F. P. Martius",
                                     "C. F. Philipp von Martius",
                                     "K. F. von. Martius")] <- "C. F. P. von Martius"
  df$recordedBy[df$recordedBy %in% c("Maas")] <- "P. J. M. Maas"
  df$recordedBy <- gsub("^S.* Miotto",
                        "S. T. S. Miotto", df$recordedBy)
  df$recordedBy[df$recordedBy %in% c("Martinelli")] <- "G. Martinelli"
  df$recordedBy[df$recordedBy %in% c("L. A. Mattos Silva",
                                     "L. A. Matts Silva")] <- "L. A. Mattos-Silva"
  df$recordedBy[df$recordedBy %in% c("H. L. Mello Barreto",
                                     "H. L. M. Barreto")] <- "H. L. Mello-Barreto"
  df$recordedBy[df$recordedBy %in% c("R. Mello Silva",
                                     "R. mello-silva",
                                     "R. Mello")] <- "R. Mello-Silva"
  df$recordedBy[df$recordedBy %in% c("Mori",
                                     "S. Mori")] <- "S. A. Mori"
  df$recordedBy[df$recordedBy %in% c("P. Watson Moonlight")] <- "P. W. Moonlight"

  # O
  df$recordedBy[df$recordedBy %in% c("R. Paulo Orlandi")] <- "R. P. Orlandi"

  # P
  df$recordedBy <- gsub("^J. Paula Souza$|^J. Paula-Sousa$|^J. Paulo-Souza$|^J. Paula$|^J. P. Souza$|J. P. Sousa$",
                        "J. Paula-Souza", df$recordedBy)
  df$recordedBy[df$recordedBy %in% c("R. Toby Pennington")] <- "R. T. Pennington"
  df$recordedBy <- gsub("^G. P. Silva$",
                        "G. Pereira-Silva", df$recordedBy)
  df$recordedBy[df$recordedBy %in% c("G. C. Pereira Pinto")] <- "G. C. P. Pinto"
  df$recordedBy[df$recordedBy %in% c("J. James Pipoly",
                                     "J. Pipoly",
                                     "I. I. I. JJ-Pipoly")] <- "J. J. Pipoly"
  df$recordedBy[df$recordedBy %in% c("J. Pirani",
                                     "Pirani",
                                     "J. Rubens Pirani")] <- "J. R. Pirani"
  tf <- grepl("^J.*M.*Pires|^J. Murça Pires$", df$recordedBy)
  df$recordedBy[tf] <- "J. M. Pires"
  df$recordedBy[df$recordedBy %in% c("G. T. Prace")] <- "G. T. Prance"
  tf <- grepl("^C. E. B. Proen|^C.* Proenc|^C. E. Barnes Proença$", df$recordedBy)
  df$recordedBy[tf] <- "C. E. B. Proença"

  # Q
  df$recordedBy[df$recordedBy %in% c("L. P. Queiróz")] <- "L. P. Queiroz"
  df$recordedBy <- gsub("^L..P. Queiroz|^L. P. Queiroz.$|^L. P. De Queiroz",
                        "L. P. Queiroz", df$recordedBy)

  # R
  df$recordedBy <- gsub("^-R. -- Reitz|Pe. Raulino Reitz|^Reitz$|^P. R. Reitz$",
                        "R. Reitz", df$recordedBy)
  df$recordedBy[df$recordedBy %in% c("C. Tolledo Rizzini",
                                     "Rizzini")] <- "C. T. Rizzini"

  # S
  df$recordedBy[df$recordedBy %in% c("Sellow")] <- "F. Sellow"
  df$recordedBy[df$recordedBy %in% c("Spruce")] <- "R. Spruce"
  df$recordedBy <- gsub("^M. Fragomenir Simon$|^M. Simon|^Simon$|^M. Fragomeni Simon$",
                        "M. F. Simon", df$recordedBy)
  df$recordedBy <- gsub("^V. C. Sousa|^V. Castro Souza$ ",
                        "V. C. Souza", df$recordedBy)
  tf <- grepl("^R. Sch.*Rodrigues", df$recordedBy)
  df$recordedBy[tf] <- "R. Schütz-Rodrigues"
  df$recordedBy[df$recordedBy %in% c("Schwacke")] <- "C. A. W. Schwacke"
  df$recordedBy[df$recordedBy %in% c("Schultes",
                                     "R. Evans Schultes")] <- "R. E. Schultes"

  # T
  df$recordedBy[df$recordedBy %in% c("W. Wayt Thomas",
                                     "W. Thomas")] <- "W. W. Thomas"

  # V
  tf <- grepl("^J.*Valls$", df$recordedBy)
  df$recordedBy[tf] <- "J. F. M. Valls"

  # Z
  df$recordedBy[df$recordedBy %in% c("D. C. Zapii",
                                     "D. Zappi")] <- "D. C. Zappi"
  df$recordedBy[df$recordedBy %in% c("J. Zarucchi")] <- "J. L. Zarucchi"

  #_____________________________________________________________________________
  # Cleaning general collectors

  df$recordedBy[grepl("Academia Brasileira de C", df$recordedByOriginal)] <- "Academia Brasileira de Ciências"
  df$recordedBy[grepl("Equipe do Jardim Bot", df$recordedByOriginal)] <- "Equipe do Jardim Botânico de Brasília"

  #_____________________________________________________________________________
  # Cleaning Spanish-like collectors
  df$recordedBy[df$recordedBy %in% c("N. A. Zamora Villalobos")] <- "N. Zamora"
  df$recordedBy[df$recordedBy %in% c("G. Ibarra Manriquez")] <- "G. Ibarra"
  df$recordedBy[df$recordedBy %in% c("H. Mendoza Cifuentes")] <- "H. Mendoza"
  df$recordedBy[df$recordedBy %in% c("J. Rafael Garcia")] <- "J. R. García"
  df$recordedBy[df$recordedBy %in% c("M. Sousa Sánchez")] <- "M. Sousa"
  df$recordedBy[df$recordedBy %in% c("A. Reyes",
                                     "A. Reyes Garcia")] <- "A. Reyes-García"
  df$recordedBy[df$recordedBy %in% c("R. Vasquez Martinez",
                                     "Rod. Vasquez",
                                     "R. Vasquez")] <- "R. Vásquez"
  df$recordedBy[df$recordedBy %in% c("J. Schunke Vigo",
                                     "J. Schunke",
                                     "J. V. Schunke",
                                     "J. M. Schunke")] <- "J. Schunke-Vigo"
  df$recordedBy[df$recordedBy %in% c("M. M. arbo",
                                     "M. N. Arbo")] <- "M. M. Arbo"

  return(df)
}


#_______________________________________________________________________________
# Cleaning Asian-like names when they are in unicode characters from "UTF-8" encoding.

# Pre-cleaning before standardizing the collector names  ####
.preunicodeclean <- function(df, tf){

  message(".preunicodeclean $recordedBy Asian names #1")

  tfa <- grepl("[,]|[;]", df$recordedBy[tf])
  tfb <- grepl("(.*,.*,){1,}|(.*;.*;){1,}", df$recordedBy[tf][tfa])

  # Chinese names with more than two collectors
  if(any(tfb)){
    df$addCollector[tf][tfa] <-
      ifelse(tfb, "et al.", as.character(df$addCollector[tf][tfa]))
    df$recordedBy[tf][tfa][tfb] <-
      gsub(",.+", "", df$recordedBy[tf][tfa][tfb])
  }

  # Chinese names with just two collectors
  if(any(!tfb)){
    message(".preunicodeclean $recordedBy Asian names #2")
    add_asian_twocollA <- data.frame(collector=stringr::str_extract_all(df$recordedBy, "[,|;].+",
                                                                        simplify = TRUE))
    if(length(add_asian_twocollA) == 0){
      add_asian_twocollA <- data.frame(collector=rep(NA, length(row.names(df))))
    }
    add_asian_twocollA$collector <- as.character(add_asian_twocollA$collector)
    # a much better way two copy characters from one column to another
    df$addCollector[tf][tfa][!tfb] <-
      add_asian_twocollA$collector[tf][tfa][!tfb]
    df$addCollector <- gsub(",", "", df$addCollector)
    df$recordedBy[tf][tfa][!tfb] <-
      gsub(",.+", "", df$recordedBy[tf][tfa][!tfb])
  }

  # Chinese names with more than two collectors separated by space
  #"<U+738B><U+542F><U+65E0> <U+5218><U+745B>"
  tf <- grepl("(.*[>]\\s[<][[:upper:]]){2,}", df$recordedBy)
  if(any(tf)){
    message(".preunicodeclean $recordedBy Asian names #3")
    df$addCollector[tf] <- paste("et al.")
    df$recordedBy[tf] <- gsub("\\s.+", "", df$recordedBy[tf])
  }

  # Chinese names with more than two collectors that are not separated
  #"T.N.Liou<U+3001>P.C.Tsoong"
  tf <- grepl("[[:upper:]][[:lower:]]+[<][[:upper:]]", df$recordedBy)
  if(any(tf)){
    message(".preunicodeclean $recordedBy Asian names #4")
    df$addCollector[tf] <- paste("et al.")
    df$recordedBy[tf] <- gsub("[<].+", "", df$recordedBy[tf])
  }

  # Chinese names with mixed unicode and Latin-like characters
  # Extracting names also seperated by a space
  # "T.N.Liou <U+3001>P.C.Tsoong"
  tf <- grepl("[[:upper:]][[:lower:]]+\\s[<][[:upper:]]", df$recordedBy)
  tfa <- grepl("([[:lower:]]+)$", df$recordedBy[tf])
  if(any(tfa)){
    message(".preunicodeclean $recordedBy Asian names #5")
    df$addCollector[tf][tfa] <- paste("et al.")
    df$recordedBy[tf][tfa] <- gsub("\\s.+", "", df$recordedBy[tf][tfa])
  }

  # Chinese names with two collectors separated by space
  #"<U+738B><U+542F><U+65E0> <U+5218><U+745B>"
  tf <- grepl("(.*[>]\\s[<][[:upper:]]){1}", df$recordedBy)
  if(any(tf)){
    message(".preunicodeclean $recordedBy Asian names #6")
    df$addCollector[tf] <- gsub(".*?\\s", "", df$recordedBy[tf])
    df$recordedBy[tf] <- gsub("\\s.+", "", df$recordedBy[tf])
  }

  # Chinese names with mixed unicode and Latin-like characters
  # Extracting names also seperated by a space
  # "T.N.Liou<U+3001>P.C.Tsoong"
  tf <- grepl("[[:upper:]][[:lower:]]+\\s[<][[:upper:]]", df$recordedBy)
  if(any(tf)){
    message(".preunicodeclean $recordedBy Asian names #7")
    df$addCollector[tf] <- gsub(".*?\\s", "", df$recordedBy[tf])
    df$recordedBy[tf] <- gsub("\\s.+", "", df$recordedBy[tf])
  }

  return(df)
}


#_______________________________________________________________________________
# Cleaning Asian-like names when they are in unicode characters ####
.unicodeclean <- function(df, tf) {

  # ## Converting Asian-like names into Latin alphabet
  #
  # require(stringi)
  # convertCHINESE <-  stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1",
  #                                               c("<U+674E><U+9E23><U+5188>",
  #                                                 "<U+4F55><U+666F>",
  #                                                 "Domingos Cardoso, <U+9EC4><U+555F><U+658C>",
  #                                                 "<U+8983><U+704F><U+5BCC>,<U+674E><U+4E2D><U+63D0>")))
  #
  # convertCHINESE1 <-  stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", Ormosia.df$recordedBy[TESTE]))

  message(".unicodeclean $recordedBy Asian-like names in unicode")

  #library(tmcn)
  ## If we wanted to keep in original Chinese characters
  #df$recordedBy[tf] <- revUTF8(df$recordedBy[tf])

  ## Parsing the Chinese characters into Latin characters
  asian_to_latinA <- tmcn::toPinyin(revUTF8(df$recordedBy[tf]),
                                    capitalize = T)
  parseA <- tmcn::strextract(asian_to_latinA, "(?<first>[[:upper:]][[:lower:]]+)",
                             perl = TRUE)
  df$recordedBy[tf] <- sapply(parseA, paste, collapse=" ")


  tf <- grepl("[+][0-9]", df$addCollector)
  if (any(tf)) {
    message(".unicodeclean $recordedBy Asian-like names in unicode")
    ## If we wanted to keep in original Chinese characters
    #df$addCollector[tf] <- revUTF8(df$addCollector[tf])

    ## Parsing the Chinese characters into Latin characters
    asian_to_latinB <- tmcn::toPinyin(revUTF8(df$addCollector[tf]),
                                      capitalize = T)
    parseB <- tmcn::strextract(asian_to_latinB, "(?<first>[[:upper:]][[:lower:]]+)",
                               perl = TRUE)
    df$addCollector[tf] <- sapply(parseB, paste, collapse=" ")
  }

  return(df)
}


#_______________________________________________________________________________
# Standardize additional collectors in the newly created column addCollector ####
.addcollclean <- function(df) {

  message(".addcollclean $addCollector")
  # Inserting NAs in and empty cell
  df$addCollector <- gsub("^$|^\\s$", NA, df$addCollector)

  # Deleting any numbers from $addCollector
  tf <- grepl("[0-9]", df$addCollector)
  if (any(tf)){
    df$addCollector[tf] <- gsub("[0-9]", "", df$addCollector[tf])
  }

  # Deleting blank space at the beginning and end of the cell
  df$addCollector <- gsub("^[[:space:]]", "", df$addCollector)
  df$addCollector <- gsub("[[:space:]]{2}", " ", df$addCollector)
  df$addCollector <- gsub("[[:space:]]$", "", df$addCollector)

  # Adding NA for cells with "s.c."
  df$addCollector[which(df$addCollector %in% c("s.c."))] <- NA


  temp <- c("Grupo", "Alunos", "Plantas Vasculares", "British Guiana Forestry",
            "de estudios")
  tf <- grepl(paste0(temp, collapse = "|"), df$addCollector)
  if (any(tf)){
    df$addCollector <- ifelse(tf, "et al.", as.character(df$addCollector))
  }


  tf <- grepl("[[:lower:]]+\\s+[[:upper:]][[:lower:]]+[,]+\\s+[[:upper:]][[:lower:]]+\\s+[[:upper:]][[:lower:]]+",
              df$addCollector)
  if (any(tf)){
    df$addCollector <- ifelse(tf, "et al.", as.character(df$addCollector))
  }

  tf <- grepl("(.*[-].*[-]){1,}", df$addCollector)
  if (any(tf)){
    df$addCollector <- ifelse(tf, "et al.", as.character(df$addCollector))
  }

  # Examples like "L Cardin-A C Borges"
  tf <- grepl("\\s+[[:upper:]][[:lower:]]+[-]+[[:upper:]]+\\s", df$addCollector)
  if (any(tf)){
    df$addCollector <- ifelse(tf, "et al.", as.character(df$addCollector))
  }

  tf <- grepl("(.*[[:space:]].*[[:space:]]){3,}", df$addCollector)
  if (any(tf)){
    df$addCollector <- ifelse(tf, "et al.", as.character(df$addCollector))
  }

  tf <- grepl("[;]+[[:upper:]][[:lower:]]+[,]", df$addCollector)
  if (any(tf)){
    df$addCollector[tf] <- gsub(";", "", df$addCollector[tf])
  }

  tf <- grepl("([[:upper:]][.]){1,}[[:upper:]][[:lower:]]", df$addCollector)
  if (any(tf)){
    df$addCollector[tf] <- gsub("[.]", ". ", df$addCollector[tf])
  }

  tf <- grepl("([[:upper:]][.]){2,}\\s+[[:upper:]][[:lower:]]", df$addCollector)
  if (any(tf)){
    # Use sub as it matches only the first occurrence of pattern
    df$addCollector[tf] <- sub("[.]", ". ", df$addCollector[tf])
  }

  tf <- grepl("([[:upper:]][.]){1,}\\s+[[:upper:]][[:lower:]]+\\s+[[:upper:]][.]", df$addCollector)
  if (any(tf)){
    # Deleting last name after last space
    df$addCollector[tf] <- gsub("\\s[^ ]+$", "", df$addCollector[tf])
  }

  # Cleaning collector names like Acero, E.
  tf <- grepl(",", df$addCollector)
  if (any(tf)){
    temp_df <- data.frame(initials=stringr::str_extract_all(df$addCollector, ",.+",
                                                            simplify = TRUE))
    if(length(temp_df) == 0){
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)
    temp_df$initials[tf] <-
      gsub(",", "", temp_df$initials[tf])
    temp_df$initials[tf] <-
      gsub("^\\s", "", temp_df$initials[tf])

    # Then delete all initials from the main column addCollector
    df$addCollector[tf] <-
      gsub(",.+", "", df$addCollector[tf])
    df$addCollector[tf] <-
      paste(as.character(temp_df$initials[tf]),
            as.character(df$addCollector[tf]), sep=" ")
  }


  # Make the names in just the first letter capitalized
  tf <- grepl("([[:upper:]]){4,}", df$addCollector)
  if (any(tf)){
    df$addCollector[tf] <- gsub("\\b([a-z])", "\\U\\1",
                                tolower(df$addCollector[tf]), perl = TRUE)
  }


  # cleaning examples like "Egler WA", "Trotz N"
  tf <- grepl("[[:lower:]]+\\s([[:upper:]]){1,}$", df$addCollector)
  tfA <- !grepl("[.]", df$addCollector[tf])
  if (any(tfA)){
    temp_df <- data.frame(initials=stringr::str_extract_all(df$addCollector, "\\s.+",
                                                            simplify = TRUE))
    if(length(temp_df) == 0){
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)
    temp_df$initials[tf][tfA] <-
      gsub("^\\s", "", temp_df$initials[tf][tfA])

    # Then delete all initials from the main column addCollector
    df$addCollector[tf][tfA] <-
      sub("\\s.+", "", df$addCollector[tf][tfA])
    df$addCollector[tf][tfA] <-
      paste(as.character(temp_df$initials[tf][tfA]),
            as.character(df$addCollector[tf][tfA]), sep=" ")

  }

  # cleaning examples like "F. Chigo S"
  tf <- grepl("[[:lower:]]+\\s([[:upper:]]){1,}$", df$addCollector)
  if (any(tf)){
    df$addCollector[tf] <- gsub("\\s[^ ]+$", "", df$addCollector[tf])
  }

  # cleaning examples like "Patricia Gómez A."
  tf <- grepl("\\s[[:upper:]][[:lower:]]+\\s([[:upper:]][.]){1}$", df$addCollector)
  if (any(tf)){
    df$addCollector[tf] <- gsub("\\s[^ ]+$", "", df$addCollector[tf])
  }

  # cleaning examples like "Sueroque F.", "Jaramillo R."
  tf <- grepl("[[:lower:]]+\\s([[:upper:]][.]){1}$", df$addCollector)
  if (any(tf)){
    temp_df <- data.frame(initials=stringr::str_extract_all(df$addCollector, "\\s.+",
                                                            simplify = TRUE))
    if(length(temp_df) == 0){
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)
    temp_df$initials[tf] <-
      gsub("^\\s", "", temp_df$initials[tf])

    # Then delete all initials from the main column addCollector
    df$addCollector[tf] <- sub("\\s.+", "", df$addCollector[tf])
    df$addCollector[tf] <- paste(as.character(temp_df$initials[tf]),
                                 as.character(df$addCollector[tf]), sep=" ")
  }

  # cleaning examples like "."
  tf <- grepl("^[.]$", df$addCollector)
  if (any(tf)){
    df$addCollector[tf] <- gsub("^[.]$", NA, df$addCollector[tf])
  }

  # cleaning examples like " . "
  tf <- grepl("\\s[.]\\s", df$addCollector)
  if (any(tf)){
    df$addCollector[tf] <- gsub("\\s[.]\\s", ". ", df$addCollector[tf])
  }

  # cleaning examples like "TA Naves,"
  tf <- grepl(",", df$addCollector)
  if (any(tf)){
    df$addCollector[tf] <- gsub(",", "", df$addCollector[tf])
  }

  # cleaning examples like "Steege H ter", "Paie I bin"
  tf <- grepl("[[:lower:]]+\\s[[:upper:]]\\s[[:lower:]]+$", df$addCollector)
  if (any(tf)){
    temp_df <- data.frame(initials=stringr::str_extract_all(df$addCollector, "\\s.+",
                                                            simplify = TRUE))
    if(length(temp_df) == 0){
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)
    temp_df$initials[tf] <-
      gsub("^\\s", "", temp_df$initials[tf])

    # Then delete all initials from the main column addCollector
    df$addCollector[tf] <- sub("\\s.+", "", df$addCollector[tf])
    df$addCollector[tf] <- paste(as.character(temp_df$initials[tf]),
                                 as.character(df$addCollector[tf]), sep=" ")
  }

  # cleaning examples like "Wilde-Duyfjes BEE de"
  tf <- grepl("[[:upper:]][[:lower:]]+\\s([[:upper:]]){2,}", df$addCollector)
  if (any(tf)){
    temp_df <- data.frame(initials=stringr::str_extract_all(df$addCollector, "\\s.+",
                                                            simplify = TRUE))
    if(length(temp_df) == 0){
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)
    temp_df$initials[tf] <-
      gsub("^\\s", "", temp_df$initials[tf])

    # Then delete all initials from the main column addCollector
    df$addCollector[tf] <- sub("\\s.+", "", df$addCollector[tf])
    df$addCollector[tf] <- paste(as.character(temp_df$initials[tf]),
                                 as.character(df$addCollector[tf]), sep=" ")
  }

  # Cleaning particles like de, da, dos, do
  tf <- grepl(" de", df$addCollector)
  nondel.van <- grepl(paste(c("van den", "van der"), collapse = "|"), df$addCollector)
  df$addCollector[which(tf - nondel.van == T)] <-
    gsub(" de", " ", df$addCollector[which(tf - nondel.van == T)])

  tf <- grepl("[.]de", df$addCollector)
  df$addCollector[tf] <- gsub("de", " ", df$addCollector[tf])
  tf <- grepl(" De ", df$addCollector)
  df$addCollector[tf] <- gsub(" De ", " ", df$addCollector[tf])
  tf <- grepl(" DE ", df$addCollector)
  df$addCollector[tf] <- gsub(" DE ", " ", df$addCollector[tf])
  tf <- grepl("^De ", df$addCollector)
  df$addCollector[tf] <- gsub("^De ", "", df$addCollector[tf])
  tf <- grepl("^de ", df$addCollector)
  df$addCollector[tf] <- gsub("^de ", "", df$addCollector[tf])
  tf <- grepl("[.]\\sDE$", df$addCollector)
  df$addCollector[tf] <- gsub(" DE$", "", df$addCollector[tf])

  tf <- grepl("[[:space:]]da$", df$addCollector)
  df$addCollector[tf] <- gsub(" da", "", df$addCollector[tf])
  tf <- grepl(" da ", df$addCollector)
  df$addCollector[tf] <- gsub(" da ", " ", df$addCollector[tf])
  tf <- grepl("[.]da", df$addCollector)
  df$addCollector[tf] <- gsub("da", " ", df$addCollector[tf])
  tf <- grepl("[.]\\sDA\\s", df$addCollector)
  df$addCollector[tf] <- gsub("DA\\s", "", df$addCollector[tf])

  tf <- grepl(" dos", df$addCollector)
  df$addCollector[tf] <- gsub(" dos", " ", df$addCollector[tf])
  tf <- grepl(" do", df$addCollector)
  df$addCollector[tf] <- gsub(" do", " ", df$addCollector[tf])
  tf <- grepl("[.]dos", df$addCollector)
  df$addCollector[tf] <- gsub("dos", " ", df$addCollector[tf])
  tf <- grepl("[.]do", df$addCollector)
  df$addCollector[tf] <- gsub("do", " ", df$addCollector[tf])

  #_____________________________________________________________________________
  # Abreviate first name
  # Mark the names like "Sergio M Faria", "Domingos S Cardoso", "Marcelo T Nascimento"
  # these names are not separated by comma or full period in the initials
  tf <- grepl("[[:lower:]]+\\s+([[:upper:]]{1,})+\\s", df$addCollector)
  if (any(tf)){
    # Extracting collector initials
    temp_df <- data.frame(initials=stringr::str_extract_all(df$addCollector, "^(\\S*\\s+)",
                                                            simplify = TRUE))
    if(length(temp_df) == 0){
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)

    temp_df$initials[!tf] <- NA
    temp_df$initials[tf] <-
      gsub("^ ", "", temp_df$initials[tf])
    temp_df$initials[tf] <-
      abbreviate(temp_df$initials[tf],
                 minlength = 1, strict = T, dot = F, use.classes = F)
    # Deleting first name, which is before first space
    df$addCollector[tf] <- gsub("^(\\S*\\s+)", "", df$addCollector[tf])
    df$addCollector <- ifelse(!is.na(temp_df$initials == TRUE),
                              paste(as.character(temp_df$initials),
                                    as.character(df$addCollector), sep=""),
                              as.character(df$addCollector))
  }

  # Abbreviate names like "David J.N. Hind", "Jorge C. A. Lima", "Grady L. Webster"
  tf <- grepl("[[:upper:]][[:lower:]]+\\s+(.*[[:upper:]][.]){1,}\\s+[[:alpha:]]{3}",
              df$addCollector)
  if (any(tf)){
    # Extracting collector initials
    temp_df <- data.frame(initials=stringr::str_extract_all(df$addCollector, "^(\\S*\\s+)",
                                                            simplify = TRUE))
    if(length(temp_df) == 0){
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)
    temp_df$initials[!tf] <- NA
    temp_df$initials[tf] <-
      gsub("^ ", "", temp_df$initials[tf])
    temp_df$initials[tf] <- abbreviate(temp_df$initials[tf],
                                       minlength = 1, strict = T, dot = T, use.classes = F)
    # Deleting first name, which is before first space
    df$addCollector[tf] <- gsub("^(\\S*\\s+)", "", df$addCollector[tf])
    df$addCollector <- ifelse(!is.na(temp_df$initials == TRUE),
                              paste(as.character(temp_df$initials),
                                    as.character(df$addCollector), sep=" "),
                              as.character(df$addCollector))
  }

  #_____________________________________________________________________________
  # Abreviate first name
  # Mark the names like "Sergio Faria",
  # these names are not separated by comma or full period in the initials
  tf <- grepl("^[[:upper:]][[:lower:]]+\\s+[[:upper:]][[:lower:]]+", df$addCollector)
  if (any(tf)){
    # Extracting collector initials
    temp_df <- data.frame(initials=stringr::str_extract_all(df$addCollector, "^(\\S*\\s+)",
                                                            simplify = TRUE))
    if(length(temp_df) == 0){
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)

    temp_df$initials[!tf] <- NA
    temp_df$initials[tf] <-
      gsub("^ ", "", temp_df$initials[tf])
    temp_df$initials[tf] <- abbreviate(temp_df$initials[tf],
                                       minlength = 1, strict = T, dot = T, use.classes = F)
    # Deleting first name, which is before first space
    df$addCollector[tf] <- gsub("^(\\S*\\s+)", "", df$addCollector[tf])
    df$addCollector <- ifelse(!is.na(temp_df$initials == TRUE),
                              paste(as.character(temp_df$initials),
                                    as.character(df$addCollector), sep=" "),
                              as.character(df$addCollector))
  }

  #_____________________________________________________________________________
  # Adding full period in names like D Cardoso, DD Cardoso, DDD Cardoso
  tf <- grepl("^([[[:upper:]]){1,}\\s[[:upper:]][[:lower:]]", df$addCollector)
  if (any(tf)){
    # Extracting initials
    temp_df <- data.frame(initials=stringr::str_extract_all(df$addCollector, "^(\\S*\\s+)",
                                                            simplify = TRUE))
    if(length(temp_df) == 0){
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)

    temp_df$initials[tf] <- gsub(" $", "", temp_df$initials[tf])
    # adding a space between all initials
    temp_df$initials[tf] <- lapply(temp_df$initials[tf],
                                   function(x) trimws(gsub("([[:alpha:]])", " \\1", x)))
    # adding a space at the end initials
    temp_df$initials[tf] <- gsub("$", " ", temp_df$initials[tf])
    # replace space by full periods
    temp_df$initials[tf] <-
      gsub(" ", ".", temp_df$initials[tf])
    # adding space between each inicials now with full period
    # put the space after the "\\1 "
    temp_df$initials[tf] <- lapply(temp_df$initials[tf],
                                   function(x) trimws(gsub("([[:punct:]])", "\\1 ", x)))
    temp_df <- data.frame(initials=unlist(temp_df$initials))
    temp_df$initials <- as.character(temp_df$initials)
    # Remove first words before first space
    df$addCollector[tf] <- gsub("^(\\S*)", "", df$addCollector[tf])
    df$addCollector[tf] <- gsub("^.", "", df$addCollector[tf])

    df$addCollector[tf] <- paste(as.character(temp_df$initials[tf]),
                                 as.character(df$addCollector[tf]), sep=" ")
  }


  tf <- grepl("([[[:upper:]][.]){2,}|[[[:upper:]][.]{1,}[[:upper:]][[:lower:]]+", df$addCollector)
  if (any(tf)){
    df$addCollector[tf] <- gsub("[.]", ". ", df$addCollector[tf])
    df$addCollector[tf] <- gsub("[[:space:]]{2}", " ", df$addCollector[tf])
    df$addCollector[tf] <- gsub("[.]$", "", df$addCollector[tf])
    df$addCollector[tf] <- gsub("[[:space:]]$", "", df$addCollector[tf])
  }

  # Cleaning examples like "H ter Steege", "I bin Paie", "PP-H But"
  tf <- grepl("([[[:upper:]]){1,}\\s", df$addCollector)
  if (any(tf)){
    # Extracting initials
    temp_df <- data.frame(initials=stringr::str_extract_all(df$addCollector, "^(\\S*\\s+)",
                                                            simplify = TRUE))
    if(length(temp_df) == 0){
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(temp_df$initials)

    temp_df$initials[tf] <- gsub(" $", "", temp_df$initials[tf])
    # adding a space between all initials
    temp_df$initials[tf] <- lapply(temp_df$initials[tf],
                                   function(x) trimws(gsub("([[:alpha:]])", " \\1", x)))
    # adding a space at the end initials
    temp_df$initials[tf] <- gsub("$", " ", temp_df$initials[tf])
    # replace space by full periods
    temp_df$initials[tf] <-
      gsub(" ", ".", temp_df$initials[tf])
    # adding space between each inicials now with full period
    # put the space after the "\\1 "
    temp_df$initials[tf] <- lapply(temp_df$initials[tf],
                                   function(x) trimws(gsub("([[:punct:]])", "\\1 ", x)))
    temp_df <- data.frame(initials=unlist(temp_df$initials))
    temp_df$initials <- as.character(temp_df$initials)
    # Remove first words before first space
    df$addCollector[tf] <- gsub("^(\\S*)", "", df$addCollector[tf])
    df$addCollector[tf] <- gsub("^.", "", df$addCollector[tf])

    df$addCollector[tf] <- paste(as.character(temp_df$initials[tf]),
                                 as.character(df$addCollector[tf]), sep=" ")

    # correcting the examples like "P. P- . H." by first removing the first duplicated initial
    tfA <- grepl("[-]\\s[.]", df$addCollector[tf])
    df$addCollector[tf][tfA] <- gsub("^(\\S*\\s+)", "",
                                     df$addCollector[tf][tfA])
    df$addCollector[tf][tfA] <- gsub("[-]\\s[.]\\s",".-", df$addCollector[tf][tfA])
  }


  #Cleaning examples like J. R. M Ferreira"
  tf <- grepl("([[[:upper:]]){1,}\\s", df$addCollector)
  tfA <- grepl("([[[:upper:]][.])", df$addCollector[tf])
  if (any(tfA)){
    df$addCollector[tf][tfA] <- gsub("\\s", ". ", df$addCollector[tf][tfA])
    df$addCollector[tf][tfA] <- gsub("[.][.]\\s", "", df$addCollector[tf][tfA])
  }

  #Cleaning examples like "C. H. R.  Paula.", "G. Calero Ch."
  tf <- grepl("\\s[[[:upper:]][[:lower:]]+[.]", df$addCollector)
  if (any(tf)){
    df$addCollector[tf] <- gsub("[.]$", "", df$addCollector[tf])

    tfA <- grepl("\\s[[[:upper:]][[:lower:]]+\\s", df$addCollector[tf])
    df$addCollector[tf][tfA] <- gsub("\\s[^ ]+$", "", df$addCollector[tf][tfA])

  }

  # Further cleaning examples like "A. .R. Lopes"
  df$addCollector <- gsub("[.]\\s[.]", ". ", df$addCollector)

  # Removing a full period at the end of the name
  df$addCollector <- gsub("^[[:space:]]", "", df$addCollector)
  df$addCollector <- gsub("[[:space:]]{2}", " ", df$addCollector)


  # Cleaning examples like "AORibeiro"
  tf <- grepl("^[[:upper:]]{2,}[[:lower:]]+", as.character(df$addCollector))
  if (any(tf)){

    # lets separate the initials from the surname first
    df$addCollector[tf] <- gsub("([[:upper:]])([[:upper:]][[:lower:]])", "\\1 \\2", df$addCollector[tf])

    # strsplit(df$addCollector[tf],
    #          split = "(?<=[[:upper:]])(?=[[:upper:]][[:lower:]])", perl = TRUE)[[1]]

    # Adding the period
    # Extracting initials
    temp_df <- data.frame(initials=stringr::str_extract_all(df$addCollector[tf], "^(\\S*\\s+)",
                                                            simplify = TRUE))
    if(length(temp_df) == 0){
      temp_df <- data.frame(initials=rep(NA, length(row.names(df))))
    }
    temp_df$initials <- as.character(gsub(" $", "", temp_df$initials))

    # adding a space between all initials
    temp_df$initials <- gsub("([[:upper:]])([[:upper:]])", "\\1 \\2", temp_df$initials)

    # temp_df$initials <- lapply(temp_df$initials,
    #                                  function(x) trimws(gsub("([[:alpha:]])", " \\1", x)))
    # adding a space at the end initials
    temp_df$initials <- gsub("$", " ", temp_df$initials)
    # replace space by full periods
    temp_df$initials <- gsub(" ", ". ", temp_df$initials)
    temp_df$initials <- gsub("\\s$", "", temp_df$initials)


    # Remove first words before first space
    df$addCollector[tf] <- gsub("^(\\S*)", "", df$addCollector[tf])
    df$addCollector[tf] <- gsub("^.", "", df$addCollector[tf])

    df$addCollector[tf] <- paste(as.character(temp_df$initials),
                                 as.character(df$addCollector[tf]), sep=" ")
  }

  return(df)
}


#_______________________________________________________________________________
# Pre-cleaning collector numbers before standardizing collector names ####
.prenbrclean <- function(df){

  # Clean e.g. "Nakajima, J.N. 3101;...", "Harley, R.M. 20580;..."
  tf <- grepl("[[:digit:]];", df$recordedBy)
  # Extracting only numbers
  #https://stackoverflow.com/questions/14543627/extracting-numbers-from-vectors-of-strings
  #as.character(as.numeric(gsub("\\D", "", df$recordedBy[tf])))
  if (any(tf)) {
    message(".prenbrclean $recordedBy numbers #1")
    df$recordNumber[tf] <- ifelse(is.na(df$recordNumber[tf]),
                                  as.character(as.numeric(gsub("\\D", "", df$recordedBy[tf]))),
                                  as.character(df$recordNumber[tf]))

    # Replacing or removing numbers for nothing/ deleting numbers
    df$recordedBy[tf] <- gsub("\\d", "", df$recordedBy[tf])
  }

  # Finding examples like "Martius, C.F.P. von (no. Obs. 1935)"
  # "Martius, C.F.P. von (no. [Obs. 1383])", "Luetzelburg, P. von (no. 142)"
  tf <- grepl("[(]no[.]\\sObs[.]|[(]no[.]\\s[[]Obs[.]|\\s[(]no[.]\\s", df$recordedBy)
  # Extracting only numbers
  #https://stackoverflow.com/questions/14543627/extracting-numbers-from-vectors-of-strings
  #as.character(as.numeric(gsub("\\D", "", df$recordedBy[tf])))
  if (any(tf)) {
    message(".prenbrclean $recordedBy numbers #2")
    df$recordNumber[tf] <- ifelse(is.na(df$recordNumber[tf]),
                                  as.character(as.numeric(gsub("\\D", "", df$recordedBy[tf]))),
                                  as.character(df$recordNumber[tf]))

    # Replacing or removing numbers for nothing/ deleting numbers
    df$recordedBy[tf] <- gsub("\\s[(].+", "", df$recordedBy[tf])
  }

  # Clean e.g. "8470 G.H. Turner", "67-1240 N.C. Henderson", "1042 J. Campbell-Snelling, M. Chambers"
  tf <- grepl("^[0-9]\\s[[:upper:]]", df$recordedBy)
  if (any(tf)) {
    message(".prenbrclean $recordedBy numbers #3")
    df$recordNumber[tf] <- gsub("\\s.+", "", df$recordedBy[tf])
    # Remove all before the the first comma and space
    df$recordedBy[tf] <- sub(".*?\\s", "", df$recordedBy[tf])
  }

  # Clean e.g. "C Davis 812"
  tf <- grepl("[[:upper:]][[:lower:]]+\\s[0-9]", df$recordedBy)
  if (any(tf)) {
    message(".prenbrclean $recordedBy numbers #4")
    df$recordedBy[tf] <- gsub(";\\sBiology.*", "", df$recordedBy[tf])
    # Remove everything BEFORE the last space
    df$recordNumber[tf] <- sub(".*\\s", "", df$recordedBy[tf])
    df$recordNumber[tf] <- gsub("[[:alpha:]]", NA, df$recordNumber[tf])
    # Remove everything AFTER the last space
    df$recordedBy[tf] <- sub("\\s+[^ ]+$", "", df$recordedBy[tf])
  }

  # Clean e.g. "C Davis D-14"
  tf <- grepl("[[:upper:]][[:lower:]]+\\s[[:alpha:]][-][0-9]", df$recordedBy)
  if (any(tf)) {
    message(".prenbrclean $recordedBy numbers #5")
    # Remove everything BEFORE the last space
    df$recordNumber[tf] <- sub(".*\\s", "", df$recordedBy[tf])
    # Remove everything AFTER the last space
    df$recordedBy[tf] <- sub("\\s+[^ ]+$", "", df$recordedBy[tf])
  }

  # Clean e.g. "Mark Hughes Sumatra 2011"
  tf <- grepl("\\sSumatra\\s", df$recordedBy)
  if (any(tf)) {
    message(".prenbrclean $recordedBy numbers #6")
    df$recordedBy <- gsub("\\sSumatra.+", "", df$recordedBy)
  }

  # Clean e.g.
  # Jesus, M.L.B. de 132
  tf <- grepl("[[:upper:]][.]\\s[[:lower:]]+\\s[0-9]+$", df$recordedBy)
  if (any(tf)) {
    message(".prenbrclean $recordedBy numbers #7")
    df <- .rm_before_after(df, tf, ".*\\s", "\\s+[^ ]+$")
  }

  # Clean e.g.
  # Brade, A.C. 17713; Altamiro, B. & Mello Filho, L.E.
  tf <- grepl("([[:upper:]][.]){1,}\\s[0-9]+;", df$recordedBy)
  if (any(tf)) {
    message(".prenbrclean $recordedBy numbers #8")
    df <- .rm_before_after(df, tf, ";.*", "[0-9]+")
  }

  # Clean e.g.
  # Conceicao, A.A. 1161
  tf <- grepl("([[:upper:]][.]){1,}\\s[0-9]+$", df$recordedBy)
  if (any(tf)) {
    message(".prenbrclean $recordedBy numbers #9")
    df <- .rm_before_after(df, tf, ".*\\s", "\\s+[^ ]+$")
  }

  # Clean e.g.
  # L.M.NASCIMENTO481
  tf <- grepl("[[:upper:]][.][[:upper:]]+[0-9]+$", df$recordedBy)
  if (any(tf)) {
    message(".prenbrclean $recordedBy numbers #10")
    df <- .rm_before_after(df, tf, ".*[[:upper:]]", "[0-9]+[^ ]+$")
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
  # Girardi-Deiro, 1174
  # R. Záchia, 1914
  # douglass18 or julia_santos1998

  # remover numeros
  # A. Carvalho (1)
  # J.S. Silva (1); A.L.B. Sartori & F.M. Alves


  # tf <- grepl("[(]", df$recordedBy)
  # if (any(tf)) {
  #   message(".prenbrclean $recordedBy numbers #10")
  #   df <- .rm_before_after(df, tf, ".*[[:upper:]]", "[0-9]+[^ ]+$")
  # }
  #

  # To get many examples like

  # [15] "Acevedo-Rodríguez 16730"
  # [16] "Hatschbach Sobrinho 23446"
  # [17] "Hatschbach Sobrinho 13215"
  # [18] "Acevedo-Rodríguez 16738"
  # [19] "Acevedo-Rodríguez 16741"
  # [20] "Adalardo de Oliveira 2775"
  # [21] "Hatschbach Sobrinho 2826"
  # [22] "Fernandes s.n. (EAC 11333)"

  # tf <- !is.na(df$recordNumber)
  # tftf <- nchar(df$recordNumber[tf]) > 20
  # if(any(tftf)){
  #   df$recordNumber[tf][tftf] <- NA
  #   df$recordNumberOriginal[tf][tftf] <- NA
  # }
  #

  return(df)
}

# Side function to remove text BEFORE and AFTER a pattern
.rm_before_after <- function(df, tf, pattern_before, pattern_after){
  # Remove everything BEFORE
  df$recordNumber[tf] <- sub(pattern_before, "", df$recordedBy[tf])
  # Remove everything AFTER
  df$recordedBy[tf] <- sub(pattern_after, "", df$recordedBy[tf])

  return(df)
}


#_______________________________________________________________________________
# Auxiliary function for cleaning numbers at $recordNumber ####
.std_recordNumber <- function(df,
                              colnames_df = colnames_df,
                              colname_recordNumber = colname_recordNumber) {

  if (colname_recordNumber != "recordNumber") {
    names(df)[colnames_df %in% colname_recordNumber] <- "recordNumber"
  }

  message(".std_recordNumber $recordNumber")

  # Clean $recordNumber as character
  df$recordNumber <- as.character(df$recordNumber)

  # Adding NAs in unumbered collections
  df$recordNumber[which(df$recordNumber %in% c("s/n",
                                               "s.n.",
                                               "s. n.",
                                               "PCDs/n",
                                               "S.n.",
                                               "S.N.",
                                               "S. N.",
                                               "s.n",
                                               "s,n,",
                                               "sn",
                                               "SN",
                                               "N",
                                               "N.",
                                               "n",
                                               "n.",
                                               "nd",
                                               "possibly"))] <- NA

  tf <- grepl("s[.]n[.]", df$recordNumber)
  if (any(tf)){
    df$recordNumber[tf] <- NA
  }

  # Adding NAs  to empty cells
  df$recordNumber <- gsub("^$", NA, trimws(df$recordNumber))

  # Finding "s/nº"
  # https://en.wikipedia.org/wiki/ISO/IEC_8859-1
  tf <- grepl("s/n\\xba", df$recordNumber)
  if (any(tf)){
    df$recordNumber[tf] <- gsub("s/n\\xba", NA, df$recordNumber[tf])

  }

  tf <- grepl("[[:upper:]][[:lower:]]+\\s([0-9]){1,}\\s[[:print:]]", df$recordNumber)
  if (any(tf)){
    # Remove all after last space
    df$recordNumber[tf] <- gsub("\\s+[^ ]+$", "", df$recordNumber[tf])
  }

  # Remove spaces at beginning and end
  df$recordNumber <- gsub("^\\s", "", df$recordNumber)
  df$recordNumber <- gsub("\\s$", "", df$recordNumber)
  df$recordNumber <- gsub("[[:space:]]{2}", " ", df$recordNumber)

  # Remove names
  tf <- grepl("[A-Za-z]", df$recordNumber)
  # Now deleting all spaces before the numbers
  if (any(tf)){
    df$recordNumber[tf] <- gsub(".+? ", "", df$recordNumber[tf])
  }

  # General cleaning
  df$recordNumber <- gsub("&nf;", "", df$recordNumber)
  df$recordNumber <- gsub("^-", "", df$recordNumber)
  df$recordNumber <- gsub("-$", "", df$recordNumber)
  df$recordNumber <- gsub("CFCR-", "CFCR", df$recordNumber)

  # Deleting leading zeros
  # https://stackoverflow.com/questions/23538576/removing-leading-zeros-from-alphanumeric-characters-in-r
  tf <- !grepl("/|-", df$recordNumber)
  if (any(tf)){
    df$recordNumber[tf] <- gsub("(?<![0-9])0+", "",
                                df$recordNumber[tf], perl = TRUE)

  }

  # # Finding examples like "Harley 22573", "Hind PCD3547"
  # namesA <- grepl("[[:upper:]][[:lower:]]+\\s", df$recordNumber)
  # #df$recordNumber[namesA]
  # if (any(namesA)){
  #   df$recordNumber[namesA] <- gsub("^(\\S*\\s+)", "", df$recordNumber[namesA])
  #   df$recordNumber[namesA] <- gsub("^(\\S*\\s+)", "", df$recordNumber[namesA])
  #}

  # # Finding examples like "Harley22573"
  tf <- grepl("^[[:upper:]][[:lower:]]+([0-9]){1,}$", df$recordNumber)
  if (any(tf)){
    # Remove all letters and keep numbers
    df$recordNumber[tf] <- gsub("[^0-9.]", "", df$recordNumber[tf])
  }

  tf <- grepl("-Duplicate$", df$recordNumber)
  if (any(tf)){
    # Remove all letters and keep numbers
    df$recordNumber[tf] <- gsub("-Duplicate", "", df$recordNumber[tf])
  }

  tf <- grepl("[[:upper:]][[:lower:]]+\\s([0-9]){1,}\\s[A-Z]", df$recordNumberOriginal)
  if (any(tf)){
    df$recordNumber[tf] <- gsub("^(\\S*\\s\\S*\\s+)", "", df$recordNumberOriginal[tf])
  }

  tfa <- grepl("[0-9]\\s[A-Za-z]", df$recordNumberOriginal)
  if (any(tfa)){
    df$recordNumber[which(tfa - tf == T)] <-
      gsub("\\s", "", df$recordNumberOriginal[which(tfa - tf == T)])
  }

  tf <- grepl("^[-]$", df$recordNumber)
  if (any(tf)){
    df$recordNumber[tf] <- NA
  }

  tf <- grepl("[#][?][#]", df$recordNumber)
  if (any(tf)){
    df$recordNumber[tf] <-
      gsub("[#][?][#]", "", df$recordNumber[tf])
  }

  # cleaning examples of collection number as date
  tf <- grepl("([0-9]){1,}[/]([0-9]){1,}[/]([0-9]){1,}", df$recordNumber)
  if (any(tf)){
    df$recordNumber[tf] <- NA
  }

  tf <- grepl("[[]|[]]|[(]", df$recordNumber)
  if (any(tf)){
    df$recordNumber[tf] <- gsub("\\s.+", "", df$recordNumber[tf])
    df$recordNumber[tf] <- gsub("[[]", "", df$recordNumber[tf])
    df$recordNumber[tf] <- gsub("[]]", "", df$recordNumber[tf])
    df$recordNumber[tf] <- gsub("[(]", "", df$recordNumber[tf])
    df$recordNumber[tf] <- gsub("[)]", "", df$recordNumber[tf])
  }

  tf <- grepl("s[/]n|s[.]n[.]", df$recordNumber)
  if (any(tf)){
    df$recordNumber[tf] <- NA
  }

  # Fixing examples "Bullock, AA  712" "Heller, AA  6135"
  tf <- grepl("[[:lower:]]+[,]\\s([[:upper:]]){1,}\\s[0-9]", df$recordNumberOriginal)
  if (any(tf)){

    tfa <- grepl("[/]", df$recordNumberOriginal[tf])
    df$recordNumber[tf][tfa] <- gsub("[^0-9.-/]", "", df$recordNumberOriginal[tf][tfa])

    tfa <- grepl("[-]", df$recordNumberOriginal[tf])
    df$recordNumber[tf][tfa] <- gsub("[^0-9.-]", "", df$recordNumberOriginal[tf][tfa])
    df$recordNumber[tf][tfa] <- gsub("^[-]", "", df$recordNumber[tf][tfa])
  }

  tf <- grepl("[.]\\s|[?]|[*]|p[.]p[.]|[#]", df$recordNumber)
  if (any(tf)){
    df$recordNumber[tf] <- gsub("[.]\\s|[?]|[*]|p[.]p[.]|[#]", "", df$recordNumber[tf])
  }

  tf <- grepl("[-]{2}", df$recordNumber)
  if (any(tf)){
    df$recordNumber[tf] <- gsub("--", "-", df$recordNumber[tf])
  }

  tf <- grepl("& ", df$recordNumber)
  if (any(tf)){
    df$recordNumber[tf] <- gsub("&.*", "", df$recordNumber[tf])
  }

  tf <- grepl(",", df$recordNumber)
  if (any(tf)){
    df$recordNumber[tf] <- gsub(",.*", "", df$recordNumber[tf])
  }

  # Cleaning collection numbers that appear as dates
  # We first coerce all the columns to as.Date and see which ones succeed
  tf <- sapply(df$recordNumber, function(x) !all(is.na(as.Date(as.character(x),format="%d/%m/%Y"))))
  if (any(tf)){
    df$recordNumber[tf] <- NA
  }
  tf <- sapply(df$recordNumber, function(x) !all(is.na(as.Date(as.character(x),format="%m/%d/%Y"))))
  if (any(tf)){
    df$recordNumber[tf] <- NA
  }


  df$recordNumber[which(df$recordNumber %in% c("s"))] <- NA

  df$recordNumber <- gsub("[.]", "", df$recordNumber)


  return(df)
}
