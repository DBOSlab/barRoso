# Deep search across the whole database for the same collector written in
# different ways, e.g. "L. Queiroz" and "L. P. Queiroz", or "A. Delgado-Salinas",
# "A. Delgado Salinas" and "A. D. Salinas", used by barroso_std() before finding
# duplicates.
# Author: Domingos Cardoso


#_______________________________________________________________________________
# Initials and surname of standardized collector names, temporarily in the same
# form, e.g. "L. P. Queiroz" into "L P" and "queiroz", and "A. Delgado-Salinas",
# "A. Delgado Salinas" and "A. D. Salinas" all into "A D" and "salinas": the
# first surnames of compound surnames become initials, and particles like "de",
# "von" are dropped. Names without initials, e.g. teams, get NA ####
.collector_parts <- function(x) {
  parts <- lapply(strsplit(.ascii_squish(x), "\\s+"), function(w) {
    w <- w[!is.na(w) & nzchar(w)]
    initial <- grepl("^(\\p{Lu}[.])+(-\\p{Lu}[.]?)?$", w, perl = TRUE)
    if (!any(initial) || all(initial)) return(NULL)
    first_surname <- min(which(!initial))
    initials <- w[seq_len(first_surname - 1)]
    # Initials after the surname are not expected in standardized names
    if (length(initials) == 0) return(NULL)
    surname <- w[first_surname:length(w)]
    surname <- surname[!grepl("^\\p{Ll}+$", surname, perl = TRUE)]
    # Not names, e.g. "P. E. E. Plantae Portoricenses Sintenis" or "M. l. AG. Olivera"
    if (length(surname) == 0 || length(surname) > 2 ||
        !all(grepl("^\\p{Lu}\\p{Ll}+(-\\p{Lu}\\p{Ll}+)*$", surname, perl = TRUE))) return(NULL)
    surname <- unlist(strsplit(surname, "-"))
    written <- toupper(unlist(regmatches(initials, gregexpr("\\p{Lu}", initials, perl = TRUE))))
    letters <- c(written, toupper(substr(surname[-length(surname)], 1, 1)))
    list(initials = letters, written = written, surname = tolower(surname[length(surname)]))
  })
  parts
}

# The initials of one name fit those of another, with the same first initial and
# the others in the same order, e.g. "L" in "L P", "D S" in "D B O S", but not
# "L M" in "L P" ####
.initials_fit <- function(short, long) {
  if (length(short) > length(long) || short[1] != long[1]) return(FALSE)
  pos <- 1
  for (s in short[-1]) {
    k <- match(s, long[-seq_len(pos)])
    if (is.na(k)) return(FALSE)
    pos <- pos + k
  }
  TRUE
}


#_______________________________________________________________________________
# The names of collectors written in different ways, with the same first initial,
# surname and collector number, and the same date or municipality, become the
# most complete name. Returns the names, with the attribute "changed". With
# `fixed`, those records are not changed, but their names can be taken by the
# others, e.g. the records of a database ####
.match_collectors <- function(coll, num, year, month, day, municipality,
                              fixed = rep(FALSE, length(coll))) {
  n <- length(coll)
  changed <- rep(FALSE, n)
  out <- structure(coll, changed = changed, examples = character(0), ambiguous = 0)
  ok <- !is.na(coll) & !is.na(num) & !coll %in% "Unknown"
  if (!any(ok)) return(out)

  names_u <- unique(coll[ok])
  parts <- .collector_parts(names_u)
  has_parts <- !vapply(parts, is.null, logical(1))
  if (!any(has_parts)) return(out)
  first <- surname <- rep(NA_character_, length(names_u))
  first[has_parts] <- vapply(parts[has_parts], function(p) p$initials[1], character(1))
  surname[has_parts] <- vapply(parts[has_parts], `[[`, character(1), "surname")

  idx <- match(coll, names_u)
  key <- ifelse(ok & has_parts[idx],
                paste(first[idx], surname[idx], tolower(.ascii_squish(num)), sep = "\r"), NA)
  date <- ifelse(is.na(year) | is.na(month) | is.na(day), NA,
                 paste(year, month, day, sep = "-"))
  muni <- tolower(.ascii_squish(municipality))
  # Frequency of each name, with its spellings without accents, e.g. "F. Falcon"
  # and "F. Falcón"
  folded <- .ascii_squish(coll[ok])
  freq_folded <- table(folded)

  examples <- character(0)
  ambiguous <- 0
  for (rows in split(which(!is.na(key)), key[!is.na(key)])) {
    forms <- unique(coll[rows])
    if (length(forms) < 2) next
    # The most complete form first: more initials, then, for the same initials
    # written in other ways (e.g. "A. Delgado-Salinas" and "A. D. Salinas"), the
    # most frequent in the whole data, then the spelling with accents (e.g.
    # "F. Falcón" rather than "F. Falcon"), then the longest
    init <- lapply(forms, function(f) parts[[match(f, names_u)]]$initials)
    written <- lapply(forms, function(f) parts[[match(f, names_u)]]$written)
    accents <- nchar(forms) - nchar(gsub("[^\\x01-\\x7f]", "", forms, perl = TRUE))
    o <- order(-lengths(init), -as.numeric(freq_folded[.ascii_squish(forms)]), -accents,
               -nchar(forms))
    forms <- forms[o]
    init <- init[o]
    written <- written[o]
    # A name gets more initials only from initials written as such, e.g. "L." into
    # "L. P.", but not "D. Cardoso" into "D. Queiroz Cardoso", or the same initials
    # written in other ways, e.g. "A. D. Salinas" and "A. Delgado-Salinas"
    can_take <- function(k, g) {
      .initials_fit(init[[k]], init[[g]]) &&
        (identical(init[[k]], init[[g]]) ||
           (length(written[[g]]) > length(written[[k]]) &&
              .initials_fit(written[[k]], written[[g]])))
    }
    for (k in seq_along(forms)[-1]) {
      for (r in rows[coll[rows] == forms[k] & !fixed[rows]]) {
        # More complete forms with the initials of this one, and a record with the
        # same date or municipality
        cand <- which(vapply(seq_len(k - 1), function(g) {
          g_rows <- rows[coll[rows] == forms[g]]
          can_take(k, g) &&
            any((!is.na(date[r]) & date[g_rows] %in% date[r]) |
                  (!is.na(muni[r]) & muni[g_rows] %in% muni[r]))
        }, logical(1)))
        if (length(cand) == 0) next
        # Different people, e.g. "L. P. Queiroz" and "L. M. Queiroz" for "L. Queiroz":
        # the name cannot be chosen
        best <- cand[1]
        if (!all(vapply(cand, function(g) .initials_fit(init[[g]], init[[best]]) &&
                        .initials_fit(written[[g]], written[[best]]), logical(1)))) {
          ambiguous <- ambiguous + 1
          next
        }
        out[r] <- forms[best]
        changed[r] <- TRUE
        examples <- c(examples, paste(forms[k], "\u2192", forms[best]))
      }
    }
  }
  attr(out, "ambiguous") <- ambiguous
  attr(out, "changed") <- changed
  attr(out, "examples") <- unique(examples)
  out
}


#_______________________________________________________________________________
# Deep search in a data frame, with a message ####
.match_collectors_df <- function(df, colname_recordedBy = "recordedBy",
                                 colname_recordNumber = "recordNumber",
                                 colname_municipality = "municipality",
                                 fixed = rep(FALSE, nrow(df))) {
  if (nrow(df) == 0 || !colname_recordedBy %in% names(df)) return(df)
  matched <- .match_collectors(.col_or_na(df, colname_recordedBy),
                               .col_or_na(df, colname_recordNumber),
                               .col_or_na(df, "year"), .col_or_na(df, "month"),
                               .col_or_na(df, "day"), .col_or_na(df, colname_municipality),
                               fixed = fixed)
  changed <- attr(matched, "changed")
  if (any(changed)) {
    df[[colname_recordedBy]][changed] <- as.character(matched)[changed]
    ex <- attr(matched, "examples")
    message(paste0("barroso_std: ", sum(changed), " records of collectors written in other ",
                   "ways got their most complete name, as they have the same first initial, ",
                   "surname and number, and the same date or municipality, e.g. ",
                   paste(utils::head(ex, 3), collapse = ", ")))
  }
  if (attr(matched, "ambiguous") > 0) {
    message(paste0("barroso_std: ", attr(matched, "ambiguous"), " records kept with their ",
                   "names, as they match the names of different collectors, e.g. ",
                   "\"L. Queiroz\" with \"L. P. Queiroz\" and \"L. M. Queiroz\""))
  }
  df
}
