# Small functions to add new records to a database already standardized by
# barroso_std(), used with its argument `database`.
# Author: Domingos Cardoso


# Columns added by barroso_flag_duplicates() ####
.duplicate_columns <- c("duplicate", "duplicateGroup", "identificationConflict",
                        "identificationUnresolved", "duplicateIdentifications",
                        "duplicateCollectionCodes", "duplicateCatalogNumbers")

# Columns with an identifier of each record in the source databases, e.g.
# gbifID of GBIF and occurrenceID of REFLORA and JABOT ####
.id_columns <- c("gbifID", "occurrenceID")


#_______________________________________________________________________________
# A database as a data frame, or read from an .xlsx, .xls, .csv or .rds file ####
.read_database <- function(database) {
  if (is.character(database)) {
    .check_file(database, c("xlsx", "xls", "csv", "rds"), arg = "database")
    ext <- tolower(tools::file_ext(database))
    database <- switch(ext,
                       rds = readRDS(database),
                       csv = utils::read.csv(database, check.names = FALSE,
                                             stringsAsFactors = FALSE, encoding = "UTF-8"),
                       readxl::read_excel(database, guess_max = 1e6))
  }
  if (!is.data.frame(database)) {
    stop("`database` must be a data frame or the path to an .xlsx, .csv or .rds file.",
         call. = FALSE)
  }
  database <- as.data.frame(database, check.names = FALSE)
  message(paste0("barroso_std: database with ", nrow(database), " records"))
  database
}


#_______________________________________________________________________________
# Identifiers as text, e.g. 1234567890 (numbers read from spreadsheets) into
# "1234567890", not "1.234568e+09" ####
.id_text <- function(x) {
  if (is.numeric(x)) x <- format(x, scientific = FALSE, trim = TRUE)
  x <- trimws(as.character(x))
  x[x %in% c("", "NA")] <- NA
  x
}

# Keys of the identifiers of each record, by datasource when both have it ####
.id_keys <- function(df, col, by_source) {
  id <- .id_text(df[[col]])
  source <- if (by_source) .col_or_na(df, "datasource") else ""
  ifelse(is.na(id), NA, paste(source, col, id, sep = "\r"))
}


#_______________________________________________________________________________
# New records already in the database, with the same gbifID or occurrenceID of
# the same datasource ####
.rm_by_id <- function(df, database) {
  cols <- intersect(.id_columns, intersect(names(df), names(database)))
  if (length(cols) == 0) {
    message(paste("barroso_std: no identifier columns (gbifID or occurrenceID) in both the new",
                  "records and the database, so they are compared by their catalog numbers"))
    return(df)
  }
  by_source <- "datasource" %in% names(df) && "datasource" %in% names(database)
  known <- rep(FALSE, nrow(df))
  for (col in cols) {
    new <- .id_keys(df, col, by_source)
    known <- known | (!is.na(new) & new %in% .id_keys(database, col, by_source))
  }
  # Records without identifier, compared by their catalog numbers below
  no_id <- Reduce(`&`, lapply(cols, function(col) is.na(.id_text(df[[col]]))))
  message(paste0("barroso_std: ", sum(known), " of ", nrow(df), " new records already in ",
                 "the database (same ", paste(cols, collapse = " or "),
                 if (by_source) " of the same datasource", ") removed",
                 if (any(no_id)) paste0("; ", sum(no_id), " new records without identifier are ",
                                        "compared by their catalog numbers")))
  df[!known, , drop = FALSE]
}


#_______________________________________________________________________________
# Herbarium sheets as keys, e.g. "MO 2839102", "MO2839102" and "MO-2839102" into
# "MO2839102" ####
.sheet_key <- function(x) {
  x <- toupper(gsub("[^[:alnum:]]", "", x))
  x[x %in% ""] <- NA
  x
}

# Herbarium sheets of the database, in catalogNumber and among the duplicates of
# each record in duplicateCatalogNumbers, e.g. "MO 2839102 | JBB 13548" ####
.database_sheets <- function(database, colname_collectionCode) {
  own <- .code_catalog(.col_or_na(database, colname_collectionCode),
                       .col_or_na(database, "catalogNumber"))
  own[is.na(.col_or_na(database, "catalogNumber"))] <- NA
  dups <- unlist(strsplit(stats::na.omit(.col_or_na(database, "duplicateCatalogNumbers")),
                          "\\s*[|]\\s*"))
  # Herbaria without catalog numbers, e.g. "K", are not sheets
  dups <- dups[grepl("[0-9]", dups)]
  keys <- .sheet_key(c(own, dups))
  unique(keys[!is.na(keys)])
}

# New records of herbarium sheets already in the database: by their catalog
# numbers or, without catalog number, when all their standardized fields are the
# same of a record of the database (herbarium, collector, number, name, date and
# place), e.g. "E. Ule s.n. (US)" of Ormosia arborea, without date ####
.rm_by_catalog <- function(df, database, colname_collectionCode,
                           fields = c("recordedBy", "recordNumber", "genus", "specificEpithet",
                                      "year", "month", "day", "country", "stateProvince",
                                      "municipality", "locality")) {
  catalog <- .col_or_na(df, "catalogNumber")
  key <- .sheet_key(.code_catalog(.col_or_na(df, colname_collectionCode), catalog))
  by_catalog <- !is.na(catalog) & !is.na(key) &
    key %in% .database_sheets(database, colname_collectionCode)
  fingerprint <- function(d) {
    v <- lapply(c(colname_collectionCode, fields), function(col) tolower(.ascii_squish(.col_or_na(d, col))))
    do.call(paste, c(v, sep = "\r"))
  }
  by_fields <- !by_catalog & is.na(catalog) & fingerprint(df) %in% fingerprint(database)
  message(paste0("barroso_std: ", sum(by_catalog | by_fields), " of ", nrow(df), " new records of ",
                 "herbarium sheets already in the database removed (", sum(by_catalog),
                 " by catalogNumber or duplicateCatalogNumbers, ", sum(by_fields),
                 " without catalog number, with the same fields)"))
  df[!(by_catalog | by_fields), , drop = FALSE]
}


#_______________________________________________________________________________
# New records added to the database. Duplicates of records of the database (same
# collector and number) are added to their duplicate columns and, with
# rm_duplicates, removed ####
.merge_database <- function(df, database, rm_duplicates,
                            colname_recordedBy = "recordedBy",
                            colname_recordNumber = "recordNumber",
                            colname_genus = "genus",
                            colname_specificEpithet = "specificEpithet",
                            colname_collectionCode = "collectionCode",
                            colname_municipality = "municipality",
                            match_collectors = TRUE) {

  names_of <- function(d) {
    genus <- .col_or_na(d, colname_genus)
    epithet <- .epithet(d, genus, colname_specificEpithet)
    list(genus = genus, epithet = epithet,
         species = ifelse(is.na(genus), NA, ifelse(is.na(epithet), genus, paste(genus, epithet))))
  }
  sp_new <- names_of(df)
  sp_db <- names_of(database)

  # Collectors written in other ways in the database and in the new records, e.g.
  # "L. Queiroz" and "L. P. Queiroz" with the same number and date: the new records
  # get the most complete name, but the names of the database are not changed. To
  # find their duplicates, both are compared with their most complete names
  coll <- c(.col_or_na(database, colname_recordedBy), .col_or_na(df, colname_recordedBy))
  if (match_collectors) {
    both <- .bind_mixed(list(database[, intersect(c("year", "month", "day", colname_municipality),
                                                    names(database)), drop = FALSE],
                             df[, intersect(c("year", "month", "day", colname_municipality),
                                            names(df)), drop = FALSE]))
    args <- list(c(.col_or_na(database, colname_recordNumber), .col_or_na(df, colname_recordNumber)),
                 .col_or_na(both, "year"), .col_or_na(both, "month"), .col_or_na(both, "day"),
                 .col_or_na(both, colname_municipality))
    in_db <- seq_len(nrow(database))
    fixed <- do.call(.match_collectors, c(list(coll), args,
                                         list(fixed = seq_along(coll) %in% in_db)))
    if (any(attr(fixed, "changed"))) {
      new_names <- as.character(fixed)[-in_db]
      changed <- attr(fixed, "changed")[-in_db]
      df[[colname_recordedBy]][changed] <- new_names[changed]
      message(paste0("barroso_std: ", sum(changed), " new records got the most complete name of ",
                     "their collectors in the database, e.g. ",
                     paste(utils::head(attr(fixed, "examples"), 3), collapse = ", ")))
    }
    coll <- as.character(do.call(.match_collectors, c(list(coll), args)))
  }
  key_db <- .duplicate_key(database, coll[seq_len(nrow(database))],
                           .col_or_na(database, colname_recordNumber), sp_db$species)
  key_new <- .duplicate_key(df, coll[-seq_len(nrow(database))],
                            .col_or_na(df, colname_recordNumber), sp_new$species)

  # The duplicate columns in both, as in barroso_flag_duplicates()
  for (col in .duplicate_columns) {
    empty <- if (col %in% c("duplicate", "identificationConflict", "identificationUnresolved")) NA else
      if (col == "duplicateGroup") NA_real_ else NA_character_
    if (!col %in% names(database)) database[[col]] <- rep(empty, nrow(database))
    if (!col %in% names(df)) df[[col]] <- rep(empty, nrow(df))
  }
  database$duplicate <- as.logical(database$duplicate)
  database$identificationConflict <- as.logical(database$identificationConflict)
  database$duplicateGroup <- suppressWarnings(as.numeric(database$duplicateGroup))
  df$duplicateGroup <- suppressWarnings(as.numeric(df$duplicateGroup))

  # Blocks of the new records numbered after those of the database
  last <- max(c(0, database$duplicateGroup), na.rm = TRUE)
  df$duplicateGroup <- df$duplicateGroup + last
  last <- max(c(last, df$duplicateGroup), na.rm = TRUE)

  split_bar <- function(x) unlist(strsplit(as.character(x[!is.na(x)]), "\\s*[|]\\s*"))
  collapse <- function(x) {
    x <- unique(x[!is.na(x) & nzchar(x)])
    if (length(x) == 0) NA_character_ else paste(x, collapse = " | ")
  }
  code_new <- .col_or_na(df, colname_collectionCode)
  code_db <- .col_or_na(database, colname_collectionCode)
  cat_new <- .code_catalog(code_new, .col_or_na(df, "catalogNumber"))
  cat_db <- .code_catalog(code_db, .col_or_na(database, "catalogNumber"))

  in_db <- !is.na(key_new) & key_new %in% key_db
  for (k in unique(key_new[in_db])) {
    d <- which(key_db == k)
    n <- which(key_new == k)
    ids <- c(sp_db$species[d], split_bar(database$duplicateIdentifications[d]),
             sp_new$species[n], split_bar(df$duplicateIdentifications[n]))
    codes <- c(code_db[d], split_bar(database$duplicateCollectionCodes[d]),
               code_new[n], split_bar(df$duplicateCollectionCodes[n]))
    cats <- c(cat_db[d], split_bar(database$duplicateCatalogNumbers[d]),
              cat_new[n], split_bar(df$duplicateCatalogNumbers[n]))
    # A conflict when the new duplicates have other complete names or genera
    complete <- unique(c(sp_db$species[d][!is.na(sp_db$epithet[d])],
                         sp_new$species[n][!is.na(sp_new$epithet[n])]))
    genera <- unique(stats::na.omit(c(sp_db$genus[d], sp_new$genus[n])))
    conflict <- length(complete) > 1 || length(genera) > 1 ||
      any(database$identificationConflict[d] %in% TRUE)
    group <- database$duplicateGroup[d][!is.na(database$duplicateGroup[d])][1]
    if (is.na(group)) {
      last <- last + 1
      group <- last
    }
    rows <- list(database = d, df = if (!rm_duplicates) n else integer(0))
    for (w in names(rows)) {
      i <- rows[[w]]
      if (length(i) == 0) next
      x <- if (w == "database") database else df
      x$duplicate[i] <- TRUE
      x$duplicateGroup[i] <- group
      x$identificationConflict[i] <- conflict
      x$duplicateIdentifications[i] <- collapse(ids)
      x$duplicateCollectionCodes[i] <- collapse(codes)
      x$duplicateCatalogNumbers[i] <- collapse(cats)
      if (w == "database") database <- x else df <- x
    }
  }
  if (any(in_db)) {
    message(paste0("barroso_std: ", sum(in_db), " new records are duplicates of ",
                   length(unique(key_new[in_db])), " records of the database",
                   if (rm_duplicates) ", added to their duplicate columns and removed" else
                     ", flagged with them"))
  }
  if (rm_duplicates) df <- df[!in_db, , drop = FALSE]
  message(paste0("barroso_std: ", nrow(df), " new records added to the database of ",
                 nrow(database), " records (newRecord == TRUE)"))

  database$newRecord <- FALSE
  df$newRecord <- rep(TRUE, nrow(df))
  .bind_mixed(list(database, df))
}


#_______________________________________________________________________________
# Bind data frames whose columns have different types, e.g. numeric year in one
# and text in another, which are combined as text ####
.bind_mixed <- function(list_df, .id = NULL) {
  type <- function(x) if (is.numeric(x)) "numeric" else class(x)[1]
  types <- lapply(list_df, function(df) vapply(df, type, character(1)))
  all_types <- split(unlist(types, use.names = FALSE),
                     unlist(lapply(types, names), use.names = FALSE))
  mixed <- names(all_types)[vapply(all_types, function(x) length(unique(x)) > 1, logical(1))]
  list_df <- lapply(list_df, function(df) {
    cols <- intersect(mixed, names(df))
    df[cols] <- lapply(df[cols], as.character)
    df
  })
  dplyr::bind_rows(list_df, .id = .id)
}

# The database as it is, when all new records are already in it ####
.no_new_records <- function(database) {
  message("barroso_std: no new records to add to the database")
  database$newRecord <- rep(FALSE, nrow(database))
  database
}

