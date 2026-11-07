#' Standardize Biodiversity Records Across Multiple Fields
#'
#' @author Domingos Cardoso
#'
#' @description
#' A wrapper function that performs integrated cleaning and standardization of
#' biodiversity collection records using the `barRoso` package. This includes
#' harmonizing taxonomic, geographic, collector, and type status information,
#' as well as flagging or removing unvouchered and duplicate specimens. Determiner
#' names in `identifiedBy` are standardized with the same rules of collector names.
#'
#' @details
#' This function orchestrates several `std_*` functions from the `barRoso`
#' package to clean records from virtual herbaria and biodiversity portals. It
#' handles multilingual field names, missing data, inconsistent formatting,
#' and dataset chunking for large inputs. The function also detects and optionally
#' removes duplicate records and specimens lacking voucher information.
#'
#' The final data frame is sorted by species, `recordedBy` and `recordNumber`. All
#' records of a block of duplicates stay together under the name of the block, which
#' is the most frequent name among its duplicates. For example, a duplicate identified
#' as `"Ormosia ruddiana"` goes together with its three duplicates identified as
#' `"Ormosia amazonica"`, in the place of the latter species.
#'
#' @usage
#' barroso_std(...,
#'             unvouchered = TRUE,
#'             delunkcoll = FALSE,
#'             flag_missid = TRUE,
#'             flag_duplicates = TRUE,
#'             rm_duplicates = FALSE,
#'             index_herbariorum = TRUE,
#'             colname_recordedBy = "recordedBy",
#'             colname_recordNumber = "recordNumber",
#'             colname_identifiedBy = "identifiedBy",
#'             colname_continent = "continent",
#'             colname_country = "country",
#'             colname_stateProvince = "stateProvince",
#'             colname_county = "county",
#'             colname_municipality = "municipality",
#'             colname_locality = "locality",
#'             colname_collectionCode = "collectionCode",
#'             colname_institutionCode = "institutionCode",
#'             colname_typeStatus = "typeStatus",
#'             colname_family = "family",
#'             colname_genus = "genus",
#'             colname_specificEpithet = "specificEpithet",
#'             rm_original_column = TRUE)
#'
#' @param ... Input data frame containing raw biodiversity records.
#' @param unvouchered Logical; if `TRUE`, remove records of wood, seed, fruit,
#'   pollen and spirit collections (e.g. `"INPAw"`, `"INPA-Carpoteca"`), and
#'   observations without a specimen (`basisOfRecord` like `"HUMAN_OBSERVATION"`)
#'   (default: `TRUE`).
#' @param delunkcoll Logical; if `TRUE`, removes records with unknown or missing collectors (default: `FALSE`).
#' @param flag_missid Reserved for future use. Currently not implemented.
#' @param flag_duplicates Logical; if `TRUE`, flags duplicates with a logical column (default: `TRUE`).
#' @param rm_duplicates Logical; if `TRUE`, removes duplicate specimens (default: `FALSE`).
#' @param index_herbariorum Logical; if `TRUE`, checks collection codes against the API
#' of Index Herbariorum, always keeping the original codes. See
#' [std_collection()]. Use `FALSE` to work offline (default: `TRUE`).
#' @param colname_recordedBy Column name for collector names.
#' @param colname_recordNumber Column name for collector number.
#' @param colname_identifiedBy Column name for determiner names. The column is
#' standardized with [std_identifiedBy()] only when present in the data.
#' @param colname_collectionCode Column name for collection code.
#' @param colname_continent Column name for continent.
#' @param colname_country Column name for country.
#' @param colname_stateProvince Column name for state/province.
#' @param colname_county Column name for county.
#' @param colname_municipality Column name for municipality.
#' @param colname_locality Column name for locality.
#' @param colname_institutionCode Column name for institution code.
#' @param colname_typeStatus Column name for type status.
#' @param colname_family Column name for family.
#' @param colname_genus Column name for genus.
#' @param colname_specificEpithet Column name for specific epithet.
#' @param rm_original_column Logical; if `TRUE`, remove original columns after standardization,
#' except that of collection codes when `index_herbariorum = TRUE`.
#'
#' @return A fully cleaned and standardized data frame ready for downstream reconciliation,
#' duplicate handling, and label generation.
#'
#' @examples
#' \dontrun{
#' df <- read.csv("raw_herbarium_data.csv")
#' df_std <- barroso_std(df,
#'                       colname_country = "pais",
#'                       colname_stateProvince = "estado",
#'                       rm_duplicates = TRUE)
#' }
#'
#' @importFrom dplyr bind_rows
#' @importFrom tibble add_column
#' @importFrom magrittr %>%
#'
#' @export

barroso_std <- function(...,
                        unvouchered = TRUE,
                        delunkcoll = FALSE,
                        flag_missid = TRUE,
                        flag_duplicates = TRUE,
                        rm_duplicates = FALSE,
                        index_herbariorum = TRUE,
                        colname_recordedBy = "recordedBy",
                        colname_recordNumber = "recordNumber",
                        colname_identifiedBy = "identifiedBy",
                        colname_continent = "continent",
                        colname_country = "country",
                        colname_stateProvince = "stateProvince",
                        colname_county = "county",
                        colname_municipality = "municipality",
                        colname_locality = "locality",
                        colname_collectionCode = "collectionCode",
                        colname_institutionCode = "institutionCode",
                        colname_typeStatus = "typeStatus",
                        colname_family = "family",
                        colname_genus = "genus",
                        colname_specificEpithet = "specificEpithet",
                        rm_original_column = TRUE) {

  # Get the name of the input dataset
  namedsource <- .namedherbsource(...)
  df <- namedsource[[1]]
  n_df = names(namedsource[1])

  # Check the arguments ####
  .check_df(df)
  .check_flag(unvouchered, delunkcoll, flag_missid, flag_duplicates, rm_duplicates,
              index_herbariorum, rm_original_column)
  .check_colnames(colname_recordedBy = colname_recordedBy,
                  colname_recordNumber = colname_recordNumber,
                  colname_identifiedBy = colname_identifiedBy,
                  colname_continent = colname_continent,
                  colname_country = colname_country,
                  colname_stateProvince = colname_stateProvince,
                  colname_county = colname_county,
                  colname_municipality = colname_municipality,
                  colname_locality = colname_locality,
                  colname_collectionCode = colname_collectionCode,
                  colname_institutionCode = colname_institutionCode,
                  colname_typeStatus = colname_typeStatus,
                  colname_family = colname_family,
                  colname_genus = colname_genus,
                  colname_specificEpithet = colname_specificEpithet)
  .check_cols(df, colname_recordedBy = colname_recordedBy,
              colname_recordNumber = colname_recordNumber)
  l_df = nrow(df)

  #_____________________________________________________________________________
  # Standardizing and cleaning the database ####
  message(paste("Standardizing", n_df, "herbarium database"))

  # Remove records of wood, seed, fruit or spirit collections, and observations,
  # before the codes like "INPA-Carpoteca" are standardized into "INPA"
  if (unvouchered) {
    df <- .delunvouchered(df,
                          colname_collectionCode = colname_collectionCode)
    l_df <- nrow(df)
  }

  # Divide too large entered database
  if (l_df > 10000) {
    chunk <- 1000
    r  <- rep(1:ceiling(l_df/chunk), each=chunk)[1:l_df]
    dfchunk <- split(df, r)
  } else {
    dfchunk <- list(df)
  }

  l_dfchunk = length(dfchunk)
  cleaned_df <- list()
  for (i in 1:l_dfchunk) {
    if (l_dfchunk > 1) {
      message(paste("Chunking", paste(n_df, ":", sep=""), i, "of", l_dfchunk))
    }

    # Fill in with NAs all empty cells, and cells with the text "NA", as in
    # spreadsheets exported from R, except in code columns, where "NA" can be
    # a real code (e.g. countryCode of Namibia, or herbarium acronym)
    # lapply keeps the data frame even for a chunk of a single record
    na_text <- !grepl("Code$", names(dfchunk[[i]]))
    dfchunk[[i]] <- as.data.frame(Map(function(x, na_text) {
      x <- as.character(x)
      x[trimws(x) %in% c("", if (na_text) "NA")] <- NA
      x
    }, dfchunk[[i]], na_text), check.names = FALSE)

    # Standardizing main collector column recordedBy####
    cleaned_df[[i]] <- std_recordedBy(df = dfchunk[[i]],
                                      colname_recordedBy = colname_recordedBy,
                                      colname_recordNumber = colname_recordNumber,
                                      rm_original_column = FALSE)

    # Standardizing determiner column identifiedBy ####
    if (colname_identifiedBy %in% names(cleaned_df[[i]])) {
      cleaned_df[[i]] <- std_identifiedBy(df = cleaned_df[[i]],
                                          colname_identifiedBy = colname_identifiedBy,
                                          rm_original_column = FALSE)
    }

    # Standardizing herbarium acronyms within the column collectionCode ####
    # The asterisk will mark all collections without acronym from Index Herbariorum
    cleaned_df[[i]] <- std_collection(df = cleaned_df[[i]],
                                      colname_collectionCode = colname_collectionCode,
                                      colname_institutionCode = colname_institutionCode,
                                      index_herbariorum = index_herbariorum,
                                      rm_original_column = rm_original_column)

    # Standardizing place-related columns ####
    # continent, country, stateProvince, county and municipality
    cleaned_df[[i]] <- std_place(df = cleaned_df[[i]],
                                 colname_continent = colname_continent,
                                 colname_country = colname_country,
                                 colname_stateProvince = colname_stateProvince,
                                 colname_county = colname_county,
                                 colname_municipality = colname_municipality,
                                 colname_locality = colname_locality,
                                 rm_original_column = rm_original_column)

    # Cleaning taxa columns: family, genus, species ####
    cleaned_df[[i]] <- std_taxa(df = cleaned_df[[i]],
                                colname_family = colname_family,
                                colname_genus = colname_genus,
                                colname_specificEpithet = colname_specificEpithet,
                                rm_original_column = rm_original_column)

  }

  # Combine list of dataframes even when they differ in number of columns
  df <- dplyr::bind_rows(cleaned_df)

  #_____________________________________________________________________________
  # Standardize information at typeStatus ####
  df <- std_types(df = df,
                  colname_typeStatus = colname_typeStatus,
                  rm_original_column = rm_original_column)


  #_____________________________________________________________________________
  # Remove herbarium records with unknown collector ####
  if (delunkcoll) {
    if (any(df[[colname_recordedBy]] %in% c("Unknown", NA))) {

      unkcoll <- which(df[[colname_recordedBy]] %in% c("Unknown", NA))
      df <- df[-unkcoll, ]
    }
  }

  #_____________________________________________________________________________
  # Flag and remove duplicate specimens ####
  if (flag_duplicates & rm_duplicates == FALSE) {
    df <- barroso_flag_duplicates(df,
                                  rm_duplicates = FALSE,
                                  colname_recordedBy = colname_recordedBy,
                                  colname_recordNumber = colname_recordNumber,
                                  colname_genus = colname_genus,
                                  colname_specificEpithet = colname_specificEpithet,
                                  colname_collectionCode = colname_collectionCode)
  }

  if (flag_duplicates & rm_duplicates |
      flag_duplicates == FALSE & rm_duplicates) {
    df <- barroso_flag_duplicates(df,
                                  rm_duplicates = TRUE,
                                  colname_recordedBy = colname_recordedBy,
                                  colname_recordNumber = colname_recordNumber,
                                  colname_genus = colname_genus,
                                  colname_specificEpithet = colname_specificEpithet,
                                  colname_collectionCode = colname_collectionCode)
  }

  #_____________________________________________________________________________
  # Sort by species, collector and collector number ####
  df <- .sort_records(df,
                      colname_recordedBy = colname_recordedBy,
                      colname_recordNumber = colname_recordNumber,
                      colname_genus = colname_genus,
                      colname_specificEpithet = colname_specificEpithet)

  return(df)
}


#_______________________________________________________________________________
# Sort by species, collector and collector number (as a number when possible).
# All records of a block of duplicates go together under the name of the block,
# chosen as in barroso_flag_duplicates(): a name with specific epithet, then the
# most frequent one, then the first one in alphabetical order ####
.sort_records <- function(df,
                          colname_recordedBy = "recordedBy",
                          colname_recordNumber = "recordNumber",
                          colname_genus = "genus",
                          colname_specificEpithet = "specificEpithet") {

  genus <- .col_or_na(df, colname_genus)
  epithet <- .epithet(df, genus, colname_specificEpithet)
  species <- ifelse(is.na(genus), .col_or_na(df, "species"),
                    ifelse(is.na(epithet), genus, paste(genus, epithet)))

  block_species <- species
  if ("duplicateGroup" %in% names(df)) {
    group <- df$duplicateGroup
    for (i in split(seq_len(nrow(df)), group)) {
      names_i <- species[i][!is.na(species[i])]
      complete <- species[i][!is.na(species[i]) & !is.na(epithet[i])]
      if (length(complete) > 0) names_i <- complete
      if (length(names_i) > 0) {
        freq <- table(names_i)
        block_species[i] <- names(freq)[which.max(freq)]
      }
    }
  } else {
    group <- rep(NA_integer_, nrow(df))
  }

  number <- .col_or_na(df, colname_recordNumber)
  number_value <- suppressWarnings(as.numeric(gsub("\\D.*$", "", number)))

  df <- df[order(block_species,
                 .col_or_na(df, colname_recordedBy),
                 number_value,
                 number,
                 group,
                 species != block_species), , drop = FALSE]
  rownames(df) <- NULL
  df
}
