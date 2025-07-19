#' Standardize Biodiversity Records Across Multiple Fields
#'
#' @author Domingos Cardoso
#'
#' @description
#' A wrapper function that performs integrated cleaning and standardization of
#' biodiversity collection records using the `barRoso` package. This includes
#' harmonizing taxonomic, geographic, collector, and type status information,
#' as well as flagging or removing unvouchered and duplicate specimens.
#'
#' @details
#' This function orchestrates several `std_*` functions from the `barRoso`
#' package to clean records from virtual herbaria and biodiversity portals. It
#' handles multilingual field names, missing data, inconsistent formatting,
#' and dataset chunking for large inputs. The function also detects and optionally
#' removes duplicate records and specimens lacking voucher information.
#'
#' @usage
#' barroso_std(...,
#'             unvouchered = TRUE,
#'             delunkcoll = FALSE,
#'             flag_missid = TRUE,
#'             flag_duplicates = TRUE,
#'             rm_duplicates = FALSE,
#'             colname_recordedBy = "recordedBy",
#'             colname_recordNumber = "recordNumber",
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
#' @param unvouchered Logical; if `TRUE`, remove unvouchered wood/seed/spirit specimens (default: `TRUE`).
#' @param delunkcoll Logical; if `TRUE`, removes records with unknown collectors (default: `FALSE`).
#' @param flag_missid Reserved for future use. Currently not implemented.
#' @param flag_duplicates Logical; if `TRUE`, flags duplicates with a logical column (default: `TRUE`).
#' @param rm_duplicates Logical; if `TRUE`, removes duplicate specimens (default: `FALSE`).
#' @param colname_recordedBy Column name for collector names.
#' @param colname_recordNumber Column name for collector number.
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
#' @param rm_original_column Logical; if `TRUE`, remove original columns after standardization.
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
                        colname_recordedBy = "recordedBy",
                        colname_recordNumber = "recordNumber",
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
  l_df = nrow(df)

  #_____________________________________________________________________________
  # Standardizing and cleaning the database ####
  message(paste("Standardizing", n_df, "herbarium database"))

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

    # Fill in with NAs all empty cells
    dfchunk[[i]] <- as.data.frame(sapply(dfchunk[[i]], function(x) gsub("^$", NA, x)))

    # Standardizing main collector column recordedBy####
    cleaned_df[[i]] <- std_recordedBy(df = dfchunk[[i]],
                                      colname_recordedBy = colname_recordedBy,
                                      colname_recordNumber = colname_recordNumber,
                                      rm_original_column = FALSE)

    # Standardizing herbarium acronyms within the column collectionCode ####
    # The asterisk will mark all collections without acronym from Index Herbariorum
    cleaned_df[[i]] <- std_collection(df = cleaned_df[[i]],
                                      colname_collectionCode = colname_collectionCode,
                                      colname_institutionCode = colname_institutionCode,
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
  # Remove specimen/records from mostly unvouchered wood or seed or spirit collections
  if (unvouchered) {
    df <- .delunvouchered(df,
                          colname_collectionCode = colname_collectionCode)
  }

  #_____________________________________________________________________________
  # Remove herbarium records with unknown collector ####
  if (delunkcoll) {
    if (any(df[[colname_recordedBy]] %in% c("Unknown"))) {

      unkcoll <- which(df[[colname_recordedBy]] %in% c("Unknown"))
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
                                  colname_specificEpithet = colname_specificEpithet)
  }

  if (flag_duplicates & rm_duplicates |
      flag_duplicates == FALSE & rm_duplicates) {
    df <- barroso_flag_duplicates(df,
                                  rm_duplicates = TRUE,
                                  colname_recordedBy = colname_recordedBy,
                                  colname_recordNumber = colname_recordNumber,
                                  colname_genus = colname_genus,
                                  colname_specificEpithet = colname_specificEpithet)
  }

  return(df)
}
