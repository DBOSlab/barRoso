#' Fix Species Names with Global and Brazilian Taxonomic Backbones
#'
#' @author Domingos Cardoso
#'
#' @description
#' Updates misspelled names and synonyms of herbarium records into accepted names,
#' using the World Checklist of Vascular Plants (WCVP) of
#' [Plants of the World Online](https://powo.science.kew.org/) as a global
#' backbone, through the package [expowo](https://dboslab.github.io/expowo/), and
#' by default the [Flora e Funga do Brasil](https://floradobrasil.jbrj.gov.br/) (FFB),
#' through the package [floraR](https://dboslab.github.io/floraR-website/). It is
#' meant to be run after [barroso_cat()] and before [barroso_std()], so that
#' duplicates identified with synonyms or misspelled names are recognized.
#'
#' @details
#' Each unique binomial built from `genus` and `specificEpithet` is searched just
#' once, so large datasets (e.g. all records of REFLORA and JABOT) are harmonized
#' quickly. Names without a specific epithet (e.g. `"Ormosia"`, `"Ormosia sp."`) are
#' not searched.
#'
#' - **WCVP**: all names of the families in the data (and, with `flora_brasil = TRUE`,
#'   the families of their genera in FFB) are read at once with
#'   `expowo::powoSpecies(family, synonyms = TRUE)`, from the WCVP kept in the cache
#'   of expowo. Names are first matched exactly; misspelled names are matched to the
#'   closest name of the same genus, with a relative distance of the specific epithet
#'   up to `max_distance` (e.g. `"Ormosia fastigiatta"` to `"Ormosia fastigiata"`).
#'   Misspelled genera are matched in the same way. Homonyms with different accepted
#'   names (and ties between misspelled names) are left as they are, as
#'   `"ambiguous"`.
#' - **FFB** (`flora_brasil = TRUE`): the names are searched at once with
#'   `floraR::flora_search()`, which also corrects misspellings of the specific
#'   epithet. Names found in FFB follow FFB, which is preferred for the Brazilian
#'   flora, while the other names follow WCVP. A misspelling found in FFB is not
#'   taken for a name found as it is in WCVP, as names of other countries are often
#'   close to Brazilian names (e.g. `"Ormosia panamensis"` and `"Ormosia paraensis"`).
#'   The FFB dataset is downloaded once into the barRoso cache folder.
#'
#' Accepted infraspecific names are reduced to the binomial (e.g. `"Ormosia nobilis
#' var. bolivarensis"` as a synonym of `"Ormosia bolivarensis"`). When a name is
#' updated, `family`, `genus`, `specificEpithet`, and when present `species`,
#' `scientificName`, `scientificNameAuthorship` and `taxonName`, get the accepted
#' name, and `infraspecificEpithet` becomes `NA`. The family is also updated for
#' accepted names in another family (e.g. `"Leguminosae"` into `"Fabaceae"`).
#'
#' Three columns are added after `specificEpithet`:
#' - `taxonOriginal`: the binomial (or genus) before the harmonization.
#' - `taxonStatus`: `"accepted"`, `"synonym"`, `"misspelled"` (the misspelled name of
#'   an accepted name), `"misspelled synonym"`, `"ambiguous"`, `"not found"`, or `NA`
#'   for names without specific epithet.
#' - `taxonSource`: `"FFB"` or `"WCVP"`.
#'
#' The first use downloads the WCVP (by expowo) and the FFB dataset (by floraR),
#' which are then kept in cache folders for the next uses.
#'
#' @usage
#' barroso_fix_names(df,
#'                        flora_brasil = TRUE,
#'                        max_distance = 0.2,
#'                        vascular_only = FALSE,
#'                        colname_family = "family",
#'                        colname_genus = "genus",
#'                        colname_specificEpithet = "specificEpithet",
#'                        verbose = TRUE)
#'
#' @param df A data frame with herbarium records, e.g. from [barroso_cat()].
#' @param flora_brasil Logical; if `TRUE`, names found in the Flora e Funga do Brasil
#' follow FFB, and the other names follow WCVP. If `FALSE`, all names follow WCVP
#' (default: `TRUE`).
#' @param max_distance Maximum relative distance (0 to 1) between a misspelled name
#' and an accepted or synonym name (default: `0.2`).
#' @param vascular_only Logical; if `TRUE`, records of families that are not in WCVP,
#' which has only vascular plants (e.g. mosses, fungi and algae), and records
#' without family are removed (default: `FALSE`).
#' @param colname_family Column name for the family (default: `"family"`).
#' @param colname_genus Column name for the genus (default: `"genus"`).
#' @param colname_specificEpithet Column name for the specific epithet (default:
#' `"specificEpithet"`). When missing, the epithet is taken from a binomial in the
#' column `species`, as in GBIF downloads.
#' @param verbose Logical; if `TRUE`, prints messages about the progress and a summary
#' (default: `TRUE`).
#'
#' @return The data frame with accepted names and the columns `taxonOriginal`,
#' `taxonStatus` and `taxonSource`.
#'
#' @examples
#' \dontrun{
#' data_all <- barroso_cat(list(REFLORA = data_reflora, JABOT = data_jabot),
#'                         keep_source = "JABOT")
#' data_all <- barroso_fix_names(data_all, vascular_only = TRUE)
#' table(data_all$taxonStatus)
#' data_all <- barroso_std(data_all)
#' }
#'
#' @export

barroso_fix_names <- function(df,
                                   flora_brasil = TRUE,
                                   max_distance = 0.2,
                                   vascular_only = FALSE,
                                   colname_family = "family",
                                   colname_genus = "genus",
                                   colname_specificEpithet = "specificEpithet",
                                   verbose = TRUE) {

  # Check the arguments ####
  .check_df(df)
  .check_flag(flora_brasil, vascular_only, verbose)
  .check_number(max_distance, 0, 1)
  .check_colnames(colname_family = colname_family, colname_genus = colname_genus,
                  colname_specificEpithet = colname_specificEpithet)
  .check_cols(df, colname_family = colname_family, colname_genus = colname_genus)
  .check_package("expowo", repo = "DBOSlab/expowo", version = "3.2")
  if (flora_brasil) .check_package("floraR", repo = "DBOSlab/floraR")

  # Remove the columns of a previous run
  df <- df[, !names(df) %in% c("taxonOriginal", "taxonStatus", "taxonSource"), drop = FALSE]

  family <- .col_or_na(df, colname_family)
  genus <- .col_or_na(df, colname_genus)
  epithet <- .epithet(df, genus, colname_specificEpithet)
  # Not placeholders like "sp.", "cf. arborea" or "indet."
  epithet[!grepl("^[[:lower:]-]+$", epithet) | epithet %in% c("sp", "spp", "cf", "aff", "indet")] <- NA
  name <- ifelse(is.na(epithet), NA_character_, paste(genus, epithet))
  names_u <- unique(stats::na.omit(name))

  #_____________________________________________________________________________
  # FFB, searched first, as its families are also used for WCVP ####
  ffb <- NULL
  if (flora_brasil && length(names_u) > 0) {
    if (verbose) message("barroso_fix_names: searching ", length(names_u),
                         " names in the Flora e Funga do Brasil")
    ffb <- .ffb_names(names_u, max_distance)
  }

  #_____________________________________________________________________________
  # WCVP of the families in the data ####
  families <- unique(stats::na.omit(c(family, ffb$family)))
  if (verbose) message("barroso_fix_names: reading the WCVP of ", length(families),
                       " families")
  wcvp <- .wcvp_names(families)

  wcvp_match <- .match_wcvp(names_u, wcvp, max_distance)

  #_____________________________________________________________________________
  # FFB first, then WCVP ####
  res <- wcvp_match
  res$source <- ifelse(is.na(res$accepted), NA_character_, "WCVP")
  if (!is.null(ffb)) {
    i <- match(names_u, ffb$name)
    # A misspelling in FFB is not taken for a name found as it is in WCVP, as
    # names of other countries are often close to Brazilian names, e.g.
    # "Ormosia panamensis" and "Ormosia paraensis"
    exact_wcvp <- res$status %in% c("accepted", "synonym")
    ffb_misspelled <- grepl("misspelled", ffb$status[i])
    tf <- !is.na(i) & !is.na(ffb$accepted[i]) & !(ffb_misspelled & exact_wcvp)
    res[tf, c("accepted", "author", "family", "status")] <-
      ffb[i[tf], c("accepted", "author", "family", "status")]
    res$source[tf] <- "FFB"
  }

  #_____________________________________________________________________________
  # Update the records ####
  k <- match(name, res$name)
  status <- ifelse(is.na(name), NA_character_, res$status[k])
  source <- res$source[k]
  found <- !is.na(k) & !is.na(res$accepted[k])
  changed <- found & res$accepted[k] != name

  original <- ifelse(is.na(name), genus, name)
  acc <- res$accepted[k]
  acc_genus <- sub("\\s.*", "", acc)
  acc_epithet <- sub("^\\S+\\s+", "", acc)
  acc_name <- ifelse(is.na(res$author[k]) | res$author[k] %in% "", acc,
                     paste(acc, res$author[k]))

  df[[colname_genus]][changed] <- acc_genus[changed]
  if (colname_specificEpithet %in% names(df)) {
    df[[colname_specificEpithet]][changed] <- acc_epithet[changed]
  }
  if ("species" %in% names(df)) df$species[changed] <- acc[changed]
  if ("taxonName" %in% names(df)) df$taxonName[changed] <- acc[changed]
  if ("scientificName" %in% names(df)) df$scientificName[changed] <- acc_name[changed]
  if ("scientificNameAuthorship" %in% names(df)) {
    df$scientificNameAuthorship[changed] <- res$author[k][changed]
  }
  if ("infraspecificEpithet" %in% names(df)) df$infraspecificEpithet[changed] <- NA

  new_family <- found & !is.na(res$family[k]) & !family %in% res$family[k]
  df[[colname_family]][new_family] <- res$family[k][new_family]

  after <- if (colname_specificEpithet %in% names(df)) colname_specificEpithet else colname_genus
  df <- tibble::add_column(df, taxonOriginal = original, taxonStatus = status,
                           taxonSource = source, .after = after)

  if (verbose) {
    tb <- table(factor(res$status, levels = c("accepted", "synonym", "misspelled",
                                              "misspelled synonym", "ambiguous",
                                              "not found")))
    message(paste0("barroso_fix_names: ", length(names_u), " names, ",
                   paste(tb, names(tb), collapse = ", "), "; ", sum(changed),
                   " records updated (", sum(source %in% "FFB" & changed), " by FFB, ",
                   sum(source %in% "WCVP" & changed), " by WCVP)"))
  }

  #_____________________________________________________________________________
  # Vascular plants only ####
  if (vascular_only) {
    tf <- df[[colname_family]] %in% wcvp$family
    if (verbose) message("barroso_fix_names: removing ", sum(!tf),
                         " records of families not in WCVP or without family")
    df <- df[tf, , drop = FALSE]
  }

  df
}


#_______________________________________________________________________________
# Auxiliary functions of barroso_fix_names ####

# Binomial names of WCVP for some families, with their accepted names: one row
# per name and accepted name, from expowo ####
.wcvp_names <- function(families) {
  w <- lapply(families, function(f) {
    tryCatch(suppressMessages(expowo::powoSpecies(family = f, synonyms = TRUE,
                                                  verbose = FALSE)),
             error = function(e) NULL)
  })
  w <- do.call(rbind, lapply(w, function(x) {
    if (is.null(x) || nrow(x) == 0) return(NULL)
    x[, c("family", "taxon_name", "authors", "status", "accepted_name")]
  }))
  empty <- data.frame(name = character(), status = character(), accepted = character(),
                      author = character(), family = character())
  if (is.null(w)) return(empty)

  binomial <- "^(\\S+\\s+[[:lower:]-]+)(\\s.*)?$"
  acc <- w[w$status == "Accepted" & grepl("^\\S+\\s+[[:lower:]-]+$", w$taxon_name), ]
  accepted <- data.frame(name = acc$taxon_name, status = "accepted", accepted = acc$taxon_name,
                         author = acc$authors, family = acc$family)

  syn <- w[w$status != "Accepted" & grepl("^\\S+\\s+[[:lower:]-]+$", w$taxon_name) &
             grepl(binomial, w$accepted_name), ]
  syn_acc <- sub(binomial, "\\1", syn$accepted_name)
  i <- match(syn_acc, accepted$name)
  synonyms <- data.frame(name = syn$taxon_name, status = "synonym", accepted = syn_acc,
                         author = accepted$author[i],
                         family = ifelse(is.na(i), syn$family, accepted$family[i]))

  out <- unique(rbind(accepted, synonyms))
  out$genus <- sub("\\s.*", "", out$name)
  out
}

# One result per name: exact matches, then misspelled names of the same genus ####
.match_wcvp <- function(names_u, wcvp, max_distance) {
  res <- data.frame(name = names_u, status = "not found", accepted = NA_character_,
                    author = NA_character_, family = NA_character_)
  if (length(names_u) == 0 || nrow(wcvp) == 0) return(res)

  resolve <- function(rows) {
    r <- wcvp[rows, , drop = FALSE]
    if (any(r$status == "accepted")) return(r[r$status == "accepted", ][1, ])
    if (length(unique(r$accepted)) == 1) return(r[1, ])
    NULL
  }

  by_name <- split(seq_len(nrow(wcvp)), wcvp$name)
  genera <- unique(wcvp$genus)
  by_genus <- split(wcvp$name, wcvp$genus)
  by_genus <- lapply(by_genus, unique)

  for (j in seq_along(names_u)) {
    nm <- names_u[j]
    misspelled <- FALSE
    rows <- by_name[[nm]]
    if (is.null(rows)) {
      g <- sub("\\s.*", "", nm)
      ep <- sub("^\\S+\\s+", "", nm)
      if (!g %in% genera) {
        g <- .closest(g, genera, max_distance)
        if (is.na(g)) next
      }
      cand <- by_genus[[g]]
      cand_ep <- sub("^\\S+\\s+", "", cand)
      best <- .closest(ep, cand_ep, max_distance)
      if (is.na(best)) next
      rows <- by_name[[paste(g, best)]]
      misspelled <- TRUE
    }
    r <- resolve(rows)
    if (is.null(r)) {
      res$status[j] <- "ambiguous"
      next
    }
    res$accepted[j] <- r$accepted
    res$author[j] <- r$author
    res$family[j] <- r$family
    res$status[j] <- paste(c(if (misspelled) "misspelled", if (r$status == "synonym") "synonym"),
                           collapse = " ")
    if (res$status[j] == "") res$status[j] <- "accepted"
  }
  res
}

# The single closest string within a relative distance, or NA ####
.closest <- function(x, candidates, max_distance) {
  if (length(candidates) == 0) return(NA_character_)
  d <- as.vector(utils::adist(tolower(x), tolower(candidates))) / nchar(x)
  if (min(d) > max_distance) return(NA_character_)
  best <- unique(candidates[d == min(d)])
  if (length(best) != 1) return(NA_character_)
  best
}

# Names searched at once in FFB with floraR, with the accepted names, their authors
# and families from the FFB dataset, downloaded into the barRoso cache folder ####
.ffb_names <- function(names_u, max_distance) {
  cache <- tools::R_user_dir("barRoso", which = "cache")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  old <- setwd(cache)
  on.exit(setwd(old), add = TRUE)

  found <- suppressWarnings(suppressMessages(
    floraR::flora_search(names_u, version = "latest", max_distance = max_distance,
                         show_correct = TRUE, genus_fuzzy = FALSE, progress_bar = FALSE,
                         rm_flora_database = FALSE, verbose = FALSE)))
  dwca <- suppressMessages(floraR::flora_parse(path = "flora_download", version = "latest",
                                               verbose = FALSE))
  taxon <- dwca[[1L]][["data"]][["taxon.txt"]]
  taxon <- taxon[taxon$taxonomicStatus %in% "NOME_ACEITO", ]

  accepted <- sub("^(\\S+\\s+\\S+).*", "\\1", found$Accepted.taxon.Name)
  accepted[!grepl("^\\S+\\s+[[:lower:]-]+$", accepted)] <- NA
  i <- match(accepted, taxon$taxonName)
  synonym <- found$taxonomicStatus %in% "SINONIMO"
  misspelled <- !found$Correct.Spelling %in% TRUE
  status <- ifelse(synonym & misspelled, "misspelled synonym",
                   ifelse(synonym, "synonym", ifelse(misspelled, "misspelled", "accepted")))

  data.frame(name = found$Search,
             status = ifelse(is.na(accepted), "not found", status),
             accepted = accepted,
             author = ifelse(is.na(i), NA_character_, taxon$scientificNameAuthorship[i]),
             family = ifelse(is.na(i), found$family, taxon$family[i]))
}
