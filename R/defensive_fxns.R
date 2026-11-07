# Small functions to evaluate user input for the main functions and
# return meaningful errors.
# Author: Domingos Cardoso


#_______________________________________________________________________________
# A data frame (or tibble) with at least one record ####
.check_df <- function(df, arg = "df") {
  if (!is.data.frame(df)) {
    stop(paste0("`", arg, "` must be a data frame, not ",
                if (is.null(df)) "NULL" else paste0("a ", class(df)[1]), "."),
         call. = FALSE)
  }
  if (nrow(df) == 0) {
    stop(paste0("`", arg, "` has no records."), call. = FALSE)
  }
  invisible(df)
}


#_______________________________________________________________________________
# Names of columns given in the colname_* arguments, e.g.
# .check_colnames(colname_recordedBy = colname_recordedBy) ####
.check_colnames <- function(...) {
  args <- list(...)
  for (a in names(args)) {
    x <- args[[a]]
    if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) {
      stop(paste0("`", a, "` must be the name of a column, e.g. \"",
                  sub("^colname_", "", a), "\"."), call. = FALSE)
    }
  }
  invisible(TRUE)
}


#_______________________________________________________________________________
# Required columns, naming the argument to set when they have other names, e.g.
# .check_cols(df, colname_recordedBy = "collector") ####
.check_cols <- function(df, ..., arg = "df") {
  cols <- list(...)
  missing <- !unlist(cols) %in% names(df)
  if (any(missing)) {
    a <- names(cols)[missing]
    stop(paste0("Column(s) not found in `", arg, "`: ",
                paste0("'", unlist(cols)[missing], "'", collapse = ", "),
                ". If they have other names, set ",
                paste0("`", a[nzchar(a)], "`", collapse = ", "), "."),
         call. = FALSE)
  }
  invisible(TRUE)
}


#_______________________________________________________________________________
# Logical arguments as a single TRUE or FALSE, e.g. .check_flag(rm_duplicates) ####
.check_flag <- function(...) {
  args <- list(...)
  arg_names <- vapply(as.list(substitute(list(...)))[-1], deparse, character(1))
  for (i in seq_along(args)) {
    x <- args[[i]]
    if (!is.logical(x) || length(x) != 1 || is.na(x)) {
      stop(paste0("`", arg_names[i], "` must be TRUE or FALSE."), call. = FALSE)
    }
  }
  invisible(TRUE)
}


#_______________________________________________________________________________
# One of some values, e.g. .check_choice(language, c("en", "pt", "es")) ####
.check_choice <- function(x, choices, arg = deparse(substitute(x))) {
  if (!is.character(x) || length(x) != 1 || !x %in% choices) {
    stop(paste0("`", arg, "` must be one of: ", paste(choices, collapse = ", "), "."),
         call. = FALSE)
  }
  invisible(x)
}


#_______________________________________________________________________________
# A single number between limits, e.g. .check_number(max_distance, 0, 1) ####
.check_number <- function(x, min = -Inf, max = Inf, arg = deparse(substitute(x))) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < min || x > max) {
    range <- if (is.finite(min) && is.finite(max)) paste(" between", min, "and", max) else
      if (is.finite(min)) paste(" of at least", min) else
        if (is.finite(max)) paste(" of at most", max) else ""
    stop(paste0("`", arg, "` must be a number", range, "."), call. = FALSE)
  }
  invisible(x)
}


#_______________________________________________________________________________
# A single text, or NULL when allowed ####
.check_string <- function(x, null_ok = FALSE, arg = deparse(substitute(x))) {
  if (is.null(x) && null_ok) return(invisible(x))
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) {
    stop(paste0("`", arg, "` must be a single text", if (null_ok) " or NULL", "."),
         call. = FALSE)
  }
  invisible(x)
}


#_______________________________________________________________________________
# An existing file with one of some extensions, e.g. .check_file(path, "xlsx") ####
.check_file <- function(path, ext = NULL, arg = deparse(substitute(path))) {
  .check_string(path, arg = arg)
  if (!file.exists(path)) {
    stop(paste0("File not found: '", path, "'."), call. = FALSE)
  }
  if (!is.null(ext) && !tolower(tools::file_ext(path)) %in% ext) {
    stop(paste0("`", arg, "` must be a ", paste0(".", ext, collapse = " or "), " file."),
         call. = FALSE)
  }
  invisible(path)
}


#_______________________________________________________________________________
# A folder name without the final "/", or NULL when allowed ####
.check_dir <- function(dir, null_ok = FALSE, arg = deparse(substitute(dir))) {
  if (is.null(dir) && null_ok) return(dir)
  .check_string(dir, arg = arg)
  sub("[/\\\\]+$", "", dir)
}


#_______________________________________________________________________________
# Suggested packages, with how to install them, e.g.
# .check_package("expowo", repo = "DBOSlab/expowo", version = "3.2") ####
.check_package <- function(pkgs, repo = NULL, version = NULL) {
  ok <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  if (all(ok) && !is.null(version)) {
    ok <- utils::packageVersion(pkgs) >= version
  }
  if (!all(ok)) {
    missing <- pkgs[!ok]
    how <- if (is.null(repo)) {
      paste0("install.packages(c(\"", paste(missing, collapse = "\", \""), "\"))")
    } else {
      paste0("remotes::install_github(\"", repo, "\")")
    }
    stop(paste0("The package(s) ", paste(missing, collapse = ", "),
                if (!is.null(version)) paste0(" (>= ", version, ")"),
                " are needed. Install them with ", how, "."),
         call. = FALSE)
  }
  invisible(TRUE)
}
