# Small functions to evaluate user input for the main functions and
# return meaningful errors.
# Author: Domingos Cardoso

#_______________________________________________________________________________
# Check if the dir input is "character" type and if it has a "/" in the end
.arg_check_dir <- function(x) {
  # Check classes
  class_x <- class(x)
  if (!"character" %in% class_x) {
    stop(paste0("The argument dir should be a character, not '", class_x, "'."),
         call. = FALSE)
  }
  if (grepl("[/]$", x)) {
    x <- gsub("[/]$", "", x)
  }
  return(x)
}


#_______________________________________________________________________________
# Check if the xlsx_path input is "character" type
.arg_check_xlsx_path <- function(xlsx_path) {
  if (!is.character(xlsx_path) || length(xlsx_path) != 1L || !nzchar(xlsx_path)) {
    stop("`xlsx_path` must be a non-empty character scalar.", call. = FALSE)
  }
}
