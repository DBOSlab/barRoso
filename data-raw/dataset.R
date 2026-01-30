## code to prepare `dataset` goes here

library(readxl)

#-------------------------------------------------------------------------------
# Load each Ouratea morphological dataset
list.files("data-raw")
# Adding dataset for tests

morphological_dataset <- readxl::read_excel("data-raw/morphological_dataset.xlsx")
usethis::use_data(morphological_dataset, overwrite = TRUE)


unique(paste(morphological_dataset$Genus, morphological_dataset$Species))

names(morphological_dataset)
