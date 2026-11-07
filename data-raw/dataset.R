## Code to prepare the example dataset `chars_dataset`

library(readxl)

#-------------------------------------------------------------------------------
# Morphological data of 33 specimens of 15 Ouratea species, taken from the
# spreadsheet Descricoes_Ouratea_Diagonal_Seca_Brasil__ORIGINAL.xlsx. Several
# measurements of the same specimen are separated by "|", e.g. "4.3 | 5 | 6.5 | 10.5".
# Counts are in their own columns, with just numbers: the pairs of secondary veins
# were taken from "LEAF SECONDARY-VENATION" (e.g. "6 - 12 pairs, curved distally
# towards the marginal vein") into "LEAF SECONDARY-VENATION pairs" ("6 | 12"), and
# "INFLORESCENCE flower number" ("with around 70 flowers") is now
# "INFLORESCENCE flowers" ("70")

chars_dataset <- readxl::read_excel("data-raw/chars_dataset.xlsx", col_types = "text")
usethis::use_data(chars_dataset, overwrite = TRUE)

unique(paste(chars_dataset$Genus, chars_dataset$Species))
names(chars_dataset)
