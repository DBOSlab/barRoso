#' @name morphological_dataset
#'
#' @docType data
#'
#' @title
#' Morphological Data for \emph{Ouratea} Species (Ochnaceae)
#'
#' @description
#' A comprehensive morphological dataset containing detailed taxonomic,
#' geographical, and morphological information for 15 species of the
#' genus \emph{Ouratea} (family Ochnaceae). This dataset was compiled
#' for taxonomic revision and description generation purposes.
#'
#' The dataset includes 131 variables covering taxonomic identification,
#' collection details, geographical information, and morphological
#' characters across different plant organs (habit, leaves, stipules,
#' inflorescences, flowers, fruits, and seeds).
#'
#' @format
#' A data frame with 122 observations (specimens) and 131 variables:
#' \describe{
#'   \item{\code{Genus}}{Character. Genus name (all "Ouratea").}
#'   \item{\code{Species}}{Character. Species epithet.}
#'   \item{\code{Author}}{Character. Taxonomic authority.}
#'   \item{\code{Collector}}{Character. Name of primary collector.}
#'   \item{\code{et al.}}{Character. Additional collectors, if any.}
#'   \item{\code{Collection number}}{Character. Collector's number.}
#'   \item{\code{Collection date}}{Character. Date of collection.}
#'   \item{\code{Type}}{Character. Type specimen status.}
#'   \item{\code{Collection}}{Character. Herbarium acronym or collection.}
#'   \item{\code{Barcode}}{Character. Herbarium barcode number.}
#'   \item{\code{Country}}{Character. Country of collection.}
#'   \item{\code{State}}{Character. State or province.}
#'   \item{\code{City}}{Character. City or municipality.}
#'   \item{\code{Locality}}{Character. Specific locality description.}
#'   \item{\code{Latitude}}{Character. Geographic latitude.}
#'   \item{\code{Longitude}}{Character. Geographic longitude.}
#'   \item{\code{Altitude}}{Character. Elevation in meters.}
#'   \item{\code{Phytophysiognomy}}{Character. Vegetation type.}
#'   \item{\code{HABIT}}{Character. Growth habit description.}
#'   \item{\code{HABIT height (m)}}{Character. Plant height in meters.}
#'   \item{\code{HABIT branch growth}}{Character. Branch growth pattern.}
#'   \item{\code{HABIT branching pattern}}{Character. Branching architecture.}
#'   \item{\code{HABIT STEM Bark}}{Character. Stem bark characteristics.}
#'   \item{\code{HABIT STEM color}}{Character. Stem color.}
#'   \item{\code{HABIT STEM indument}}{Character. Stem pubescence.}
#'   \item{\code{STIPULE number}}{Character. Number of stipules.}
#'   \item{\code{STIPULE position}}{Character. Stipule position.}
#'   \item{\code{STIPULE persistence}}{Character. Stipule persistence.}
#'   \item{\code{STIPULE length (mm)}}{Character. Stipule length in mm.}
#'   \item{\code{STIPULE width (mm)}}{Character. Stipule width in mm.}
#'   \item{\code{STIPULE shape}}{Character. Stipule shape.}
#'   \item{\code{STIPULE BASE}}{Character. Stipule base morphology.}
#'   \item{\code{STIPULE MARGIN}}{Character. Stipule margin characteristics.}
#'   \item{\code{STIPULE APEX}}{Character. Stipule apex morphology.}
#'   \item{\code{STIPULE indument}}{Character. Stipule pubescence.}
#'   \item{\code{LEAF phyllotaxis}}{Character. Leaf arrangement on stem.}
#'   \item{\code{LEAF arrangement on branch}}{Character. Leaf arrangement.}
#'   \item{\code{LEAF PETIOLE shape}}{Character. Petiole shape.}
#'   \item{\code{LEAF PETIOLE Striations}}{Character. Petiole striations.}
#'   \item{\code{LEAF PETIOLE indument}}{Character. Petiole pubescence.}
#'   \item{\code{LEAF PETIOLE length (cm)}}{Character. Petiole length in cm.}
#'   \item{\code{LEAF PETIOLE width (cm)}}{Character. Petiole width in cm.}
#'   \item{\code{LEAF BLADE style}}{Character. Leaf blade style.}
#'   \item{\code{LEAF BLADE brightness}}{Character. Leaf blade brightness.}
#'   \item{\code{LEAF BLADE texture}}{Character. Leaf blade texture.}
#'   \item{\code{LEAF BLADE shape}}{Character. Leaf blade shape.}
#'   \item{\code{LEAF BLADE length (cm)}}{Character. Leaf blade length in cm.}
#'   \item{\code{LEAF BLADE width (cm)}}{Character. Leaf blade width in cm.}
#'   \item{\code{LEAF BLADE BASE}}{Character. Leaf blade base morphology.}
#'   \item{\code{LEAF BLADE APEX}}{Character. Leaf blade apex morphology.}
#'   \item{\code{LEAF MARGINS}}{Character. Leaf margin type.}
#'   \item{\code{LEAF MARGINS percentual serrate}}{Character. Serration extent.}
#'   \item{\code{LEAF MARGINS edges}}{Character. Margin edge characteristics.}
#'   \item{\code{LEAF venation}}{Character. Leaf venation type.}
#'   \item{\code{LEAF SECONDARY-VENATION}}{Character. Secondary venation pattern.}
#'   \item{\code{LEAF SECONDARY-VENATION visibility}}{Character. Venation visibility.}
#'   \item{\code{LEAF TERTIARY-VENATION}}{Character. Tertiary venation pattern.}
#'   \item{\code{LEAF TERTIARY-VENATION visibility}}{Character. Visibility of tertiary veins.}
#'   \item{\code{LEAF MIDVEIN}}{Character. Midvein characteristics.}
#'   \item{\code{LEAF indument}}{Character. Leaf pubescence.}
#'   \item{\code{INFLORESCENCE type}}{Character. Inflorescence type.}
#'   \item{\code{INFLORESCENCE relative size}}{Character. Inflorescence size relative to leaves.}
#'   \item{\code{INFLORESCENCE position}}{Character. Inflorescence position.}
#'   \item{\code{INFLORESCENCE further notes}}{Character. Additional inflorescence notes.}
#'   \item{\code{INFLORESCENCE length (cm)}}{Character. Inflorescence length in cm.}
#'   \item{\code{INFLORESCENCE width (cm)}}{Character. Inflorescence width in cm.}
#'   \item{\code{INFLORESCENCE flower number}}{Character. Number of flowers per inflorescence.}
#'   \item{\code{INFLORESCENCE AXES indument}}{Character. Inflorescence axis pubescence.}
#'   \item{\code{INFLORESCENCE PEDUNCLE}}{Character. Peduncle description.}
#'   \item{\code{INFLORESCENCE PEDUNCLE length (cm)}}{Character. Peduncle length in cm.}
#'   \item{\code{INFLORESCENCE PEDUNCLE width (cm)}}{Character. Peduncle width in cm.}
#'   \item{\code{INFLORESCENCE PEDUNCLE indument}}{Character. Peduncle pubescence.}
#'   \item{\code{INFLORESCENCE BRACT}}{Character. Bract description.}
#'   \item{\code{INFLORESCENCE BRACT distribution}}{Character. Bract distribution.}
#'   \item{\code{INFLORESCENCE BRACT shape}}{Character. Bract shape.}
#'   \item{\code{INFLORESCENCE BRACT length (mm)}}{Character. Bract length in mm.}
#'   \item{\code{INFLORESCENCE BRACT width (mm)}}{Character. Bract width in mm.}
#'   \item{\code{INFLORESCENCE BRACT LONGITUDINAL-VENATION}}{Character. Bract venation.}
#'   \item{\code{INFLORESCENCE BRACT MARGINS}}{Character. Bract margin characteristics.}
#'   \item{\code{FLOWER symmetry}}{Character. Flower symmetry.}
#'   \item{\code{FLOWER PEDICEL indument}}{Character. Pedicel pubescence.}
#'   \item{\code{FLOWER PEDICEL length (mm)}}{Character. Pedicel length in mm.}
#'   \item{\code{FLOWER PEDICEL width (mm)}}{Character. Pedicel width in mm.}
#'   \item{\code{FLOWER SEPALS arrangement}}{Character. Sepal arrangement.}
#'   \item{\code{FLOWER SEPALS color}}{Character. Sepal color.}
#'   \item{\code{FLOWER SEPALS indument}}{Character. Sepal pubescence.}
#'   \item{\code{FLOWER SEPALS shape}}{Character. Sepal shape.}
#'   \item{\code{FLOWER SEPALS length (mm)}}{Character. Sepal length in mm.}
#'   \item{\code{FLOWER SEPALS width (mm)}}{Character. Sepal width in mm.}
#'   \item{\code{FLOWER SEPALS BASE}}{Character. Sepal base morphology.}
#'   \item{\code{FLOWER SEPALS APEX}}{Character. Sepal apex morphology.}
#'   \item{\code{FLOWER SEPALS margin}}{Character. Sepal margin characteristics.}
#'   \item{\code{FLOWER SEPALS persistence}}{Character. Sepal persistence.}
#'   \item{\code{FLOWER PETALS number}}{Character. Number of petals.}
#'   \item{\code{FLOWER PETALS arrangement}}{Character. Petal arrangement.}
#'   \item{\code{FLOWER PETALS color}}{Character. Petal color.}
#'   \item{\code{FLOWER PETALS texture}}{Character. Petal texture.}
#'   \item{\code{FLOWER PETALS indument}}{Character. Petal pubescence.}
#'   \item{\code{FLOWER PETALS shape}}{Character. Petal shape.}
#'   \item{\code{FLOWER PETALS length (mm)}}{Character. Petal length in mm.}
#'   \item{\code{FLOWER PETALS width (mm)}}{Character. Petal width in mm.}
#'   \item{\code{FLOWER PETALS BASE}}{Character. Petal base morphology.}
#'   \item{\code{FLOWER PETALS APEX}}{Character. Petal apex morphology.}
#'   \item{\code{FLOWER STAMEN number}}{Character. Number of stamens.}
#'   \item{\code{FLOWER STAMEN filament}}{Character. Stamen filament description.}
#'   \item{\code{FLOWER STAMEN ANTHERS color}}{Character. Anther color.}
#'   \item{\code{FLOWER STAMEN ANTHERS texture}}{Character. Anther texture.}
#'   \item{\code{FLOWER STAMEN ANTHERS indument}}{Character. Anther pubescence.}
#'   \item{\code{FLOWER STAMEN ANTHERS compartments}}{Character. Anther compartment number.}
#'   \item{\code{FLOWER STAMEN ANTHERS dehiscence}}{Character. Anther dehiscence type.}
#'   \item{\code{FLOWER STAMEN ANTHERS length (mm)}}{Character. Anther length in mm.}
#'   \item{\code{FLOWER STAMEN ANTHERS width (mm)}}{Character. Anther width in mm.}
#'   \item{\code{FLOWER GYNOECIUM position}}{Character. Gynoecium position.}
#'   \item{\code{FLOWER GYNOECIUM length (mm)}}{Character. Gynoecium length in mm.}
#'   \item{\code{FLOWER GYNOECIUM indument}}{Character. Gynoecium pubescence.}
#'   \item{\code{FLOWER GYNOECIUM CARPELS}}{Character. Number of carpels.}
#'   \item{\code{FLOWER GYNOECIUM STYLE length (mm)}}{Character. Style length in mm.}
#'   \item{\code{FLOWER GYNOECIUM STYLE color}}{Character. Style color.}
#'   \item{\code{FLOWER GYNOECIUM STIGMA}}{Character. Stigma description.}
#'   \item{\code{FRUIT known}}{Character. Whether fruits are known.}
#'   \item{\code{FRUIT length (cm)}}{Character. Fruit length in cm.}
#'   \item{\code{FRUIT CARPOPHORE form}}{Character. Carpophere form.}
#'   \item{\code{FRUIT CARPOPHORE color}}{Character. Carpophere color.}
#'   \item{\code{FRUIT CARPOPHORE length (cm)}}{Character. Carpophere length in cm.}
#'   \item{\code{FRUIT CARPOPHORE width (cm)}}{Character. Carpophere width in cm.}
#'   \item{\code{FRUIT MERICARP number}}{Character. Number of mericarps.}
#'   \item{\code{FRUIT MERICARP length (mm)}}{Character. Mericarp length in mm.}
#'   \item{\code{FRUIT MERICARP width (mm)}}{Character. Mericarp width in mm.}
#'   \item{\code{FRUIT MERICARP form}}{Character. Mericarp form.}
#'   \item{\code{SEED length (mm)}}{Character. Seed length in mm.}
#'   \item{\code{SEED width (mm)}}{Character. Seed width in mm.}
#'   \item{\code{SEED apical hook}}{Character. Presence of apical hook.}
#' }
#'
#' @details
#' This dataset was specifically designed to demonstrate the capabilities
#' of the \code{barroso_write_taxon_descr()} function in the barRoso package.
#' The morphological character columns follow specific naming conventions:
#'
#' \itemize{
#'   \item \strong{Simple characters}: Column names without hierarchical structure
#'         (e.g., "HABIT", "Author")
#'   \item \strong{Measurements}: Include units in parentheses
#'         (e.g., "LEAF BLADE length (cm)")
#'   \item \strong{Hierarchical characters}: Use ALL CAPS for grouping terms
#'         (e.g., "LEAF BLADE BASE", "FLOWER SEPALS APEX")
#' }
#'
#' The dataset includes 15 species of \emph{Ouratea}:
#' \itemize{
#'   \item \emph{Ouratea concinna}
#'   \item \emph{Ouratea crenulata}
#'   \item \emph{Ouratea disticha}
#'   \item \emph{Ouratea gladiifolia}
#'   \item \emph{Ouratea hexasperma}
#'   \item \emph{Ouratea hostilis}
#'   \item \emph{Ouratea macranthos}
#'   \item \emph{Ouratea neoweddelliana}
#'   \item \emph{Ouratea ovalis}
#'   \item \emph{Ouratea parvifolia}
#'   \item \emph{Ouratea pilinervosa}
#'   \item \emph{Ouratea planchonii}
#'   \item \emph{Ouratea pubescens}
#'   \item \emph{Ouratea salicifolia}
#'   \item \emph{Ouratea spectabilis}
#' }
#'
#' The data includes herbarium specimen information, geographical coordinates,
#' and comprehensive morphological characters organized by plant organ systems.
#'
#' @source
#' Primary data collection from herbarium specimens and field observations
#' compiled for taxonomic revision of the genus \emph{Ouratea} (Ochnaceae).
#'
#' @usage data(morphological_dataset)
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(morphological_dataset)
#'
#' # View the structure
#' str(morphological_dataset)
#'
#' # List the species included
#' unique(paste(morphological_dataset$Genus, morphological_dataset$Species))
#'
#' # Count specimens per species
#' table(paste(morphological_dataset$Genus, morphological_dataset$Species))
#'
#' # Example: Generate descriptions for all species
#' library(barRoso)
#' barroso_write_taxon_descr(
#'   xlsx_path = system.file("extdata", "morphological_dataset.xlsx",
#'                          package = "barRoso"),
#'   species_cols = c("Genus", "Species", "Author"),
#'   character_cols = 19:131  # Morphological character columns
#' )
#' }
#'
#' @keywords datasets
#' @keywords morphology
#' @keywords taxonomy
#' @keywords Ochnaceae
#' @keywords Ouratea
NULL
