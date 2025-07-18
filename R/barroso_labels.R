#' Generate Herbarium Labels from Field Book Records
#'
#' @author Domingos Cardoso
#'
#' @description
#' Creates printable herbarium labels from field book data stored in a CSV-formatted file.
#' This function is optimized for records from the USA by generating geographic maps
#' at the county level. For records from other countries, only the country-level map
#' is included. If geographic coordinates are available, specimen points are plotted
#' on the maps. The function also retrieves taxon authorities and performs automatic
#' nomenclatural updates using the [`lcvplants`](https://idiv-biodiversity.github.io/lcvplants/)
#' package.
#'
#' @details
#' The function performs extensive pre-processing including trimming extra spaces,
#' fixing punctuation, removing artifacts, and inserting default values for missing
#' taxon/specimen info. Scientific names are cross-referenced against the
#' `lcvplants` database to update taxonomy and retrieve the correct author citation.
#' Locality and habitat descriptions are automatically formatted for line breaks.
#' Labels are arranged in grids of six per page and exported as paginated PDF files.
#'
#' @usage
#' barroso_labels(fieldbook = NULL,
#'                dir_create = "results_herbarium_labels",
#'                file_label = "herbarium_labels.pdf")
#'
#' @param fieldbook A data frame or CSV file path containing herbarium collection
#' records. Columns must include taxonomic, locality, and collector fields. See
#' the README for details on expected field names and formatting.
#'
#' @param dir_create Output directory for label files. A subfolder named with the
#' current date will be created inside this directory. Default: `"results_herbarium_labels"`.
#'
#' @param file_label Base name for the generated label PDFs. When multiple pages
#' are required, numbered suffixes will be added automatically. Default: `"herbarium_labels.pdf"`.
#'
#' @return A series of PDF files containing formatted herbarium labels saved
#' in the specified output directory.
#'
#' @examples
#' \dontrun{
#' barroso_labels(fieldbook = "fieldbook_collections.csv")
#' }
#'
#' @importFrom magrittr "%>%"
#' @importFrom lcvplants lcvp_search
#' @importFrom cowplot ggdraw draw_plot plot_grid save_plot
#' @importFrom ggplot2 ggplot theme geom_sf geom_point aes annotate element_rect
#' @importFrom ggthemes theme_map
#' @importFrom ggspatial layer_spatial
#' @importFrom maps map
#' @importFrom sf st_as_sf st_bbox
#'
#' @export
#'

barroso_labels <- function(fieldbook = NULL,
                           dir_create = "results_herbarium_labels",
                           file_label = "herbarium_labels.pdf") {

  #_______________________________________________________________________________
  # Making corrections in the database
  for (i in seq_along(names(fieldbook))) {
    fieldbook[[i]] <- gsub("^$", NA, fieldbook[[i]])
    fieldbook[[i]] <- gsub("\\s{2,}", " ", fieldbook[[i]])
  }

  fieldbook$decimalLatitude <- as.numeric(fieldbook$decimalLatitude)
  fieldbook$decimalLongitude <- as.numeric(fieldbook$decimalLongitude)
  fieldbook$recordNumber <- as.character(fieldbook$recordNumber)

  fieldbook$country <- gsub("United States|United States of America", "USA", fieldbook$country)
  fieldbook$county <- gsub("\\sCounty$|\\sCo[.]$", "", fieldbook$county)

  fieldbook$locality <- gsub("-", "\\\u00ad", fieldbook$locality)
  fieldbook$vegetation <- gsub("-", "\\\u00ad", fieldbook$vegetation)
  fieldbook$plantDescription <- gsub("-", "\\\u00ad", fieldbook$plantDescription)
  fieldbook$vernacularName <- gsub("-", "\\\u00ad", fieldbook$vernacularName)

  #_______________________________________________________________________________
  # Stop the function if NA value is found within any stateProvince column
  tf_na <- is.na(fieldbook$stateProvince)
  if(any(tf_na)) {
    stop(paste("\nCell(s) with NA found in the stateProvince column.\n
             Please check your Excel field book!")
    )
  }

  #_______________________________________________________________________________
  # Adding a final punctuation at some cells
  tf_na <- !is.na(fieldbook$locality)
  tf <- !grepl("[.]$", fieldbook$locality[tf_na])
  if(any(tf)) {
    fieldbook$locality[tf_na][tf] <- gsub("$", ".", fieldbook$locality[tf_na][tf])
  }
  tf_na <- !is.na(fieldbook$vegetation)
  tf <- !grepl("[.]$", fieldbook$vegetation[tf_na])
  if(any(tf)) {
    fieldbook$vegetation[tf_na][tf] <- gsub("$", ".", fieldbook$vegetation[tf_na][tf])
  }
  tf_na <- !is.na(fieldbook$plantDescription)
  tf <- !grepl("[.]$", fieldbook$plantDescription[tf_na])
  if(any(tf)) {
    fieldbook$plantDescription[tf_na][tf] <- gsub("$", ".", fieldbook$plantDescription[tf_na][tf])
  }

  tf <- is.na(fieldbook$species)
  if(any(tf)) {
    fieldbook$species[tf] <- "sp."
  }
  tf <- is.na(fieldbook$recordNumber)
  if(any(tf)) {
    fieldbook$recordNumber[tf] <- "s.n."
  }

  #_______________________________________________________________________________
  # Get the world country level map
  world <- sf::st_as_sf(maps::map("world", fill=TRUE, plot =FALSE))

  full_map_list <- list()
  for (i in 1:length(fieldbook$species)) {

    fieldbook_temp <- fieldbook[i, ]

    print(paste(paste0(i,"/",length(fieldbook$species)), "Making label for",
                fieldbook_temp$genus, fieldbook_temp$species,
                fieldbook_temp$recordedBy, fieldbook_temp$recordNumber))

    fieldbook_temp$locality <- ifelse(is.na(fieldbook_temp$locality), "", fieldbook_temp$locality)
    fieldbook_temp$vegetation <- ifelse(is.na(fieldbook_temp$vegetation), "", fieldbook_temp$vegetation)
    fieldbook_temp$locality_vegetation <- paste(fieldbook_temp$locality, fieldbook_temp$vegetation)
    if (fieldbook_temp$locality_vegetation %in% " ") {
      fieldbook_temp$locality_vegetation <- ""
    }

    ncharloc <- nchar(fieldbook_temp$locality_vegetation)

    #_______________________________________________________________________________
    # Break the text into different lines depending on the number of characters for each line
    fieldbook_temp$locality_vegetation <- gsub("(.{1,85})(\\s|$)", "\\1\n",
                                               fieldbook_temp$locality_vegetation)

    #_______________________________________________________________________________
    # Get authority for each taxon automatically using lcvplants package
    if (is.na(fieldbook_temp$infraspecies)) {
      authority <- paste(fieldbook_temp$genus, fieldbook_temp$species)
    } else {
      authority <- paste(fieldbook_temp$genus, fieldbook_temp$species, fieldbook_temp$infraspecies)
    }
    authority <- lcvplants::lcvp_search(authority)

    if (!is.null(authority)) {
      if (is.na(fieldbook_temp$infraspecies)) {
        taxon <- sub("^(\\S*\\s+\\S+).*", "\\1", authority$Output.Taxon)
        authority <- gsub(".*^(\\S*\\s+\\S+)", "", authority$Output.Taxon)
        # Updating taxon name
        if (paste(fieldbook_temp$genus, fieldbook_temp$species) != taxon) {
          fieldbook_temp$genus <- gsub("\\s.*", "", taxon)
          fieldbook_temp$species <- gsub(".*\\s", "", taxon)
        }
      } else {
        taxon <- sub("^(\\S*\\s+\\S+\\s+\\S+\\s+\\S+).*", "\\1", authority$Output.Taxon)
        authority <- gsub(".*^(\\S*\\s+\\S+\\s+\\S+\\s+\\S+)", "", authority$Output.Taxon)
      }
    } else {
      authority <- " "
    }

    #_______________________________________________________________________________
    # Mapping
    if (fieldbook_temp$country == "USA") {

      ctr <- sf::st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))

      county <- sf::st_as_sf(maps::map("county", fieldbook_temp$stateProvince,
                                       fill=TRUE, plot =FALSE))
      state <- ctr[ctr$ID %in% tolower(fieldbook_temp$stateProvince), ]

      county_temp <- county[gsub(".+,", "", county$ID) %in% tolower(fieldbook_temp$county), ]

      ctr_bbox <- ctr %>%
        sf::st_as_sf(coords = c("X2","X1"), crs = 4326) %>%
        sf::st_bbox()
    }

    if (fieldbook_temp$country != "USA") {
      ctr <- world[world$ID %in% fieldbook_temp$country, ]
      ctr_bbox <- ctr %>%
        sf::st_as_sf(coords = c("X2","X1"), crs = 4326) %>%
        sf::st_bbox()
    }

    # Create an empty frame to plot the label
    df <- data.frame()
    outline <- ggplot2::ggplot(df) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(colour = "black",
                                                fill = NA,
                                                size = 1.5),
        rect = ggplot2::element_blank()
      )

    # Create the country map
    ctr_map <- ggplot2::ggplot(ctr) +
      #geom_sf(color = "#2b2b2b", fill = "white", size=0.125) +
      ggplot2::geom_sf(colour = "gray70",
                       fill = ifelse(fieldbook_temp$country == "USA", NA, "gray95"),
                       size = 0.2) +
      ggthemes::theme_map() +
      if (fieldbook_temp$country == "USA") {
        ggspatial::layer_spatial(state, color = "gray20", fill = "gray95", size = 0.4)
        #coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"), datum = NA) +
      }

    if (fieldbook_temp$country != "USA") {
      ctr_map <- ctr_map +
        ggplot2::geom_point(data = fieldbook_temp,
                            ggplot2::aes(x = decimalLongitude,
                                         y = decimalLatitude),
                            shape =  21,
                            alpha = 0.9,
                            size = 3,  # control for point size
                            stroke = 0.1, # control for point border width
                            colour = "black",
                            fill = "#D53E4F",
                            show.legend = FALSE)
    }

    # Create the county map for USA
    if (fieldbook_temp$country == "USA") {
      county_map <- ggplot2::ggplot(county) +
        ggplot2::geom_sf(colour = "gray70", fill = "gray95", size = 0.2) +
        ggspatial::layer_spatial(county_temp, color = "gray20", fill = "gray95", size = 0.4) +
        ggthemes::theme_map() +
        ggplot2::geom_point(data = fieldbook_temp,
                            ggplot2::aes(x = decimalLongitude,
                                         y = decimalLatitude),
                            shape =  21,
                            alpha = 0.9,
                            size = 3,  # control for point size
                            stroke = 0.1, # control for point border width
                            colour = "black",
                            fill = "#D53E4F",
                            show.legend = FALSE)
    }

    if (is.na(fieldbook_temp$infraspecies)) {
      sciname <- paste0("~bold(", gsub(" ", "~", taxon), ")")
    } else {
      sciname <- paste0("~bold(", fieldbook_temp$genus, "~", fieldbook_temp$species, ")",
                        "~", gsub(" ", "~bold(", fieldbook_temp$infraspecies), ")")
    }

    fullmap <- cowplot::ggdraw() +
      cowplot::draw_plot(outline) +
      ggplot2::annotate("text", x = 0.1, y = 0.76,
                        label = paste0(paste0(fieldbook_temp$herbarium, "\n"),
                                       ifelse(is.na(fieldbook_temp$catalogNumber),
                                              "", fieldbook_temp$catalogNumber)),
                        colour = "black",
                        size = 4.5) +
      ggplot2::annotate("text", x = 0.77, y = 0.84,
                        label = paste("FLORA OF\n", ifelse(fieldbook_temp$country == "USA",
                                                           toupper(fieldbook_temp$stateProvince),
                                                           toupper(fieldbook_temp$country))),
                        colour = "black",
                        size = 8) +
      ggplot2::annotate("text", x = 0.77, y = 0.67,
                        label = fieldbook_temp$vernacularName,
                        colour = "black",
                        size = 4) +
      ggplot2::annotate("text", x = 0.77, y = 0.62,
                        label = toupper(fieldbook_temp$family),
                        colour = "black",
                        size = 4) +
      ggplot2::annotate("text", x = 0.05, y = 0.4,
                        label = paste0(ifelse(is.na(fieldbook_temp$stateProvince),
                                              toupper(fieldbook_temp$country),
                                              toupper(fieldbook_temp$stateProvince)),
                                       ifelse(is.na(fieldbook_temp$county), ".", ", "),

                                       ifelse(is.na(fieldbook_temp$county), "",
                                              paste(fieldbook_temp$county,
                                                    ifelse(fieldbook_temp$country == "USA", "Co.   ", "   "))),

                                       ifelse(is.na(fieldbook_temp$decimalLatitude), "",
                                              fieldbook_temp$decimalLatitude),
                                       ifelse(is.na(fieldbook_temp$decimalLatitude), "", ", "),
                                       ifelse(is.na(fieldbook_temp$decimalLongitude), "",
                                              fieldbook_temp$decimalLongitude), "   ",
                                       ifelse(is.na(fieldbook_temp$altitude), "",
                                              fieldbook_temp$altitude), "\n",
                                       fieldbook_temp$locality_vegetation),
                        colour = "black",
                        size = 4,
                        hjust = 0,
                        vjust = 0.7) +
      ggplot2::annotate("text", x = 0.1, y = 0.55,
                        label = sciname,
                        colour = "black",
                        size = 6,
                        hjust = 0,
                        parse = TRUE) +
      ggplot2::annotate("text", x = 0.2, y = 0.5,
                        label = authority,
                        colour = "black",
                        size = 5,
                        hjust = 0) +
      ggplot2::annotate("text", x = 0.05, y = 0.08,
                        label = paste(fieldbook_temp$recordedBy,
                                      fieldbook_temp$recordNumber, " ",
                                      ifelse(is.na(fieldbook_temp$addCollector), "",
                                             fieldbook_temp$addCollector), "   ",
                                      paste0(ifelse(is.na(fieldbook_temp$day), "", fieldbook_temp$day), " ",
                                             ifelse(is.na(fieldbook_temp$month), "", fieldbook_temp$month), " ",
                                             ifelse(is.na(fieldbook_temp$year), "Unknown collection date", fieldbook_temp$year))),
                        colour = "black",
                        size = 4.5,
                        hjust = 0) +
      ggplot2::annotate("text", x = 0.05, y = ifelse(ncharloc > 300, 0.17, 0.25),
                        label = paste(ifelse(is.na(fieldbook_temp$plantDescription), "",
                                             gsub("(.{1,85})(\\s|$)", "\\1\n", fieldbook_temp$plantDescription))),
                        colour = "black",
                        size = 3.5,
                        hjust = 0)

    if (fieldbook_temp$country == "USA") {
      fullmap <- fullmap +
        cowplot::draw_plot(ctr_map, x = 0.02, y = 0.8, width = 0.2, height = 0.2) +
        cowplot::draw_plot(county_map, x = 0.2, y = 0.62, width = 0.35, height = 0.35)
    }
    if (fieldbook_temp$country != "USA") {
      fullmap <- fullmap +
        cowplot::draw_plot(ctr_map, x = 0.2, y = 0.55, width = 0.45, height = 0.45)
    }


    full_map_list[[i]] <- fullmap + ggplot2::theme(plot.margin = ggplot2::unit(c(0.7, 0.7, 0.7, 0.7), "cm"))

  }

  #_______________________________________________________________________________
  # Split the plots into chunks
  chunk_length <- 6
  full_map_list <- split(full_map_list,
                         ceiling(seq_along(full_map_list) / chunk_length))

  for (i in seq_along(full_map_list)) {

    # Adding empty labels when any chunk has less than 6 labels so as to fit the size
    if (length(full_map_list[[i]]) < 6) {
      for (l in (length(full_map_list[[i]])+1):6) {
        outline <- cowplot::ggdraw() + cowplot::draw_plot(outline)
        full_map_list[[i]][[l]] <- outline + ggplot2::theme(plot.margin = ggplot2::unit(c(0.7, 0.7, 0.7, 0.7), "cm"))
      }
    }

    allabels <- cowplot::plot_grid(full_map_list[[i]][[1]],
                                   full_map_list[[i]][[2]],
                                   full_map_list[[i]][[3]],
                                   full_map_list[[i]][[4]],
                                   full_map_list[[i]][[5]],
                                   full_map_list[[i]][[6]],
                                   ncol = 2, nrow = 3, align = "hv")

    # Create a new directory to save the results with current date
    # If there is no directory... make one!
    todaydate <- format(Sys.time(), "%d%b%Y")
    if (!dir.exists(paste0(dir_create, "/"))) {
      dir.create(paste0(dir_create, "/"))
    }
    if (!dir.exists(paste0(dir_create, "/", todaydate))) {
      dir.create(paste0(dir_create, "/", todaydate))
    }
    # If directory was created during a previous search, get its name to save results
    folder_name <- paste0(paste0(dir_create, "/"), todaydate)
    print(paste0("Writing '", folder_name, "' on disk."))

    file_label <- gsub("[.]pdf", "", file_label)

    cowplot::save_plot(paste0(folder_name, "/", file_label, "_", i, ".pdf"), allabels,
                       ncol = 2, # we're saving a grid plot of 2 columns
                       nrow = 3, # and 3 rows
                       base_height = 6,
                       base_aspect_ratio = 1.2,
                       base_width = NULL)
  }

}
