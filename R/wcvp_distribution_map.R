#' Plot distribution maps for species, genera or families
#'
#' @param range Simple features (`sf`) data frame of the type output by [`wcvp_distribution()`].
#' @param crop_map Logical. Crop map extent to distribution? Defaults to `FALSE`.
#' @param native Logical. Include native range? Defaults to `TRUE`.
#' @param introduced Logical. Include introduced range? Defaults to `TRUE`.
#' @param extinct Logical. Include extinct range? Defaults to `TRUE`.
#' @param location_doubtful Logical. Include occurrences that are thought to be
#'     doubtful? Defaults to `TRUE`.
#'
#' @import ggplot2 dplyr
#' @importFrom rlang .data
#' @importFrom sf sf_use_s2 st_bbox st_shift_longitude st_centroid
#'
#' @export
#'
#' @details The colour scheme mirrors that used by Plants of the World (POWO;
#' https://powo.science.kew.org/), where green is native, purple is introduced, red is extinct and orange is doubtful. See Examples for how to use custom colours.
#'
#' @return A [`ggplot2::ggplot`] of the distribution.
#'
#' @examples
#' \dontrun{
#' p <- wcvp_distribution_map(wcvp_distribution("Callitris", taxon_rank = "genus"))
#' p
#' # now only the native range, and cropped to range extent
#' p <- wcvp_distribution_map(wcvp_distribution("Callitris", taxon_rank = "genus"),
#'   introduced = FALSE, crop_map = TRUE
#' )
#' p
#' # now with different colours
#' # note that this taxon only has native and introduced occurrences, so only two colours are needed
#' p <- wcvp_distribution_map(wcvp_distribution("Callitris", taxon_rank = "genus"))
#' p +
#'   # for polygons
#'   ggplot2::scale_fill_manual(values = c("red", "blue")) +
#'   # for points (islands)
#'   ggplot2::scale_colour_manual(values = c("red", "blue"))
#'   }
#'
wcvp_distribution_map <- function(range, crop_map = FALSE, native = TRUE, introduced = TRUE,
                                  extinct = TRUE, location_doubtful = TRUE) {
  occurrence_type <- NULL

  suppressMessages(sf_use_s2(FALSE))

  occurrence_types <- c("native", "introduced", "extinct", "location_doubtful")
  show_types <- occurrence_types[c(native, introduced, extinct, location_doubtful)]
  range <- filter(range, .data$occurrence_type %in% show_types)

  bbox <- st_bbox(range)
  range.area <- ((bbox[3] - bbox[1]) * (bbox[4] - bbox[2]))

  if (crop_map) {
    crop_details <- calculate_map_crop_(range, range.area, bbox)
    range <- crop_details$range
    range.area <- crop_details$range.area
  }

  suppressWarnings(range.centroids <- st_centroid(range))

  p <- powo_map(range, range.centroids)

  if (crop_map) {
    p <- p + coord_sf(xlim = crop_details$xlims, ylim = crop_details$ylims, expand = FALSE)
  } else {
    p <- p + coord_sf(expand = FALSE)
  }

  p
}

#' Plot a POWO style map for given range and range centroids.
#'
#' @param range_sf A simple features (`sf`) data frame of range polygons
#' @param centroids_sf A simple features (`sf`) data frame of range centroids
#'
#' @return A ggplot map of the range
#'
#' @import ggplot2
#' @export
powo_map <- function(range_sf, centroids_sf) {
  world <- rWCVP::wgsrpd3

  color_breaks <- unique(range_sf$occurrence_type)

  p <- ggplot(world) +
    geom_sf(fill = "white", col = "gray90") +
    geom_sf(data = range_sf, aes_(fill = ~occurrence_type), col = "gray90") +
    geom_sf(data = centroids_sf, aes_(col = ~occurrence_type), size = 2) +
    scale_fill_powo(breaks = color_breaks, name = "Status") +
    scale_colour_powo(breaks = color_breaks) +
    guides(colour = "none") +
    theme(
      panel.background = element_rect(fill = "#b8dee6"),
      panel.grid = element_blank(),
      plot.margin = margin(0, 0, 0, 0, "cm"),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )

  if (rlang::is_installed("rWCVPdata")) {
    coast <- rWCVPdata::coast
    p <- p + geom_sf(data = coast, fill = "transparent", col = "#89c7d5")
  }

  p
}

#' POWO colour palette for range maps
#'
#' Range maps displayed on the [POWO](https://powo.science.kew.org/) website have a fixed,
#' discrete colour palette based on the type of taxon occurrence
#' in a region.
#'
#' @rdname powo_pal
#' @export
powo_pal <- function() {
  c(
    "absent" = "#ffffff",
    "native" = "#72994c",
    "introduced" = "#995499",
    "extinct" = "#e22d2d",
    "location_doubtful" = "#ea962e"
  )
}

#' @importFrom ggplot2 scale_color_manual
#' @rdname powo_pal
#' @inheritParams ggplot2::scale_colour_manual
#' @export
scale_color_powo <- function(...) {
  scale_color_manual(values = powo_pal(), ...)
}

#' @rdname powo_pal
#' @export
scale_colour_powo <- scale_color_powo

#' @importFrom ggplot2 scale_color_manual
#' @rdname powo_pal
#' @inheritParams ggplot2::scale_fill_manual
#' @export
scale_fill_powo <- function(...) {
  scale_fill_manual(values = powo_pal(), ...)
}

#' Calculate crop area.
#'
#' @noRd
#'
calculate_map_crop_ <- function(range, range.area, bbox) {
  bbox2 <- st_bbox(st_shift_longitude(range))
  range.area2 <- ((bbox2[3] - bbox2[1]) * (bbox2[4] - bbox2[2]))

  if (range.area2 < (range.area / 1.5)) {
    range <- st_shift_longitude(range)

    range.buffer <- range.area2^0.3
    xlims <- c(bbox2[1] - range.buffer, bbox2[3] + range.buffer)
    ylims <- c(bbox2[2] - range.buffer, bbox2[4] + range.buffer)

    if (xlims[1] < 1) xlims[1] <- 1
    if (xlims[2] > 359) xlims[2] <- 359
    if (ylims[1] < -90) ylims[1] <- -90
    if (ylims[2] > 83) ylims[2] <- 83

    range.area <- range.area2
  } else {
    range.buffer <- range.area^0.3
    xlims <- c(bbox[1] - range.buffer, bbox[3] + range.buffer)
    ylims <- c(bbox[2] - range.buffer, bbox[4] + range.buffer)

    if (xlims[1] < -180) xlims[1] <- -180
    if (xlims[2] > 180) xlims[2] <- 180
    if (ylims[1] < -90) ylims[1] <- -90
    if (ylims[2] > 83) ylims[2] <- 83
  }

  list(
    range = range,
    range.area = range.area,
    xlims = xlims,
    ylims = ylims
  )
}
