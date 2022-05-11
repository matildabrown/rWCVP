#' Plot distribution maps for species, genera or families
#'
#' @param range sf object of the type output by \code{get_distribution()}.
#' @param crop.map Logical. Crop map extent to distribution? Defaults to FALSE.
#' @param native Logical. Include native range? Defaults to TRUE.
#' @param introduced Logical. Include introduced range? Defaults to TRUE.
#' @param extinct Logical. Include extinct range? Defaults to TRUE.
#' @param location_doubtful Logical. Include occurrences that are thought to be
#'     doubtful? Defaults to TRUE.
#'
#' @import ggplot2
#' @details The colour scheme mirrors that used by Plants of the World (POWO;
#' https://powo.science.kew.org/).
#'
#' @return List of length two, with global and local maps of distribution as
#' ggplot objects. If taxon has global distribution, only the first element is returned.
#' @export
#'
#' @examples
#' p <- plot_distribution(get_distribution("Callitris", rank="genus"))
#' p[[1]]
#' p[[2]]
#' #now only the native range
#' p <- plot_distribution(get_distribution("Callitris", rank="genus"), introduced=FALSE)
#' p[[1]]
#' p[[2]]
plot_distribution <- function(range, crop.map=FALSE, native = TRUE, introduced = TRUE,
                             extinct = TRUE, location_doubtful = TRUE){
 occurrence_type <- NULL

  shown <- native
  showi <- introduced
  showe <- extinct
  showl <- location_doubtful
  requireNamespace("sf")
  suppressMessages(sf::sf_use_s2(FALSE))

colors <- c("absent" = "white",
            "native" = "#72994c",
            "introduced" = "#995499",
            "extinct" ="#e22d2d",
            "location doubtful" = "#ea962e")



if(shown==FALSE) range <- range %>% dplyr::filter(occurrence_type != "native")
if(showi==FALSE) range <- range %>% dplyr::filter(occurrence_type != "introduced")
if(showe==FALSE) range <- range %>% dplyr::filter(occurrence_type != "extinct")
if(showl==FALSE) range <- range %>% dplyr::filter(occurrence_type != "location doubtful")

bbox <- sf::st_bbox(range)
bbox2 <- sf::st_bbox(sf::st_shift_longitude(range))

range.area <- ((bbox[3]-bbox[1])*(bbox[4]-bbox[2]))
range.area2 <- ((bbox2[3]-bbox2[1])*(bbox2[4]-bbox2[2]))

suppressWarnings(range.centroids <- sf::st_centroid(range))

if(range.area2<(range.area/1.5)){

  range2 <- sf::st_shift_longitude(range)

  range.area <- ((bbox2[3]-bbox2[1])*(bbox2[4]-bbox2[2]))
  range.buffer <- range.area^0.3
  xlims <- c(bbox2[1]-range.buffer,bbox2[3]+range.buffer)
  ylims <- c(bbox2[2]-range.buffer,bbox2[4]+range.buffer)

  if(xlims[1]< 1) xlims[1] <- 1
  if(xlims[2]> 359) xlims[2] <- 359
  if(ylims[1]< -90) ylims[1] <- -90
  if(ylims[2]> 83) ylims[2] <- 83

  world <- rWCVPdata::wgsprd3_pacific
  coast <- rWCVPdata::coast_pacific
  suppressWarnings(range.centroids2 <- sf::st_centroid(range))

} else {
  range2 <- range
  range.area <- ((bbox[3]-bbox[1])*(bbox[4]-bbox[2]))
  range.buffer <- range.area^0.3
  xlims <- c(bbox[1]-range.buffer,bbox[3]+range.buffer)
  ylims <- c(bbox[2]-range.buffer,bbox[4]+range.buffer)

  if(xlims[1]< -180) xlims[1] <- -180
  if(xlims[2]> 180) xlims[2] <- 180
  if(ylims[1]< -90) ylims[1] <- -90
  if(ylims[2]> 83) ylims[2] <- 83
  world <- rWCVPdata::wgsprd3
  coast <- rWCVPdata::coast
  suppressWarnings(range.centroids2 <- sf::st_centroid(range))
}

wgsprd3 <- rWCVPdata::wgsprd3

if(crop.map==FALSE){

p <- ggplot(wgsprd3) +
  geom_sf(fill="white", col="gray90")+
  theme(panel.background = element_rect(fill = "#b8dee6"),
        panel.grid = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  geom_sf(data=range, aes(fill=occurrence_type), col="gray90") +
  geom_sf(data=rWCVPdata::coast, fill="transparent", col="#89c7d5") +
  geom_sf(data=range.centroids, aes(col=range$occurrence_type), size=2)+
  coord_sf(expand=FALSE)+
  scale_fill_manual(
    values=colors,
    breaks=unique(range$occurrence_type)
  )+
  scale_colour_manual(
    values=colors,
    breaks=unique(range$occurrence_type)
  )+ guides(colour="none")+
  guides(fill=guide_legend(title="Status"))

} else {

p <- ggplot(world) +
  geom_sf(fill="white", col="gray90")+
  theme(panel.background = element_rect(fill = "#b8dee6"),
        panel.grid = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  geom_sf(data=range2, aes(fill=occurrence_type), col="gray90") +
  geom_sf(data=coast, fill="transparent", col="#89c7d5") +
  geom_sf(data=range.centroids2, aes(col=occurrence_type), size=2)+
  coord_sf(xlim =xlims, ylim =ylims, expand=FALSE)+
  scale_fill_manual(
    values=colors,
    breaks=unique(range$occurrence_type)
  )+
  scale_colour_manual(
    values=colors,
    breaks=unique(range$occurrence_type)
  )+ guides(colour="none")+
  guides(fill=guide_legend(title="Status"))
}

return(p)

}
