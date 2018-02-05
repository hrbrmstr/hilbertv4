#' "Map" theme for Hilbert IPv4 maps
#'
#' @md
#' @param fill background fill color (defaults to "black")
#' @export
#' @examples
#' # only doing this to generate some random values to show
#' ip_df <- data.frame(ip=sample(400000000, 1000000, replace=TRUE))
#'
#' ggplot(ip_df, aes(ip=ip)) +
#'   stat_hilbert_v4(bpp=16) +
#'   coord_hilbert_v4(bpp=16) +
#'   viridis::scale_fill_viridis(name="IPv4 count per pixel", trans="log2") +
#'   theme_hilbert_v4()
theme_hilbert_v4 <- function(fill = "black") {
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = fill)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank()) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
    ggplot2::theme(axis.text = ggplot2::element_blank()) +
    ggplot2::theme(axis.title = ggplot2::element_blank())
}