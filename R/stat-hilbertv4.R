#' @rdname stat_hilbert_v4
#' @export
StatHilbertV4 <- ggplot2::ggproto(

  "StatHilbertV4",
  ggplot2::Stat,

  extra_params = c("na.rm", "bpp", "bpi"),

  setup_params = function(data, params) {
    params
  },

  setup_data = function(self, data, params) {
    data$ip <- if (is.character(data$ip)) iptools::ip_to_numeric(data$ip) else data$ip
    data <- cbind(data, ips_to_xy(data$ip, params$bpi, params$bpp))
    data <- dplyr::count(data, PANEL, x, y)
    data$ip <- 0
    data$y <- -(data$y)
    data
  },

  compute_panel = function(self, data, scales, ...) {
    data
  },

  default_aes = ggplot2::aes(fill = calc(n)),
  required_aes = c("ip")

)

#' ggplot2 `Stat` for Hilbert IPv4 heatmaps
#'
#' @inheritParams ggplot2::geom_raster
#' @param bpi bits per image (very likely changing to a CIDR notation)
#' @param bpp specifies the number of address space bits assigned to each pixel
#'        in the output image. By default each pixel represents a /24 network,
#'        which corresponds to 8 host bits (i.e., 256 hosts). Valid values are
#'        8, 10, 12, 14, and 16 (for now).
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
stat_hilbert_v4 <- function(mapping = NULL, data = NULL, geom = "raster",
                            bpi=32, bpp=c(8, 10, 12, 14, 16),
                            position = "identity", na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, ...) {

  layer(
    stat = StatHilbertV4,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      bpi = bpi,
      bpp = bpp[1],
      ...
    )
  )

}
