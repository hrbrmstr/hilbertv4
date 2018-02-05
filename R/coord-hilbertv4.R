#' ggplot2 `Coord` for Hilbert IPv4 heatmaps
#'
#' @rdname coord_hilbert_v4
#' @export
CoordHilbertV4 <- ggplot2::ggproto(
  "CoordHilbertV4",
  ggplot2::CoordCartesian,
  aspect = function(self, ranges) {
    diff(ranges$y.range) / diff(ranges$x.range) * self$ratio
  }
)

#' @inheritParams ggplot2::coord_fixed
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
coord_hilbert_v4 <- function(xlim = NULL, ylim = NULL, bpp=c(8, 10, 12, 14, 16), expand=FALSE) {

  mx <- 4096/c(`8`=1, `10`=2, `12`=4, `14`=8, `16`=16)[as.character(bpp[1])]

  # note: negation is used due to the way hilbert ipv4 y is computed
  if (!is.null(ylim)) ylim <- -(ylim)

  ggplot2::ggproto(
    NULL,
    CoordHilbertV4,
    limits = list(
      x = xlim %||% c(0, mx),
      y = ylim %||% c(0, -mx) # note: negation is used due to the way hilbert ipv4 y is computed
    ),
    ratio = 1,
    expand = expand
  )

}