#' Create and Annotate 'Hilbert Curve' 'IPv4' Heatmaps
#'
#' @section Hilbert Curves:
#' A 12th-order Hilbert Curve to represnet the entire IPv4
#' address space. Locating a particular IP address along the curve can be
#' confusing at first. Here is what a 2nd-order Hilbert curve looks like:
#'
#'     0---1   14--15
#'         |   |
#'     3---2   13--12
#'     |            |
#'     4   7---8   11
#'     |   |   |    |
#'     5---6   9---10
#'
#' @md
#' @name hilbertv4
#' @docType package
#' @author Bob Rudis (bob@@rud.is)
#' @importFrom dplyr count
#' @importFrom ggplot2 Stat ggproto Geom CoordCartesian theme element_blank element_rect aes
#' @importFrom iptools ip_to_numeric
#' @useDynLib hilbertv4
#' @importFrom Rcpp sourceCpp
NULL