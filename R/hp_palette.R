#' Harry Potter Colour Map.
#'
#' This function creates a vector of \code{n} equally spaced colors along the
#' 'HP colour map' created by an average calculated for all the colours present in every frame of the pictures.
#'
#' @param n The number of colors (\eqn{\ge 1}) to be in the palette.
#'
#' @param alpha	The alpha transparency, a number in [0,1], see argument alpha in
#' \code{\link[grDevices]{hsv}}.
#'
#' @param begin The (corrected) hue in [0,1] at which the hp colormap begins.
#'
#' @param end The (corrected) hue in [0,1] at which the hp colormap ends.
#'
#' @param direction Sets the order of colors in the scale. If 1, the default, colors
#' are ordered from darkest to lightest. If -1, the order of colors is reversed.
#'
#' @param movie A character string indicating the colormap movie to use. Eight
#' movies are available: 1,2,3,4,5,6,7 and 8. It is also accepted to desigate
#' the 8th movie as 7.2 and the 7th movie as 7.1.
#'
#' @return \code{hp} returns a character vector.
#'
#' @author Alejandro Jiménez Rico \email{aljrico@@gmail.com}, \href{https://aljrico.github.io}{Personal Blog}
#'
#' @details
#'
#' \if{html}{Here are the color scales:
#'
#'   \out{<div style="text-align: center">}\figure{hp-scales.png}{movies: style="width:750px;max-width:90\%;"}\out{</div>}
#'
#'   }
#' \if{latex}{Here are the color scales:
#'
#'   \out{\begin{center}}\figure{hp-scales.png}\out{\end{center}}
#'   }
#'
#'
#' Semi-transparent colors (\eqn{0 < alpha < 1}) are supported only on some
#' devices: see \code{\link[grDevices]{rgb}}.
#'
#' @examples
#' library(ggplot2)
#' library(hexbin)
#'
#' dat <- data.frame(x = rnorm(10000), y = rnorm(10000))
#'
#' ggplot(dat, aes(x = x, y = y)) +
#'   geom_hex() + coord_fixed() +
#'   scale_fill_gradientn(colours = hp(256, movie = 1))
#'
#' # using code from RColorBrewer to demo the palette
#' n = 200
#' image(
#'   1:n, 1, as.matrix(1:n),
#'   col = hp(n, movie = 1),
#'   xlab = "hp n", ylab = "", xaxt = "n", yaxt = "n", bty = "n"
#' )
#'
#'


#'
#' @export
#'
hp <- function(n, alpha = 1, begin = 0, end = 1, direction = 1, movie = 1) {
	if (begin < 0 | begin > 1 | end < 0 | end > 1) {
		stop("begin and end must be in [0,1]")
	}

	if (abs(direction) != 1) {
		stop("direction must be 1 or -1")
	}

	if (direction == -1) {
		tmp <- begin
		begin <- end
		end <- tmp
	}

	if(movie == 7.1) movie <- 7
	if(movie == 7.2) movie <- 8

	hp.map <- harrypotterLite:::hp.map
	colnames(hp.map) <- c("R", "G", "B", "movie")

	map <- hp.map[hp.map$movie == movie, ]
	map$order <- map$R + map$G + map$B^2
	map <- map[order(map[,"order"]),]
	map <- map[,c("R","G","B","movie")]
	map_cols <- grDevices::rgb(map$R, map$G, map$B, maxColorValue = 255)
	fn_cols <- grDevices::colorRamp(map_cols, space = "Lab", interpolate = "spline")
	cols <- fn_cols(seq(begin, end, length.out = n)) / 255
	grDevices::rgb(cols[, 1], cols[, 2], cols[, 3], alpha = alpha)
}


#' @rdname hp
#'
#' @return  \code{hpMap} returns a \code{n} lines data frame containing the
#' red (\code{R}), green (\code{G}), blue (\code{B}) and alpha (\code{alpha})
#' channels of \code{n} equally spaced colors along the 'Harry Potter' colour map.
#' \code{n = 256} by default.
#'
hpMap <- function(n = 256, alpha = 1, begin = 0, end = 1, direction = 1, movie = 1) {
	if (begin < 0 | begin > 1 | end < 0 | end > 1) {
		stop("begin and end must be in [0,1]")
	}

	if (abs(direction) != 1) {
		stop("direction must be 1 or -1")
	}

	if (direction == -1) {
		tmp <- begin
		begin <- end
		end <- tmp
	}


	hp.map <- harrypotterLite:::hp.map
	colnames(hp.map) <- c("R", "G", "B", "movie")

	map <- hp.map[hp.map$movie == movie, ]
	map$order <- map$R + map$G + map$B^2
	map <- map[order(map[,"order"]),]
	map <- map[,c("R","G","B","movie")]

	map_cols <- grDevices::rgb(map$R, map$G, map$B, maxColorValue = 255)
	fn_cols <- grDevices::colorRamp(map_cols, space = "Lab", interpolate = "spline")
	cols <- fn_cols(seq(begin, end, length.out = n)) / 255
	data.frame(R = cols[, 1], G = cols[, 2], B = cols[, 3], alpha = alpha)
}
