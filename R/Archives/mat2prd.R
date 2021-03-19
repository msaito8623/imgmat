#' Conversion of an image matrix into a dataframe.
#'
#' This functions takes a matrix which represents an image and returns a dataframe from it, so that the dataframe can be immediately used to fit a GAM.
#'
#' @param mat An image matrix, which will be converted to a dataframe.
#' @param rev.y If TRUE, y-axis is reversed.
#' @return A dataframe, which has three columns: x, y, and fitted values.
#' @author Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
#' @keywords utilities
#' @examples
#' library(mgcv)
#' set.seed(2)
#' x = 1:100
#' y = 1:100
#' d = expand.grid('x'=x, 'y'=y)
#' d$brightness = 'brightness'=sample.int(255, nrow(d), replace=TRUE)
#' m = bam(brightness ~ te(x,y), data=d, method='fREML', discrete=TRUE)
#' mat = mdl2mat(m)
#' prd = mat2prd(mat)
#' head(prd)
#' @export
#'
mat2prd <- function (mat, rev.y=TRUE) {
	cnd = reshape2::melt(mat)
	colnames(cnd) = c('x', 'y', 'fit')
	cnd$x = cnd$x - 1
	if (rev.y) {
		cnd$y = cnd$y - max(cnd$y)
	} else {
		cnd$y = cnd$y -1
	}
	return(cnd)
}
