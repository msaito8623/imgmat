#' Conversion from a GAM model to a predicted matrix.
#'
#' This function receives an ultrasound GAM and produces a matirx, which is the image the GAM is trained on. The current version of the function assumes that a GAM is trained on just one image.
#'
#' @param mdl A GAM model object, which has been trained on an (ultrasound) image with a model structure "z~te(x,y)".
#' @param rotate How many times to turn a result matrix clockwise.
#' @return An image matrix, which should be (approximately) the same as the one the GAM is trained on.
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
#' print(mat[1:5,1:5])
#' @export
#'
mdl2mat <- function (mdl, rotate=1) {
	cnd = mdl2prd(mdl)
	if (min(cnd$x)<1) {
		cnd$x = cnd$x + 1
	}
	if (min(cnd$y)<0) {
		cnd$y = (-1) * cnd$y
	}
	if (min(cnd$y)<1) {
		cnd$y = cnd$y + 1
	}
	mat = matrix(0L, ncol=length(unique(cnd$x)), nrow=length(unique(cnd$y)))
	for ( i in 1:nrow(cnd) ) {
		cx = cnd$x[i]
		cy = cnd$y[i]
		cz = cnd$fit[i]
		mat[cy, cx] = cz
	}
	if (rotate>0) {
		rot <- function(x) t(apply(x, 2, rev))
		for ( i in 1:rotate ) {
			mat = rot(mat)
		}
	}
	return(mat)
}
