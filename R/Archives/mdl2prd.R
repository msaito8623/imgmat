#' Generation of a dataframe with predicted values by a GAM.
#'
#' This function receives a GAM model object and returns a dataframe with predicted values by the model. The current version assumes a GAM model provided is an ultrasound GAM model, i.e. a GAM which has brightness of each pixel of ultrasound images as a dependent variable and x- and y-coordinates as predictors. Therefore, the GAM given to this function must have been trained with predictors named "x" and "y". This restriction might be loosened and the function might be generalized in the future, if necessary.
#'
#' @param mdl A GAM model object fitted by mgcv::gam or mgcv::bam.
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
#' p = mdl2prd(m)
#' head(p)
#' @export
#'
mdl2prd <- function (mdl) {
	xxx = seq(min(mdl$model$x), max(mdl$model$x), by=1)
	yyy = seq(min(mdl$model$y), max(mdl$model$y), by=1)
	cnd = expand.grid(xxx, yyy)
	colnames(cnd) = c('x', 'y')
	cnd$fit = predict.gam(mdl, newdata=cnd)
	return(cnd)
}
