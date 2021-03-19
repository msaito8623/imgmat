#' Prediction dataframe to a ggplot object.
#'
#' This function takes a dataframe that has 'x', 'y', and 'fit' as columns and returns a ggplot object for a contour plot.
#'
#' @import ggplot2
#' @param prd A prediction dataframe, which must have 'x', 'y', and 'fit' as columns.
#' @param .title A string for the main title.
#' @param diff Boolean to indicate if the output is a difference plot.
#' @param .zlim A numeric whose length is 2.
#' @return A ggplot object that depicts a filled contour plot.
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
#' plt = prd2plt(prd)
#' print(plt)
#' @export
#'
prd2plt <- function (prd, .title=NULL, diff=FALSE, .xlab=NULL, .ylab=NULL, .zlim=NULL, no.margin=FALSE) {
	plt = ggplot(prd, aes(x, y, z=fit))
	plt = plt + geom_raster(aes(fill=fit))
	plt = plt + geom_contour()
	plt = plt + labs(title=.title, fill='')
	plt = plt + xlab(.xlab)
	plt = plt + ylab(.ylab)
	mycolors = c('black','darkblue','darkgreen','green','yellow','red','white')
	if (diff) {
		plt = plt + scale_fill_gradientn(limits = .zlim, colours=mycolors)
	} else {
		plt = plt + scale_fill_gradientn(limits = .zlim, colours=mycolors)
	}
	if (no.margin) {
		plt = plt + theme(legend.position='none')
		plt = plt + theme(plot.title=element_blank())
		plt = plt + theme(axis.title=element_blank())
		plt = plt + theme(axis.text=element_blank())
		plt = plt + theme(axis.ticks=element_blank())
		plt = plt + theme(plot.margin=margin(-0.65,-0.65,-0.75,-0.75, 'cm'))
		plt = plt + theme(panel.grid.major=element_blank())
		plt = plt + theme(panel.grid.minor=element_blank())
		plt = plt + theme(panel.border=element_blank())
		plt = plt + theme(panel.background=element_blank())
		plt = plt + theme(panel.spacing=unit(0,'cm'))
	}
	return(plt)
}
