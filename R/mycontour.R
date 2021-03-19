#' Filled contour plot based on ggplot
#'
#' This function renders a ggplot object to be able to produce a filled contour plot with contour lines and labels. (NOTE: the values used for the x and y axes must form a grid for the current version of this function.)
#'
#' @param plt A ggplot object. It should already have the input dataframe and the aes values for x, y, and z.
#' @param ylim A 2 value vector.
#' @param zlim A 2 value vector.
#' @param breaks An integer for the number of contour lines. The first and last values will be the ends of the zlim scale. Therefore, (breaks-2) contour lines will be visible in the result plot.
#' @return A ggplot object, which can displays a basic filled contour with contour lines and labels.
#' @import ggplot2 mgcv metR RColorBrewer myutil
#' @author Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
#' @keywords utilities
#' @examples
#' set.seed(10)
#' dat = mgcv::gamSim()
#' mdl = mgcv::gam(y ~ te(x0,x1), data=dat)
#' vclist = list(x0=list(edge=0,len.out=100), x1=list(edge=0,len.out=100))
#' dat = create_ndat(mdl$model, vclist)
#' dat$fit = as.vector(mgcv::predict.gam(mdl, newdata=dat))
#' plt = ggplot2::ggplot(dat, aes(x=x0, y=x1, z=fit))
#' plt = mycontour(plt)
#' print(plt)
#' @export
#'
mycontour = function (plt, ylim=NULL, zlim=NULL, breaks=10) {
	floor_dec = function(x, level=1) round(x - 5*10^(-level-1), level)
	ceiling_dec = function(x, level=1) round(x + 5*10^(-level-1), level)
	zvec = plt$data[[plt$labels$z]]
	cmin = floor_dec(min(zvec,na.rm=TRUE),2)
	cmax = ceiling_dec(max(zvec,na.rm=TRUE),2)
	breaks1 = round(seq(cmin, cmax, length.out=breaks),2)
	breaks2 = round(seq(cmin, cmax, length.out=breaks*2),2)

	plt = plt + metR::geom_contour_fill(breaks=breaks2)
	plt = plt + metR::geom_contour2(breaks=breaks1)
	plt = plt + metR::geom_text_contour(aes(fontface='bold'), stroke=0.1, breaks=breaks1, skip=1)
	mycolors = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(length(breaks2)))
	plt = plt + scale_fill_gradientn(name=NULL, limits=zlim, colours=mycolors)
	return(plt)
}
