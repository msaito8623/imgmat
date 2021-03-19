#' Find no-difference in an difference surface
#'
#' This function assumes the input matrix represents a difference term in GAM. This difference term being zero indicates no significant difference between the reference level and the factor level the difference surface represents. This function takes the upper-bound surface and lower-bound surface and find where they (confidence intervals) overlap. The no-difference cells will be replaced by NA (default).
#'
#' @param mat A matrix, assumed to be the main (fitted) surface.
#' @param upr A matrix, assumed to be a upper-bound surface.
#' @param lwr A matrix, assumed to be a lower-bound surface.
#' @param replace.by A numeric value whose length is 1. The non-significant cells will be replaced by the value specified by this argument.
#' @return A matrix, where non-significant cells are replaced by NA (default).
#' @import ggplot2 mgcv metR RColorBrewer myutil
#' @author Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
#' @keywords utilities
#' @examples
#' x = seq(-1,1,length.out=100)
#' y = seq(-1,1,length.out=100)
#' f = rep(c(0,1), each=50)
#' z = x^2 +  y^2 + f*x*y + rnorm(100, sd=0.01)
#' dat = data.frame(x=x, y=y, z=z, f=f)
#' dat$f = factor(dat$f)
#' dat$f = as.ordered(relevel(dat$f, '0'))
#' contrasts(dat$f) = 'contr.treatment'
#' mdl = gam(z ~ te(x,y) + te(x,y,by=f), data=dat)
#' vclist = list(x=list(edge=0,len.out=100), y=list(edge=0,len.out=100), f='1')
#' dat = create_ndat(mdl$model, vclist)
#' prd = mgcv::predict.gam(mdl, newdata=dat, type='terms', terms=mdl$smooth[[2]]$label, se=1)
#' dat$fit = prd$fit[,1]
#' dat$se  = prd$se.fit[,1]
#' dat$upr = dat$fit + 1.98*dat$se
#' dat$lwr = dat$fit - 1.98*dat$se
#' mat = dat2mat(dat, 'x', 'y', 'fit')
#' upr = dat2mat(dat, 'x', 'y', 'upr')
#' lwr = dat2mat(dat, 'x', 'y', 'lwr')
#' mat = replace_nodiff(mat, upr, lwr)
#' dat = mat2dat(mat)
#' plt = ggplot2::ggplot(dat, aes(x=x, y=y, z=z))
#' plt = mycontour(plt)
#' print(plt)
#' @export
#'
replace_nodiff = function (mat, upr, lwr, replace.by=NA) {
	pos = (upr>0)!=(lwr>0)
	mat[pos] = replace.by
	return(mat)
}
