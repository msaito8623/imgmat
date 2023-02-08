#' Dataframe to a matrix
#'
#' This function converts a dataframe to a matrix. X-, y-, and z-axes can be given as arguments.
#'
#' @param dat A dataframe, which is to be converted to a matrix.
#' @param xcol A string, which must be one of the column names of the dataframe given by "dat". This column vector will be used as positions along x-axis of the result matrix.
#' @param ycol A string, which must be one of the column names of the dataframe given by "dat". This column vector will be used as positions along y-axis of the result matrix.
#' @param zcol A string, which must be one of the column names of the dataframe given by "dat". This column vector will be used as z-values (colors) of the result matrix.
#' @return A matrix.
#' @author Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
#' @keywords utilities
#' @examples
#' dat = expand.grid('x'=1:4, 'y'=1:3)
#' dat$z = 1:nrow(dat)
#' mat = dat2mat(dat, xcol='x', ycol='y', zcol='z')
#' print(mat)
#' @export
#'
dat2mat = function (dat, xcol, ycol, zcol) {
	dat = dat[,c(xcol,ycol,zcol)]
	dat[[xcol]] = to_index(dat[[xcol]])
	dat[[ycol]] = to_index(dat[[ycol]])
	if (nrow(unique(dat[,c(xcol,ycol)]))!=nrow(dat)) {
		stop('Duplicated rows detected.')
	}
	mat = matrix(NA, ncol=length(unique(dat[[xcol]])), nrow=length(unique(dat[[ycol]])))
	for ( i in 1:nrow(dat) ) {
		cx = dat[[xcol]][i]
		cy = dat[[ycol]][i]
		cz = dat[[zcol]][i]
		mat[cy, cx] = cz
	}
	return(mat)
}
