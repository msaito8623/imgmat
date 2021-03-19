#' Image matrix to a dataframe
#'
#' This function converts a 2-dimensional matrix to a dataframe.
#'
#' @param mat A 2-dimensional matrix to be converted to a dataframe.
#' @return A dataframe, which has x, y, and z columns.
#' @author Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
#' @keywords utilities
#' @examples
#' mat = matrix(1:12, nrow=3, byrow=TRUE)
#' dat = mat2dat(mat)
#' print(dat)
#' @export
#'
mat2dat = function (mat) {
	dat = reshape2::melt(mat)
	colnames(dat) = c('y','x','z')
	return(dat)
}
