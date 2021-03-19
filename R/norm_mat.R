#' Normalize values in a matrix
#'
#' This function normalizes values in a matrix within a certain range (default 0-255).
#'
#' @param mat A matrix, whose values will be normalized.
#' @param rng A range of normalized values, which is c(0,255) as default.
#' @return A matrix whose values are normalized to a given range.
#' @author Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
#' @keywords utilities
#' @examples
#' mat0 = matrix(seq(-2,9,by=1), nrow=4)
#' mat1 = norm_mat(mat0)
#' mat2 = norm_mat(mat0, c(-1,1))
#' print(mat0)
#' print(mat1)
#' print(mat2)
#' @export
#'
norm_mat <- function (mat, rng=c(0,255)) {
	original.dim = dim(mat)
	dim(mat) = NULL
	mat = normalize(mat, rng)
	dim(mat) = original.dim
	return(mat)
}
