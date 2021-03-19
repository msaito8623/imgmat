#' Conversion of a continuous vector to a discrete vector
#'
#' This function simply sorts elements in a vector and give indices to each element. E.g. c(0.3, -0.08, 4.2) --> c(2, 1, 3). Therefore, please note that intervals between elements are discarded.
#'
#' @param vec A numeric vector. Its minimum value becomes 1. Its maximum value becomes the length of the vector.
#' @return A numeric vector.
#' @author Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
#' @keywords utilities
#' @examples
#' vec = c(0.3, -0.08, 4.2)
#' print(to_index(vec))
#' @export
#'
to_index = function (vec) {
	vec = as.numeric(factor(vec, levels=sort(unique(vec)), labels=1:length(unique(vec))))
	return(vec)
}
