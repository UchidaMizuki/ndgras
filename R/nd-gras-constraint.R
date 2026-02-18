#' Create a constraint for the nD-GRAS method
#'
#' @param margin An integer vector indicating the dimensions to constrain.
#' @param target An array (or vector) representing the target marginal sums.
#'
#' @return A list containing the margin and target.
#'
#' @export
nd_gras_constraint <- function(margin, target) {
  target <- as.array(target)
  vctrs::vec_check_size(dim(target), vctrs::vec_size(margin))

  list(margin = margin, target = target)
}
