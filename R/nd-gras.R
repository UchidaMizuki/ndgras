#' N-dimensional Generalized RAS (nD-GRAS) method
#'
#' @param source An array to be adjusted.
#' @param constraints A list of constraints created by [nd_gras_constraint()].
#' @param ... Additional arguments (currently unused).
#' @param tolerance A numeric value indicating the convergence tolerance. Default is `1e-10`.
#' @param max_iterations An integer indicating the maximum number of iterations. Default is `1000`.
#'
#' @return A list containing the following components:
#'   \item{target}{The adjusted n-dimensional array.}
#'   \item{margins}{A list of margins used in the constraints.}
#'   \item{multipliers}{A list of multipliers for each constraint.}
#'   \item{iterations}{The number of iterations performed.}
#'   \item{converged}{A logical value indicating whether the algorithm converged.}
#'
#' @export
nd_gras <- function(
  source,
  constraints,
  ...,
  tolerance = 1e-10,
  max_iterations = 1000
) {
  source <- as.array(source)
  constraints <- nd_gras_validate_constraints(constraints)
  tolerance <- nd_gras_validate_tolerance(tolerance)
  max_iterations <- nd_gras_validate_max_iterations(max_iterations)

  source_positive <- pmax(source, 0)
  source_negative <- pmax(-source, 0)

  margins <- purrr::map(constraints, \(constraint) {
    constraint$margin
  })
  targets <- purrr::map(constraints, \(constraint) {
    constraint$target
  })
  multipliers <- purrr::map(margins, \(margin) {
    array(1, dim = dim(source)[margin])
  })

  for (iteration in seq_len(max_iterations)) {
    multipliers_old <- multipliers
    for (index in seq_along(constraints)) {
      marginal_positive <- nd_gras_marginal(
        source = source_positive,
        margins_other = margins[-index],
        multipliers_other = multipliers[-index],
        margin = margins[[index]],
        type = "positive"
      )
      marginal_negative <- nd_gras_marginal(
        source = source_negative,
        margins_other = margins[-index],
        multipliers_other = multipliers[-index],
        margin = margins[[index]],
        type = "negative"
      )
      multipliers[[index]] <- nd_gras_multiplier(
        marginal_positive = marginal_positive,
        marginal_negative = marginal_negative,
        target = targets[[index]]
      )
    }

    converged <- nd_gras_converged(
      multipliers = multipliers,
      multipliers_old = multipliers_old,
      tolerance = tolerance
    )
    if (converged) {
      break
    }
  }

  target <- nd_gras_target(
    source_positive = source_positive,
    source_negative = source_negative,
    margins = margins,
    multipliers = multipliers
  )
  list(
    target = target,
    margins = margins,
    multipliers = multipliers,
    iterations = iteration,
    converged = converged
  )
}

nd_gras_scale <- function(
  source,
  margins,
  multipliers,
  type
) {
  scaled <- source
  for (index in seq_along(margins)) {
    scaled <- sweep(
      scaled,
      margins[[index]],
      multipliers[[index]],
      switch(type, positive = "*", negative = "/")
    )
  }
  scaled
}

nd_gras_marginal <- function(
  source,
  margins_other,
  multipliers_other,
  margin,
  type
) {
  scaled <- nd_gras_scale(
    source = source,
    margins = margins_other,
    multipliers = multipliers_other,
    type = type
  )
  apply(scaled, margin, sum)
}

nd_gras_multiplier <- function(
  marginal_positive,
  marginal_negative,
  target
) {
  multiplier <- ifelse(
    marginal_positive == 0,
    -marginal_negative / target,
    (target + sqrt(target^2 + 4 * marginal_positive * marginal_negative)) /
      (2 * marginal_positive)
  )
  multiplier[is.na(multiplier)] <- 1
  multiplier
}

nd_gras_converged <- function(multipliers, multipliers_old, tolerance) {
  all(purrr::map2_lgl(
    multipliers,
    multipliers_old,
    function(multiplier, multiplier_old) {
      max(abs(multiplier - multiplier_old)) < tolerance
    }
  ))
}

nd_gras_target <- function(
  source_positive,
  source_negative,
  margins,
  multipliers
) {
  scaled_positive <- nd_gras_scale(
    source = source_positive,
    margins = margins,
    multipliers = multipliers,
    type = "positive"
  )
  scaled_negative <- nd_gras_scale(
    source = source_negative,
    margins = margins,
    multipliers = multipliers,
    type = "negative"
  )
  scaled_positive - scaled_negative
}
