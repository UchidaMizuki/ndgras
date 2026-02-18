#' N-dimensional Generalized RAS (nD-GRAS) method
#'
#' @param source An array to be adjusted.
#' @param constraints A list of constraints created by [nd_gras_constraint()].
#' @param ... Additional arguments (currently unused).
#' @param tolerance A numeric value indicating the convergence tolerance. Default is `1e-10`.
#' @param max_iterations An integer indicating the maximum number of iterations. Default is `1000`.
#' @param verbose A logical value indicating whether to print progress messages. Default is `FALSE`.
#'
#' @return A list containing the following components:
#'   \item{target}{The adjusted n-dimensional array.}
#'   \item{margins}{A list of margins used in the constraints.}
#'   \item{multipliers}{A list of multipliers for each constraint.}
#'   \item{iterations}{The number of iterations performed.}
#'   \item{max_change_multipliers}{The maximum absolute difference between multipliers of the last two iterations.}
#'   \item{converged}{A logical value indicating whether the algorithm converged.}
#'
#' @export
nd_gras <- function(
  source,
  constraints,
  ...,
  tolerance = 1e-10,
  max_iterations = 1000,
  verbose = FALSE
) {
  source <- as.array(source)
  constraints <- nd_gras_validate_constraints(constraints)
  tolerance <- nd_gras_validate_tolerance(tolerance)
  max_iterations <- nd_gras_validate_max_iterations(max_iterations)

  margins <- purrr::map(constraints, \(constraint) {
    vctrs::vec_as_location(
      constraint$margin,
      vctrs::vec_size(dim(source)),
      names(dimnames(source))
    )
  })
  targets <- purrr::map(constraints, \(constraint) {
    constraint$target
  })
  multipliers <- purrr::map(margins, \(margin) {
    array(1, dim = dim(source)[margin])
  })

  source <- nd_gras_initialize_source(
    source = source,
    margins = margins,
    targets = targets
  )
  source_positive <- pmax(source, 0)
  source_negative <- pmax(-source, 0)

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

    max_change_multipliers <- max(
      purrr::map2_dbl(
        multipliers,
        multipliers_old,
        function(multiplier, multiplier_old) {
          max(abs(multiplier - multiplier_old))
        }
      )
    )
    if (verbose) {
      cli::cli_inform(
        "Iteration {iteration}: Max change of multipliers = {max_change_multipliers}"
      )
    }

    converged <- max_change_multipliers < tolerance
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
    max_change_multipliers = max_change_multipliers,
    converged = converged
  )
}

nd_gras_initialize_source <- function(source, margins, targets) {
  multipliers <- purrr::map(targets, \(target) {
    is.na(target) | target != 0
  })
  nd_gras_scale(
    source = source,
    margins = margins,
    multipliers = multipliers,
    type = "positive"
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
