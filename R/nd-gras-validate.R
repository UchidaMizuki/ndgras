nd_gras_validate_constraints <- function(constraints) {
  vctrs::obj_check_list(constraints)

  required_names <- c("margin", "target")
  have_required_names <- purrr::map_lgl(constraints, \(constraint) {
    all(required_names %in% names(constraint))
  })
  if (!all(have_required_names)) {
    cli::cli_abort(
      "{.arg constraints} must have required names: {.field {required_names}}."
    )
  }
  constraints
}

nd_gras_validate_tolerance <- function(tolerance) {
  tolerance <- vctrs::vec_cast(tolerance, double())
  vctrs::vec_check_size(tolerance, size = 1)

  if (tolerance <= 0) {
    cli::cli_abort(
      "{.arg tolerance} must be greater than zero."
    )
  }
  tolerance
}

nd_gras_validate_max_iterations <- function(max_iterations) {
  max_iterations <- vctrs::vec_cast(max_iterations, integer())
  vctrs::vec_check_size(max_iterations, size = 1)

  if (max_iterations <= 0) {
    cli::cli_abort(
      "{.arg max_iterations} must be greater than zero."
    )
  }
  max_iterations
}
