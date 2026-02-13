test_that("nd_gras() works", {
  set.seed(1234)

  dim_prior <- c(3, 4, 5)
  prior <- array(
    sample(seq(-1, 1, by = 0.1), prod(dim_prior), replace = TRUE),
    dim = dim_prior
  )

  margin_1 <- 1:2
  target_1 <- array(runif(prod(dim_prior[margin_1])), dim = dim_prior[margin_1])
  target_1 <- target_1 / sum(target_1)

  margin_2 <- 3
  target_2 <- array(runif(prod(dim_prior[margin_2])), dim = dim_prior[margin_2])
  target_2 <- target_2 / sum(target_2)

  result <- nd_gras(
    prior = prior,
    constraints = list(
      nd_gras_constraint(margin_1, target_1),
      nd_gras_constraint(margin_2, target_2)
    )
  )
  expect_equal(apply(result, margin_1, sum), target_1)
  expect_equal(apply(result, margin_2, sum), as.vector(target_2))
})
