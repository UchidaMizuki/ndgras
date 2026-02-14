describe("nd_gras_constraint()", {
  it("can check target dimensions", {
    expect_no_error(nd_gras_constraint(2, 1:3))
    expect_no_error(nd_gras_constraint(c(1, 3), matrix(1:4, 2, 2)))
    expect_error(nd_gras_constraint(2, matrix(1:4, 2, 2)))
  })
})

describe("nd_gras()", {
  set.seed(1234)

  dim_source <- c(3, 4, 5)
  source <- array(
    sample(seq(-1, 1, by = 0.1), prod(dim_source), replace = TRUE),
    dim = dim_source
  )

  margin_1 <- 1:2
  target_1 <- array(
    runif(prod(dim_source[margin_1])),
    dim = dim_source[margin_1]
  )
  target_1 <- target_1 / sum(target_1)

  margin_2 <- 3
  target_2 <- array(
    runif(prod(dim_source[margin_2])),
    dim = dim_source[margin_2]
  )
  target_2 <- target_2 / sum(target_2)

  it("can converge", {
    result <- nd_gras(
      source = source,
      constraints = list(
        nd_gras_constraint(margin_1, target_1),
        nd_gras_constraint(margin_2, target_2)
      )
    )
    expect_equal(apply(result$target, margin_1, sum), target_1)
    expect_equal(apply(result$target, margin_2, sum), as.vector(target_2))
  })

  it("can converge when targets contain NA", {
    target_1[2, 3] <- NA
    target_2[2] <- NA

    result <- nd_gras(
      source = source,
      constraints = list(
        nd_gras_constraint(margin_1, target_1),
        nd_gras_constraint(margin_2, target_2)
      )
    )
    target_1_result <- apply(result$target, margin_1, sum)
    target_1_result[2, 3] <- NA
    expect_equal(target_1_result, target_1)

    target_2_result <- apply(result$target, margin_2, sum)
    target_2_result[2] <- NA
    expect_equal(target_2_result, as.vector(target_2))
  })
})
