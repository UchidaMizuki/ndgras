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

  margin1 <- 1:2
  target1 <- array(
    runif(prod(dim_source[margin1])),
    dim = dim_source[margin1]
  )
  target1 <- target1 / sum(target1)

  margin2 <- 3
  target2 <- array(
    runif(prod(dim_source[margin2])),
    dim = dim_source[margin2]
  )
  target2 <- target2 / sum(target2)

  it("can converge", {
    result <- nd_gras(
      source = source,
      constraints = list(
        nd_gras_constraint(margin1, target1),
        nd_gras_constraint(margin2, target2)
      )
    )
    expect_equal(apply(result$target, margin1, sum), target1)
    expect_equal(apply(result$target, margin2, sum), as.vector(target2))
  })

  it("can converge when targets contain NA", {
    target1[2, 3] <- NA
    target2[2] <- NA

    result <- nd_gras(
      source = source,
      constraints = list(
        nd_gras_constraint(margin1, target1),
        nd_gras_constraint(margin2, target2)
      )
    )
    target1_result <- apply(result$target, margin1, sum)
    target1_result[2, 3] <- NA
    expect_equal(target1_result, target1)

    target2_result <- apply(result$target, margin2, sum)
    target2_result[2] <- NA
    expect_equal(target2_result, as.vector(target2))
  })

  it("can converge when targets contain zero", {
    target1[2, 3] <- 0
    target1 <- target1 / sum(target1)

    target2[2] <- 0
    target2 <- target2 / sum(target2)

    result <- nd_gras(
      source = source,
      constraints = list(
        nd_gras_constraint(margin1, target1),
        nd_gras_constraint(margin2, target2)
      )
    )
    expect_true(all(result$target[2, 3, ] == 0))
    expect_true(all(result$target[,, 2] == 0))
    expect_equal(apply(result$target, margin1, sum), target1)
    expect_equal(apply(result$target, margin2, sum), as.vector(target2))
  })

  it("can converge in the same way as mipfp::Ipfp()", {
    skip_on_cran()
    skip_if_not_installed("mipfp")

    source <- array(
      sample(seq(0, 1, by = 0.1), prod(dim_source), replace = TRUE),
      dim = dim_source
    )

    result_nd_gras <- nd_gras(
      source = source,
      constraints = list(
        nd_gras_constraint(margin1, target1),
        nd_gras_constraint(margin2, target2)
      )
    )
    result_mipfp <- mipfp::Ipfp(
      seed = source,
      target.list = list(margin1, margin2),
      target.data = list(target1, target2)
    )
    expect_equal(result_nd_gras$target, result_mipfp$x.hat)
  })
})
