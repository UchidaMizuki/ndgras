
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ndgras

<!-- badges: start -->

<!-- badges: end -->

The goal of ndgras is to provide an implementation of the n-dimensional
Generalized RAS (nD-GRAS) method. This method adjusts an n-dimensional
array to match specified marginal sums while preserving the structure of
the original array as much as possible. It is capable of handling
negative values in the source array using the generalized RAS (GRAS)
algorithm.

## Installation

You can install the development version of ndgras from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("UchidaMizuki/ndgras")
```

## Example

This is a basic example which shows you how to adjust a 3-dimensional
array:

``` r
library(ndgras)

# Create a source array (2x2x2)
source <- array(1:8 - 2, dim = c(2, 2, 2))
source
#> , , 1
#> 
#>      [,1] [,2]
#> [1,]   -1    1
#> [2,]    0    2
#> 
#> , , 2
#> 
#>      [,1] [,2]
#> [1,]    3    5
#> [2,]    4    6

# Define target marginal sums
# Constraint on dimension 1
target1 <- c(10, 20)
# Constraint on dimension 2
target2 <- c(12, 18)
# Constraint on dimension 3
target3 <- c(14, 16)

# Note: The sum of each target vector must be equal (Total = 30)

constraints <- list(
  nd_gras_constraint(1, target1),
  nd_gras_constraint(2, target2),
  nd_gras_constraint(3, target3)
)

# Apply nD-GRAS
result <- nd_gras(source, constraints)

# The adjusted array
result$target
#> , , 1
#> 
#>             [,1]      [,2]
#> [1,] -0.05256448  3.988625
#> [2,]  0.00000000 10.063939
#> 
#> , , 2
#> 
#>          [,1]     [,2]
#> [1,] 4.493693 1.570246
#> [2,] 7.558871 2.377189

# Check marginal sums
apply(result$target, 1, sum)
#> [1] 10 20
apply(result$target, 2, sum)
#> [1] 12 18
apply(result$target, 3, sum)
#> [1] 14 16
```
