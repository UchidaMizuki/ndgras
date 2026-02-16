
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

# Apply nD-GRAS
result <- nd_gras(
  source = source,
  constraints = list(
    nd_gras_constraint(1, target1),
    nd_gras_constraint(2, target2),
    nd_gras_constraint(3, target3)
  )
)

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

## References

- [Valderas-Jaramillo, Juan Manuel, and José Manuel
  Rueda-Cantuche. 2021. “The Multidimensional nD‐GRAS Method:
  Applications for the Projection of Multiregional Input–Output
  Frameworks and Valuation Matrices.” Papers in Regional Science: The
  Journal of the Regional Science Association International 100 (6):
  1599–1625.](https://doi.org/10.1111/pirs.12625)
- [Junius, Theo, and Jan Oosterhaven. 2003. “The Solution of Updating or
  Regionalizing a Matrix with Both Positive and Negative Entries.”
  Economic Systems Research : Journal of the International Input-Output
  Association 15 (1):
  87–96.](https://doi.org/10.1080/0953531032000056954)
- [Lenzen, Manfred, Richard Wood, and Blanca Gallego. 2007. “Some
  Comments on the GRAS Method.” Economic Systems Research : Journal of
  the International Input-Output Association 19 (4):
  461–65.](https://doi.org/10.1080/09535310701698613)
