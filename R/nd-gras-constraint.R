nd_gras_constraint <- function(margin, target) {
  margin <- vctrs::vec_cast(margin, integer())
  target <- as.array(target)
  vctrs::vec_check_size(dim(target), vctrs::vec_size(margin))

  list(margin = margin, target = target)
}
