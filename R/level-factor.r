
define_level_factor <- function (factor_sets) {
  factor_names = colnames(factor_sets)
  added_factors = paste(factor_names, collapse = " + 1*")
  paste0("level =~ 1*",added_factors)
}

