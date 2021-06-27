
define_level_factor <- function (factor_sets, variable_of_interest_name) {
  factor_names = colnames(factor_sets)
  added_factors = paste(factor_names, collapse = " + 1*")
  paste0(variable_of_interest_name, "_level =~ 1*",added_factors)
}

