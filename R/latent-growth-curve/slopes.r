define_slope_weight_for_factor = function (factor_name, slope_weight) {
  paste0(slope_weight, "*", factor_name)
}

define_slope_factor <- function (factor_sets, variable_of_interest_name, slope_weights) {
  factor_names = colnames(factor_sets)
  factor_names_with_weights = mapply(define_slope_weight_for_factor, factor_names, slope_weights)
  added_factors = paste(factor_names_with_weights, collapse = " + ")
  paste0(variable_of_interest_name,"_slope =~ ",added_factors)
}
