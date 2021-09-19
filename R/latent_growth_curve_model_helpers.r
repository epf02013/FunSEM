set_time_invariant_covariate = function (variable_to_set_level_of, covariate_name, regress_slope_on_covariate) {
  regressing_level = paste0(variable_to_set_level_of,"_level ~ ", covariate_name)
  if(!regress_slope_on_covariate) return(regressing_level)
  regressing_slope = paste0(variable_to_set_level_of,"_slope ~ ", covariate_name)
  paste(regressing_level, regressing_slope, sep="\n")
}
