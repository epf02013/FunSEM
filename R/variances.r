define_level_slope_variances <- function (variable_of_interest_name, include_slope = FALSE) {
  level_name = paste0(variable_of_interest_name,"_level")
  level_to_level = paste0(level_name, " ~~ ", level_name)
  if (!include_slope) {
    return(level_to_level)
  }
  slope_name = paste0(variable_of_interest_name,"_slope")
  slope_to_slope = paste0(slope_name, " ~~ ", slope_name)

  level_to_slope = paste0(level_name, " ~~ ", slope_name)
  paste(level_to_level, level_to_slope, slope_to_slope, sep = "\n")
}
